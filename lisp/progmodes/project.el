;;; project.el --- Operations on the current project  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024 Free Software Foundation, Inc.
;; Version: 0.11.1
;; Package-Requires: ((emacs "26.1") (xref "1.7.0"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains generic infrastructure for dealing with
;; projects, some utility functions, and commands using that
;; infrastructure.
;;
;; The goal is to make it easier for Lisp programs to operate on the
;; current project, without having to know which package handles
;; detection of that project type, parsing its config files, etc.
;;
;; This file consists of following parts:
;;
;; Infrastructure (the public API):
;;
;; Function `project-current' that returns the current project
;; instance based on the value of the hook `project-find-functions',
;; and several generic functions that act on it.
;;
;; `project-root' must be defined for every project.
;; `project-files' can be overridden for performance purposes.
;; `project-ignores' and `project-external-roots' describe the project
;; files and its relations to external directories.  `project-files'
;; should be consistent with `project-ignores'.
;;
;; `project-buffers' can be overridden if the project has some unusual
;; shape (e.g. it contains files residing outside of its root, or some
;; files inside the root must not be considered a part of it).  It
;; should be consistent with `project-files'.
;;
;; This list can change in future versions.
;;
;; Transient project:
;;
;; An instance of this type can be returned by `project-get-project' if no
;; project was detected automatically, and the user had to pick a
;; directory manually.  The fileset it describes is the whole
;; directory, with the exception of some standard ignored files and
;; directories.  This type has little purpose otherwise, as the only
;; generic function it provides an override for is `project-root'.
;;
;; VC-aware project:
;;
;; Originally conceived as an example implementation, now it's a
;; relatively fast backend that delegates to 'git ls-files' or 'hg
;; status' to list the project's files.  It honors the VC ignore
;; files, but supports additions to the list using the user option
;; `project-vc-ignores' (usually through .dir-locals.el).  See the
;; customization group `project-vc' for other options that control its
;; behavior.
;;
;; If the repository is using any other VCS than Git or Hg, the file
;; listing uses the default mechanism based on `find-program'.
;;
;; This project type can also be used for non-VCS controlled
;; directories, see the variable `project-vc-extra-root-markers'.
;;
;; Utils:
;;
;; `project-combine-directories' and `project-subtract-directories',
;; mainly for use in the abovementioned generics' implementations.
;;
;; `project-known-project-roots' and `project-remember-project' to
;; interact with the "known projects" list.
;;
;; Commands:
;;
;; `project-prefix-map' contains the full list of commands defined in
;; this package.  This map uses the prefix `C-x p' by default.
;; Type `C-x p f' to find file in the current project.
;; Type `C-x p C-h' to see all available commands and bindings.
;;
;; All commands defined in this package are implemented using the
;; public API only.  As a result, they will work with any project
;; backend that follows the protocol.
;;
;; Any third-party code that wants to use this package should likewise
;; target the public API.  Use any of the built-in commands as the
;; example.
;;
;; How to create a new backend:
;;
;; - Consider whether you really should, or whether there are other
;; ways to reach your goals.  If the backend's performance is
;; significantly lower than that of the built-in one, and it's first
;; in the list, it will affect all commands that use it.  Unless you
;; are going to be using it only yourself or in special circumstances,
;; you will probably want it to be fast, and it's unlikely to be a
;; trivial endeavor.  `project-files' is the method to optimize (the
;; default implementation gets slower the more files the directory
;; has, and the longer the list of ignores is).
;;
;; - Choose the format of the value that represents a project for your
;; backend (we call it project instance).  Don't use any of the
;; formats from other backends.  The format can be arbitrary, as long
;; as the datatype is something `cl-defmethod' can dispatch on.  The
;; value should be stable (when compared with `equal') across
;; invocations, meaning calls to that function from buffers belonging
;; to the same project should return equal values.
;;
;; - Write a new function that will determine the current project
;; based on the directory and add it to `project-find-functions'
;; (which see) using `add-hook'.  It is a good idea to depend on the
;; directory only, and not on the current major mode, for example.
;; Because the usual expectation is that all files in the directory
;; belong to the same project (even if some/most of them are ignored).
;;
;; - Define new methods for some or all generic functions for this
;; backend using `cl-defmethod'.  A `project-root' method is
;; mandatory, `project-files' is recommended, the rest are optional.

;;; TODO:

;; * Reliably cache the list of files in the project, probably using
;;   filenotify.el (if supported) to invalidate.  And avoiding caching
;;   if it's not available (manual cache invalidation is not nice).
;;
;; * Build tool related functionality.  Start with a `project-build'
;;   command, which should provide completions on tasks to run, and
;;   maybe allow entering some additional arguments.  This might
;;   be handled better with a separate API, though.  Then we won't
;;   force every project backend to be aware of the build tool(s) the
;;   project is using.
;;
;; * Command to (re)build the tag files in all project roots.  To that
;;   end, we might need to add a way to provide file whitelist
;;   wildcards for each root to limit etags to certain files (in
;;   addition to the blacklist provided by ignores), and/or allow
;;   specifying additional tag regexps.
;;
;; * UI for the user to be able to pick the current project for the
;;   whole Emacs session, independent of the current directory.  Or,
;;   in the more advanced case, open a set of projects, and have some
;;   project-related commands to use them all.  E.g., have a command
;;   to search for a regexp across all open projects.
;;
;; * Support for project-local variables: a UI to edit them, and a
;;   utility function to retrieve a value.  Probably useless without
;;   support in various built-in commands.  In the API, we might get
;;   away with only adding a `project-configuration-directory' method,
;;   defaulting to the project root the current file/buffer is in.
;;   And prompting otherwise.  How to best mix that with backends that
;;   want to set/provide certain variables themselves, is up for
;;   discussion.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'vc-svn)

(defgroup project nil
  "Operations on the current project."
  :version "28.1"
  :group 'tools)

(defvar project-find-functions (list #'project-try-vc)
  "Identify project for prevailing directory.
Functions added to this abnormal hook must accept
a directory and return a project instance, the
type of which must be dispatchable by `cl-defmethod',
i.e., a cons or cl-struct.")

(define-obsolete-variable-alias
  'project-current-inhibit-prompt
  'project-current-directory-override
  "29.1")

(defvar project-current-directory-override nil
  "Value to use instead of `default-directory' when detecting the project.
When it is non-nil, `project-current' will always skip prompting too.")

(defcustom project-prompter #'project-prompt-project-dir
  "Function to call to prompt for a project.
Called with no arguments and should return a project root dir."
  :type '(choice (const :tag "Prompt for a project directory"
                        project-prompt-project-dir)
                 (const :tag "Prompt for a project name"
                        project-prompt-project-name)
                 (function :tag "Custom function" nil))
  :group 'project
  :version "30.1")

;;;###autoload
(defun project-get-project (&optional directory)
  "Return the project for DIRECTORY, and mark as most recently used.
DIRECTORY defaults to `default-directory'.  If no project obtains
from DIRECTORY, prompt the user for an alternate directory.  If
no project obtains from the alternate, return the \"transient\"
project instance and do not adjust recently used projects."
  (let* ((directory (or directory
                        project-current-directory-override
                        default-directory))
         (pr (project--find-in-directory directory)))
    (when (and (not pr)
               (not project-current-directory-override))
      (setq directory (project-prompt-project-dir)
            pr (project--find-in-directory directory)))
    (if pr
        (prog1 pr
          (project-remember-project pr))
      (prog1 (cons 'transient directory)
        (project--remove-from-project-list directory nil)))))

;;;###autoload
(defun project-current (&optional maybe-prompt directory)
  "Return the project for DIRECTORY.
DIRECTORY defaults to `default-directory'.
Under MAYBE-PROMPT, calls `project-get-project'."
  ;; Gradually replace occurrences of (project-current t)
  ;; with (project-most-recent-project), and replace (project-current nil dir)
  ;; with (let ((default-directory dir)) (project-current))
  (if maybe-prompt
      (project-get-project directory)
    (let ((pr (project--find-in-directory
               (or directory
                   project-current-directory-override
                   default-directory))))
      (prog1 pr (when pr (project-remember-project pr))))))

(defsubst project--find-in-directory (dir)
  (run-hook-with-args-until-success 'project-find-functions dir))

(defvar project--within-roots-fallback nil)

(cl-defgeneric project-root (project)
  "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute.")

(cl-defmethod project-root (project
                            &context (project--within-roots-fallback
                                      (eql nil)))
  (let ((project--within-roots-fallback t))
    (project-root project)))

(cl-defgeneric project-roots (project)
  "Return the list containing the current project root.

The function is obsolete, all projects have one main root anyway,
and the rest should be possible to express through
`project-external-roots'."
  ;; FIXME: Can we specify project's version here?
  ;; FIXME: Could we make this affect cl-defmethod calls too?
  (declare (obsolete project-root "0.3.0"))
  (let ((project--within-roots-fallback t))
    (list (project-root project))))

;; FIXME: Add MODE argument, like in `ede-source-paths'?
(cl-defgeneric project-external-roots (_project)
  "Return the list of external roots for PROJECT.

It's the list of directories outside of the project that are
still related to it.  If the project deals with source code then,
depending on the languages used, this list should include the
headers search path, load path, class path, and so on."
  nil)

(cl-defgeneric project-name (project)
  "A human-readable name for the PROJECT.
Nominally unique, but not enforced."
  (file-name-nondirectory (directory-file-name (project-root project))))

(defsubst project--annotate-prompt (project prompt)
  (if project
      (format "[%s] %s" (project-name project) prompt)
    prompt))

(cl-defgeneric project-ignores (_project _dir)
  "Return the list of glob patterns to ignore inside DIR.
Patterns can match both regular files and directories.
To root an entry, start it with `./'.  To match directories only,
end it with `/'.  DIR must be either `project-root' or one of
`project-external-roots'."
  ;; TODO: Document and support regexp ignores as used by Hg.
  ;; TODO: Support whitelist entries.
  (require 'grep)
  (defvar grep-find-ignored-files)
  (nconc
   (mapcar
    (lambda (dir)
      (concat dir "/"))
    vc-directory-exclusion-list)
   grep-find-ignored-files))

(defun project--file-completion-table (all-files)
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      '(metadata . ((category . project-file))))
     (t
      (complete-with-action action all-files string pred)))))

(cl-defmethod project-root ((project (head transient)))
  (cdr project))

(defsubst project--filter-meta-files (files)
  "Return FILES without autosave or backup entries."
  (cl-remove-if
   (lambda (name)
     (or (backup-file-name-p (file-name-nondirectory name))
         (auto-save-file-name-p (file-name-nondirectory name))))
   files))

(cl-defgeneric project-files (project &optional dirs)
  "Return a list of files in directories DIRS in PROJECT.
DIRS is a list of absolute directories; it should be some
subset of the project root and external roots.

The default implementation uses `find-program'.  PROJECT is used
to find the list of ignores for each directory."
  (project--filter-meta-files
   (mapcan (lambda (dir)
             (project--files-in-directory dir (project--dir-ignores project dir)))
           (or dirs (list (project-root project))))))

(defun project--files-in-directory (dir ignores &optional files)
  (require 'find-dired)
  (require 'xref)
  (let* ((default-directory dir)
         (command (format "%s -H . %s -type f %s -print0"
                          find-program
                          (xref--find-ignores-arguments ignores "./")
                          (if files
                              (concat (shell-quote-argument "(")
                                      " -name "
                                      (mapconcat
                                       #'shell-quote-argument
                                       (split-string files)
                                       (concat " -o -name "))
                                      " "
                                      (shell-quote-argument ")"))
                            "")))
         res)
    (with-temp-buffer
      (let ((status (process-file-shell-command command nil t))
            (pt (point-min)))
        (unless (zerop status)
          (goto-char (point-min))
          (if (and (not (eql status 127))
                   (search-forward "Permission denied\n" nil t))
              (let ((end (1- (point))))
                (re-search-backward "\\`\\|\0")
                (error "File listing failed: %s"
                       (buffer-substring (1+ (point)) end)))
            (error "File listing failed: %s" (buffer-string))))
        (goto-char pt)
        (while (search-forward "\0" nil t)
          (push (buffer-substring-no-properties (1+ pt) (1- (point))) res)
          (setq pt (point)))
        (setq res (sort res #'string<))))
    (let* ((remote-id (file-remote-p default-directory))
           (path (if remote-id
                     (concat remote-id
                             (directory-file-name
                              (file-name-unquote
                               (file-local-name
                                (expand-file-name default-directory)))))
                   (directory-file-name default-directory))))
      (mapcar (lambda (file) (concat path file)) res))))

(cl-defgeneric project-buffers (project)
  "Return the list of all live buffers that belong to PROJECT.

The default implementation matches each buffer to PROJECT root using
the buffer's value of `default-directory'."
  (let ((root (expand-file-name (file-name-as-directory (project-root project))))
        bufs)
    (dolist (buf (buffer-list))
      (when (string-prefix-p root (expand-file-name
                                   (buffer-local-value 'default-directory buf)))
        (push buf bufs)))
    (nreverse bufs)))

(defgroup project-vc nil
  "VC-aware project implementation."
  :version "25.1"
  :group 'project)

(defcustom project-vc-ignores nil
  "List of patterns to add to `project-ignores'."
  :type '(repeat string))
;; Change to `list-of-strings-p' when support for Emacs 28 is dropped.
;;;###autoload(put 'project-vc-ignores 'safe-local-variable (lambda (val) (and (listp val) (not (memq nil (mapcar #'stringp val))))))

(defcustom project-vc-merge-submodules t
  "Non-nil to consider submodules part of the parent project.

After changing this variable (using Customize or .dir-locals.el)
you might have to restart Emacs to see the effect."
  :type 'boolean
  :version "28.1"
  :package-version '(project . "0.2.0"))
;;;###autoload(put 'project-vc-merge-submodules 'safe-local-variable #'booleanp)

(defcustom project-vc-include-untracked t
  "When non-nil, the VC-aware project backend includes untracked files."
  :type 'boolean
  :version "29.1")
;;;###autoload(put 'project-vc-include-untracked 'safe-local-variable #'booleanp)

(defcustom project-vc-name nil
  "When non-nil, the name of the current VC-aware project.

The best way to change the value a VC-aware project reports as
its name, is by setting this in .dir-locals.el."
  :type '(choice (const :tag "Default to the base name" nil)
                 (string :tag "Custom name"))
  :version "29.1"
  :package-version '(project . "0.9.0"))
;;;###autoload(put 'project-vc-name 'safe-local-variable #'stringp)

(defcustom project-vc-extra-root-markers nil
  "Identify a project that isn't version-controlled.

Examples include \".dir-locals.el\", \"package.json\", \"pom.xml\",
\"requirements.txt\", \"Gemfile\", \"*.gemspec\", \"autogen.sh\"."
  :type '(repeat string)
  :version "29.1"
  :package-version '(project . "0.9.0"))
;; Change to `list-of-strings-p' when support for Emacs 28 is dropped.
;;;###autoload(put 'project-vc-extra-root-markers 'safe-local-variable (lambda (val) (and (listp val) (not (memq nil (mapcar #'stringp val))))))

(defvar project-vc-external-roots-function (lambda () tags-table-list)
  "Function that returns a list of external roots.

It should return a list of directory roots that contain source
files related to the current buffer.

The directory names should be absolute.  Used in the VC-aware
project backend implementation of `project-external-roots'.")

(defvar project-vc-backend-markers-alist
  `((Git . ".git")
    (Hg . ".hg")
    (Bzr . ".bzr")
    (SVN . ,vc-svn-admin-directory)
    (DARCS . "_darcs")
    (Fossil . ".fslckout")
    (Got . ".got"))
  "Associative list assigning root markers to VC backend symbols.
See `project-vc-extra-root-markers' for the marker value format.")

(defun project-try-vc (dir)
  "Return DIR's \\='project-vc property consisting of backend and root."
  (let* ((locate-dominating-stop-dir-regexp
          (or vc-ignore-dir-regexp locate-dominating-stop-dir-regexp))
         (extra-markers (project--value-in-dir 'project-vc-extra-root-markers dir))
         (marker-p
          (lambda (d)
            (when-let ((prop
                        (or (vc-file-getprop d 'project-vc)
                            (when-let ((files (when (file-directory-p d)
                                                (directory-files d nil nil 'nosort))))
                              (catch 'done
                                (dolist (pair project-vc-backend-markers-alist)
                                  (cl-destructuring-bind (backend . marker) pair
                                    (when (cl-some
                                           (let (case-fold-search)
                                             (apply-partially #'string-match-p
                                                              (wildcard-to-regexp marker)))
                                           files)
                                      (let ((root d))
                                        (when (and (eq backend 'Git)
                                                   (project--vc-merge-submodules-p root)
                                                   (project--submodule-p root))
                                          (setq root (vc-call-backend
                                                      'Git 'root
                                                      (file-name-directory (directory-file-name root)))))
                                        (throw 'done (list 'vc backend root))))))
                                (dolist (marker extra-markers)
                                  (when (cl-some
                                         (let (case-fold-search)
                                           (apply-partially #'string-match-p
                                                            (wildcard-to-regexp marker)))
                                         files)
                                    (throw 'done (list 'vc nil d)))))))))
              (prog1 t (vc-file-setprop dir 'project-vc prop))))))
    (locate-dominating-file dir marker-p))
  (vc-file-getprop dir 'project-vc))

(defun project--submodule-p (root)
  ;; XXX: We only support Git submodules for now.
  ;;
  ;; For submodules, at least, we expect the users to prefer them to
  ;; be considered part of the parent project.  For those who don't,
  ;; there is the custom var now.
  ;;
  ;; Some users may also set up things equivalent to Git submodules
  ;; using "git worktree" (for example).  However, we expect that most
  ;; of them would prefer to treat those as separate projects anyway.
  (let* ((gitfile (expand-file-name ".git" root)))
    (cond
     ((file-directory-p gitfile)
      nil)
     ((with-temp-buffer
        (insert-file-contents gitfile)
        (goto-char (point-min))
        ;; Kind of a hack to distinguish a submodule from
        ;; other cases of .git files pointing elsewhere.
        (looking-at "gitdir: [./]+/\\.git/modules/"))
      t)
     (t nil))))

(cl-defmethod project-root ((project (head vc)))
  (nth 2 project))

(cl-defmethod project-external-roots ((project (head vc)))
  (project-subtract-directories
   (project-combine-directories
    (mapcar
     #'file-name-as-directory
     (funcall project-vc-external-roots-function)))
   (list (project-root project))))

(cl-defmethod project-files ((project (head vc)) &optional dirs)
  (mapcan
   (lambda (dir)
     (let ((ignores (project--value-in-dir 'project-vc-ignores (nth 2 project)))
           (backend (cadr project)))
       (when backend
         (require (intern (concat "vc-" (downcase (symbol-name backend))))))
       (project--filter-meta-files
        (if (and (file-equal-p dir (nth 2 project))
                 (or (eq backend 'Hg)
                     (and (eq backend 'Git)
                          (or (not ignores)
                              (version<= "1.9" (vc-git--program-version))))))
            (project--vc-list-files dir backend ignores)
          (project--files-in-directory dir (project--dir-ignores project dir))))))
   (or dirs (list (project-root project)))))

(declare-function vc-git--program-version "vc-git")
(declare-function vc-git--run-command-string "vc-git")
(declare-function vc-hg-command "vc-hg")

(defun project--vc-list-files (dir backend extra-ignores)
  (defvar vc-git-use-literal-pathspecs)
  (pcase backend
    (`Git
     (let* ((default-directory dir)
	    (args '("-z" "-c" "--exclude-standard"))
            (vc-git-use-literal-pathspecs nil)
            (include-untracked (project--value-in-dir
                                'project-vc-include-untracked
                                dir))
            (submodules (project--git-submodules))
            files)
       (setq args (append args
                          (and (version<= "2.35" (vc-git--program-version))
                               '("--sparse"))
                          (and include-untracked '("-o"))))
       (when extra-ignores
         (setq args (append args
                            (cons "--"
                                  (mapcar
                                   (lambda (i)
                                     (format
                                      ":(exclude,glob,top)%s"
                                      (if (string-match "\\*\\*" i)
                                          ;; Looks like pathspec glob
                                          ;; format already.
                                          i
                                        (if (string-match "\\./" i)
                                            ;; ./abc -> abc
                                            (setq i (substring i 2))
                                          ;; abc -> **/abc
                                          (setq i (concat "**/" i))
                                          ;; FIXME: '**/abc' should also
                                          ;; match a directory with that
                                          ;; name, but doesn't (git 2.25.1).
                                          ;; Maybe we should replace
                                          ;; such entries with two.
                                          (if (string-match "/\\'" i)
                                              ;; abc/ -> abc/**
                                              (setq i (concat i "**"))))
                                        i)))
                                   extra-ignores)))))
       (setq files
             (seq-keep
              (lambda (file)
                (unless (or (member file submodules)
                            ;; Should occur for sparse directories
                            ;; only, when sparse index is enabled.
                            (directory-name-p file))
                  (concat (file-name-as-directory dir) file)))
              (split-string
               (apply #'vc-git--run-command-string nil "ls-files" args)
               "\0" t)))
       (when (project--vc-merge-submodules-p default-directory)
         ;; Unfortunately, 'ls-files --recurse-submodules' conflicts with '-o'.
         (let* ((sub-files
                 (mapcar
                  (lambda (module)
                    (when (file-directory-p module)
                      (project--vc-list-files
                       (concat (file-name-as-directory dir) module)
                       backend
                       extra-ignores)))
                  submodules)))
           (setq files (apply #'nconc files sub-files))))
       ;; 'git ls-files' returns duplicate entries for merge conflicts.
       ;; XXX: Better solutions welcome, but this seems cheap enough.
       (delete-consecutive-dups files)))
    (`Hg
     (let* ((default-directory dir)
            (include-untracked (project--value-in-dir
                                'project-vc-include-untracked
                                dir))
            (args (list (concat "-mcard" (and include-untracked "u"))
                        "--no-status"
                        "-0")))
       (when extra-ignores
         (setq args (nconc args (mapcan (lambda (i) (list "--exclude" i))
                                        extra-ignores))))
       (with-temp-buffer
         (apply #'vc-hg-command t 0 "." "status" args)
         (mapcar (lambda (s) (concat (file-name-as-directory dir) s))
                 (split-string (buffer-string) "\0" t)))))))

(defun project--vc-merge-submodules-p (dir)
  (project--value-in-dir
   'project-vc-merge-submodules
   dir))

(defun project--git-submodules ()
  ;; 'git submodule foreach' is much slower.
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents ".gitmodules")
        (let (res)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*path *= *\\(.+\\)" nil t)
            (push (match-string 1) res))
          (nreverse res)))
    (file-missing nil)))

(cl-defmethod project-ignores ((project (head vc)) dir)
  (let* ((root (nth 2 project))
         (backend (cadr project)))
    (append
     (when (and backend
                (file-equal-p dir root))
       (delq
        nil
        (mapcar
         (lambda (entry)
           (cond
            ((eq ?! (aref entry 0))
             ;; No support for whitelisting (yet).
             nil)
            ((string-match "\\(/\\)[^/]" entry)
             ;; FIXME: This seems to be Git-specific.
             ;; And / in the entry (start or even the middle) means
             ;; the pattern is "rooted".  Or actually it is then
             ;; relative to its respective .gitignore (of which there
             ;; could be several), but we only support .gitignore at
             ;; the root.
             (if (= (match-beginning 0) 0)
                 (replace-match "./" t t entry 1)
               (concat "./" entry)))
            (t entry)))
         (condition-case nil
             (vc-call-backend backend 'ignore-completion-table root)
           (vc-not-supported () nil)))))
     (project--value-in-dir 'project-vc-ignores root)
     (mapcar
      (lambda (dir)
        (concat dir "/"))
      vc-directory-exclusion-list))))

(defun project-combine-directories (&rest lists-of-dirs)
  "Return a sorted and culled list of directory names.
Appends the elements of LISTS-OF-DIRS together, removes
non-existing directories, as well as directories a parent of
whose is already in the list."
  (let* ((dirs (sort
                (mapcar
                 (lambda (dir)
                   (file-name-as-directory (expand-file-name dir)))
                 (apply #'append lists-of-dirs))
                #'string<))
         (ref dirs))
    ;; Delete subdirectories from the list.
    (while (cdr ref)
      (if (string-prefix-p (car ref) (cadr ref))
          (setcdr ref (cddr ref))
        (setq ref (cdr ref))))
    (cl-delete-if-not #'file-exists-p dirs)))

(defun project-subtract-directories (files dirs)
  "Return a list of elements from FILES that are outside of DIRS.
DIRS must contain directory names."
  ;; Sidestep the issue of expanded/abbreviated file names here.
  (cl-set-difference files dirs :test #'file-in-directory-p))

(defun project--value-in-dir (var dir)
  (with-temp-buffer
    (setq default-directory dir)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer))
    (symbol-value var)))

(declare-function tramp-tramp-file-p "tramp")
(cl-defmethod project-buffers ((project (head vc)))
  (let* ((root (expand-file-name (file-name-as-directory (project-root project))))
         (modules (when (and (not (project--vc-merge-submodules-p root))
                             (not (project--submodule-p root)))
                    (mapcar (lambda (m) (format "%s%s/" root m))
                            (project--git-submodules))))
         bufs)
    (dolist (buf (buffer-list))
      (when-let ((dir (expand-file-name (buffer-local-value 'default-directory buf)))
                 (not-tramp-p (or (not (fboundp 'tramp-tramp-file-p))
                                  (not (funcall #'tramp-tramp-file-p dir))))
                 (project-p (equal project (project-current nil dir)))
                 (not-submod-p (cl-every (lambda (module)
                                           (not (string-prefix-p module dir)))
                                         modules)))
        (push buf bufs)))
    (nreverse bufs)))

(cl-defmethod project-name ((project (head vc)))
  (or (project--value-in-dir 'project-vc-name (project-root project))
      (cl-call-next-method)))


;;; Project commands

;;;###autoload
(defvar project-prefix-map
  (let ((map (make-sparse-keymap
              (lambda ()
                (when-let ((pr (project--most-recent-project)))
                  (format "[%s]" (project-name pr)))))))
    (define-key map "!" 'project-shell-command)
    (define-key map "&" 'project-async-shell-command)
    (define-key map "f" 'project-find-file)
    (define-key map "F" 'project-or-external-find-file)
    (define-key map "b" 'project-switch-to-buffer)
    (define-key map "s" 'project-shell)
    (define-key map "D" 'project-find-dir)
    (define-key map "d" 'project-dired)
    (define-key map "v" 'project-vc-dir)
    (define-key map "c" 'project-compile)
    (define-key map "e" 'project-eshell)
    (define-key map "k" 'project-kill-buffers)
    (define-key map "p" 'project-switch-project)
    (define-key map "g" 'project-find-regexp)
    (define-key map "G" 'project-or-external-find-regexp)
    (define-key map "r" 'project-query-replace-regexp)
    (define-key map "x" 'project-execute-extended-command)
    (define-key map "o" 'project-any-command)
    (define-key map "\C-b" 'project-list-buffers)
    map)
  "Keymap for project commands.")

;;;###autoload (define-key ctl-x-map "p" project-prefix-map)

(defvar project-other-window-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" #'project-display-buffer)
    map)
  "Keymap for project commands that display buffers in other windows.")

(defvar project-other-frame-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" #'project-display-buffer-other-frame)
    map)
  "Keymap for project commands that display buffers in other frames.")

(defun project--other-place-command (action &optional map)
  (let* ((key (read-key-sequence-vector nil t))
         (place-cmd (lookup-key map key))
         (generic-cmd (lookup-key project-prefix-map key))
         (switch-to-buffer-obey-display-actions t)
         ;; Don't override if KEY is frame- or window-specifically mapped.
         ;; This condition precludes place-specific maps from inheriting
         ;; from project-prefix-map (bug#42210)
         (display-buffer-overriding-action (unless place-cmd action)))
    (if-let ((cmd (or place-cmd generic-cmd)))
        (call-interactively cmd)
      (user-error "%s is undefined" (key-description key)))))

(defun project--other-place-prefix (place &optional extra-keymap)
  (cl-assert (member place '(window frame tab)))
  (prefix-command-preserve-state)
  (let ((inhibit-message t)) (funcall (intern (format "other-%s-prefix" place))))
  (message "Display next project command buffer in a new %s..." place)
  ;; Should return exitfun from set-transient-map
  (set-transient-map (if extra-keymap
                         (make-composed-keymap project-prefix-map
                                               extra-keymap)
                       project-prefix-map)))

;;;###autoload
(defun project-other-window-command ()
  "Run project command, displaying resultant buffer in another window.

The following commands are available:

\\{project-prefix-map}
\\{project-other-window-map}"
  (interactive)
  (if (< emacs-major-version 30)
      (project--other-place-command '((display-buffer-pop-up-window)
                                      (inhibit-same-window . t))
                                    project-other-window-map)
    (project--other-place-prefix 'window project-other-window-map)))

;;;###autoload (define-key ctl-x-4-map "p" #'project-other-window-command)

;;;###autoload
(defun project-other-frame-command ()
  "Run project command, displaying resultant buffer in another frame.

The following commands are available:

\\{project-prefix-map}
\\{project-other-frame-map}"
  (interactive)
  (if (< emacs-major-version 30)
      (project--other-place-command '((display-buffer-pop-up-frame))
                                    project-other-frame-map)
    (project--other-place-prefix 'frame project-other-frame-map)))

;;;###autoload (define-key ctl-x-5-map "p" #'project-other-frame-command)

;;;###autoload
(defun project-other-tab-command ()
  "Run project command, displaying resultant buffer in a new tab.

The following commands are available:

\\{project-prefix-map}"
  (interactive)
  (if (< emacs-major-version 30)
      (project--other-place-command '((display-buffer-in-new-tab)))
    (project--other-place-prefix 'tab)))

;;;###autoload
(when (bound-and-true-p tab-prefix-map)
  (define-key tab-prefix-map "p" #'project-other-tab-command))

;;;###autoload
(defun other-project-prefix ()
  "\"Switch\" to another project before running an Emacs command.
Makes sure the next command invoked asks for the project to run it in."
  (interactive)
  (prefix-command-preserve-state)
  (letrec ((depth (minibuffer-depth))
           (echofun (lambda () "[switch-project]"))
           (around-fun
            (lambda (command &rest _args)
              (advice-remove this-command around-fun)
              (unless (or (eq this-command 'other-project-prefix)
                          (eq last-command-event help-char))
                (let ((root (funcall project-prompter)))
                  (if (or (string-prefix-p "project-"
                                           (symbol-name this-command))
                          (get this-command 'project-aware))
                      (let ((project-current-directory-override root))
                        (call-interactively command))
                    (let ((default-directory root))
                      (call-interactively command)))))))
           (prefun
            (lambda ()
              (unless (> (minibuffer-depth) depth)
                (remove-hook 'pre-command-hook prefun)
                (remove-hook 'prefix-command-echo-keystrokes-functions echofun)
                (when (and this-command (symbolp this-command))
                  (advice-add this-command :around around-fun))))))
    (add-hook 'pre-command-hook prefun)
    (add-hook 'prefix-command-echo-keystrokes-functions echofun)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map project-prefix-map)
      ;; Doesn't work ;-(
      ;; (define-key map (vector help-char)
      ;;             (lambda () (interactive) (help-form-show)))
      (set-transient-map map))
    (message (concat "Type " (project--keymap-prompt) " or any global key"))))

(declare-function grep-read-files "grep")
(declare-function xref--find-ignores-arguments "xref")

;;;###autoload
(defun project-find-regexp (regexp)
  "Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'."
  (interactive (list (project--read-regexp)))
  (require 'xref)
  (require 'grep)
  (let* ((caller-dir default-directory)
         (pr (project-most-recent-project))
         (default-directory (project-root pr))
         (files
          (if (not current-prefix-arg)
              (project-files pr)
            (let ((dir (read-directory-name "Base directory: "
                                            caller-dir nil t)))
	      (setq default-directory (file-name-as-directory dir))
              (project--files-in-directory
               dir nil (grep-read-files regexp))))))
    (xref-show-xrefs
     (apply-partially #'project--find-regexp-in-files regexp files)
     nil)))

(defun project--dir-ignores (project dir)
  (let ((root (project-root project)))
    (if (not (file-in-directory-p dir root))
        (project-ignores nil nil)       ;The defaults.
      (let ((ignores (project-ignores project root)))
        (if (file-equal-p root dir)
            ignores
          ;; FIXME: Update the "rooted" ignores to relate to DIR instead.
          (cl-delete-if (lambda (str) (string-prefix-p "./" str))
                        ignores))))))

;;;###autoload
(defun project-or-external-find-regexp (regexp)
  "Find all matches for REGEXP in the project roots or external roots."
  (interactive (list (project--read-regexp)))
  (require 'xref)
  (let* ((pr (project-most-recent-project))
         (default-directory (project-root pr))
         (files
          (project-files pr (cons
                             (project-root pr)
                             (project-external-roots pr)))))
    (xref-show-xrefs
     (apply-partially #'project--find-regexp-in-files regexp files)
     nil)))

(defun project--find-regexp-in-files (regexp files)
  (unless files
    (user-error "Empty file list"))
  (let ((xrefs (xref-matches-in-files regexp files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    xrefs))

(defvar project-regexp-history-variable 'grep-regexp-history)

(defun project--read-regexp ()
  (let ((result "")
        (sym (thing-at-point 'symbol t)))
    (while (string-empty-p
            (setq result (read-regexp
                          (project--annotate-prompt
                           (project-most-recent-project) "Find regexp")
                          (when sym (regexp-quote sym))
                          project-regexp-history-variable))))
    result))

(defun project--find-default-from (filename project)
  "Ensure FILENAME is in PROJECT.

Usually, just return FILENAME.  But if
`project-current-directory-override' is set, adjust it to be
relative to PROJECT instead.

This supports using a relative file name from the current buffer
when switching projects with `project-switch-project' and then
using a command like `project-find-file'."
  (if-let (filename-proj (and project-current-directory-override
                            (project-current nil default-directory)))
      ;; file-name-concat requires Emacs 28+
      (concat (file-name-as-directory (project-root project))
              (file-relative-name filename (project-root filename-proj)))
    filename))

;;;###autoload
(defun project-root-find-file (filename)
  "Edit file FILENAME.

Interactively, prompt for FILENAME, defaulting to the root directory of
the current project."
  (declare (interactive-only find-file))
  (interactive (list (read-file-name "Find file in root: "
                                     (project-root (project-current t)) nil
                                     (confirm-nonexistent-file-or-buffer))))
  (find-file filename t))

;;;###autoload
(defun project-find-file (&optional include-all)
  "Visit a file (with completion) in the current project.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".  If none, the current
buffer's file name is used.

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'."
  (interactive "P")
  (let* ((pr (project-most-recent-project))
         (root (project-root pr))
         (dirs (list root)))
    (project-find-file-in
     (delq nil (list (and buffer-file-name (project--find-default-from
                                            buffer-file-name pr))
                     (thing-at-point 'filename)))
     dirs pr include-all)))

;;;###autoload
(defun project-or-external-find-file (&optional include-all)
  "Visit a file (with completion) in the current project or external roots.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".  If none, the current
buffer's file name is used.

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'."
  (interactive "P")
  (defvar project-file-history-behavior)
  (let* ((pr (project-most-recent-project))
         (dirs (cons
                (project-root pr)
                (project-external-roots pr)))
         (project-file-history-behavior t))
    (project-find-file-in
     (delq nil (list (and buffer-file-name (project--find-default-from
                                            buffer-file-name pr))
                     (thing-at-point 'filename)))
     dirs pr include-all)))

(defcustom project-read-file-name-function #'project--read-file-cpd-relative
  "Function to call to read a file name from a list.
For the arguments list, see `project--read-file-cpd-relative'."
  :type '(choice (const :tag "Read with completion from relative names"
                        project--read-file-cpd-relative)
                 (const :tag "Read with completion from absolute names"
                        project--read-file-absolute)
                 (function :tag "Custom function" nil))
  :group 'project
  :version "27.1")

(defcustom project-file-history-behavior t
  "If `relativize', entries in `file-name-history' are adjusted.

History entries shown in `project-find-file', `project-find-dir',
(from `file-name-history') are adjusted to be relative to the
current project root, instead of the project which added those
paths.  This only affects history entries added by earlier calls
to `project-find-file' or `project-find-dir'.

This has the effect of sharing more history between projects."
  :type '(choice (const :tag "Default behavior" t)
                 (const :tag "Adjust to be relative to current" relativize))
  :group 'project
  :version "30.1")

(defun project--transplant-file-name (filename project)
  (when-let ((old-root (get-text-property 0 'project filename)))
    (expand-file-name
     (file-relative-name filename old-root)
     (project-root project))))

(defun project--read-file-cpd-relative (prompt
                                        all-files &optional predicate
                                        hist mb-default)
  "Read a file name, prompting with PROMPT.
ALL-FILES is a list of possible file name completions.

PREDICATE and HIST have the same meaning as in `completing-read'.

MB-DEFAULT is used as part of \"future history\", to be inserted
by the user at will."
  (let* ((common-parent-directory
          (let ((common-prefix (try-completion "" all-files)))
            (if (> (length common-prefix) 0)
                (file-name-directory common-prefix))))
         (cpd-length (length common-parent-directory))
         (prompt (if (zerop cpd-length)
                     prompt
                   (concat prompt (format " in %s" common-parent-directory))))
         (included-cpd (when (member common-parent-directory all-files)
                         (setq all-files
                               (delete common-parent-directory all-files))
                         t))
         (mb-default (mapcar (lambda (mb-default)
                               (if (and common-parent-directory
                                        mb-default
                                        (file-name-absolute-p mb-default))
                                   (file-relative-name
                                    mb-default common-parent-directory)
                                 mb-default))
                             (if (listp mb-default) mb-default (list mb-default))))
         (substrings (mapcar (lambda (s) (substring s cpd-length)) all-files))
         (_ (when included-cpd
              (setq substrings (cons "./" substrings))))
         (new-collection (project--file-completion-table substrings))
         (abs-cpd (expand-file-name common-parent-directory))
         (abs-cpd-length (length abs-cpd))
         (relname (cl-letf ((history-add-new-input nil)
			    ((symbol-value hist)
                             (mapcan
                              (lambda (s)
                                (setq s (expand-file-name s))
                                (and (string-prefix-p abs-cpd s)
                                     (not (eq abs-cpd-length (length s)))
                                     (list (substring s abs-cpd-length))))
                              (symbol-value hist))))
                    (project--completing-read-strict prompt
                                                     new-collection
                                                     predicate
                                                     hist mb-default)))
         (absname (expand-file-name relname common-parent-directory)))
    absname))

(defun project--read-file-absolute (prompt
                                    all-files &optional predicate
                                    hist mb-default)
  (project--completing-read-strict prompt
                                   (project--file-completion-table all-files)
                                   predicate
                                   hist mb-default))

(defun project--read-file-name ( project prompt
                                 all-files &optional predicate
                                 hist mb-default)
  "Call `project-read-file-name-function' with appropriate history.

Depending on `project-file-history-behavior', entries are made
project-relative where possible."
  (let ((file
         (cl-letf ((history-add-new-input nil)
                   ((symbol-value hist)
                    (if (eq project-file-history-behavior 'relativize)
                        (mapcar
                         (lambda (f)
                           (or (project--transplant-file-name f project) f))
                         (symbol-value hist))
                      (symbol-value hist))))
           (funcall project-read-file-name-function
                    prompt all-files predicate hist mb-default))))
    (when (and hist history-add-new-input)
      (add-to-history hist
                      (propertize file 'project (project-root project))))
    file))

(defun project-find-file-in (suggested-filename dirs project &optional include-all)
  "Complete a file name in DIRS in PROJECT and visit the result.

SUGGESTED-FILENAME is a file name, or part of it, which
is used as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files from DIRS, except for VCS
directories listed in `vc-directory-exclusion-list'."
  (let* ((vc-dirs-ignores (mapcar
                           (lambda (dir)
                             (concat dir "/"))
                           vc-directory-exclusion-list))
         (all-files (if include-all
                        (mapcan (lambda (dir)
                                  (project--files-in-directory dir vc-dirs-ignores))
                                dirs)
                      (project-files project dirs)))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (file (project--read-file-name
                project (project--annotate-prompt
                         (project-most-recent-project) "Find file")
                all-files nil 'file-name-history
                suggested-filename)))
    (if (string-empty-p file)
        (user-error "You didn't specify the file")
      (find-file file))))

(defun project--completing-read-strict (prompt
                                        collection &optional predicate
                                        hist mb-default)
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local minibuffer-default-add-function
                    (lambda ()
                      (let ((minibuffer-default mb-default))
                        (minibuffer-default-add-completions)))))
    (completing-read (format "%s: " prompt)
                     collection predicate 'confirm
                     nil
                     hist)))

;;;###autoload
(defun project-find-dir ()
  "Start Dired in a directory inside the current project.

The current buffer's `default-directory' is available as part of
\"future history\"."
  (interactive)
  (let* ((project (project-most-recent-project))
         (all-files (project-files project))
         (completion-ignore-case read-file-name-completion-ignore-case)
         ;; FIXME: This misses directories without any files directly
         ;; inside.  Consider DIRS-ONLY as an argument for
         ;; `project-files-filtered', and see
         ;; https://stackoverflow.com/a/50685235/615245 for possible
         ;; implementation.
         (all-dirs (mapcar #'file-name-directory all-files))
         (dir (project--read-file-name
               project (project--annotate-prompt project "Dired")
               ;; Some completion UIs show duplicates.
               (delete-dups all-dirs)
               nil 'file-name-history
               (and default-directory
                    (project--find-default-from default-directory project)))))
    (dired dir)))

;;;###autoload
(defun project-dired ()
  "Start Dired in the current project's root."
  (interactive)
  (dired (project-root (project-most-recent-project))))

;;;###autoload
(defun project-vc-dir ()
  "Run VC-Dir in the current project's root."
  (interactive)
  (vc-dir (project-root (project-most-recent-project))))

(declare-function comint-check-proc "comint")

;;;###autoload
(defun project-shell ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-root (project-most-recent-project)))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (if (comint-check-proc shell-buffer)
            (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
          (shell shell-buffer))
      (shell (generate-new-buffer-name default-project-shell-name)))))

;;;###autoload
(defun project-eshell ()
  "Start Eshell in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists."
  (interactive)
  (defvar eshell-buffer-name)
  (let* ((default-directory (project-root (project-most-recent-project)))
         (eshell-buffer-name (project-prefixed-buffer-name "eshell"))
         (eshell-buffer (get-buffer eshell-buffer-name)))
    (if (and eshell-buffer (not current-prefix-arg))
        (pop-to-buffer eshell-buffer (bound-and-true-p display-comint-buffer-action))
      (eshell t))))

;;;###autoload
(defun project-async-shell-command ()
  "Run `async-shell-command' in the current project's root directory."
  (declare (interactive-only async-shell-command))
  (interactive)
  (let ((default-directory (project-root (project-most-recent-project))))
    (call-interactively #'async-shell-command)))

;;;###autoload
(defun project-shell-command ()
  "Run `shell-command' in the current project's root directory."
  (declare (interactive-only shell-command))
  (interactive)
  (let ((default-directory (project-root (project-most-recent-project))))
    (call-interactively #'shell-command)))

(declare-function fileloop-continue "fileloop" ())

;;;###autoload
(defun project-search (regexp)
  "Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue]."
  (interactive "sSearch (regexp): ")
  (fileloop-initialize-search
   regexp (project-files (project-most-recent-project)) 'default)
  (fileloop-continue))

;;;###autoload
(defun project-query-replace-regexp (from to)
  "Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
At that prompt, the user must type a character saying what to do
with the match.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.
If you exit the `query-replace', you can later continue the
`query-replace' loop using the command \\[fileloop-continue]."
  (interactive
   (let ((query-replace-read-from-regexp-default 'find-tag-default-as-regexp))
     (pcase-let ((`(,from ,to)
                  (query-replace-read-args (project--annotate-prompt
                                            (project-most-recent-project)
                                            "Query replace (regexp)") t t)))
       (list from to))))
  (fileloop-initialize-replace
   from to
   (cl-delete-if-not (lambda (file)
                       "Filter out gpg keyfiles, etc."
                       (and (file-regular-p file)
                            (not (find-file-name-handler file 'insert-file-contents))))
                 (project-files (project-most-recent-project)))
   'default)
  (fileloop-continue))

(defvar compilation-read-command)
(declare-function compilation-read-command "compile")
(declare-function recompile "compile")

(defun project-prefixed-buffer-name (mode)
  (concat "*"
          (if-let ((proj (project-current nil)))
              (project-name proj)
            (file-name-nondirectory
             (directory-file-name default-directory)))
          "-"
          (downcase mode)
          "*"))

(defcustom project-compilation-buffer-name-function nil
  "Function to compute the name of a project compilation buffer.
If non-nil, it overrides `compilation-buffer-name-function' for
`project-compile'."
  :version "28.1"
  :group 'project
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Prefixed with project name"
                        project-prefixed-buffer-name)
                 (function :tag "Custom function")))

;;;###autoload
(defun project-compile ()
  "Compile in the project root.
Note rather than utilize `compile-history', compile.el reassigns
`compilation-directory' and `compilation-command' for the benefit
of the `recompile' command."
  (declare (interactive-only compile))
  (interactive)
  (when-let ((project (project-most-recent-project))
             (default-directory (project-root project))
             (compilation-buffer-name-function
              (or project-compilation-buffer-name-function
                  compilation-buffer-name-function))
             (command (let ((eval-command (eval compile-command)))
                        (if (or (null eval-command)
                                compilation-read-command
                                current-prefix-arg)
                            (read-shell-command
                             (project--annotate-prompt
                              project
                              (format "Run in %s: " default-directory))
                             eval-command
                             (if (equal (car compile-history) eval-command)
                                 '(compile-history . 1)
                               'compile-history))
	                  eval-command))))
    (compile command (consp current-prefix-arg))))

(defun project-recompile (&optional edit-command)
  "Run `recompile' with appropriate buffer."
  (declare (interactive-only recompile))
  (interactive "P")
  (let ((compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             ;; Should we error instead?  When there's no
             ;; project-specific naming, there is no point in using
             ;; this command.
             compilation-buffer-name-function)))
    (recompile edit-command)))

(defcustom project-ignore-buffer-conditions nil
  "List of conditions to filter the buffers to be switched to.
If any of these conditions are satisfied for a buffer in the
current project, `project-switch-to-buffer',
`project-display-buffer' and `project-display-buffer-other-frame'
ignore it.
See the doc string of `project-kill-buffer-conditions' for the
general form of conditions."
  :type '(repeat (choice regexp function symbol
                         (cons :tag "Major mode"
                               (const major-mode) symbol)
                         (cons :tag "Derived mode"
                               (const derived-mode) symbol)
                         (cons :tag "Negation"
                               (const not) sexp)
                         (cons :tag "Conjunction"
                               (const and) sexp)
                         (cons :tag "Disjunction"
                               (const or) sexp)))
  :version "29.1"
  :group 'project
  :package-version '(project . "0.8.2"))

(defun project--read-project-buffer ()
  (let* ((pr (project-most-recent-project))
         (buffers (funcall (project--list-buffers-closure pr)))
         (name (read-buffer
                (project--annotate-prompt pr "Switch to: ")
                (buffer-name (car buffers))
                'confirm
                (lambda (buffer)
                  "BUFFER is an entry (BUF-NAME . BUF-OBJ)"
                  (and (memq (cdr buffer) buffers)
                       (not (project--buffer-check
                             (cdr buffer)
                             project-ignore-buffer-conditions)))))))
    (if (bufferp name) (buffer-name name) name)))

;;;###autoload
(defun project-switch-to-buffer (buffer-or-name)
  "Display buffer BUFFER-OR-NAME in the selected window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical."
  (interactive (list (project--read-project-buffer)))
  (switch-to-buffer buffer-or-name))

;;;###autoload
(defun project-display-buffer (buffer-or-name)
  "Display BUFFER-OR-NAME in some window, without selecting it.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer' as a subroutine, which see
for how it is determined where the buffer will be displayed."
  (interactive (list (project--read-project-buffer)))
  (display-buffer buffer-or-name))

;;;###autoload
(defun project-display-buffer-other-frame (buffer-or-name)
  "Display BUFFER-OR-NAME preferably in another frame.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer-other-frame' as a subroutine,
which see for how it is determined where the buffer will be
displayed."
  (interactive (list (project--read-project-buffer)))
  (display-buffer-other-frame buffer-or-name))

(defun project--list-buffers-closure (&optional pr)
  (apply-partially
   (lambda (pr*)
     (seq-filter
      (lambda (buffer)
        (and (not (eq buffer (current-buffer)))
             (or (buffer-file-name buffer)
                 (and (not (eq (aref (buffer-name buffer) 0) ? ))
                      (not Buffer-menu-files-only)))))
      (project-buffers pr*)))
   (or pr (project-most-recent-project))))

(defcustom project-buffers-viewer 'project-list-buffers-buffer-menu
  "Function to use in `project-list-buffers' to render the list.

It should accept two arguments: PROJECT and FILES-ONLY.  The latter
means that only file-visiting buffers should be displayed."
  :group 'project
  :version "30.1"
  :type '(radio (function-item project-list-buffers-buffer-menu)
                (function-item project-list-buffers-ibuffer)))

;;;###autoload
(defun project-list-buffers (&optional arg)
  "Display a list of project buffers.
The list is displayed in a buffer named \"*Buffer List*\".

By default, all project buffers are listed except those whose names
start with a space (which are for internal use).  With prefix argument
ARG, show only buffers that are visiting files."
  (interactive "P")
  (display-buffer (list-buffers-noselect arg (project--list-buffers-closure))))

(defun project-list-buffers-buffer-menu (project &optional files-only)
  "List buffers for PROJECT in Buffer-menu mode.
If FILES-ONLY is non-nil, only show the file-visiting buffers."
  (let ((buffer-list-function
         (lambda ()
           (seq-filter
            (lambda (buffer)
              (let ((name (buffer-name buffer))
                    (file (buffer-file-name buffer)))
                (and (or Buffer-menu-show-internal
                         (not (string= (substring name 0 1) " "))
                         file)
                     (not (eq buffer (current-buffer)))
                     (or file (not Buffer-menu-files-only)))))
            (project-buffers project)))))
    (display-buffer
     (if (version< emacs-version "29.0.50")
         (let ((buf (list-buffers-noselect
                     files-only (with-current-buffer
                                    (get-buffer-create "*Buffer List*")
                                  (setq-local Buffer-menu-show-internal nil)
                                  (let ((Buffer-menu-files-only files-only))
                                    (funcall buffer-list-function))))))
           (with-current-buffer buf
             (setq-local revert-buffer-function
                         (lambda (&rest _ignored)
                           (list-buffers--refresh
                            (funcall buffer-list-function))
                           (tabulated-list-print t))))
           buf)
       (list-buffers-noselect files-only buffer-list-function)))))

(defun project-list-buffers-ibuffer (project &optional files-only)
  "List buffers for PROJECT using Ibuffer.
If FILES-ONLY is non-nil, only show the file-visiting buffers."
  (ibuffer t (format "*Ibuffer-%s*" (project-name project))
           `((predicate . (and
                           (or ,(not files-only) buffer-file-name)
                           (member (current-buffer)
                                   (project-buffers ',project)))))))

(defcustom project-kill-buffer-conditions
  '(buffer-file-name    ; All file-visiting buffers are included.
    ;; Most of temp and logging buffers (aside from hidden ones):
    (and
     (major-mode . fundamental-mode)
     "\\`[^ ]")
    ;; non-text buffer such as xref, occur, vc, log, ...
    (and (derived-mode . special-mode)
         (not (major-mode . help-mode))
         (not (derived-mode . gnus-mode)))
    (derived-mode . compilation-mode)
    (derived-mode . dired-mode)
    (derived-mode . diff-mode)
    (derived-mode . comint-mode)
    (derived-mode . eshell-mode)
    (derived-mode . change-log-mode))
  "List of conditions to kill buffers related to a project.
This list is used by `project-kill-buffers'.
Each condition is either:
- a regular expression, to match a buffer name,
- a predicate function that takes a buffer object as argument
  and returns non-nil if the buffer should be killed,
- a cons-cell, where the car describes how to interpret the cdr.
  The car can be one of the following:
  * `major-mode': the buffer is killed if the buffer's major
    mode is eq to the cons-cell's cdr.
  * `derived-mode': the buffer is killed if the buffer's major
    mode is derived from the major mode in the cons-cell's cdr.
  * `not': the cdr is interpreted as a negation of a condition.
  * `and': the cdr is a list of recursive conditions, that all have
    to be met.
  * `or': the cdr is a list of recursive conditions, of which at
    least one has to be met.

If any of these conditions are satisfied for a buffer in the
current project, it will be killed."
  :type '(repeat (choice regexp function symbol
                         (cons :tag "Major mode"
                               (const major-mode) symbol)
                         (cons :tag "Derived mode"
                               (const derived-mode) symbol)
                         (cons :tag "Negation"
                               (const not) sexp)
                         (cons :tag "Conjunction"
                               (const and) sexp)
                         (cons :tag "Disjunction"
                               (const or) sexp)))
  :version "29.1"
  :group 'project
  :package-version '(project . "0.8.2"))

(defcustom project-kill-buffers-display-buffer-list nil
  "Non-nil to display list of buffers to kill before killing project buffers.
Used by `project-kill-buffers'."
  :type 'boolean
  :version "29.1"
  :group 'project
  :package-version '(project . "0.8.2"))
;;;###autoload(put 'project-kill-buffers-display-buffer-list 'safe-local-variable #'booleanp)

;; FIXME: Could this be replaced by `buffer-match-p' in Emacs 29+?
(defun project--buffer-check (buf conditions)
  "Check if buffer BUF matches any element of the list CONDITIONS.
See `project-kill-buffer-conditions' or
`project-ignore-buffer-conditions' for more details on the
form of CONDITIONS."
  (catch 'match
    (dolist (c conditions)
      (when (cond
             ((stringp c)
              (string-match-p c (buffer-name buf)))
             ((functionp c)
              (funcall c buf))
             ((eq (car-safe c) 'major-mode)
              (eq (buffer-local-value 'major-mode buf)
                  (cdr c)))
             ((eq (car-safe c) 'derived-mode)
              (provided-mode-derived-p
               (buffer-local-value 'major-mode buf)
               (cdr c)))
             ((eq (car-safe c) 'not)
              (not (project--buffer-check buf (cdr c))))
             ((eq (car-safe c) 'or)
              (project--buffer-check buf (cdr c)))
             ((eq (car-safe c) 'and)
              (seq-every-p
               (apply-partially #'project--buffer-check
                                buf)
               (mapcar #'list (cdr c)))))
        (throw 'match t)))))

(defun project--buffers-to-kill (pr)
  "Return list of buffers in project PR to kill.
What buffers should or should not be killed is described
in `project-kill-buffer-conditions'."
  (let (bufs)
    (dolist (buf (project-buffers pr))
      (when (project--buffer-check buf project-kill-buffer-conditions)
        (push buf bufs)))
    bufs))

;;;###autoload
(defun project-kill-buffers (&optional no-confirm)
  "Kill the buffers belonging to the current project.
Two buffers belong to the same project if their project
instances, as reported by `project-current' in each buffer, are
identical.  Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed.  If NO-CONFIRM
is non-nil, the command will not ask the user for confirmation.
NO-CONFIRM is always nil when the command is invoked
interactively.

Also see the `project-kill-buffers-display-buffer-list' variable."
  (interactive)
  (let* ((pr (project-most-recent-project))
         (bufs (project--buffers-to-kill pr))
         (query-user (lambda ()
                       (yes-or-no-p
                        (project--annotate-prompt
                         pr
                         (format "Kill %d buffers in %s? "
                                 (length bufs)
                                 (project-root pr)))))))
    (cond (no-confirm
           (mapc #'kill-buffer bufs))
          ((null bufs)
           (message "No buffers to kill"))
          (project-kill-buffers-display-buffer-list
           (when
               (with-current-buffer-window
                   (get-buffer-create "*Buffer List*")
                   `(display-buffer--maybe-at-bottom
                     (dedicated . t)
                     (window-height . (fit-window-to-buffer))
                     (preserve-size . (nil . t))
                     (body-function
                      . ,#'(lambda (_window)
                             (list-buffers-noselect nil bufs))))
                   #'(lambda (window _value)
                       (with-selected-window window
                         (unwind-protect
                             (funcall query-user)
                           (when (window-live-p window)
                             (quit-restore-window window 'kill))))))
             (mapc #'kill-buffer bufs)))
          ((funcall query-user)
           (mapc #'kill-buffer bufs)))))


;;; Project list

(defcustom project-list-file (locate-user-emacs-file "projects")
  "File in which to save the list of known projects."
  :type 'file
  :version "28.1"
  :group 'project)

(defvar project--list (when (file-exists-p project-list-file)
                        (with-temp-buffer
                          (insert-file-contents project-list-file)
                          (cl-remove-duplicates
			   (seq-keep
			    (lambda (elem)
			      (when-let ((name (car-safe elem))
                                         (string-p (stringp name)))
			        (list (if (file-remote-p name) name
				        (abbreviate-file-name name)))))
			    (read (current-buffer)))
                           :test #'equal)))
  "List structure containing root directories of known projects.
With some possible metadata (to be decided).")

(defun project--write-project-list ()
  "Save `project--list' in `project-list-file'."
  (unless (bound-and-true-p ert--running-tests)
    (let ((filename project-list-file))
      (with-temp-buffer
        (insert ";;; -*- lisp-data -*-\n")
        (let ((print-length nil)
              (print-level nil))
          (pp (mapcar (lambda (elem)
                        (let ((name (car elem)))
                          (list (if (file-remote-p name) name
                                  (expand-file-name name)))))
                      project--list)
              (current-buffer)))
        (write-region nil nil filename nil 'silent)))))

(defsubst project--most-recent-project ()
  (or (project-current)
      (when-let ((mru (caar project--list)))
        (project--find-in-directory mru))))

;;;###autoload
(defun project-most-recent-project ()
  (let ((pr (or (project--most-recent-project)
                (project-get-project))))
    (prog1 pr (project-remember-project pr))))

;;;###autoload
(defun project-remember-project (pr &optional _no-write)
  "Add project PR to the front of the project list.
Save the result in `project-list-file' if the list of projects
has changed, and NO-WRITE is nil."
  (let* ((dir (abbreviate-file-name (project-root pr)))
         (extant (cl-find-if (lambda (entry) (equal dir (car entry)))
                             project--list)))
    (when (eq (current-buffer) (window-buffer))
      (setq project--list (delq extant project--list))
      (push (list dir) project--list))
    ;; Decided against writing to disk to update most recent project.
    (unless extant
      (project--write-project-list))))

(defun project--remove-from-project-list (project-root confirm)
  "Remove directory PROJECT-ROOT of a missing project from the project list.
If the directory was in the list before the removal, save the
result in `project-list-file'.  Announce the project's removal
from the list using REPORT-MESSAGE, which is a format string
passed to `message' as its first argument."
  (if-let ((ent (assoc (abbreviate-file-name project-root) project--list))
           (killed-p (or (not (file-exists-p project-root))
                         (with-temp-buffer
                           (let* ((default-directory project-root)
                                  (to-delete (project-current)))
                             (project-kill-buffers (not confirm))
                             (cl-some #'buffer-live-p
                                      (project-buffers to-delete)))))))
      (progn
        (setq project--list (delq ent project--list))
        (message "Removed project %s" project-root)
        (project--write-project-list))
    (message "Kept project %s" project-root)))

;;;###autoload
(defun project-forget-project (project-root &optional confirm)
  "Remove directory PROJECT-ROOT from the project list.
PROJECT-ROOT is the root directory of a known project listed in
the project list.  To prevent `project-buffers' from immediately
reinserting the project, kill open buffers associated
with PROJECT-ROOT unless CONFIRM directs otherwise."
  (interactive (list (funcall project-prompter) t))
  (project--remove-from-project-list project-root confirm))

(defun project--prev-buffer (predicate)
  "Return previous buffer satisfying PREDICATE which takes buffer."
  (let* ((switch-to-prev-buffer-skip
          (lambda (_window buffer _bury-or-kill)
            "Skip plus not is a double negative."
            (not (funcall predicate buffer))))
         (prev-buffer (save-window-excursion (switch-to-prev-buffer))))
    ;; `switch-to-prev-buffer' won't return nil if predicate cannot be met.
    ;; Thus, this end-run predicate check.
    (when (funcall predicate prev-buffer)
      prev-buffer)))

(defun project-prompt-project-dir ()
  "Prompt the user for a directory that is one of the known project roots.
It's also possible to enter an arbitrary directory not in the list."
  (let* (dir
         (from (project--most-recent-project))
         (dir-choice "... (choose a dir)")
         (choices (project--file-completion-table
                   (append project--list `(,dir-choice))))
         (default (when-let ((prev-buffer
                              (project--prev-buffer
                               (lambda (buffer)
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (when-let ((project (project-current)))
                                       (not (equal from project)))))))))
                    (with-current-buffer prev-buffer
                      (project-root (project-current))))))
    (while (string-empty-p
            ;; Even under require-match, `completing-read' allows RET
            ;; to yield an empty string.
            (setq dir (completing-read
                       (project--annotate-prompt
                        from
                        (format "Select project%s: "
                                (if default
                                    (format " (default %s)" default)
                                  "")))
                       choices nil t nil nil default))))
    (if (equal dir dir-choice)
        (read-directory-name (project--annotate-prompt from "Select directory: ")
                             default-directory nil t)
      dir)))

(defun project-prompt-project-name ()
  "Prompt the user for a project, by name, that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
  (let* ((dir-choice "... (choose a dir)")
         (choices
          (let (ret)
            (dolist (dir (project-known-project-roots))
              ;; we filter out directories that no longer map to a project,
              ;; since they don't have a clean project-name.
              (if-let (proj (project--find-in-directory dir))
                  (push (cons (project-name proj) proj) ret)))
            ret))
         ;; XXX: Just using this for the category (for the substring
         ;; completion style).
         (table (project--file-completion-table
                 (reverse (cons dir-choice choices))))
         (pr-name ""))
    (while (equal pr-name "")
      ;; If the user simply pressed RET, do this again until they don't.
      (setq pr-name (completing-read "Select project: " table nil t)))
    (if (equal pr-name dir-choice)
        (read-directory-name "Select directory: " default-directory nil t)
      (let ((proj (assoc pr-name choices)))
        (if (stringp proj) proj (project-root (cdr proj)))))))

;;;###autoload
(defun project-known-project-roots ()
  "Return the list of root directories of all known projects."
  (mapcar #'car project--list))

;;;###autoload
(defun project-execute-extended-command ()
  "Execute an extended command in project root."
  (declare (interactive-only command-execute))
  (interactive)
  (let ((default-directory (project-root (project-most-recent-project))))
    (call-interactively #'execute-extended-command)))

;;;###autoload
(defun project-any-command (&optional overriding-map prompt-format)
  "Run the next command in the current project.

If the command name starts with `project-', or its symbol has
property `project-aware', it gets passed the project to use
with the variable `project-current-directory-override'.
Otherwise, `default-directory' is temporarily set to the current
project's root.

If OVERRIDING-MAP is non-nil, it will be used as
`overriding-terminal-local-map' to provide shorter bindings
from that map which will take priority over the global ones."
  (interactive)
  (let* ((pr (project-current t))
         (prompt-format (or prompt-format "[execute in %s]:"))
         (command (let ((overriding-terminal-local-map overriding-map))
                    (key-binding (read-key-sequence
                                  (format prompt-format (project-root pr)))
                                 t)))
         (root (project-root pr)))
    (when command
      (if (when (symbolp command)
            (or (string-prefix-p "project-" (symbol-name command))
                (get command 'project-aware)))
          (let ((project-current-directory-override root))
            (call-interactively command))
        (let ((default-directory root))
          (call-interactively command))))))

;;;###autoload
(defun project-prefix-or-any-command ()
  "Run the next command in the current project.
Works like `project-any-command', but also mixes in the shorter
bindings from `project-prefix-map'."
  (interactive)
  (project-any-command project-prefix-map "[execute in %s]:"))

(defun project-remember-projects-under (dir &optional recursive)
  "Index all projects below a directory DIR.
If RECURSIVE is non-nil, recurse into all subdirectories to find
more projects.  After finishing, a message is printed summarizing
the progress.  The function returns the number of detected
projects."
  (interactive "DDirectory: \nP")
  (let ((dirs (if recursive
                  (directory-files-recursively dir "" t)
                (directory-files dir t)))
        (known (make-hash-table :size (* 2 (length project--list))
                                :test #'equal))
        (count 0))
    (dolist (project (mapcar #'car project--list))
      (puthash project t known))
    (dolist (subdir dirs)
      (when-let (((file-directory-p subdir))
                 (project (project--find-in-directory subdir))
                 (project-root (project-root project))
                 ((not (gethash project-root known))))
        (project-remember-project project t)
        (puthash project-root t known)
        (message "Found %s..." project-root)
        (setq count (1+ count))))
    (if (zerop count)
        (message "No projects were found")
      (project--write-project-list)
      (message "%d project%s were found"
               count (if (= count 1) "" "s")))
    count))

(defun project-forget-zombie-projects ()
  "Forget all known projects that don't exist any more."
  (interactive)
  (dolist (proj (project-known-project-roots))
    (unless (file-exists-p proj)
      (project-forget-project proj))))

(defun project-forget-projects-under (dir &optional recursive)
  "Forget all known projects below a directory DIR.
If RECURSIVE is non-nil, recurse into all subdirectories to
remove all known projects.  After finishing, a message is printed
summarizing the progress.  The function returns the number of
forgotten projects."
  (interactive "DDirectory: \nP")
  (let ((count 0))
    (if recursive
        (dolist (proj (project-known-project-roots))
          (when (file-in-directory-p proj dir)
            (project-forget-project proj)
            (setq count (1+ count))))
      (dolist (proj (project-known-project-roots))
        (when (file-equal-p (file-name-directory proj) dir)
          (project-forget-project proj)
          (setq count (1+ count)))))
    (if (zerop count)
        (message "No projects were forgotten")
      (project--write-project-list)
      (message "%d project%s were forgotten"
               count (if (= count 1) "" "s")))
    count))


;;; Project switching

(defcustom project-switch-commands
  '((project-find-file "Find file")
    (project-find-regexp "Find regexp")
    (project-dired "Dired")
    (project-vc-dir "VC-Dir")
    (project-eshell "Eshell")
    (project-any-command "Other"))
  "Alist mapping commands to descriptions.
Used by `project-switch-project' to construct a dispatch menu of
commands available upon \"switching\" to another project.

Each element is of the form (COMMAND LABEL &optional KEY) where
COMMAND is the command to run when KEY is pressed.  LABEL is used
to distinguish the menu entries in the dispatch menu.  If KEY is
absent, COMMAND must be bound in `project-prefix-map', and the
key is looked up in that map.

The value can also be a symbol, the name of the command to be
invoked immediately without any dispatch menu."
  :version "28.1"
  :group 'project
  :package-version '(project . "0.6.0")
  :type '(choice
          (repeat :tag "Commands menu"
           (list
            (symbol :tag "Command")
            (string :tag "Label")
            (choice :tag "Key to press"
                    (const :tag "Infer from the keymap" nil)
                    (character :tag "Explicit key"))))
          (const :tag "Use both short keys and global bindings"
                 project-prefix-or-any-command)
          (symbol :tag "Custom command")))

(defcustom project-switch-use-entire-map nil
  "Whether `project-switch-project' will use the entire `project-prefix-map'.
If nil, `project-switch-project' will only recognize commands
listed in `project-switch-commands', and will signal an error
when other commands are invoked.  If this is non-nil, all the
keys in `project-prefix-map' are valid even if they aren't
listed in the dispatch menu produced from `project-switch-commands'."
  :type 'boolean
  :group 'project
  :version "28.1")

(defcustom project-key-prompt-style (if (facep 'help-key-binding)
                                        t
                                      'brackets)
  "Which presentation to use when asking to choose a command by key.

When `brackets', use text brackets and `bold' for the character.
Otherwise, use the face `help-key-binding' in the prompt."
  :type '(choice (const :tag "Using help-key-binding face" t)
                 (const :tag "Using bold face and brackets" brackets))
  :group 'project
  :version "30.1")

(defun project--keymap-prompt ()
  "Return a prompt for the project switching using the prefix map."
  (let (keys)
    (map-keymap
     (lambda (evt _)
       (when (characterp evt) (push evt keys)))
     project-prefix-map)
    (mapconcat (lambda (key) (help-key-description (string key) nil)) keys " ")))

(defun project--menu-prompt ()
  "Return a prompt for the project switching dispatch menu."
  (mapconcat
   (pcase-lambda (`(,cmd ,label ,key))
     (when (characterp cmd) ; Old format, apparently user-customized.
       (let ((tmp cmd))
         ;; TODO: Add a deprecation warning, probably.
         (setq cmd key
               key tmp)))
     (let ((key (if key
                    (vector key)
                  (where-is-internal cmd (list project-prefix-map) t))))
       (if (not (eq project-key-prompt-style 'brackets))
           (format "%s %s"
                   (propertize (key-description key) 'face 'help-key-binding)
                   label)
         (format "[%s] %s"
                 (propertize (key-description key) 'face 'bold)
                 label))))
   project-switch-commands
   "  "))

(defun project--switch-project-command ()
  (let* ((commands-menu
          (mapcar
           (lambda (row)
             (if (characterp (car row))
                 ;; Deprecated format.
                 ;; XXX: Add a warning about it?
                 (reverse row)
               row))
           project-switch-commands))
         (commands-map
          (let ((temp-map (make-sparse-keymap)))
            (set-keymap-parent temp-map project-prefix-map)
            (dolist (row commands-menu temp-map)
              (when-let ((cmd (nth 0 row))
                         (keychar (nth 2 row)))
                (define-key temp-map (vector keychar) cmd)))))
         command
         choice)
    (while (not command)
      (let* ((overriding-local-map commands-map)
             (prompt (if project-switch-use-entire-map
                         (project--keymap-prompt)
                       (project--menu-prompt))))
        (when choice
          (setq prompt (concat prompt
                               (format " %s: %s"
                                       (propertize "Unrecognized input"
                                                   'face 'warning)
                                       (help-key-description choice nil)))))
        (setq choice (read-key-sequence (concat "Choose: " prompt)))
        (when (setq command (lookup-key commands-map choice))
          (when (numberp command) (setq command nil))
          (unless (or project-switch-use-entire-map
                      (assq command commands-menu))
            (setq command nil)))
        (let ((global-command (lookup-key (current-global-map) choice)))
          (when (memq global-command
                      '(keyboard-quit keyboard-escape-quit))
            (call-interactively global-command)))))
    (message nil)
    command))

;;;###autoload
(defun project-switch-project (dir)
  "Switch to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (if-let ((pr (let ((default-directory dir))
                 (project-current)))
           (mru (cl-find-if #'buffer-file-name (project-buffers pr))))
      (project-switch-to-buffer mru)
    (let ((command (if (symbolp project-switch-commands)
                       project-switch-commands
                     (project--switch-project-command))))
      (let ((project-current-directory-override dir))
        (call-interactively command)))))

;;;###autoload
(defun project-uniquify-dirname-transform (dirname)
  "Uniquify name of directory DIRNAME using `project-name', if in a project.

If you set `uniquify-dirname-transform' to this function,
slash-separated components from `project-name' will be appended to
the buffer's directory name when buffers from two different projects
would otherwise have the same name."
  (if-let (proj (project-current nil dirname))
      (let ((root (project-root proj)))
        (expand-file-name
         (file-name-concat
          (file-name-directory root)
          (project-name proj)
          (file-relative-name dirname root))))
    dirname))

;;; Project mode-line

;;;###autoload
(defcustom project-mode-line nil
  "Whether to show current project name and Project menu on the mode line.
This feature requires the presence of the following item in
`mode-line-format': `(project-mode-line project-mode-line-format)'; it
is part of the default mode line beginning with Emacs 30."
  :type 'boolean
  :group 'project
  :version "30.1")

(defvar project-menu-entry
  `(menu-item "Project" ,(bound-and-true-p menu-bar-project-menu)))

(defvar project-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] project-menu-entry)
    map))

(defvar project-mode-line-face nil
  "Face name to use for the project name on the mode line.")

(defvar project-mode-line-format '(:eval (project-mode-line-format)))
(put 'project-mode-line-format 'risky-local-variable t)

(defun project-mode-line-format ()
  "Compose the project mode-line."
  (when-let ((project (project-current)))
    ;; Preserve the global value of 'last-coding-system-used'
    ;; that 'write-region' needs to set for 'basic-save-buffer',
    ;; but updating the mode line might occur at the same time
    ;; during saving the buffer and 'project-name' can change
    ;; 'last-coding-system-used' when reading the project name
    ;; from .dir-locals.el also enables flyspell-mode (bug#66825).
    (let ((last-coding-system-used last-coding-system-used))
      (concat
       " "
       (propertize
        (project-name project)
        'face project-mode-line-face
        'mouse-face 'mode-line-highlight
        'help-echo "mouse-1: Project menu"
        'local-map project-mode-line-map)))))

(provide 'project)
;;; project.el ends here
