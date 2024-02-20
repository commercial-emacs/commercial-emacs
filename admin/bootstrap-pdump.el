;;; bootstrap-pdump.el --- load up standardly loaded Lisp files for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1986, 1992, 1994, 2001-2024 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;; Have an uninitialized emacs load a bunch of shit then
;; `dump-emacs-portable' an image (a .pdmp file) so that subsequent
;; emacs invocations conceive fully formed.
;;
;; A non-nil `pdumper--pure-pool' is the sole differentiator of
;; the final pdump run from the initial bootstrap run.

;;; Code:

;; At genesis we can only use C primitives.

(let ((subdirs '("lisp/emacs-lisp"
                 "lisp/progmodes"
                 "lisp/language"
                 "lisp/international"
                 "lisp/textmodes"
                 "lisp/vc"
                 "admin"))
      (parent-dir (file-name-directory (car load-path))))
  ;; Without benefit of startup.el's command line processing,
  ;; we brutally hardcode load-path for the pre-dump.
  (setq load-path (nconc (mapcar (function
                                  (lambda (subdir)
                                    (expand-file-name subdir parent-dir)))
                                 subdirs)
                         load-path)))

;; For the post-dump, the load-path is hacked together by
;; `update-subdirs`, a `make` target dating back to 1997.
(message "Using load-path %s" load-path)

;; native comp + interpreted elisp = more stack
(setq max-lisp-eval-depth (max max-lisp-eval-depth 3400))

;; avoid huge char-tables to reduce dump size
(setq inhibit-load-charset-map t)

;; We don't want to have any undo records in the dumped Emacs.
(set-buffer "*scratch*")
(setq buffer-undo-list t)

;; Bootstrap remains a game of "disable shit until it works."
(setq native-comp-disable-subr-trampolines t)

(load "emacs-lisp/debug-early")
(load "emacs-lisp/byte-run")
(load "emacs-lisp/backquote")
(load "subr")
(load "keymap")

;; subr.el defines after-load-functions and add-hook
(add-hook 'after-load-functions (lambda (_) (garbage-collect)))

(load "version")

(load "widget")
(load "custom")
(load "emacs-lisp/map-ynp")
(load "international/mule")
(load "international/mule-conf")
(load "env")
(load "format")
(load "bindings")
(load "window")  ; Needed here for `replace-buffer-in-windows'.
;; We are now capable of resizing the mini-windows, so give the
;; variable its advertised default value (it starts as nil, see
;; xdisp.c).
(setq resize-mini-windows 'grow-only)
(setq load-source-file-function #'load-with-code-conversion)
(load "files")

;; Load-time macro-expansion can only take effect after setting
;; load-source-file-function because of where it is called in lread.c.
(load "emacs-lisp/macroexp")
(if (and (not (byte-code-function-p (symbol-function 'macroexpand-all)))
         (not (subr-native-elisp-p (symbol-function 'macroexpand-all))))
    (progn
      ;; Since loaddefs is not yet loaded, macroexp's uses of pcase will simply
      ;; fail until pcase is explicitly loaded.  This also means that we have to
      ;; disable eager macro-expansion while loading pcase.
      (let ((macroexp--pending-eager-loads '(skip))) (load "emacs-lisp/pcase"))
      ;; Re-load macroexp so as to eagerly macro-expand its uses of pcase.
      (let ((max-lisp-eval-depth (* 2 max-lisp-eval-depth)))
        (load "emacs-lisp/macroexp"))))

(load "cus-face")
(load "faces")  ; after here, `defface' may be used.

;; Two poorly named files defining autoloaded functions.
(if pdumper--pure-pool
    (load "loaddefs")
  (load "ldefs-boot.el"))

(let ((new (make-hash-table :test #'equal)))
  ;; Now that loaddefs has populated definition-prefixes, purify its contents.
  (maphash (lambda (k v) (puthash (purify-if-dumping k) (purify-if-dumping v) new))
           definition-prefixes)
  (setq definition-prefixes new))

(load "button")                  ;After loaddefs, because of define-minor-mode!
(load "emacs-lisp/cl-preloaded")
(load "emacs-lisp/oclosure")          ;Used by cl-generic
(load "obarray")        ;abbrev.el is implemented in terms of obarrays.
(load "abbrev")         ;lisp-mode.el and simple.el use define-abbrev-table.

(load "help")

(load "jka-cmpr-hook")
(load "epa-hook")
;; Any Emacs Lisp source file (*.el) loaded here after can contain
;; multilingual text.
(load "international/mule-cmds")
(load "case-table")
;; This file doesn't exist when building a development version of Emacs
;; from the repository.  It is generated just after temacs is built.
(load "international/charprop.el" t)
(load "international/characters")
(load "international/cp51932")
(load "international/eucjp-ms")

(load "composite")
(load "indent")
(load "emacs-lisp/cl-generic")
(load "simple")
(load "emacs-lisp/seq")
(load "emacs-lisp/nadvice")
(load "minibuffer") ; Needs cl-generic, seq (and define-minor-mode).
(load "frame")
(load "startup")
(load "term/tty-colors")
(load "font-core")
(load "emacs-lisp/syntax")
(load "font-lock")
(load "jit-lock")

(load "mouse")
(if (boundp 'x-toolkit-scroll-bars)
    (load "scroll-bar"))
(load "select")
(load "emacs-lisp/timer")
(load "emacs-lisp/easymenu")
(load "isearch")
(load "rfn-eshadow")

(load "menu-bar")
(load "tab-bar")
(load "emacs-lisp/lisp")
(load "textmodes/page")
(load "register")
(load "textmodes/paragraphs")
(load "progmodes/prog-mode")
(load "emacs-lisp/lisp-mode")
(load "textmodes/text-mode")
(load "textmodes/fill")
(load "newcomment")

(load "replace")
(load "emacs-lisp/tabulated-list")
(load "buff-menu")

(if (fboundp 'x-create-frame)
    (progn
      (load "fringe")
      ;; Needed by `imagemagick-register-types'
      (load "emacs-lisp/regexp-opt")
      (load "image")
      (load "international/fontset")
      (load "dnd")
      (load "tool-bar")))

(if (featurep 'dynamic-setting)
    (load "dynamic-setting"))

(if (featurep 'x)
    (progn
      (load "x-dnd")
      (load "term/common-win")
      (load "term/x-win")))

(if (featurep 'haiku)
    (progn
      (load "term/common-win")
      (load "term/haiku-win")))

(if (or (eq system-type 'windows-nt)
        (featurep 'w32))
    (progn
      (load "term/common-win")
      (load "w32-vars")
      (load "term/w32-win")
      (load "disp-table")
      (when (eq system-type 'windows-nt)
        (load "w32-fns")
        (load "ls-lisp")
        (load "dos-w32"))))
(if (eq system-type 'ms-dos)
    (progn
      (load "dos-w32")
      (load "dos-fns")
      (load "dos-vars")
      ;; Don't load term/common-win: it isn't appropriate for the `pc'
      ;; ``window system'', which generally behaves like a terminal.
      (load "term/internal")
      (load "term/pc-win")
      (load "ls-lisp")
      (load "disp-table"))) ; needed to setup ibm-pc char set, see internal.el
(if (featurep 'ns)
    (progn
      (load "term/common-win")
      ;; Don't load ucs-normalize.el unless uni-*.el files were
      ;; already produced, because it needs uni-*.el files that might
      ;; not be built early enough during bootstrap.
      (when (featurep 'charprop)
        (load "international/mule-util")
        (load "international/ucs-normalize")
        (load "term/ns-win"))))
(if (featurep 'pgtk)
    (progn
      (load "pgtk-dnd")
      (load "term/common-win")
      (load "term/pgtk-win")))
(if (fboundp 'x-create-frame)
    ;; Do it after loading term/foo-win.el since the value of the
    ;; mouse-wheel-*-event vars depends on those files being loaded or not.
    (load "mwheel"))

;; progmodes/elisp-mode.el must be after w32-fns.el, to avoid this:
;;"Eager macro-expansion failure: (void-function w32-convert-standard-filename)"
;; which happens while processing 'elisp-flymake-byte-compile', when
;; elisp-mode.elc is outdated.
(load "progmodes/elisp-mode")

;; Preload some constants and floating point functions.
(load "emacs-lisp/float-sup")

(load "vc/vc-hooks")
(load "vc/ediff-hook")
(load "uniquify")
(load "electric")
(load "paren")

(load "emacs-lisp/shorthands")

(load "emacs-lisp/eldoc")
(load "emacs-lisp/cconv")
(when (and (compiled-function-p (symbol-function 'cconv-fv))
           (compiled-function-p (symbol-function 'macroexpand-all)))
  (setq internal-make-interpreted-closure-function
        #'cconv-make-interpreted-closure))
(load "cus-start") ;Late to reduce customize-rogue (needs loaddefs.el anyway)
(unless (eq system-type 'ms-dos)
  (load "tooltip"))
(load "international/iso-transl") ; Binds Alt-[ and friends.

;; Used by `kill-buffer', for instance.
(load "emacs-lisp/rmc")

;; Load language-specific files.
(dolist (path load-path)
  (when (equal "language" (file-name-nondirectory path))
    (dolist (el-file (directory-files path nil "\\.el$"))
      (let ((lang (file-name-sans-extension el-file)))
        (unless (string-match-p "-util\\'" lang)
          (load (concat "language/" lang)))))))

;; This file doesn't exist when building a development version of Emacs
;; from the repository.  It is generated just after temacs is built.
(load "leim/leim-list.el" t)

(let ((lp load-path))
  (load "site-load" t)
  (unless (equal lp load-path)
    ;; We reset load-path after dumping.
    (message "Warning: load-path change from site-load has no effect")))

;; Actively check for advised functions during preload since:
;; - advices in Emacs's core are generally considered bad style;
;; - `Snarf-documentation' looses docstrings of primitives advised
;;   during preload (bug#66032#20).
(mapatoms
 (lambda (f)
   (and (advice--p (symbol-function f))
        ;; Don't make it an error because it's not serious enough and
        ;; it can be annoying during development.  Also there are still
        ;; circumstances where we use advice on preloaded functions.
        (message "Warning: Advice installed on preloaded function %S" f))))

;; Make sure default-directory is unibyte when dumping.  This is
;; because we cannot decode and encode it correctly (since the locale
;; environment is not, and should not be, set up).  default-directory
;; is used every time we call expand-file-name, which we do in every
;; file primitive.  So the only workable solution to support building
;; in non-ASCII directories is to manipulate unibyte strings in the
;; current locale's encoding.
(when (multibyte-string-p default-directory)
  (error "default-directory must be unibyte when dumping Emacs!"))

(unless (eq system-type 'ms-dos)
  (setq emacs-repository-version (ignore-errors (emacs-repository-get-version))
        emacs-repository-branch (ignore-errors (emacs-repository-get-branch))
        emacs-repository-get-tag (ignore-errors (emacs-repository-get-tag)))
  (let* ((base (concat "emacs-" emacs-version "."))
	 (versions (mapcar (lambda (name)
                             (string-to-number
                              (substring name (length base)
                                         (when (eq system-type 'windows-nt) -4))))
			   (file-name-all-completions base default-directory))))
    ;; Unless --dumping-overwrite, multiple binaries cumulate, each
    ;; distinguished by a "build number" suffix.
    (defconst emacs-build-number
      (if versions (1+ (apply #'max versions)) 1))))

(when pdumper--pure-pool
  (message "Finding pointers to doc strings...")
  (Snarf-documentation "DOC")
  (message "Finding pointers to doc strings...done"))

(let ((lp load-path))
  (load "site-init" t)
  (unless (equal lp load-path)
    ;; We reset load-path after dumping.
    (message "Warning: load-path change from site-init has no effect")))

(when (featurep 'native-compile)
  ;; Trampoline compilation without byte-compiled bytecomp.elc
  ;; (equivalently, a null pdumper--pure-pool) incurs dreaded eager
  ;; macroexpansion cycles (more bootstrap fake-it-til-you-make-it).
  (setq native-comp-disable-subr-trampolines (null pdumper--pure-pool)))

(setq current-load-list nil)
;; Avoid storing references to build directory in the binary.
(setq custom-current-group-alist nil)

(setq preloaded-file-list
      (delq
       nil
       (mapcar
        (lambda (file)
          (catch 'relative-name
            (dolist (path (sort (copy-sequence load-path)
                                (lambda (a b) (< (length a) (length b)))))
              (let ((rx (format "^%s\\(\\S-+\\)"
                                (regexp-quote
                                 (file-name-as-directory path)))))
                (save-match-data
                  (when (string-match rx file)
                    (throw 'relative-name (file-name-sans-extension
                                           (match-string 1 file)))))))))
        (mapcar #'car load-history))))

(when (boundp 'pdumper--doctor-load-history)
  (eval pdumper--doctor-load-history))

;; We keep the load-history data in PURE space.
;; Make sure that the spine of the list is not in pure space because it can
;; be destructively mutated in lread.c:build_load_history.
(setq load-history (mapcar #'purify-if-dumping load-history))
(set-buffer-modified-p nil)

(remove-hook 'after-load-functions (lambda (_) (garbage-collect)))
(setq inhibit-load-charset-map nil)

(clear-charset-maps)
(garbage-collect)

;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo "*scratch*")

(defvar comp-subr-arities-h)
(when (featurep 'native-compile)
  ;; Save the arity for all primitives so the compiler can always
  ;; retrieve it even in case of redefinition.
  (mapatoms (lambda (f)
              (when (subr-primitive-p (symbol-function f))
                (puthash f (func-arity f) comp-subr-arities-h))))
  ;; Set up the mechanism to allow inhibiting native-comp via
  ;; file-local variables.
  (defvar comp--no-native-compile (make-hash-table :test #'equal)))

(when pdumper--pure-pool
  (let ((strings 0)
        (vectors 0)
        (bytecodes 0)
        (conses 0)
        (others 0))
    (maphash (lambda (k v)
               (cond
                ((stringp k) (setq strings (1+ strings)))
                ((vectorp k) (setq vectors (1+ vectors)))
                ((consp k)   (setq conses  (1+ conses)))
                ((byte-code-function-p v) (setq bytecodes (1+ bytecodes)))
                (t           (setq others  (1+ others)))))
             pdumper--pure-pool)
    (message "Pure-hashed: %d strings, %d vectors, %d conses, %d bytecodes, %d others"
             strings vectors conses bytecodes others)))

(unless (garbage-collect)
  (setq pure-space-overflow t))

(message "Dumping to %s" (pdumping-output))
(condition-case nil
    (delete-file (pdumping-output))
  (file-error nil))

(let ((output-path (expand-file-name (pdumping-output) invocation-directory)))
  (condition-case nil
      (let ((name (format "emacs-%s.%d" emacs-version emacs-build-number))
            (exe (if (eq system-type 'windows-nt) ".exe" "")))
        (let (lexical-binding)
          (dump-emacs-portable output-path))
        (while (string-match "[^-+_.a-zA-Z0-9]+" name)
          (setq name (concat (downcase (substring name 0 (match-beginning 0)))
                             "-"
                             (substring name (match-end 0)))))
        (unless (member "--pdump-overwrite" command-line-args)
          (message "Adding name %s" (concat name exe))
          ;; When this runs on Windows, invocation-directory is not
          ;; necessarily the current directory.
          (add-name-to-file (expand-file-name (concat "emacs" exe)
                                              invocation-directory)
                            (expand-file-name (concat name exe)
                                              invocation-directory)
                            t)
          (when pdumper--pure-pool
            (message "Adding name %s" (concat name ".pdmp"))
            (add-name-to-file (expand-file-name "emacs.pdmp"
                                                invocation-directory)
                              (expand-file-name (concat name ".pdmp")
                                                invocation-directory)
                              t))))
    (error (ignore-errors (delete-file output-path)))))

(kill-emacs) ;crude

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; bootstrap-pdump.el ends here
