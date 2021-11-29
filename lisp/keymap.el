;;; keymap.el --- Keymap functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; This library deals with the "new" keymap binding interface: The
;; only key syntax allowed by these functions is the `kbd' one.

;;; Code:



(defun keymap--check (key)
  "Signal an error if KEY doesn't have a valid syntax."
  (unless (key-valid-p key)
    (error "%S is not a valid key definition; see `key-valid-p'" key)))

(defun keymap--compile-check (&rest keys)
  (dolist (key keys)
    (when (or (vectorp key)
              (and (stringp key) (not (key-valid-p key))))
      (byte-compile-warn "Invalid `kbd' syntax: %S" key))))

(defun keymap-set (keymap key definition)
  "Set key sequence KEY to DEFINITION in KEYMAP.
KEY is a string that satisfies `key-valid-p'.

DEFINITION is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right) and
    STRING is the menu item name (which is used only if the containing
    keymap has been created with a menu name, see `make-keymap'),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)"
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  ;; If we're binding this key to another key, then parse that other
  ;; key, too.
  (when (stringp definition)
    (keymap--check definition)
    (setq definition (key-parse definition)))
  (define-key keymap (key-parse key) definition))

(defun keymap-global-set (key command)
  "Give KEY a global binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.

KEY is a string that satisfies `key-valid-p'.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set key globally: " nil t)))
     (list key
           (read-command (format "Set key %s to command: "
                                 (key-description key))))))
  (keymap-set (current-global-map) key command))

(defun keymap-local-set (key command)
  "Give KEY a local binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.

KEY is a string that satisfies `key-valid-p'.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (interactive "KSet key locally: \nCSet key %s locally to command: ")
  (let ((map (current-local-map)))
    (unless map
      (use-local-map (setq map (make-sparse-keymap))))
    (keymap-set map key command)))

(defun keymap-global-unset (key &optional remove)
  "Remove global binding of KEY (if any).
KEY is a string that satisfies `key-valid-p'.

If REMOVE (interactively, the prefix arg), remove the binding
instead of unsetting it.  See `keymap-unset' for details."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (interactive
   (list (key-description (read-key-sequence "Set key locally: "))
         current-prefix-arg))
  (keymap-unset (current-global-map) key remove))

(defun keymap-local-unset (key &optional remove)
  "Remove local binding of KEY (if any).
KEY is a string that satisfies `key-valid-p'.

If REMOVE (interactively, the prefix arg), remove the binding
instead of unsetting it.  See `keymap-unset' for details."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (interactive
   (list (key-description (read-key-sequence "Unset key locally: "))
         current-prefix-arg))
  (when (current-local-map)
    (keymap-unset (current-local-map) key remove)))

(defun keymap-unset (keymap key &optional remove)
  "Remove key sequence KEY from KEYMAP.
KEY is a string that satisfies `key-valid-p'.

If REMOVE, remove the binding instead of unsetting it.  This only
makes a difference when there's a parent keymap.  When unsetting
a key in a child map, it will still shadow the same key in the
parent keymap.  Removing the binding will allow the key in the
parent keymap to be used."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  (define-key keymap (key-parse key) nil remove))

(defun keymap-substitute (keymap olddef newdef &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF wherever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys that are defined as OLDDEF in OLDMAP.

If you don't specify OLDMAP, you can usually get the same results
in a cleaner way with command remapping, like this:
  (define-key KEYMAP [remap OLDDEF] NEWDEF)
\n(fn OLDDEF NEWDEF KEYMAP &optional OLDMAP)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (unless prefix
    (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (prefix1 (vconcat prefix [nil]))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(defun keymap-set-after (keymap key definition &optional after)
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `keymap-set' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t or omitted, the new binding goes at the end of the keymap.
AFTER should be a single event type--a symbol or a character, not a sequence.

Bindings are always added before any inherited map.

The order of bindings in a keymap matters only when it is used as
a menu, so this function is not useful for non-menu keymaps."
  (declare (indent defun)
           (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  (when after
    (keymap--check after))
  (define-key-after keymap (key-parse key) definition
    (and after (key-parse after))))

(defun key-parse (keys)
  "Convert KEYS to the internal Emacs key representation.
See `kbd' for a descripion of KEYS."
  (declare (pure t) (side-effect-free t))
  ;; A pure function is expected to preserve the match data.
  (save-match-data
    (let ((case-fold-search nil)
          (len (length keys)) ; We won't alter keys in the loop below.
          (pos 0)
          (res []))
      (while (and (< pos len)
                  (string-match "[^ \t\n\f]+" keys pos))
        (let* ((word-beg (match-beginning 0))
               (word-end (match-end 0))
               (word (substring keys word-beg len))
               (times 1)
               key)
          ;; Try to catch events of the form "<as df>".
          (if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
              (setq word (match-string 0 word)
                    pos (+ word-beg (match-end 0)))
            (setq word (substring keys word-beg word-end)
                  pos word-end))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
                      (progn
                        (setq word (concat (match-string 1 word)
                                           (match-string 3 word)))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key (list (intern word))))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" keys pos)))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (setq bits (+ bits
                                   (cdr
                                    (assq (aref word 0)
                                          '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                            (?H . ?\H-\^@) (?M . ?\M-\^@)
                                            (?s . ?\s-\^@) (?S . ?\S-\^@))))))
                     (setq prefix (+ prefix 2))
                     (setq word (substring word 2)))
                   (when (string-match "^\\^.$" word)
                     (setq bits (+ bits ?\C-\^@))
                     (setq prefix (1+ prefix))
                     (setq word (substring word 1)))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (let ((n 0))
                       (dolist (ch (cdr (string-to-list word)))
                         (setq n (+ (* n 8) ch -48)))
                       (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\^@) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (mapcar (lambda (x) (+ x bits))
                                            (append word nil))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\^@)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (dolist (_ (number-sequence 1 times))
              (setq res (vconcat res key))))))
      (if (and (>= (length res) 4)
               (eq (aref res 0) ?\C-x)
               (eq (aref res 1) ?\()
               (eq (aref res (- (length res) 2)) ?\C-x)
               (eq (aref res (- (length res) 1)) ?\)))
          (apply #'vector (let ((lres (append res nil)))
                            ;; Remove the first and last two elements.
                            (setq lres (cdr (cdr lres)))
                            (nreverse lres)
                            (setq lres (cdr (cdr lres)))
                            (nreverse lres)))
        res))))

(defun key-valid-p (keys)
  "Say whether KEYS is a valid `kbd' sequence.
A `kbd' sequence is a string consisting of one and more key
strokes.  The key strokes are separated by a space character.

Each key stroke is either a single character, or the name of an
event, surrounded by angle brackets.  In addition, any key stroke
may be preceded by one or more modifier keys.  Finally, a limited
number of characters have a special shorthand syntax.

Here's some example key sequences.

  \"f\"           (the key 'f')
  \"S o m\"       (a three key sequence of the keys 'S', 'o' and 'm')
  \"C-c o\"       (a two key sequence of the keys 'c' with the control modifier
                 and then the key 'o')
  \"H-<left>\"    (the key named \"left\" with the hyper modifier)
  \"M-RET\"       (the \"return\" key with a meta modifier)
  \"C-M-<space>\" (the \"space\" key with both the control and meta modifiers)

These are the characters that have shorthand syntax:
NUL, RET, TAB, LFD, ESC, SPC, DEL.

Modifiers have to be specified in this order:

   A-C-H-M-S-s

which is

   Alt-Control-Hyper-Meta-Shift-super"
  (declare (pure t) (side-effect-free t))
  (and
   (stringp keys)
   (string-match-p "\\`[^ ]+\\( [^ ]+\\)*\\'" keys)
   (save-match-data
     (catch 'exit
       (let ((prefixes
              "\\(A-\\)?\\(C-\\)?\\(H-\\)?\\(M-\\)?\\(S-\\)?\\(s-\\)?")
             (case-fold-search nil))
         (dolist (key (split-string keys " "))
           ;; Every key might have these modifiers, and they should be
           ;; in this order.
           (when (string-match (concat "\\`" prefixes) key)
             (setq key (substring key (match-end 0))))
           (unless (or (and (= (length key) 1)
                            ;; Don't accept control characters as keys.
                            (not (< (aref key 0) ?\s))
                            ;; Don't accept Meta'd characters as keys.
                            (or (multibyte-string-p key)
                                (not (<= 127 (aref key 0) 255))))
                       (and (string-match-p "\\`<[-_A-Za-z0-9]+>\\'" key)
                            ;; Don't allow <M-C-down>.
                            (= (progn
                                 (string-match
                                  (concat "\\`<" prefixes) key)
                                 (match-end 0))
                               1))
                       (string-match-p
                        "\\`\\(NUL\\|RET\\|TAB\\|LFD\\|ESC\\|SPC\\|DEL\\)\\'"
                        key))
             ;; Invalid.
             (throw 'exit nil)))
         t)))))

(defun key-translate (from to)
  "Translate character FROM to TO on the current terminal.
This function creates a `keyboard-translate-table' if necessary
and then modifies one entry in it.

Both KEY and TO are strings that satisfy `key-valid-p'."
  (declare (compiler-macro
            (lambda (form) (keymap--compile-check from to) form)))
  (keymap--check from)
  (keymap--check to)
  (or (char-table-p keyboard-translate-table)
      (setq keyboard-translate-table
	    (make-char-table 'keyboard-translate-table nil)))
  (aset keyboard-translate-table (key-parse from) (key-parse to)))

(defun keymap-lookup (keymap key &optional accept-default no-remap position)
  "Return the binding for command KEY.
KEY is a string that satisfies `key-valid-p'.

If KEYMAP is nil, look up in the current keymaps.  If non-nil, it
should either be a keymap or a list of keymaps, and only these
keymap(s) will be consulted.

The binding is probably a symbol with a function definition.

Normally, `keymap-lookup' ignores bindings for t, which act as
default bindings, used when nothing else in the keymap applies;
this makes it usable as a general function for probing keymaps.
However, if the optional second argument ACCEPT-DEFAULT is
non-nil, `keymap-lookup' does recognize the default bindings,
just as `read-key-sequence' does.

Like the normal command loop, `keymap-lookup' will remap the
command resulting from looking up KEY by looking up the command
in the current keymaps.  However, if the optional third argument
NO-REMAP is non-nil, `keymap-lookup' returns the unmapped
command.

If KEY is a key sequence initiated with the mouse, the used keymaps
will depend on the clicked mouse position with regard to the buffer
and possible local keymaps on strings.

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the lookup
occurs in the keymaps associated with it instead of KEY.  It can also
be a number or marker, in which case the keymap properties at the
specified buffer position instead of point are used."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  (when (and keymap (not position))
    (error "Can't pass in both keymap and position"))
  (if keymap
      (let ((value (lookup-key (key-parse key) keymap accept-default)))
        (when (and (not no-remap)
                   (symbolp value))
          (or (command-remapping value) value)))
    (key-binding (kbd key) accept-default no-remap position)))

(defun keymap-local-lookup (keys &optional accept-default)
  "Return the binding for command KEYS in current local keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this."
  (declare (compiler-macro (lambda (form) (keymap--compile-check keys) form)))
  (when-let ((map (current-local-map)))
    (keymap-lookup map keys accept-default)))

(defun keymap-global-lookup (keys &optional accept-default message)
  "Return the binding for command KEYS in current global keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.
This function's return values are the same as those of `keymap-lookup'
\(which see).

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this.

If MESSAGE (and interactively), message the result."
  (declare (compiler-macro (lambda (form) (keymap--compile-check keys) form)))
  (interactive
   (list (key-description (read-key-sequence "Look up key in global keymap: "))
         nil t))
  (let ((def (keymap-lookup (current-global-map) keys accept-default)))
    (when message
      (message "%s is bound to %s globally" keys def))
    def))

(provide 'keymap)

;;; keymap.el ends here
