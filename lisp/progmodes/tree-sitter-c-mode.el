;;; tree-sitter-c-mode.el --- Major mode for editing C files -*- lexical-binding: t -*-

(require 'tree-sitter-prog-mode)

(declare-function tree-sitter-node-child-by-field-name "tree-sitter.c")
(declare-function tree-sitter-node-end "tree-sitter.c")
(declare-function tree-sitter-node-start "tree-sitter.c")
(declare-function tree-sitter-node-type "tree-sitter.c")
(declare-function tree-sitter-root-node "tree-sitter.c")
(declare-function tree-sitter-node-equal "tree-sitter.c")
(declare-function tree-sitter-node-parent "tree-sitter.c")
(declare-function tree-sitter-node-at "tree-sitter.c")

(defun tree-sitter-c-forward-sexp (&optional arg)
  "Specialization of `tree-sitter-forward-sexp' for C.
For lack of a proper captures scm formally describing
what sexp is in C, hand-wave it to be compound_statement
or parenthesized_expression."
  (unless (fixnump arg) (setq arg 1))
  (let ((ntimes (abs arg)))
    (catch 'done
      (prog1 0
        (dotimes (i ntimes)
          (let* ((node (if (>= arg 0)
                           (tree-sitter-node-deepest (tree-sitter-node-at))
                         (backward-char)
                         (tree-sitter-node-shallowest
                          (let ((found (tree-sitter-node-at nil t)))
                            (while (and (not found) (>= (point) (point-min)))
                              (backward-char)
                              (setq found (tree-sitter-node-at nil t)))
                            found))))
                 (parent (when-let ((prospective (tree-sitter-node-parent node)))
                           (unless (tree-sitter-node-equal
                                    (tree-sitter-root-node) prospective)
                             prospective)))
                 (parent-type (tree-sitter-node-type parent))
                 (sexp-p (and (or (equal parent-type "parenthesized_expression")
                                  (equal parent-type "argument_list")
                                  (equal parent-type "compound_statement"))
                              (if (>= arg 0)
                                  (eq (point) (tree-sitter-node-start parent))
                                (eq (point) (1- (tree-sitter-node-end parent))))))
                 (c (if (>= arg 0)
                        (if sexp-p
                            (tree-sitter-node-end parent)
                          (tree-sitter-node-end node))
                      (if sexp-p
                          (tree-sitter-node-start parent)
                        (when-let ((not-root-p parent)
                                   (preceding (tree-sitter-node-preceding
                                               (tree-sitter-node-deepest node))))
                          (tree-sitter-node-start
                           (tree-sitter-node-deepest
                            (tree-sitter-node-at
                             (1- (tree-sitter-node-end preceding))))))))))
            (if c
                (goto-char c)
              (throw 'done (- ntimes i)))))))))

(defun tree-sitter-c-add-log-current-defun ()
  "Kind of insane."
  (when-let ((def-dec
              (save-excursion
                (unless (equal
                         "function_definition"
                         (tree-sitter-node-type
                          (tree-sitter-node-shallowest
                           (tree-sitter-node-at nil :precise))))
                  (beginning-of-defun))
                (tree-sitter-node-child-by-field-name
                 (tree-sitter-node-shallowest (tree-sitter-node-at nil :precise))
                 "declarator")))
             (dec-dec
              (tree-sitter-node-child-by-field-name def-dec "declarator")))
    (buffer-substring-no-properties (tree-sitter-node-start dec-dec)
                                    (tree-sitter-node-end dec-dec))))

;;;###autoload
(define-derived-mode tree-sitter-c-mode tree-sitter-prog-mode "C"
  "Have tree-sitter replace syntax-ppss."
  (setq-local forward-sexp-function #'tree-sitter-c-forward-sexp
              comment-start "/* "
              comment-end " */"
              comment-start-skip "\\(?://+\\|/\\*+\\)\\s *"
              add-log-current-defun-function #'tree-sitter-c-add-log-current-defun))

(provide 'tree-sitter-c-mode)

;;; tree-sitter-c-mode.el ends here
