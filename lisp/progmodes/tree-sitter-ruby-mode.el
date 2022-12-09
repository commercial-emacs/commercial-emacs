;;; tree-sitter-ruby-mode.el --- Major mode for editing Ruby files -*- lexical-binding: t -*-

(require 'tree-sitter-prog-mode)

;;;###autoload
(define-derived-mode tree-sitter-ruby-mode tree-sitter-prog-mode "Ruby"
  "Have tree-sitter replace syntax-ppss."
  (setq-local beginning-of-defun-function (apply-partially #'tree-sitter-beginning-of-defun "method")
              end-of-defun-function (apply-partially #'tree-sitter-end-of-defun "method")))

(provide 'tree-sitter-ruby-mode)

;;; tree-sitter-ruby-mode.el ends here
