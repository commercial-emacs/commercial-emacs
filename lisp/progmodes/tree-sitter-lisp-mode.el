;;; tree-sitter-lisp-mode.el --- Common Lisp mode -*- lexical-binding: t -*-

(require 'tree-sitter-prog-mode)

;;;###autoload
(define-derived-mode tree-sitter-lisp-mode tree-sitter-prog-mode "Lisp"
  "Have tree-sitter replace syntax-ppss.")

(provide 'tree-sitter-lisp-mode)

;;; tree-sitter-lisp-mode.el ends here
