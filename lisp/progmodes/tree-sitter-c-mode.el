;;; tree-sitter-c-mode.el --- Major mode for editing C files -*- lexical-binding: t -*-

(require 'tree-sitter-prog-mode)

;;;###autoload
(define-derived-mode tree-sitter-c-mode tree-sitter-prog-mode "C"
  "Have tree-sitter replace syntax-ppss.")

(provide 'tree-sitter-c-mode)

;;; tree-sitter-c-mode.el ends here
