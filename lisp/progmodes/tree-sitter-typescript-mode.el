;;; tree-sitter-typescript-mode.el -*- lexical-binding: t -*-

(require 'tree-sitter-prog-mode)

;;;###autoload
(define-derived-mode tree-sitter-typescript-mode tree-sitter-prog-mode "TS"
  "Have tree-sitter replace syntax-ppss.")

(provide 'tree-sitter-typescript-mode)

;;; tree-sitter-c-mode.el ends here
