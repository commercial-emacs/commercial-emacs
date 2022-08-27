;;; tree-sitter-elisp-mode.el --- Emacs Lisp mode -*- lexical-binding: t -*-

(require 'tree-sitter-prog-mode)

;;;###autoload
(define-derived-mode tree-sitter-elisp-mode tree-sitter-prog-mode "ELisp"
  "Have tree-sitter replace syntax-ppss.")

(provide 'tree-sitter-elisp-mode)

;;; tree-sitter-elisp-mode.el ends here
