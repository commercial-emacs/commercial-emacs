;;; macro-problem.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: tools
;; Version: 2.0

;;; Code:

(require 'macro-aux)

; would fail if macro-aux did not come first
; in package--reload-previously-loaded
(macro-aux-new-func)

(defmacro macro-problem-1 ( &rest forms)
  "Description."
  `(progn ,(cadr (car forms))))


(defun macro-problem-func ()
  "Description."
  (list (macro-problem-1 '1 'b)
        (macro-aux-1 'a 'b)))

(defmacro macro-problem-3 (&rest _)
  "Description."
  10)

(defun macro-problem-10-and-90 ()
  "Description."
  (list (macro-problem-3 haha) (macro-aux-3 hehe)))

(provide 'macro-problem)
;;; macro-problem.el ends here
