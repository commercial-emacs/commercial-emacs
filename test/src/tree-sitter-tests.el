;;; tree-sitter-tests.el --- tests for src/tree-sitter.c         -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert)
(require 'tree-sitter)

(ert-deftest tree-sitter-basic-parsing ()
  "Test basic parsing routines."
  (require 'tree-sitter-json)
  (with-temp-buffer
    (let ((parser (tree-sitter-create-parser
                   (current-buffer) (tree-sitter-json))))
      (should
       (eq parser (car tree-sitter-parser-list)))
      (should
       (equal (tree-sitter-node-string
               (tree-sitter-parser-root-node parser))
              "(ERROR)"))

      (insert "[1,2,3]")
      (should
       (equal (tree-sitter-node-string
               (tree-sitter-parser-root-node parser))
              "(document (array (number) (number) (number)))"))

      (goto-char (point-min))
      (forward-char 3)
      (insert "{\"name\": \"Bob\"},")
      (should
       (equal
        (tree-sitter-node-string
         (tree-sitter-parser-root-node parser))
        "(document (array (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number) (number)))")))))

(ert-deftest tree-sitter-node-api ()
  "Tests for node API."
  (require 'tree-sitter-json)
  (with-temp-buffer
    (let (parser root-node doc-node object-node pair-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (tree-sitter-create-parser
                      (current-buffer) (tree-sitter-json)))
        (setq root-node (tree-sitter-parser-root-node
                         parser)))
      ;; `tree-sitter-node-type'.
      (should (eq 'document (tree-sitter-node-type root-node)))
      ;; `tree-sitter-node-check'.
      (should (eq t (tree-sitter-node-check root-node 'named)))
      (should (eq nil (tree-sitter-node-check root-node 'missing)))
      (should (eq nil (tree-sitter-node-check root-node 'extra)))
      (should (eq nil (tree-sitter-node-check root-node 'has-error)))
      ;; `tree-sitter-node-child'.
      (setq doc-node (tree-sitter-node-child root-node 0))
      (should (eq 'array (tree-sitter-node-type doc-node)))
      (should (equal (tree-sitter-node-string doc-node)
                     "(array (number) (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number))"))
      ;; `tree-sitter-node-child-count'.
      (should (eql 9 (tree-sitter-node-child-count doc-node)))
      (should (eql 4 (tree-sitter-node-child-count doc-node t)))
      ;; `tree-sitter-node-field-name-for-child'.
      (setq object-node (tree-sitter-node-child doc-node 2 t))
      (setq pair-node (tree-sitter-node-child object-node 0 t))
      (should (eq 'object (tree-sitter-node-type object-node)))
      (should (eq 'pair (tree-sitter-node-type pair-node)))
      (should (equal "key"
                     (tree-sitter-node-field-name-for-child
                      pair-node 0)))
      ;; `tree-sitter-node-child-by-field-name'.
      (should (equal "(string (string_content))"
                     (tree-sitter-node-string
                      (tree-sitter-node-child-by-field-name
                       pair-node "key"))))
      ;; `tree-sitter-node-next-sibling'.
      (should (equal "(number)"
                     (tree-sitter-node-string
                      (tree-sitter-node-next-sibling object-node t))))
      (should (equal "(\",\")"
                     (tree-sitter-node-string
                      (tree-sitter-node-next-sibling object-node))))
      ;; `tree-sitter-node-prev-sibling'.
      (should (equal "(number)"
                     (tree-sitter-node-string
                      (tree-sitter-node-prev-sibling object-node t))))
      (should (equal "(\",\")"
                     (tree-sitter-node-string
                      (tree-sitter-node-prev-sibling object-node))))
      ;; `tree-sitter-node-first-child-for-byte'.
      (should (equal "(number)"
                     (tree-sitter-node-string
                      (tree-sitter-node-first-child-for-byte
                       doc-node 3 t))))
      (should (equal "(\",\")"
                     (tree-sitter-node-string
                      (tree-sitter-node-first-child-for-byte
                       doc-node 3))))
      ;; `tree-sitter-node-descendant-for-byte-range'.
      (should (equal "(\"{\")"
                     (tree-sitter-node-string
                      (tree-sitter-node-descendant-for-byte-range
                       root-node 6 7))))
      (should (equal "(object (pair key: (string (string_content)) value: (string (string_content))))"
                     (tree-sitter-node-string
                      (tree-sitter-node-descendant-for-byte-range
                       root-node 6 7 t)))))))

(ert-deftest tree-sitter-query-api ()
  "Tests for query API."
  (require 'tree-sitter-json)
  (with-temp-buffer
    (let (parser root-node pattern doc-node object-node pair-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (tree-sitter-create-parser
                      (current-buffer) (tree-sitter-json)))
        (setq root-node (tree-sitter-parser-root-node
                         parser))
        (setq pattern "(string) @string
(pair key: (_) @keyword)
(number) @number"))

      (should
       (equal
        '((number . "1") (number . "2")
          (keyword . "\"name\"")
          (string . "\"name\"")
          (string . "\"Bob\"")
          (number . "3"))
        (mapcar (lambda (entry)
                  (cons (car entry)
                        (tree-sitter-node-content
                         (cdr entry))))
                (tree-sitter-query-capture root-node pattern)))))))

(ert-deftest tree-sitter-narrow ()
  "Tests if narrowing works."
  (require 'tree-sitter-json)
  (with-temp-buffer
    (let (parser root-node pattern doc-node object-node pair-node)
      (progn
        (insert "xxx[1,{\"name\": \"Bob\"},2,3]xxx")
        (narrow-to-region (+ (point-min) 3) (- (point-max) 3))
        (setq parser (tree-sitter-create-parser
                      (current-buffer) (tree-sitter-json)))
        (setq root-node (tree-sitter-parser-root-node
                         parser)))
      ;; This test is from the basic test.
      (should
       (equal
        (tree-sitter-node-string
         (tree-sitter-parser-root-node parser))
        "(document (array (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number) (number)))"))

      (widen)
      (goto-char (point-min))
      (insert "ooo")
      (should (equal "oooxxx[1,{\"name\": \"Bob\"},2,3]xxx"
                     (buffer-string)))
      (delete-region 10 26)
      (should (equal "oooxxx[1,2,3]xxx"
                     (buffer-string)))
      (narrow-to-region (+ (point-min) 6) (- (point-max) 3))
      ;; This test is also from the basic test.
      (should
       (equal (tree-sitter-node-string
               (tree-sitter-parser-root-node parser))
              "(document (array (number) (number) (number)))"))
      (widen)
      (goto-char (point-max))
      (insert "[1,2]")
      (should (equal "oooxxx[1,2,3]xxx[1,2]"
                     (buffer-string)))
      (narrow-to-region (- (point-max) 5) (point-max))
      (should
       (equal (tree-sitter-node-string
               (tree-sitter-parser-root-node parser))
              "(document (array (number) (number)))"))
      (widen)
      (goto-char (point-min))
      (insert "[1]")
      (should (equal "[1]oooxxx[1,2,3]xxx[1,2]"
                     (buffer-string)))
      (narrow-to-region (point-min) (+ (point-min) 3))
      (should
       (equal (tree-sitter-node-string
               (tree-sitter-parser-root-node parser))
              "(document (array (number)))")))))

(ert-deftest tree-sitter-range ()
  "Tests if range works."
  (require 'tree-sitter-json)
  (with-temp-buffer
    (let (parser root-node pattern doc-node object-node pair-node)
      (progn
        (insert "[[1],oooxxx[1,2,3],xxx[1,2]]")
        (setq parser (tree-sitter-create-parser
                      (current-buffer) (tree-sitter-json)))
        (setq root-node (tree-sitter-parser-root-node
                         parser)))
      ;; TODO: signaling error crashes Emacs on Mac.
      ;; (should-error
      ;;  (tree-sitter-parser-set-included-ranges
      ;;   parser '((1 . 6) (5 . 20)))
      ;;  :type '(tree-sitter-set-range-error))

      (tree-sitter-parser-set-included-ranges
       parser '((1 . 6) (12 . 20) (23 . 29)))
      (should (equal '((1 . 6) (12 . 20) (23 . 29))
                     (tree-sitter-parser-included-ranges parser)))
      (should (equal "(document (array (array (number)) (array (number) (number) (number)) (array (number) (number))))"
                     (tree-sitter-node-string
                      (tree-sitter-parser-root-node parser))))
      )))

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
