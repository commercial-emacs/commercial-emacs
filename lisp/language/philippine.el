;;; philippine.el --- Philippine languages support  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: समीर सिंह Sameer Singh <lumarzeli30@gmail.com>
;; Keywords: multilingual, input method, i18n, Philippines

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

;; This file contains definitions of Philippine language environments, and
;; setups for displaying the scripts used there.

;;; Code:

(set-language-info-alist
 "Tagalog" '((charset unicode)
             (coding-system utf-8)
             (coding-priority utf-8)
             (input-method . "tagalog")
             (sample-text . "Tagalog (ᜊᜌ᜔ᜊᜌᜒᜈ᜔)	ᜃᜓᜋᜓᜐ᜔ᜆ")
             (documentation . "\
Tagalog language using the Baybayin script is supported in
this language environment.")))

;; Tagalog composition rules
(let ((akshara              "[\x1700-\x1711\x171F]")
      (vowel                "[\x1712\x1713]")
      (virama               "\x1714")
      (pamudpod             "\x1715"))
  (set-char-table-range composition-function-table
                        '(#x1714 . #x1714)
                        (list (vector
                               ;; Akshara virama syllables
                               (concat akshara virama vowel "?")
                               1 'font-shape-gstring)))
  (set-char-table-range composition-function-table
                        '(#x1715 . #x1715)
                        (list (vector
                               ;; Akshara pamudpod syllables
                               (concat akshara pamudpod vowel "?")
                               1 'font-shape-gstring))))

(provide 'philippine)
;;; philippine.el ends here
