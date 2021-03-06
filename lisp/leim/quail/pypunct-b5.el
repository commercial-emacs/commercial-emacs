;;; pypunct-b5.el --- Quail packages for Chinese (pinyin + extra symbols)  -*- lexical-binding: t -*-

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Author: Ken'ichi Handa <handa@gnu.org>

;; Keywords: multilingual, input method, Chinese

;; This file is NOT part of GNU Emacs.

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

;;; Code:

(require 'quail)

(load "quail/PY-b5")
(load "quail/Punct-b5")

(quail-define-package
 "chinese-py-punct-b5" "Chinese-BIG5" "拼符"
 t
 "中文輸入【拼音】 and `v' for 標點符號輸入

This is the combination of the input method `chinese-py-b5' and
`chinese-punct-b5'.

You can enter normal Chinese characters by the same way as `chinese-py-b5'.
And, you can enter symbols by typing `v' followed by any key sequences
defined in `chinese-punct-b5'.

For instance, typing `v' and `%' insert `％'.
")

(setcar (nthcdr 2 quail-current-package)
	(nth 2 (assoc "chinese-py-b5" quail-package-alist)))

(quail-defrule "v" (nth 2 (assoc "chinese-punct-b5" quail-package-alist)))

;;; pypunct-b5.el ends here
