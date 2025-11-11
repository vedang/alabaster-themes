;;; alabaster-common.el --- Common Alabaster theme definitions -*- lexical-binding:t -*-

;; Copyright (C) 2025  Niki Tonsky

;; Author: Niki Tonsky <nikki.me>
;; Maintainer: Niki Tonsky <nikki.me>
;; URL: https://github.com/tonsky/sublime-scheme-alabaster
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces, theme, minimal

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Common color definitions and utilities for Alabaster theme variants.
;; Maintains exact color fidelity to original Sublime Text scheme.

;;; Code:

;;; Base color constants (exact Sublime Text Alabaster colors)

(defconst alabaster-active "#007acc" "Cursor and highlights color")
(defconst alabaster-selection "#BFDBFE" "Selection background")
(defconst alabaster-blue "#325CC0" "Definitions color")
(defconst alabaster-green "#448C27" "Strings color")
(defconst alabaster-red "#AA3731" "Comments and errors color")
(defconst alabaster-magenta "#7A3E9D" "Constants color")
(defconst alabaster-grey "#777777" "Punctuation and dim text color")
(defconst alabaster-orange "#FFBC5D" "Highlights and search color")

(provide 'alabaster-common)
;;; alabaster-common.el ends here
