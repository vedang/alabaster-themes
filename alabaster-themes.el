;;; alabaster.el --- Alabaster light theme -*- lexical-binding:t -*-

;; Copyright (C) 2025  Your Name

;; Author: Your Name <your.email@example.com>
;; Maintainer: Your Name <your.email@example.com>
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
;;
;; The `alabaster' theme is a minimal light theme with foreground highlighting.
;; It maintains exact color fidelity to the original Sublime Text Alabaster scheme.

;;; Code:

(require 'seq)
(eval-when-compile (require 'subr-x))
  (require 'alabaster-common))

;;;###theme-autoload
(deftheme alabaster
  "Minimal light theme with foreground highlighting."
  :background-mode 'light
  :kind 'color-scheme
  :family 'alabaster)

(alabaster-themes-theme alabaster alabaster-palette alabaster-palette-overrides)

(provide-theme 'alabaster)
;;; alabaster.el ends here
