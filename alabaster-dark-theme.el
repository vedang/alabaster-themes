;;; alabaster-dark-theme.el --- Alabaster dark theme -*- lexical-binding:t -*-

;; Copyright (C) 2025 Nikita Prokopov

;; Author: Nikita Prokopov
;; Maintainer: Vedang Manerikar
;; URL: https://github.com/vedang/alabaster-themes
;; Version: 1.0.1
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
;; The `alabaster-dark' theme is a minimal dark theme with foreground highlighting.
;; It maintains the Alabaster design philosophy in a dark variant.

;;; Code:

;;;###theme-autoload
(deftheme alabaster-dark
  "Minimal dark theme with foreground highlighting."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'alabaster)

(defconst alabaster-dark-palette
  '(
;;; Basic values

    (bg-main     "#0E1415")
    (fg-main     "#CECECE")
    (bg-dim      "#1a1a1a")
    (fg-dim      "#666666")
    (bg-alt      "#1f2526")
    (fg-alt      "#a0a0a0")

    (bg-active   "#293334")
    (bg-inactive "#121818")

;;; Basic hues (dark variants)

    (red             "#DFDF8E")
    (green           "#95CB82")
    (yellow          "#CD974B")
    (blue            "#8AB1F0")
    (magenta         "#CC8BC9")

;;; Background hues

    (bg-red-subtle      "#332020")
    (bg-green-subtle    "#1f2a1f")
    (bg-yellow-subtle   "#332a20")
    (bg-blue-subtle     "#202633")
    (bg-magenta-subtle  "#2f2030")

;;; Diffs

    (bg-added          "#1f3a1f")
    (bg-added-faint    "#2a4a2a")
    (bg-added-refine   "#0f2f0f")
    (fg-added          "#95CB82")

    (bg-changed        "#3a2f1f")
    (bg-changed-faint  "#4a3f2a")
    (bg-changed-refine "#2f2f0f")
    (fg-changed        "#CD974B")

    (bg-removed        "#3a1f1f")
    (bg-removed-faint  "#4a2a2a")
    (bg-removed-refine "#2f0f0f")
    (fg-removed        "#ff6b6b")

;;; Special hues

    (bg-mode-line       "#293334")
    (fg-mode-line       "#CECECE")
    (bg-completion      "#202633")
    (bg-hover           "#293334")
    (bg-hl-line         "#1a1a1a")
    (bg-region          "#293334")
    (bg-err             "#332020")
    (bg-warning         "#332a20")
    (bg-info            "#1f2a1f")

    (border        "#444444")
    (cursor        "#CD974B")
    (fg-intense    "#ffffff")

    (modeline-err     "#ff6b6b")
    (modeline-warning "#CD974B")
    (modeline-info    "#8AB1F0")

    (underline-err     "#ff6b6b")
    (underline-warning "#CD974B")
    (underline-info    "#8AB1F0")

    (fg-region             "#CECECE")
    (link-alt              ,blue)
    (bg-search-current     "#CD974B")
    (bg-search-lazy        "#332a20")
    (bg-search-replace     "#332020")
    (bg-search-match       "#202633")
    (bg-search-rx-group-0  "#1f2a1f")
    (bg-search-rx-group-1  "#332020")
    (bg-search-rx-group-2  "#332a20")
    (bg-search-rx-group-3  "#202633")

    (bg-char-0             "#332020")
    (bg-char-1             "#1f2a1f")
    (bg-char-2             "#332a20")
    (bg-paren              "#202633")
    (bg-red-intense        "#ff6b6b")

    (rainbow-0             ,blue)
    (rainbow-1             ,magenta)
    (rainbow-2             ,green)
    (rainbow-3             ,yellow)
    (rainbow-4             ,red)
    (rainbow-5             ,blue)
    (rainbow-6             ,magenta)
    (rainbow-7             ,green)
    (rainbow-8             ,yellow)

;;; Mappings

;;;; General mappings

    (err red)
    (warning yellow)
    (info green)

    (link blue)
    (name blue)
    (keybind red)
    (identifier magenta)
    (prompt blue)

    (builtin red)
    (comment red)
    (constant magenta)
    (docstring green)
    (fnname blue)
    (keyword fg-main)
    (preprocessor blue)
    (string green)
    (type fg-main)
    (variable blue)

    (bg-fringe unspecified)
    (fg-fringe fg-dim)

    (fg-term-black           "black")
    (fg-term-red             red)
    (fg-term-green           green)
    (fg-term-yellow          yellow)
    (fg-term-blue            blue)
    (fg-term-magenta         magenta)
    (fg-term-cyan            blue)
    (fg-term-white           "gray65")

    (bg-term-black           "black")
    (bg-term-red             red)
    (bg-term-green           green)
    (bg-term-yellow          yellow)
    (bg-term-blue            blue)
    (bg-term-magenta         magenta)
    (bg-term-cyan            blue)
    (bg-term-white           "gray65"))
  "The `alabaster-dark' palette.")

(defcustom alabaster-dark-palette-overrides nil
  "Overrides for `alabaster-dark-palette'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

(alabaster-themes-theme alabaster-dark alabaster-dark-palette alabaster-dark-palette-overrides)

(provide-theme 'alabaster-dark)
;;; alabaster-dark-theme.el ends here
