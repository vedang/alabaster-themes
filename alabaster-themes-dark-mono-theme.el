;;; alabaster-themes-dark-mono-theme.el --- Alabaster dark mono theme -*- lexical-binding:t -*-

;; Copyright (C) 2025  Nikita Prokopov

;; Author: Nikita Prokopov
;; Maintainer: Vedang Manerikar
;; URL: https://github.com/vedang/alabaster-themes
;; Version: 1.1.0
;; Package-Requires: ((emacs "28.1"))
;; Local Variables:
;; package-lint-main-file: "alabaster-themes.el"
;; End:
;; Keywords: faces, theme, minimal

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; The `alabaster-themes-dark-mono' theme is a minimal dark theme with
;; monochromatic highlighting. Only errors and warnings use color,
;; everything else is monochrome.

;;; Code:

(require 'alabaster-themes)

;;;###theme-autoload
(deftheme alabaster-themes-dark-mono
  "Minimal dark theme with monochromatic highlighting."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'alabaster)

(eval-and-compile (defconst alabaster-themes-dark-mono-palette
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

;;; Basic hues (mostly monochrome)

    (red             "#ff6b6b")
    (green           "#CECECE")
    (yellow          "#CD974B")
    (blue            "#CECECE")
    (magenta         "#CECECE")

;;; Background hues

    (bg-red-subtle      "#332020")
    (bg-green-subtle    "#1a1a1a")
    (bg-yellow-subtle   "#332a20")
    (bg-blue-subtle     "#1a1a1a")
    (bg-magenta-subtle  "#1a1a1a")

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
    (bg-completion      "#1a1a1a")
    (bg-hover           "#1a1a1a")
    (bg-hl-line         "#1a1a1a")
    (bg-region          "#1a1a1a")
    (bg-err             "#332020")
    (bg-warning         "#332a20")
    (bg-info            "#1f2a1f")

    (border        "#444444")
    (cursor        "#CD974B")
    (fg-intense    "#ffffff")

    (modeline-err     "#ff6b6b")
    (modeline-warning "#666666")
    (modeline-info    "#666666")

    (underline-err     "#ff6b6b")
    (underline-warning "#666666")
    (underline-info    "#666666")

    (fg-region             "#CECECE")
    (link-alt              ,fg-main)
    (bg-search-current     "#666666")
    (bg-search-lazy        "#1a1a1a")
    (bg-search-replace     "#332020")
    (bg-search-match       "#1a1a1a")
    (bg-search-rx-group-0  "#1f2a1f")
    (bg-search-rx-group-1  "#332020")
    (bg-search-rx-group-2  "#332a20")
    (bg-search-rx-group-3  "#1a1a1a")

    (bg-char-0             "#332020")
    (bg-char-1             "#1f2a1f")
    (bg-char-2             "#332a20")
    (bg-paren              "#1a1a1a")
    (bg-red-intense        "#ff6b6b")

    (rainbow-0             ,fg-main)
    (rainbow-1             ,fg-main)
    (rainbow-2             ,fg-main)
    (rainbow-3             ,yellow)
    (rainbow-4             ,red)
    (rainbow-5             ,fg-main)
    (rainbow-6             ,fg-main)
    (rainbow-7             ,fg-main)
    (rainbow-8             ,yellow)

;;; Mappings

;;;; General mappings

    (err red)
    (warning yellow)
    (info fg-main)

    (link fg-main)
    (name fg-main)
    (keybind red)
    (identifier fg-main)
    (prompt fg-main)

    (builtin fg-main)
    (comment fg-dim)
    (constant fg-main)
    (docstring fg-main)
    (fnname fg-main)
    (keyword fg-main)
    (preprocessor fg-main)
    (string fg-main)
    (type fg-main)
    (variable fg-main)

    (bg-fringe unspecified)
    (fg-fringe fg-dim)

    (fg-term-black           "black")
    (fg-term-red             red)
    (fg-term-green           fg-main)
    (fg-term-yellow          yellow)
    (fg-term-blue            fg-main)
    (fg-term-magenta         fg-main)
    (fg-term-cyan            fg-main)
    (fg-term-white           "gray65")

    (bg-term-black           "black")
    (bg-term-red             red)
    (bg-term-green           bg-main)
    (bg-term-yellow          bg-main)
    (bg-term-blue            bg-main)
    (bg-term-magenta         bg-main)
    (bg-term-cyan            bg-main)
    (bg-term-white           "gray65")))
  "The `alabaster-themes-dark-mono' palette.")

(defcustom alabaster-themes-dark-mono-palette-overrides nil
  "Overrides for `alabaster-themes-dark-mono-palette'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

(alabaster-themes-theme alabaster-themes-dark-mono alabaster-themes-dark-mono-palette alabaster-themes-dark-mono-palette-overrides)

(provide-theme 'alabaster-themes-dark-mono)
;;; alabaster-themes-dark-mono-theme.el ends here
