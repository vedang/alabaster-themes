;;; alabaster-themes-light-mono-theme.el --- Alabaster mono light theme -*- lexical-binding:t -*-

;; Copyright (C) 2025 Nikita Prokopov

;; Author: Nikita Prokopov <@tonsky>
;; Maintainer: Vedang Manerikar <@vedang>
;; URL: https://github.com/vedang/alabaster-themes
;; Version: 2.1.0
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
;; The `alabaster-themes-light-mono' theme is a minimal light theme
;; with monochromatic highlighting.  Only errors and warnings use
;; color, everything else is monochrome.

;;; Code:

(require 'alabaster-themes)

;;;###theme-autoload
(deftheme alabaster-themes-light-mono
  "Minimal light theme with monochromatic highlighting."
  :background-mode 'light
  :kind 'color-scheme
  :family 'alabaster)

(eval-and-compile (defconst alabaster-themes-light-mono-palette
                    '(
;;; Basic values

                      (bg-main     "#F7F7F7")
                      (fg-main     "#000000")
                      (bg-dim      "#f0f0f0")
                      (fg-dim      "#777777")
                      (bg-alt      "#ffffff")
                      (fg-alt      "#333333")

                      (bg-active   "#e0e0e0")
                      (bg-inactive "#f5f5f5")

;;; Basic hues (mostly monochrome)

                      (red             "#AA3731")
                      (green           "#000000")
                      (yellow          "#FFBC5D")
                      (blue            "#000000")
                      (magenta         "#000000")

;;; Background hues

                      (bg-red-subtle      "#FFE0E0")
                      (bg-green-subtle    "#f0f0f0")
                      (bg-yellow-subtle   "#f0f0f0")
                      (bg-blue-subtle     "#f0f0f0")
                      (bg-magenta-subtle  "#f0f0f0")

;;; Diffs

                      (bg-added          "#d4f6d4")
                      (bg-added-faint    "#e8fae8")
                      (bg-added-refine   "#b8e6b8")
                      (fg-added          "#005000")

                      (bg-changed        "#ffe5b9")
                      (bg-changed-faint  "#ffefc5")
                      (bg-changed-refine "#ffd09f")
                      (fg-changed        "#553d00")

                      (bg-removed        "#ffd4d8")
                      (bg-removed-faint  "#ffe3e3")
                      (bg-removed-refine "#ffc0ca")
                      (fg-removed        "#8f1313")

;;; Special hues

                      (bg-mode-line       "#e0e0e0")
                      (fg-mode-line       "#000000")
                      (bg-completion      "#f0f0f0")
                      (bg-hover           "#f0f0f0")
                      (bg-hl-line         "#f0f0f0")
                      (bg-region          "#f0f0f0")
                      (bg-err             "#FFE0E0")
                      (bg-warning         "#f0f0f0")
                      (bg-info            "#f0f0f0")

                      (border        "#cccccc")
                      (cursor        "#007acc")
                      (fg-intense    "#000000")

                      (modeline-err     "#AA3731")
                      (modeline-warning "#777777")
                      (modeline-info    "#777777")

                      (underline-err     "#AA3731")
                      (underline-warning "#777777")
                      (underline-info    "#777777")

                      (fg-region             "#000000")
                      (link-alt              ,fg-main)
                      (bg-search-current     "#777777")
                      (bg-search-lazy        "#f0f0f0")
                      (bg-search-replace     "#FFE0E0")
                      (bg-search-match       "#f0f0f0")
                      (bg-search-rx-group-0  "#f0f0f0")
                      (bg-search-rx-group-1  "#FFE0E0")
                      (bg-search-rx-group-2  "#f0f0f0")
                      (bg-search-rx-group-3  "#f0f0f0")

                      (bg-char-0             "#FFE0E0")
                      (bg-char-1             "#f0f0f0")
                      (bg-char-2             "#f0f0f0")
                      (bg-paren              "#f0f0f0")
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
                  "The `alabaster-themes-light-mono' palette.")

(defcustom alabaster-themes-light-mono-palette-overrides nil
  "Overrides for `alabaster-themes-light-mono-palette'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

(alabaster-themes-theme alabaster-themes-light-mono alabaster-themes-light-mono-palette alabaster-themes-light-mono-palette-overrides)

(provide-theme 'alabaster-themes-light-mono)
;;; alabaster-themes-light-mono-theme.el ends here
