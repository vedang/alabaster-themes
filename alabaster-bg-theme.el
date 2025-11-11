;;; alabaster-bg-theme.el --- Alabaster BG light theme -*- lexical-binding:t -*-

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
;;
;; The `alabaster-bg' theme is a minimal light theme with background highlighting.
;; It uses subtle background colors to highlight syntax elements.

;;; Code:

;;;###theme-autoload
(deftheme alabaster-bg
  "Minimal light theme with background highlighting."
  :background-mode 'light
  :kind 'color-scheme
  :family 'alabaster)

(defconst alabaster-bg-palette
  '(
;;; Basic values

    (bg-main     "#ffffff")
    (fg-main     "#000000")
    (bg-dim      "#f5f5f5")
    (fg-dim      "#777777")
    (bg-alt      "#fafafa")
    (fg-alt      "#333333")

    (bg-active   "#e0e0e0")
    (bg-inactive "#f0f0f0")

;;; Basic hues

    (red             "#AA3731")
    (green           "#448C27")
    (yellow          "#FFBC5D")
    (blue            "#325CC0")
    (magenta         "#7A3E9D")

;;; Background hues (used for background highlighting)

    (bg-red-subtle      "#FFE0E0")
    (bg-green-subtle    "#F1FADF")
    (bg-yellow-subtle   "#FFFABC")
    (bg-blue-subtle     "#DBF1FF")
    (bg-magenta-subtle  "#F9E0FF")

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
    (bg-completion      "#DBF1FF")
    (bg-hover           "#BFDBFE")
    (bg-hl-line         "#f5f5f5")
    (bg-region          "#B4D8FD")
    (bg-err             "#FFE0E0")
    (bg-warning         "#FFFABC")
    (bg-info            "#F1FADF")

    (border        "#cccccc")
    (cursor        "#007acc")
    (fg-intense    "#000000")

    (modeline-err     "#AA3731")
    (modeline-warning "#FFBC5D")
    (modeline-info    "#325CC0")

    (underline-err     "#AA3731")
    (underline-warning "#FFBC5D")
    (underline-info    "#325CC0")

    (fg-region             "#000000")
    (link-alt              ,blue)
    (bg-search-current     "#FFBC5D")
    (bg-search-lazy        "#FFFABC")
    (bg-search-replace     "#FFE0E0")
    (bg-search-match       "#DBF1FF")
    (bg-search-rx-group-0  "#F1FADF")
    (bg-search-rx-group-1  "#FFE0E0")
    (bg-search-rx-group-2  "#FFFABC")
    (bg-search-rx-group-3  "#DBF1FF")

    (bg-char-0             "#FFE0E0")
    (bg-char-1             "#F1FADF")
    (bg-char-2             "#FFFABC")
    (bg-paren              "#DBF1FF")
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
    (comment yellow)
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
  "The `alabaster-bg' palette.")

(defcustom alabaster-bg-palette-overrides nil
  "Overrides for `alabaster-bg-palette'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

(alabaster-themes-theme alabaster-bg alabaster-bg-palette alabaster-bg-palette-overrides)

(provide-theme 'alabaster-bg)
;;; alabaster-bg-theme.el ends here
