;;; alabaster-common.el --- Common Alabaster theme definitions -*- lexical-binding:t -*-

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
;; Common color definitions and utilities for Alabaster theme variants.
;; Maintains exact color fidelity to original Sublime Text scheme.

;;; Code:

(require 'alabaster-theme)

;;; Base color constants (exact Sublime Text Alabaster colors)

(defconst alabaster-active "#007acc" "Cursor and highlights color")
(defconst alabaster-selection "#BFDBFE" "Selection background")
(defconst alabaster-blue "#325CC0" "Definitions color")
(defconst alabaster-green "#448C27" "Strings color")
(defconst alabaster-red "#AA3731" "Comments and errors color")
(defconst alabaster-magenta "#7A3E9D" "Constants color")
(defconst alabaster-grey "#777777" "Punctuation and dim text color")
(defconst alabaster-orange "#FFBC5D" "Highlights and search color")

;;; Palette overrides

(defcustom alabaster-themes-common-palette-overrides nil
  "Set palette overrides for all the Alabaster themes.

Mirror the elements of a theme's palette, overriding their value.
The palette variables are named THEME-NAME-palette, while
individual theme overrides are THEME-NAME-palette-overrides.  The
THEME-NAME is one of the symbols in `alabaster-themes-collection'.

Individual theme overrides take precedence over these common
overrides."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

;;; Alabaster theme palette (light, foreground highlighting)

(defconst alabaster-palette
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

;;; Basic hues

    (red             "#AA3731")
    (green           "#448C27")
    (yellow          "#FFBC5D")
    (blue            "#325CC0")
    (magenta         "#7A3E9D")

;;; Background hues

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
    (bg-hl-line         "#f0f0f0")
    (bg-region          "#BFDBFE")
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
  "The `alabaster' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

(defcustom alabaster-palette-overrides nil
  "Overrides for `alabaster-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Alabaster themes,
refer to `alabaster-themes-common-palette-overrides'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

;;; Alabaster BG theme palette (light, background highlighting)

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

;;; Alabaster Dark theme palette (dark, foreground highlighting)

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

;;; Alabaster Mono theme palette (light, monochromatic)

(defconst alabaster-mono-palette
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
    (bg-term-white           "gray65"))
  "The `alabaster-mono' palette.")

(defcustom alabaster-mono-palette-overrides nil
  "Overrides for `alabaster-mono-palette'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

;;; Alabaster Dark Mono theme palette (dark, monochromatic)

(defconst alabaster-dark-mono-palette
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
    (bg-term-white           "gray65"))
  "The `alabaster-dark-mono' palette.")

(defcustom alabaster-dark-mono-palette-overrides nil
  "Overrides for `alabaster-dark-mono-palette'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

(provide 'alabaster-common)
;;; alabaster-common.el ends here
