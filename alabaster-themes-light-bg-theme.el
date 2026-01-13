;;; alabaster-themes-light-bg-theme.el --- Alabaster BG light theme -*- lexical-binding:t -*-

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
;; The `alabaster-themes-light-bg' theme is a minimal light theme with
;; background highlighting.  It uses subtle background colors to
;; highlight syntax elements.  Unlike other Alabaster themes, this
;; variant requires custom face definitions because it uses background
;; colors for syntax highlighting instead of foreground.  The standard
;; palette mappings work for foreground highlighting but cannot
;; express the background-first approach needed here.

;;; Code:

(require 'alabaster-themes)

;;;###theme-autoload
(deftheme alabaster-themes-light-bg
  "Minimal light theme with background highlighting."
  :background-mode 'light
  :kind 'color-scheme
  :family 'alabaster)

(eval-and-compile (defconst alabaster-themes-light-bg-palette
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
                      (bg-term-white           "gray65")))
                  "The `alabaster-themes-light-bg' palette.")

(defcustom alabaster-themes-light-bg-palette-overrides nil
  "Overrides for `alabaster-themes-light-bg-palette'."
  :group 'alabaster-themes
  :type '(repeat (list symbol (choice symbol string))))

;;; Face specifications for background highlighting

(eval-and-compile (defvar alabaster-bg-faces
                    '(
;;;; basic faces
                      `(default ((,c :background ,bg-main :foreground ,fg-main)))
                      `(cursor ((,c :background ,cursor)))
                      `(fringe ((,c :background ,bg-dim :foreground ,fg-dim)))
                      `(line-number ((,c :inherit fringe)))
                      `(line-number-current-line ((,c ,@(alabaster-themes--bold) :foreground ,fg-intense)))
                      `(hl-line ((,c :background ,bg-hl-line)))
                      `(region ((,c :background ,bg-region :foreground ,fg-region)))
                      `(highlight ((,c :background ,bg-hover :foreground ,fg-intense)))
                      `(isearch ((,c :background ,bg-search-current :foreground ,fg-intense)))
                      `(lazy-highlight ((,c :background ,bg-search-lazy :foreground ,fg-intense)))
                      `(match ((,c :background ,bg-search-match)))
                      `(button ((,c :foreground ,link :underline ,border)))
                      `(link ((,c :foreground ,link :underline ,border)))
                      `(link-visited ((,c :foreground ,link-alt :underline ,border)))
                      `(minibuffer-prompt ((,c :foreground ,prompt)))
                      `(error ((,c ,@(alabaster-themes--bold) :foreground ,err)))
                      `(warning ((,c ,@(alabaster-themes--bold) :foreground ,warning)))
                      `(success ((,c ,@(alabaster-themes--bold) :foreground ,info)))
                      `(shadow ((,c :foreground ,fg-dim)))
                      `(tooltip ((,c :background ,bg-alt :foreground ,fg-intense)))

;;;; font-lock faces with background highlighting
                      `(font-lock-comment-face ((,c :background ,bg-yellow-subtle :foreground ,fg-main)))
                      `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
                      `(font-lock-string-face ((,c :background ,bg-green-subtle :foreground ,fg-main)))
                      `(font-lock-doc-face ((,c :inherit font-lock-string-face)))
                      `(font-lock-keyword-face ((,c :foreground ,keyword)))
                      `(font-lock-builtin-face ((,c :foreground ,builtin)))
                      `(font-lock-function-name-face ((,c :background ,bg-blue-subtle :foreground ,fg-main)))
                      `(font-lock-variable-name-face ((,c :foreground ,variable)))
                      `(font-lock-type-face ((,c :foreground ,type)))
                      `(font-lock-constant-face ((,c :background ,bg-magenta-subtle :foreground ,fg-main)))
                      `(font-lock-preprocessor-face ((,c :foreground ,preprocessor)))
                      `(font-lock-warning-face ((,c :inherit warning)))

;;;; mode-line faces
                      `(mode-line ((,c :background ,bg-mode-line :foreground ,fg-mode-line)))
                      `(mode-line-inactive ((,c :background ,bg-inactive :foreground ,fg-dim)))
                      `(mode-line-buffer-id ((,c ,@(alabaster-themes--bold))))
                      `(mode-line-emphasis ((,c ,@(alabaster-themes--bold))))
                      `(mode-line-highlight ((,c :inherit highlight)))

;;;; diff faces
                      `(diff-added ((,c :background ,bg-added :foreground ,fg-added)))
                      `(diff-changed ((,c :background ,bg-changed :foreground ,fg-changed)))
                      `(diff-removed ((,c :background ,bg-removed :foreground ,fg-removed)))
                      `(diff-header ((,c ,@(alabaster-themes--bold))))
                      `(diff-file-header ((,c ,@(alabaster-themes--bold) :background ,bg-alt)))
                      `(diff-hunk-header ((,c :background ,bg-active :foreground ,fg-intense)))
                      `(diff-context ((,c :foreground ,fg-dim)))
                      `(diff-indicator-added ((,c :inherit diff-added)))
                      `(diff-indicator-changed ((,c :inherit diff-changed)))
                      `(diff-indicator-removed ((,c :inherit diff-removed)))
                      `(diff-refine-added ((,c :background ,bg-added-refine :foreground ,fg-added)))
                      `(diff-refine-changed ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
                      `(diff-refine-removed ((,c :background ,bg-removed-refine :foreground ,fg-removed)))

;;;; Search and highlight faces
                      `(isearch-fail ((,c :background ,bg-err :foreground ,fg-intense)))
                      `(query-replace ((,c :inherit isearch)))
                      `(lazy-highlight ((,c :background ,bg-search-lazy :foreground ,fg-intense)))
                      `(match ((,c :background ,bg-search-match)))

;;;; Parenthesis matching
                      `(show-paren-match ((,c :background ,bg-paren :foreground ,fg-intense)))
                      `(show-paren-match-expression ((,c :background ,bg-alt)))
                      `(show-paren-mismatch ((,c :background ,bg-red-intense :foreground ,fg-intense)))

;;;; Line highlighting
                      `(hl-line ((,c :background ,bg-hl-line)))
                      `(highlight ((,c :background ,bg-hover :foreground ,fg-intense)))
                      `(region ((,c :background ,bg-region :foreground ,fg-region)))

;;;; Compilation and grep
                      `(compilation-line-number ((,c :foreground ,fg-dim)))
                      `(compilation-error ((,c :inherit error)))
                      `(compilation-warning ((,c :inherit warning)))
                      `(compilation-info ((,c :inherit success)))
                      `(compilation-mode-line-exit ((,c :inherit success)))
                      `(compilation-mode-line-run ((,c :foreground ,fg-alt)))
                      `(compilation-mode-line-fail ((,c :inherit error)))

;;;; Flymake
                      `(flymake-error ((,c :underline (:style wave :color ,underline-err))))
                      `(flymake-warning ((,c :underline (:style wave :color ,underline-warning))))
                      `(flymake-note ((,c :underline (:style wave :color ,underline-info))))

;;;; Flycheck
                      `(flycheck-error ((,c :underline (:style wave :color ,underline-err))))
                      `(flycheck-warning ((,c :underline (:style wave :color ,underline-warning))))
                      `(flycheck-info ((,c :underline (:style wave :color ,underline-info))))

;;;; Fringe
                      `(fringe ((,c :background ,bg-dim :foreground ,fg-dim)))
                      `(line-number ((,c :inherit fringe)))
                      `(line-number-current-line ((,c ,@(alabaster-themes--bold) :foreground ,fg-intense)))

;;;; Tooltip
                      `(tooltip ((,c :background ,bg-alt :foreground ,fg-intense)))

;;;; Completions
                      `(completions-common-part ((,c :foreground ,fg-alt)))
                      `(completions-first-difference ((,c :foreground ,fg-intense)))

;;;; Widget
                      `(widget-field ((,c :background ,bg-active :foreground ,fg-intense)))
                      `(widget-button ((,c :foreground ,link)))
                      `(widget-button-pressed ((,c :foreground ,link-alt)))
                      `(widget-documentation ((,c :foreground ,docstring)))
                      `(widget-single-line-field ((,c :background ,bg-active :foreground ,fg-intense)))

;;;; Button
                      `(button ((,c :foreground ,link :underline ,border)))
                      `(link ((,c :foreground ,link :underline ,border)))
                      `(link-visited ((,c :foreground ,link-alt :underline ,border)))

;;;; Escape and prompt faces
                      `(minibuffer-prompt ((,c :foreground ,prompt)))
                      `(escape-glyph ((,c :foreground ,yellow)))
                      `(homoglyph ((,c :foreground ,yellow)))

;;;; Trailing whitespace
                      `(trailing-whitespace ((,c :background ,bg-err)))

;;;; Cursor
                      `(cursor ((,c :background ,cursor)))

;;;; Tab bar
                      `(tab-bar ((,c :background ,bg-dim :foreground ,fg-dim)))
                      `(tab-bar-tab ((,c :background ,bg-main :foreground ,fg-main)))
                      `(tab-bar-tab-inactive ((,c :background ,bg-inactive :foreground ,fg-dim)))

;;;; Tab line
                      `(tab-line ((,c :background ,bg-dim :foreground ,fg-dim)))
                      `(tab-line-tab ((,c :background ,bg-main :foreground ,fg-main)))
                      `(tab-line-tab-inactive ((,c :background ,bg-inactive :foreground ,fg-dim)))
                      `(tab-line-tab-current ((,c :inherit tab-line-tab)))

;;;; Window divider
                      `(window-divider ((,c :foreground ,border)))
                      `(window-divider-first-pixel ((,c :foreground ,border)))
                      `(window-divider-last-pixel ((,c :foreground ,border)))

;;;; Header line
                      `(header-line ((,c :background ,bg-alt :foreground ,fg-main))))
                    "Face specifications for Alabaster light-bg theme with background highlighting."))

(alabaster-themes-theme alabaster-themes-light-bg alabaster-themes-light-bg-palette alabaster-themes-light-bg-palette-overrides alabaster-bg-faces)

(provide-theme 'alabaster-themes-light-bg)
;;; alabaster-themes-light-bg-theme.el ends here
