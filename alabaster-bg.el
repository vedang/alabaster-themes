;;; alabaster-bg.el --- Alabaster BG light theme for Emacs -*- lexical-binding:t -*-

;;; Commentary:
;; Alabaster BG light theme - minimal highlighting with background colors.
;; Uses colored backgrounds instead of text colors for the layered effect.

;;; Code:

(require 'alabaster-common)

(deftheme alabaster-bg "Alabaster BG light theme")

(custom-theme-set-variables
 'alabaster-bg
 '(cursor-color alabaster-active)
 '(fringe-mode "#00000010"))

(custom-theme-set-faces
 'alabaster-bg
 ;; Basic appearance
 `(default ((t (:foreground ,alabaster-bg-fg :background ,alabaster-bg-bg))))
 `(cursor ((t (:background ,alabaster-active))))
 `(fringe ((t (:background "#00000010"))))
 `(line-number ((t (:foreground "#00000090" :background "#00000010"))))
 `(line-number-current-line ((t (:foreground ,alabaster-bg-fg :background ,alabaster-selection))))
 `(hl-line ((t (:background "#00000010"))))

 ;; Selection and highlighting
 `(region ((t (:background "#B4D8FD"))))
 `(highlight ((t (:background ,alabaster-orange :foreground "#000"))))
 `(isearch ((t (:background ,alabaster-orange :foreground "#000"))))
 `(lazy-highlight ((t (:background ,alabaster-orange :foreground "#000"))))

 ;; Font lock faces - using background colors
 `(font-lock-comment-face ((t (:foreground "#000" :background ,alabaster-bg-yellow))))
 `(font-lock-string-face ((t (:foreground "#000" :background ,alabaster-bg-green))))
 `(font-lock-constant-face ((t (:foreground "#7A3E9D")))) ; Constants use foreground as in original
 `(font-lock-number-face ((t (:foreground "#7A3E9D"))))
 `(font-lock-keyword-face ((t (:foreground "#000")))) ; Keywords not highlighted
 `(font-lock-function-name-face ((t (:foreground "#000" :background ,alabaster-bg-blue))))
 `(font-lock-variable-name-face ((t (:foreground "#000" :background ,alabaster-bg-blue))))
 `(font-lock-type-face ((t (:foreground "#000")))) ; Types not highlighted
 `(font-lock-builtin-face ((t (:foreground "#000")))) ; Builtins not highlighted

 ;; Punctuation
 `(font-lock-punctuation-face ((t (:foreground "#00000090"))))

 ;; Inner brackets (special handling for nested structure)
 `(font-lock-regexp-grouping-construct ((t (:foreground "#00000075"))))

 ;; Error handling
 `(error ((t (:foreground "#c33" :background ,alabaster-bg-red))))
 `(warning ((t (:foreground ,alabaster-orange))))

 ;; Mode line
 `(mode-line ((t (:foreground "#000" :background "#00000010" :box nil))))
 `(mode-line-inactive ((t (:foreground "#00000090" :background "#00000010" :box nil))))

 ;; Git diff colors
 `(diff-added ((t (:foreground "hsl(100, 50%, 50%)"))))
 `(diff-removed ((t (:foreground "hsl(2, 65%, 50%)"))))
 `(diff-changed ((t (:foreground "hsl(30, 85%, 50%)")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster-bg)
(provide 'alabaster-bg)

;;; alabaster-bg.el ends here