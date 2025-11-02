;;; alabaster.el --- Alabaster light theme for Emacs -*- lexical-binding:t -*-

;;; Commentary:
;; Alabaster light theme - minimal highlighting with foreground colors.
;; Maintains exact color fidelity to original Sublime Text Alabaster scheme.

;;; Code:

(require 'alabaster-common)

(deftheme alabaster "Alabaster light theme")

(custom-theme-set-variables
 'alabaster
 '(cursor-color alabaster-active)
 '( fringe-mode alabaster-light-line-highlight))

(custom-theme-set-faces
 'alabaster
 ;; Basic appearance
 `(default ((t (:foreground ,alabaster-light-fg :background ,alabaster-light-bg))))
 `(cursor ((t (:background ,alabaster-active))))
 `(fringe ((t (:background ,alabaster-light-line-highlight))))
 `(line-number ((t (:foreground ,alabaster-grey :background ,alabaster-light-line-highlight))))
 `(line-number-current-line ((t (:foreground ,alabaster-light-fg :background ,alabaster-selection))))
 `(hl-line ((t (:background ,alabaster-light-line-highlight))))

 ;; Selection and highlighting
 `(region ((t (:background ,alabaster-selection))))
 `(highlight ((t (:background ,alabaster-orange :foreground ,alabaster-light-fg))))
 `(isearch ((t (:background ,alabaster-orange :foreground ,alabaster-light-fg))))
 `(lazy-highlight ((t (:background ,alabaster-orange :foreground ,alabaster-light-fg))))

 ;; Font lock faces - 4-class system
 `(font-lock-comment-face ((t (:foreground ,alabaster-red))))
 `(font-lock-string-face ((t (:foreground ,alabaster-green))))
 `(font-lock-constant-face ((t (:foreground ,alabaster-magenta))))
 `(font-lock-number-face ((t (:foreground ,alabaster-magenta))))
 `(font-lock-keyword-face ((t (:foreground ,alabaster-light-fg)))) ; Keywords not highlighted
 `(font-lock-function-name-face ((t (:foreground ,alabaster-blue))))
 `(font-lock-variable-name-face ((t (:foreground ,alabaster-blue))))
 `(font-lock-type-face ((t (:foreground ,alabaster-light-fg)))) ; Types not highlighted
 `(font-lock-builtin-face ((t (:foreground ,alabaster-light-fg)))) ; Builtins not highlighted

 ;; Punctuation
 `(font-lock-punctuation-face ((t (:foreground ,alabaster-grey))))

 ;; Error handling
 `(error ((t (:foreground ,alabaster-red :background ,(concat alabaster-red "33")))))
 `(warning ((t (:foreground ,alabaster-orange))))

 ;; Mode line
 `(mode-line ((t (:foreground ,alabaster-light-fg :background ,alabaster-light-line-highlight :box nil))))
 `(mode-line-inactive ((t (:foreground ,alabaster-grey :background ,alabaster-light-line-highlight :box nil))))

 ;; Git diff colors
 `(diff-added ((t (:foreground "hsl(100, 50%, 50%)"))))
 `(diff-removed ((t (:foreground "hsl(2, 65%, 50%)"))))
 `(diff-changed ((t (:foreground "hsl(30, 85%, 50%)")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster)
(provide 'alabaster)

;;; alabaster.el ends here