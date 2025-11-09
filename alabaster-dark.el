;;; alabaster-dark.el --- Alabaster dark theme for Emacs -*- lexical-binding:t -*-

;;; Commentary:
;; Alabaster dark theme - minimal highlighting for dark mode.
;; Maintains color relationships from light theme.

;;; Code:

(eval-and-compile
  (require 'alabaster-common))

(deftheme alabaster-dark "Alabaster dark theme"
          :background-mode 'dark
          :kind 'color-scheme
          :family 'alabaster)

(custom-theme-set-variables
 'alabaster-dark
 '(cursor-color alabaster-dark-active))

(custom-theme-set-faces
 'alabaster-dark
 ;; Basic appearance
 `(default ((t (:foreground ,alabaster-dark-fg :background ,alabaster-dark-bg))))
 `(cursor ((t (:background ,alabaster-dark-active))))
 `(fringe ((t (:background ,alabaster-dark-line-highlight))))
 `(line-number ((t (:foreground ,alabaster-dark-dim-grey :background ,alabaster-dark-line-highlight))))
 `(line-number-current-line ((t (:foreground ,alabaster-dark-fg :background ,alabaster-dark-selection))))
 `(hl-line ((t (:background ,alabaster-dark-line-highlight))))

 ;; Selection and highlighting
 `(region ((t (:background ,alabaster-dark-selection))))
 `(highlight ((t (:background ,alabaster-dark-active :foreground "#000"))))
 `(isearch ((t (:background ,alabaster-dark-active :foreground "#000"))))
 `(lazy-highlight ((t (:background ,alabaster-dark-active :foreground "#000"))))

 ;; Font lock faces - 4-class system with dark colors
 `(font-lock-comment-face ((t (:foreground ,alabaster-dark-comment))))
 `(font-lock-string-face ((t (:foreground ,alabaster-dark-string))))
 `(font-lock-constant-face ((t (:foreground ,alabaster-dark-constant))))
 `(font-lock-number-face ((t (:foreground ,alabaster-dark-constant))))
 `(font-lock-keyword-face ((t (:foreground ,alabaster-dark-fg)))) ; Keywords not highlighted
 `(font-lock-function-name-face ((t (:foreground ,alabaster-dark-definition-blue)))) ; Light blue for definitions
 `(font-lock-variable-name-face ((t (:foreground ,alabaster-dark-definition-blue))))
 `(font-lock-type-face ((t (:foreground ,alabaster-dark-fg)))) ; Types not highlighted
 `(font-lock-builtin-face ((t (:foreground ,alabaster-dark-fg)))) ; Builtins not highlighted

 ;; Punctuation
 `(font-lock-punctuation-face ((t (:foreground ,alabaster-dark-dim-grey))))

 ;; Error handling
 `(error ((t (:foreground ,alabaster-dark-error :background ,(concat alabaster-dark-error "33")))))
 `(warning ((t (:foreground ,alabaster-dark-active))))

 ;; Mode line
 `(mode-line ((t (:foreground ,alabaster-dark-fg :background alabaster-dark-line-highlight :box nil))))
 `(mode-line-inactive ((t (:foreground ,alabaster-dark-dim-grey :background alabaster-dark-line-highlight :box nil))))

 ;; Git diff colors
 `(diff-added ((t (:foreground "hsl(100, 50%, 50%)"))))
 `(diff-removed ((t (:foreground "hsl(2, 65%, 50%)"))))
 `(diff-changed ((t (:foreground "hsl(30, 85%, 50%)")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster-dark)
(provide 'alabaster-dark)

;;; alabaster-dark.el ends here
