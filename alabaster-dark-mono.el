;;; alabaster-dark-mono.el --- Alabaster mono dark theme for Emacs -*- lexical-binding:t -*-

;;; Commentary:
;; Alabaster mono dark theme - monochromatic dark with minimal highlighting.
;; Only highlights errors, search results, and cursor.

;;; Code:

(require 'alabaster-common)

(deftheme alabaster-dark-mono "Alabaster mono dark theme")

(custom-theme-set-variables
 'alabaster-dark-mono
 '(cursor-color alabaster-dark-active))

(custom-theme-set-faces
 'alabaster-dark-mono
 ;; Basic appearance - everything monochrome dark
 `(default ((t (:foreground ,alabaster-dark-fg :background ,alabaster-dark-bg))))
 `(cursor ((t (:background ,alabaster-dark-active))))
 `(fringe ((t (:background ,alabaster-dark-line-highlight))))
  `(line-number ((t (:foreground ,alabaster-dark-dim-grey :background ,alabaster-dark-line-highlight))))
 `(line-number-current-line ((t (:foreground ,alabaster-dark-fg :background ,alabaster-dark-line-highlight))))
 `(hl-line ((t (:background ,alabaster-dark-line-highlight))))

 ;; Selection and highlighting
 `(region ((t (:background ,alabaster-dark-line-highlight))))
  `(highlight ((t (:background ,alabaster-dark-active :foreground ,alabaster-dark-highlight-text))))
  `(isearch ((t (:background ,alabaster-dark-active :foreground ,alabaster-dark-highlight-text))))
  `(lazy-highlight ((t (:background ,alabaster-dark-active :foreground ,alabaster-dark-highlight-text))))

 ;; Font lock faces - mostly monochrome
  `(font-lock-comment-face ((t (:foreground ,alabaster-dark-dim-grey))))
 `(font-lock-string-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-constant-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-number-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-keyword-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-function-name-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-variable-name-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-type-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-builtin-face ((t (:foreground ,alabaster-dark-fg))))
 `(font-lock-punctuation-face ((t (:foreground ,alabaster-dark-fg))))

 ;; Error handling - only errors highlighted
  `(error ((t (:foreground ,alabaster-dark-error :background ,alabaster-dark-error-bg))))
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

(provide-theme 'alabaster-dark-mono)
(provide 'alabaster-dark-mono)

;;; alabaster-dark-mono.el ends here