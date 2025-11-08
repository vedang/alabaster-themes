;;; alabaster-mono.el --- Alabaster mono light theme for Emacs -*- lexical-binding:t -*-

;;; Commentary:
;; Alabaster mono light theme - monochromatic with minimal highlighting.
;; Only highlights errors, search results, and cursor.

;;; Code:

(require 'alabaster-common)

(deftheme alabaster-mono "Alabaster mono light theme")

(custom-theme-set-variables
 'alabaster-mono
 '(cursor-color alabaster-active)
 '(fringe-mode alabaster-light-line-highlight))

(custom-theme-set-faces
 'alabaster-mono
 ;; Basic appearance - everything monochrome
 `(default ((t (:foreground ,alabaster-light-fg :background ,alabaster-light-bg))))
 `(cursor ((t (:background ,alabaster-active))))
 `(fringe ((t (:background ,alabaster-light-line-highlight))))
 `(line-number ((t (:foreground ,alabaster-grey :background ,alabaster-light-line-highlight))))
 `(line-number-current-line ((t (:foreground ,alabaster-light-fg :background ,alabaster-light-line-highlight))))
 `(hl-line ((t (:background ,alabaster-light-line-highlight))))

 ;; Selection and highlighting
 `(region ((t (:background ,alabaster-light-line-highlight))))
 `(highlight ((t (:background ,alabaster-orange :foreground ,alabaster-light-fg))))
 `(isearch ((t (:background ,alabaster-orange :foreground ,alabaster-light-fg))))
 `(lazy-highlight ((t (:background ,alabaster-orange :foreground ,alabaster-light-fg))))

 ;; Font lock faces - mostly monochrome
 `(font-lock-comment-face ((t (:foreground ,alabaster-grey))))
 `(font-lock-string-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-constant-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-number-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-keyword-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-function-name-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-variable-name-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-type-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-builtin-face ((t (:foreground ,alabaster-light-fg))))
 `(font-lock-punctuation-face ((t (:foreground ,alabaster-light-fg))))

 ;; Error handling - only errors highlighted
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

(provide-theme 'alabaster-mono)
(provide 'alabaster-mono)

;;; alabaster-mono.el ends here