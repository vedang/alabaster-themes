;;; alabaster-theme.el --- Alabaster theme package for Emacs -*- lexical-binding:t -*-

;;; Commentary:
;; Alabaster theme package with all 5 variations.
;; Minimal highlighting based on the original Sublime Text design philosophy.
;;
;; Available themes:
;; - alabaster (light, foreground highlighting)
;; - alabaster-bg (light, background highlighting)
;; - alabaster-dark (dark, foreground highlighting)
;; - alabaster-mono (light, monochromatic)
;; - alabaster-dark-mono (dark, monochromatic)
;;
;; Installation:
;;   (require 'alabaster-theme)
;;   (load-theme 'alabaster t) ; or any other variant
;;
;; Customization:
;;   All colors are customizable via the alabaster-* variables.
;;   See the group 'alabaster' for customization options.

;;; Code:

;; Define customization group
(defgroup alabaster nil
  "Alabaster theme customization."
  :group 'faces
  :prefix "alabaster-")

;; Load common definitions
(require 'alabaster-common)

;; Theme loaders
(defcustom alabaster-theme 'alabaster
  "Default Alabaster theme variant to load."
  :type '(choice (const :tag "Alabaster (light)" alabaster)
                 (const :tag "Alabaster BG (light, background)" alabaster-bg)
                 (const :tag "Alabaster Dark" alabaster-dark)
                 (const :tag "Alabaster Mono (light)" alabaster-mono)
                 (const :tag "Alabaster Dark Mono" alabaster-dark-mono))
  :group 'alabaster)

;;;###autoload
(defun alabaster-load-theme (&optional variant)
  "Load Alabaster theme variant.
VARIANT should be one of: alabaster, alabaster-bg, alabaster-dark,
alabaster-mono, alabaster-dark-mono. Defaults to `alabaster-theme'."
  (interactive
   (list (intern (completing-read "Theme variant: "
                                  '(alabaster alabaster-bg alabaster-dark
                                           alabaster-mono alabaster-dark-mono)
                                  nil t))))
  (require (intern (format "alabaster%s"
                          (if (eq variant 'alabaster) ""
                            (concat "-" (symbol-name variant))))))
  (load-theme variant t))

;;;###autoload
(defun alabaster-switch-theme ()
  "Cycle through all Alabaster theme variants."
  (interactive)
  (let* ((themes '(alabaster alabaster-bg alabaster-dark alabaster-mono alabaster-dark-mono))
         (current (car (seq-filter (lambda (theme) (custom-theme-enabled-p theme)) themes)))
         (next (or (cadr (member current themes)) (car themes))))
    (when current
      (disable-theme current))
    (when next
      (load-theme next t)
      (message "Loaded %s theme" next))))

;; Auto-load themes when package is loaded
;;;###autoload
(and load-file-name
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))

(provide 'alabaster-theme)
;;; alabaster-theme.el ends here

;; Theme metadata for MELPA
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces theme minimal
;; URL: https://github.com/tonsky/sublime-scheme-alabaster
;; Version: 1.0.0