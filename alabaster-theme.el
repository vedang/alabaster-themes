;;; alabaster-theme.el --- Alabaster themes collection -*- lexical-binding:t -*-

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
;;
;; The `alabaster-themes' are a collection of minimal light and dark themes
;; for GNU Emacs based on the original Sublime Text Alabaster color scheme.
;;
;; Available themes:
;; - alabaster (light, foreground highlighting)
;; - alabaster-bg (light, background highlighting)
;; - alabaster-dark (dark, foreground highlighting)
;; - alabaster-mono (light, monochromatic)
;; - alabaster-dark-mono (dark, monochromatic)

;;; Code:

(require 'seq)
(eval-when-compile (require 'subr-x))

(defgroup alabaster-themes ()
  "Minimal Alabaster themes."
  :group 'faces
  :link '(url-link :tag "Homepage" "https://github.com/tonsky/sublime-scheme-alabaster")
  :prefix "alabaster-themes-"
  :tag "Alabaster Themes")

(defconst alabaster-themes-light-themes
  '(alabaster
    alabaster-bg
    alabaster-mono)
  "List of symbols with the light Alabaster themes.")

(defconst alabaster-themes-dark-themes
  '(alabaster-dark
    alabaster-dark-mono)
  "List of symbols with the dark Alabaster themes.")

(defconst alabaster-themes-collection
  (append alabaster-themes-light-themes alabaster-themes-dark-themes)
  "Symbols of all the Alabaster themes.")

(defcustom alabaster-themes-post-load-hook nil
  "Hook that runs after loading an Alabaster theme.
This is used by the commands `alabaster-themes-select' and
`alabaster-themes-load-random'."
  :type 'hook
  :group 'alabaster-themes)

(defcustom alabaster-themes-disable-other-themes t
  "Disable all other themes when loading an Alabaster theme.

When the value is non-nil, the commands will disable all other
themes while loading the specified Alabaster theme.  This is done
to ensure that Emacs does not blend two or more themes: such
blends lead to awkward results that undermine the work of the designer.

When the value is nil, the commands will only disable other themes
within the Alabaster collection."
  :group 'alabaster-themes
  :type 'boolean)

;;; Helper functions

(defun alabaster-themes--retrieve-palette-value (color palette)
  "Return COLOR from PALETTE.
Use recursion until COLOR is retrieved as a string.  Refrain from
doing so if the value of COLOR is not a key in the PALETTE.

Return `unspecified' if the value of COLOR cannot be determined.
This symbol is accepted by faces and is thus harmless."
  (let ((value (car (alist-get color palette))))
    (cond
     ((or (stringp value)
          (eq value 'unspecified))
      value)
     ((and (symbolp value) value)
      (alabaster-themes--retrieve-palette-value value palette))
     (t
      'unspecified))))

(defun alabaster-themes--list-enabled-themes ()
  "Return list of `custom-enabled-themes' with alabaster- prefix."
  (seq-filter
   (lambda (theme)
     (string-prefix-p "alabaster-" (symbol-name theme)))
   custom-enabled-themes))

(defun alabaster-themes--enable-themes (&optional subset)
  "Enable all Alabaster themes.
With optional SUBSET as a symbol of `light' or `dark', enable only those
themes."
  (let ((themes (cond
                 ((eq subset 'dark) alabaster-themes-dark-themes)
                 ((eq subset 'light) alabaster-themes-light-themes)
                 (t alabaster-themes-collection))))
    (mapc
     (lambda (theme)
       (unless (memq theme custom-known-themes)
         (load-theme theme :no-confirm :no-enable)))
     themes)))

(defun alabaster-themes--list-known-themes ()
  "Return list of `custom-known-themes' with alabaster- prefix."
  (alabaster-themes--enable-themes)
  (seq-filter
   (lambda (theme)
     (string-prefix-p "alabaster-" (symbol-name theme)))
   custom-known-themes))

(defun alabaster-themes--current-theme ()
  "Return first enabled Alabaster theme."
  (car (or (alabaster-themes--list-enabled-themes)
           (alabaster-themes--list-known-themes))))

(defun alabaster-themes--palette-symbol (theme &optional overrides)
  "Return THEME palette as a symbol.
With optional OVERRIDES, return THEME palette overrides as a
symbol."
  (when-let* ((suffix (cond
                       ((and theme overrides)
                        "palette-overrides")
                       (theme
                        "palette"))))
    (intern (format "%s-%s" theme suffix))))

(defun alabaster-themes--palette-value (theme &optional overrides)
  "Return palette value of THEME with optional OVERRIDES."
  (let ((base-value (symbol-value (alabaster-themes--palette-symbol theme))))
    (if overrides
        (append (symbol-value (alabaster-themes--palette-symbol theme :overrides))
                alabaster-themes-common-palette-overrides
                base-value)
      base-value)))

(defun alabaster-themes--current-theme-palette (&optional overrides)
  "Return palette value of active Alabaster theme, else produce `user-error'.
With optional OVERRIDES return palette value plus whatever
overrides."
  (if-let* ((theme (alabaster-themes--current-theme)))
      (if overrides
          (alabaster-themes--palette-value theme :overrides)
        (alabaster-themes--palette-value theme))
    (user-error "No enabled Alabaster theme could be found")))

;;; Theme definition macro

(defmacro alabaster-themes-theme (name palette &optional overrides)
  "Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.

Optional OVERRIDES are appended to PALETTE, overriding
corresponding entries."
  (declare (indent 0))
  (let ((sym (gensym))
        (colors (mapcar #'car (symbol-value palette))))
    `(let* ((c '((class color) (min-colors 256)))
            (,sym (alabaster-themes--palette-value ',name ',overrides))
            ,@(mapcar (lambda (color)
                        (list color
                              `(alabaster-themes--retrieve-palette-value ',color ,sym)))
                      colors))
       (custom-theme-set-faces ',name ,@alabaster-themes-faces)
       (custom-theme-set-variables ',name ,@alabaster-themes-custom-variables))))

(defmacro alabaster-themes-with-colors (&rest body)
  "Evaluate BODY with colors from current palette bound."
  (declare (indent 0))
  (let* ((sym (gensym))
         (colors (mapcar #'car (alabaster-themes--current-theme-palette))))
    `(let* ((c '((class color) (min-colors 256)))
            (,sym (alabaster-themes--current-theme-palette :overrides))
            ,@(mapcar (lambda (color)
                        (list color
                              `(alabaster-themes--retrieve-palette-value ',color ,sym)))
                      colors))
       (ignore c ,@colors)
       ,@body)))

;;; Faces and variables

(defgroup alabaster-themes-faces ()
  "Faces defined by the Alabaster themes."
  :group 'alabaster-themes
  :prefix "alabaster-themes-"
  :tag "Alabaster Themes Faces")

;; Define face specs placeholder
(defvar alabaster-themes-faces '()
  "Face specifications for Alabaster themes.
This is populated by the individual theme files.")

(defvar alabaster-themes-custom-variables '()
  "Custom variable specifications for Alabaster themes.")

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'alabaster-theme)
;;; alabaster-theme.el ends here
