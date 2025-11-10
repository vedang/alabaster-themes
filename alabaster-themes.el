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
(provide 'alabaster-themes)

;;;; Theme selection commands

(defun alabaster-themes--annotate-theme (theme)
  "Return completion annotation for THEME."
  (when-let* ((symbol (intern-soft theme))
              (doc-string (get symbol 'theme-documentation)))
    (format " -- %s"
            (propertize
             (car (split-string doc-string "\\."))
             'face 'completions-annotations))))

(defun alabaster-themes--completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

(defun alabaster-themes--load-subset (subset)
  "Return the `light' or `dark' SUBSET of the Alabaster themes.
If SUBSET is neither `light' nor `dark', return all the known Alabaster themes."
  (alabaster-themes--completion-table 'theme (alabaster-themes--enable-themes subset)))

(defun alabaster-themes--maybe-prompt-subset (variant)
  "Helper function for `alabaster-themes--select-prompt' VARIANT argument."
  (cond
   ((null variant))
   ((or (eq variant 'light) (eq variant 'dark)) variant)
   (t (alabaster-themes--choose-subset))))

(defun alabaster-themes--choose-subset ()
  "Use `read-multiple-choice' to return `dark' or `light' variant."
  (intern
   (cadr
    (read-multiple-choice
     "Variant"
     '((?d "dark" "Load a dark theme")
       (?l "light" "Load a light theme"))
     "Limit to the dark or light subset of the Alabaster themes collection."))))

(defvar alabaster-themes--select-theme-history nil
  "Minibuffer history of `alabaster-themes--select-prompt'.")

(defun alabaster-themes--select-prompt (&optional prompt variant)
  "Minibuffer prompt for `alabaster-themes-select'.
With optional PROMPT string, use it.  Else use a generic prompt.

With optional VARIANT as a non-nil value, prompt to limit the
set of themes to either dark or light variants.  Then limit the
completion candidates accordingly.

If VARIANT is either `light' or `dark' then use it directly
instead of prompting the user for a choice.

When VARIANT is nil, all Alabaster themes are candidates for completion."
  (let* ((subset (alabaster-themes--maybe-prompt-subset variant))
         (themes (alabaster-themes--load-subset subset))
         (completion-extra-properties `(:annotation-function ,#'alabaster-themes--annotate-theme)))
    (intern
     (completing-read
      (or prompt "Select Alabaster Theme: ")
      themes
      nil t nil
      'alabaster-themes--select-theme-history))))

(defun alabaster-themes--disable-themes ()
  "Disable themes per `alabaster-themes-disable-other-themes'."
  (mapc #'disable-theme
        (if alabaster-themes-disable-other-themes
            custom-enabled-themes
          (alabaster-themes--list-known-themes))))

(defun alabaster-themes-load-theme (theme)
  "Load THEME while disabling other Alabaster themes.
Which themes are disabled is determined by the user option
`alabaster-themes-disable-other-themes'.

Run the `alabaster-themes-post-load-hook' as the final step after
loading the THEME.

Return THEME."
  (alabaster-themes--disable-themes)
  (load-theme theme :no-confirm)
  (run-hooks 'alabaster-themes-post-load-hook)
  theme)

;;;; Select a theme using minibuffer completion

;;;###autoload
(defun alabaster-themes-select (theme &optional _variant)
  "Load an Alabaster THEME using minibuffer completion.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `alabaster-themes-post-load-hook' after loading the theme.

When called from Lisp, THEME is the symbol of a theme.  VARIANT
is ignored in this scenario."
  (interactive (list (alabaster-themes--select-prompt nil current-prefix-arg)))
  (alabaster-themes-load-theme theme))

;;;###autoload
(defun alabaster-themes-select-light (theme)
  "Load a light Alabaster THEME.
Run `alabaster-themes-post-load-hook' after loading the theme.

Also see `alabaster-themes-select-dark'.

This command is the same as `alabaster-themes-select' except it only
prompts for light themes when called interactively.  Calling it
from Lisp behaves the same as `alabaster-themes-select' for the THEME
argument, meaning that it loads the Alabaster THEME regardless of
whether it is light or dark."
  (interactive
   (list
    (alabaster-themes--select-prompt "Select light Alabaster theme: " 'light)))
  (alabaster-themes-load-theme theme))

;;;###autoload
(defun alabaster-themes-select-dark (theme)
  "Load a dark Alabaster THEME.
Run `alabaster-themes-post-load-hook' after loading the theme.

Also see `alabaster-themes-select-light'.

This command is the same as `alabaster-themes-select' except it only
prompts for dark themes when called interactively.  Calling it
from Lisp behaves the same as `alabaster-themes-select' for the THEME
argument, meaning that it loads the Alabaster THEME regardless of
whether it is light or dark."
  (interactive
   (list
    (alabaster-themes--select-prompt "Select dark Alabaster theme: " 'dark)))
  (alabaster-themes-load-theme theme))
