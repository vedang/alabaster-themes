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

;;;; Theme management commands

(defun alabaster-themes--toggle-theme-p ()
  "Return non-nil if `alabaster-themes-to-toggle' are valid."
  (condition-case nil
      (dolist (theme alabaster-themes-to-toggle)
        (or (memq theme alabaster-themes-collection)
            (memq theme (alabaster-themes--list-known-themes))
            (error "`%s' is not part of `alabaster-themes-collection'" theme)))
    (error nil)
    (:success alabaster-themes-to-toggle)))

;;;###autoload
(defun alabaster-themes-toggle ()
  "Toggle between the two `alabaster-themes-to-toggle'.
If `alabaster-themes-to-toggle' does not specify two Alabaster themes, inform
the user about it while prompting with completion for a theme
among our collection (this is practically the same as the
`alabaster-themes-select' command).

Run `alabaster-themes-post-load-hook' after loading the theme."
  (interactive)
  (if-let* ((themes (alabaster-themes--toggle-theme-p))
            (one (car themes))
            (two (cadr themes)))
      (if (eq (car custom-enabled-themes) one)
          (alabaster-themes-load-theme two)
        (alabaster-themes-load-theme one))
    (alabaster-themes-load-theme
     (alabaster-themes--select-prompt
      (concat "Set two `alabaster-themes-to-toggle'; "
              "switching to theme selection for now: ")))))

;;;; Load a theme at random

(defun alabaster-themes--minus-current (&optional variant)
  "Return list of Alabaster themes minus the current one.
VARIANT is either `light' or `dark', which stand for
`alabaster-themes-light-themes' and `alabaster-themes-dark-themes',
respectively.  Else check against the return value of
`alabaster-themes--list-known-themes'."
  (let* ((list (when variant
                 (if (eq variant 'dark)
                     alabaster-themes-dark-themes
                   alabaster-themes-light-themes)))
         (sequence (or list (alabaster-themes--list-known-themes)))
         (themes (copy-sequence sequence)))
    (delete (alabaster-themes--current-theme) themes)))

;;;###autoload
(defun alabaster-themes-load-random (&optional variant)
  "Load an Alabaster theme at random, excluding the current one.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `alabaster-themes-post-load-hook' after loading the theme.

When called from Lisp, VARIANT is either the `dark' or `light'
symbol."
  (interactive (list (when current-prefix-arg (alabaster-themes--choose-subset))))
  (let* ((themes (alabaster-themes--minus-current variant))
         (n (random (length themes)))
         (pick (nth n themes))
         (loaded (if (null pick) (car themes) pick)))
    (alabaster-themes-load-theme loaded)
    (message "Loaded `%s'" loaded)))

;;;; Rotate through a list of themes

(defun alabaster-themes--rotate (themes)
  "Rotate THEMES rightward such that the car is moved to the end."
  (if (proper-list-p themes)
      (let* ((index (seq-position themes (alabaster-themes--current-theme)))
             (offset (1+ index)))
        (append (nthcdr offset themes) (take offset themes)))
    (error "The `%s' is not a list" themes)))

(defun alabaster-themes--rotate-p (themes)
  "Return a new theme among THEMES if it is possible to rotate to it."
  (if-let* ((new-theme (car (alabaster-themes--rotate themes))))
      (if (eq new-theme (alabaster-themes--current-theme))
          (car (alabaster-themes--rotate-p (alabaster-themes--rotate themes)))
        new-theme)
    (error "Cannot determine a theme among `%s'" themes)))

;;;###autoload
(defun alabaster-themes-rotate (themes)
  "Rotate to the next theme among THEMES.
When called interactively THEMES is the value of `alabaster-themes-to-rotate'.

If the current theme is already the next in line, then move to the one
after.  Perform the rotation rightwards, such that the first element in
the list becomes the last.  Do not modify THEMES in the process."
  (interactive (list alabaster-themes-to-rotate))
  (unless (proper-list-p themes)
    "This is not a list of themes: `%s'" themes)
  (let ((candidate (alabaster-themes--rotate-p themes)))
    (if (memq candidate alabaster-themes-collection)
        (progn
          (message "Rotating to `%s'" (propertize (symbol-name candidate) 'face 'success))
          (alabaster-themes-load-theme candidate))
      (user-error "`%s' is not part of the Alabaster collection" candidate))))


;;;; Preview a theme palette

(defun alabaster-themes--list-colors-get-mappings (palette)
  "Get the semantic palette entries in PALETTE.
PALETTE is the value of a variable like `alabaster-palette'."
  (seq-remove
   (lambda (cell)
     (stringp (cadr cell)))
   palette))

(defun alabaster-themes--list-colors-tabulated (theme &optional mappings)
  "Return a data structure of THEME palette or MAPPINGS for tabulated list."
  (let* ((current-palette (alabaster-themes--palette-value theme mappings))
         (palette (if mappings
                      (alabaster-themes--list-colors-get-mappings current-palette)
                    current-palette)))
    (mapcar (lambda (cell)
              (pcase-let* ((`(,name ,value) cell)
                           (name-string (format "%s" name))
                           (value-string (format "%s" value))
                           (value-string-padded (string-pad value-string 30))
                           (color (alabaster-themes--retrieve-palette-value name current-palette)))
                (list name
                      (vector
                       (if (and (symbolp value)
                                (not (eq value 'unspecified)))
                           "Yes"
                         "")
                       name-string
                       (propertize value-string 'face `( :foreground ,color))
                       (propertize value-string-padded 'face (list :background color
                                                                   :foreground (if (string= color "unspecified")
                                                                                   (readable-foreground-color (alabaster-themes--retrieve-palette-value 'bg-main current-palette))
                                                                                 (readable-foreground-color color))))))))
            palette)))

(defvar alabaster-themes-current-preview nil)
(defvar alabaster-themes-current-preview-show-mappings nil)

(defun alabaster-themes--set-tabulated-entries ()
  "Set the value of `tabulated-list-entries' with palette entries."
  (setq-local tabulated-list-entries
              (alabaster-themes--list-colors-tabulated alabaster-themes-current-preview alabaster-themes-current-preview-show-mappings)))

;;;###autoload
(defun alabaster-themes-list-colors (theme &optional mappings)
  "Preview the palette of the Alabaster THEME of choice.
With optional prefix argument for MAPPINGS preview only the semantic
color mappings instead of the complete palette."
  (interactive
   (let ((prompt (if current-prefix-arg
                     "Preview palette mappings of THEME: "
                   "Preview palette of THEME: ")))
     (list
      (alabaster-themes--select-prompt prompt)
      current-prefix-arg)))
  (let ((buffer (get-buffer-create (format (if mappings "*%s-list-mappings*" "*%s-list-all*") theme))))
    (with-current-buffer buffer
      (let ((alabaster-themes-current-preview theme)
            (alabaster-themes-current-preview-show-mappings mappings))
        (alabaster-themes-preview-mode)))
    (pop-to-buffer buffer)))

(defalias 'alabaster-themes-preview-colors 'alabaster-themes-list-colors
  "Alias for `alabaster-themes-list-colors'.")

;;;###autoload
(defun alabaster-themes-list-colors-current (&optional mappings)
  "Like `alabaster-themes-list-colors' with optional MAPPINGS for the current theme."
  (interactive "P")
  (alabaster-themes-list-colors (alabaster-themes--current-theme) mappings))

(defalias 'alabaster-themes-preview-colors-current 'alabaster-themes-list-colors-current
  "Alias for `alabaster-themes-list-colors-current'.")

(define-derived-mode alabaster-themes-preview-mode tabulated-list-mode "Alabaster palette"
  "Major mode to display a Alabaster themes palette."
  :interactive nil
  (setq-local tabulated-list-format
              [("Mapping?" 10 t)
               ("Symbol name" 30 t)
               ("As foreground" 30 t)
               ("As background" 0 t)])
  (alabaster-themes--set-tabulated-entries)
  (tabulated-list-init-header)
  (tabulated-list-print))


;;;; Heading customization

(defconst alabaster-themes-weights
  '(thin ultralight extralight light semilight regular medium
         semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defconst alabaster-themes--headings-choice
  '(set :tag "Properties" :greedy t
        (const :tag "Proportionately spaced font (variable-pitch)" variable-pitch)
        (choice :tag "Font weight (must be supported by the typeface)"
                (const :tag "Bold (default)" nil)
                (const :tag "Thin" thin)
                (const :tag "Ultra-light" ultralight)
                (const :tag "Extra-light" extralight)
                (const :tag "Light" light)
                (const :tag "Semi-light" semilight)
                (const :tag "Regular" regular)
                (const :tag "Medium" medium)
                (const :tag "Semi-bold" semibold)
                (const :tag "Extra-bold" extrabold)
                (const :tag "Ultra-bold" ultrabold))
        (radio :tag "Height"
               (float :tag "Floating point to adjust height by")
               (cons :tag "Cons cell of `(height . FLOAT)'"
                     (const :tag "The `height' key (constant)" height)
                     (float :tag "Floating point"))))
  "Refer to the doc string of `alabaster-themes-headings'.
This is a helper variable intended for internal use.")

(defcustom alabaster-themes-headings nil
  "Heading styles with optional list of values per heading level.

This is an alist that accepts a (KEY . LIST-OF-VALUES)
combination.  The KEY is either a number, representing the
heading's level (0-8) or t, which pertains to the fallback style.
The named keys `agenda-date' and `agenda-structure' apply to the
Org agenda.

Level 0 is used for what counts as a document title or
equivalent, such as the #+title construct we find in Org files.
Levels 1-8 are regular headings.

The LIST-OF-VALUES covers symbols that refer to properties, as
described below.  Here is a complete sample with various
stylistic combinations, followed by a presentation of all
available properties:

    (setq alabaster-themes-headings
          (quote ((1 light variable-pitch 1.5)
                  (2 regular 1.3)
                  (3 1.1)
                  (agenda-date 1.3)
                  (agenda-structure variable-pitch light 1.8)
                  (t variable-pitch))))

By default (a nil value for this variable), all headings have a
bold typographic weight, a font family that is the same as the
`default' face (typically monospaced), and a height that is equal
to the `default' face's height.

- A `variable-pitch' property changes the font family of the
  heading to that of the `variable-pitch' face (normally a
  proportionately spaced typeface).

- The symbol of a weight attribute adjusts the font of the
  heading accordingly, such as `light', `semibold', etc.  Valid
  symbols are defined in the variable `alabaster-themes-weights'.
  The absence of a weight means that bold will be used by virtue
  of inheriting the `bold' face.

- A number, expressed as a floating point (e.g. 1.5), adjusts the
  height of the heading to that many times the base font size.
  The default height is the same as 1.0, though it need not be
  explicitly stated.  Instead of a floating point, an acceptable
  value can be in the form of a cons cell like (height . FLOAT)
  or (height FLOAT), where FLOAT is the given number.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (semibold)
    (variable-pitch semibold)
    (variable-pitch semibold 1.3)
    (variable-pitch semibold (height 1.3))   ; same as above
    (variable-pitch semibold (height . 1.3)) ; same as above

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq alabaster-themes-headings
          (quote ((1 light variable-pitch 1.5)
                  (2 regular 1.3)
                  (3 1.1)
                  (t variable-pitch))))

When defining the styles per heading level, it is possible to
pass a non-nil non-list value (e.g. t) instead of a list of
properties.  This will retain the original aesthetic for that
level.  For example:

    (setq alabaster-themes-headings
          (quote ((1 . t)           ; keep the default style
                  (2 variable-pitch 1.2)
                  (t variable-pitch)))) ; style for all other headings

    (setq alabaster-themes-headings
          (quote ((1 variable-pitch 1.6)
                  (2 1.3)
                  (t . t)))) ; default style for all other levels"
  :group 'alabaster-themes
  :type `(alist
          :options ,(mapcar (lambda (el)
                              (list el alabaster-themes--headings-choice))
                            '(0 1 2 3 4 5 6 7 8 t agenda-date agenda-structure))
          :key-type symbol
          :value-type ,alabaster-themes--headings-choice))

(defun alabaster-themes--weight (list)
  "Search for `alabaster-themes--heading' weight in LIST."
  (catch 'found
    (dolist (elt list)
      (when (memq elt alabaster-themes-weights)
        (throw 'found elt)))))

(defun alabaster-themes--property-lookup (properties alist-key list-pred default)
  "Return value from property alist or list.
Check PROPERTIES for an alist value that corresponds to
ALIST-KEY.  If no alist is present, search the PROPERTIES
list given LIST-PRED, using DEFAULT as a fallback."
  (if-let* ((val (or (alist-get alist-key properties)
                     (seq-filter (lambda (x) (funcall list-pred x)) properties)
                     default))
            ((listp val)))
      (car val)
    val))

(defun alabaster-themes--heading (level)
  "Conditional styles for `alabaster-themes-headings' per LEVEL heading."
  (let* ((key (alist-get level alabaster-themes-headings))
         (style (or key (alist-get t alabaster-themes-headings)))
         (style-listp (listp style))
         (properties style)
         (var (when (and style-listp (memq 'variable-pitch properties)) 'variable-pitch))
         (weight (when style-listp (alabaster-themes--weight style))))
    (list :inherit
          (cond
           ((not style-listp) 'bold)
           (weight var)
           (var (append (list 'bold) (list var)))
           (t 'bold))
          :height
          (if style-listp
              (alabaster-themes--property-lookup properties 'height #'floatp 'unspecified)
            'unspecified)
          :weight
          (or weight 'unspecified))))


;;;; Font options

(defcustom alabaster-themes-mixed-fonts nil
  "Non-nil to enable inheritance from `fixed-pitch' in some faces.

This is done to allow spacing-sensitive constructs, such as Org
tables and code blocks, to remain monospaced when users opt for a
proportionately spaced font as their default or when they use
something like the command `variable-pitch-mode'."
  :group 'alabaster-themes
  :type 'boolean)

(defcustom alabaster-themes-variable-pitch-ui nil
  "Use proportional fonts (`variable-pitch') in UI elements.
This includes the mode line, header line, tab bar, and tab line."
  :group 'alabaster-themes
  :type 'boolean)

(defun alabaster-themes--fixed-pitch ()
  "Conditional application of `fixed-pitch' inheritance."
  (when alabaster-themes-mixed-fonts
    (list :inherit 'fixed-pitch)))

(defun alabaster-themes--variable-pitch-ui ()
  "Conditional application of `variable-pitch' in the UI."
  (when alabaster-themes-variable-pitch-ui
    (list :inherit 'variable-pitch)))

