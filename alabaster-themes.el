;;; alabaster-theme.el --- Alabaster themes collection -*- lexical-binding:t -*-

;; Copyright (C) 2025 Nikita Prokopov

;; Author: Nikita Prokopov
;; Maintainer: Vedang Manerikar
;; URL: https://github.com/vedang/alabaster-themes
;; Version: 1.0.1
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
  :link '(url-link :tag "Homepage" "https://github.com/vedang/alabaster-themes")
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

(defmacro alabaster-themes-theme (name palette &optional overrides faces)
  "Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.

Optional OVERRIDES are appended to PALETTE, overriding
corresponding entries.

Optional FACES can be used to provide custom face specifications
instead of the default `alabaster-themes-faces'."
  (declare (indent 0))
  (let ((sym (gensym))
        (colors (mapcar #'car (symbol-value palette))))
    `(let* ((c '((class color) (min-colors 256)))
            (,sym (alabaster-themes--palette-value ',name ',overrides))
            ,@(mapcar (lambda (color)
                        (list color
                              `(alabaster-themes--retrieve-palette-value ',color ,sym)))
                      colors))
       (custom-theme-set-faces ',name ,@(or (and faces (symbol-value faces)) alabaster-themes-faces))
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

;; Define face specs
(defvar alabaster-themes-faces
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

;;;; font-lock faces
    `(font-lock-comment-face ((,c :foreground ,comment)))
    `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
    `(font-lock-string-face ((,c :foreground ,string)))
    `(font-lock-doc-face ((,c :inherit font-lock-string-face)))
    `(font-lock-keyword-face ((,c :foreground ,keyword)))
    `(font-lock-builtin-face ((,c :foreground ,builtin)))
    `(font-lock-function-name-face ((,c :foreground ,fnname)))
    `(font-lock-variable-name-face ((,c :foreground ,variable)))
    `(font-lock-type-face ((,c :foreground ,type)))
    `(font-lock-constant-face ((,c :foreground ,constant)))
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

;;;; helper faces
    `(alabaster-themes-heading-0 ((,c ,@(alabaster-themes--heading 0) :foreground ,blue)))
    `(alabaster-themes-heading-1 ((,c ,@(alabaster-themes--heading 1) :foreground ,blue)))
    `(alabaster-themes-heading-2 ((,c ,@(alabaster-themes--heading 2) :foreground ,blue)))
    `(alabaster-themes-heading-3 ((,c ,@(alabaster-themes--heading 3) :foreground ,blue)))
    `(alabaster-themes-heading-4 ((,c ,@(alabaster-themes--heading 4) :foreground ,blue)))
    `(alabaster-themes-heading-5 ((,c ,@(alabaster-themes--heading 5) :foreground ,blue)))
    `(alabaster-themes-heading-6 ((,c ,@(alabaster-themes--heading 6) :foreground ,blue)))
    `(alabaster-themes-heading-7 ((,c ,@(alabaster-themes--heading 7) :foreground ,blue)))
    `(alabaster-themes-heading-8 ((,c ,@(alabaster-themes--heading 8) :foreground ,blue)))
    `(alabaster-themes-mark-delete ((,c :inherit error :background ,bg-err)))
    `(alabaster-themes-mark-select ((,c :inherit success :background ,bg-info)))
    `(alabaster-themes-mark-other ((,c :inherit warning :background ,bg-warning)))
    `(alabaster-themes-search-current ((,c :background ,bg-search-current :foreground ,fg-intense)))
    `(alabaster-themes-search-lazy ((,c :background ,bg-search-lazy :foreground ,fg-intense)))
    `(alabaster-themes-search-replace ((,c :background ,bg-search-replace :foreground ,fg-intense)))
    `(alabaster-themes-search-rx-group-0 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-intense)))
    `(alabaster-themes-search-rx-group-1 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-intense)))
    `(alabaster-themes-search-rx-group-2 ((,c :background ,bg-search-rx-group-2 :foreground ,fg-intense)))
    `(alabaster-themes-search-rx-group-3 ((,c :background ,bg-search-rx-group-3 :foreground ,fg-intense)))
    `(alabaster-themes-search-match ((,c :background ,bg-search-match)))
    `(alabaster-themes-underline-error ((,c :underline (:style wave :color ,underline-err))))
    `(alabaster-themes-underline-info ((,c :underline (:style wave :color ,underline-info))))
    `(alabaster-themes-underline-warning ((,c :underline (:style wave :color ,underline-warning))))

;;;; magit faces
    `(magit-bisect-bad ((,c :inherit error)))
    `(magit-bisect-good ((,c :inherit success)))
    `(magit-bisect-skip ((,c :inherit warning)))
    `(magit-blame-dimmed ((,c :inherit shadow)))
    `(magit-blame-highlight ((,c :background ,bg-active :foreground ,fg-intense)))
    `(magit-branch-local ((,c :foreground ,blue)))
    `(magit-branch-remote ((,c :foreground ,magenta)))
    `(magit-branch-upstream ((,c :inherit italic)))
    `(magit-branch-warning ((,c :inherit warning)))
    `(magit-cherry-equivalent ((,c :foreground ,magenta)))
    `(magit-cherry-unmatched ((,c :foreground ,blue)))
    `(magit-diff-added ((,c :background ,bg-added-faint :foreground ,fg-added)))
    `(magit-diff-added-highlight ((,c :background ,bg-added :foreground ,fg-added)))
    `(magit-diff-base ((,c :background ,bg-changed-faint :foreground ,fg-changed)))
    `(magit-diff-base-highlight ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(magit-diff-context ((,c :inherit shadow)))
    `(magit-diff-context-highlight ((,c :background ,bg-dim)))
    `(magit-diff-file-heading ((,c ,@(alabaster-themes--bold) :foreground ,blue)))
    `(magit-diff-file-heading-highlight ((,c :inherit magit-diff-file-heading :background ,bg-alt)))
    `(magit-diff-file-heading-selection ((,c ,@(alabaster-themes--bold) :background ,bg-hover :foreground ,fg-intense)))
    `(magit-diff-hunk-heading ((,c :background ,bg-alt)))
    `(magit-diff-hunk-heading-highlight ((,c ,@(alabaster-themes--bold) :background ,bg-active :foreground ,fg-intense)))
    `(magit-diff-hunk-heading-selection ((,c ,@(alabaster-themes--bold) :background ,bg-hover :foreground ,fg-intense)))
    `(magit-diff-hunk-region ((,c ,@(alabaster-themes--bold))))
    `(magit-diff-removed ((,c :background ,bg-removed-faint :foreground ,fg-removed)))
    `(magit-diff-removed-highlight ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(magit-diff-whitespace-warning ((,c :inherit warning)))
    `(magit-dimmed ((,c :inherit shadow)))
    `(magit-hash ((,c :foreground ,fg-dim)))
    `(magit-head ((,c :foreground ,blue)))
    `(magit-header-line ((,c ,@(alabaster-themes--bold) :background ,bg-alt)))
    `(magit-log-author ((,c :foreground ,fg-alt)))
    `(magit-log-date ((,c :foreground ,fg-dim)))
    `(magit-log-graph ((,c :foreground ,border)))
    `(magit-popup-argument ((,c ,@(alabaster-themes--bold) :foreground ,yellow)))
    `(magit-popup-disabled-argument ((,c :inherit shadow)))
    `(magit-popup-heading ((,c ,@(alabaster-themes--bold) :foreground ,blue)))
    `(magit-popup-key ((,c :foreground ,keybind)))
    `(magit-popup-option-value ((,c :foreground ,constant)))
    `(magit-process-ng ((,c :inherit error)))
    `(magit-process-ok ((,c :inherit success)))
    `(magit-reflog-amend ((,c :foreground ,magenta)))
    `(magit-reflog-checkout ((,c :foreground ,blue)))
    `(magit-reflog-cherry-pick ((,c :foreground ,green)))
    `(magit-reflog-commit ((,c :foreground ,green)))
    `(magit-reflog-merge ((,c :foreground ,green)))
    `(magit-reflog-other ((,c :foreground ,magenta)))
    `(magit-reflog-rebase ((,c :foreground ,magenta)))
    `(magit-reflog-remote ((,c :foreground ,blue)))
    `(magit-reflog-reset ((,c :inherit error)))
    `(magit-refname ((,c :foreground ,fg-dim)))
    `(magit-section-heading ((,c ,@(alabaster-themes--bold) :foreground ,blue)))
    `(magit-section-heading-selection ((,c ,@(alabaster-themes--bold) :background ,bg-hover :foreground ,fg-intense)))
    `(magit-section-highlight ((,c :background ,bg-alt)))
    `(magit-sequence-drop ((,c :foreground ,fg-dim)))
    `(magit-sequence-head ((,c :foreground ,blue)))
    `(magit-sequence-part ((,c :foreground ,yellow)))
    `(magit-sequence-stop ((,c :foreground ,green)))
    `(magit-signature-bad ((,c :inherit error)))
    `(magit-signature-error ((,c :inherit error)))
    `(magit-signature-expired ((,c :inherit warning)))
    `(magit-signature-good ((,c :inherit success)))
    `(magit-signature-revoked ((,c :inherit warning)))
    `(magit-signature-untrusted ((,c :inherit warning)))
    `(magit-tag ((,c :foreground ,constant)))

;;;; org faces
    `(org-agenda-calendar-daterange ((,c :foreground ,fg-alt)))
    `(org-agenda-calendar-event ((,c :foreground ,fg-main)))
    `(org-agenda-calendar-sexp ((,c :inherit (italic org-agenda-calendar-event))))
    `(org-agenda-clocking ((,c :background ,bg-warning :foreground ,warning)))
    `(org-agenda-column-dateline ((,c :background ,bg-alt)))
    `(org-agenda-current-time ((,c :foreground ,fg-intense)))
    `(org-agenda-date ((,c :foreground ,blue)))
    `(org-agenda-date-today ((,c :inherit org-agenda-date :underline t)))
    `(org-agenda-date-weekend ((,c :inherit org-agenda-date :foreground ,fg-alt)))
    `(org-agenda-date-weekend-today ((,c :inherit org-agenda-date-today :foreground ,fg-alt)))
    `(org-agenda-diary ((,c :inherit org-agenda-calendar-sexp)))
    `(org-agenda-dimmed-todo-face ((,c :inherit shadow)))
    `(org-agenda-done ((,c :foreground ,fg-dim)))
    `(org-agenda-filter-category ((,c ,@(alabaster-themes--bold) :foreground ,err)))
    `(org-agenda-filter-effort ((,c ,@(alabaster-themes--bold) :foreground ,err)))
    `(org-agenda-filter-regexp ((,c ,@(alabaster-themes--bold) :foreground ,err)))
    `(org-agenda-filter-tags ((,c ,@(alabaster-themes--bold) :foreground ,err)))
    `(org-agenda-restriction-lock ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(org-agenda-structure ((,c ,@(alabaster-themes--bold) :foreground ,fg-alt)))
    `(org-agenda-structure-filter ((,c :inherit org-agenda-structure :foreground ,warning)))
    `(org-agenda-structure-secondary ((,c :inherit font-lock-doc-face)))
    `(org-archived ((,c :background ,bg-alt :foreground ,fg-main)))
    `(org-block ((,c :background ,bg-inactive :extend t)))
    `(org-block-begin-line ((,c :inherit (shadow) :background ,bg-dim :extend t)))
    `(org-block-end-line ((,c :inherit org-block-begin-line)))
    `(org-checkbox ((,c :foreground ,warning)))
    `(org-checkbox-statistics-done ((,c :foreground ,fg-dim)))
    `(org-checkbox-statistics-todo ((,c :foreground ,warning)))
    `(org-clock-overlay ((,c :background ,bg-hover)))
    `(org-code ((,c :foreground ,fg-alt)))
    `(org-column ((,c :background ,bg-alt)))
    `(org-column-title ((,c ,@(alabaster-themes--bold) :background ,bg-active :foreground ,fg-intense)))
    `(org-date ((,c :foreground ,blue :underline t)))
    `(org-date-selected ((,c :foreground ,fg-intense :inverse-video t)))
    `(org-default ((,c :inherit default)))
    `(org-document-info ((,c :foreground ,fg-dim)))
    `(org-document-info-keyword ((,c :foreground ,comment)))
    `(org-document-title ((,c ,@(alabaster-themes--bold) :foreground ,blue :height 1.2)))
    `(org-done ((,c :foreground ,fg-dim)))
    `(org-drawer ((,c :foreground ,comment)))
    `(org-ellipsis ((,c :foreground ,fg-dim)))
    `(org-footnote ((,c :foreground ,blue :underline t)))
    `(org-formula ((,c :foreground ,yellow)))
    `(org-headline-done ((,c :foreground ,fg-dim)))
    `(org-hide ((,c :foreground ,bg-main)))
    `(org-latex-and-related ((,c :foreground ,magenta)))
    `(org-level-1 ((,c ,@(alabaster-themes--heading 1) :foreground ,blue)))
    `(org-level-2 ((,c ,@(alabaster-themes--heading 2) :foreground ,blue)))
    `(org-level-3 ((,c ,@(alabaster-themes--heading 3) :foreground ,blue)))
    `(org-level-4 ((,c ,@(alabaster-themes--heading 4) :foreground ,blue)))
    `(org-level-5 ((,c ,@(alabaster-themes--heading 5) :foreground ,blue)))
    `(org-level-6 ((,c ,@(alabaster-themes--heading 6) :foreground ,blue)))
    `(org-level-7 ((,c ,@(alabaster-themes--heading 7) :foreground ,blue)))
    `(org-level-8 ((,c ,@(alabaster-themes--heading 8) :foreground ,blue)))
    `(org-link ((,c :foreground ,link :underline t)))
    `(org-list-dt ((,c ,@(alabaster-themes--bold))))
    `(org-macro ((,c :foreground ,magenta)))
    `(org-meta-line ((,c :foreground ,comment)))
    `(org-mode-line-clock ((,c :inherit mode-line)))
    `(org-mode-line-clock-overrun ((,c :inherit mode-line :foreground ,err)))
    `(org-priority ((,c :foreground ,yellow)))
    `(org-property-value ((,c :foreground ,fg-alt)))
    `(org-quote ((,c :inherit org-block)))
    `(org-scheduled ((,c :foreground ,fg-main)))
    `(org-scheduled-previously ((,c :foreground ,warning)))
    `(org-scheduled-today ((,c :foreground ,fg-intense)))
    `(org-sexp-date ((,c :foreground ,blue)))
    `(org-special-keyword ((,c :foreground ,comment)))
    `(org-table ((,c :foreground ,fg-main)))
    `(org-tag ((,c :foreground ,comment)))
    `(org-tag-group ((,c :inherit org-tag :weight bold)))
    `(org-target ((,c :foreground ,blue)))
    `(org-time-grid ((,c :foreground ,fg-dim)))
    `(org-todo ((,c :foreground ,warning)))
    `(org-upcoming-deadline ((,c :foreground ,warning)))
    `(org-verbatim ((,c :foreground ,fg-alt)))
    `(org-verse ((,c :inherit org-block)))
    `(org-warning ((,c :inherit warning)))

;;;; dired faces
    `(dired-broken-symlink ((,c :inherit (error link))))
    `(dired-directory ((,c :foreground ,blue)))
    `(dired-flagged ((,c :inherit alabaster-themes-mark-delete)))
    `(dired-header ((,c ,@(alabaster-themes--bold))))
    `(dired-ignored ((,c :inherit shadow)))
    `(dired-mark ((,c :foreground ,fg-intense)))
    `(dired-marked ((,c :inherit alabaster-themes-mark-select)))
    `(dired-symlink ((,c :inherit link)))
    `(dired-warning ((,c :inherit warning)))
;;;; dired-subtree
    `(dired-subtree-depth-1-face (( )))
    `(dired-subtree-depth-2-face (( )))
    `(dired-subtree-depth-3-face (( )))
    `(dired-subtree-depth-4-face (( )))
    `(dired-subtree-depth-5-face (( )))
    `(dired-subtree-depth-6-face (( )))
;;;; diredfl
    `(diredfl-deletion ((,c :inherit dired-flagged)))
    `(diredfl-dir-name ((,c :inherit dired-directory)))
    `(diredfl-dir-priv ((,c :inherit dired-directory)))
    `(diredfl-flag-mark ((,c :inherit dired-marked)))
    `(diredfl-flag-mark-line ((,c :inherit dired-marked)))
    `(diredfl-symlink ((,c :inherit dired-symlink)))
    `(diredfl-tagged-autofile-name ((,c :inherit (diredfl-autofile-name dired-marked))))
;;;; image-dired
    `(image-dired-thumb-flagged ((,c :background ,err)))
    `(image-dired-thumb-header-file-name ((,c ,@(alabaster-themes--bold))))
    `(image-dired-thumb-header-file-size ((,c :foreground ,info)))
    `(image-dired-thumb-mark ((,c :background ,info)))

;;;; consult faces
    `(consult-async-split ((,c :inherit warning)))
    `(consult-file ((,c :foreground ,name)))
    `(consult-key ((,c :foreground ,keybind)))
    `(consult-imenu-prefix ((,c :inherit shadow)))
    `(consult-line-number ((,c :inherit shadow)))
    `(consult-line-number-prefix ((,c :inherit shadow)))
    `(consult-separator ((,c :foreground ,border)))

;;;; company-mode
    `(company-echo-common ((,c ,@(alabaster-themes--bold) :foreground ,blue)))
    `(company-preview ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,c :inherit company-echo-common)))
    `(company-preview-search ((,c :background ,bg-yellow-subtle :foreground ,fg-intense)))
    `(company-scrollbar-bg ((,c :background ,bg-active)))
    `(company-scrollbar-fg ((,c :background ,fg-main)))
    `(company-template-field ((,c :background ,bg-active :foreground ,fg-intense)))
    `(company-tooltip ((,c :background ,bg-inactive)))
    `(company-tooltip-annotation ((,c :foreground ,fg-dim)))
    `(company-tooltip-common ((,c :inherit company-echo-common)))
    `(company-tooltip-deprecated ((,c :inherit company-tooltip :strike-through t)))
    `(company-tooltip-mouse ((,c :inherit highlight)))
    `(company-tooltip-scrollbar-thumb ((,c :background ,fg-alt)))
    `(company-tooltip-scrollbar-track ((,c :background ,bg-alt)))
    `(company-tooltip-search ((,c :background ,bg-hover)))
    `(company-tooltip-search-selection ((,c :background ,bg-hover :underline t)))
    `(company-tooltip-selection ((,c :background ,bg-completion)))

;;;; corfu
    `(corfu-current ((,c :background ,bg-completion)))
    `(corfu-bar ((,c :background ,fg-main)))
    `(corfu-border ((,c :background ,bg-active)))
    `(corfu-default ((,c :background ,bg-inactive)))
    `(corfu-candidate-overlay-face ((,c :inherit shadow)))
    `(corfu-quick1 ((,c ,@(alabaster-themes--bold) :background ,bg-red-subtle)))
    `(corfu-quick2 ((,c ,@(alabaster-themes--bold) :background ,bg-green-subtle)))

;;;; all-the-icons
    `(all-the-icons-blue ((,c :foreground ,blue)))
    `(all-the-icons-blue-alt ((,c :foreground ,blue)))
    `(all-the-icons-cyan ((,c :foreground ,blue)))
    `(all-the-icons-cyan-alt ((,c :foreground ,blue)))
    `(all-the-icons-dblue ((,c :foreground ,blue)))
    `(all-the-icons-dcyan ((,c :foreground ,blue)))
    `(all-the-icons-dgreen ((,c :foreground ,fg-dim)))
    `(all-the-icons-dmaroon ((,c :foreground ,magenta)))
    `(all-the-icons-dorange ((,c :foreground ,red)))
    `(all-the-icons-dpink ((,c :foreground ,magenta)))
    `(all-the-icons-dpurple ((,c :foreground ,magenta)))
    `(all-the-icons-dred ((,c :foreground ,red)))
    `(all-the-icons-dsilver ((,c :foreground ,blue)))
    `(all-the-icons-dyellow ((,c :foreground ,yellow)))
    `(all-the-icons-green ((,c :foreground ,green)))
    `(all-the-icons-lblue ((,c :foreground ,blue)))
    `(all-the-icons-lcyan ((,c :foreground ,blue)))
    `(all-the-icons-lgreen ((,c :foreground ,green)))
    `(all-the-icons-lmaroon ((,c :foreground ,magenta)))
    `(all-the-icons-lorange ((,c :foreground ,red)))
    `(all-the-icons-lpink ((,c :foreground ,magenta)))
    `(all-the-icons-lpurple ((,c :foreground ,magenta)))
    `(all-the-icons-lred ((,c :foreground ,red)))
    `(all-the-icons-lsilver ((,c :foreground "gray50")))
    `(all-the-icons-lyellow ((,c :foreground ,yellow)))
    `(all-the-icons-maroon ((,c :foreground ,magenta)))
    `(all-the-icons-orange ((,c :foreground ,yellow)))
    `(all-the-icons-pink ((,c :foreground ,magenta)))
    `(all-the-icons-purple ((,c :foreground ,magenta)))
    `(all-the-icons-purple-alt ((,c :foreground ,blue)))
    `(all-the-icons-red ((,c :foreground ,red)))
    `(all-the-icons-silver ((,c :foreground ,blue)))
    `(all-the-icons-yellow ((,c :foreground ,yellow)))
    `(all-the-icons-dired-dir-face ((,c :foreground ,blue)))

;;;; helpful
    `(helpful-heading ((,c :inherit alabaster-themes-heading-1)))

;;;; marginalia
    `(marginalia-archive ((,c :foreground ,blue)))
    `(marginalia-char ((,c :foreground ,yellow)))
    `(marginalia-date ((,c :foreground ,fg-alt)))
    `(marginalia-documentation ((,c :inherit italic :foreground ,docstring)))
    `(marginalia-file-name (( )))
    `(marginalia-file-owner ((,c :inherit shadow)))
    `(marginalia-file-priv-dir (( )))
    `(marginalia-file-priv-exec ((,c :foreground ,magenta)))
    `(marginalia-file-priv-link ((,c :foreground ,link)))
    `(marginalia-file-priv-no ((,c :inherit shadow)))
    `(marginalia-file-priv-other ((,c :foreground ,blue)))
    `(marginalia-file-priv-rare ((,c :foreground ,blue)))
    `(marginalia-file-priv-read ((,c :foreground ,green)))
    `(marginalia-file-priv-write ((,c :foreground ,yellow)))
    `(marginalia-function ((,c :foreground ,fnname)))
    `(marginalia-key ((,c :foreground ,keybind)))
    `(marginalia-lighter ((,c :inherit shadow)))
    `(marginalia-mode ((,c :foreground ,constant)))
    `(marginalia-modified ((,c :inherit warning)))
    `(marginalia-null ((,c :inherit shadow)))
    `(marginalia-number ((,c :foreground ,constant)))
    `(marginalia-size ((,c :foreground ,variable)))
    `(marginalia-string ((,c :foreground ,string)))
    `(marginalia-symbol ((,c :foreground ,builtin)))
    `(marginalia-true (( )))
    `(marginalia-type ((,c :foreground ,type)))
    `(marginalia-value ((,c :inherit shadow)))
    `(marginalia-version ((,c :foreground ,magenta)))

;;;; which-key
    `(which-key-command-description-face ((,c :foreground ,fg-main)))
    `(which-key-group-description-face ((,c :foreground ,keyword)))
    `(which-key-highlighted-command-face ((,c :foreground ,warning :underline t)))
    `(which-key-key-face ((,c :foreground ,keybind)))
    `(which-key-local-map-description-face ((,c :foreground ,fg-main)))
    `(which-key-note-face ((,c :inherit shadow)))
    `(which-key-separator-face ((,c :inherit shadow)))
    `(which-key-special-key-face ((,c :inherit error)))

;;;; vertico
    `(vertico-current ((,c :background ,bg-completion)))
    `(vertico-group-title ((,c ,@(alabaster-themes--bold) :foreground ,name)))
;;;; vertico-quick
    `(vertico-quick1 ((,c ,@(alabaster-themes--bold) :background ,bg-char-0)))
    `(vertico-quick2 ((,c ,@(alabaster-themes--bold) :background ,bg-char-1)))

;;;; orderless
    `(orderless-match-face-0 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-0)))
    `(orderless-match-face-1 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-1)))
    `(orderless-match-face-2 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-2)))
    `(orderless-match-face-3 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-3)))

;;;; embark
    `(embark-collect-group-title ((,c ,@(alabaster-themes--bold) :foreground ,name)))
    `(embark-keybinding ((,c :foreground ,keybind)))
    `(embark-keybinding-repeat ((,c ,@(alabaster-themes--bold))))
    `(embark-selected ((,c :inherit alabaster-themes-mark-select)))

;;;; flycheck
    `(flycheck-error ((,c :inherit alabaster-themes-underline-error)))
    `(flycheck-fringe-error ((,c :inherit alabaster-themes-mark-delete)))
    `(flycheck-fringe-info ((,c :inherit alabaster-themes-mark-select)))
    `(flycheck-fringe-warning ((,c :inherit alabaster-themes-mark-other)))
    `(flycheck-info ((,c :inherit alabaster-themes-underline-info)))
    `(flycheck-warning ((,c :inherit alabaster-themes-underline-warning)))

;;;; flymake
    `(flymake-error ((,c :inherit alabaster-themes-underline-error)))
    `(flymake-error-echo ((,c :inherit error)))
    `(flymake-note ((,c :inherit alabaster-themes-underline-info)))
    `(flymake-note-echo ((,c :inherit success)))
    `(flymake-warning ((,c :inherit alabaster-themes-underline-warning)))
    `(flymake-warning-echo ((,c :inherit warning)))

;;;; diff-hl
    `(diff-hl-change ((,c :background ,bg-changed-refine)))
    `(diff-hl-delete ((,c :background ,bg-removed-refine)))
    `(diff-hl-insert ((,c :background ,bg-added-refine)))

;;;; avy
    `(avy-background-face ((,c :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,c ,@(alabaster-themes--bold) :background ,bg-active)))
    `(avy-lead-face ((,c ,@(alabaster-themes--bold) :background ,bg-char-0)))
    `(avy-lead-face-0 ((,c ,@(alabaster-themes--bold) :background ,bg-char-1)))
    `(avy-lead-face-1 ((,c :background ,bg-inactive)))
    `(avy-lead-face-2 ((,c ,@(alabaster-themes--bold) :background ,bg-char-2)))

;;;; show-paren
    `(show-paren-match ((,c :background ,bg-paren :foreground ,fg-intense)))
    `(show-paren-match-expression ((,c :background ,bg-alt)))
    `(show-paren-mismatch ((,c :background ,bg-red-intense :foreground ,fg-intense)))

;;;; git-gutter-fringe
    `(git-gutter-fr:added ((,c :background ,bg-added :foreground ,fg-added)))
    `(git-gutter-fr:deleted ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(git-gutter-fr:modified ((,c :background ,bg-changed :foreground ,fg-changed)))

;;;; highlight-indentation
    `(highlight-indentation-face ((,c :background ,bg-dim)))

;;;; rainbow-delimiters
    `(rainbow-delimiters-base-error-face ((,c :inherit ,(if alabaster-themes-no-bold
                                                            '(show-paren-mismatch)
                                                          '(bold show-paren-mismatch)))))
    `(rainbow-delimiters-base-face    ((,c :foreground ,rainbow-0)))
    `(rainbow-delimiters-depth-1-face ((,c :foreground ,rainbow-0)))
    `(rainbow-delimiters-depth-2-face ((,c :foreground ,rainbow-1)))
    `(rainbow-delimiters-depth-3-face ((,c :foreground ,rainbow-2)))
    `(rainbow-delimiters-depth-4-face ((,c :foreground ,rainbow-3)))
    `(rainbow-delimiters-depth-5-face ((,c :foreground ,rainbow-4)))
    `(rainbow-delimiters-depth-6-face ((,c :foreground ,rainbow-5)))
    `(rainbow-delimiters-depth-7-face ((,c :foreground ,rainbow-6)))
    `(rainbow-delimiters-depth-8-face ((,c :foreground ,rainbow-7)))
    `(rainbow-delimiters-depth-9-face ((,c :foreground ,rainbow-8)))
    `(rainbow-delimiters-mismatched-face ((,c :background ,bg-red-intense :foreground ,fg-intense)))
    `(rainbow-delimiters-unmatched-face ((,c :inherit ,(if alabaster-themes-no-bold
                                                           '(rainbow-delimiters-mismatched-face)
                                                         '(bold rainbow-delimiters-mismatched-face)))))

;;;; wgrep
    `(wgrep-delete-face ((,c :inherit warning)))
    `(wgrep-done-face ((,c :background ,bg-info :foreground ,info)))
    `(wgrep-face ((,c ,@(alabaster-themes--bold))))
    `(wgrep-file-face ((,c :foreground ,fg-alt)))
    `(wgrep-reject-face ((,c :background ,bg-err :foreground ,err)))

;;;; eglot
    `(eglot-mode-line ((,c ,@(alabaster-themes--bold) :foreground ,modeline-info)))
    `(eglot-diagnostic-tag-unnecessary-face ((,c :inherit alabaster-themes-underline-info)))

;;;; lsp (via doom-modeline integration)
    `(doom-modeline-lsp-error ((,c ,@(alabaster-themes--bold))))
    `(doom-modeline-lsp-running (( )))
    `(doom-modeline-lsp-success ((,c ,@(alabaster-themes--bold) :foreground ,modeline-info)))
    `(doom-modeline-lsp-warning ((,c ,@(alabaster-themes--bold) :foreground ,modeline-warning)))

;;;; yasnippet
    `(yas-field-highlight ((,c :background ,bg-hover)))
    `(yas-field-debug-face ((,c :background ,bg-err)))

;;;; smartparens
    `(sp-pair-overlay-face ((,c :background ,bg-hover)))
    `(sp-wrap-overlay-face ((,c :background ,bg-hover)))
    `(sp-wrap-tag-overlay-face ((,c :background ,bg-hover)))
    `(sp-show-pair-enclosing ((,c :inherit highlight)))
    `(sp-show-pair-match-face ((,c :inherit show-paren-match)))
    `(sp-show-pair-mismatch-face ((,c :inherit show-paren-mismatch)))

;;;; highlight-symbol
    `(highlight-symbol-face ((,c :background ,bg-hover)))

;;;; highlight-numbers
    `(highlight-numbers-number ((,c :foreground ,constant)))

;;;; git-gutter (non-fringe version)
    `(git-gutter:added ((,c :foreground ,fg-added)))
    `(git-gutter:deleted ((,c :foreground ,fg-removed)))
    `(git-gutter:modified ((,c :foreground ,fg-changed)))
    `(git-gutter:unchanged ((,c :foreground ,fg-dim)))

;;;; custom
    `(custom-button ((,c :box (:color ,border :style released-button))))
    `(custom-button-mouse ((,c :inherit (highlight custom-button))))
    `(custom-button-pressed ((,c :inherit (secondary-selection custom-button))))
    `(custom-changed ((,c :background ,bg-changed)))
    `(custom-comment ((,c :inherit shadow)))
    `(custom-comment-tag ((,c :inherit ,(if alabaster-themes-no-bold
                                            '(shadow)
                                          '(bold shadow)))))
    `(custom-face-tag ((,c ,@(alabaster-themes--bold) :foreground ,type)))
    `(custom-group-tag ((,c ,@(alabaster-themes--bold) :foreground ,builtin)))
    `(custom-group-tag-1 ((,c ,@(alabaster-themes--bold) :foreground ,constant)))
    `(custom-invalid ((,c :inherit error :strike-through t)))
    `(custom-modified ((,c :inherit custom-changed)))
    `(custom-rogue ((,c :inherit custom-invalid)))
    `(custom-set ((,c :inherit success)))
    `(custom-state ((,c :foreground ,fg-alt)))
    `(custom-themed ((,c :inherit custom-changed)))
    `(custom-variable-tag ((,c ,@(alabaster-themes--bold) :foreground ,variable)))
    `(custom-variable-obsolete ((,c :inherit shadow)))

;;;; elfeed
    `(elfeed-log-date-face ((,c :foreground ,fg-dim)))
    `(elfeed-log-debug-level-face ((,c :inherit shadow)))
    `(elfeed-log-error-level-face ((,c :inherit error)))
    `(elfeed-log-info-level-face ((,c :inherit success)))
    `(elfeed-log-warn-level-face ((,c :inherit warning)))
    `(elfeed-search-date-face ((,c :foreground ,fg-dim)))
    `(elfeed-search-feed-face ((,c :foreground ,magenta)))
    `(elfeed-search-filter-face ((,c ,@(alabaster-themes--bold))))
    `(elfeed-search-last-update-face ((,c ,@(alabaster-themes--bold) :foreground ,fg-dim)))
    `(elfeed-search-tag-face ((,c :foreground ,blue)))
    `(elfeed-search-title-face ((,c :foreground ,fg-dim)))
    `(elfeed-search-unread-title-face ((,c ,@(alabaster-themes--bold) :foreground ,fg-main)))

;;;; eww
    `(eww-form-checkbox ((,c :inherit widget-field)))
    `(eww-form-file ((,c :inherit widget-field)))
    `(eww-form-select ((,c :inherit widget-field)))
    `(eww-form-submit ((,c :background ,bg-active :foreground ,fg-intense)))
    `(eww-form-text ((,c :inherit widget-field)))
    `(eww-form-textarea ((,c :inherit eww-form-text)))
    `(eww-invalid-certificate ((,c :foreground ,err)))
    `(eww-valid-certificate ((,c :foreground ,info)))

;;;; message/gnus
    `(message-cited-text-1 ((,c :foreground ,blue)))
    `(message-cited-text-2 ((,c :foreground ,magenta)))
    `(message-cited-text-3 ((,c :foreground ,green)))
    `(message-cited-text-4 ((,c :foreground ,yellow)))
    `(message-header-cc ((,c :foreground ,fg-alt)))
    `(message-header-name ((,c :foreground ,keyword)))
    `(message-header-newsgroups ((,c :foreground ,fg-alt)))
    `(message-header-other ((,c :foreground ,fg-alt)))
    `(message-header-subject ((,c ,@(alabaster-themes--bold) :foreground ,fg-intense)))
    `(message-header-to ((,c ,@(alabaster-themes--bold) :foreground ,fg-intense)))
    `(message-header-x-header ((,c :foreground ,comment)))
    `(message-mml ((,c :foreground ,comment)))
    `(message-separator ((,c :inherit shadow)))
    `(gnus-button ((,c :inherit button :underline nil)))
    `(gnus-cite-attribution ((,c :inherit italic)))
    `(gnus-emphasis-bold ((,c ,@(alabaster-themes--bold))))
    `(gnus-emphasis-italic ((,c :inherit italic)))
    `(gnus-emphasis-underline ((,c :underline t)))
    `(gnus-emphasis-underline-italic ((,c :inherit italic :underline t)))
    `(gnus-group-mail-1 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-1)))
    `(gnus-group-mail-2 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-2)))
    `(gnus-group-mail-3 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-3)))
    `(gnus-group-mail-low ((,c :foreground ,rainbow-4)))
    `(gnus-group-news-1 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-1)))
    `(gnus-group-news-2 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-2)))
    `(gnus-group-news-3 ((,c ,@(alabaster-themes--bold) :foreground ,rainbow-3)))
    `(gnus-group-news-low ((,c :foreground ,rainbow-4)))
    `(gnus-signature ((,c :inherit italic)))
    `(gnus-summary-cancelled ((,c :background ,bg-err :foreground ,err)))
    `(gnus-summary-high-ancient ((,c :foreground ,fg-alt)))
    `(gnus-summary-high-read ((,c :foreground ,fg-dim)))
    `(gnus-summary-high-ticked ((,c :foreground ,warning)))
    `(gnus-summary-high-unread ((,c ,@(alabaster-themes--bold) :foreground ,fg-intense)))
    `(gnus-summary-low-ancient ((,c :inherit gnus-summary-high-ancient :height 0.9)))
    `(gnus-summary-low-read ((,c :inherit gnus-summary-high-read :height 0.9)))
    `(gnus-summary-low-ticked ((,c :inherit gnus-summary-high-ticked :height 0.9)))
    `(gnus-summary-low-unread ((,c :inherit gnus-summary-high-unread :height 0.9)))
    `(gnus-summary-normal-ancient ((,c :inherit gnus-summary-high-ancient)))
    `(gnus-summary-normal-read ((,c :inherit gnus-summary-high-read)))
    `(gnus-summary-normal-ticked ((,c :inherit gnus-summary-high-ticked)))
    `(gnus-summary-normal-unread ((,c :inherit gnus-summary-high-unread)))
    `(gnus-summary-selected ((,c :inherit alabaster-themes-mark-select)))
    )
  "Face specifications for Alabaster themes.")

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



;;;; Theme management commands


;;;; Load a theme at random


;;;; Rotate through a list of themes



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


;;;###autoload

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
           ((not style-listp)
            ;; If bold is disabled, just inherit var or nothing
            (if alabaster-themes-no-bold
                (or var 'default)
              'bold))
           (weight var)
           (var
            (if alabaster-themes-no-bold
                var
              (append (list 'bold) (list var))))
           (t
            (if alabaster-themes-no-bold
                'default
              'bold)))
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

(defcustom alabaster-themes-no-bold nil
  "When non-nil, remove bold weight from faces to match original Alabaster philosophy.
The original Alabaster theme avoids font variations entirely."
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

(defun alabaster-themes--bold ()
  "Conditional application of `bold' inheritance.
Returns bold inheritance unless `alabaster-themes-no-bold' is non-nil."
  (unless alabaster-themes-no-bold
    (list :inherit 'bold)))
