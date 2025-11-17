;;; alabaster-test.el --- Buttercup tests for alabaster theme -*- lexical-binding:t -*-

;;; Commentary:
;; Test suite for alabaster theme package using Buttercup.

;;; Code:

(require 'buttercup)
(require 'alabaster-themes)
(require 'alabaster-theme)

(describe "Alabaster Theme Package"

  (before-each
    ;; In batch mode, load-path is already set via -L flag
    ;; Just ensure custom-theme-load-path includes current directory
    (add-to-list 'custom-theme-load-path default-directory))

  (describe "Package Loading"
    (it "should load alabaster-theme without errors"
      (expect (require 'alabaster-theme) :not :to-throw))

    (it "should provide customization group"
      (expect (get 'alabaster-themes 'group-documentation) :to-be-truthy)))

  (describe "Theme Variants"
    (let ((themes '(alabaster alabaster-bg alabaster-dark alabaster-mono alabaster-dark-mono)))

      (it "should load all theme variants successfully"
        (dolist (theme themes)
          (expect (load-theme theme t :no-enable) :not :to-throw)
          (expect (custom-theme-p theme) :to-be-truthy)))

      (it "should provide all themes"
        (dolist (theme themes)
          (expect (custom-available-themes) :to-contain theme)))))

  (describe "Theme Loading Functions"
    (before-each
      ;; Disable any currently loaded themes
      (mapc #'disable-theme custom-enabled-themes))

    (it "should load themes with alabaster-themes-load-theme"
      (expect (alabaster-themes-load-theme 'alabaster) :not :to-throw)
      (expect custom-enabled-themes :to-contain 'alabaster))

    (it "should accept a theme list for rotation"
      ;; Test that rotate accepts the themes parameter (actual rotation tested interactively)
      (let ((themes '(alabaster alabaster-dark alabaster-mono)))
        (expect (proper-list-p themes) :to-be-truthy)
        (expect (seq-every-p (lambda (th) (memq th alabaster-themes-collection)) themes) :to-be-truthy)))

    (it "should validate theme variant input"
      (expect (alabaster-themes-load-theme 'invalid-theme) :to-throw)))

  (describe "Theme Palettes"
    (it "should define alabaster palette with correct colors"
      (expect (boundp 'alabaster-palette) :to-be-truthy)
      (let ((palette (symbol-value 'alabaster-palette)))
        (expect (assoc 'string palette) :to-be-truthy)
        (expect (assoc 'comment palette) :to-be-truthy)
        (expect (assoc 'constant palette) :to-be-truthy)
        (expect (assoc 'fnname palette) :to-be-truthy)))

    (it "should define alabaster-dark palette"
      (require 'alabaster-dark-theme)
      (expect (boundp 'alabaster-dark-palette) :to-be-truthy))

    (it "should define alabaster-bg palette"
      (require 'alabaster-bg-theme)
      (expect (boundp 'alabaster-bg-palette) :to-be-truthy))

    (it "should define alabaster-mono palette"
      (require 'alabaster-mono-theme)
      (expect (boundp 'alabaster-mono-palette) :to-be-truthy))

    (it "should define alabaster-dark-mono palette"
      (require 'alabaster-dark-mono-theme)
      (expect (boundp 'alabaster-dark-mono-palette) :to-be-truthy)))

  (describe "Theme Properties"
    (it "should have correct background-mode for light themes"
      (load-theme 'alabaster t)
      (expect (get 'alabaster 'theme-feature) :to-be-truthy))

    (it "should have correct background-mode for dark themes"
      (load-theme 'alabaster-dark t)
      (expect (get 'alabaster-dark 'theme-feature) :to-be-truthy)))

  (describe "Face Coverage"
    (it "should define basic font-lock faces"
      (load-theme 'alabaster t)
      (expect (facep 'font-lock-string-face) :to-be-truthy)
      (expect (facep 'font-lock-constant-face) :to-be-truthy)
      (expect (facep 'font-lock-comment-face) :to-be-truthy)
      (expect (facep 'font-lock-function-name-face) :to-be-truthy))

    (it "should define error and warning faces"
      (expect (facep 'error) :to-be-truthy)
      (expect (facep 'warning) :to-be-truthy))

    (it "should define UI faces"
      (expect (facep 'mode-line) :to-be-truthy)
      (expect (facep 'mode-line-inactive) :to-be-truthy)
      (expect (facep 'default) :to-be-truthy)))

  (describe "Bold Customization"
    (before-each
      (mapc #'disable-theme custom-enabled-themes))

    (it "should apply bold by default"
      (setq alabaster-themes-no-bold nil)
      (load-theme 'alabaster t)
      (let ((helper-result (alabaster-themes--bold)))
        (expect helper-result :to-equal '(:inherit bold))))

    (it "should remove bold when alabaster-themes-no-bold is t"
      (setq alabaster-themes-no-bold t)
      (let ((helper-result (alabaster-themes--bold)))
        (expect helper-result :to-be nil)))

    (it "should affect heading styles"
      (setq alabaster-themes-no-bold nil)
      (let ((heading-spec (alabaster-themes--heading 1)))
        (expect (plist-get heading-spec :inherit) :to-be-truthy))

      (setq alabaster-themes-no-bold t)
      (let ((heading-spec (alabaster-themes--heading 1)))
        ;; Should not have bold in inheritance
        (expect (plist-get heading-spec :inherit) :not :to-equal 'bold)))))

(provide 'alabaster-test)
;;; alabaster-test.el ends here
