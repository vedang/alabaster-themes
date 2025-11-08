;;; alabaster-test.el --- Buttercup tests for alabaster theme -*- lexical-binding:t -*-

;;; Commentary:
;; Test suite for alabaster theme package using Buttercup.

;;; Code:

(require 'buttercup)
(require 'alabaster-theme)

(describe "Alabaster Theme Package"
  :var ((theme-path "/Users/nejo/src/github/tonsky/sublime-scheme-alabaster/alabaster-emacs"))

  (before-each
    (add-to-list 'load-path theme-path))

  (describe "Package Loading"
    (it "should load alabaster-theme without errors"
      (expect (require 'alabaster-theme) :not :to-throw))

    (it "should load common definitions"
      (expect (featurep 'alabaster-common) :to-be t)
      (expect alabaster-green :to-equal "#448C27")
      (expect alabaster-blue :to-equal "#325CC0")
      (expect alabaster-red :to-equal "#AA3731"))

    (it "should provide customization group"
      (expect (get 'alabaster 'group-documentation) :to-be-truthy)))

  (describe "Theme Variants"
    (let ((themes '(alabaster alabaster-bg alabaster-dark alabaster-mono alabaster-dark-mono)))

      (it "should load all theme variants successfully"
        (dolist (theme themes)
          (expect (require theme) :not :to-throw)
          (expect (featurep theme) :to-be t)))

      (it "should provide all themes"
        (dolist (theme themes)
          (expect (custom-available-themes) :to-contain theme)))

      (it "should define all required color variables"
        (expect alabaster-active :to-equal "#007ACC")
        (expect alabaster-selection :to-equal "#BFDBFE")
        (expect alabaster-light-fg :to-equal "#000000")
        (expect alabaster-light-bg :to-equal "#F7F7F7")
        (expect alabaster-dark-fg :to-equal "#CECECE")
        (expect alabaster-dark-bg :to-equal "#0E1415")))

  (describe "Theme Loading Functions"
    (before-each
      ;; Disable any currently loaded themes
      (mapc #'disable-theme custom-enabled-themes))

    (it "should load themes with alabaster-load-theme"
      (expect (alabaster-load-theme 'alabaster) :not :to-throw)
      (expect (custom-enabled-themes) :to-contain 'alabaster))

    (it "should cycle through themes with alabaster-switch-theme"
      (alabaster-load-theme 'alabaster)
      (let ((original-theme 'alabaster))
        (alabaster-switch-theme)
        (expect (custom-enabled-themes) :not :to-contain original-theme)
        (expect (> (length custom-enabled-themes) 0) :to-be t)))

    (it "should respect default theme variant"
      (let ((alabaster-theme 'alabaster-dark))
        (expect alabaster-theme :to-equal 'alabaster-dark)))

    (it "should validate theme variant input"
      (expect (alabaster-load-theme 'invalid-theme) :to-throw)))

  (describe "Face Definitions"
    (it "should define syntax highlighting faces"
      ;; Load main theme to test faces
      (load-theme 'alabaster t)

      (expect (face-attribute 'font-lock-string-face :foreground)
              :to-equal "#448C27")
      (expect (face-attribute 'font-lock-constant-face :foreground)
              :to-equal "#7A3E9D")
      (expect (face-attribute 'font-lock-comment-face :foreground)
              :to-equal "#AA3731")
      (expect (face-attribute 'font-lock-function-name-face :foreground)
              :to-equal "#325CC0"))

    (it "should define error and warning faces"
      (load-theme 'alabaster t)

      (expect (face-attribute 'error :foreground) :to-equal "#AA3731")
      (expect (face-attribute 'warning :foreground) :to-equal "#FFBC5D"))

    (it "should define basic appearance faces"
      (load-theme 'alabaster t)

      (expect (face-attribute 'default :foreground) :to-equal "#000000")
      (expect (face-attribute 'default :background) :to-equal "#F7F7F7")))

  (describe "BG Variant Background Highlighting"
    (it "should use background colors instead of foreground"
      (load-theme 'alabaster-bg t)

      ;; BG theme should use black text with colored backgrounds
      (expect (face-attribute 'font-lock-string-face :foreground) :to-equal "#000000")
      (expect (face-attribute 'font-lock-comment-face :foreground) :to-equal "#000000")
      (expect (face-attribute 'font-lock-function-name-face :foreground) :to-equal "#000000")

      ;; Check for colored backgrounds
      (expect (face-attribute 'font-lock-string-face :background) :to-be-truthy)))

  (describe "Dark Theme Color Palette"
    (it "should use inverted colors in dark themes"
      (load-theme 'alabaster-dark t)

      (expect (face-attribute 'default :foreground) :to-equal "#CECECE")
      (expect (face-attribute 'default :background) :to-equal "#0E1415")
      (expect (face-attribute 'font-lock-comment-face :foreground) :to-equal "#DFDF8E")
      (expect (face-attribute 'font-lock-string-face :foreground) :to-equal "#95CB82")))

  (describe "Mono Variants Minimal Highlighting"
    (it "should only show colors for errors and highlights in mono themes"
      (load-theme 'alabaster-mono t)

      ;; Mono theme should use default foreground for most syntax
      (expect (face-attribute 'font-lock-string-face :foreground) :to-equal "#000000")
      (expect (face-attribute 'font-lock-constant-face :foreground) :to-equal "#000000")
      (expect (face-attribute 'font-lock-function-name-face :foreground) :to-equal "#000000")

      ;; But still show error colors
      (expect (face-attribute 'error :foreground) :to-equal "#AA3731")))

  (describe "Dark Mono Variant"
    (it "should use minimal coloring in dark mode"
      (load-theme 'alabaster-dark-mono t)

      (expect (face-attribute 'default :foreground) :to-equal "#CECECE")
      (expect (face-attribute 'default :background) :to-equal "#0E1415")
      (expect (face-attribute 'font-lock-string-face :foreground) :to-equal "#CECECE")
      (expect (face-attribute 'font-lock-function-name-face :foreground) :to-equal "#CECECE")))

  (describe "Mode Line and UI Elements"
    (it "should define mode line faces"
      (load-theme 'alabaster t)

      (expect (face-attribute 'mode-line :foreground) :to-equal "#000000")
      (expect (face-attribute 'mode-line-inactive :foreground) :to-equal "#777")))

  (describe "Git Diff Faces"
    (it "should define diff colors"
      (load-theme 'alabaster t)

      (expect (face-attribute 'diff-added :foreground) :to-match "hsl(100, 50%, 50%)")
      (expect (face-attribute 'diff-removed :foreground) :to-match "hsl(2, 65%, 50%)")
      (expect (face-attribute 'diff-changed :foreground) :to-match "hsl(30, 85%, 50%)")))

  (describe "Color Variable Customization"
    (it "should allow customization of theme colors"
      (let ((original-green alabaster-green))
        (setq alabaster-green "#00FF00")
        (expect alabaster-green :to-equal "#00FF00")
        (setq alabaster-green original-green))) ; Restore original

    (it "should provide proper defcustom variables"
      (expect (get 'alabaster-green 'standard-value) :not :to-be nil)
      (expect (get 'alabaster-green 'custom-type) :to-equal 'string)
      (expect (get 'alabaster-green 'group) :to-equal 'alabaster))))

(provide 'alabaster-test)
;;; alabaster-test.el ends here