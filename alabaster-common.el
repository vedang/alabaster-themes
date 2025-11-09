;;; alabaster-common.el --- Common Alabaster theme definitions -*- lexical-binding:t -*-

;;; Commentary:
;; Common color definitions and utilities for Alabaster theme variants.
;; Maintains exact color fidelity to original Sublime Text scheme.

;;; Code:

(deftheme alabaster-common "Shared Alabaster color palette")

;; Main Alabaster palette (exact Sublime colors)
(defcustom alabaster-active "#007ACC" "Cursor and highlights color" :type 'string :group 'alabaster)
(defcustom alabaster-selection "#BFDBFE" "Selection background" :type 'string :group 'alabaster)
(defcustom alabaster-blue "#325CC0" "Definitions color" :type 'string :group 'alabaster)
(defcustom alabaster-green "#448C27" "Strings color" :type 'string :group 'alabaster)
(defcustom alabaster-red "#AA3731" "Comments and errors color" :type 'string :group 'alabaster)
(defcustom alabaster-magenta "#7A3E9D" "Constants color" :type 'string :group 'alabaster)
(defcustom alabaster-grey "#777" "Punctuation color" :type 'string :group 'alabaster)
(defcustom alabaster-orange "#FFBC5D" "Highlights and search color" :type 'string :group 'alabaster)

;; Light theme colors
(defcustom alabaster-light-fg "#000000" "Light theme foreground" :type 'string :group 'alabaster)
(defcustom alabaster-light-bg "#F7F7F7" "Light theme background" :type 'string :group 'alabaster)
(defcustom alabaster-light-line-highlight "#f0f0f0" "Light theme line highlight" :type 'string :group 'alabaster)

;; Dark theme colors
(defcustom alabaster-dark-fg "#CECECE" "Dark theme foreground" :type 'string :group 'alabaster)
(defcustom alabaster-dark-bg "#0E1415" "Dark theme background" :type 'string :group 'alabaster)
(defcustom alabaster-dark-line-highlight "#ffffff10" "Dark theme line highlight" :type 'string :group 'alabaster)
(defcustom alabaster-dark-active "#CD974B" "Dark theme cursor color" :type 'string :group 'alabaster)

;; BG variant colors
(defcustom alabaster-bg-blue "#DBF1FF" "BG variant definitions background" :type 'string :group 'alabaster)
(defcustom alabaster-bg-green "#F1FADF" "BG variant strings background" :type 'string :group 'alabaster)
(defcustom alabaster-bg-dark-green "#DBECB6" "BG variant escapes background" :type 'string :group 'alabaster)
(defcustom alabaster-bg-red "#FFE0E0" "BG variant comments background" :type 'string :group 'alabaster)
(defcustom alabaster-bg-magenta "#F9E0FF" "BG variant constants background" :type 'string :group 'alabaster)
(defcustom alabaster-bg-yellow "#FFFABC" "BG variant comments alt background" :type 'string :group 'alabaster)
(defcustom alabaster-bg-fg "#000" "BG variant foreground" :type 'string :group 'alabaster)
(defcustom alabaster-bg-bg "#fff" "BG variant background" :type 'string :group 'alabaster)

;; Dark variant specific colors
(defcustom alabaster-dark-comment "#DFDF8E" "Dark theme comments" :type 'string :group 'alabaster)
(defcustom alabaster-dark-string "#95CB82" "Dark theme strings" :type 'string :group 'alabaster)
(defcustom alabaster-dark-constant "#CC8BC9" "Dark theme constants" :type 'string :group 'alabaster)
(defcustom alabaster-dark-selection "#293334" "Dark theme selection" :type 'string :group 'alabaster)

;; Additional utility colors
(defcustom alabaster-transparent-black "#00000010" "Very transparent black for backgrounds" :type 'string :group 'alabaster)
(defcustom alabaster-semi-transparent-black "#00000090" "Semi-transparent black for dimmed text" :type 'string :group 'alabaster)
(defcustom alabaster-semi-transparent-black-2 "#00000075" "Alternative semi-transparent black" :type 'string :group 'alabaster)
(defcustom alabaster-bg-region "#B4D8FD" "BG variant region selection color" :type 'string :group 'alabaster)
(defcustom alabaster-bg-error "#c33" "BG variant error color" :type 'string :group 'alabaster)
(defcustom alabaster-dark-dim-grey "#666" "Dark theme dim grey color" :type 'string :group 'alabaster)
(defcustom alabaster-dark-definition-blue "#8AB1F0" "Dark theme definition blue color" :type 'string :group 'alabaster)
(defcustom alabaster-dark-error "#ff6b6b" "Dark theme error color" :type 'string :group 'alabaster)
(defcustom alabaster-dark-error-bg "#332020" "Dark theme error background color" :type 'string :group 'alabaster)
(defcustom alabaster-dark-highlight-text "#000" "Dark theme text color for highlights" :type 'string :group 'alabaster)

(provide 'alabaster-common)
;;; alabaster-common.el ends here