;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;; -*- lexical-binding: t -*-

;; Increase garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

;; For measuring startup time
;;(defvar last-file-name-handler-alist file-name-handler-alist)

;; Prevent unwanted runtime calls
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Turn off the tool bar
(tool-bar-mode -1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable Auto revert mode
(global-auto-revert-mode 1)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Turn off electric mode so no auto indenting
(setq electric-indent-mode nil)

;; Turn on delete selection mode
(delete-selection-mode 1)

;; Auto-follow symlinks
(setq vc-follow-symlinks t)

;; Turn off org adapt indentation to not include an extra white space for the heading
(setq org-adapt-indentation nil)


;; full path in title bar and in mode line
(setq-default frame-title-format "%b (%f)")

;; Allow to resize images
(setq org-image-actual-width nil)

;; For emacs lsp booster - https://github.com/blahgeek/emacs-lsp-booster?tab=readme-ov-file#configure-lsp-mode
(setenv "LSP_USE_PLISTS" "true")

(put 'narrow-to-region 'disabled nil)
