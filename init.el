;; Define package repositories
(require 'package)

;; From Melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                 (not (gnutls-available-p))))
    (proto (if no-ssl "http" "https")))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)  
  (package-refresh-contents))


;; The packages you want installedn. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    restart-emacs

    yaml-mode
    
    ;; Relative Line Numbers
    linum-relative

    ;; Copy or Delete a whole line on cursor
    whole-line-or-region

    ;; Color themes
    color-theme-sanityinc-tomorrow

    ;; Get env vars from shell
    exec-path-from-shell

    ;; Language Server Protocol
    lsp-mode
    eglot
    
    ;; Company Backends
    company-jedi

    ;; Python specific
    elpy

    ;; Use fd for dired
    fd-dired

    ;; Dockerfile Mode
    dockerfile-mode

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; Use ripgrep for searching
    rg

    ;; Latest Rust mode
    rustic

    ))

;; Install/update packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Dired settings
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
    
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(require 'dired-x)

;; Set color theme
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-bright t)

;; Set region color
(set-face-attribute 'region nil :background "yellow" :foreground "brown")

;; Set global company mode 
(add-hook 'after-init-hook 'global-company-mode)

;; Ripgrep settings
;; Use Ctrl-c s
(require 'rg)
(rg-enable-menu)
(setq rg-executable "/usr/local/bin/rg")
(setq rg-group-result 1)

;; Pasting text should still word wrap
(setq term-suppress-hard-newline t)

;; Desktop mode
(desktop-save-mode 1)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; No cursor blinking
(blink-cursor-mode 0)

;; Cursor color
(set-cursor-color "blue")

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Turn off the tool bar
(tool-bar-mode -1)

;; Show line numbers
(require 'linum-relative)
(linum-relative-on)
(global-linum-mode t)

;; increase font size for better readability
(set-face-attribute 'default nil :height 180)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
;; (setq create-lockfiles nil)

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

;; Set whole line or region mode
(whole-line-or-region-global-mode)


;; Scrolling
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))


(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)
 
;; Change set-mark command
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Auto-save options
(setq auto-save-default t)
(setq auto-save-visited-file-name t)
(setq auto-save-timeout 2)

(put 'narrow-to-region 'disabled nil)


;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Enable fd with find-file-in-project
(setq ffip-use-rust-fd t)

;; OSX: Use mdfind for locate
(setq locate-command "mdfind")

;; Org mode settings
(setq org-startup-indented t
      org-hide-leading-stars t
      org-odd-levels-only t)

;; eglot (LSP) settings
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

;; Enable elpy
(add-hook 'python-mode-hook 'elpy-enable)

(global-set-key (kbd "<f3>") 'xref-find-definitions)