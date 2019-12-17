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

    ;; Shows key bindings for incomplete commands
    which-key

    ;; Jump to anywhere in the visible buffer
    avy

    ;; File modes
    typescript-mode
    yaml-mode
    dockerfile-mode
    rustic

    ;; ido for everything?
    ido-completing-read+
    
    ;; Copy or Delete a whole line on cursor
    whole-line-or-region

    ;; Color themes
    color-theme-sanityinc-tomorrow

    ;; Get env vars from shell
    exec-path-from-shell

    ;; Language Server Protocol
    lsp-mode
    
    ;; Company Backends
    company-jedi
    company-lsp

    ;; Python specific
    elpy

    ;; Use fd for dired
    fd-dired

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    amx

    ;; Org Journal
    org-journal

    ;; Use deadgrep (rg) for searching
    deadgrep

    ;; Dim other windows
    dimmer

    ))

;; Install/update packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Turn on which-key minor mode
(require 'which-key)
(which-key-mode)

;; Turn on dimmer
(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-mode t)
(setq dimmer-fraction 0.5)

;; Turn on org-journal
(require 'org-journal)
(setq org-journal-dir "~/org/journal/")
(setq org-journal-date-format "%A, %d %B %Y")

;; avy settings
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

;; flycheck hook
(add-hook 'flyspell-mode-hook (local-set-key (kbd "C-}") 'flyspell-auto-correct-previous-word))


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

;; Deadgrip settings
(global-set-key (kbd "C-c r") #'deadgrep)

;; Pasting text should still word wrap
(setq term-suppress-hard-newline t)

;; Desktop mode
(desktop-save-mode 1)

;; Always turn on line wrap from screen
(global-visual-line-mode 1)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(setq ffip-prefer-ido-mode t)

;; amex
(require 'amx)
(amx-mode 1)

;; No cursor blinking
(blink-cursor-mode 0)

;; Cursor color
(set-cursor-color "blue")

;; full path in title bar and in mode line
(setq-default frame-title-format "%b (%f)")

(setq-default mode-line-format
   (quote
    ("%f     " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))

;; Turn off the tool bar
(tool-bar-mode -1)

;; Turn off line numbers
(global-linum-mode 0)

;; increase font size for better readability
(set-face-attribute 'default nil :height 180)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

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


;; Scrolling in place (M-n and M-p)
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
 
;; Add another command to set-mark
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Auto-save options
(setq auto-save-default t)
(setq auto-save-visited-file-name t)
(setq auto-save-timeout 1)


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/emacs/autosaves/" t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq-default backup-directory-alist (quote ((".*" . "~/emacs/backups/"))))


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

;; LSP settings
(add-hook 'js-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)

;; Enable elpy
(add-hook 'python-mode-hook 'elpy-enable)

;; Go back to global mark shortcut
(global-set-key (kbd "C-`") 'pop-global-mark)

;; LSP shortcuts
(global-set-key (kbd "<f2>") 'lsp-describe-thing-at-point)
(global-set-key (kbd "<f3>") 'lsp-find-definition)
(global-set-key (kbd "<f4>") 'lsp-find-references)

