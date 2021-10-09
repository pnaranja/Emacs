;;; package -- Summary
;;; Commentary:
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)


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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-expand-minimally t))

;; super-save - https://github.com/bbatsov/super-save
(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-idle-duration 1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
)

;; Terminal emulator
(use-package vterm
  :defer 2
)

;; To emulate '.' in VIM
(use-package dot-mode
  :hook
  (find-file-hooks . dot-mode-on)
  :config
  (global-dot-mode t)
)

;; Shows key bindings for incomplete commands
(use-package which-key
  :defer 2
  :config
  (which-key-mode)
)

;; Dim other windows
(use-package dimmer
  :defer 2
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t)
  (setq dimmer-fraction 0.4)
)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (setq ivy-use-selectable-prompt t)
)

(use-package ivy-rich
  :defer 2
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
)

(use-package counsel
  :defer 2
  :config
  ;; Replace M-x (execute-extended-command)
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (setq counsel-grep-base-command
	"rg -i -M 120 --no-heading --line-number --color never %s %s")
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
)

;; Activate pos-tip
(use-package pos-tip :defer 2)

;; Copy or Delete a whole line on cursor
(use-package whole-line-or-region
  :defer 2
)

(use-package org-journal
  :defer 2
  :config
  (setq org-journal-dir "~/journal/emacs_journal")
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-format "%Y-%m-%d.org")
)

;; Distraction Free writing
(use-package olivetti
  :hook (olivetti-mode . org-journal-mode-hook)
)

;; Vlang
(use-package v-mode
  :defer 2
  :init
  ;; Remove verilog mode since it's covers *.v files which I now want to refer to Vlang
  (defun replace-alist-mode (alist oldmode newmode)
    (dolist (aitem alist)
      (if (eq (cdr aitem) oldmode)
	  (setcdr aitem newmode))))

  ;; not sure what mode you want here. You could default to 'fundamental-mode
  (replace-alist-mode auto-mode-alist 'verilog-mode 'v-mode)
)

(use-package org-roam
  :defer 2
  :init
  (setq org-roam-v2-ack t)
  :config
  (global-set-key (kbd "C-c n l") 'org-roam-capture)
  (global-set-key (kbd "C-c n f") 'org-roam-node-find)
  (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c n b") 'org-roam-buffer)
  (global-set-key (kbd "C-c n g") 'org-roam-graph)
  (setq org-roam-directory "~/journal/org-roam")

  ;; Org Capture and Agenda settings - http://pragmaticemacs.com/emacs/org-mode-basics-vi-a-simple-todo-list/
  ;; set key for agenda
  (global-set-key (kbd "C-c a") 'org-agenda)

  ;;file to save todo items
  (setq org-agenda-files '("~/.notes"))

  (setq org-agenda-window-setup (quote current-window))
  ;;capture todo items using C-c c t
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/.notes/todo.org" "Tasks")
         "* TODO [#A] %?")))

  ;; Replace keys for cycle-agenda-files to org-agenda since I care more for that
  (global-set-key (kbd "C-'") 'org-agenda)

  ;; Org mode settings
  (setq org-startup-indented t
	org-hide-leading-stars t
	org-hide-emphasis-markers t
	org-odd-levels-only t)
)

(use-package nim-mode
  :hook
  (nim-mode . rainbow-delimiters-mode)
  (nim-mode . subword-mode)
  (nim-mode . nimsuggest-mode)
)

(use-package lsp-mode
  :defer 2
  :config
  ;; LSP shortcuts
  (global-unset-key (kbd "C-c l"))
  (global-set-key (kbd "C-c l p") 'lsp-describe-thing-at-point)
  (global-set-key (kbd "C-c l d") 'lsp-find-definition)
  (global-set-key (kbd "C-c l r") 'lsp-find-references)

  ;; LSP settings
  (setq lsp-headerline-breadcrumb-enable 1)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-rust-server 'rust-analyzer)

  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'rustic-mode-hook #'lsp)
  (add-hook 'nim-mode-hook #'lsp)
)

(use-package tide :defer 2
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
)


(use-package js2-mode :defer 2
  :config
  ;; Use js2-mode for JS files
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
)

(use-package typescript-mode :defer 2)
(use-package yaml-mode :defer 2)
(use-package dockerfile-mode :defer 2)
(use-package groovy-mode :defer 2)
(use-package json-mode :defer 2)
(use-package rustic :defer 2)

(use-package lsp-ui :defer 2)

;; Python specific
(use-package elpy
  :defer 2
  :ensure t
  :init
  (elpy-enable)
)

(use-package py-autopep8
  :defer 2
  :hook
  (elpy-mode-hook . py-autopep8-enable-on-save)
)

(use-package magit
  :defer 2
  :config
  (global-set-key (kbd "C-c g") 'magit-file-dispatch)
  (global-set-key (kbd "C-x g") 'magit)
  ;; From https://scripter.co/narrowing-the-author-column-in-magit/
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width :author 18))
)

(use-package avy
  :init
  (global-set-key (kbd "C-j") 'avy-goto-char)
  (global-set-key (kbd "C-M-;") 'avy-goto-char-timer)
  (defun change-cycle-agenda-files-key ()
  (local-set-key (kbd "C-j") 'avy-goto-char)
  (local-unset-key (kbd "C-'"))
  (local-set-key (kbd "C-'") 'org-agenda))
  :config
  (add-hook 'org-mode-hook 'change-cycle-agenda-files-key)
)

;; Access files in a docker container
(use-package docker-tramp
  :defer 2
)

(use-package deadgrep
  :defer 2
  :config
  (global-set-key (kbd "C-c s") #'deadgrep)
)

;; For sending HTTP Requests
(use-package verb
  :defer 2
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
)


;; Get env vars from shell
(use-package exec-path-from-shell :defer 2)


;; Use fd for dired
(use-package fd-dired :defer 2)

;; Company Packages
(use-package company-lsp :defer 2)
(use-package company-jedi :defer 2)
(use-package company-quickhelp 
  :defer 2
  :hook
  (after-init-hook . global-company-mode)
  :config
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (company-quickhelp-mode)
)

(use-package helpful 
  :defer 2
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h .") #'helpful-at-point)
)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package amx :defer 2)

(use-package vc-msg
  :defer 2
  :init
  (defun vc-msg-hook-setup (vcs-type commit-info)
  ;; copy commit id to clipboard
  (message (format "%s\n%s\n%s\n%s"
                   (plist-get commit-info :id)
                   (plist-get commit-info :author)
                   (plist-get commit-info :author-time)
                   (plist-get commit-info :author-summary))))
  :hook
  (vc-msg-hook . 'vc-msg-hook-setup)
  :config
  (global-set-key (kbd "C-x v j") 'vc-msg-show)
)

;; move text easily up and down
(use-package move-text 
  :defer 2
  :config
  (move-text-default-bindings)
)

(use-package color-theme-sanityinc-tomorrow :defer 1)

(use-package flycheck
  :defer 2
  :config
  (add-hook 'after-init-hook 'flycheck-mode)

  ;; Always turn on flyspell in org mode
  (add-hook 'org-mode-hook 'flyspell-mode)
  (customize-set-variable 'ispell-program-name "aspell")
  (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
)

(use-package restart-emacs :defer 2)


;; Add to Path
(setq exec-path (append exec-path '("/usr/local/bin")))


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

;; Enable Auto revert mode
(global-auto-revert-mode 1)

;; Display relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'visual)
(setq display-line-numbers-width 0)

;; Set key for comment-or-uncomment-region
(global-set-key (kbd "M-/") #'comment-or-uncomment-region)

;; Set color theme
(load-theme 'sanityinc-tomorrow-bright t)

;; Pasting text should still word wrap
(setq term-suppress-hard-newline t)

;; Desktop mode
(desktop-save-mode 1)

;; Always turn on line wrap from screen
(global-visual-line-mode 1)

(global-set-key (kbd "C-c f") #'project-find-file)

;; https://www.murilopereira.com/how-to-open-a-file-in-emacs/
;; Might make find file faster?
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;; shortcut to zap up to char
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") #'zap-up-to-char)

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
;; Has a parameter to pass if you want scroll >1 lines (C-u <number>)
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

;; Scrolling in place (M-n and M-p)
;; Has a parameter to pass if you want scroll >1 lines (C-u <number>)
(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)

(setq org-hide-block-startup t)
 
;; Add another command to set-mark
(global-set-key (kbd "M-SPC") 'set-mark-command)


;; https://batsov.com/articles/2012/03/08/emacs-tip-number-5-save-buffers-automatically-on-buffer-or-window-switch/
;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/emacs/autosaves/" t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq-default backup-directory-alist (quote ((".*" . "~/emacs/backups/"))))


(put 'narrow-to-region 'disabled nil)

;; Enable fd with find-file-in-project
(setq ffip-use-rust-fd t)

;; OSX: Use mdfind for locate
(setq locate-command "mdfind")


;; Turn off org adapt indentation to not include an extra white space for the heading
(setq org-adapt-indentation nil)

;;https://explog.in/notes/writingsetup.html
(set-face-attribute 'default nil :family "Menlo" :height 200)
(set-face-attribute 'fixed-pitch nil :family "Menlo" :height 200)
(set-face-attribute 'variable-pitch nil :family "Menlo" :height 200)
(set-face-attribute 'line-number nil :family "Menlo" :height 200)
(set-face-attribute 'line-number-current-line nil :family "Menlo" :height 200)

(add-hook 'text-mode-hook
          'variable-pitch-mode)

;; Cycle through ordered lists
(global-set-key (kbd "C-c l") 'org-cycle-list-bullet)

;; Set Menlo font in the buffer
;; https://stackoverflow.com/questions/20866169/change-the-font-of-current-buffer-in-emacs
(defun set-menlo-in-buffer ()
  (interactive "sFont Family: ")
  (defface tmp-buffer-local-face 
    '((t :family "Menlo"))
    "Temporary buffer-local face")
  (buffer-face-set 'tmp-buffer-local-face))

;; Menlo font for the calendar so it's misaligned
(add-hook 'calendar-mode-hook 'set-menlo-in-buffer)

;; Calendar shortcut
(global-set-key (kbd "C-x c") 'calendar)

;; Set region color
(set-face-attribute 'region nil :background "yellow" :foreground "brown")

; Allow to resize images
(setq org-image-actual-width nil)

(defun replace_underscores_with_spaces ()
  "Replace those 'underscores' from gmail to spaces"
  (interactive)
  (while (search-forward " " nil t)
    (replace-match " " nil t)))

(global-set-key (kbd "C-c r") 'replace_underscores_with_spaces)


;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))


;; https://gist.github.com/leavesofgrass/23cf0f61e0092e36dbbaa3f33e4dd060
;; Minify buffer contents
(defun minify-buffer-contents()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

(global-set-key (kbd "C-c m") 'minify-buffer-contents)

(global-set-key (kbd "C-c i") 'string-insert-rectangle)


;; Go back to global mark shortcut
(global-set-key (kbd "C-`") 'pop-global-mark)

;; Shortcuts for registers
(global-set-key  (kbd "C-c y") 'copy-to-register )
(global-set-key  (kbd "C-c p") 'insert-register )

;; Check startup time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist last-file-name-handler-alist)
  (efs/display-startup-time))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dired-x yaml-mode whole-line-or-region which-key vterm verb vc-msg v-mode use-package tide super-save smex rustic rg restart-emacs real-auto-save py-autopep8 projectile org-roam org-journal olivetti nim-mode move-text magit lsp-ui lsp-python-ms lsp-pyright linum-relative json-mode js2-mode ivy-rich ido-completing-read+ helpful groovy-mode find-file-in-project fd-dired exec-path-from-shell esup emacsql-sqlite3 elpy dot-mode dockerfile-mode docker-tramp dimmer deadgrep dash-functional counsel company-quickhelp company-lsp company-jedi color-theme-sanityinc-tomorrow avy async amx)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
