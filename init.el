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

    ;; Terminal emulator
    vterm

    ;; Shows key bindings for incomplete commands
    which-key

    ;; Jump to anywhere in the visible buffer
    avy

    ;; Access files in a docker container
    docker-tramp

    ;; File modes
    js2-mode
    typescript-mode
    yaml-mode
    dockerfile-mode
    rustic
    groovy-mode
    json-mode

    ;; Copy or Delete a whole line on cursor
    whole-line-or-region

    ;; Color themes
    color-theme-sanityinc-tomorrow

    ;; Get env vars from shell
    exec-path-from-shell

    ;; Language Server Protocol
    lsp-mode
    lsp-ui

    ;; Pos-Tip
    pos-tip
    
    ;; Company Packages
    company-jedi
    company-lsp
    company-quickhelp
    
    ;; Python specific
    elpy
    py-autopep8

    ;; Nim LSP
    nim-mode

    ;; Use fd for dired
    fd-dired

    ;; ivy and counsel
    ivy
    ivy-rich
    counsel

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    ;; amx

    ;; ido for everything?
    ;; ido-completing-read+
    

    ;; Org Journal
    org-journal

    ;; Distraction Free writing
    olivetti

    ;; Use deadgrep (rg) for searching
    deadgrep

    ;; Dim other windows
    dimmer

    ;; For sending HTTP Requests
    verb

    ;; git tools
    magit
    vc-msg

    ;; Roam
    org-roam

    ;; move text easily up and down
    move-text

    ;; Better help files
    helpful

    v-mode

    ))

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


;; Install/update packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Activate libvterm
(require 'vterm)

;; Enable move-text
(move-text-default-bindings)

;; Enable Auto revert mode
(global-auto-revert-mode 1)

;; Display relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'visual)
(setq display-line-numbers-width 0)

;; Turn on which-key minor mode
(require 'which-key)
(which-key-mode)

;; Turn on dimmer
(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-mode t)
(setq dimmer-fraction 0.5)

;; Set key for comment-or-uncomment-region
(global-set-key (kbd "M-/") #'comment-or-uncomment-region)

;; avy settings
(global-set-key (kbd "C-M-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-'") 'avy-goto-char-2)

;; Dired settings
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
    
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(require 'dired-x)

;; Set color theme
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-bright t)

;; Activate pos-tip
(require 'pos-tip)

;; Company settings
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; Deadgrip settings
(global-set-key (kbd "C-c s") #'deadgrep)

;; Pasting text should still word wrap
(setq term-suppress-hard-newline t)

;; Desktop mode
(desktop-save-mode 1)

;; Always turn on line wrap from screen
(global-visual-line-mode 1)

;; ivy/counsel/swiper settings
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)
(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s %s")

(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(global-set-key (kbd "C-c f") #'project-find-file)

;; https://www.murilopereira.com/how-to-open-a-file-in-emacs/
;; Might make find file faster?
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)


;; Settings for Helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

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

;; Turn off line numbers
;; (global-linum-mode 0)

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

;; Scrolling in place (M-n and M-p)
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
(setq auto-save-timeout 2)
(setq  auto-save-visited-mode t)
(setq auto-save-visited-interval 2)

;; http://ergoemacs.org/emacs/emacs_auto_save.html
(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t ))

(setq after-focus-change-function 'xah-save-all-unsaved)

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

;; Org mode settings
(setq org-startup-indented t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-odd-levels-only t)

;; Replace keys for cycle-agenda-files for avy
(defun change-cycle-agenda-files-key ()
  (local-set-key (kbd "C-'") 'avy-goto-char-2))

(add-hook 'org-mode-hook 'change-cycle-agenda-files-key)
(add-hook 'org-mode-hook 'org-hide-block-all)
(setq org-hide-block-startup t)


;; Turn off org adapt indentation to not include an extra white space for the heading
(setq org-adapt-indentation nil)

;;https://explog.in/notes/writingsetup.html
(set-face-attribute 'default nil :family "Geneva" :height 200)
(set-face-attribute 'fixed-pitch nil :family "Menlo" :height 200)
(set-face-attribute 'variable-pitch nil :family "Geneva" :height 200)
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

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

; Allow to resize images
(setq org-image-actual-width nil)


;; vc-msg settings
(defun vc-msg-hook-setup (vcs-type commit-info)
  ;; copy commit id to clipboard
  (message (format "%s\n%s\n%s\n%s"
                   (plist-get commit-info :id)
                   (plist-get commit-info :author)
                   (plist-get commit-info :author-time)
                   (plist-get commit-info :author-summary))))
(add-hook 'vc-msg-hook 'vc-msg-hook-setup)
(global-set-key (kbd "C-x v j") 'vc-msg-show)

;; Use js2-mode for JS files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Turn on org-journal
(require 'org-journal)
(setq org-journal-dir "~/journal/emacs_journal")
(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-file-format "%Y-%m-%d.org")

;; Turn on olivetti
(require 'olivetti)
(add-hook 'org-journal-mode-hook 'olivetti-mode)

;; Always turn on flyspell in org mode
(add-hook 'org-mode-hook 'flyspell-mode)
(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))

;; Org Capture and Agenda settings - http://pragmaticemacs.com/emacs/org-mode-basics-vi-a-simple-todo-list/
;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;file to save todo items
(setq org-agenda-files '("~/.notes"))

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/.notes/todo.org" "Tasks")
         "* TODO [#A] %?")))

;; Remove verilog mode since it's covers *.v files which I now want to refer to Vlang
(defun replace-alist-mode (alist oldmode newmode)
  (dolist (aitem alist)
    (if (eq (cdr aitem) oldmode)
    (setcdr aitem newmode))))

;; not sure what mode you want here. You could default to 'fundamental-mode
(replace-alist-mode auto-mode-alist 'verilog-mode 'v-mode)


;; https://gist.github.com/leavesofgrass/23cf0f61e0092e36dbbaa3f33e4dd060
;; Minify buffer contents
(defun minify-buffer-contents()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

(define-key global-map (kbd "C-c b") 'minify-buffer-contents)

(define-key global-map (kbd "C-c i") 'string-insert-rectangle)

;; Vlang
(require 'v-mode)

;; Org Roam
(require 'org-roam)
(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n j") #'org-roam-jump-to-index)
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-graph)
(define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
(define-key org-mode-map (kbd "C-c n c") #'org-roam-db-build-cache)
(setq org-roam-directory "~/journal/org-roam")
(setq org-roam-index-file "~/journal/org-roam/index.org")
(setq org-roam-completion-system 'ivy)
(org-roam-mode +1)

;; Nim Settings
(require 'nim-mode)
(add-hook 'nim-mode-hook #'rainbow-delimiters-mode)
(add-hook 'nim-mode-hook #'subword-mode)
(add-hook 'nim-mode-hook #'nimsuggest-mode)

;; Rust LSP settings for Rust Analyzer
(setq lsp-rust-server 'rust-analyzer)

;; LSP settings
(require 'lsp)
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'rustic-mode-hook #'lsp)
(add-hook 'nim-mode-hook #'lsp)


(setq lsp-headerline-breadcrumb-enable 1)

;; Enable flycheck
(global-flycheck-mode)
(add-hook 'after-init-hook 'flycheck-mode)

;; Enable elpy
(elpy-enable)

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Magit settings
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(global-set-key (kbd "C-x g") 'magit)

;; Go back to global mark shortcut
(global-set-key (kbd "C-`") 'pop-global-mark)

;; LSP shortcuts
(global-set-key (kbd "<f2>") 'lsp-describe-thing-at-point)
(global-set-key (kbd "<f3>") 'lsp-find-definition)
(global-set-key (kbd "<f4>") 'lsp-find-references)

;; Shortcuts for registers
(global-set-key  (kbd "C-c y") 'copy-to-register )
(global-set-key  (kbd "C-c p") 'insert-register )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode whole-line-or-region which-key verb vc-msg tide smex rustic rg restart-emacs org-roam org-journal olivetti nim-mode linum-relative json-mode js2-mode ivy-rich ido-completing-read+ helpful groovy-mode find-file-in-project fd-dired exec-path-from-shell emacsql-sqlite elpy dockerfile-mode dimmer deadgrep counsel company-quickhelp company-lsp company-jedi color-theme-sanityinc-tomorrow avy amx)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
