;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;;; package -- Summary
;;; Commentary:

;; Set custom set vars in a different file instead of at the end of the init file.
;; Show no errors or messages when loading
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

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

(eval-when-compile
  (require 'use-package))

(use-package esup
  :defer
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa
  :config
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0)
)

(when (memq window-system '(mac ns x)) 

  ;; On OS X, an Emacs instance started from the graphical user
  ;; interface will have a different environment than a shell in a
  ;; terminal window, because OS X does not run a shell during the
  ;; login. Obviously this will lead to unexpected results when
  ;; calling external utilities like make from Emacs.
  ;; This library works around this problem by copying important
  ;; environment variables from the user's shell.
  ;; https://github.com/purcell/exec-path-from-shell
  (use-package 
    exec-path-from-shell 
    :commands exec-path-from-shell
    :ensure t
    :demand
    :config
    (exec-path-from-shell-initialize)
  )

  ;; OSX: Use mdfind for locate
  (setq locate-command "mdfind")

)



(use-package 
  elisp-format 
  :defer
  :ensure t 
  :config (require 'elisp-format))

;; super-save - https://github.com/bbatsov/super-save
(use-package 
  super-save 
  :ensure t 
  :demand
  :config 
  (setq super-save-idle-duration .75)
  (setq super-save-auto-save-when-idle t) 
  (setq super-save-max-buffer-size 9999999999999999)
  (super-save-mode +1) 
  (setq auto-save-default nil)

  ;; To emulate '.' in VIM
  ;; sneak in a setting for repeat
  (global-set-key (kbd "C-.") 'repeat)

  ;; Easier keys for macros
  (global-set-key (kbd "M-9") 'kmacro-start-macro)
  (global-set-key (kbd "M-0") 'kmacro-stop-macro)
  (global-set-key (kbd "M--") 'kmacro-end-and-call-macro)

)

(use-package 
 auto-compile
 :ensure t 
 :config
 ;(setq load-prefer-newer t)
 (auto-compile-on-load-mode)
 (auto-compile-on-save-mode)
)

(use-package 
  doom-modeline 
  :ensure t 
  :hook (after-init . doom-modeline-mode) 
  :config (setq doom-modeline-height 15) 
  (setq doom-modeline-bar-width 8) 
  (setq doom-modeline-window-width-limit 3) 
  (setq doom-modeline-lsp t))

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package 
  projectile 
  :ensure t 
  :defer
  :init (projectile-mode +1) 
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map) 
	      ("C-c f" . #'projectile-find-file))
  :config
  ;; Enable fd with find-file-in-project
  (setq ffip-use-rust-fd t)

  ;; Enable projectile outside of projects
  (setq projectile-require-project-root nil)

  (setq projectile-enable-caching nil)

)


;; Company Packages
(use-package 
  company 
  :ensure t)
(use-package 
  company-jedi 
  :commands company-jedi
  :ensure t)
(use-package 
  company-quickhelp 
  :ensure t 
  :hook (after-init-hook . global-company-mode) 
  :config (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin) 
  (company-quickhelp-mode))

;; Move mini-buffer to top mini frame
(use-package
  mini-frame
  :ensure t
  :config
  (mini-frame-mode 1)
  (custom-set-variables
  '(mini-frame-show-parameters
   '((top . 10)
     (width . 0.7)
     (left . 0.5))))
)


;; Shows key bindings for incomplete commands
(use-package 
  which-key 
  :ensure t 
  :config 
  (which-key-mode)
  (which-key-setup-minibuffer)
  (setq which-key-popup-type 'minibuffer)
)

;; Dim other windows
(use-package 
  dimmer 
  :ensure t 
  :config (dimmer-configure-which-key) 
  (dimmer-mode t) 
  (setq dimmer-fraction 0.4))


(use-package 
  counsel 
  :ensure t 
  :config
  (global-set-key (kbd "M-x") #'counsel-M-x) 
  (global-set-key (kbd "C-x C-f") 'counsel-find-file) 
  (global-set-key (kbd "C-x r b") 'counsel-bookmark) 
  (global-set-key (kbd "C-M-y") 'counsel-yank-pop) 
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s") 
  (setq counsel-describe-function-function #'helpful-callable) 
  (setq counsel-describe-variable-function #'helpful-variable)


  ;; Display relative line numbers
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'visual)
  (setq display-line-numbers-width 0)

  ;; https://www.murilopereira.com/how-to-open-a-file-in-emacs/
  ;; Might make find file faster?
  (remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

)

(use-package 
  ivy 
  :ensure t 
  :config (ivy-mode 1) 
  (setq ivy-use-virtual-buffers t) 
  (setq ivy-count-format "(%d/%d) ") 
  (global-set-key (kbd "C-s") 'swiper-isearch) 
  (setq ivy-use-selectable-prompt t))

(use-package 
  ivy-rich 
  :ensure t 
  :config (ivy-rich-mode 1) 
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))


(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))


(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))


(use-package 
  flycheck 
  :ensure t 
  :defer
  :commands flycheck
  :config (add-hook 'after-init-hook 'flycheck-mode)

  ;; Always turn on flyspell in org mode
  (add-hook 'org-mode-hook 'flyspell-mode) 
  (customize-set-variable 'ispell-program-name "aspell") 
  (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra")))


;; Activate pos-tip
(use-package 
  pos-tip 
  :defer
  )

;; Copy or Delete a whole line on cursor
(use-package 
  whole-line-or-region 
  :ensure t 
  :config
  ;; Set key for comment-or-uncomment-region
  (global-set-key (kbd "M-/") #'comment-or-uncomment-region)

)

(use-package 
  org-journal 
  :ensure t 
  :defer
  :commands org-journal
  :config (setq org-journal-dir "~/journal/emacs_journal") 
  (setq org-journal-date-format "%A, %d %B %Y") 
  (setq org-journal-file-format "%Y-%m-%d.org"))

;; Distraction Free writing
(use-package 
  olivetti 
  :ensure t 
  :defer
  :hook (olivetti-mode . org-journal-mode-hook))

;; Vlang
(use-package 
  v-mode 
  :ensure t 
  :defer
  :commands v-mode
  :config
  ;; Remove verilog mode since it's covers *.v files which I now want to refer to Vlang
  (defun replace-alist-mode (alist oldmode newmode) 
    (dolist (aitem alist) 
      (if (eq (cdr aitem) oldmode) 
	  (setcdr aitem newmode))))

  ;; not sure what mode you want here. You could default to 'fundamental-mode
  (replace-alist-mode auto-mode-alist 'verilog-mode 'v-mode))


;; https://zzamboni.org/post/how-to-insert-screenshots-in-org-documents-on-macos/
(use-package 
  org-download 
  :after org 
  :defer
  :ensure t 
  :defer nil 
  :custom (org-download-method 'directory) 
  (org-download-image-dir "images") 
  (org-download-heading-lvl nil) 
  (org-download-timestamp "%Y%m%d-%H%M%S_") 
  (org-image-actual-width 300) 
  (org-download-screenshot-method "/usr/local/bin/pngpaste %s") 
  :config 
  (require 'org-download)

  ;; Pasting text should still word wrap
  (setq term-suppress-hard-newline t)
)



(use-package 
  nim-mode 
  :ensure t 
  :defer
  :commands nim-mode
  :hook (nim-mode . rainbow-delimiters-mode) 
  (nim-mode . subword-mode) 
  (nim-mode . nimsuggest-mode))



(use-package 
  lsp-ui 
  :requires lsp-mode 
  flycheck 
  :config (setq lsp-ui-doc-enable t lsp-ui-doc-use-childframe t lsp-ui-doc-position 'top
		lsp-ui-doc-include-signature t lsp-ui-sideline-enable nil lsp-ui-flycheck-enable t
		lsp-ui-flycheck-list-position 'right lsp-ui-flycheck-live-reporting t
		lsp-ui-peek-enable t lsp-ui-peek-list-width 60 lsp-ui-peek-peek-height 25) 
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


(use-package 
  lsp-mode 
  :ensure t 
  :commands lsp-mode
  :config
  ;; LSP shortcuts
  (global-unset-key (kbd "s-l"))
  (global-set-key (kbd "s-l l") 'lsp-find-references) 
  (global-set-key (kbd "s-l d") 'lsp-describe-thing-at-point)
  (global-set-key (kbd "s-l f") 'lsp-find-definition)

  ;; LSP settings
  (setq lsp-headerline-breadcrumb-enable 1) 
  (setq read-process-output-max (* 1024 1024)) 
  (setq lsp-rust-server 'rust-analyzer) 

  (setq lsp-enable-snippet nil)

  (add-hook 'c-mode-hook #'lsp) 
  (add-hook 'c++-mode-hook #'lsp) 
  (add-hook 'js-mode-hook #'lsp) 
  (add-hook 'typescript-mode-hook #'lsp) 
  (add-hook 'rustic-mode-hook #'lsp) 
  (add-hook 'nim-mode-hook #'lsp)


)

(use-package
  lsp-ivy
  :ensure t
  :requires lsp-mode 
)

(use-package 
  tide 
  :ensure t 
  :defer
  :commands tide
  :config (defun setup-tide-mode () 
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
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package 
  web-mode 
  :ensure t 
  :defer
  :commands web-mode
  :config (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)) 
  (add-hook 'web-mode-hook (lambda () 
			     (when (string-equal "tsx" (file-name-extension buffer-file-name)) 
			       (setup-tide-mode)))))


(use-package 
  js2-mode 
  :ensure t 
  :defer
  :commands js2-mode
  :config
  ;; Use js2-mode for JS files
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package 
  typescript-mode 
  :commands typescript-mode
  :ensure t
  :defer
)

(use-package 
  yaml-mode 
  :defer
  :commands yaml-mode
  :ensure t
)

(use-package 
  dockerfile-mode 
  :defer
  :commands dockerfile-mode
  :ensure t
)

(use-package 
  groovy-mode 
  :defer
  :commands groovy-mode
  :ensure t
)

(use-package 
  json-mode 
  :defer
  :commands json-mode
  :ensure t
)

(use-package 
  rustic 
  :defer
  :commands rustic
  :ensure t
)


;; Python specific
(use-package lsp-pyright
  :ensure t
  :defer
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
(use-package 
  py-autopep8 
  :ensure t 
  :defer
  :commands py-autopep8
  )

(use-package robot-mode
  :ensure t
  :defer
  :config
  (add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))
  (global-set-key (kbd "C-c TAB") 'robot-mode-add-argument) 
  
)

(use-package 
  avy 
  :ensure t 
  :init (global-set-key (kbd "C-j") 'avy-goto-char-timer) 
  (defun change-cycle-agenda-files-key () 
    (local-set-key (kbd "C-j") 'avy-goto-char-timer) 
    (local-unset-key (kbd "C-'")) 
    (local-set-key (kbd "C-'") 'org-agenda))

  ;;https://gist.github.com/karthink/af013ffd77fe09e67360f040b57b4c7b
  (defun avy-show-dispatch-help () 
    (let* ((len (length "avy-action-")) 
	   (fw (frame-width)) 
	   (raw-strings (mapcar (lambda (x) 
				  (format "%2s: %-19s" (propertize (char-to-string (car x)) 'face
								   'aw-key-face) 
					  (substring (symbol-name (cdr x)) len)))
				avy-dispatch-alist)) 
	   (max-len (1+ (apply #'max (mapcar #'length raw-strings)))) 
	   (strings-len (length raw-strings)) 
	   (per-row (floor fw max-len)) display-strings) 
      (cl-loop for string in raw-strings for N from 1 to strings-len do (push (concat string " ")
									      display-strings) 
	       (when (= (mod N per-row) 0) 
		 (push "\n" display-strings))) 
      (message "%s" (apply #'concat (nreverse display-strings))))) 
  (defun avy-action-copy-whole-line (pt) 
    (save-excursion (goto-char pt) 
		    (cl-destructuring-bind (start . end) 
			(bounds-of-thing-at-point 'line) 
		      (copy-region-as-kill start end))) 
    (select-window (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-whole-line (pt) 
    (save-excursion (goto-char pt) 
		    (kill-whole-line)) 
    (select-window (cdr (ring-ref avy-ring 0)))
    t)
  :config (add-hook 'org-mode-hook 'change-cycle-agenda-files-key) 
  (add-hook 'v-mode-hook 'change-cycle-agenda-files-key)

  ;; Add option to copy whole line
  (setf (alist-get ?N avy-dispatch-alist) 'avy-action-copy-whole-line (alist-get ?K
										 avy-dispatch-alist)
	'avy-action-kill-whole-line)
)

(use-package 
  avy-zap 
  :ensure t
  :config 
  (global-set-key (kbd "M-Z") 'avy-zap-to-char-dwim)
  (global-set-key (kbd "M-z") 'avy-zap-up-to-char-dwim)

)


(use-package 
  deadgrep 
  :ensure t
  :config (global-set-key (kbd "C-c s") #'deadgrep))

(use-package 
  org 
  :mode ("\\.org\\'" . org-mode) 
  :config 
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-startup-folded t)
)

;; Verb - For sending HTTP Requests
(use-package 
  verb 
  :commands verb
  :defer
  :ensure t)

;; Use fd for dired
(use-package 
  fd-dired 
  :commands fd-dired
  :ensure t)


(use-package 
  helpful 
  :ensure t 
  :defer
  :commands helpful
  :config (global-set-key (kbd "C-h f") #'helpful-callable) 
  (global-set-key (kbd "C-h v") #'helpful-variable) 
  (global-set-key (kbd "C-h k") #'helpful-key) 
  (global-set-key (kbd "C-h .") #'helpful-at-point))

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package 
  amx 
  :ensure t)

(use-package 
  vc-msg 
  :ensure t 
  :defer
  :init (defun vc-msg-hook-setup (vcs-type commit-info)
	  ;; copy commit id to clipboard
	  (message (format "%s\n%s\n%s\n%s" (plist-get commit-info 
						       :id) 
			   (plist-get commit-info 
				      :author) 
			   (plist-get commit-info 
				      :author-time) 
			   (plist-get commit-info 
				      :author-summary)))) 
  :hook (vc-msg-hook . 'vc-msg-hook-setup) 
  :config (global-set-key (kbd "C-x v j") 'vc-msg-show))

;; move text easily up and down
(use-package 
  move-text 
  :ensure t 
  :config (move-text-default-bindings))

(use-package 
  color-theme-sanityinc-tomorrow 
  :ensure t
  :config
  ;; Set color theme
  (load-theme 'sanityinc-tomorrow-bright t)

  ;; Cursor color
  (set-cursor-color "blue")

  ;; Set region color
  (set-face-attribute 'region nil 
  		    :background "yellow" 
  		    :foreground "brown")

  ;;https://explog.in/notes/writingsetup.html
  (set-face-attribute 'default nil 
  		    :family "Menlo" 
  		    :height 200)
  (set-face-attribute 'fixed-pitch nil 
  		    :family "Menlo" 
  		    :height 200)
  (set-face-attribute 'variable-pitch nil 
  		    :family "Menlo" 
  		    :height 200)
  (set-face-attribute 'line-number nil 
  		    :family "Menlo" 
  		    :height 200)
  (set-face-attribute 'line-number-current-line nil 
  		    :family "Menlo" 
  		    :height 200)
  
  (add-hook 'text-mode-hook 'variable-pitch-mode)
  
  ;; Set Menlo font in the buffer
  ;; https://stackoverflow.com/questions/20866169/change-the-font-of-current-buffer-in-emacs
  (defun set-menlo-in-buffer () 
    (interactive "sFont Family: ") 
    (defface tmp-buffer-local-face '((t :family "Menlo")) 
      "Temporary buffer-local face") 
    (buffer-face-set 'tmp-buffer-local-face))
  
  ;; Menlo font for the calendar so it's misaligned
  (add-hook 'calendar-mode-hook 'set-menlo-in-buffer)

  ;;set colours for priorities
  (setq org-priority-faces '((?A . 
  			       (:foreground "#F0DFAF" 
  					    :weight bold)) 
  			   (?B . 
  			       (:foreground "LightSteelBlue")) 
  			   (?C . 
  			       (:foreground "OliveDrab"))))
    
)

(use-package 
  restart-emacs 
  :ensure t
  :config
)


;; Terminal emulator
(use-package 
  vterm 
  :ensure t 
  :commands vterm
  )


(use-package 
  magit
  :ensure t 
  :defer
  :commands magit
  :config (global-unset-key (kbd "C-c M-g")) 
  (global-set-key (kbd "C-c g") 'magit-file-dispatch) 
  (global-set-key (kbd "C-c F") 'magit-pull) 
  (global-set-key (kbd "C-c B") 'magit-branch) 
  (global-set-key (kbd "C-x g") 'magit)
  ;; From https://scripter.co/narrowing-the-author-column-in-magit/
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width 
			     :author 18)))

(use-package 
  diff-hl
  :ensure t 
  :defer
  :config
  (global-diff-hl-mode)
)


(use-package 
  org-roam 
  :ensure t 
  :defer
  :config 
  (setq org-roam-v2-ack t) 
  (global-set-key (kbd "C-c n l") 'org-roam-capture) 
  (global-set-key (kbd "C-c n f") 'org-roam-node-find) 
  (global-set-key (kbd "C-c n i") 'org-roam-node-insert) 
  (global-set-key (kbd "C-c n b") 'org-roam-buffer-display-dedicated) 
  (global-set-key (kbd "C-c n g") 'org-roam-graph) 
  (setq org-roam-directory "~/journal/org-roam") 
  (org-roam-db-autosync-mode) 
  (add-to-list 'display-buffer-alist '("\\*org-roam\\*" (display-buffer-in-direction) 
				       (direction . right) 
				       (window-width . 0.33) 
				       (window-height . fit-window-to-buffer)))
)



;; ***********************
;; Miscellaneous Settings
;; **********************

;; Set kill current buffer to need an extra more characters
(global-unset-key (kbd "s-k"))
(global-set-key (kbd "M-s-k") 'kill-current-buffer)

 ;; Set whole line or region mode
(whole-line-or-region-global-mode)

;; Go back to global mark shortcut
;; This traverses buffers.
(global-set-key (kbd "C-`") 'pop-global-mark)

;; Add another command to set-mark
(global-set-key (kbd "s-SPC") 'set-mark-command)

;; Can press C-u set-mark-command to go back to last position in the buffer
;; Then press subsequent set-mark-command to go back to the next last position
(setq set-mark-command-repeat-pop 1)

;; Calendar shortcut
(global-set-key (kbd "C-x c") 'calendar)

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

;; Easily create scratch buffers
(defun generate-scratch-buffer () 
  "Create and switch to a temporary scratch buffer with a random
       name." 
  (interactive) 
  (switch-to-buffer (generate-new-buffer-name "scratchbuffer")) 
  (json-mode))
(global-set-key  (kbd "C-c b") 'generate-scratch-buffer )
  
;; Scrolling in place (M-n and M-p)
;; Has a parameter to pass if you want scroll >1 lines (C-u <number>)
(defun scroll-up-in-place (n) 
  (interactive "p") 
  (next-line n) 
  (unless (eq (window-end) 
	      (point-max)) 
    (scroll-up n)))

(defun scroll-down-in-place (n) 
  (interactive "p") 
  (previous-line n) 
  (unless (eq (window-start) 
	      (point-min)) 
    (scroll-down n)))

(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)

;; Always turn on line wrap from screen
(global-visual-line-mode 1)

;; Enable line and column number mode
(setq column-number-mode 1)
(setq line-number-mode 1)

;; Desktop mode
(desktop-save-mode 1)

;; https://gist.github.com/leavesofgrass/23cf0f61e0092e36dbbaa3f33e4dd060
;; Minify buffer contents
(defun minify-buffer-contents() 
  "Minifies the buffer contents by removing whitespaces." 
  (interactive) 
  (delete-whitespace-rectangle (point-min) 
			       (point-max)) 
  (mark-whole-buffer) 
  (goto-char (point-min)) 
  (while (search-forward "\n" nil t) 
    (replace-match "" nil t)))

(defun copy-end-of-line()
 "Copy to end of line into kill ring"
 (interactive)
 (push-mark nil nil 1)
 (end-of-visual-line)
 (copy-region-as-kill nil nil (buffer-substring (mark) (point)))
 (pop-to-mark-command)
)

(defun mark-whole-line()
  (interactive)
  (beginning-of-visual-line)
  (set-mark-command nil)
  (end-of-visual-line)
) 

(global-set-key (kbd "M-k") 'copy-end-of-line)

(global-set-key (kbd "M-s-v") 'mark-whole-line)

(global-set-key (kbd "C-c m") 'minify-buffer-contents)

(global-set-key (kbd "C-c i") 'string-rectangle)

;; Shortcuts for registers
(global-set-key  (kbd "C-c y") 'copy-to-register )
(global-set-key  (kbd "C-c p") 'insert-register )

;; Org mode settings
(setq org-startup-indented t org-hide-leading-stars t org-hide-emphasis-markers t
	org-odd-levels-only t)

;; Fold all org blocks when opening org files
(setq org-cycle-hide-block-startup t)

;; Org Capture and Agenda settings - http://pragmaticemacs.com/emacs/org-mode-basics-vi-a-simple-todo-list/
;; set key for agenda
(global-set-key (kbd "C-'") 'org-agenda)

;;file to save todo items
(setq org-agenda-files '("~/.notes")) 
(setq org-agenda-window-setup (quote current-window))
;;capture todo items using C-c c t
(global-set-key (kbd "C-c c") 'org-capture) 
(setq org-capture-templates '(("t" "todo" entry (file+headline "~/.notes/todo.org" "Tasks")
				 "* TODO [#A] %?")))

(defun replace_underscores_with_spaces () 
  "Replace those 'underscores' from gmail to spaces" 
  (interactive) 
  (while (search-forward " " nil t) 
    (replace-match " " nil t)))

(global-set-key (kbd "C-c r") 'replace_underscores_with_spaces)

(defun kill-file-and-directory-buffers ()
  "Kill all buffers that are visiting files or directories."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (seq-filter
               (lambda (x)
                 (or (buffer-file-name x)
                     (eq 'dired-mode (buffer-local-value 'major-mode x))))
               (buffer-list)))))

(global-set-key  (kbd "C-x M-d") 'kill-file-and-directory-buffers)




(defun efs/display-startup-time () 
  (message "Emacs loaded in %s with %d garbage collections." 
	   (format "%.2f seconds" (float-time
		(time-subtract
		 after-init-time
		 before-init-time)))
	         gcs-done))

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024)
                  file-name-handler-alist file-name-handler-alist-original))
	  (efs/display-startup-time)
)

;; Previous suggestion
;; (add-hook 'emacs-startup-hook 
;; 	  (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1 file-name-handler-alist
;; 		last-file-name-handler-alist) 
;; 	  (efs/display-startup-time))
