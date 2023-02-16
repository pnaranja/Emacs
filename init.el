;;; package -- Summary
;;; Commentary:
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6 file-name-handler-alist nil)


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
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa
  :config
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0)
)

(use-package 
  elisp-format 
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
  

)

(use-package 
  doom-modeline 
  :ensure t 
  :hook (after-init . doom-modeline-mode) 
  :config (setq doom-modeline-height 15) 
  (setq doom-modeline-bar-width 8) 
  (setq doom-modeline-window-width-limit 3) 
  (setq doom-modeline-lsp t))

(use-package 
  projectile 
  :ensure t 
  :init (projectile-mode +1) 
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map) 
	      ("C-c f" . #'projectile-find-file))
  :config
  ;; Enable fd with find-file-in-project
  (setq ffip-use-rust-fd t)

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


;; Shows key bindings for incomplete commands
(use-package 
  which-key 
  :ensure t 
  :config (which-key-mode))

;; Dim other windows
(use-package 
  dimmer 
  :ensure t 
  :config (dimmer-configure-which-key) 
  (dimmer-mode t) 
  (setq dimmer-fraction 0.4))

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


(use-package 
  all-the-icons-ivy-rich 
  :ensure t 
  :init (all-the-icons-ivy-rich-mode 1))


(use-package 
  counsel 
  :ensure t 
  :config
  ;; Replace M-x (execute-extended-command)
  (global-set-key (kbd "M-x") #'counsel-M-x) 
  (global-set-key (kbd "C-x C-f") 'counsel-find-file) 
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
  flycheck 
  :ensure t 
  :commands flycheck
  :config (add-hook 'after-init-hook 'flycheck-mode)

  ;; Always turn on flyspell in org mode
  (add-hook 'org-mode-hook 'flyspell-mode) 
  (customize-set-variable 'ispell-program-name "aspell") 
  (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra")))


;; Activate pos-tip
(use-package 
  pos-tip 
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
  :commands org-journal
  :config (setq org-journal-dir "~/journal/emacs_journal") 
  (setq org-journal-date-format "%A, %d %B %Y") 
  (setq org-journal-file-format "%Y-%m-%d.org"))

;; Distraction Free writing
(use-package 
  olivetti 
  :ensure t 
  :hook (olivetti-mode . org-journal-mode-hook))

;; Vlang
(use-package 
  v-mode 
  :ensure t 
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
  :ensure t 
  :defer nil 
  :custom (org-download-method 'directory) 
  (org-download-image-dir "images") 
  (org-download-heading-lvl nil) 
  (org-download-timestamp "%Y%m%d-%H%M%S_") 
  (org-image-actual-width 300) 
  (org-download-screenshot-method "/usr/local/bin/pngpaste %s") 
  :bind ("C-M-y" . org-download-screenshot) 
  :config 
  (require 'org-download)

  ;; Pasting text should still word wrap
  (setq term-suppress-hard-newline t)
)



(use-package 
  nim-mode 
  :ensure t 
  :commands nim-mode
  :hook (nim-mode . rainbow-delimiters-mode) 
  (nim-mode . subword-mode) 
  (nim-mode . nimsuggest-mode))

(use-package 
  lsp-mode 
  :ensure t 
  :demand
  :commands lsp-mode
  :config
  ;; LSP shortcuts
  (global-set-key (kbd "C-c l p") 'lsp-ui-peek-find-references) 

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

(use-package
  lsp-ivy
  :ensure t)

(use-package 
  tide 
  :ensure t 
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
  :commands web-mode
  :config (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)) 
  (add-hook 'web-mode-hook (lambda () 
			     (when (string-equal "tsx" (file-name-extension buffer-file-name)) 
			       (setup-tide-mode)))))


(use-package 
  js2-mode 
  :ensure t 
  :commands js2-mode
  :config
  ;; Use js2-mode for JS files
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package 
  typescript-mode 
  :commands typescript-mode
  :ensure t)
(use-package 
  yaml-mode 
  :commands yaml-mode
  :ensure t)
(use-package 
  dockerfile-mode 
  :commands dockerfile-mode
  :ensure t)
(use-package 
  groovy-mode 
  :commands groovy-mode
  :ensure t)
(use-package 
  json-mode 
  :commands json-mode
  :ensure t)
(use-package 
  rustic 
  :commands rustic
  :ensure t)

(use-package 
  lsp-ui 
  :requires lsp-mode 
  flycheck 
  :config (setq lsp-ui-doc-enable t lsp-ui-doc-use-childframe t lsp-ui-doc-position 'top
		lsp-ui-doc-include-signature t lsp-ui-sideline-enable nil lsp-ui-flycheck-enable t
		lsp-ui-flycheck-list-position 'right lsp-ui-flycheck-live-reporting t
		lsp-ui-peek-enable t lsp-ui-peek-list-width 60 lsp-ui-peek-peek-height 25) 
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


;; Python specific
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
(use-package 
  py-autopep8 
  :ensure t 
  :commands py-autopep8
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

  ;; Scrolling in place (M-n and M-p)
  ;; Has a parameter to pass if you want scroll >1 lines (C-u <number>)
  (defun scroll-down-in-place (n) 
    (interactive "p") 
    (previous-line n) 
    (unless (eq (window-start) 
  	      (point-min)) 
      (scroll-down n)))
  
  ;; Scrolling in place (M-n and M-p)
  ;; Has a parameter to pass if you want scroll >1 lines (C-u <number>)
  (defun scroll-up-in-place (n) 
    (interactive "p") 
    (next-line n) 
    (unless (eq (window-end) 
  	      (point-max)) 
      (scroll-up n)))
  
  (global-set-key "\M-n" 'scroll-up-in-place)
  (global-set-key "\M-p" 'scroll-down-in-place)

  ;; Always turn on line wrap from screen
  (global-visual-line-mode 1)
  
  ;; shortcut to zap up to char
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-z") #'zap-up-to-char)
  
)

(use-package 
  deadgrep 
  :ensure t
  :config (global-set-key (kbd "C-c s") #'deadgrep))

;; Verb - For sending HTTP Requests
(use-package 
  verb 
  :commands verb
  :ensure t)
(use-package 
  org 
  :mode ("\\.org\\'" . org-mode) 
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-startup-folded t)
)

;; Get env vars from shell
(use-package 
  exec-path-from-shell 
  :commands exec-path-from-shell
  :ensure t
)


;; Use fd for dired
(use-package 
  fd-dired 
  :commands fd-dired
  :ensure t)



(use-package 
  helpful 
  :ensure t 
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
  ;; Check startup time
  (defun efs/display-startup-time () 
    (message "Emacs loaded in %s with %d garbage collections." (format "%.2f seconds" (float-time
  										     (time-subtract
  										      after-init-time
  										      before-init-time)))
  	   gcs-done))
  
  (add-hook 'emacs-startup-hook 
  	  (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1 file-name-handler-alist
  		last-file-name-handler-alist) 
  	  (efs/display-startup-time))
  
)


;; Terminal emulator
(use-package 
  vterm 
  :ensure t 
  :commands vterm
  )



 


;; Enable Auto revert mode
(global-auto-revert-mode 1)

;; Enable line and column number mode
(setq column-number-mode 1)
(setq line-number-mode 1)

;; Desktop mode
(desktop-save-mode 1)

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


;; full path in title bar and in mode line
;; (setq-default frame-title-format "%b (%f)")

;; (setq-default mode-line-format
;;    (quote
;;     ("%f     " mode-line-position
;;      (vc-mode vc-mode)
;;      "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))

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

;; Add another command to set-mark
(global-set-key (kbd "M-SPC") 'set-mark-command)

(put 'narrow-to-region 'disabled nil)

;; OSX: Use mdfind for locate
(setq locate-command "mdfind")


;; Turn off org adapt indentation to not include an extra white space for the heading
(setq org-adapt-indentation nil)


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

(global-set-key (kbd "M-k") 'copy-end-of-line)

(global-set-key (kbd "C-c m") 'minify-buffer-contents)

(global-set-key (kbd "C-c i") 'string-rectangle)


;; Go back to global mark shortcut
(global-set-key (kbd "C-`") 'pop-global-mark)

;; Shortcuts for registers
(global-set-key  (kbd "C-c y") 'copy-to-register )
(global-set-key  (kbd "C-c p") 'insert-register )

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

  ;; Org mode settings
  (setq org-startup-indented t org-hide-leading-stars t org-hide-emphasis-markers t
	org-odd-levels-only t)

  ;; Calendar shortcut
  (global-set-key (kbd "C-x c") 'calendar)
  
  					; Allow to resize images
  (setq org-image-actual-width nil)
  
  (defun replace_underscores_with_spaces () 
    "Replace those 'underscores' from gmail to spaces" 
    (interactive) 
    (while (search-forward " " nil t) 
      (replace-match " " nil t)))
  
  (global-set-key (kbd "C-c r") 'replace_underscores_with_spaces)
  
  (setq org-cycle-hide-block-startup t)
  


(use-package 
  magit
  :demand
  :ensure t 
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
  org-roam 
  :ensure t 
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0blayout dired-x yaml-mode whole-line-or-region which-key vterm verb vc-msg v-mode use-package tide super-save smex rustic rg restart-emacs real-auto-save py-autopep8 projectile org-roam org-journal olivetti nim-mode move-text magit lsp-ui lsp-python-ms lsp-pyright linum-relative json-mode js2-mode ivy-rich ido-completing-read+ helpful groovy-mode find-file-in-project fd-dired exec-path-from-shell esup emacsql-sqlite3 dot-mode dimmer deadgrep dash-functional counsel company-quickhelp company-lsp company-jedi color-theme-sanityinc-tomorrow avy async amx))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
