;; -*- lexical-binding: t -*-
;;; init.el --- early bird -*- no-byte-compile: t -*-

;; Set custom set vars in a different file instead of at the end of the init file.
;; Show no errors or messages when loading
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq package-archives nil)

;; From Melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/"))
               t)
  (add-to-list 'package-archives
               (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))
               t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-expand-minimally t)
  (require 'use-package))

(setq package-install-upgrade-built-in t)

(use-package esup
  :defer t
  :ensure t
  :pin melpa
  :config
  (setq esup-depth 0))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :demand t
    :config (exec-path-from-shell-initialize))
  (setq locate-command "mdfind"))

;; Emacs 30+ has built-in modus themes
(use-package emacs
  :init
  (require-theme 'modus-themes)
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  (modus-themes-load-theme 'modus-vivendi-tritanopia))

;; super-save
(use-package super-save
  :ensure t
  :demand t
  :config
  (setq super-save-idle-duration 0.75
        super-save-auto-save-when-idle t
        super-save-max-buffer-size 9999999999999999)
  (super-save-mode +1)
  (setq auto-save-default nil)

 ;; LSP issues when saving edits in files in node_modules.  Turn off auto save in those files
 (setq super-save-exclude '("/node_modules/"))
)

(use-package auto-compile
  :ensure t
  :defer t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 15
        doom-modeline-bar-width 8
        doom-modeline-window-width-limit 3
        doom-modeline-lsp t))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Inconsolata Nerd Font"))

(use-package projectile
  :ensure t
  :defer t
  :init (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :config
  (setq ffip-use-rust-fd t
        projectile-require-project-root nil
        projectile-enable-caching nil))

;; ECA
(use-package eca
  :ensure t
  :defer t
  :bind (("C-c e c" . eca-chat)
         ("C-c e a" . eca-chat-add-context-at-point))
  :config
  (setq eca-custom-command nil
        eca-completion-enable t)
  (global-eca-completion-mode 1))

;; === Modern Completion Stack ===
(use-package vertico
  :ensure t
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package consult
  :ensure t
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("C-x C-f" . consult-find)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("C-c s"   . consult-ripgrep))
  :custom
  (consult-preview-key "M-.")
  :config
  (setq consult-project-function #'projectile-project-root))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; In-buffer completion: Corfu + Cape (replaces Company)
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)))

(use-package cape
  :ensure t
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; LSP (updated for Corfu)
(use-package lsp-mode
  :ensure t
  :demand t
  :commands lsp-mode
  :custom
  (lsp-completion-provider :none)   ; Let Corfu handle completions
  :config
  (setq lsp-headerline-breadcrumb-enable 1
        read-process-output-max (* 1024 1024)
        lsp-rust-server 'rust-analyzer
        lsp-enable-snippet nil)

  (global-unset-key (kbd "s-l"))
  (global-set-key (kbd "s-l l") 'lsp-find-references)
  (global-set-key (kbd "s-l d") 'lsp-describe-thing-at-point)
  (global-set-key (kbd "s-l f") 'lsp-find-definition)

  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'rustic-mode-hook #'lsp)
  (add-hook 'nim-mode-hook #'lsp)
  (add-hook 'graphql-mode-hook #'lsp))

(use-package lsp-ui
  :ensure t
  :requires lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-peek-enable t
        lsp-ui-doc-show-with-cursor t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ivy :ensure t :requires lsp-mode)  ; optional, can remove if unused

;; Other language / mode packages (kept from your original)
(use-package
 tide
 :ensure t
 :defer t
 :commands tide
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
))

(use-package
 web-mode
 :ensure t
 :defer t
 :commands web-mode
 :config (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
 (add-hook
  'web-mode-hook
  (lambda ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))
)

(use-package
 js2-mode
 :ensure t
 :demand
 :commands js2-mode
 :config
 ;; Use js2-mode for JS files
 (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package
 typescript-mode
 :commands typescript-mode
 :ensure t
 :demand)

;; Python specific
(use-package
 lsp-pyright
 :ensure t
 :defer t
 :hook
 (python-mode
  .

  (lambda ()
    (require 'lsp-pyright)
    (lsp)))) ; or lsp-deferred

(use-package py-autopep8 :ensure t :defer t :commands py-autopep8)

(use-package
 robot-mode
 :ensure t
 :defer t
 :config
 (add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))
 (global-set-key (kbd "C-c TAB") 'robot-mode-add-argument))

;; Vlang
(use-package
 v-mode
 :ensure t
 :defer t
 :commands v-mode
 :config
 ;; Remove verilog mode since it's covers *.v files which I now want to refer to Vlang
 (defun replace-alist-mode (alist oldmode newmode)
   (dolist (aitem alist)
     (if (eq (cdr aitem) oldmode)
         (setcdr aitem newmode))))

 (replace-alist-mode auto-mode-alist 'verilog-mode 'v-mode)
)

(use-package yaml-mode :defer t :commands yaml-mode :ensure t)
(use-package dockerfile-mode :ensure t :commands dockerfile-mode :defer t)
(use-package groovy-mode :defer t :commands groovy-mode :ensure t)
(use-package json-mode :defer t :commands json-mode :ensure t)
(use-package rustic :defer t :commands rustic :ensure t)
(use-package dart-mode :ensure t)
(use-package flutter :ensure t)
(use-package lsp-dart :ensure t)

;; Flycheck, Flyspell, etc.
(use-package flycheck :ensure t :defer t)
(use-package flyspell
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(use-package pos-tip :defer t)

;; === Your other utilities ===
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode)
  (global-set-key (kbd "M-/") #'comment-or-uncomment-region))

(use-package rainbow-delimiters
  :ensure t
  :demand t
  :custom (rainbow-delimiters-max-face-count 4)
  :hook ((prog-mode yaml-mode xml-mode mhtml-mode)
         . rainbow-delimiters-mode))

;;; Show Paren when inside of them
(define-advice show-paren-function (:around (fn) fix)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn))))
)

(use-package
 org-journal
 :ensure t
 :demand
 :hook (org-journal-mode . writeroom-mode)
 :config
 (setq org-journal-dir "~/journal/emacs_journal")
 (setq org-journal-date-format "%A, %d %B %Y")
 (setq org-journal-file-format "%Y-%m-%d.org")
)

;; Distraction Free writing
(use-package
 writeroom-mode
 :ensure t
 :init
 (defcustom writeroom-fullscreen-effect 'maximized
  "Effect applied when enabling fullscreen.
  The value can be `fullboth', in which case fullscreen is
  activated, or `maximized', in which case the relevant frame is
  maximized but window decorations are still available."
   :group 'writeroom
   :type '(choice (const :tag "Fullscreen" fullboth)
		  (const :tag "Maximized" maximized)))
 :config
 (advice-add 'text-scale-adjust :after
   #'visual-fill-column-adjust)
)

(use-package
 avy
 :ensure t

 :init
 (global-set-key (kbd "C-j") 'avy-goto-char-timer)
 (defun change-cycle-agenda-files-key ()
   (local-set-key (kbd "C-j") 'avy-goto-char-timer)
   (local-unset-key (kbd "C-'"))
   (local-set-key (kbd "C-'") 'org-agenda))

 ;;https://gist.github.com/karthink/af013ffd77fe09e67360f040b57b4c7b
 (defun avy-show-dispatch-help ()
   (let* ((len (length "avy-action-"))
          (fw (frame-width))
          (raw-strings
           (mapcar
            (lambda (x)
              (format "%2s: %-19s"
                      (propertize (char-to-string (car x))
                                  'face
                                  'aw-key-face)
                      (substring (symbol-name (cdr x)) len)))
            avy-dispatch-alist))
          (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
          (strings-len (length raw-strings))
          (per-row (floor fw max-len))
          display-strings)
     (cl-loop
      for
      string
      in
      raw-strings
      for
      N
      from
      1
      to
      strings-len
      do
      (push (concat string " ") display-strings)
      (when (= (mod N per-row) 0)
        (push "\n" display-strings)))
     (message "%s" (apply #'concat (nreverse display-strings)))))
 :config
 (add-hook 'org-mode-hook 'change-cycle-agenda-files-key)
 (add-hook 'v-mode-hook 'change-cycle-agenda-files-key)
 (setq avy-keys '(?a ?s ?g ?h ?j ?k ?l))

 (defun avy-action-copy-whole-line (pt)
   (save-excursion
     (goto-char pt)
     (cl-destructuring-bind
      (start . end)
      (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
   (select-window (cdr (ring-ref avy-ring 0)))
   t)
 (defun avy-action-kill-whole-line (pt)
   (save-excursion
     (goto-char pt)
     (kill-whole-line))
   (select-window (cdr (ring-ref avy-ring 0)))
   t)

 (defun avy-action-copy-sexp (pt)
   "Copy the sexp at PT and yank it at the current point"
   (interactive)
   (let ((orig-point (point))
         (sexp-text
          (save-excursion
            (goto-char pt)
            (thing-at-point 'sexp))))
     (kill-new sexp-text)
     (message "Copied: %s" sexp-text))
   (select-window (cdr (ring-ref avy-ring 0)))
   t)
 (defun avy-action-copy-end-of-line (pt)
   (save-excursion
     (goto-char pt)
     (copy-end-of-line))
   (select-window (cdr (ring-ref avy-ring 0)))
   t)

 (defun avy-action-kill-end-of-line (pt)
   (save-excursion
     (goto-char pt)
     (kill-visual-line))
   (select-window (cdr (ring-ref avy-ring 0)))
   t)
 (defun avy-action-mark-from-current-pt (pt)
   (activate-mark)
   (goto-char pt))

 (defun avy-lsp-find-definition (pt)
   "Find definition for symbol under avy selection using lsp-find-definition."
   (interactive)
   (goto-char pt)
   (lsp-find-definition))


 ;; Add option to dispatch list
 (setf
  (alist-get ?C avy-dispatch-alist) 'avy-action-copy-whole-line
  (alist-get ?M avy-dispatch-alist) 'avy-action-mark-from-current-pt
  (alist-get ?D avy-dispatch-alist) 'avy-action-kill-whole-line
  (alist-get ?N avy-dispatch-alist) 'avy-action-copy-end-of-line
  (alist-get ?K avy-dispatch-alist) 'avy-action-kill-end-of-line
  (alist-get ?c avy-dispatch-alist) 'avy-action-copy-sexp
  (alist-get ?f avy-dispatch-alist) 'avy-lsp-find-definition))


(use-package
 avy-zap
 :ensure t
 :config
 (global-set-key (kbd "M-Z") 'avy-zap-to-char-dwim)
 (global-set-key (kbd "M-z") 'avy-zap-up-to-char-dwim))


(use-package deadgrep
  :ensure t
  :config (global-set-key (kbd "C-c s") #'deadgrep))  ; Note: C-c s also used by consult-ripgrep

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-startup-folded t
        org-startup-indented t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-odd-levels-only t
        org-cycle-hide-block-startup t)
  (global-set-key (kbd "C-'") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

(use-package org-roam
  :ensure t
  :after org
  :defer t
  :hook (org-mode . org-roam-db-autosync-enable)
  :config
  (setq org-roam-v2-ack t
        org-roam-directory "~/journal/org-roam")
  (org-roam-db-autosync-mode)
  (global-set-key (kbd "C-c n l") 'org-roam-capture)
  (global-set-key (kbd "C-c n f") 'org-roam-node-find)
  (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c n b") 'org-roam-buffer-display-dedicated)
  (global-set-key (kbd "C-c n g") 'org-roam-graph))

;; https://zzamboni.org/post/how-to-insert-screenshots-in-org-documents-on-macos/
(use-package
 org-download
 :after org
 :defer t
 :ensure t
 :defer t
 :custom
 (org-download-method 'directory)
 (org-download-image-dir "images")
 (org-download-heading-lvl nil)
 (org-download-timestamp "%Y%m%d-%H%M%S_")
 (org-image-actual-width 300)
 (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
 :config (require 'org-download)

 ;; Pasting text should still word wrap
 (setq term-suppress-hard-newline t))


;; Verb - For sending HTTP Requests
(use-package
 verb
 :commands verb
 :defer t
 :ensure t
 :config
 (defun replace-double-quotes-in-string-no-prompt (str)
   "From Bing AI: Replace all double quotes with a backslash and double quotes in the given string without prompting the user."
   (with-temp-buffer
     (insert str)
     (goto-char (point-min))
     (while (search-forward "\"" nil t)
       (replace-match "\\\\\""))
     (buffer-string)))
 (defun graphql-to-json (rs)
   ;; Modify RS and return it (RS is a request specification, type `verb-request-spec')
   (oset
    rs
    body
    (replace-double-quotes-in-string-no-prompt (oref rs body)))
   (oset rs body (replace-regexp-in-string "\n" " " (oref rs body)))
   (oset rs body (format "{\"query\": \"%s \"}" (oref rs body)))
   rs)
 (defun graphql-mutation-to-json (rs)
   ;; Modify RS and return it (RS is a request specification, type `verb-request-spec')
   (oset
    rs
    body
    (replace-double-quotes-in-string-no-prompt (oref rs body)))
   (oset rs body (replace-regexp-in-string "\n" " " (oref rs body)))
   (oset
    rs
    body
    (format "{\"query\": \"mutation { %s } \"}" (oref rs body)))
   rs))

(use-package graphql :ensure t :defer t)

(use-package
 magit
 :ensure t
 :defer t
 :commands magit
 :config
 (global-unset-key (kbd "C-c M-g"))
 (global-set-key (kbd "C-c g") 'magit-file-dispatch)
 (global-set-key (kbd "C-c F") 'magit-pull)
 (global-set-key (kbd "C-c B") 'magit-branch)
 (global-set-key (kbd "C-x g") 'magit)
 ;; From https://scripter.co/narrowing-the-author-column-in-magit/
 (setq magit-log-margin
       '(t "%Y-%m-%d %H:%M" magit-log-margin-width :author 18)))

(use-package helpful
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h .") #'helpful-at-point))

(use-package vterm :ensure t :demand t)
(use-package diff-hl :ensure t :config (global-diff-hl-mode))
(use-package restart-emacs :ensure t)
(use-package elisp-autofmt :ensure t :defer t :hook (emacs-lisp-mode . elisp-autofmt-mode))
(use-package casual-symbol-overlay
  :ensure t
  :config (global-set-key (kbd "C-c o") 'casual-symbol-overlay-tmenu))

(use-package ultra-scroll
  :ensure t
  :init (setq scroll-conservatively 101 scroll-margin 0)
  :config (ultra-scroll-mode 1))

;; Move mini-buffer to top mini frame
(use-package
 mini-frame
 :ensure t
 :config (mini-frame-mode 1)
 (custom-set-variables
  '(mini-frame-show-parameters
    '((top . 10) (width . 0.7) (left . 0.5)))))


;; Shows key bindings for incomplete commands
(use-package
 which-key
 :ensure t
 :config
 (which-key-mode)
 (which-key-setup-minibuffer)
 (setq which-key-popup-type 'minibuffer))

;; Dim other windows
(use-package
 dimmer
 :ensure t
 :config
 (dimmer-configure-which-key)
 (dimmer-mode t)
 (setq dimmer-fraction 0.6))

;; Accent selected window
(use-package
 selected-window-accent-mode
 :ensure t
 :custom
 (selected-window-accent-fringe-thickness 6)
 (selected-window-accent-custom-color nil)
 (selected-window-accent-mode-style 'tiling))


;; === Miscellaneous Settings ===
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "M-9") 'kmacro-start-macro)
(global-set-key (kbd "M-0") 'kmacro-stop-macro)
(global-set-key (kbd "M--") 'kmacro-end-and-call-macro)

(tool-bar-mode -1)
(setq find-file-visit-truename t)

;; Display relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'visual)
(setq display-line-numbers-width 0)

;; move text easily up and down
(use-package move-text :ensure t :config (move-text-default-bindings))

;; Trying Prot's settings
;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:8d8c57cc-04c9-408f-aca1-6493bc5d8f0d
(let ((mono-spaced-font "Monaco")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 260)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; Kill current buffer with extra confirmation
(global-unset-key (kbd "s-k"))
(global-set-key (kbd "M-s-k") 'kill-current-buffer)

(global-set-key (kbd "C-`") 'pop-global-mark)
(global-set-key (kbd "s-SPC") 'set-mark-command)
(setq set-mark-command-repeat-pop 1)
(global-set-key (kbd "M-`") (kbd "C-u C-SPC"))

(global-set-key (kbd "C-x c") 'calendar)

;; Auto-save on buffer/window switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
;; Similar advice for windmove (kept from original)

(make-directory "~/emacs/autosaves/" t)
(setq-default backup-directory-alist '((".*" . "~/emacs/backups/")))

;; Scratch buffer
(defun generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "scratchbuffer"))
  (json-mode))
(global-set-key (kbd "C-c b") 'generate-scratch-buffer)

;; Scroll in place
(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))
(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)

(global-visual-line-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(desktop-save-mode 1)

;; Minify functions
(defun minify-buffer-contents ()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "" nil t)))
(global-set-key (kbd "C-x m") 'minify-buffer-contents)

(defun minify-buffer-region (start end)
  "Remove newline and space characters in the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward-regexp "[\n ]" nil t)
      (replace-match ""))))
(global-set-key (kbd "C-c m") 'minify-buffer-region)

;; Other custom functions and keys from your original
(global-set-key (kbd "M-s") 'copy-sexp-on-point)
(global-set-key (kbd "M-k") 'copy-end-of-line)
(global-set-key (kbd "M-g v") 'mark-whole-line)
(global-set-key (kbd "C-c y") 'copy-to-register)
(global-set-key (kbd "C-c p") 'insert-register)  ; note: conflicts with projectile, adjust if needed
(global-set-key (kbd "C-c r") 'replace_underscores_with_spaces)
(global-set-key (kbd "C-x M-d") 'kill-file-and-directory-buffers)
(global-set-key (kbd "s-f") 'get-buffer-filename)

;; LSP booster advice (kept from original)
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))
(advice-add (if (fboundp 'json-parse-buffer) 'json-parse-buffer 'json-read)
            :around #'lsp-booster--advice-json-parse)

;; Final startup hook
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(put 'upcase-region 'disabled nil)
