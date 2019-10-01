;; Set default evil mode as normal
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-default-state 'normal)

;; Set the new <leader> and other keys
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f" 'ido-find-file
  "w" 'save-buffer
  "c" 'clipboard-kill-ring-save
  "p" 'clipboard-yank
  "l" 'buffer-menu)

;;;; Moving to different windows
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

