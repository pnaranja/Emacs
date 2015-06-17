;; Set the new <leader> and other keys
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "w" 'save-buffer
  "bn" 'next-buffer
  "bd" 'kill-buffer
  "l" 'buffer-menu
  "bo" 'kill-other-buffers)

;;;; Moving to different windows
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
