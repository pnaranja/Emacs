(require 'sublimity)
;; (require 'sublimity-scroll)
(require 'sublimity-map)
(require 'sublimity-attractive)

(setq sublimity-scroll-weight 5
      sublimity-scroll-drift-length 5)

(setq sublimity-map-size 25)
(setq sublimity-map-fraction 0.3)
(setq sublimity-map-text-scale -7)

(sublimity-map-set-delay 2)

(add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode)))

(setq sublimity-attractive-centering-width nil)

(sublimity-attractive-hide-bars)
(sublimity-attractive-hide-vertical-border)
(sublimity-attractive-hide-fringes)
(sublimity-attractive-hide-modelines)
