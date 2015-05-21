(require 'use-package)

(use-package powerline
  :init
  (powerline-default-theme)
  ;; (powerline-center-theme)
  :config
  (setq powerline-display-hud nil)
  (setq powerline-default-separator 'zigzag)
  (setq powerline-text-scale-factor 1.1)
  )

(provide 'setup-powerline)
