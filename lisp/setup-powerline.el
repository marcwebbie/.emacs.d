(require 'use-package)

(use-package powerline
  :init
  (powerline-default-theme)
  :config
  (setq powerline-default-separator 'zigzag)
  )

(provide 'setup-powerline)
