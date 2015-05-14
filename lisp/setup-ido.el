(require 'use-package)

(use-package ido
  :defer 2
  :init
  (ido-mode t)
  :config
  (ido-vertical-mode t)
  (flx-ido-mode t))

(provide 'setup-ido)
