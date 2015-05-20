(require 'use-package)
(require 'ido-vertical-mode)

(use-package ido
  :defer 3
  :init
  (ido-mode t)
  :config
  (setq ido-case-fold t)
  (setq ido-show-dot-for-dired nil)
  (setq ido-file-extensions-order '(".py" ".rb" ".el" ".js"))
  (add-to-list 'ido-ignore-files '(".DS_Store" ".pyc"))
  (add-to-list 'ido-ignore-directories '("__pycache__", ".git"))
  (use-package ido-vertical-mode
    :config
    (ido-vertical-mode 1)
    )
  (use-package ido-ubiquitous
    :config
    (ido-ubiquitous-mode t))
  (use-package flx-ido
    :config
    (flx-ido-mode t))
  )
(provide 'setup-ido)
