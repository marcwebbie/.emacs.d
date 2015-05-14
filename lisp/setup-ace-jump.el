(require 'use-package)

(use-package ace-jump-mode
  :defer 3
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode-pop-mark))
  :config
  (setq ace-jump-mode-case-fold t)
  )

(provide 'setup-ace-jump)
