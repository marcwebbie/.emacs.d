(require 'use-package)

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode t)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  )

(provide 'setup-golden-ratio)
