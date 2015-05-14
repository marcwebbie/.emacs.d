(require 'use-package)

(use-package ace-window
  :defer 3
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?q ?w ?e ?r ?a ?s ?d ?f))
  )

(provide 'setup-ace-window)
