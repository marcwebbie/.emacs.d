(require 'use-package)

(use-package recentf
  :commands recentf-mode
  :bind (("C-c f" . recentf-ido-find-file))
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000))

(provide 'setup-recentf)
