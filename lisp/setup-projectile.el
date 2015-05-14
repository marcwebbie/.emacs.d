(require 'use-package)

(use-package projectile
  :defer 1
  :init
  (projectile-global-mode t)
  :config
  ;; (setq projectile-enable-caching t)
  (setq projectile-use-git-grep t)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-completion-system 'grizzl)
  (add-to-list 'projectile-globally-ignored-files ".DS_Store" "*.pyc"))

(provide 'setup-projectile)
