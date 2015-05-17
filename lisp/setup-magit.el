(use-package magit
  :diminish magit-auto-revert-mode
  :bind ("C-x g" . magit-status)
  :commands magit-status
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (bind-key "q" 'magit-quit-session magit-status-mode-map))

(provide 'setup-magit)
