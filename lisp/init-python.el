(use-package python
  :defer 3
  :init
  (add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))
  :config
  (bind-key "<f9>" 'mw/add-py-debug python-mode-map)
  (bind-key "C-<f9>" 'mw/add-pudb-debug python-mode-map)

  (use-package eldoc-mode
    :commands (eldoc-mode)
    :init (add-hook 'python-mode-hook 'eldoc-mode))

  (use-package anaconda-mode
    :init
    ;; (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook '(lambda () (anaconda-mode))))
  )

(provide 'init-python)
