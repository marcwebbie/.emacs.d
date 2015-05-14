(use-package python
  :defer 3
  :init
  :config
  (add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))
  (bind-key "<f9>" 'mw/add-py-debug python-mode-map)
  (bind-key "C-<f9>" 'mw/add-pudb-debug python-mode-map)

  (use-package eldoc-mode
    :commands (eldoc-mode)
    :init (add-hook 'python-mode-hook 'eldoc-mode))

  (use-package anaconda-mode
    :init
    ;; (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook '(lambda () (anaconda-mode))))

    (use-package elpy
      :bind (("C-c t" . elpy-test-django-runner)
             ("C-c C-f" . elpy-find-file)
             ("C-c C-;" . mw/set-django-settings-module))
      :init
      (elpy-enable)
      (defun mw/set-elpy ()
        (let ((python (executable-find "python")))
          (setq
           elpy-test-discover-runner-command (list python "-m" "unittest")
           elpy-test-django-runner-command (list python "manage.py" "test" "--noinput"))))
      (add-hook 'pyvenv-post-activate-hooks 'mw/set-elpy)
      :config
      (delete 'elpy-module-highlight-indentation elpy-modules)
      (delete 'elpy-module-flymake elpy-modules)
      (delete 'elpy-module-yasnippet elpy-modules)
      (setq elpy-rpc-backend "jedi")
      )
  )

(provide 'setup-python)
