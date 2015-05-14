(require 'use-package)

(use-package ruby-mode
  :disabled t
  :config
  (progn
    (add-hook 'ruby-mode-hook (lambda() (setq mode-name "rb")))
    (use-package ruby-tools
      :diminish ruby-tools-mode)
    (use-package ruby-test-mode
      :diminish ruby-test-mode
      :bind ("C-c t b" . ruby-test-run))
    (use-package shoulda
      :config
      (progn
        (bind-key "C-c t s" 'shoulda-run-should-at-point ruby-mode-map)
        (bind-key "C-c t c" 'shoulda-run-context-at-point  ruby-mode-map)))
    (use-package rbenv
      :config
      (progn
        (global-rbenv-mode)
        (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:"
                               (getenv "HOME") "/.rbenv/bin:"
                               (getenv "PATH")))
        (setq exec-path
              (cons (concat (getenv "HOME") "/.rbenv/shims")
                    (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))))
    (use-package inf-ruby
      :config
      (progn
        (add-hook 'after-init-hook 'inf-ruby-switch-setup)
        (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)))
    (use-package rspec-mode
      :config
      (progn
        (setq rspec-use-rake-flag nil)
        (defadvice rspec-compile (around rspec-compile-around activate)
          "Use BASH shell for running the specs because of ZSH issues."
          (let ((shell-file-name "/bin/bash")) ad-do-it)))))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(provide 'setup-ruby)
