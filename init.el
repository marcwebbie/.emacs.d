;; Turn off mouse interface early in startup to avoid momentary display
(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(require 'use-package)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (expand-file-name file user-emacs-directory)))

(load-local "appearance")
(load-local "defaults")
(load-local "defuns")
(load-local "hippie")


;;;; Packages

(use-package ace-jump-mode)

(use-package ack-and-a-half
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

(use-package auto-complete
  :diminish auto-complete-mode
  :config (progn
            (global-auto-complete-mode t)))

(use-package coffee-mode
  :init
  (progn
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :config
  (progn
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)))

(use-package css-mode
  :if (not noninteractive)
  :ensure css-mode
  :config (setq css-indent-offset 2))

(use-package diminish)

(use-package drag-stuff
  :ensure drag-stuff
  :diminish drag-stuff-mode
  :init (drag-stuff-mode t))

(use-package emacs-lisp-mode
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode)
  :init (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el"))))

(use-package emmet-mode
  :ensure emmet-mode
  :diminish emmet-mode
  :init
  (progn
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))

(use-package expand-region
  :if (not noninteractive)
  :ensure expand-region
  :bind (("C-M-SPC" . er/expand-region)
         ("C-M-@" . er/expand-region)))

(use-package exec-path-from-shell)

(use-package ido
  :if (not noninteractive)
  :config
  (progn
    (use-package ido-vertical-mode
      :ensure ido-vertical-mode)
    (use-package flx
      :ensure flx)
    (use-package flx-ido
      :ensure flx-ido)
    (use-package ido-ubiquitous
      :config (ido-ubiquitous-mode 1)
      :ensure ido-ubiquitous)
    (setq ido-enable-flex-matching t
          ido-use-faces nil
          flx-ido-use-faces t
          ido-create-new-buffer 'always)
    (ido-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)
    (setq ido-file-extensions-order '(".py" ".rb" ".el" ".js"))
    (add-to-list 'ido-ignore-files '(".DS_Store" ".pyc"))
    (add-to-list 'ido-ignore-directories '("__pycache__" ".pyc"))
    (flx-ido-mode 1)))

(use-package files
  :config (progn
            (setq auto-save-default nil)
            (setq backup-directory-alist
                  `(("." . ,(expand-file-name
                             (concat user-emacs-directory "backups")))))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (setq flycheck-display-errors-function nil)))

(use-package git-gutter
  :diminish git-gutter-mode
  :init (global-git-gutter-mode t))

(use-package haml-mode)

(use-package ibuffer
  :if (not noninteractive)
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil
          ibuffer-saved-filter-groups
          (list (append
                 (cons "default"
                       ;; Generate filters by major modes from the
                       ;; auto-mode-alist
                       (let ((mode-filters))
                         (dolist (element auto-mode-alist)
                           (when (ignore-errors (fboundp (cdr element)))
                             (let* ((mode (cdr element))
                                    (name (if (string-match "\\(-mode\\)?\\'"
                                                            (symbol-name mode))
                                              (capitalize
                                               (substring (symbol-name mode)
                                                          0 (match-beginning 0)))
                                            (symbol-name mode))))
                               (when (not (assoc-string name mode-filters))
                                 (setq mode-filters
                                       (cons (list name (cons 'mode mode))
                                             mode-filters))))))
                         mode-filters))
                 ;; Custom added filters.
                 '(("Magit" (name . "^\\*magit"))
                   ("Irc" (mode . rcirc-mode))
                   ("Css" (mode . scss-mode))
                   ("W3m" (name . "^\\*w3m"))))))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))))

(use-package imenu-anywhere)

(use-package ispell
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(use-package flyspell
  :requires ispell
  :config
    (add-hook 'text-mode-hook 'turn-on-flyspell)
    ;; (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
    (setq-default js2-basic-offset 2)
    (setq-default js2-auto-indent-p t)
    (setq-default js2-cleanup-whitespace t)
    (setq-default js2-enter-indents-newline t)
    (setq-default js2-global-externs "jQuery $")
    (setq-default js2-indent-on-enter-key t)
    (setq-default js2-show-parse-errors nil) ;; We'll let fly do the error parsing...
    (setq-default js2-mode-indent-ignore-first-tab t)))

(use-package magit
  :diminish magit-auto-revert-mode
  :init
  (progn
    (set-default 'magit-stage-all-confirm nil)
    (set-default 'magit-unstage-all-confirm nil)))

(use-package menu-bar
  :bind ("M-k" . kill-this-buffer))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook (lambda() (setq mode-name "md")))
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package multiple-cursors)

(use-package nyan-mode
  :init (nyan-mode 1))

(use-package org
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ruby . t)
     (lisp . t)
     (R . t)))
  :config
  (add-hook 'org-mode-hook
            (lambda()
              (flyspell-mode 1))))

(use-package pallet
  :init (pallet-mode t))

(use-package pelican-mode)

(use-package projectile
  :diminish projectile-mode
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-use-git-grep t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package projectile-rails
  :init (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package puppet-mode)

(use-package pyenv-mode)

(use-package python
  :config
  (progn
    (add-hook 'python-mode-hook
              '(lambda () (pyenv-mode)))
    (add-hook 'python-mode-hook
              '(lambda () (anaconda-mode)))
    (add-hook 'python-mode-hook
          '(lambda ()
             (flycheck-mode)
             (flycheck-select-checker 'python-flake8)))
    (add-hook 'python-mode-hook (lambda() (setq mode-name "py")))))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package rainbow-delimiters
  :if (not noninteractive)
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package region-bindings-mode
  :if (not noninteractive)
  :config (progn
            (bind-key "m" 'mc/mark-more-like-this-extended region-bindings-mode-map)
            (bind-key "a" 'mc/mark-all-like-this region-bindings-mode-map)
            (bind-key "p" 'mc/mark-previous-like-this region-bindings-mode-map)
            (bind-key "n" 'mc/mark-next-like-this region-bindings-mode-map)
            (bind-key "b" 'mc/skip-to-previous-like-this region-bindings-mode-map)
            (bind-key "f" 'mc/skip-to-next-like-this region-bindings-mode-map)
            (bind-key "P" 'mc/unmark-previous-like-this region-bindings-mode-map)
            (bind-key "N" 'mc/unmark-next-like-this region-bindings-mode-map)
            (bind-key "u" 'er/contract-region region-bindings-mode-map)
            (setq region-bindings-mode-disabled-modes '(term-mode))
            (setq region-bindings-mode-disable-predicates
                  (list (lambda () buffer-read-only)))
            (region-bindings-mode-enable)))


(use-package re-builder
  :init (setq reb-re-syntax 'string))

(use-package rst
  :config
  (add-hook 'rst-mode-hook
            (lambda()
              (flyspell-mode 1))))

(use-package ruby-mode
  :init
  (progn
    (add-hook 'ruby-mode-hook (lambda() (setq mode-name "rb")))
    (use-package ruby-tools
      :diminish ruby-tools-mode)
    (use-package ruby-test-mode
      :diminish ruby-test-mode)
    (use-package rbenv
      :init
      (progn
        (global-rbenv-mode)
        (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
        (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))))
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

(use-package s)

(use-package sass-mode)

(use-package saveplace
  :config (setq-default save-place t))

(use-package scala-mode
  :init (modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8)
  :mode "\\.\\(scala\\|sbt\\)\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package shoulda)

(use-package slime
  :mode (("\\.lisp$" . lisp-mode)
         ("\\.clisp$" . lisp-mode))
  :config
  (progn
    (setq inferior-lisp-program (executable-find "clisp"))
    (setq slime-contribs '(slime-fancy))
    (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
    (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "*" "*")
      (sp-local-pair "(" nil :bind "C-("))
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(use-package smex
  :init (smex-initialize))

(use-package subword
  :diminish subword-mode)

(use-package virtualenvwrapper
  :init
  (progn
    (venv-initialize-interactive-shells) ;; interactive shell support
    (venv-initialize-eshell) ;; eshell support
    (setq venv-location "~/.virtualenvs/")))

(use-package visual-regexp
  :config
  (progn
    (use-package visual-regexp-steroids)))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
          ("\\.html\\.erb\\'" . web-mode)
          ("\\.mustache\\'" . web-mode)
          ("\\.jinja\\'" . web-mode))
  :init
  (progn
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-pairing nil))
  :config
  (progn
    (setq web-mode-engines-alist
          '(("django" . "\\.djhtml")
            ;; ("django" . my-current-buffer-django-p)) ;; set engine to django on django buffer
            ("django" . "templates/.*\\.html")))
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-style-padding 2)
                (setq web-mode-script-padding 2)
                (setq web-mode-markup-indent-offset 2)
                (define-key web-mode-map [(return)] 'newline-and-indent)))))

(use-package winner
  :config (winner-mode 1))

(use-package yaml-mode)


;;;; Bindings
(load-local "key-bindings")


;;;; OSX settings
(when (eq system-type 'darwin) (load-local "osx"))


;;;; Custom variables
(setq custom-file (expand-file-name "custom.el" use-emacs-directory))
(load-local "custom" 'noerror)


(provide 'init)
;;; init.el ends here
