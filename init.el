;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'f)
(require 'use-package)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "appearance")
(when (eq system-type 'darwin) (load-local "mac"))
(load-local "defaults")
(load-local "defuns")
(load-local "hippie")


;;;; Hooks

;; Clean trailling whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; handle shell colours
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;; Packages

(use-package ace-jump-mode)

(use-package ack-and-a-half)

(use-package auto-complete
  :init (global-auto-complete-mode t))

(use-package coffee-mode
  :init
  (progn
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))

(use-package diminish
  :config
  (progn
    (eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
    (eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode))
    (eval-after-load "eldoc" '(diminish 'eldoc-mode))
    (eval-after-load "flycheck" '(diminish 'flycheck-mode))
    (eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
    (eval-after-load "guide-key" '(diminish 'guide-key-mode))
    (eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
    (eval-after-load "smartparens" '(diminish 'smartparens-mode))
    (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))))

(use-package drag-stuff
  :init (drag-stuff-mode t))

(use-package emacs-lisp-mode
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package expand-region)

(use-package flx-ido
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    (setq ido-use-faces nil)
    (setq ido-file-extensions-order '(".py" ".rb" ".el" ".coffee" ".js"))
    (add-to-list 'ido-ignore-files "\\.DS_Store"))
  :config
  (progn
    (use-package ido-vertical-mode
      :init (ido-vertical-mode 1))
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode 1))))

(use-package flycheck
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package git-gutter
  :init (global-git-gutter-mode t))

(use-package haml-mode)

(use-package magit
  :init
  (progn
    (set-default 'magit-stage-all-confirm nil)
    (set-default 'magit-unstage-all-confirm nil)))

(use-package markdown-mode
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package multiple-cursors)

(use-package nyan-mode
  :init (nyan-mode 1))

(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package projectile-rails
  :init (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package ruby-mode
  :init
  (progn
    (use-package ruby-tools)
    (use-package ruby-test-mode)))

(use-package s)

(use-package sass-mode)

(use-package saveplace
  :config (setq-default save-place t))

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-python)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(use-package smex
  :init (smex-initialize))

(use-package visual-regexp
  :config
  (progn
    (use-package visual-regexp-steroids)))

(use-package winner
  :config (winner-mode 1))

(use-package yaml-mode)


;;;; Bindings

(load-local "key-bindings")

;;;; ends init.el
