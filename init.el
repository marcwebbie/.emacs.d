;; Turn off mouse interface early in startup to avoid momentary display
(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (expand-file-name file user-emacs-directory)))

(load-local "appearance")
(when (eq system-type 'darwin) (load-local "mac"))
(load-local "defaults")
(load-local "defuns")
(load-local "hippie")
(load-local "jstestdriver")


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
  :init
  (progn
    (global-auto-complete-mode t)
    (add-hook 'python-mode-hook 'jedi:ac-setup)))

(use-package coffee-mode
  :init
  (progn
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))

(use-package diminish)

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (drag-stuff-mode t))

(use-package emacs-lisp-mode
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode)
  :init (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el"))))

(use-package emmet-mode
  :diminish emmet-mode
  :init
  (progn
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))

(use-package expand-region)

(use-package flx-ido
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    (use-package ido-vertical-mode
      :init (ido-vertical-mode 1))
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode 1)
      (setq ido-use-faces nil)
      (setq ido-file-extensions-order '(".py" ".rb" ".el" ".js" ".coffee"))
      (add-to-list 'ido-ignore-files "\\.DS_Store"))))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
    (setq flycheck-display-errors-function nil)))

(use-package git-gutter
  :diminish git-gutter-mode
  :init (global-git-gutter-mode t))

(use-package haml-mode)

(use-package imenu-anywhere)

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
     (R . t))))

(use-package pomodoro
  :init (pomodoro-add-to-mode-line))

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

(use-package python
  :config (add-hook 'python-mode-hook (lambda() (setq mode-name "py"))))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package re-builder
  :init (setq reb-re-syntax 'string))

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
    (use-package robe
      :init (add-hook 'ruby-mode-hook 'robe-mode))
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

(use-package skewer-mode
  :init
  (progn
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode)
    (setq httpd-port 9090)))

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
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(use-package smex
  :init (smex-initialize))

(use-package subword
  :diminish subword-mode)

(use-package swoop)

(use-package visual-regexp
  :config
  (progn
    (use-package visual-regexp-steroids)))

(use-package web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)
                        (define-key web-mode-map [(return)] 'newline-and-indent)))))

(use-package winner
  :config (winner-mode 1))

(use-package yaml-mode)

(use-package yari)


;;;; Bindings
(load-local "key-bindings")


;; (custom-set-variables
;;  )
(custom-set-faces
 ;; '(font-lock-function-name-face ((t (:bold t))))
 ;; '(font-lock-keyword-face ((t (:bold t)))))
 '(web-mode-html-tag-face ((t (:foreground "#ffb070" :background nil))))
 '(sp-show-pair-match-face ((t (:foreground "#d33682"))))
 '(sp-show-pair-mismatch-face ((t (:foreground "#ff0000"))))
 )


;;;; ends init.el
