(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'palette)
(require 'use-package)


;; ===========================================
;; custom
;; ===========================================
(setq debug-on-error t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-startup-message t)

(setq default-directory (f-full (getenv "HOME")))

(setq backup-directory-alist `(("." . "~/.saves")))

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "sane-defaults")
(load-local "defuns")
(load-local "appearance")
(load-local "hippie")
(load-local "keybindings")


;; ===========================================
;; Setup ido
;; ===========================================
(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-file-extensions-order '(".rb" ".el" ".coffee" ".js"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config (setq ido-use-faces nil)) ;; disable ido faces to see flx highlights.

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))    


;; ===========================================
;; Setup smex
;; ===========================================
(use-package smex
  :init (smex-initialize))


;; ===========================================
;; Setup flycheck
;; ===========================================
(use-package flycheck
  :config
  (progn
    (make-variable-buffer-local 'flycheck-idle-change-delay)
    (add-hook 'after-init-hook 'global-flycheck-mode)))


;; ===========================================
;; Setup autocomplete
;; ===========================================
(use-package auto-complete
  :init (global-auto-complete-mode t))


;; ===========================================
;; Setup yasnippet
;; ===========================================
(use-package popup)
(use-package yasnippet
  :init (yas-global-mode t))


;; ===========================================
;; Setup smartparens
;; ===========================================
(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

;; ===========================================
;; Setup ruby
;; ===========================================
(use-package smartparens-ruby) 

(use-package inf-ruby
  :config (progn
            (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
            (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
            (add-hook 'after-init-hook 'inf-ruby-switch-setup))
  :bind ("C-c C-z" . run-ruby))

(use-package ruby-mode
  :config (setq ruby-deep-indent-paren nil)
  :bind (("C-M-h" . backward-kill-word)
         ("C-M-n" . scroll-up-five)
         ("C-M-p" . scroll-down-five))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Thorfile" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(use-package robe
  :init(progn
         (add-hook 'ruby-mode-hook 'robe-mode)
         (add-hook 'robe-mode-hook 'robe-ac-setup))
  :bind("M-." . robe-jump))

(use-package projectile-rails
  :init(progn
         (add-hook 'projectile-mode-hook 'projectile-rails-on)))


;; (use-package ruby-test-mode
;;   :disabled t
;;   :config (add-hook 'ruby-mode-hook 'ruby-test-mode))

;; ===========================================
;; Setup python
;; ===========================================
(use-package elpy
  :init (elpy-enable)
  :mode (("\\.wsgi\\'" . python-mode)))


;; ===========================================
;; Setup projectile
;; ===========================================
(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'flx-ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

;; ===========================================
;; Setup quickrun
;; ===========================================
(use-package quickrun)


;; ===========================================
;; Setup search
;; ===========================================
(use-package visual-regexp
  :init (use-package visual-regexp-steroids)
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         ("C-r" . vr/isearch-backward)
         ("C-s" . vr/isearch-forward)))


;; ===========================================
;; Setup diminish
;; ===========================================
(use-package diminish
  :config (progn ()))


;; ===========================================
;; Setup rainbow-delimiters
;; ===========================================
(use-package rainbow-delimiters
  init: (global-rainbow-delimiters-mode))


;; ===========================================
;; Setup magit
;; ===========================================
(use-package magit
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t))
  :bind ("C-x g" . magit-status))


;; ===========================================
;; Setup emacs-lisp-mode
;; ===========================================
(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :bind (("M-&" . lisp-complete-symbol)
         ("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))


;; ===========================================
;; Setup rainbow-delimiter-mode
;; ===========================================
(use-package rainbow-delimiters
  :init (global-rainbow-delimiters-mode))


;; ===========================================
;; Setup multiple-cursors
;; ===========================================
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


;; ===========================================
;; Setup 
;; ===========================================


(provide 'init)
