;;----------------------------------------------------------------------------
;;;; Init
;;----------------------------------------------------------------------------
(defun is-osx ()
  (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;;;; Defaults
;;----------------------------------------------------------------------------

;; Turn off mouse interface early in startup to avoid momentary display
(setq inhibit-startup-message t)
(menu-bar-mode +1)
(if (not (is-osx)) (menu-bar-mode +1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(if (file-exists-p "~/.cask/cask.el")
    (require 'cask "~/.cask/cask.el")
  (require 'cask "/usr/local/share/emacs/site-lisp/cask.el"))
(cask-initialize)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (expand-file-name file user-emacs-directory)))

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(let ((workon-home (expand-file-name "~/.pyenv/versions")))
  (setenv "WORKON_HOME" workon-home)
  (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home))


;;----------------------------------------------------------------------------
;;;; Loading
;;----------------------------------------------------------------------------


(load-local "appearance")
(load-local "defaults")
(load-local "defuns")
(load-local "vendor/hippie")
(load-local "vendor/tdd")


;;----------------------------------------------------------------------------
;;;; Packages
;;----------------------------------------------------------------------------

;; Packages setup
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(provide 'occur)
(provide 'osx)
(provide 'personal)
(provide 'text)


(use-package pallet
  :init (pallet-mode t))


(use-package tdd
  :bind (("C-<f5>" . tdd-mode)
         ("C-%" . tdd-mode))
  :commands tdd-mode
  :init (setq tdd-success-symbol " ⬤ "
              tdd-failure-symbol " ⬛ "))


(use-package ace-jump-mode
  :if (not noninteractive)
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode-pop-mark))
  :config
  (progn
    (setq ace-jump-mode-case-fold t)
    (ace-jump-mode-enable-mark-sync)
    (setq ace-jump-mode-submode-list
          '(ace-jump-word-mode ace-jump-char-mode ace-jump-line-mode))))


(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))


(use-package auto-complete
  :disabled t
  :diminish auto-complete-mode
  :config
  (global-auto-complete-mode t))


(use-package ace-jump-buffer
  :bind ("C-c b" . ace-jump-buffer)
  :commands ace-jump-buffer)


(use-package buffer-menu
  :bind ("C-x C-b" . buffer-menu)
  :commands buffer-menu)


(use-package coffee-mode
  :disabled t
  :config
  (progn
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))


(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))


(use-package cc-mode
  :disabled t
  :mode ("\\.h\\'" . c++-mode)
  :config
  (progn
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)))


(use-package compile
  :bind ("<f5>" . recompile)
  :init
  (defun disable-truncate-on-compilation ()
    (progn
      (setq truncate-lines nil)
      (set (make-local-variable 'truncate-partial-width-windows) nil)))
  ;; (add-hook 'compilation-mode-hook 'disable-truncate-on-compilation)
  )


(use-package css-mode
  :if (not noninteractive)
  :config (setq css-indent-offset 2))


(use-package drag-stuff
  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down))
  :diminish drag-stuff-mode
  :init
  (drag-stuff-global-mode t))


(use-package dired
  :config
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))
  (bind-key  "C-a" 'dired-back-to-start-of-files dired-mode-map)
  (bind-key  "k" 'dired-do-delete dired-mode-map))


(use-package hippie
  :bind (("C-." . hippie-expand-no-case-fold)
         ("C-:" . hippie-expand-lines)))


(use-package emacs-lisp-mode
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode)
  :config (progn
            (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el")))))


(use-package emmet-mode
  :diminish emmet-mode
  :config
  (progn
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))


(use-package eshell
  :bind ("C-c C-e" . eshell))


(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-M-SPC" . er/expand-region)
         ("C-+" . er/contract-region)))


(use-package exec-path-from-shell
  :if (is-osx)
  :defer t
  :init (exec-path-from-shell-initialize))


(use-package ido
  :commands ido-mode
  :init
  (ido-mode t)
  :config
  (ido-vertical-mode t)
  (ido-ubiquitous t)
  (flx-ido-mode t)
  (setq ido-enable-flex-matching t) ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  ;; Customize match list
  (setq ido-file-extensions-order '(".py" ".rb" ".el" ".js"))
  (add-to-list 'ido-ignore-files '(".DS_Store" ".pyc"))
  (add-to-list 'ido-ignore-directories '("__pycache__")))


(use-package files
  :bind (("C-c R" . rename-this-buffer-and-file)
         ("C-c D" . delete-this-buffer-and-file))
  :config
  (progn
    (setq auto-save-default nil)
    (global-auto-revert-mode 1)
    (setq make-backup-files nil) ; stop creating those backup~ files
    (setq auto-save-default nil) ; stop creating those #autosave# files
    (add-hook 'after-save-hook 'whitespace-cleanup)
    (add-hook 'before-save-hook 'delete-trailing-whitespace)))


(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (setq flycheck-display-errors-function nil)))


(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-c v =" . git-gutter:popup-hunk) ;; show hunk diff
         ("C-c v p" . git-gutter:previous-hunk)
         ("C-c v n" . git-gutter:next-hunk)
         ("C-c v s" . git-gutter:stage-hunk)
         ("C-c v r" . git-gutter:revert-hunk))
  :init (global-git-gutter-mode t))


(use-package gitignore-mode)


(use-package haml-mode
  :disabled t)


(use-package imenu
  :bind (("M-i" . imenu-anywhere)))


(use-package ispell
  :disabled t
  :defer t
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  :config
  (progn
    (bind-key "<f6>" 'mw/spell-dictionary)
    (bind-key "C-?" 'ispell-word)
    (bind-key "C-c i e" (λ (ispell-change-dictionary "en_GB") (flyspell-buffer)))
    (bind-key "C-c i f" (λ (ispell-change-dictionary "fr_FR") (flyspell-buffer)))
    (bind-key "C-c i p" (λ (ispell-change-dictionary "pt_BR") (flyspell-buffer)))))


(use-package flyspell
  :disabled t
  :requires ispell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode t))


(use-package js
  :requires js2-mode
  :mode ("\\.json$" . js-mode)
  :config
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))
    (use-package js2-mode
      :mode (("\\.js$" . js2-mode)
             ("Jakefile$" . js2-mode))
      :interpreter "node"
      :config
      (progn
        (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
        (setq-default js2-basic-offset 2)
        (setq-default js2-auto-indent-p t)
        (setq-default js2-cleanup-whitespace t)
        (setq-default js2-enter-indents-newline t)
        (setq-default js2-global-externs "jQuery $")
        (setq-default js2-indent-on-enter-key t)
        (setq-default js2-show-parse-errors nil)
        (setq-default js2-mode-indent-ignore-first-tab t)))))


(use-package magit
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


(use-package menu-bar
  :bind ("M-k" . kill-this-buffer))


(use-package make-mode
  :defer t)


(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook (lambda() (setq mode-name "md")))))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-w" . mc/mark-all-words-like-this)))


(use-package nyan-mode
  :disabled t
  :init (nyan-mode 1))


(use-package occur
  :bind ("C-c C-o" . occur)
  :config
  (bind-key "n" 'occur-next occur-mode-map)
  (bind-key "p" 'occur-prev occur-mode-map))


(use-package org
  :disabled t
  :config
  (progn
    (eval-after-load "org-toc-autoloads"
      '(progn
         (if (require 'org-toc nil t)
             (add-hook 'org-mode-hook 'org-toc-enable)
           (warn "org-toc not found"))))
    (setq org-src-fontify-natively t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (ruby . t)
       (lisp . t)
       (R . t)))))


(use-package projectile
  :init
  (projectile-global-mode t)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-use-git-grep t)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-require-project-root nil)
    ;; (setq projectile-completion-system 'grizzl)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store" "*.pyc")))


(use-package puppet-mode
  :disabled t)


(use-package python
  :bind (("<f9>" . mw/add-py-debug)
         ("C-<f9>" . mw/add-pudb-debug)
         ("M-<f9>" . mw/add-ipdb-debug)
         )
  :init
  (progn
    (add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))
    (add-hook 'python-mode-hook
              '(lambda ()
                 (flycheck-mode)
                 (flycheck-select-checker 'python-flake8))))
  :config
  (progn
    (use-package eldoc-mode
      :commands (eldoc-mode)
      :init (add-hook 'python-mode-hook 'eldoc-mode))

    (use-package anaconda-mode
      :init
      (add-hook 'python-mode-hook '(lambda () (anaconda-mode)))
      (add-to-list 'company-backends 'company-anaconda))

    (use-package elpy
      :bind (("C-c t" . elpy-test-django-runner)
             ("C-c C-f" . elpy-find-file)
             ("C-c C-;" . mw/set-django-settings-module))
      :init
      (elpy-enable)
      (defalias 'workon 'pyvenv-workon)
      (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
      (add-hook 'pyvenv-post-activate-hooks 'elpy-rpc-restart)

      ;; (defun mw/set-elpy ()
      ;;   (setq elpy-test-django-runner-command '("./manage.py" "test" "--noinput"))
      ;;   (setq compilation-environment '("PATH=/Users/username/.pyenv/versions/myapp/bin/")))
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
      (setq elpy-rpc-backend "rope")
      (elpy-use-ipython))

    (use-package pip-requirements
      :mode "\\requirements.txt\\'"
      :config (pip-requirements-mode))

    (use-package pyvenv
      :init
      (add-hook 'python-mode-hook 'pyvenv-mode))

    (use-package virtualenvwrapper
      :init
      (venv-initialize-interactive-shells) ;; interactive shell support
      (venv-initialize-eshell) ;; eshell support
      (setq venv-location "~/.pyenv/versions"))
    ))


(use-package rainbow-mode
  :disabled t
  :diminish rainbow-mode
  :config
  (add-hook 'sass-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode))


(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package recentf
  :commands recentf-mode
  :bind (("C-c f" . recentf-ido-find-file))
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 100))


(use-package fold-this
  :commands fold-this)


(use-package region-bindings-mode
  :if (not noninteractive)
  :config
  (progn
    ;; multiple-cursors
    (bind-key "a" 'mc/mark-all-like-this region-bindings-mode-map)
    (bind-key "e" 'mc/edit-lines region-bindings-mode-map)
    (bind-key "p" 'mc/mark-previous-like-this region-bindings-mode-map)
    (bind-key "P" 'mc/unmark-previous-like-this region-bindings-mode-map)
    (bind-key "n" 'mc/mark-next-like-this region-bindings-mode-map)
    (bind-key "N" 'mc/unmark-next-like-this region-bindings-mode-map)

    ;; expand-regions
    (bind-key "f" 'er/mark-defun region-bindings-mode-map)
    (bind-key "u" 'er/mark-url region-bindings-mode-map)
    (bind-key "c" 'er/mark-python-block region-bindings-mode-map)

    ;; fold-this
    (bind-key "." 'fold-this region-bindings-mode-map)

    (setq region-bindings-mode-disabled-modes '(term-mode))
    (setq region-bindings-mode-disable-predicates
          (list (lambda () buffer-read-only)))
    (region-bindings-mode-enable)))


(use-package re-builder
  :disabled t
  :init (setq reb-re-syntax 'string))


(use-package rst
  :defer t
  :init
  (add-hook 'rst-mode-hook
            (lambda()
              (flyspell-mode 1))))


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


(use-package s
  :init
  (bind-key "C-c m -" (λ (replace-region-by 's-dashed-words)))
  (bind-key "C-c m _" (λ (replace-region-by 's-snake-case)))
  (bind-key "C-c m c" (λ (replace-region-by 's-lower-camel-case)))
  (bind-key "C-c m C" (λ (replace-region-by 's-upper-camel-case))))


(use-package sass-mode
  :disabled t
  :defer t)


(use-package saveplace
  :config (setq-default save-place t))

(use-package savehist
  :defer t
  :commands savehist-mode
  :init
  (progn
    (savehist-mode 1)
    (setq savehist-file "~/.emacs.d/savehist")
    (setq history-length 1000)))

(use-package scala-mode
  :disabled t
  :config (modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8)
  :mode "\\.\\(scala\\|sbt\\)\\'")


(use-package scss-mode
  :disabled t
  :mode "\\.scss\\'")


(use-package gradle-mode
  :disabled t
  :mode "\\.gradle\\'")


(use-package shell
  :config
  ;; handle shell colours
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))


(use-package slime
  :disabled t
  :mode (("\\.lisp$" . lisp-mode)
         ("\\.clisp$" . lisp-mode))
  :config
  (progn
    (setq inferior-lisp-program (executable-find "clisp"))
    (setq slime-contribs '(slime-fancy))
    (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
    (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))))


(use-package nyan-mode
  :disabled t
  :config (nyan-mode t))


(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package sublimity
  :disabled t
  :init (sublimity-mode 1))


(use-package ace-window
  :disabled t
  :bind (("M-o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package smartparens
  :bind (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
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
         ("C-M-t" . sp-transpose-sexp))
  :init
  (progn
    (smartparens-global-mode 1)
    (smartparens-strict-mode 1)
    (show-smartparens-global-mode t)
    (setq smartparens-global-strict-mode t)
    (setq
     sp-highlight-wrap-overlay nil
     sp-autoescape-string-quote nil
     sp-autoskip-closing-pair 'always
     blink-matching-paren t))
  :config
  (progn
    (set-face-attribute 'sp-show-pair-match-face nil :background nil :underline "Purple")
    (require 'smartparens-config)
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))
    (setq sp-autoinsert-if-followed-by-word nil)))

(use-package smex
  :bind (("M-x" . smex)
         ("C-x C-m" . smex))
  :init (smex-initialize))


(use-package subword
  :diminish subword-mode
  :init
  (progn
    (global-subword-mode 1)
    (defadvice subword-upcase (before upcase-word-advice activate)
      (unless (looking-back "\\b")
        (backward-word)))

    (defadvice subword-downcase (before downcase-word-advice activate)
      (unless (looking-back "\\b")
        (backward-word)))

    (defadvice subword-capitalize (before capitalize-word-advice activate)
      (unless (looking-back "\\b")
        (backward-word)))))


(use-package text
  :bind (("C-x j" . eval-and-replace))
  :init (add-hook 'text-mode-hook (lambda() (visual-line-mode 1))))


(use-package visual-regexp
  :disabled t
  ;; :config
  ;; (use-package visual-regexp-steroids)
  :bind (("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)
         ("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))


(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
          ("\\.html\\.erb\\'" . web-mode)
          ("\\.html\\.ejs\\'" . web-mode)
          ("\\.ejs\\'" . web-mode)
          ("\\.mustache\\'" . web-mode)
          ("\\.jinja\\'" . web-mode))
  :config
  (progn
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-pairing t)
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
  ;; Undo/redo window configuration with C-c <left>/<right>
  :init (winner-mode 1))


(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.pass\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :bind ("C-m" . newline-and-indent))


(use-package osx
  :if (is-osx)
  :init
  (progn
    ;; ;; installed by: `brew install aspell --all`
    (setq ispell-program-name "/usr/local/bin/aspell"
          ispell-extra-args '("--sug-mode=ultra"))

    ;; Switch the Cmd and Meta keys
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta)
    (setq ns-function-modifier 'hyper)

    ;; Make the browser the OS X default
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)

    ;; Move to trash when deleting stuff
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs")
    ))


(use-package personal
  :config
  (progn
    (bind-key "C-x C-c" (λ (if (y-or-n-p "Quit Emacs? ") (save-buffers-kill-emacs))))
    (bind-key "<f6>" 'linum-mode)
    (bind-key "<f8>" (λ (find-file (f-expand "init.el" user-emacs-directory))))
    (bind-key "C-<f8>" (λ (find-file (f-expand "appearance.el" user-emacs-directory))))
    (bind-key "<f7>" 'ansi-term)

    (bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
    (bind-key "C-j" 'newline-and-indent)
    (bind-key "C-z" 'zap-up-to-char)
    (bind-key "C-|" 'align-regexp)

    (bind-key "C-M-;" 'comment-or-uncomment-current-line-or-region)

    (bind-key "C-c d" 'duplicate-current-line-or-region)
    (bind-key "C-c g" 'google)
    (bind-key "C-c n" 'clean-up-buffer-or-region)
    (bind-key "C-c y" 'youtube)

    (bind-key "M-h" 'kill-to-beginning-of-line)
    (bind-key "M-g M-g" 'goto-line-with-feedback)
    (bind-key "M-j" (λ (join-line -1)))
    (bind-key "M-<up>" 'open-line-above)
    (bind-key "M-<down>" 'open-line-below)
    ))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (f-exists? "user.el")
  (load-local "user"))

(when (f-exists? "custom.el")
  (load-local "custom"))

(provide 'init)
