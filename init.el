;;============================================================
;; Package initialize
;;============================================================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; use-package
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; (setq use-package-verbose t)


;;============================================================
;; Defaults
;;============================================================

(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(tooltip-mode -1)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Coding system
(define-coding-system-alias 'UTF-8 'utf-8)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Don't be so stingy on the memory, we have lots now.
(setq gc-cons-threshold 200000000)

;; No tabs please
(setq tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; Backup files
(setq make-backup-files nil)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

(setq echo-keystrokes 0.1)
(setq delete-by-moving-to-trash t)
(set-default 'sentence-end-double-space nil)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

(auto-compression-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load custom-file))

(defalias 'which 'executable-find)

(setq *spell-program* (which "aspell"))

;; Auto save buffers
(setq auto-save-mode  nil)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;============================================================
;; System
;;============================================================
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (setq ns-use-srgb-colorspace nil)
  )


;;============================================================
;; Bootstrap
;;============================================================
(defun load-local (filename)
  (load (expand-file-name filename user-emacs-directory)))


;;============================================================
;; Appearance
;;============================================================
(global-hl-line-mode -1)
;; Linum mode
(global-linum-mode -1)
(setq linum-format "%4d ")
(add-hook 'prog-mode-hook 'linum-mode)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)  ;; disable annoying blink-matching-paren

;; Scrolling
;; =========================
(setq scroll-error-top-bottom t
      visible-bell nil
      scroll-conservatively 10000
      scroll-margin 10
      auto-window-vscroll nil)

;; Themes
;; ==================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Dark Themes
;; ==================================================
;; (use-package material-theme :ensure t :init (load-theme 'material :no-confirm))
(use-package monokai-theme :ensure t :init (load-theme 'monokai :no-confirm))
;; (use-package spacemacs-theme :ensure t :init (load-theme 'spacemacs-dark :no-confirm))
;; (use-package solarized-theme :ensure t :init (load-theme 'solarized-dark :no-confirm))
;; (use-package molokai-theme :ensure t :init (load-theme 'molokai :no-confirm))
;; (use-package atom-dark-theme :ensure t :init (load-theme 'atom-dark :no-confirm))
;; (use-package darkmine-theme :ensure t :init (load-theme 'darkmine :no-confirm))
;; (use-package cyberpunk-theme :ensure t :init (load-theme 'cyberpunk :no-confirm))
;; (use-package ample-theme :ensure t :init (load-theme 'ample :no-confirm))
;; (use-package badger-theme :ensure t :init (load-theme 'badger :no-confirm))
;; (use-package darktooth-theme :ensure t :init (load-theme 'darktooth :no-confirm))
;; (use-package gruvbox-theme :ensure t :init (load-theme 'gruvbox :no-confirm))
;; (use-package zenburn-theme :ensure t :init (load-theme 'zenburn :no-confirm))
;; (use-package tangotango-theme :ensure t :init (load-theme 'tangotango :no-confirm))

;; Light Themes
;; ==================================================
;; (use-package spacemacs-theme :ensure t :init (load-theme 'spacemacs-light :no-confirm))
;; (use-package leuven-theme :ensure t :init (load-theme 'leuven :no-confirm))
;; (use-package solarized-theme :ensure t :init (load-theme 'solarized-light :no-confirm))
;; (use-package hemisu-theme :ensure t :init (load-theme 'hemisu-ligth :no-confirm))
;; (use-package tango-plus-theme :ensure t :init (load-theme 'tango-plus :no-confirm))
;; (use-package color-theme-sanityinc-tomorrow :ensure t :init (load-theme 'sanityinc-tomorrow-day :noconfirm))
;; (use-package ample-theme :ensure t :init (load-theme 'ample-light :no-confirm))

;; Fonts
;; =========================
(defun mw/set-best-font (fonts)
  (when fonts
    (let* ((fontname (car (car fonts)))
           (fontsize (car (last (car fonts))))
           (fontstring (format "%s-%d" fontname fontsize)))
      (if (member fontname (font-family-list)) (set-frame-font fontstring)
        (mw/set-best-font (cdr fonts))))))


(mw/set-best-font '(
                    ("Menlo" 15)
                    ("Inconsolata" 16)
                    ("DejaVu Sans Mono" 15)
                    ("Ubuntu Mono" 16)
                    ("Monaco" 15)
                    ("Roboto Mono" 14)
                    ("Droid Sans Mono Dotted" 14)
                    ("Source Code Pro" 14)
                    ("Anonymous Pro" 14)
                    ("Droid Sans Mono" 14)
                    ("Liberation Mono" 14)
                    ("Code New Roman" 14)
                    ("Fantasque Sans Mono" 18)
                    ))

;;============================================================
;; Loading
;;============================================================
(load-local "defuns")

;;; Major mode abbrevs
;; (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Ⓔ")))
;; (add-hook 'python-mode-hook (lambda() (setq mode-name "Ⓟ")))
;; (add-hook 'js2-mode-hook (lambda() (setq mode-name "Ⓙ")))
;; (add-hook 'web-mode-hook (lambda() (setq mode-name "Ⓦ")))

;;; Minor Mode diminish
(eval-after-load "xterm-title" '(diminish 'xterm-title-mode))
(eval-after-load "hi-lock" '(diminish 'hi-lock-mode))
(eval-after-load "outline" '(diminish 'outline-minor-mode))

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-mode))

(add-to-list 'auto-mode-alist '(".zshrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".zshenv$" . shell-script-mode))

;;; global hook modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 (executable-find "terminal-notifier")
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))

;;============================================================
;; Packages
;;============================================================

(use-package s
  :ensure t)

(use-package bind-key
  :ensure s
  :config
  ;; Editing
  (bind-key "C-c d" 'duplicate-current-line-or-region)
  (bind-key "C-j" 'newline-and-indent)
  (bind-key "C-c n" 'clean-up-buffer-or-region)
  (bind-key "C-M-;" 'comment-or-uncomment-current-line-or-region)
  (bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
  (bind-key "C-z" 'zap-up-to-char)
  (bind-key "C-|" 'align-regexp)
  (bind-key "C-x c" 'copy-file-name-to-clipboard)

  (bind-key "M-j" (λ (join-line -1)))
  (bind-key "M-h" 'kill-to-beginning-of-line)
  (bind-key "M-g M-g" 'goto-line-with-feedback)
  (bind-key "M-<up>" 'open-line-above)
  (bind-key "M-<down>" 'open-line-below)

  ;; Buffer/Files
  (bind-key "C-c R" 'rename-this-buffer-and-file)
  (bind-key "C-c D" 'delete-this-buffer-and-file)
  (bind-key "<f8>" (λ (find-file (expand-file-name "init.el" user-emacs-directory))))
  (bind-key "<f5>" 'recompile)
  (bind-key "C-x C-c" (λ (if (y-or-n-p "Quit Emacs? ") (save-buffers-kill-emacs))))

  ;; Search
  (bind-key "C-c g" 'google)
  (bind-key "C-c y" 'youtube)

  ;; Naming
  (bind-key "C-c m -" (lambda () (interactive) (replace-region-by 's-dashed-words)))
  (bind-key "C-c m _" (lambda () (interactive) (replace-region-by 's-snake-case)))
  (bind-key "C-c m c" (lambda () (interactive) (replace-region-by 's-lower-camel-case)))
  (bind-key "C-c m C" (lambda () (interactive) (replace-region-by 's-upper-camel-case)))

  ;; Number
  (bind-key "C-c . +" 'my-increment-number-at-point)
  (bind-key "C-c . -" 'my-decrement-number-at-point)
  )

(use-package diminish)

;;#############################
;; Interface
;;#############################

(use-package uniquify
  :config
  (setf uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode)
  :config
  (setq-default auto-revert-interval 3))


;;#############################
;; System
;;#############################
;; automagically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(add-hook 'auto-revert-tail-mode-hook 'etc-log-tail-handler)

(use-package ispell
  :bind (("<f6>" . ispell-change-dictionary)
         ("C-<f6>" . ispell-buffer))
  :if (which "aspell")
  :init
  (setq ispell-program-name (which "aspell"))
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-dictionary "fr_FR")
  )

(use-package paradox
  :ensure t
  :commands (paradox-list-packages))

(use-package super-save
  :ensure t
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5
        super-save-triggers (append super-save-triggers '("magit-status")))
  (super-save-mode +1)
  )

(use-package conf-mode
  :mode ".*\\.coveragerc")


;;#############################
;; Shell
;;#############################
(use-package shell
  :mode (("\\.bash" . shell-script-mode)
         ("\\.zsh" . shell-script-mode)
         ("\\.fish" . shell-script-mode))
  )

(use-package exec-path-from-shell
  :ensure t
  :defer 5
  :if *is-a-mac*
  :config
  (exec-path-from-shell-initialize)
  )


;;#############################
;; Git
;;#############################
(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . magit-status)
         ("C-c v b" . magit-blame))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-display-buffer-function
        (lambda (buffer)
          (if magit-display-buffer-noselect
              ;; the code that called `magit-display-buffer-function'
              ;; expects the original window to stay alive, we can't go
              ;; fullscreen
              (magit-display-buffer-traditional buffer)
            (delete-other-windows)
            ;; make sure the window isn't dedicated, otherwise
            ;; `set-window-buffer' throws an error
            (set-window-dedicated-p nil nil)
            (set-window-buffer nil buffer)
            ;; return buffer's window
            (get-buffer-window buffer))))
  (setq magit-revert-buffers t)
  (add-hook 'magit-status-mode-hook 'delete-other-windows)
  (bind-key "q" 'magit-quit-session magit-status-mode-map))

(use-package git-gutter
  :disabled t
  :ensure t
  :bind (("C-c v =" . git-gutter:popup-hunk) ;; show hunk diff
         ("C-c v p" . git-gutter:previous-hunk)
         ("C-c v n" . git-gutter:next-hunk)
         ("C-c v s" . git-gutter:stage-hunk)
         ("C-c v r" . git-gutter:revert-hunk))
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode t)
  )

(use-package diff-hl
  :ensure t
  :demand t
  :bind (("C-c v p" . diff-hl-previous-hunk)
         ("C-c v n" . diff-hl-next-hunk)
         ("C-c v r" . diff-hl-revert-hunk))
  :config
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-mode +1)
  )

(use-package git-timemachine
  :ensure t
  :bind ("C-c v t" . git-timemachine))

(use-package gitconfig-mode
  :ensure t
  :mode ".*\\.gitconfig")


;;#############################
;; Navigation
;;#############################
(use-package saveplace
  :ensure t
  :init
  (if (fboundp #'save-place-mode)
      (save-place-mode +1)
    (setq-default save-place t)))

(use-package dired-x
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files
    (concat dired-omit-files "\\|\\.pdf$\\|^__pycache__$\\|^\\.git$\\|^\\.pyc$\\|^\\.DS_Store$"))
  )

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :bind (("M-]" . highlight-symbol-next)
         ("M-[" . highlight-symbol-prev))
  :init
  (add-hook 'prog-mode-hook (λ (highlight-symbol-mode t))))

(use-package bm
  :ensure t
  :bind (
         ("C-c b m" . bm-toggle)
         ("C-c b [" . bm-previous)
         ("C-c b ]" . bm-next)
         ("C-c b l" . bm-show-all)
         )
  :config
  (bind-key "n" 'bm-show-next bm-show-mode-map)
  (bind-key "p" 'bm-show-prev bm-show-mode-map))

(use-package tdd
  :load-path "vendor"
  :bind ("C-<f5>" . tdd-mode)
  :config
  (require 'tdd))

(use-package compile
  :config
  (defadvice compilation-start (before mw-pytest-compilation-start-before (command &optional mode name-function highlight-regexp) activate)
      (message "Set compile command: %s" command)
      (setq compile-command command)
      )
  )


(use-package projectile
  :ensure t
  :init
  (projectile-global-mode t)
  :config
  (setq projectile-enable-caching t
        projectile-use-git-grep t
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root nil
        projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
        projectile-completion-system 'ivy
        )
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'projectile-globally-ignored-files "*.pyc")
  (add-to-list 'projectile-globally-ignored-files "*.python-version")
  (add-to-list 'projectile-globally-ignored-files "*.egg-info")
  (add-to-list 'projectile-globally-ignored-directories "__pycache__")
  (add-to-list 'projectile-globally-ignored-directories ".env")
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (add-to-list 'projectile-globally-ignored-directories ".cask")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  )

(use-package counsel
  :ensure t
  :demand t
  :bind* (("M-i" . counsel-imenu)
          ("C-c s a" . counsel-ag)
          ("C-c s g" . counsel-git-grep)
          ("C-c s k" . counsel-descbinds)
          ("C-x C-f" . counsel-find-file)
          )
  :config
  (use-package swiper
    :ensure t
    :bind (("C-s" . swiper)
           ("C-r" . swiper))
    )

  (use-package ivy
    :bind (("C-c f" . ivy-recentf))
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
    (setq ivy-display-style 'fancy)
    ;; (setq ivy-height 10)
    (ivy-mode +1))

  (use-package imenu
    :init
    (defun imenu-mark-use-package ()
      (add-to-list 'imenu-generic-expression
                   '("use-package"
                     "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
    (add-hook 'emacs-lisp-mode-hook #'imenu-mark-use-package))

  ;; Keybindings
  (global-set-key [remap execute-extended-command] #'counsel-M-x)
  (global-set-key [remap describe-function] #'counsel-describe-function)
  (global-set-key [remap describe-variable] #'counsel-describe-variable)
  (global-set-key [remap imenu] #'counsel-imenu)
  (global-set-key [remap find-file] #'counsel-find-file)
  )

(use-package recentf
  :ensure t
  :config
  (recentf-mode +1)
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-saved-items 100))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode t)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-extra-commands 'swtich-window)
  )

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode-pop-mark))
  :config
  (setq ace-jump-mode-case-fold t)
  )

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?q ?w ?e ?r ?a ?s ?d ?f))
  )


;;#############################
;; Completion
;;#############################
(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc
  :init
  (add-hook 'emacs-lisp-mode 'eldoc-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company
  :ensure t
  :commands global-company-mode
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'global-company-mode)

  :config
  (setq company-tooltip-limit 20) ;; bigger popup window
  (setq company-idle-delay 0.5)   ;; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)     ;; remove annoying blinking
  (setq company-show-numbers t)   ;; show numbers for easy selection

  (bind-key "C-n" #'company-select-next company-active-map)
  (bind-key "C-p" #'company-select-previous company-active-map)
  (bind-key "<tab>" #'company-complete company-active-map)
  (bind-key "M-?" #'company-show-doc-buffer company-active-map)
  (bind-key "M-." #'company-show-location company-active-map)
  )

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package hippie
  :load-path "vendor"
  :bind (("C-." . hippie-expand-no-case-fold)
         ("C-," . hippie-expand-lines))
  :config
  (require 'hippie))


;;#############################
;; Editing
;;#############################
(use-package drag-stuff
  :ensure t
  :bind* (("M-p" . drag-stuff-up)
          ("M-n" . drag-stuff-down)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-w" . mc/mark-all-words-like-this)))

(use-package expand-region
  :ensure t
  :demand t
  :bind (("C-M-SPC" . er/expand-region)
         ("C-+" . er/contract-region))
  )

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
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
  :commands (smartparens-mode show-smartparens-mode)
  :init
  (smartparens-global-mode 1)
  (smartparens-strict-mode 1)
  (show-smartparens-global-mode t)
  (setq smartparens-global-strict-mode t)
  (require 'smartparens-config)
  )

(use-package region-bindings-mode
  :ensure t
  :commands (region-bindings-mode-enable)
  :init
  (region-bindings-mode-enable)
  :config
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
  (bind-key "b" 'er/mark-python-block region-bindings-mode-map)
  (bind-key "m" 'er/mark-method-call region-bindings-mode-map)
  (bind-key "-" 'er/contract-region region-bindings-mode-map)
  (bind-key "+" 'er/expand-region region-bindings-mode-map)
  (bind-key "=" 'er/expand-region region-bindings-mode-map)
  (bind-key "SPC" 'er/expand-region region-bindings-mode-map)

  (setq region-bindings-mode-disabled-modes '(term-mode))
  (setq region-bindings-mode-disable-predicates
        (list (lambda () buffer-read-only)))

  ;; ispell
  (bind-key "s" 'ispell-region region-bindings-mode-map)
  )

(use-package subword
  :diminish subword-mode
  :init
  (global-subword-mode 1)
  :config
  (defadvice subword-upcase (before upcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word)))

  (defadvice subword-downcase (before downcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word)))

  (defadvice subword-capitalize (before capitalize-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word))))


;;#############################
;; Modeline
;;#############################
(use-package spaceline-config
  :ensure spaceline
  :config
  ;; Toggles
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-flycheck-info-off)
  (spaceline-toggle-flycheck-error-off)
  (spaceline-toggle-flycheck-warning-off)
  (spaceline-toggle-nyan-cat-on)
  (spaceline-toggle-evil-state-on)
  (spaceline-toggle-buffer-size-off)

  ;; Appearance
  ;; (spaceline-emacs-theme)
  (spaceline-spacemacs-theme)
  (setq powerline-height '20)
  (setq powerline-default-separator 'box)
  ;; (setq powerline-default-separator 'arrow-fade)
  ;; (setq powerline-default-separator 'bar)
  ;; (setq powerline-default-separator 'slant)
  ;; (setq powerline-default-separator 'wave)
  ;; (setq powerline-default-separator 'utf-8)
  ;; (setq powerline-default-separator 'curve)
  ;; (setq powerline-default-separator 'chamfer)
  ;; (setq powerline-default-separator 'roundstub)

  (use-package nyan-mode
    :ensure t
    :config
    (nyan-mode t)
    )

  (use-package powerline-evil
    :ensure t
    )
  )


;;#############################
;; Buffers/Files
;;#############################
(use-package files
  :bind (("C-c R" . rename-this-buffer-and-file)
         ("C-c D" . delete-this-buffer-and-file)))

(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer))


;;#############################
;; Major Modes
;;#############################
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.html\\.ejs\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-engines-alist
        '(("django" . "\\.djhtml")
          ;;("django" . mw/buffer-django-p) ;; set engine to django on django buffer
          ("django" . "templates/.*\\.html")))
  (add-hook 'web-mode-hook (lambda () (emmet-mode)))
  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-style-padding 2)
              (setq web-mode-script-padding 2)
              (setq web-mode-markup-indent-offset 2)
              (define-key web-mode-map [(return)] 'newline-and-indent))))

(use-package jinja2-mode
  :ensure t
  :mode (("app/views/.*\\.html" . jinja2-mode)
         (".*\\.jinja" . jinja2-mode)
         (".*\\.jinja2" . jinja2-mode)))

(use-package jade-mode
  :ensure t
  :mode ".*\\.jade")

(use-package js2-mode
  :ensure t
  :mode ".*\\.js"
  :interpreter "node"
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map))

(use-package json-mode
  :ensure t
  :mode ".*\\.json")

(use-package evil
  :ensure t
  :bind (("<f6>" . evil-mode)))

(use-package python
  :init
  (require 'python)
  :bind (:map python-mode-map
              ("<f9>" . mw/python--add-pdb-breakpoint)
              ("C-<f9>" . mw/python--add-pudb-breakpoint)
              ("M-<f9>" . mw/python--add-ipdb-breakpoint)
              ("C-M-<f9>" . mw/python--remove-breakpoints)
         )
  :config
  ;; Defaults
  (setq-default python-indent 4)
  (setq python-fill-docstring-style 'onetwo)

  ;; Hooks
  (add-hook 'python-mode-hook 'mw/python--add-todo-fixme-bug-hightlight)
  (add-hook 'python-mode-hook 'mw/python--add-debug-highlight)

  (use-package auto-virtualenv
    :ensure t
    :config
    (setq auto-virtualenv-dir "~/.virtualenvs")
    (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
    (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
    (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv))

  (use-package anaconda-mode
    :ensure t
    :demand t
    :diminish anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (setq company-tooltip-align-annotations t
          company-dabbrev-downcase nil
          company-dabbrev-code-everywhere t)

    (use-package company-anaconda
      :ensure t
      :init
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-anaconda))))

  (use-package pip-requirements
    :ensure t
    :mode "\\requirements.txt\\'"
    :config (pip-requirements-mode))

  (use-package elpy
    :disabled t
    :ensure t
    :diminish elpy-mode
    :config
    (setq elpy-rpc-backend "jedi")
    (defun elpy-setup ()
      (interactive)
      (progn
        (compile (format "%s install -U jedi flake8 importmagic autopep8 yapf" (executable-find "pip")))))
    )

  (use-package py-autopep8
    :ensure t
    :commands (py-autopep8-buffer))

  (use-package pytest
    :disabled t
    :ensure t
    :demand t
    :bind* (:map python-mode-map
                 ("C-c t p" . pytest-one)
                 ("C-c t P" . copy-pytest-test-to-clipboard)
                 ("C-c t a" . pytest-all)
                 ("C-c t m" . pytest-module)
                 ("C-c t d" . pytest-directory))
    :config
    (defun copy-pytest-test-to-clipboard ()
      (interactive)
      (let ((testname (pytest-py-testable)))
        (when testname
          (kill-new (format "py.test -x -s %s" testname))
          (message "Copied '%s' to the clipboard." (testname)))))
      )

  (use-package py-test
    :ensure t
    :demand t
    :bind* (:map python-mode-map
                 ("C-c t p" . py-test-run-test-at-point)
                 ("C-c t P" . copy-pytest-test-to-clipboard)
                 ("C-c t d" . py-test-run-directory)
                 ("C-c t m" . py-test-run-file)
                 )
    :config
    (defun copy-pytest-test-to-clipboard ()
      (interactive)
      (let ((testname (pytest-py-testable)))
        (when testname
          (kill-new (format "py.test -x -s %s" testname))
          (message "Copied '%s' to the clipboard." (testname)))))
      )

  (use-package nose
    :ensure t
    :demand t
    :bind* (:map python-mode-map
                 ("C-c t n" . nosetests-one)
                 ("C-c t N" . copy-nosetest-test-to-clipboard))
    :config
    (defun copy-nosetest-test-to-clipboard ()
      (interactive)
      (let ((testname (format "%s:%s" buffer-file-name (nose-py-testable))))
        (when testname
          (kill-new (format "nosetests -x -s %s" testname))
          (message "Copied '%s' to the clipboard." testname)))))
  )

(use-package yaml-mode
  :ensure t
  :mode ((".*\\.pass" . yaml-mode)
         ("\\.passpierc" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1)))
  )

(use-package coffee-mode
  :ensure t
  :config
  (setq coffee-tab-width 2))

(use-package markdown-mode
  :ensure t
  :mode ((".*\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode)))

(use-package gist
  :ensure t
  :demand t)

(use-package pig-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode))

(use-package less-css-mode
  :ensure t)

(use-package ruby-mode
  :mode (
         ("\\.rake$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)
         ("Fastfile$" . ruby-mode)
         ("Gymfile$" . ruby-mode)
         ("Matchfile$" . ruby-mode)
         ("Appfile$" . ruby-mode)))

(use-package irony
  :ensure t
  :defer t
  :mode (("\\.h$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ("\\.m$" . objc-mode)
         )
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package company-irony
    :ensure t
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))))

(use-package php-mode
  :ensure t)

(use-package puppet-mode
  :ensure t)
