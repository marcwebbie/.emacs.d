;;============================================================
;; Defaults
;;============================================================

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
    t)

(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(tooltip-mode -1)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Don't be so stingy on the memory, we have lots now.
(setq gc-cons-threshold 200000000)

;; No tabs please
(setq tab-width 4)
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

(setenv "WORKON_HOME" (expand-file-name "~/.pyenv/versions"))
(setenv "VIRTUALENVWRAPPER_HOOK_DIR" (expand-file-name "~/.pyenv/versions"))

(defalias 'which 'executable-find)

(setq *spell-program* (which "aspell"))


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
  )


;;============================================================
;; Bootstrap
;;============================================================
(defun load-local (filename)
  (load (expand-file-name filename user-emacs-directory)))

;; cask
;; (if (file-exists-p "~/.cask/cask.el")
;;    (require 'cask "~/.cask/cask.el")
;;  (require 'cask "/usr/local/share/emacs/site-lisp/cask.el"))
;; (require 'cask (dotemacs-file "vendor/cask"))
;; (cask-initialize)

;; install quelpa
;; (if (require 'quelpa nil t)
;;     (quelpa-self-upgrade)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;;     (eval-buffer)))

;; install quelpa without self upgrading
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; install use-package and the quelpa handler
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)

;; use-package
(package-install 'use-package)
(require 'use-package)
(setq use-package-verbose t)


;;============================================================
;; Appearance
;;============================================================
(global-hl-line-mode -1)
(global-linum-mode -1)
(blink-cursor-mode -1)

;; Scrolling
;; =========================
(setq scroll-error-top-bottom t
      visible-bell t
      scroll-conservatively 10000
      scroll-margin 10
      auto-window-vscroll nil)

;; Themes
;; =========================
(setq color-theme-is-global t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package solarized-theme :ensure t :init (load-theme 'solarized-dark :no-confirm))
;; (use-package solarized-theme :ensure t :init (load-theme 'solarized-light :no-confirm))
;; (use-package material-theme :ensure t :init (load-theme 'material :no-confirm))
;; (use-package cyberpunk-theme :ensure t :init (load-theme 'cyberpunk :no-confirm))
;; (use-package warm-night-theme :ensure t :init (load-theme 'warm-night :no-confirm))
;; (use-package smyx-theme :ensure t :init (load-theme 'smyx :no-confirm))
;; (use-package noctilux-theme :ensure t :init (load-theme 'noctilux :no-confirm))
;; (use-package monokai-theme :ensure t :init (load-theme 'monokai :no-confirm))
;; (use-package molokai-theme :ensure t :init (load-theme 'molokai :no-confirm))
;; (use-package cherry-blossom-theme :ensure t :init (load-theme 'cherry-blossom :no-confirm))
;; (use-package hemisu-dark-theme :ensure t :init (load-theme 'hemisu-dark :no-confirm))
;; (use-package hemisu-ligth-theme :ensure t :init (load-theme 'hemisu-ligth :no-confirm))
;; (use-package material-theme :ensure t :init (load-theme 'material :no-confirm))
;; (use-package material-light-theme :ensure t :init (load-theme 'material-light :no-confirm))
;; (use-package badger-theme :ensure t :init (load-theme 'badger :no-confirm))
;; (use-package darktooth-theme :ensure t :init (load-theme 'darktooth :no-confirm))
;; (use-package gruvbox-theme :ensure t :init (load-theme 'gruvbox :no-confirm))
;; (use-package flatui-theme :ensure t :init (load-theme 'flatui :no-confirm))
;; (use-package tango-plus-theme :ensure t :init (load-theme 'tango-plus :no-confirm))
;; (use-package flatland-black-theme :ensure t :init (load-theme 'flatland-black :no-confirm))
;; (use-package ample-theme :ensure t :init (load-theme 'ample :no-confirm))


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
                   ("Liberation Mono" 14)
                   ("Droid Sans Mono Dotted" 14)
                   ("Inconsolata" 16)
                   ("Anonymous Pro" 16)
                   ("Source Code Pro" 14)
                   ("Ubuntu Mono" 16)
                   ("Monaco" 14)
                   ("Roboto Mono" 15)
                   ("Code New Roman" 14)
                   ("Cousine" 15)
                   ("Ubuntu Mono" 16)
                   ("Menlo" 14)
                   ("Fantasque Sans Mono" 18)
                  ))

;;============================================================
;; Loading
;;============================================================
(load-local "defuns")

;; Major mode abbrevs
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Ⓔ")))
(add-hook 'python-mode-hook (lambda() (setq mode-name "Ⓟ")))
(add-hook 'js2-mode-hook (lambda() (setq mode-name "Ⓙ")))

;; Minor Mode diminish
(eval-after-load "xterm-title" '(diminish 'xterm-title-mode))
(eval-after-load "hi-lock" '(diminish 'hi-lock-mode))
(eval-after-load "outline" '(diminish 'outline-minor-mode))


;;============================================================
;; Packages
;;============================================================

(use-package bind-key
  :config
  ;; Editing
  (bind-key "C-c d" 'duplicate-current-line-or-region)
  (bind-key "C-j" 'newline-and-indent)
  (bind-key "C-c n" 'clean-up-buffer-or-region)
  (bind-key "C-M-;" 'comment-or-uncomment-current-line-or-region)
  (bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
  (bind-key "C-z" 'zap-up-to-char)
  (bind-key "C-|" 'align-regexp)

  (bind-key "M-j" (λ (join-line -1)))
  (bind-key "M-h" 'kill-to-beginning-of-line)
  (bind-key "M-g M-g" 'goto-line-with-feedback)
  (bind-key "M-<up>" 'open-line-above)
  (bind-key "M-<down>" 'open-line-below)

  ;; Buffer/Files
  (bind-key "C-c R" 'rename-this-buffer-and-file)
  (bind-key "C-c D" 'delete-this-buffer-and-file)
  (bind-key "<f8>" (λ (find-file (f-expand "init.el" user-emacs-directory))))
  (bind-key "<f5>" 'recompile)
  (bind-key "C-x C-c" (λ (if (y-or-n-p "Quit Emacs? ") (save-buffers-kill-emacs))))

  ;; Search
  (bind-key "C-c g" 'google)
  (bind-key "C-c y" 'youtube)

  ;; Naming
  (bind-key "C-c m -" (λ (replace-region-by 's-dashed-words)))
  (bind-key "C-c m _" (λ (replace-region-by 's-snake-case)))
  (bind-key "C-c m c" (λ (replace-region-by 's-lower-camel-case)))
  (bind-key "C-c m C" (λ (replace-region-by 's-upper-camel-case)))
)

(use-package diminish)

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
  :ensure t)


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
  :if *is-a-mac*
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  )

(use-package dired-x
  :init
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^__pycache__$\\|\\.git"))

(use-package realgud
  :ensure t
  :disabled t
  )


;;#############################
;; Ace
;;#############################
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
;; Git
;;#############################
(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . magit-status)
         ("C-c v b" . magit-blame))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (magit-with-refresh
       (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))
  (setq magit-revert-buffers t)
  (add-hook 'magit-status-mode-hook 'delete-other-windows)
  (bind-key "C-c C-a" 'magit-quit-session magit-status-mode-map)
  (bind-key "q" 'magit-quit-session magit-status-mode-map))

(use-package git-gutter
  :ensure t
  :bind (("C-c v =" . git-gutter:popup-hunk) ;; show hunk diff
         ("C-c v p" . git-gutter:previous-hunk)
         ("C-c v n" . git-gutter:next-hunk)
         ("C-c v s" . git-gutter:stage-hunk)
         ("C-c v r" . git-gutter:revert-hunk))
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t)
  (custom-set-variables
   '(git-gutter:window-width 1)
   '(git-gutter:modified-sign "█") ;; two space
   '(git-gutter:added-sign "█")    ;; multiple character is OK
   '(git-gutter:deleted-sign "█"))
  )

(use-package git-timemachine
  :ensure t
  :bind ("C-c v t" . git-timemachine))


;;#############################
;; Navigation
;;#############################
(use-package saveplace
  :ensure t
  :init
  (if (fboundp #'save-place-mode)
      (save-place-mode +1)
    (setq-default save-place t)))

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
  (bind-key "p" 'bm-show-prev bm-show-mode-map)
  )

(use-package bookmark+
  :disabled t
  :ensure t)

(use-package sublimity
  :disabled t
  :ensure t
  :config
  (sublimity-mode +1)
  (use-package sublimity-map
    :ensure t
    :disabled t
    :config
    (setq sublimity-map-size 20)
    (setq sublimity-map-fraction 0.3)
    (setq sublimity-map-text-scale -7)))

(use-package tdd-mode
  :load-path "vendor/tdd"
  :bind ("C-<f5>" . tdd-mode))

(use-package ido
  :ensure t
  :init
  (ido-mode t)
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ;; ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory)
        ido-default-file-method 'selected-window
        ido-file-extensions-order '(".py" ".rb" ".el" ".js")
        ido-auto-merge-work-directories-length -1)
  (add-to-list 'ido-ignore-files '(".DS_Store" ".pyc"))
  (add-to-list 'ido-ignore-directories '("__pycache__", ".git"))

  (use-package ido-vertical-mode
    :ensure t
    :config
    (setq ido-vertical-decorations (list "\n➜ " "" "\n" "\n..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n" ""))
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

  (use-package ido-ubiquitous
    :ensure t
    :config
    (ido-ubiquitous-mode +1))

  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode +1))
  )

(use-package visual-regexp
  :disabled t
  :ensure t
  :bind (("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)
         ("C-q" . vr/query-replace))
  )

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode t)
  :config
  (setq projectile-enable-caching t
        projectile-use-git-grep t
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root t
        projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
        ;; (setq projectile-completion-system 'helm)
        ;; (setq projectile-completion-system 'grizzl)
        ;; (setq projectile-completion-system 'ivy)
        )
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'projectile-globally-ignored-files "*.pyc")
  (add-to-list 'projectile-globally-ignored-files "*.python-version")
  (add-to-list 'projectile-globally-ignored-files "*.egg-info")
  (add-to-list 'projectile-globally-ignored-directories "__pycache__")
  (add-to-list 'projectile-globally-ignored-directories ".env")
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (add-to-list 'projectile-globally-ignored-directories ".cask")
  )

(use-package swiper
  :ensure t
  :disabled t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t))

(use-package recentf
  :ensure t
  :bind (("C-x f" . recentf-ido-find-file)
         ("C-c f" . recentf-ido-find-file))
  :init
  (recentf-mode +1)
  :config
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-saved-items 100))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-x C-m" . smex)))

(use-package helm
  :ensure t
  :disabled t
  :init (helm-mode))

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


;;#############################
;; Web
;;#############################
(use-package restclient
  :ensure t
  :mode (("\\.rest" . restclient-mode)))


;;#############################
;; Completion
;;#############################
(use-package eldoc-mode
  :diminish eldoc-mode
  :commands eldoc-mode
  :init
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (use-package company-tern
    :disabled t
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :init
  (guide-key-mode +1)
  :config
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c p" "C-c p 4" "C-c p s" "C-c m" "C-c C-r" "C-c C-p")))


;;#############################
;; Editing
;;#############################
(use-package hippie
  :load-path "vendor/hippie"
  :bind ("C-." . hippie-expand))

(use-package swiper
  :ensure t
  :disabled t
  :bind (("C-r" . swiper)
         ("C-s" . swiper)))

(use-package drag-stuff
  :ensure t
  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-w" . mc/mark-all-words-like-this))
  )

(use-package expand-region
  :ensure t
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
  :defer 5
  :config
  (region-bindings-mode-enable)

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
  (bind-key "m" 'er/mark-method-call region-bindings-mode-map)
  (bind-key "-" 'er/contract-region region-bindings-mode-map)
  (bind-key "+" 'er/expand-region region-bindings-mode-map)
  (bind-key "SPC" 'er/expand-region region-bindings-mode-map)

  (setq region-bindings-mode-disabled-modes '(term-mode))
  (setq region-bindings-mode-disable-predicates
        (list (lambda () buffer-read-only)))

  ;; ispell
  (bind-key "s" 'ispell-region region-bindings-mode-map)
  )

(use-package subword
  :defer 5
  :diminish subword-mode
  :config
  (global-subword-mode 1)
  (defadvice subword-upcase (before upcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word)))

  (defadvice subword-downcase (before downcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word)))

  (defadvice subword-capitalize (before capitalize-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word))))

(use-package imenu-anywhere
  :ensure t
  :bind (("M-i" . ido-imenu-anywhere))
  :init
  (defun jcs-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("use-package"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'jcs-use-package)
  :config
  (setq imenu-anywhere-delimiter-ido " @ "))


;;#############################
;; Modeline
;;#############################
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (use-package nyan-mode
    :ensure t
    :config
    (nyan-mode)))

(use-package powerline
  :ensure t
  :disabled t
  :config
  (powerline-default-theme)
  ;; (setq powerline-display-hud nil)
  (setq powerline-default-separator 'curve)
  )

(use-package re-builder
  :ensure t
  :config
  (setq reb-re-syntax 'string)
  )


;;#############################
;; Buffers/Files
;;#############################
(use-package files
  :bind (("C-c R" . rename-this-buffer-and-file)
         ("C-c D" . delete-this-buffer-and-file))
  :config
  (progn
    (setq auto-save-default nil)
    (setq make-backup-files nil) ; stop creating those backup~ files
    (setq auto-save-default nil) ; stop creating those #autosave# files
    (add-hook 'before-save-hook 'whitespace-cleanup)
    (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer))


;;#############################
;; Languages
;;#############################
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.html\\.ejs\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
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
                (emmet-mode)))
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-style-padding 2)
                (setq web-mode-script-padding 2)
                (setq web-mode-markup-indent-offset 2)
                (define-key web-mode-map [(return)] 'newline-and-indent)))))

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
  (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)
  )

(use-package json-mode
  :ensure t
  :mode ".*\\.json")

(use-package python
  :ensure t
  :config
  (bind-key "C-<f9>" 'mw/add-pudb-debug python-mode-map)
  (bind-key "<f9>" 'mw/add-py-debug python-mode-map)
  (add-hook 'python-mode-hook 'eldoc-mode)
  (add-hook 'python-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

  (use-package pyenv-mode
    :ensure t
    :init
    (defun auto-set-pyenv ()
      (progn
        (message "activating virtualenv")
        (pyenv-mode-set (projectile-project-name))))
    (add-hook 'projectile-switch-project-hook 'auto-set-pyenv)
    (add-hook 'python-mode-hook 'auto-set-pyenv)
    )

  (use-package anaconda-mode
  :quelpa (anaconda-mode
           :fetcher github
           :repo "proofit404/anaconda-mode"
           :files ("*.el" "*.py" "vendor/jedi/jedi" ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py")))
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

  (use-package anaconda-mode
    :disabled t
    :ensure t
    ;; :diminish anaconda-mode
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    :config
    (use-package company-anaconda
      :ensure t
      :init
      (eval-after-load "company"
        '(progn
           (add-to-list 'company-backends 'company-anaconda))))
    )

  (use-package pip-requirements
    :ensure t
    :mode "\\requirements.txt\\'"
    :config (pip-requirements-mode))

  (use-package elpy
    :ensure t
    ;; :diminish elpy-mode
    :quelpa t
    :bind (("C-c t t" . elpy-test-discover-runner)
           ("C-c t d" . elpy-test-django-runner)
           ("C-c t p" . elpy-test-pytest-runner)
           ("C-c C-f" . elpy-find-file)
           ("C-c C-;" . mw/set-django-settings-module))
    :init
    (elpy-enable)
    ;; (add-hook 'pyenv-mode-hook 'elpy-enable)
    ;; (use-package pyvenv
    ;;   :disabled t
    ;;   :ensure t
    ;;   :config
    ;;   (defalias 'workon 'pyvenv-workon)
    ;;   (add-hook 'python-mode-hook 'pyvenv-mode)
    ;;   (add-hook 'python-mode-hook 'mw/auto-activate-virtualenv)
    ;;   (add-hook 'projectile-switch-project-hook 'mw/auto-activate-virtualenv))
    ;; (add-hook 'pyvenv-post-activate-hooks 'elpy-enable)
    ;; (add-hook 'pyvenv-post-activate-hooks 'mw/set-elpy-test-runners)
    :config
    (setq elpy-test-runner 'elpy-test-pytest-runner)
    ;; (setq elpy-rpc-backend "rope")
    (setq elpy-rpc-backend "jedi"))
  )

(use-package yaml-mode
  :ensure t
  :mode ((".*\\.pass" . yaml-mode)
         ("\\.passpierc" . yaml-mode))
  )

(use-package coffee-mode
  :ensure t
  :config
  (setq coffee-tab-width 2))
