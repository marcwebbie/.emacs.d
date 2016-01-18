;;============================================================
;; Package initialize
;;============================================================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)


;;============================================================
;; Defaults
;;============================================================

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

(defalias 'which 'executable-find)

(setq *spell-program* (which "aspell"))

;; Auto save buffers
(setq auto-save-mode  nil)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
(run-with-idle-timer 5 t (lambda () (save-some-buffers t)))


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
(global-linum-mode -1)
(blink-cursor-mode -1)
(setq blink-matching-paren nil)  ;; disable annoying blink-matching-paren

;; Scrolling
;; =========================
(setq scroll-error-top-bottom t
      visible-bell t
      scroll-conservatively 10000
      scroll-margin 10
      auto-window-vscroll nil)

;; Themes
;; ==================================================
(setq color-theme-is-global t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Dark Themes
;; ==================================================
(use-package base16-theme
  :ensure t
  :init
  ;; (load-theme 'base16-default-dark :no-confirm)
  ;; (load-theme 'base16-tomorrow-dark :no-confirm)
  ;; (load-theme 'base16-bright-dark :no-confirm)
  ;; (load-theme 'base16-monokai-dark :no-confirm)
  ;; (load-theme 'base16-3024-dark :no-confirm)
  ;; (load-theme 'base16-atelierlakeside-dark :no-confirm)
  ;; (load-theme 'base16-atelierforest-dark :no-confirm)
  ;; (load-theme 'base16-flat-dark :no-confirm)
  )
(use-package material-theme :ensure t :init (load-theme 'material :no-confirm))
;; (use-package spacemacs-theme :ensure t :init (load-theme 'spacemacs-dark :no-confirm))
;; (use-package solarized-theme :ensure t :init (load-theme 'solarized-dark :no-confirm))
;; (use-package monokai-theme :ensure t :init (load-theme 'monokai :no-confirm))
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
                    ("Roboto Mono" 14)
                    ("Inconsolata" 16)
                    ("Source Code Pro" 14)
                    ("DejaVu Sans Mono" 14)
                    ("Ubuntu Mono" 16)
                    ("Menlo" 14)
                    ("Droid Sans Mono" 14)
                    ("Monaco" 14)
                    ("Anonymous Pro" 14)
                    ("Droid Sans Mono Dotted" 14)
                    ("Liberation Mono" 14)
                    ("Code New Roman" 14)
                    ("Ubuntu Mono" 16)
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
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))

(add-to-list 'auto-mode-alist '(".zshrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".zshenv$" . shell-script-mode))

;;; global hook modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)


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
  (bind-key "C-c c" 'copy-file-name-to-clipboard)

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
(use-package indent-guide
  :disabled t
  :ensure t
  :diminish indent-guide-mode
  :init
  (add-hook 'prog-mode-hook 'indent-guide-global-mode)
  :config
  (setq indent-guide-char ":"))

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
  (setq-default auto-revert-interval 1))


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
  (super-save-initialize))


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
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-copy-env "WORKON_HOME")
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

(use-package dired
  :init
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^__pycache__$\\|\\.git"))

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
  :load-path "vendor/tdd"
  :bind ("C-<f5>" . tdd-mode)
  :config
  (require 'tdd))

(use-package ido
  :disabled t
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
    :init
    (setq ido-vertical-decorations (list "\n➜ " "" "\n" "\n..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n" ""))
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
    )

  (use-package ido-ubiquitous
    :ensure t
    :init
    (ido-ubiquitous-mode +1))

  (use-package flx-ido
    :ensure t
    :init
    (flx-ido-mode +1)
    (setq ido-use-faces nil))
  )

(use-package counsel
  :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)                           ; global
         ("C-c s g" . counsel-git-grep)
         ("C-c s a" . counsel-ag)
         ("M-i" . ivy-imenu-goto)
         ("C-x C-f" . counsel-find-file))
  :bind (:map help-map                                   ; help-map
              ("f" . counsel-describe-function)
              ("v" . counsel-describe-variable)
              ("C-l" . counsel-info-lookup-symbol))
  :config
  (defun ivy-imenu-get-candidates-from (alist  &optional prefix)
    (cl-loop for elm in alist
             nconc (if (imenu--subalist-p elm)
                       (ivy-imenu-get-candidates-from
                        (cl-loop for (e . v) in (cdr elm) collect
                                 (cons e (if (integerp v) (copy-marker v) v)))
                        (concat prefix (if prefix ".") (car elm)))
                     (and (cdr elm) ; bug in imenu, should not be needed.
                          (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                          (list (cons (concat prefix (if prefix ".") (car elm))
                                      (copy-marker (cdr elm))))))))
  (defun ivy-imenu-goto ()
    "Go to buffer position"
    (interactive)
    (let ((imenu-auto-rescan t) items)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (setq items (imenu--make-index-alist t))
      (ivy-read "imenu items:"
                (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
                :action (lambda (k) (goto-char k)))))

  (defun imenu-mark-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("use-package"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'imenu-mark-use-package)
  )

(use-package swiper
  :ensure t
  :demand t
  :bind
  (([remap isearch-forward]  . swiper)
   ([remap isearch-backward] . swiper))
  :config
  (setq ivy-display-style 'fancy)
  (ivy-mode 1))

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
  (add-to-list 'projectile-globally-ignored-directories ".cask"))

(use-package recentf
  :ensure t
  :bind (("C-x f" . recentf-ido-find-file)
         ("C-c f" . recentf-ido-find-file))
  :init
  (recentf-mode +1)
  :config
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
;; Web
;;#############################
(use-package restclient
  :ensure t
  :mode (("\\.rest" . restclient-mode)))


;;#############################
;; Completion
;;#############################
(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc
  :init
  (add-hook 'emacs-lisp-mode 'eldoc-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package company
  :ensure t
  :commands global-company-mode
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'global-company-mode)
  (use-package company-tern
    :disabled t
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

;;#############################
;; Editing
;;#############################
(use-package hippie
  :load-path "vendor/hippie"
  :bind ("C-." . hippie-expand))

(use-package drag-stuff
  :ensure t
  :bind (("M-p" . drag-stuff-up)
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

(use-package imenu-anywhere
  :disabled t
  :ensure t
  :bind (("M-i" . ido-imenu-anywhere))
  :init
  (defun imenu-mark-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("use-package"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'imenu-mark-use-package)
  :config
  (setq imenu-anywhere-delimiter-ido " @ "))

(use-package re-builder
  :ensure t
  :config
  (setq reb-re-syntax 'string))


;;#############################
;; Modeline
;;#############################
(use-package smart-mode-line
  :disabled t
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (use-package nyan-mode
    :ensure t
    :config
    (nyan-mode)))

(use-package spaceline-config
  :ensure spaceline
  :config
  ;; (spaceline-spacemacs-theme)
  (spaceline-emacs-theme)
  (setq powerline-default-separator 'utf-8)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (setq powerline-default-separator 'box)
  ;; (setq powerline-default-separator 'bar)
  ;; (setq powerline-default-separator 'arrow-fade)
  ;; (setq powerline-default-separator 'slant)
  ;; (setq powerline-default-separator 'wave)
  ;; (setq powerline-default-separator 'utf-8)
  ;; (setq powerline-default-separator 'curve)
  ;; (setq powerline-default-separator 'chamfer)
  ;; (setq powerline-default-separator 'roundstub)
  (setq powerline-height '18)
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

(use-package python
  :ensure t
  :commands (python-mode)
  :config
  ;; Hooks
  (add-hook 'python-mode-hook 'mw/python--add-todo-fixme-bug-hightlight)
  (add-hook 'python-mode-hook 'mw/python--add-debug-highlight)

  ;; Bindings
  (bind-key "C-<f9>" 'mw/add-pudb-debug python-mode-map)
  (bind-key "<f9>" 'mw/add-py-debug python-mode-map)

  (use-package pyvenv
    :ensure t
    :config
    (setenv "WORKON_HOME" (first-file-exists-p (list "~/.virtualenvs" "~/.pyenv/versions")))
    (setenv "VIRTUALENVWRAPPER_HOOK_DIR" (getenv "WORKON_HOME"))
    )

  (use-package auto-virtualenv
    :load-path "vendor"
    :config
    (setq auto-virtualenv-dir "~/.virtualenvs")
    ;; (setq auto-virtualenv-dir "~/.pyenv/versions")
    (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

  (use-package anaconda-mode
    :ensure t
    :diminish anaconda-mode
    :demand t
    :init
    :config
    (add-hook 'python-mode-hook #'anaconda-mode)
    (use-package company-anaconda
      :ensure t
      :if (boundp 'company-backends)
      :init (add-to-list 'company-backends 'company-anaconda))
    )

  (use-package pip-requirements
    :ensure t
    :mode "\\requirements.txt\\'"
    :config (pip-requirements-mode))

  (use-package elpy
    :ensure t
    :diminish elpy-mode
    :bind (("C-c t" . elpy-test-django-runner)
           ("C-c C-f" . elpy-find-file)
           ("C-c C-;" . mw/set-django-settings-module)
           ("C-c C-p" . elpy-autopep8-fix-code))
    :init
    (elpy-enable)
    :config

    (setq elpy-rpc-backend "jedi")
    (add-hook 'pyvenv-post-activate-hooks 'elpy-rpc-restart)
    (setq elpy-test-django-runner-command '("python" "manage.py" "test" "--noinput"))
    (defun elpy-setup ()
      (interactive)
      (progn
        (compile (format "%s install -U jedi flake8 importmagic autopep8 yapf" (executable-find "pip")))))
    (eval-after-load "elpy"
      '(cl-dolist (key '("M-," "M-." "M-" "M-*" "M-?"))
         (define-key elpy-mode-map (kbd key) nil)))
    )

  (use-package jedi
    :disabled t
    :ensure t
    :bind (("M-." . jedi:goto-definition))
    )

  (use-package py-autopep8
    :ensure t
    :bind (("C-c C-a" . py-autopep8-buffer)))
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

(use-package markdown-mode
  :mode ((".*\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :ensure t)

(use-package pig-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode))

(use-package less-css-mode
  :ensure t)
