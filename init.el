;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs 30+ Configuration — macOS & GNU/Linux
;;; Author: Marc (marcwebbie)
;;; Goals: stable, modern, defer-friendly, Monokai-first theme, strong Python/
;;;        Django/TypeScript/Terraform stacks, optional LLM helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;---------------------------------------------------------------------------
;;; 0) HARD REQUIREMENTS & VERSION GUARD
;;;---------------------------------------------------------------------------
(when (< emacs-major-version 30)
  (error "This configuration requires Emacs 30 or newer; you have %s"
         emacs-major-version))

(setq inhibit-startup-message t)

(require 'treesit)

;; Prefer user-compiled grammars over system ones
(setq treesit-extra-load-path
      (list (expand-file-name "~/.emacs.d/tree-sitter/")))


;;;---------------------------------------------------------------------------
;;; 1) PACKAGE MANAGEMENT: STRAIGHT.EL + USE-PACKAGE
;;;---------------------------------------------------------------------------
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let* ((straight-repo-dir (expand-file-name "straight/repos/" user-emacs-directory))
       (bootstrap-file (expand-file-name "straight.el/bootstrap.el" straight-repo-dir))
       (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'bind-key) ;; Ensure bind-key for use-package bindings
(setq straight-use-package-by-default t)

;; Where straight looks for recipes (includes MELPA).
(setq straight-recipe-repositories
      '(melpa gnu-elpa-mirror nongnu-elpa el-get emacsmirror-mirror))

;; One-time refresh to ensure newest recipes.
(ignore-errors
  (when (fboundp 'straight-pull-recipe-repositories)
    (straight-pull-recipe-repositories)))

;; Proactively make sure Magit deps exist.
(dolist (pkg '(compat cond-let transient with-editor))
  (ignore-errors (straight-use-package pkg)))

(defun my/straight-refresh-recipes ()
  "Refresh straight's recipe repositories (MELPA, etc.)."
  (interactive)
  (when (fboundp 'straight-pull-recipe-repositories)
    (straight-pull-recipe-repositories)))

;;;---------------------------------------------------------------------------
;;; 2) CORE UX: MINIBUFFER, COMPLETION, HISTORY
;;;---------------------------------------------------------------------------
(use-package savehist :init (savehist-mode 1))

(use-package vertico
  :init (vertico-mode 1)
  :custom (vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :defer t
  :bind (("C-s"     . consult-line)
         ("M-i"     . consult-imenu)
         ("C-c C-j" . consult-imenu)
         ("C-x b"   . consult-buffer)))

(use-package embark :defer t)
(use-package embark-consult :after (embark consult))

;; In-buffer completion (CAPF): Corfu
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preview-current nil)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("S-TAB" . corfu-previous)))

;;;---------------------------------------------------------------------------
;;; 3) SENSIBLE DEFAULTS & UI
;;;---------------------------------------------------------------------------
(setq sentence-end-double-space nil
      indent-tabs-mode nil
      tab-width 4
      ring-bell-function #'ignore
      use-short-answers t)

(setq initial-major-mode 'fundamental-mode)
(save-place-mode 1)
(recentf-mode 1)

(setq auto-save-visited-interval 2)
(auto-save-visited-mode 1)

(setq auto-revert-avoid-polling t
      auto-revert-interval 5
      auto-revert-check-vc-info t)
(global-auto-revert-mode 1)

(line-number-mode 1)
(column-number-mode 1)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 40.0 ;; Enhance smooth scrolling
      mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

(when (display-graphic-p) (context-menu-mode 1))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-grow-only t
              display-line-numbers-width 3)

(add-hook 'text-mode-hook #'visual-line-mode)

(windmove-default-keybindings 'control)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun mw/backup-file-name (fpath)
  "Return backup path under ~/.emacs.d/emacs-backup/ for FPATH."
  (let* ((backup-root "~/.emacs.d/emacs-backup/")
         (file-path (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backup (replace-regexp-in-string "//" "/" (concat backup-root file-path "~"))))
    (make-directory (file-name-directory backup) t)
    backup))
(setq make-backup-file-name-function #'mw/backup-file-name)

(setq indicate-buffer-boundaries 'left
      show-trailing-whitespace nil)

;; Silence :distant-foreground=nil warnings
(set-face-attribute 'highlight nil :distant-foreground 'unspecified)

;; Maximize the initial frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;;---------------------------------------------------------------------------
;;; 4) KEYBOARD / OS-INTEGRATION
;;;---------------------------------------------------------------------------
(unless (eq system-type 'darwin)
  (setq x-alt-keysym 'meta
        x-super-keysym 'meta))

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none))


;;;---------------------------------------------------------------------------
;;; 5) THEMES & FONTS — Wayland/HiDPI aware, emoji + Nerd Icons fallbacks
;;;---------------------------------------------------------------------------
(use-package nerd-icons
  :defer t
  ;; We'll set the family dynamically below if present
  :custom (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package monokai-theme :defer t)
(use-package doom-themes :defer t)
(use-package spacemacs-theme :defer t)
(use-package modus-themes :defer t)
(use-package doom-themes :defer t)

(defun mw/apply-theme ()
  "Load preferred theme."
  ;; (load-theme 'monokai t)
  ;; (load-theme 'modus-vivendi t)
  ;; (load-theme 'modus-operandi t)
  (load-theme 'doom-one t)
  )

(add-hook 'after-init-hook #'mw/apply-theme)

;; Preferred fonts (hardcode top choices; assume available)
(let ((candidates '("Ubuntu Mono" "JetBrains Mono" "Fira Code" "DejaVu Sans Mono")))
  (let ((font (seq-find (lambda (f) (member f (font-family-list))) candidates)))
    (when font
      (set-frame-font (format "%s-12" font) nil t))))

;;;---------------------------------------------------------------------------
;;; 6) MODELINE & DASHBOARD
;;;---------------------------------------------------------------------------
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-time nil))

(use-package dashboard
  :defer t
  :config
  (dashboard-setup-startup-hook))

;;;---------------------------------------------------------------------------
;;; 7) DISCOVERY & NAVIGATION
;;;---------------------------------------------------------------------------
(use-package which-key :init (which-key-mode 1))

(use-package avy
  :defer t
  :bind (("C-c SPC" . avy-goto-char)))

(use-package consult :defer t)

(use-package projectile
  :init (projectile-mode 1)
  :custom
  (projectile-mode-line-function
   (lambda () (format " [%s]" (projectile-project-name))))
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package deadgrep
  :defer t
  :bind (("C-c s" . deadgrep)))

(use-package symbol-overlay
  :defer t
  :bind (("M-[" . symbol-overlay-jump-prev)
         ("M-]" . symbol-overlay-jump-next)))

;;;---------------------------------------------------------------------------
;;; 8) EDITING UTILITIES & TEXT OBJECTS
;;;---------------------------------------------------------------------------
(delete-selection-mode 1)
(use-package subword :init (global-subword-mode 1))

(use-package smartparens
  :diminish smartparens-mode
  :init (smartparens-global-mode 1)
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  (setq smartparens-global-strict-mode t)
  :bind (("C-M-k" . sp-kill-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("M-s"   . sp-splice-sexp)
         ("M-r"   . sp-splice-sexp-killing-around)
         ("C-)"   . sp-forward-slurp-sexp)
         ("C-}"   . sp-forward-barf-sexp)
         ("C-("   . sp-backward-slurp-sexp)
         ("C-{"   . sp-backward-barf-sexp)
         ("M-S"   . sp-split-sexp)
         ("M-J"   . sp-join-sexp)
         ("C-M-t" . sp-transpose-sexp)))

(use-package multiple-cursors
  :defer t
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-w" . mc/mark-all-words-like-this)))

(use-package expand-region
  :defer t
  :bind (("C-=" . er/expand-region))
  :config
  (defun er/add-python-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append er/try-expand-list '(er/mark-defun))))
  (add-hook 'python-ts-mode-hook #'er/add-python-mode-expansions))

(use-package region-bindings-mode
  :defer t
  :commands region-bindings-mode-enable
  :init (region-bindings-mode-enable)
  :config
  (let ((map region-bindings-mode-map))
    (define-key map (kbd "a") #'mc/mark-all-like-this)
    (define-key map (kbd "e") #'mc/edit-lines)
    (define-key map (kbd "p") #'mc/mark-previous-like-this)
    (define-key map (kbd "P") #'mc/skip-to-previous-like-this)
    (define-key map (kbd "n") #'mc/mark-next-like-this)
    (define-key map (kbd "N") #'mc/skip-to-next-like-this)
    (define-key map (kbd "c") #'er/mark-outer-python-block)
    (define-key map (kbd "f") #'er/mark-defun)
    (define-key map (kbd "u") #'er/mark-url)
    (define-key map (kbd "b") #'er/mark-python-block)
    (define-key map (kbd "m") #'er/mark-method-call)
    (define-key map (kbd "-") #'er/contract-region)
    (define-key map (kbd "+") #'er/expand-region)
    (define-key map (kbd "=") #'er/expand-region)
    (define-key map (kbd "SPC") #'er/expand-region))
  (setq region-bindings-mode-disabled-modes '(term-mode))
  (setq region-bindings-mode-disable-predicates (list (lambda () buffer-read-only))))

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
			   try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-c d") #'duplicate-current-line-or-region)
(global-set-key (kbd "C-M-;") #'comment-or-uncomment-current-line-or-region)

(defun duplicate-current-line-or-region (arg)
  "Duplicate current line or region ARG times."
  (interactive "p")
  (let (beg end (origin (point)))
    (when (and (region-active-p) (> (point) (mark))) (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (when (region-active-p) (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (_ arg)
        (goto-char end) (newline) (insert region) (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun comment-or-uncomment-current-line-or-region ()
  "Comment or uncomment current line or active region (whole lines)."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p) (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(defun back-to-indentation-or-beginning-of-line ()
  "Jump to indentation or beginning of line if already at indentation."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "C-a") #'back-to-indentation-or-beginning-of-line)

(defun goto-line-with-feedback ()
  "Temporarily show line numbers while prompting for a line."
  (interactive)
  (let ((was-on (bound-and-true-p display-line-numbers)))
    (unwind-protect
        (progn (unless was-on (display-line-numbers-mode 1))
               (call-interactively #'goto-line))
      (unless was-on (display-line-numbers-mode -1)))))

;;;---------------------------------------------------------------------------
;;; 9) VISUAL NICETIES
;;;---------------------------------------------------------------------------
(use-package beacon
  :init (beacon-mode 1)
  :custom
  (beacon-color "yellow")
  (beacon-size 40)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-duration 0.5)
  (beacon-blink-delay 0.3))

(use-package zoom
  :defer t
  :init (zoom-mode 1)
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-major-modes '(dired-mode help-mode)))

(use-package dimmer
  :defer t
  :init (dimmer-mode 1)
  :config
  (dimmer-configure-which-key)
  (with-eval-after-load 'magit
    (dimmer-configure-magit))
  (dimmer-configure-hydra)
  (add-hook 'special-mode-hook (lambda () (dimmer-mode -1))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package prism
  :defer t
  :hook (prog-mode . prism-mode))

;;;---------------------------------------------------------------------------
;;; 10) VCS INTEGRATION
;;;---------------------------------------------------------------------------
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  (defun my-magit-auto-branch-name-to-commit-message ()
    "Insert branch ticket number prefix into commit message, e.g. [1234] "
    (let* ((branch-name (magit-get-current-branch))
           (ticket (and branch-name
                        (string-match "\\([0-9]+\\)" branch-name)
                        (match-string 1 branch-name))))
      (when ticket (goto-char (point-min)) (insert (format "[%s] " ticket)))))
  (add-hook 'git-commit-setup-hook #'my-magit-auto-branch-name-to-commit-message))

(use-package diff-hl
  :init (global-diff-hl-mode 1)
  :config
  (bind-keys :map global-map
             ("C-c v r" . diff-hl-revert-hunk)
             ("C-c v s" . diff-hl-stage-current-hunk)
             ("C-c v n" . diff-hl-next-hunk)
             ("C-c v p" . diff-hl-previous-hunk))
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Requires: (use-package ...) already set up
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine)
  :init
  ;; Optional: start in "verbose" mode (shows commit info in the header-line)
  (setq git-timemachine-show-minibuffer-details t)
  :config
  ;; Optional: more readable commit info in header-line
  (setq git-timemachine-abbreviation-length 12)
  ;; Sensible keybinding (prefix under C-c g)
  (global-set-key (kbd "C-c v t") #'git-timemachine)
  ;; Evil-friendly keybindings when timemachine is active
  (with-eval-after-load 'evil
    (evil-define-key 'normal git-timemachine-mode-map
		     (kbd "h") #'git-timemachine-show-previous-revision
		     (kbd "l") #'git-timemachine-show-next-revision
		     (kbd "q") #'git-timemachine-quit)))

;;;---------------------------------------------------------------------------
;;; 11) FILETYPES & LANGUAGE MODES (General)
;;;---------------------------------------------------------------------------
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom (markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("M-p" . move-text-up)
              ("M-n" . move-text-down)))

(use-package jinja2-mode :mode "\\.j2\\'")
(use-package nginx-mode :mode "\\.conf\\.j2\\'")
(use-package conf-mode
  :mode (("\\.vcl\\.j2\\'" . conf-mode) ;; FIX: Use conf-mode for VCL files
         ("\\.env\\..*\\'" . conf-mode)
         ("\\.env\\'" . conf-mode)))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :hook (yaml-mode . display-line-numbers-mode)
  :custom (yaml-indent-offset 2))

(use-package restclient :mode "\\.http\\'")

;;;---------------------------------------------------------------------------
;;; 12) LSP BACKBONE
;;;---------------------------------------------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :init
  (setq lsp-completion-provider :capf
        lsp-prefer-capf t
        lsp-idle-delay 0.5
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-suggest-imports t
        lsp-completion-enable-additional-text-edits t)
  :bind (("C-c i" . lsp-organize-imports)
         ("C-c a" . lsp-execute-code-action)))

(use-package lsp-ui :defer t)

(use-package lsp-pyright
  :after lsp-mode
  :defer t
  :init
  (setq lsp-pyright-stub-path (expand-file-name "~/.local/lib/python3.11/site-packages/django-stubs"))) ;; Adjust to your python version/site-packages

;;;---------------------------------------------------------------------------
;;; 13) PYTHON & DJANGO
;;;---------------------------------------------------------------------------
(use-package python
  :custom (python-indent-offset 4)
  :init (setq python-shell-interpreter "python3"))

;;; --- Ruff Server (updated for Ruff's built-in LSP server in 2025; replaces ruff-lsp) ---
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'lsp-pyright
    (setq lsp-pyright-enable-ruff nil))  ; Disable integrated Ruff in Pyright
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () (list "ruff" "server")))
    :activation-fn (lsp-activate-on "python")
    :priority 0
    :server-id 'ruff))
  (defun mw/python-lsp-start ()
    (when (derived-mode-p 'python-ts-mode)
      (when (executable-find "pyright-langserver")  ; Note: pyright-langserver is the executable
        (require 'lsp-pyright nil t))
      (lsp-deferred)))
  (add-hook 'python-ts-mode-hook #'mw/python-lsp-start))

(defun mw/python-organize-imports-on-save ()
  "Use Ruff's LSP code action to organize imports (safe no-op if unavailable)."
  (when (and (derived-mode-p 'python-ts-mode)
             (boundp 'lsp-mode) lsp-mode)
    (ignore-errors (lsp-organize-imports))))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (remove-hook 'before-save-hook #'py-isort-before-save t)  ; Remove old isort if present
            (add-hook 'before-save-hook #'mw/python-organize-imports-on-save nil t)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))  ; Auto-format with Ruff on save

(use-package flycheck
  :init (global-flycheck-mode 1)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-idle-change-delay 0.8)
  :bind (("C-c f r" . flycheck-verify-setup)
         ("C-c f s" . flycheck-select-checker)
         ("C-c f n" . flycheck-next-error)
         ("C-c f p" . flycheck-previous-error)))
;; Optional: Disable Flycheck in Python if preferring LSP diagnostics
;; (add-hook 'python-ts-mode-hook (lambda () (flycheck-mode -1)))

(when (file-directory-p "~/Projects/auto-virtualenv/")
  (use-package auto-virtualenv
    :load-path "~/Projects/auto-virtualenv/"
    :hook ((python-ts-mode . auto-virtualenv-find-and-activate)
           (projectile-after-switch-project . auto-virtualenv-find-and-activate))
    :custom (auto-virtualenv-verbose t)))

(when (file-directory-p "~/Projects/python-tdd-mode")
  (use-package python-tdd-mode
    :load-path "~/Projects/python-tdd-mode"
    :hook (python-ts-mode . python-tdd-mode)
    :bind-keymap ("C-c t" . python-tdd-mode-command-map)
    :custom
    (python-tdd-mode-test-runner 'pytest)
    (python-tdd-mode-notify-on-pass t)
    (python-tdd-mode-notify-on-fail t)
    (python-tdd-mode-auto-run-on-save t)))

(use-package aggressive-indent
  :defer t
  :init (global-aggressive-indent-mode 1)
  :config
  (dolist (hook '(python-ts-mode-hook yaml-mode-hook web-mode-hook))
    (add-hook hook (lambda () (aggressive-indent-mode -1)))))

;;;---------------------------------------------------------------------------
;;; 14) TREESITTER (Emacs 30+)
;;;---------------------------------------------------------------------------
(require 'treesit)
(message "Treesit available? %S" (treesit-available-p))

;; Set maximum font-lock level for detailed highlighting
(setq treesit-font-lock-level 4)

;; Define sources for language grammars (for easy installation)
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

;; Remap major modes to Tree-Sitter versions
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tsx-mode . tsx-ts-mode)))

;; Function to install grammars if missing (run manually with M-x mw/install-treesit-grammars)
(defun mw/install-treesit-grammars ()
  "Install all defined Tree-Sitter grammars if not available."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;; Optional: Auto-install hook (prompts if missing when mode activates)
(add-hook 'treesit-load-language-hook
          (lambda (lang)
            (unless (treesit-language-available-p lang)
              (when (y-or-n-p (format "Install Tree-Sitter grammar for %s? " lang))
                (treesit-install-language-grammar lang)))))

;;;---------------------------------------------------------------------------
;;; 15) TYPESCRIPT / TSX
;;;---------------------------------------------------------------------------
;; Built-in modes are used via remaps in Tree-Sitter section

;; Mode associations (if not automatic)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; LSP is already hooked in LSP section

;; Optional: Additional TS-specific config
(use-package typescript-mode
  :defer t
  :custom
  (typescript-indent-level 2))  ;; Adjust as needed

;;;---------------------------------------------------------------------------
;;; 16) TERRAFORM
;;;---------------------------------------------------------------------------
(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode))
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (add-hook 'terraform-mode-hook #'outline-minor-mode))

;;;---------------------------------------------------------------------------
;;; 17) ANSIBLE / YAML / VAULT
;;;---------------------------------------------------------------------------
(use-package ansible-mode
  :disabled t
  :defer t
  :mode ("\\ansible/.*\\.ya?ml\\'" . ansible-mode)
  :hook ((ansible-mode . lsp-deferred)
         (ansible-mode . display-line-numbers-mode))
  :custom
  (ansible-vault-password-file "~/.ansible/vault_pass"))


;;;---------------------------------------------------------------------------
;;; 21) MISC KEYBINDS & UTILITIES
;;;---------------------------------------------------------------------------
(keymap-global-set "C-M-;" #'comment-or-uncomment-current-line-or-region)

(use-package move-text
  :defer t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(defun mw/open-containing-folder ()
  "Open current file's folder in system file manager."
  (interactive)
  (let ((dir (or (and buffer-file-name (file-name-directory buffer-file-name))
                 default-directory)))
    (cond
     ((eq system-type 'darwin) (call-process "open" nil 0 nil dir))
     ((eq system-type 'gnu/linux) (call-process "xdg-open" nil 0 nil dir))
     (t (user-error "Unsupported system")))))

(defun mw/copy-diff-to-clipboard (&optional remote-branch)
  "Copy diff vs REMOTE-BRANCH and commits as Markdown to clipboard."
  (interactive "sBranch to compare against (default: origin/develop): ")
  (let* ((rb (if (or (null remote-branch) (string-empty-p remote-branch))
                 "origin/develop" remote-branch))
         (diff (shell-command-to-string (format "git diff %s" rb)))
         (log (shell-command-to-string (format "git --no-pager log --oneline --decorate %s..HEAD" rb)))
         (md (concat "```diff\n" diff "```\n\n### Commits\n```\n" log "```\n")))
    (kill-new md)
    (message "Diff and commits copied to clipboard.")))
(global-set-key (kbd "C-c C") #'mw/copy-diff-to-clipboard)

;;;---------------------------------------------------------------------------
;;; 22) SYSTEM CLEANUPS / QUALITY
;;;---------------------------------------------------------------------------
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq scroll-error-top-bottom t)

(use-package jenkinsfile-mode
  :defer t
  :mode ("Jenkinsfile\\'" . jenkinsfile-mode))

;; Enhanced smooth scrolling (built-in pixel-scroll is enabled; add ultra-scroll for more fluidity)
(use-package ultra-scroll
  :straight (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  :init (ultra-scroll-mode 1)
  :custom
  (ultra-scroll-lines 1)  ;; Scroll by 1 line at a time
  (ultra-scroll-cursor t)  ;; Keep cursor centered
  )

;; Your Python and TypeScript configurations look solid overall—no major issues detected.
;; For Python: LSP-Pyright and Ruff handle pyproject.toml for settings (e.g., type stubs, formatting).
;; auto-virtualenv should detect venvs from pyproject.toml if configured with tools like Poetry.
;; Flycheck integrates well for linting. Consider adding blacken or Ruff for auto-formatting if needed.
;; For TypeScript: Now uses ts-modes with LSP; install grammars via mw/install-treesit-grammars.
;; If using Deno/Node, ensure tsserver is in PATH for LSP.
