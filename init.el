;;============================================================
;; Defaults
;;============================================================
(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(tooltip-mode -1)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(tooltip-mode -1)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'setup-default)


;;============================================================
;; Bootstrap
;;============================================================

(if (file-exists-p "~/.cask/cask.el")
    (require 'cask "~/.cask/cask.el")
  (require 'cask "/usr/local/share/emacs/site-lisp/cask.el"))
(cask-initialize)


;;============================================================
;; Appearance
;;============================================================

(global-hl-line-mode t)
(global-linum-mode t)
(blink-cursor-mode -1)

(setq visible-bell t
      font-lock-maximum-decoration nil
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Scrolling
(setq scroll-error-top-bottom t 
      scroll-conservatively 10000
      scroll-margin 10
      auto-window-vscroll nil)

;; (load-theme 'soothe :no-confirm)
(load-theme 'warm-night :no-confirm)
(set-frame-font "Droid Sans Mono-14")


;;============================================================
;; Loading
;;============================================================

;;(setq user-emacs-directory default-directory)

(defun load-local (file)
  (load (expand-file-name file user-emacs-directory)))

(load-local "defuns")


;;============================================================
;; Packages
;;============================================================

(require 'bind-key)
(require 'diminish)
(require 'use-package)
(require 'pallet)
(require 'f)
(require 's)

;; System
;;====================
(require 'setup-osx)
(require 'setup-magit)

;; Navigation
;;====================
(require 'setup-ace-jump)
(require 'setup-ace-window)
(require 'setup-golden-ratio)
(require 'setup-ido)
(require 'setup-projectile)
;; (require 'setup-smex)
(require 'setup-ivy)
(require 'setup-search)

;; Editing
;;====================
(require 'setup-smartparens)
(require 'setup-drag-stuff)
(require 'setup-expand-region)
(require 'setup-multiple-cursors)
(require 'setup-region-bindings)

;; Programming languages
;;====================
(require 'setup-python)
;; (require 'setup-ruby)


;;============================================================
;; Keybindings
;;============================================================

;; (bind-key "C-x C-c" (λ (if (y-or-n-p "Quit Emacs? ") (save-buffers-kill-emacs))))
(bind-key "<f6>" 'linum-mode)
(bind-key "<f8>" (λ (find-file (f-expand "init.el" user-emacs-directory))))
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

(bind-key "C-c m -" (λ (replace-region-by 's-dashed-words)))
(bind-key "C-c m _" (λ (replace-region-by 's-snake-case)))
(bind-key "C-c m c" (λ (replace-region-by 's-lower-camel-case)))
(bind-key "C-c m C" (λ (replace-region-by 's-upper-camel-case)))


;;============================================================
;; Afterwards configurations
;;============================================================

