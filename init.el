;; ======================
;; Defaults
;; ======================
(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(tooltip-mode -1)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-default)

;; ======================
;; Bootstrap
;; ======================

(if (file-exists-p "~/.cask/cask.el")
    (require 'cask "~/.cask/cask.el")
  (require 'cask "/usr/local/share/emacs/site-lisp/cask.el"))
(cask-initialize)


;; =======================
;; Appearance
;; =======================

(global-hl-line-mode t)
(tooltip-mode -1)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Scrolling
(setq scroll-error-top-bottom t   ; Move to beg/end of buffer before signalling an error
      scroll-conservatively 10000 ; Never recenter the screen while scrolling
      scroll-margin 10
      auto-window-vscroll nil)

(blink-cursor-mode -1)

;; (load-theme 'zonokai :no-confirm)
(set-frame-font "Droid Sans Mono-14")


;; =======================
;; Packages
;; =======================

;; default
(require 'bind-key)
(require 'diminish)
(require 'use-package)
(require 'f)
(require 's)

(require 'init-defuns)

(require 'init-ido)
(require 'init-smex)
(require 'init-osx)

(require 'init-smartparens)
(require 'init-drag-stuff)
(require 'init-expand-region)
(require 'init-multiple-cursors)
(require 'init-region-bindings)
(require 'init-python)


;; =======================
;; Keybindings
;; =======================

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
