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


;; ===========================================
;; sane-defaults
;; ===========================================
;; Show line numbers
(global-linum-mode t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
;; (require 'iso-transl)
(define-key key-translation-map [dead-tilde] "~")

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Keep cursor away from edges when scrolling up/down
;;(require 'smooth-scrolling)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; use icomplete in minibuffer
(icomplete-mode t)

;; use icomplete in minibuffer
(icomplete-mode t)

;; Save place in opened files
(use-package saveplace
  :init
  (progn 
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory ".saved-places"))))

(use-package uniquify
  :init
  (progn 
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    (setq uniquify-buffer-name-style 'forward)))


;; ===========================================
;; Setup defuns
;; ===========================================
(defun select-text-in-quote ()
  (interactive)
  (let (p1)
    (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘⦅〚⦃\"")
    (setq p1 (point))
    (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙⦆〛⦄\"")
    (set-mark p1)))


(defun mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))

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
;; Setup appearance
;; ===========================================
(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(load-theme 'solarized-dark :no-confirm)

(use-package powerline
  :init (powerline-default-theme))


;; ===========================================
;; Setup hippie
;; ===========================================
(defvar he-search-loc-backward (make-marker))
(defvar he-search-loc-forward (make-marker))

(defun try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string). It returns t if a new expansion is found, nil otherwise."
  (let (expansion)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-dabbrev-search he-search-string t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-dabbrev-search he-search-string nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          nil)
      (progn
        (he-substitute-string expansion t)
        t))))

(defun try-expand-line-closest-first (old)
  "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string). It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
        (strip-prompt (and (get-buffer-process (current-buffer))
                           comint-use-prompt-regexp
                           comint-prompt-regexp)))
    (unless old
      (he-init-string (he-line-beg strip-prompt) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

;; Hippie expand: sometimes too hip
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-closest-first
                                         try-complete-file-name
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Create own function to expand lines (C-S-.)
(defun hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line-closest-first
                                            try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

;; Don't case-fold when expanding with hippe
(defun hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))



;; ===========================================
;; Setup keybindings
;; ===========================================
;; Like readline
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Occur can show all lines in the current buffer containing a match for REGEXP.
(global-set-key (kbd "C-c o") 'occur)

;; Reindent when RET
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

(global-set-key (kbd "M-#") 'mark-current-word)
(global-set-key (kbd "M-*") 'select-text-in-quote)

(bind-key "M-+" 'shrink-window)
(bind-key "M-_" 'enlarge-window)

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
  :config
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
