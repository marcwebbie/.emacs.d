;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs Configuration for macOS and Ubuntu with Microsoft Keyboard
;;; Requires Emacs 29 or newer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (< emacs-major-version 29)
  (error (format "Emacs Bedrock only works with Emacs 29 and newer; you have version ~a" emacs-major-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if the system is not macOS (darwin) before setting the Meta key bindings
(unless (eq system-type 'darwin)
  ;; Set both Alt and Super (Windows) keys as Meta
  (setq x-alt-keysym 'meta)
  (setq x-super-keysym 'meta))

;; Package initialization
;;
;; We'll stick to the built-in GNU and non-GNU ELPAs (Emacs Lisp Package
;; Archive) for the base install, but there are some other ELPAs you could look
;; at if you want more packages. MELPA in particular is very popular. See
;; instructions at:
;;
;;    https://melpa.org/#/getting-started
;;
;; You can simply uncomment the following if you'd like to get started with
;; MELPA packages quickly:
;;
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

(use-package embark-consult
  :ensure t
  :after (embark consult))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Use common keystrokes by default
;;(cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
;; (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
;;   (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :init
  (use-package spacemacs-theme
    :ensure t)

  (use-package monokai-theme
    :ensure t)

  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; (load-theme 'doom-one t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)

    ;; or for treemacs users
    ;; (setq doom-themes-treemacs-theme "doom-wilmersdorf") ; use "doom-colors" for less minimal icon theme
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)

    ;; Load theme
    ;; Low contrast
    ;; (load-theme 'doom-wilmersdorf t)
    (load-theme 'doom-nord t)

    ;; Classics
    ;; (load-theme 'doom-solarized-dark t)
    ;; (load-theme 'doom-solarized-dark-high-contrast t)
    ;; (load-theme 'doom-tomorrow-night t)
    )

  ;; (use-package monokai-theme
  ;;   :ensure t)
  :config
  ;; (load-theme 'sanityinc-tomorrow-night t)
  ;; (load-theme 'sanityinc-tomorrow-day t)
  ;;(load-theme 'sanityinc-tomorrow-bright)
  ;;(load-theme 'sanityinc-tomorrow-blue)
  ;; (load-theme 'spacemacs-dark t)
  ;; (load-theme 'monokai t)
  ;; (add-hook 'prog-mode-hook (lambda ()
  ;;                             (setq-local global-hl-line-mode
  ;;                                         nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
;;(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
;(load-file (expand-file-name "extras/org.el" user-emacs-directory))

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-interval 2)
 '(auto-save-visited-mode t)
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "eab123a5ed21463c780e17fc44f9ffc3e501655b966729a2d5a2072832abd3ac" default))
 '(package-selected-packages
   '(symbol-overlay mmm-mode web-mode py-isort blacken lsp-pyright flycheck hippie-expand lsp-ui lsp-imenu lsp-mode flymake-ruff elpy prism projectile auto-virtualenv beacon diff-hl jenkinsfile-mode groovy-mode deadgrep real-auto-save jinja2-mode chatgpt-shell all-the-icons-dired nerd-icons-dired nerd-icons color-theme-sanityinc-tomorrow spacemacs-theme spaceline spaceline-config all-the-icons move-text highlight-indentation highlight-indentation-mode sublimity dashboard treesit-auto aggressive-indent aggressive-indent-mode zoom selected region-bindings-mode smartparens expand-region multiple-cursors drag-stuff wgrep orderless kind-icon cape corfu-terminal corfu marginalia vertico embark-consult embark consult avy json-mode yaml-mode magit which-key))
 '(zoom-mode t nil (zoom)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   USER Customizations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'yes-or-no-p 'y-or-n-p)
(setq delete-by-moving-to-trash t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load custom-file))
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq scroll-error-top-bottom t) ;; Page up and down goes to begging or end of buffer

(use-package treesit-auto
  :disabled t
  :ensure t
  :config
  (global-treesit-auto-mode))

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)


;;#############################
;; Defuns
;;#############################
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun mw/set-best-font (fonts)
  (when fonts
    (let* ((fontname (car (car fonts)))
           (fontsize (car (last (car fonts))))
           (fontstring (format "%s-%d" fontname fontsize)))
      (if (member fontname (font-family-list)) (set-frame-font fontstring)
        (mw/set-best-font (cdr fonts))))))

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))


;;;; Editing

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(defun sp-kill-sexp-with-a-twist-of-lime ()
  (interactive)
  (if (sp-point-in-string)
      (let ((end (plist-get (sp-get-string) :end)))
        (kill-region (point) (1- end)))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (if (or (comment-only-p beg end)
              (s-matches? "\\s+" (buffer-substring-no-properties beg end)))
          (kill-line)
        (sp-kill-sexp)))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (cond ((or (eq major-mode 'coffee-mode) (eq major-mode 'feature-mode))
         (let ((column
                (save-excursion
                  (back-to-indentation)
                  (current-column))))
           (move-end-of-line 1)
           (newline)
           (move-to-column column t)))
        (t
         (move-end-of-line 1)
         (newline)
         (indent-according-to-mode))))

(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (cond ((or (eq major-mode 'coffee-mode) (eq major-mode 'feature-mode))
         (let ((column
                (save-excursion
                  (back-to-indentation)
                  (current-column))))
           (move-beginning-of-line 1)
           (newline)
           (forward-line -1)
           (move-to-column column t)))
        (t
         (move-beginning-of-line 1)
         (newline)
         (forward-line -1)
         (indent-according-to-mode))))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (unless (or (eq major-mode 'coffee-mode)
                (eq major-mode 'feature-mode))
      (untabify (region-beginning) (region-end))
      (indent-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(defun kill-region-or-backward-word ()
  "kill region if active, otherwise kill backward word"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun url-decode-region (start end)
  "URL decode a region."
  (interactive "r")
  (save-excursion
    (let ((text (url-unhex-string (buffer-substring start end))))
      (delete-region start end)
      (insert text))))

(defun url-encode-region (start end)
  "URL encode a region."
  (interactive "r")
  (save-excursion
    (let ((text (url-hexify-string (buffer-substring start end))))
      (delete-region start end)
      (insert text))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line
number input."
  (interactive)
  (let ((line-numbers-off-p (if (boundp 'linum-mode)
                                (not linum-mode)
                              t)))
    (unwind-protect
        (progn
          (when line-numbers-off-p
            (linum-mode 1))
          (call-interactively 'goto-line))
      (when line-numbers-off-p
        (linum-mode -1))))
  (save-excursion
    (hs-show-block)))

(defun back-to-indentation-or-beginning-of-line ()
  "Move point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun find-project-root (dir)
  "Find project root directory"
  (f--traverse-upwards (f-dir? (f-expand ".git" it)) dir))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun re-builder-large ()
  "Just like `re-builder', only make the font and window larger."
  (interactive)

  (re-builder)
  (text-scale-increase 5)
  (set-window-text-height (selected-window) 7))

(defun ipython ()
  (interactive)
  (ansi-term "/usr/bin/ipython"))

(defun google ()
  "Search Googles with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun etc-log-tail-handler ()
  "Clean auto-revert-tail mode"
  (end-of-buffer)
  (make-variable-buffer-local 'auto-revert-interval)
  (setq auto-revert-interval 1)
  (auto-revert-set-timer)
  (make-variable-buffer-local 'auto-revert-verbose)
  (setq auto-revert-verbose nil)
  (read-only-mode t)
  (font-lock-mode 0)
  (when (fboundp 'show-smartparens-mode)
    (show-smartparens-mode 0)))


;;;; Marcwebbie

(defun mw/spell-dictionary (choice)
  "Switch between language dictionaries."
  (interactive "cChoose:  (1) English | (2) French | (3) Portuguese")
  (let ((lang (cond
               ((eq choice ?1) "en")
               ((eq choice ?2) "fr")
               ((eq choice ?3) "pt_BR"))))
    (if lang (progn
               (message (format "Chosen dictionary: %s" lang))
               (ispell-change-dictionary lang)
               ;; (setq ispell-dictionary lang)
               (flyspell-buffer)
               )
      (message "Not a valid choice"))))

(defun mw/buffer-django-p ()
  "Test if in a django template buffer"
  (save-excursion
    (search-forward-regexp "{% base\\|{% if\\|{% include\\|{% block"
                           nil
                           t)))

(defun mw/insert-lambda-char ()
  (interactive)
  (insert "λ"))

(defun mw/set-presentation-font ()
  (interactive)
  (set-frame-font "Monaco-40"))


;;#############################
;; Python
;;#############################
(defun mw/python--add-todo-fixme-bug-hightlight ()
  "Adds a highlighter for use by FIXME, TODO, BUG comment"
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

(defun mw/python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "import i?pu?db; +i?pu?db.set_trace().*$" 'hi-red-b))

(defun mw/python--add-pdb-breakpoint ()
  "Add pdb.set_trace() code and move line down"
  (interactive)
  (insert "import pdb; pdb.set_trace() # fmt: skip"))

(defun mw/python--add-pudb-breakpoint ()
  "Add pudb.set_trace() code and move line down"
  (interactive)
  (insert "import pudb; pudb.set_trace() # fmt: skip"))

(defun mw/python--add-ipdb-breakpoint ()
  "Add ipdb.set_trace() code and move line down"
  (interactive)
  (insert "import ipdb; ipdb.set_trace() # fmt: skip"))

(defun mw/python--remove-breakpoints ()
  "Remove line with a pdb/pudb/ipdb breakpoint"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "import i?pu?db; +i?pu?db.set_trace().*$")))

(defun mw/set-django-settings-module (django-settings-module)
  "set django settings module environment variable"
  (interactive "sDJANGO_SETTINGS_MODULE: ")
  (setenv "DJANGO_SETTINGS_MODULE" django-settings-module t))

(defun mw/set-elpy-test-runners ()
  "Set elpy test runners"
  (let ((python (executable-find "python")))
    (setq elpy-test-django-runner-command (list python "manage.py" "test" "--noinput"))
    (setq elpy-test-discover-runner-command (list python "-m" "unittest"))))

(defun mw/clean-python-file-hook ()
  "Clean python buffer before saving"
  (interactive)
  (progn
    (if (and (which "autopep8") (symbolp 'elpy-autopep8-fix-code))
        (elpy-autopep8-fix-code))
    (if (symbolp 'elpy-importmagic-fixup)
        (elpy-importmagic-fixup))))

(defun first-file-exists-p (filelist)
  (let ((filename (expand-file-name (car filelist))))
    (if (file-exists-p filename) filename (first-file-exists-p (cdr filelist)))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


(defun desktop-notify (title message icon)
  "Show a message with `terminal-notifier-command`."
  (shell-command
   (format "%s -title %s -message %s -sender org.gnu.Emacs -appIcon %s" (executable-find "terminal-notifier") title message icon)))


(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))


(defun my-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))

;;#############################
;; Interface
;;#############################
(defun mw/set-font ()
  "Set a default font based on available fonts, size, and display type."
  (interactive)
  (let* ((preferred-fonts '("Fira Code" "JetBrains Mono"  "Source Code Pro" "Hack"  "Iosevka" "Monaco" "Ubuntu Mono" "DejaVu Sans Mono"))
         (font-size (if (eq system-type 'darwin) 140 120))
         (chosen-font (seq-find (lambda (font) (member font (font-family-list))) preferred-fonts)))
    (when chosen-font
      (set-face-attribute 'default nil :family chosen-font :height font-size)
      (message "Font set to %s, size %d" chosen-font (/ font-size 10)))))
(mw/set-font)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(save-place-mode 1)
(setq next-line-add-newlines t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package sublimity
  :disabled t
  :ensure t
  :config
  (sublimity-mode 1))

(use-package zoom
  :ensure t
  :init
  (custom-set-variables '(zoom-mode t))
  :config
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode 1))

(use-package highlight-indentation
  :ensure t
  :config (highlight-indentation-mode t))

(use-package nerd-icons
  :ensure t
  :config
  (use-package nerd-icons-dired
    :ensure t
    :hook
    (dired-mode . nerd-icons-dired-mode)))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 24)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-time-analogue-clock nil)
  (setq doom-modeline-time 0)
  (display-time-mode 0)
  )

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode t)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-hydra)
  )

;;#############################
;; Editing
;;#############################
(delete-selection-mode 1) ;; Always replace when yanking
(global-set-key (kbd "C-c d")
                'duplicate-current-line-or-region)


;; (global-set-key (kbd "C-M ;")
;;                 'comment-or-uncomment-region)


(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-w" . mc/mark-all-words-like-this)))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (defun er/add-python-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append er/try-expand-list
                                     '(er/mark-defun))))
  (add-hook 'python-mode-hook 'er/add-python-mode-expansions)
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
  (bind-key "P" 'mc/skip-to-previous-like-this region-bindings-mode-map)
  (bind-key "n" 'mc/mark-next-like-this region-bindings-mode-map)
  (bind-key "N" 'mc/skip-to-next-like-this region-bindings-mode-map)

  ;; expand-regions
  (bind-key "c" 'er/mark-outer-python-block region-bindings-mode-map)
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
  (global-subword-mode 1))

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
  (require 'smartparens-config)
  )

(use-package move-text
  :ensure t
  :config
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))


;;#############################
;; LANGUAGE MODES
;;#############################
(use-package jinja2-mode
  :ensure t
  :mode "\\.j2\\'"
  )

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'"
  :config
  (use-package jenkinsfile-mode
    :ensure t
    :mode "\\.jenkinsfile\\'")
  )

;;#############################
;; VERSION CONTROL
;;#############################
(use-package diff-hl
  :ensure t
  :bind
  (("C-c v r" . diff-hl-revert-hunk)
   ("C-c v s" . diff-hl-stage-current-hunk)
   ("C-c v n" . diff-hl-next-hunk)
   ("C-c v p" . diff-hl-previous-hunk))
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;#############################
;; NAVIGATION
;;#############################
(keymap-global-set "C-M-;" 'comment-or-uncoment-region)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package avy
  :ensure t
  :bind
  (("C-c SPC" . avy-goto-char)
   ))

(use-package deadgrep
  :ensure t
  :bind
  ("C-c s" . deadgrep))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  :config
  (projectile-mode +1))

(use-package prism
  :ensure t
  :config
  (prism-mode +1))

(use-package consult
  :bind (("C-s" . consult-line)("C-c C-j" . consult-imenu)("M-i" . consult-imenu)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package symbol-overlay
  :ensure t
  :bind (("M-[" . symbol-overlay-jump-prev)
         ("M-]" . symbol-overlay-jump-next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Development Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Black - Python code formatter
(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-fast-unsafe t)  ;; Enable faster, potentially less safe formatting
  (blacken-allow-py36 t)
  :config

  )  ;; Support Python 3.6 syntax

;; Isort - Import sorter
(use-package py-isort
  :ensure t
  :hook (before-save . py-isort-before-save)
  :custom
  (py-isort-options '("--profile" "black"))) ;; Align with Black's style

;; Auto-Virtualenv - Automatically activate virtual environments
(use-package auto-virtualenv
  :load-path "~/Projects/auto-virtualenv/"
  :hook ((python-mode . auto-virtualenv-find-and-activate)
         (projectile-after-switch-project . auto-virtualenv-find-and-activate))
  :config
  (setq auto-virtualenv-verbose t))

;; LSP and Pyright for Python
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((python-mode . lsp)
         (typescript-mode . lsp))
  :init
  (setq lsp-completion-provider :capf      ;; Use capf for Corfu
        lsp-prefer-capf t                  ;; Prefer capf completions
        lsp-idle-delay 0.5                 ;; Reduce LSP latency
        lsp-headerline-breadcrumb-enable nil) ;; Disable header breadcrumbs
  :config
  (use-package lsp-pyright
    :ensure t
    :after lsp-mode
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp)))
    :custom
    (setq lsp-disabled-clients '(ruff))  ;; Disable Ruff LSP server
    (lsp-pyright-auto-import-completions t) ;; Enable auto-import suggestions
    (lsp-pyright-disable-organize-imports t)
    (lsp-pyright-enable-ruff nil)           ;; Disable Ruff via Pyright
    (lsp-pyright-multi-root nil)))          ;; Use single workspace for Pyright

;; Corfu - Completion framework
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                  ;; Enable auto-completion
  (corfu-auto-delay 0.2)          ;; Short delay before suggestions
  (corfu-cycle t)                 ;; Allow cycling through completions
  (corfu-auto-prefix 2)           ;; Trigger after 2 characters
  (corfu-preview-current nil)     ;; Disable preview of current candidate
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("S-TAB" . corfu-previous)))

;; Flycheck - Real-time linting
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-idle-change-delay 0.8)
  :bind
  (("C-c f r" . flycheck-verify-setup)
   ("C-c f s" . flycheck-select-checker)
   ("C-c f n" . flycheck-next-error)
   ("C-c f p" . flycheck-previous-error)))

;; Python-Specific Settings
(use-package python
  :ensure t
  :config
  (setq python-indent-offset 4) ;; Use 4 spaces for Python indentation
  (setq python-shell-interpreter "python3"))

(use-package tdd-mode
  :load-path "~/Projects/tdd-mode"
  :hook (python-mode . tdd-mode)
  :bind-keymap ("C-c t" . tdd-mode-command-map)
  :config
  (setq tdd-mode-test-runner 'pytest        ;; Use pytest as the test runner
        tdd-mode-notify-on-pass t           ;; Notify on passing tests
        tdd-mode-notify-on-fail t           ;; Notify on failing tests
        tdd-mode-auto-run-on-save t))       ;; Automatically run tests on save

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other languages and modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ansible
  :ensure t
  :defer t
  :hook
  ((yaml-mode . ansible-auto-enable)
   (ansible . ansible-doc-mode))
  :config
  ;; Automatically activate ansible-mode for playbooks and roles
  (setq ansible::section-face '(:foreground "goldenrod" :weight bold)))

(use-package ansible-doc
  :ensure t
  :defer t
  :after ansible
  :commands ansible-doc
  :bind
  (:map ansible-doc-mode-map
        ("C-c C-d" . ansible-doc))) ;; Bind ansible-doc for easy access to documentation

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package vcl-mode
  :ensure t
  :mode ("\\.vcl\\.j2\\'" . vcl-mode))

(use-package nginx-mode
  :ensure t
  :mode ("\\.conf\\.j2\\'" . nginx-mode))

(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (setq yaml-indent-offset 2))

(use-package ansible-vault
  :ensure t
  :hook (yaml-mode . ansible-vault-mode) ; Automatically enable in yaml-mode
  :config
  (setq ansible-vault-password-file "~/.ansible/vault_pass"))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))


(use-package bind-key
  :ensure s
  :init
  (use-package s
    :ensure t)
  :config
  ;; Editing
  (bind-key "C-c d" 'duplicate-current-line-or-region)
  (bind-key "C-j" 'newline-and-indent)
  (bind-key "C-c n" 'clean-up-buffer-or-region)
  (bind-key "C-M-;" 'comment-or-uncomment-current-line-or-region)
  (bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
  (bind-key "C-c q" 'query-replace)
  (bind-key "C-z" 'zap-up-to-char)
  (bind-key "M-j" (λ (join-line -1)))

  ;; Buffer/Files
  (bind-key "C-c R" 'rename-this-buffer-and-file)
  (bind-key "C-c D" 'delete-this-buffer-and-file)

  ;; Naming
  (bind-key "C-c m -" (lambda () (interactive) (replace-region-by 's-dashed-words)))
  (bind-key "C-c m _" (lambda () (interactive) (replace-region-by 's-snake-case)))
  (bind-key "C-c m c" (lambda () (interactive) (replace-region-by 's-lower-camel-case)))
  (bind-key "C-c m C" (lambda () (interactive) (replace-region-by 's-upper-camel-case)))

  ;; Number
  (bind-key "C-c =" 'my-increment-number-decimal)
  (bind-key "C-c -" 'my-decrement-number-decimal)
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (defun my-magit-auto-branch-name-to-commit-message ()
    "Automatically insert the branch's ticket number as a prefix in the commit message."
    (interactive)
    (let* ((branch-name (magit-get-current-branch))
           ;; Regex to capture the ticket number in branch names like "scien-2828-..."
           (ticket-number (and branch-name
                               (string-match "\\([0-9]+\\)" branch-name)
                               (match-string 1 branch-name))))
      (when ticket-number
        ;; Move to the beginning of the buffer and insert the formatted ticket number
        (goto-char (point-min))
        (insert (format "[%s] " ticket-number)))))

  (add-hook 'git-commit-setup-hook 'my-magit-auto-branch-name-to-commit-message)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc")
  (define-key markdown-mode-map (kbd "M-p") 'move-text-up)
  (define-key markdown-mode-map (kbd "M-n") 'move-text-down))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1) ;; Enable globally
  :config
  (setq git-gutter:update-interval 2) ;; Update every 2 seconds
  :bind (("C-x v =" . git-gutter:popup-hunk)  ;; Show changes for the current hunk
         ("C-x v r" . git-gutter:revert-hunk) ;; Revert the current hunk
         ("C-x v n" . git-gutter:next-hunk)   ;; Go to the next hunk
         ("C-x v p" . git-gutter:previous-hunk))) ;; Go to the previous hunk  ;; Stage changes in hunk ;; Go to the previous hunk


(use-package conf-mode
  :mode (("\\.env\\..*\\'" . conf-mode) ; Match files like .env.production, .env.test
         ("\\.env\\'" . conf-mode))    ; Match .env files
  )

;; Ledger-mode Configuration
(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode) ; Automatically activate for .ledger files
  :custom
  ;; General settings
  (ledger-highlight-xact-under-point t)       ; Highlight transaction under point
  (ledger-clear-whole-transactions t)        ; Clear entire transactions
  (ledger-post-amount-alignment-column 65)   ; Align amounts to column 52
  (ledger-reconcile-default-commodity "$")   ; Default commodity
  (ledger-reconcile-finish-force-quit t)     ; Close reconcile buffer after finishing

  ;; Faces customization
  (ledger-font-cleared-face '((t :foreground "green" :weight bold)))
  (ledger-font-pending-face '((t :foreground "orange" :weight bold)))
  (ledger-font-uncleared-face '((t :foreground "red" :weight bold)))
  (ledger-font-comment-face '((t :foreground "gray" :italic t)))

  ;; Reports customization
  (ledger-reports
   '(("balance" "ledger -f %(ledger-file) bal")
     ("register" "ledger -f %(ledger-file) reg")
     ("expenses by month" "ledger -f %(ledger-file) reg ^Expenses -M")
     ("income statement" "ledger -f %(ledger-file) -M bal ^Income ^Expenses")
     ("net worth" "ledger -f %(ledger-file) bal Assets Liabilities")))

  ;; Hooks
  :hook
  (ledger-mode . hl-line-mode)               ; Highlight current line
  (ledger-mode . visual-line-mode)           ; Enable line wrapping
  (ledger-mode . display-line-numbers-mode)  ; Enable line numbers

  ;; Keybindings
  :bind
  (:map ledger-mode-map
        ("C-c C-r" . ledger-reconcile)              ; Reconcile account
        ("C-c C-a" . ledger-add-transaction)        ; Add a transaction
        ("C-c C-d" . ledger-delete-current-transaction) ; Delete transaction
        ("C-c C-o C-r" . ledger-report)             ; Run a report
        ("C-c C-p" . ledger-display-balance-at-point) ; Quick balance display
        ("C-c C-f" . ledger-narrow-to-regex)        ; Narrow transactions
        ("C-c C-g" . ledger-occur-refresh)          ; Refresh occur view
        ("C-c C-k" . ledger-copy-transaction-at-point))) ; Copy transaction

(use-package request
  :ensure t
  :bind
  (("C-c g" . google-search-open)
   ("C-c G" . duckduckgo-search-best-match)
   ("C-c l" . open-business-directory))
  :config
  (defun duckduckgo-search-best-match (query)
    "Perform a DuckDuckGo search for QUERY and display the best match in the minibuffer.
If a region is active, use it as the query. Otherwise, use the word at point."
    (interactive
     (list (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (thing-at-point 'word t))))
    (let ((url (format "https://api.duckduckgo.com/?q=%s&format=json&pretty=1"
                       (url-hexify-string query))))
      (request
        url
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (let ((abstract (alist-get 'Abstract data))
                          (url (alist-get 'AbstractURL data)))
                      (if (and abstract url)
                          (message "Best match: %s (%s)" abstract url)
                        (message "No results found.")))))
        :error (cl-function (lambda (&rest _args)
                              (message "Error retrieving search results."))))))

  (defun google-search-open (query)
    "Perform a Google search for QUERY and open the result in the default browser.
If a region is active, use it as the query. Otherwise, use the word at point."
    (interactive
     (list (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (thing-at-point 'word t))))
    (browse-url (format "https://www.google.com/search?q=%s" (url-hexify-string query))))

  (defun open-business-directory (query)
    "Search for QUERY in the French business directory and open the result in the default browser.
If a region is active, use it as the query. Otherwise, use the word at point."
    (interactive
     (list (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (thing-at-point 'word t))))
    (browse-url
     (format "https://annuaire-entreprises.data.gouv.fr/rechercher?terme=%s&cp_dep_label=&cp_dep_type=&cp_dep=&fn=&n=&dmin=&dmax=&type=&label=&etat=&sap=&naf=&nature_juridique=&tranche_effectif_salarie=&categorie_entreprise="
             (url-hexify-string query)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artificial Intelligence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gptel
  :ensure t
  :config
  ;; Set up Ollama backend with mistral-nemo:latest
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"       ;; Default Ollama host
          :stream t                    ;; Enable streaming responses
          :models '(mistral-nemo:latest))) ;; Use mistral-nemo:latest model

  ;; Set default model to mistral-nemo:latest
  (setq gptel-model 'mistral-nemo:latest)

  ;; Optional: Configure token limits and temperature
  (setq gptel-max-tokens 1000
        gptel-temperature 0.7)

  ;; Optional: Set hooks for better user experience
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll) ;; Auto-scroll responses
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)) ;; Move cursor to end of response


(use-package smart-prompt
  :load-path "~/Projects/smart-prompt/"
  ;; :hook (python-mode . smart-prompt-mode)
  :custom
  (smart-prompt-keymap-prefix "C-c c") ;; Change this to "C-c g" if needed
  (smart-prompt-context-header "Prompt Context")
  :config
  ;; (smart-prompt-setup-keymap)
  (smart-prompt-global-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript and Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode "\\.tsx\\'"
  :hook (web-mode . lsp)
  :config
  (setq web-mode-enable-auto-quoting nil) ; prevents unexpected quote insertion
  (setq web-mode-code-indent-offset 2))

(use-package prettier
  :hook ((typescript-mode . prettier-mode)
         (web-mode . prettier-mode)))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-node)
  (dap-node-setup)) ;; Installs Node debug adapter


;; End
