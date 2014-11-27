;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(load-theme 'solarized-light :no-confirm)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(defun set-best-font ()
  (when (string-equal system-type "darwin")
    (set-frame-font (cond
                     ((member "Inconsolata" (font-family-list)) "Inconsolata-14")
                     ((member "Menlo" (font-family-list)) "Menlo-12")
                     ((member "Ubuntu Mono" (font-family-list)) "Ubuntu Mono-14")
                     ((member "Monaco" (font-family-list)) "Monaco-14")
                     )) t t)
  (when (string-equal system-type "gnu/linux")
    (set-frame-font (cond ((member "Inconsolata" (font-family-list)) "Inconsolata-14")
                          ((member "Ubuntu Mono" (font-family-list)) "Ubuntu Mono-14")
                          ((member "Monaco" (font-family-list)) "Monaco-14")
                          ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono-14")
                          ((member "Monospace" (font-family-list)) "Monospace-14"))) t t))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  ;; Choose best font when in windows system
  (set-best-font))

;; Configure scrolling
(setq scroll-margin 10           ; Drag the point along while scrolling
      scroll-conservatively 1000 ; Never recenter the screen while scrolling
      scroll-error-top-bottom t  ; Move to beg/end of buffer before
                                 ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;;;;
