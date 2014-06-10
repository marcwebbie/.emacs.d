;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(load-theme 'ir-black-mod :no-confirm)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(defun set-best-font ()
  (when (string-equal system-type "darwin")
    (set-frame-font (cond ((member "Monaco" (font-family-list)) "Monaco-14")
                          ((member "Ubuntu Mono" (font-family-list)) "Ubuntu Mono-14")
                          ((member "Inconsolata" (font-family-list)) "Inconsolata-14")
                          ((member "Menlo" (font-family-list)) "Menlo-14"))) t t)
  (when (string-equal system-type "gnu/linux")
    (set-frame-font (cond ((member "Ubuntu Mono" (font-family-list)) "Ubuntu Mono-12")
                          ((member "Inconsolata" (font-family-list)) "Inconsolata-12")
                          ((member "Monaco" (font-family-list)) "Monaco-12")
                          ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono-12")
                          ((member "Monospace" (font-family-list)) "Monospace-12"))) t t))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  ;; Choose best font when in windows system
  (set-best-font))

(setq scroll-margin 10
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;;;
