(defun pelican-timestamp-now ()
  "Generate a Pelican-compatible timestamp."
  (format-time-string "%Y-%m-%d %H:%M"))

(defun pelican-find-in-parents (file-name)
  "Find FILE-NAME in the default directory or one of its parents, or nil."
  (let* ((parent (expand-file-name default-directory)))
    (while (and (not (file-readable-p (concat parent file-name)))
                (not (string= parent (directory-file-name parent))))
      (setq parent (file-name-directory (directory-file-name parent))))
    (let ((found (concat parent file-name)))
      (if (file-readable-p found) found nil))))

(defun pelican-find-root ()
  "Return the root of the buffer's Pelican site, or nil."
  (let ((conf (pelican-find-in-parents "pelicanconf.py")))
    (if conf (file-name-directory conf))))

(defun pelican-conf-path ()
  "Return pelicanconf.py path"
  (let ((conf (pelican-find-in-parents "pelicanconf.py")))
    (if conf conf)))

(defun pelican-pelicanconf-var (var)
  (let ((cmd (format "cd %s && python -c 'from pelicanconf import *; print(%s)'" (pelican-find-root) var)))
    (string-trim-right (shell-command-to-string cmd))))

(defun pelican-publishconf-var (var)
  (let ((cmd (format "cd %s && python -c 'from publishconf import *; print(%s)'" (pelican-find-root) var)))
    (string-trim-right (shell-command-to-string cmd))))

;; ========================
;; Make
;; ========================

(defun pelican-make (target)
  "Execute TARGET in a Makefile at the root of the site."
  (interactive "sMake Pelican target: ")
  (let ((default-directory (pelican-find-root)))
    (if default-directory
        (let ((output (get-buffer-create "*Pelican Output*")))
          (display-buffer output)
          (pop-to-buffer output)
          (compilation-mode)
          (start-process "Pelican Makefile" output "make" target))
      (message "This doesn't look like a Pelican site."))))

(defun pelican-make-html ()
  "Generate HTML via a Makefile at the root of the site."
  (interactive)
  (pelican-make "html"))

(defun pelican-make-publish ()
  "Generate HTML via a Makefile at the root of the site."
  (interactive)
  (pelican-make "html"))

;; ========================
;; Define mode
;; ========================

;;;###autoload
(define-minor-mode pelican-mode
  "Toggle mode"
  :lighter " Pelican")

(provide 'pelican-mode)
