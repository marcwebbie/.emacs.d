;;; auto-virtualenv.el --- <description>

;; Author: Marcwebbie <marcwebbie@gmail.com>
;; URL: http://github.com/marcwebbie/auto-virtualenv.el
;; Version: 1.0
;; Keywords: Python, Virtualenv, Tools


;;; Code:

(require 'projectile)
(require 'cl-lib)
(require 'python)
(require 'pyvenv)


(defcustom virtualenv-dirs (if (file-exists-p "~/.pyenv/versions") "~/.pyenv/versions" "~/.virtualenvs")
  "The intended virtualenvs installation directory."
  :type 'directory
  :safe #'stringp
  :group 'auto-virtualenv)


(defvar auto-virtualenv-project-roots
  '(".git" ".hg" "Rakefile" "Makefile" "README" "build.xml" ".emacs-project" "Gemfile" ".projectile" "manage.py")
  "The presence of any file/directory in this list indicates a project root.")

(defvar auto-virtualenv--project-root nil
  "Used internally to cache the project root.")
(make-variable-buffer-local 'auto-virtualenv--project-root)

(defvar auto-virtualenv--versions nil
  "Used internally to cache the project root.")
(make-variable-buffer-local 'auto-virtualenv--versions)

(defun auto-virtualenv--project-root ()
  "Return the current project root directory."
  (or auto-virtualenv--project-root
      (setq auto-virtualenv--project-root
            (expand-file-name
             (or (locate-dominating-file default-directory
                                     (lambda (dir)
                                       (cl-intersection
                                        auto-virtualenv-project-roots
                                        (directory-files dir)
                                        :test 'string-equal))) "")))))
(defun auto-virtualenv--project-name ()
  "Return the project project root name"
  (file-name-nondirectory
   (directory-file-name
     (file-name-directory (auto-virtualenv--project-root)))))

(defun auto-virtualenv--versions ()
  (or auto-virtualenv--versions
      (setq auto-virtualenv--project-root
            (directory-files (expand-file-name (getenv "WORKON_HOME"))))))

(defun auto-virtualenv-find-virtualenv-name ()
  (let ((python-version-file (expand-file-name ".python-version" (auto-virtualenv--project-root))))
    (cond ((file-exists-p python-version-file)
           (with-temp-buffer (insert-file-contents python-version-file) (s-trim (buffer-string))))
          ((member (auto-virtualenv--project-name) (auto-virtualenv--versions))
           (auto-virtualenv--project-name)))))

(defun auto-virtualenv-set-virtualenv ()
  (let ((virtualenv-name (auto-virtualenv-find-virtualenv-name)))
    (when (and virtualenv-name (not (equal pyvenv-virtual-env-name (auto-virtualenv--project-name))))
      (message (format "activating virtualenv: %s" virtualenv-name))
      (pyvenv-mode +1)
      (pyvenv-workon virtualenv-name))))

(provide 'auto-virtualenv)

;;; auto-virtualenv.el ends here
