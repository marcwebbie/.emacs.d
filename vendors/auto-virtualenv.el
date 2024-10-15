;;; auto-virtualenv.el --- Auto activate Python virtualenvs

;; Copyright (C) 2017-2024 Marcwebbie

;; Author: Marcwebbie <marcwebbie@gmail.com>
;; URL: http://github.com/marcwebbie/auto-virtualenv
;; Version: 1.5.0
;; Keywords: Python, Virtualenv, Tools
;; Package-Requires: ((cl-lib "0.5") (pyvenv "1.9") (s "1.10.0"))

;;; Commentary:
;; Auto Virtualenv is an Emacs package that automatically activates
;; Python virtual environments based on the project you are working on.

;;; Code:

(require 'pyvenv)
(require 'cl-lib)

(defgroup auto-virtualenv nil
  "Automatically activate Python virtual environments."
  :group 'python)

(defcustom auto-virtualenv-virtualenvs-root
  '("~/.virtualenvs/" "~/.pyenv/versions/")
  "Directories where virtual environments are stored."
  :type '(repeat string)
  :group 'auto-virtualenv)

(defcustom auto-virtualenv-project-root-files
  '(".python-version" "manage.py" ".git")
  "Files that indicate the root of a Python project."
  :type '(repeat string)
  :group 'auto-virtualenv)

(defcustom auto-virtualenv-verbose nil
  "Enable verbose output for debugging."
  :type 'boolean
  :group 'auto-virtualenv)

(defvar auto-virtualenv-current-virtualenv nil
  "The currently activated virtual environment.")

(defvar auto-virtualenv-mode-line nil
  "String to display in the mode line for the active virtual environment.")

(defun auto-virtualenv-update-mode-line ()
  "Update the mode line to show the active virtual environment."
  (setq auto-virtualenv-mode-line
        (if auto-virtualenv-current-virtualenv
            (format " Venv: %s" (file-name-nondirectory auto-virtualenv-current-virtualenv))
          " Venv: None"))
  (force-mode-line-update))

(defun auto-virtualenv-find-project-root ()
  "Find the project root by searching for common project files."
  (let ((project-root (locate-dominating-file default-directory
                                              (lambda (dir)
                                                (cl-some (lambda (file)
                                                           (file-exists-p (expand-file-name file dir)))
                                                         auto-virtualenv-project-root-files)))))
    (when project-root
      (message "Project root found: %s" project-root)
      project-root)))

(defun auto-virtualenv-find-and-activate ()
  "Find and activate the virtual environment based on the project root."
  (let* ((project-root (auto-virtualenv-find-project-root))
         (env-name (and project-root (file-name-nondirectory (directory-file-name project-root)))))
    (when env-name
      (dolist (root auto-virtualenv-virtualenvs-root)
        (let ((virtualenv-path (expand-file-name env-name root)))
          (when (file-exists-p virtualenv-path)
            (setq auto-virtualenv-current-virtualenv virtualenv-path)
            (pyvenv-activate virtualenv-path)
            (auto-virtualenv-update-mode-line)
            (when auto-virtualenv-verbose
              (message "Activated virtualenv: %s" virtualenv-path))
            (message "Using Python: %s" (shell-command-to-string "which python")) ; Show the active Python path
            ;; Exit the loop after activating
            (setq done t)))))
    ;; Check if we activated a virtualenv, otherwise notify
    (unless auto-virtualenv-current-virtualenv
      (when auto-virtualenv-verbose
        (message "No virtualenv found for project: %s" project-root)))))

(defun auto-virtualenv-activate ()
  "Activate virtualenv automatically."
  (message "Activating virtualenv...")
  (unless auto-virtualenv-current-virtualenv
    (auto-virtualenv-find-and-activate)))

(add-hook 'find-file-hook #'auto-virtualenv-activate)

;; Add the mode line display
(defun auto-virtualenv-mode-line-update ()
  "Add the virtualenv status to the mode line."
  (setq global-mode-string (list auto-virtualenv-mode-line)))

(add-hook 'after-change-major-mode-hook #'auto-virtualenv-mode-line-update)

(provide 'auto-virtualenv)

;;; auto-virtualenv.el ends here
