;;; Commentary:

;; Auto Virtualenv is an Emacs package that automatically activates
;; Python virtual environments based on the project you are working on.
;; This eliminates the need to manually activate your virtualenv each time
;; you switch projects, making your workflow smoother and more efficient.

;; Installation:
;; To install auto-virtualenv, add the following lines to your Emacs
;; configuration file (init.el or .emacs):

;; (use-package auto-virtualenv
;;   :ensure t
;;   :config
;;   (auto-virtualenv-activate-virtualenv))

;; Configuration:
;; The package requires the following settings to function properly:

;; - `auto-virtualenv-virtualenvs-root`: Specify directories where your
;;   virtual environments are located. This is a list of directories
;;   to scan for virtualenvs.
;; - `auto-virtualenv-project-root-files`: Define files that indicate
;;   a project root (e.g., ".git", ".projectile").

;; Hook Setup:
;; To automatically activate your virtual environment when you switch
;; projects or save files, you can set up the following hooks:

;; 1. Activate virtualenv after switching projects with Projectile:
;; This hook activates the virtual environment whenever you switch
;; projects using Projectile.

(add-hook 'projectile-after-switch-project-hook
          'auto-virtualenv-activate-virtualenv)

;; 2. Activate virtualenv after saving any file:
;; This hook activates the virtual environment whenever you save a file.
;; Be cautious with this setting, as it may not be desired for all file types.

(add-hook 'after-save-hook
          'auto-virtualenv-activate-virtualenv)

;; Usage:
;; - Simply open a Python project that contains a recognized virtualenv
;;   (located in one of the specified directories).
;; - The virtualenv will be automatically activated based on the project's
;;   configuration.
;; - You can also check the *Messages* buffer to verify that the
;;   correct virtualenv is activated, and to see the output of the
;;   `which python` command.

;; To deactivate the virtual environment, you can use the `pyvenv-deactivate`
;; command, or simply close the buffer or Emacs session.

;; Enjoy a streamlined Python development experience with automatic
;; virtual environment activation!

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
