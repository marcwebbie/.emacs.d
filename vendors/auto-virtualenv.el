;;; auto-virtualenv.el --- Automatically activate Python virtualenvs -*- lexical-binding: t; -*-

;; Author: Marcwebbie <marcwebbie@gmail.com>
;; URL: http://github.com/marcwebbie/auto-virtualenv
;; Version: 2.1.0
;; Keywords: Python, Virtualenv, Tools
;; Package-Requires: ((cl-lib "0.5") (projectile "2.3.0"))

;;; Commentary:
;; Auto Virtualenv is an Emacs package that automatically activates
;; Python virtual environments based on the project you are working on.

;;; Code:

(require 'cl-lib)
(require 'projectile)

(defgroup auto-virtualenv nil
  "Automatically activate Python virtual environments."
  :group 'python)

(defcustom auto-virtualenv-global-dirs
  '("~/.virtualenvs/" "~/.pyenv/versions/" "~/.envs/" "~/.conda/" "~/.conda/envs/")
  "List of global directories to search for virtual environments by project name."
  :type '(repeat string)
  :group 'auto-virtualenv)

(defcustom auto-virtualenv-verbose t
  "Enable verbose output for debugging."
  :type 'boolean
  :group 'auto-virtualenv)

(defvar auto-virtualenv-current-virtualenv nil
  "The currently activated virtual environment.")

(defvar auto-virtualenv-mode-line nil
  "String to display in the mode line for the active virtual environment.")

(defun auto-virtualenv--debug (msg &rest args)
  "Print MSG formatted with ARGS if `auto-virtualenv-verbose' is enabled."
  (when auto-virtualenv-verbose
    (message (apply 'format (concat "[auto-virtualenv] " msg) args))))

(defun auto-virtualenv-update-mode-line ()
  "Update the mode line to show the active virtual environment in bold brackets."
  (setq auto-virtualenv-mode-line
        (if auto-virtualenv-current-virtualenv
            (propertize (format "[Venv: %s]" (file-name-nondirectory (directory-file-name auto-virtualenv-current-virtualenv)))
                        'face '(:weight bold :foreground "DeepSkyBlue"))
          (propertize "[No Venv]" 'face '(:weight bold :foreground "Firebrick"))))
  ;; Update global mode line format with the virtual environment status
  (setq global-mode-string (list auto-virtualenv-mode-line))
  (force-mode-line-update))

(defun auto-virtualenv-find-local-venv (project-root)
  "Check for a local virtual environment in PROJECT-ROOT.
Return the path if found, otherwise nil."
  (auto-virtualenv--debug "Searching for local virtualenv in project root: %s" project-root)
  (cl-some (lambda (venv-dir)
             (let ((venv-path (expand-file-name venv-dir project-root)))
               (when (file-directory-p venv-path)
                 (auto-virtualenv--debug "Local virtualenv found at: %s" venv-path)
                 venv-path)))
           '(".virtualenv" ".venv")))

(defun auto-virtualenv-read-python-version (project-root)
  "Read the virtual environment name from the .python-version file in PROJECT-ROOT."
  (let ((version-file (expand-file-name ".python-version" project-root)))
    (when (file-readable-p version-file)
      (auto-virtualenv--debug "Reading virtualenv name from .python-version file: %s" version-file)
      (string-trim (with-temp-buffer
                     (insert-file-contents version-file)
                     (buffer-string))))))

(defun auto-virtualenv-find-global-venv (env-name)
  "Search for a virtual environment with ENV-NAME in global directories."
  (auto-virtualenv--debug "Searching for global virtualenv with name: %s" env-name)
  (cl-some (lambda (dir)
             (let ((venv-path (expand-file-name env-name (expand-file-name dir))))
               (when (file-directory-p venv-path)
                 (auto-virtualenv--debug "Global virtualenv found at: %s" venv-path)
                 venv-path)))
           auto-virtualenv-global-dirs))

(defun auto-virtualenv-activate (venv-path)
  "Activate the virtual environment located at VENV-PATH."
  (auto-virtualenv-deactivate) ;; Deactivate any existing environment
  (setq auto-virtualenv-current-virtualenv (file-name-as-directory venv-path))
  ;; Update exec-path and environment PATH
  (let ((venv-bin (concat auto-virtualenv-current-virtualenv "bin")))
    (setq exec-path (cons venv-bin exec-path))
    (setenv "VIRTUAL_ENV" auto-virtualenv-current-virtualenv)
    (setenv "PATH" (concat venv-bin path-separator (getenv "PATH"))))
  (auto-virtualenv-update-mode-line)
  (auto-virtualenv--debug "Activated virtualenv: %s" auto-virtualenv-current-virtualenv))

(defun auto-virtualenv-deactivate ()
  "Deactivate any current virtual environment."
  (when auto-virtualenv-current-virtualenv
    (let ((venv-bin (concat auto-virtualenv-current-virtualenv "bin")))
      ;; Remove the virtualenv bin directory from exec-path and PATH
      (setq exec-path (delete venv-bin exec-path))
      (setenv "PATH" (mapconcat 'identity (delete venv-bin (split-string (getenv "PATH") path-separator)) path-separator))
      (setenv "VIRTUAL_ENV" nil)
      (auto-virtualenv--debug "Deactivated virtualenv: %s" auto-virtualenv-current-virtualenv)
      (setq auto-virtualenv-current-virtualenv nil)
      (auto-virtualenv-update-mode-line))))

(defun auto-virtualenv-find-and-activate ()
  "Find and activate the virtual environment based on the project root."
  (let* ((project-root (projectile-project-root))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (env-name (or (auto-virtualenv-read-python-version project-root) project-name))
         (venv-path (or (auto-virtualenv-find-local-venv project-root)
                        (auto-virtualenv-find-global-venv env-name))))
    (if venv-path
        (auto-virtualenv-activate venv-path)
      (auto-virtualenv--debug "No virtualenv found for project: %s" project-root))))

(defun auto-virtualenv-auto-activate ()
  "Auto activate virtual environment for the current project."
  (when (and (projectile-project-p)
             (not (equal auto-virtualenv-current-virtualenv (projectile-project-root))))
    (auto-virtualenv-find-and-activate)))

;; Hook functions to automatically activate virtualenv
(add-hook 'find-file-hook #'auto-virtualenv-auto-activate)
(add-hook 'projectile-after-switch-project-hook #'auto-virtualenv-auto-activate)

;; Add the mode line display
(setq-default mode-line-format
              (append mode-line-format
                      '((:eval auto-virtualenv-mode-line))))

(provide 'auto-virtualenv)

;;; auto-virtualenv.el ends here
