;;; tdd-mode.el --- TDD Mode for running Python tests automatically -*- lexical-binding: t; -*-

;; Author: Marcwebbie <marcwebbie@gmail.com>
;; Version: 1.0
;; Keywords: python, testing, tdd
;; Package-Requires: ((emacs "24.3") (ansi-color "3.0"))

;;; Commentary:
;;
;; TDD Mode enables a streamlined workflow for running tests within Python files.
;; Users can run the test at point, or a global test command, and automatically
;; re-run the last test upon saving any buffer in the project.
;;
;; Supports `pytest`, `nosetests`, and `django test` with color-coded output.
;; Notifications indicate success or failure status after each test run.
;;
;; Usage:
;; - Activate `tdd-mode` in a Python test file (usually with a test_ prefix).
;; - Use `C-c t` to run the test at point or `C-c T` for a custom/global test.
;; - Tests re-run automatically on save, with success/failure notifications.

;;; Code:

(require 'ansi-color)

(defvar tdd-mode-test-buffer "*tdd-output*"
  "Buffer name for displaying test output.")

(defvar tdd-mode-last-test-command nil
  "The last test command run by the user.")

(defvar tdd-mode-test-runner 'pytest
  "The test runner to use. Options are 'pytest, 'nosetests, and 'django.")

(defun tdd-mode-get-test-command-at-point ()
  "Generate the test command based on the test runner and function at point."
  (let ((file-name (buffer-file-name))
        (function-name (which-function))
        (runner tdd-mode-test-runner))
    (cond
     ((eq runner 'pytest)
      (when (and file-name function-name)
        (format "pytest --color=yes %s::%s" file-name function-name)))
     ((eq runner 'nosetests)
      (when (and file-name function-name)
        (format "nosetests %s:%s" file-name function-name)))
     ((eq runner 'django)
      (when file-name
        (format "python manage.py test %s" file-name)))
     (t (error "Unsupported test runner")))))

(defun tdd-mode-run-test (&optional command)
  "Run the given test COMMAND or the last test if no COMMAND is provided."
  (interactive)
  (let ((test-command (or command tdd-mode-last-test-command)))
    (when test-command
      (setq tdd-mode-last-test-command test-command) ; Save last test command
      (with-current-buffer (get-buffer-create tdd-mode-test-buffer)
        (erase-buffer)
        (insert (concat "$ " test-command "\n\n"))
        (let ((exit-code (call-process-shell-command test-command nil tdd-mode-test-buffer t)))
          (tdd-mode-apply-ansi-color) ;; Apply ANSI colors to buffer
          (display-buffer tdd-mode-test-buffer)
          (tdd-mode-notify exit-code))))))

(defun tdd-mode-run-test-at-point ()
  "Run the test at point and save it as the last command."
  (interactive)
  (let ((command (tdd-mode-get-test-command-at-point)))
    (when command
      (tdd-mode-run-test command))))

(defun tdd-mode-apply-ansi-color ()
  "Apply ANSI color codes in the test buffer for improved readability."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun tdd-mode-notify (exit-code)
  "Send a notification based on the exit code of the test."
  (if (eq exit-code 0)
      (message "✅ Test passed!")
    (message "❌ Test failed!")))

(defun tdd-mode-run-last-test-on-save ()
  "Re-run the last test command on buffer save."
  (when tdd-mode-last-test-command
    (tdd-mode-run-test tdd-mode-last-test-command)))

(defun tdd-mode-setup ()
  "Set up TDD mode with automatic re-running of the last test on save."
  (add-hook 'after-save-hook 'tdd-mode-run-last-test-on-save nil t))

;;;###autoload
(define-minor-mode tdd-mode
  "Minor mode for TDD workflow with automatic test re-running."
  :lighter " TDD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") 'tdd-mode-run-test-at-point)
            (define-key map (kbd "C-c T") 'tdd-mode-run-test)
            map)
  (if tdd-mode
      (tdd-mode-setup)
    (remove-hook 'after-save-hook 'tdd-mode-run-last-test-on-save t)))

(provide 'tdd-mode)

;;; tdd-mode.el ends here
