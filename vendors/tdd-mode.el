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

(defvar tdd-mode-project-root nil
  "Root directory of the project being tracked by `tdd-mode`.")

(defvar tdd-mode-test-runner 'pytest
  "The test runner to use. Options are 'pytest, 'nosetests, and 'django.")

(defvar tdd-mode-notify-on-pass t
  "Whether to show a notification on test pass.")

(defvar tdd-mode-notify-on-fail t
  "Whether to show a notification on test fail.")

(defun tdd-mode-get-test-command-at-point ()
  "Generate the test command for pytest with ClassName::test_function format at point."
  (let* ((file-name (buffer-file-name))
         (function-name (tdd-mode-get-function-name-at-point))
         (class-name (tdd-mode-get-class-name-at-point))
         (runner tdd-mode-test-runner))
    (cond
     ((eq runner 'pytest)
      (when (and file-name function-name)
        (if class-name
            (format "pytest --color=yes %s::%s::%s" file-name class-name function-name)
          (format "pytest --color=yes %s::%s" file-name function-name))))
     ((eq runner 'nosetests)
      (when (and file-name function-name)
        (format "nosetests %s:%s" file-name function-name)))
     ((eq runner 'django)
      (when file-name
        (format "python manage.py test %s" file-name)))
     (t (error "Unsupported test runner")))))

(defun tdd-mode-get-class-name-at-point ()
  "Retrieve the name of the class at point if the cursor is within a Python class."
  (save-excursion
    (let ((class-name nil))
      (beginning-of-defun)
      (while (and (not class-name) (re-search-backward "^\s*class\s+\\([A-Za-z0-9_]+\\)" nil t))
        (setq class-name (match-string 1)))
      class-name)))

(defun tdd-mode-get-function-name-at-point ()
  "Retrieve the name of the function at point if the cursor is within a Python function."
  (save-excursion
    (let ((function-name nil))
      (beginning-of-defun)
      (when (re-search-forward "def \\([a-zA-Z0-9_]+\\)" nil t)
        (setq function-name (match-string 1)))
      function-name)))

(defun tdd-mode-run-test (&optional command)
  "Run the given test COMMAND or the last test if no COMMAND is provided."
  (interactive)
  (let ((test-command (or command tdd-mode-last-test-command)))
    (when test-command
      (setq tdd-mode-last-test-command test-command)
      (with-current-buffer (get-buffer-create tdd-mode-test-buffer)
        (erase-buffer)
        (insert (concat "$ " test-command "\n\n"))
        (let ((exit-code (call-process-shell-command test-command nil tdd-mode-test-buffer t)))
          (tdd-mode-apply-ansi-color)
          (display-buffer tdd-mode-test-buffer)
          (tdd-mode-notify exit-code)
          (tdd-mode-log-last-test exit-code))))))

(defun tdd-mode-apply-ansi-color ()
  "Apply ANSI color codes in the test buffer for improved readability."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun tdd-mode-notify (exit-code)
  "Send a notification based on the EXIT-CODE of the test."
  (let ((message (if (eq exit-code 0) "✅ Test passed!" "❌ Test failed!")))
    (when (or (and (eq exit-code 0) tdd-mode-notify-on-pass)
              (and (not (eq exit-code 0)) tdd-mode-notify-on-fail))
      (notifications-notify
       :title "TDD Mode"
       :body message
       :urgency (if (eq exit-code 0) 'low 'critical)))
    (message message)))

(defun tdd-mode-log-last-test (exit-code)
  "Log the last test command with the EXIT-CODE and timestamp."
  (let ((log-file (concat (tdd-mode-get-project-root) ".tdd-mode-log")))
    (append-to-file (format "[%s] %s - %s\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S")
                            (if (eq exit-code 0) "PASSED" "FAILED")
                            tdd-mode-last-test-command)
                    nil
                    log-file)))

(defun tdd-mode-run-last-test-on-save ()
  "Re-run the last test command if a .py file in the project is saved."
  (when (and tdd-mode-last-test-command
             (tdd-mode-buffer-in-project-p (buffer-file-name)))
    (tdd-mode-run-test tdd-mode-last-test-command)))

(defun tdd-mode-buffer-in-project-p (file)
  "Check if FILE is within the project root."
  (let ((project-root (tdd-mode-get-project-root)))
    (and project-root
         (f-descendant-of-p file project-root)
         (string= (f-ext file) "py"))))

;;;###autoload
(define-minor-mode tdd-mode
  "Enhanced TDD mode for Python development with real-time feedback and notifications."
  :lighter " TDD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") 'tdd-mode-run-test-at-point)
            (define-key map (kbd "C-c T") 'tdd-mode-run-test)
            map)
  (if tdd-mode
      (progn
        (add-hook 'after-save-hook 'tdd-mode-run-last-test-on-save))
    (remove-hook 'after-save-hook 'tdd-mode-run-last-test-on-save)))

(provide 'tdd-mode)

;;; tdd-mode.el ends here
