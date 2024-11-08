;;; tdd-mode.el --- Enhanced TDD Mode for Python development -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'alert)

(defvar tdd-mode-test-buffer "*tdd-output*"
  "Buffer name for displaying test output.")

(defvar tdd-mode-last-test-command nil
  "The last test command run by the user.")

(defvar tdd-mode-test-runner 'pytest
  "The test runner to use. Options are 'pytest, 'nosetests, and 'django.")

(defvar tdd-mode-notify-on-pass t
  "Whether to show a notification on test pass.")

(defvar tdd-mode-notify-on-fail t
  "Whether to show a notification on test fail.")

(defvar tdd-mode-test-status "✅"
  "Current status of the last test result, shown in the mode line.")

;; Add test status to the global mode line
(defun tdd-mode-update-mode-line ()
  "Update mode line with the current test status."
  (setq global-mode-string
        (list " "
              (propertize tdd-mode-test-status 'face '(:weight bold)))))

;; Ensure mode line is updated when tdd-mode is enabled
(add-hook 'tdd-mode-hook #'tdd-mode-update-mode-line)

(defun tdd-mode-get-python-executable ()
  "Retrieve the Python executable path from the active virtual environment."
  (let ((venv (getenv "VIRTUAL_ENV")))
    (if venv
        (concat venv "/bin/python")
      (error "No active virtual environment found"))))

(defun tdd-mode-get-pytest-executable ()
  "Retrieve the pytest executable from the active virtual environment."
  (let ((venv (getenv "VIRTUAL_ENV")))
    (if (and venv (file-exists-p (concat venv "/bin/pytest")))
        (concat venv "/bin/pytest")
      (error "pytest not found in active virtual environment"))))

(defun tdd-mode-get-project-root ()
  "Detect and return the project root directory based on common project markers."
  (or (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory "setup.py")
      (user-error "Project root not found. Please ensure your project has a recognizable root marker.")))

(defun tdd-mode-get-test-command-at-point ()
  "Generate the test command for pytest with ClassName::test_function format at point."
  (let* ((file-name (buffer-file-name))
         (function-name (tdd-mode-get-function-name-at-point))
         (class-name (tdd-mode-get-class-name-at-point))
         (pytest-executable (tdd-mode-get-pytest-executable)))
    (message "[tdd-mode] Using pytest executable: %s" pytest-executable)
    (cond
     ((eq tdd-mode-test-runner 'pytest)
      (when (and file-name function-name)
        (if class-name
            (format "%s --color=yes %s::%s::%s" pytest-executable file-name class-name function-name)
          (format "%s --color=yes %s::%s" pytest-executable file-name function-name))))
     ((eq tdd-mode-test-runner 'nosetests)
      (when (and file-name function-name)
        (format "nosetests %s:%s" file-name function-name)))
     ((eq tdd-mode-test-runner 'django)
      (when file-name
        (format "%s manage.py test %s" (tdd-mode-get-python-executable) file-name)))
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
        (setq buffer-read-only nil) ;; Allow writing in the buffer
        (erase-buffer)
        (insert (concat "$ " test-command "\n\n"))
        (let ((exit-code (call-process-shell-command test-command nil tdd-mode-test-buffer t)))
          (tdd-mode-apply-ansi-color)
          (setq buffer-read-only t) ;; Set buffer as read-only after output
          (display-buffer tdd-mode-test-buffer)
          (tdd-mode-update-status exit-code)
          (tdd-mode-notify exit-code)
          (tdd-mode-log-last-test exit-code))))))

(defun tdd-mode-run-test-at-point ()
  "Run the test command at the current point, if possible."
  (interactive)
  (let ((command (tdd-mode-get-test-command-at-point)))
    (if command
        (tdd-mode-run-test command)
      (message "No test command found at point."))))

(defun tdd-mode-run-test-file ()
  "Run the current test file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (pytest-executable (tdd-mode-get-pytest-executable)))
    (if file-name
        (tdd-mode-run-test (format "%s --color=yes %s" pytest-executable file-name))
      (message "No file associated with the current buffer."))))

(defun tdd-mode-apply-ansi-color ()
  "Apply ANSI color codes in the test buffer for improved readability."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun tdd-mode-notify (exit-code)
  "Send a notification based on the EXIT-CODE of the test."
  (let ((msg (if (eq exit-code 0) "✅ Test passed!" "❌ Test failed!"))
        (severity (if (eq exit-code 0) 'normal 'high)))
    (alert msg :title "TDD Mode" :severity severity)))

(defun tdd-mode-update-status (exit-code)
  "Update the mode line badge based on the test result EXIT-CODE."
  (setq tdd-mode-test-status (if (eq exit-code 0) "✅" "❌"))
  (tdd-mode-update-mode-line))

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
  "Re-run the last test command if a .py file in the same project is saved."
  (when (and tdd-mode-last-test-command
             (tdd-mode-same-project-p))
    (tdd-mode-run-test tdd-mode-last-test-command)))

(defun tdd-mode-same-project-p ()
  "Check if the current buffer's project root matches `tdd-mode-project-root`."
  (let ((current-root (locate-dominating-file default-directory ".git")))
    (and current-root
         (string= current-root tdd-mode-project-root)
         (string= (file-name-extension (buffer-file-name)) "py"))))

;;;###autoload
(define-minor-mode tdd-mode
  "Enhanced TDD mode for Python development with real-time feedback and notifications."
  :lighter " TDD"
  ;; No keymap defined here to allow user-defined key bindings
  (if tdd-mode
      (progn
        (setq tdd-mode-project-root (tdd-mode-get-project-root))
        (tdd-mode-update-mode-line)
        (add-hook 'after-save-hook 'tdd-mode-run-last-test-on-save t t))
    (remove-hook 'after-save-hook 'tdd-mode-run-last-test-on-save t)))

(provide 'tdd-mode)

;;; tdd-mode.el ends here
