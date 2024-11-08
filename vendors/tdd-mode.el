;;; tdd-mode.el --- Enhanced TDD Mode for Python development -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'f)
(require 'alert) ;; Use alert for notifications
(require 'auto-virtualenv)
(require 'ellama) ;; Ensure ellama is required for AI insights

;; Set alert to use terminal-notifier on macOS
(when (eq system-type 'darwin)
  (setq alert-default-style 'notifier))

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

(defvar tdd-mode-sound-on-fail t
  "Whether to play a sound on test failure.")

(defvar tdd-mode-ai-insights-enabled t
  "Whether to send failing test output to Ollama via ellama for insights.")

(defun tdd-mode-get-project-root ()
  "Detect and return the project root directory based on common project markers."
  (or tdd-mode-project-root
      (setq tdd-mode-project-root
            (or (locate-dominating-file default-directory ".git")
                (locate-dominating-file default-directory "pyproject.toml")
                (locate-dominating-file default-directory "setup.py")
                (user-error "Project root not found. Please ensure your project has a recognizable root marker.")))))

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
          (tdd-mode-log-last-test exit-code)
          (when (and tdd-mode-ai-insights-enabled (not (eq exit-code 0)))
            (tdd-mode-send-to-ai-for-insights))
          (when (and tdd-mode-sound-on-fail (not (eq exit-code 0)))
            (play-sound-file "/path/to/failure-sound.wav")))))))

(defun tdd-mode-run-test-at-point ()
  "Run the test command at the current point, if possible."
  (interactive)
  (let ((command (tdd-mode-get-test-command-at-point)))
    (if command
        (tdd-mode-run-test command)
      (message "No test command found at point."))))

(defun tdd-mode-apply-ansi-color ()
  "Apply ANSI color codes in the test buffer for improved readability."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun tdd-mode-notify (exit-code)
  "Send a notification based on the EXIT-CODE of the test."
  (let ((msg (if (eq exit-code 0) "✅ Test passed!" "❌ Test failed!"))
        (urgency (if (eq exit-code 0) 'normal 'high)))
    (when (or (and (eq exit-code 0) tdd-mode-notify-on-pass)
              (and (not (eq exit-code 0)) tdd-mode-notify-on-fail))
      (alert msg :title "TDD Mode" :severity urgency))
    (message msg)))

(defun tdd-mode-send-to-ai-for-insights ()
  "Send the failing test output to Ollama via ellama for AI insights."
  (let ((output (with-current-buffer tdd-mode-test-buffer
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (message "🔍 Sending failing test output to AI for insights...")
    (ellama-chat (format "Please help analyze this test failure:\n\n%s" output))
    (message "💡 AI insights for test failure: %s" output)))

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
      (add-hook 'after-save-hook 'tdd-mode-run-last-test-on-save)
    (remove-hook 'after-save-hook 'tdd-mode-run-last-test-on-save)))

(provide 'tdd-mode)

;;; tdd-mode.el ends here
