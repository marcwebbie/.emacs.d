;;; shoulda.el --- Shoulda test support for ruby
;;; Development in Ruby


(defun shoulda:run-should-at-point ()
  "Run Shoulda should test at point"
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (let* ((name-regex "\\(\\(:[a-z0-9_]+\\)\\|\\([\"']\\([a-z0-9_ ]+\\)[\"']\\)\\)")
           (name-match (lambda () (or (match-string-no-properties 2) (match-string-no-properties 4))))
           (should (when (search-backward-regexp (concat "[ \t]*should +" name-regex "[ \t]+do") nil t)
                     (funcall name-match)))
           (context (when (search-backward-regexp (concat "[ \t]*context +" name-regex "[ \t]+do") nil t)
                      (funcall name-match))))
      (when (and should context)
        (compilation-start (concat "cd " (projectile-project-root) " && bundle exec -- ruby " (buffer-file-name) " -n /'"  should "'/"))))))

(defun shoulda:run-context-at-point ()
  "Run Shoulda context test at point"
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (let* ((name-regex "\\(\\(:[a-z0-9_]+\\)\\|\\([\"']\\([a-z0-9_ ]+\\)[\"']\\)\\)")
           (name-match (lambda () (or (match-string-no-properties 2) (match-string-no-properties 4))))
           (should (when (search-backward-regexp (concat "[ \t]*should +" name-regex "[ \t]+do") nil t)
                     (funcall name-match)))
           (context (when (search-backward-regexp (concat "[ \t]*context +" name-regex "[ \t]+do") nil t)
                      (funcall name-match))))
      (when (and should context)
        (compilation-start (concat "cd " (projectile-project-root) " && bundle exec -- ruby " (buffer-file-name) " -n /'"  context "'/"))))))

;;; shoulda ends here
