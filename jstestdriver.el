;;; jstestdriver --- Package to run jstestdrive tests
;;; Commentary:
;;  Regex found using regexp-builder: "[ \t]+test\(['\"]\\(.*?\\)['\"],"
;;
;;; Code:

(defun jstestdriver:run-should-at-point ()
  "Run jstestdriver test at point or the before the point"
  (interactive)
  (save-excursion
    (let* ((test_name (when (search-backward-regexp "[ \t]+test\(['\"]\\(.*?\\)['\"]," nil t) (match-string-no-properties 1)))
           (test_cmd (concat "bundle exec -- rake \"test:jstestdriver:ove_jquery[" test_name "]\"")))
      (if test_name
          (progn
            (print test_cmd) (compilation-start test_cmd)) (error "Test name not found!")))))

(provide 'jstestdriver)
;;; jstestdriver.el ends here
