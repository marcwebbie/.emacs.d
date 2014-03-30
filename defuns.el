;;; defuns.el --- All customized functions added here

(defun select-text-in-quote ()
  (interactive)
  (let (p1)
    (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘⦅〚⦃\"")
    (setq p1 (point))
    (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙⦆〛⦄\"")
    (set-mark p1)))

(defun mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))

(defun get-clipboard-contents-as-string ()
  "Return the value of the clipboard contents as a string."
  (let ((x-select-enable-clipboard t))
    (or (x-selection-value)
        x-last-selected-text-clipboard)))

(defun path-to-string ()
  (interactive)
  (expand-file-name (or (buffer-file-name) default-directory)))

(defun select-inside-quotes ()
  "Select text between double straight quotes on each side of cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^\"")
    (setq p1 (point))
    (skip-chars-forward "^\"")
    (setq p2 (point))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))

(defun my-mark-test-name ()
  (interactive)
  (let (test_name cmd file_path)
    (er/mark-defun)
    (forward-char 4)
    (er/mark-symbol)
    (clipboard-kill-ring-save (region-beginning) (region-end))
    (setq test_name (get-clipboard-contents-as-string))
    (setq file_path (path-to-string))
    (setq cmd (format "bundle exec -- ruby %s -n /%s/" file_path test_name))
    (message (format "running: `%s`" cmd))
    (eshell-command cmd)))

(provide 'defuns)
;;; defuns.el ends here
