;; (defcustom auto-save+-delay 1
;;   "Delay to save buffers when idle in seconds"
;;   :type 'integer
;;   :group 'auto-save+)

(require 'cl)


(defun auto-save+-unsaved-bufers-p ()
    (remove-if-not (lambda (buf) (and (buffer-file-name buf) (buffer-modified-p buf))) (buffer-list)))


(defun auto-save+-save-buffers ()
  "Save buffers if they are files"
  (if (and (buffer-file-name) (auto-save+-unsaved-bufers-p)) (save-some-buffers t)))


(provide 'auto-save+)
