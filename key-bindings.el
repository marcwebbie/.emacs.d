;;; key-bindings --- global key bindings
;;; Commentary:
;;; Code:

(bind-key "<f6>" 'linum-mode)
(bind-key "<f8>" (λ (find-file (f-expand "init.el" user-emacs-directory))))
(bind-key "<f7>" 'ansi-term)

(bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
(bind-key "C-j" 'newline-and-indent)
(bind-key "C-z" 'zap-up-to-char)

(bind-key "C-M-;" 'comment-or-uncomment-current-line-or-region)

(bind-key "C-x C-c" (λ (if (y-or-n-p "Quit Emacs? ") (save-buffers-kill-emacs))))

(bind-key "C-c C-r" 'rename-this-buffer-and-file)
(bind-key "C-c C-k" 'delete-this-buffer-and-file)

(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "C-c g" 'google)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c o" 'occur)
(bind-key "C-c y" 'youtube)

(bind-key "M-h" 'kill-to-beginning-of-line)
(bind-key "M-j" (λ (join-line -1)))
(bind-key "M-o" 'other-window)
(bind-key "M-<up>" 'open-line-above)
(bind-key "M-<down>" 'open-line-below)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(bind-key "C-x + -" (λ (replace-region-by 's-dashed-words)))
(bind-key "C-x + _" (λ (replace-region-by 's-snake-case)))
(bind-key "C-x + c" (λ (replace-region-by 's-lower-camel-case)))
(bind-key "C-x + C" (λ (replace-region-by 's-upper-camel-case)))

;; ace-jump-mode
(bind-key "C-c SPC" 'ace-jump-mode)
(bind-key "C-x SPC" 'ace-jump-mode-pop-mark)

;; drag-stuff
(bind-key "M-p" 'drag-stuff-up)
(bind-key "M-n" 'drag-stuff-down)

;; expand-region
(bind-key "C-=" 'er/expand-region)
(bind-key "C-+" 'er/contract-region)
(bind-key "C-\\" 'er/expand-region)
(bind-key "C-|" 'er/contract-region)
(bind-key "M-\\" 'er/mark-ruby-block-up)

;; git-gutter
(global-unset-key (kbd "C-c v"))
(bind-key "C-c v =" 'git-gutter:popup-hunk) ;; show hunk diff
(bind-key "C-c v p" 'git-gutter:previous-hunk)
(bind-key "C-c v n" 'git-gutter:next-hunk)
(bind-key "C-c v s" 'git-gutter:stage-hunk)
(bind-key "C-c v r" 'git-gutter:revert-hunk)

;; hippie
(bind-key "C-." 'hippie-expand-no-case-fold)
(bind-key "C-:" 'hippie-expand-lines)
(bind-key "C-," 'completion-at-point)

;; idomenu
(bind-key "M-i" 'idomenu)

;; magit
(bind-key "C-x g" 'magit-status)
(bind-key "q" 'magit-quit-session magit-status-mode-map)

;; multiple-cursors
(bind-key "C->" 'mc/mark-next-like-this)
(bind-key "C-<" 'mc/mark-previous-like-this)
(bind-key "C-c C-<" 'mc/mark-all-like-this)
(bind-key "C-c C-w" 'mc/mark-all-words-like-this)
(bind-key "C-c C-s" 'mc/mark-all-symbols-like-this)
(bind-key "C-S-c C-S-c" 'mc/edit-lines)

;; ruby-test-mode
(bind-key "C-c t b" 'ruby-test-run)

;; shoulda
(bind-key "C-c t s" 'shoulda:run-should-at-point)
(bind-key "C-c t c" 'shoulda:run-context-at-point)
(bind-key "C-c t j" 'jstestdriver:run-should-at-point)

;; smartparens
(bind-key "C-M-k" 'sp-kill-sexp-with-a-twist-of-lime)
(bind-key "C-M-f" 'sp-forward-sexp)
(bind-key "C-M-b" 'sp-backward-sexp)
(bind-key "C-M-n" 'sp-up-sexp)
(bind-key "C-M-d" 'sp-down-sexp)
(bind-key "C-M-u" 'sp-backward-up-sexp)
(bind-key "C-M-p" 'sp-backward-down-sexp)
(bind-key "C-M-w" 'sp-copy-sexp)
(bind-key "M-s" 'sp-splice-sexp)
(bind-key "M-r" 'sp-splice-sexp-killing-around)
(bind-key "C-)" 'sp-forward-slurp-sexp)
(bind-key "C-}" 'sp-forward-barf-sexp)
(bind-key "C-(" 'sp-backward-slurp-sexp)
(bind-key "C-{" 'sp-backward-barf-sexp)
(bind-key "M-S" 'sp-split-sexp)
(bind-key "M-J" 'sp-join-sexp)
(bind-key "C-M-t" 'sp-transpose-sexp)

;; smex
(bind-key "M-x" 'smex)
(bind-key "C-x C-m" 'smex)

;; visual-regexp-steroids
(bind-key "C-c r" 'vr/replace)
(bind-key "C-c q" 'vr/query-replace)

(provide 'key-bindings)
;;; key-bindings.el ends here
