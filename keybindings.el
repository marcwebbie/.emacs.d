;; Like readline
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Occur can show all lines in the current buffer containing a match for REGEXP.
(global-set-key (kbd "C-c o") 'occur)

;; Reindent when RET
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

(global-set-key (kbd "M-#") 'mark-current-word)
(global-set-key (kbd "M-*") 'select-text-in-quote)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Sorting
(global-set-key (kbd "C-<dead-tilde>") 'sort-lines)

(bind-key "M-+" 'shrink-window)
(bind-key "M-_" 'enlarge-window)

(bind-key "C-c t" 'my-mark-test-name)

;; Robe jump
(global-set-key (kbd "M-.") 'robe-jump) 

;;; end
