(require 'use-package)

(use-package git-gutter
  :defer 5
  :bind (("C-c v =" . git-gutter:popup-hunk) ;; show hunk diff
         ("C-c v p" . git-gutter:previous-hunk)
         ("C-c v n" . git-gutter:next-hunk)
         ("C-c v s" . git-gutter:stage-hunk)
         ("C-c v r" . git-gutter:revert-hunk))
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1)
  :config
  (custom-set-variables
   '(git-gutter:window-width 2)
   '(git-gutter:modified-sign "  ") ;; two space
   '(git-gutter:added-sign "  ")    ;; multiple character is OK
   '(git-gutter:deleted-sign "  "))

  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-background 'git-gutter:added "green")
  ;; (set-face-background 'git-gutter:deleted "red")
  )

(provide 'setup-git-gutter)
