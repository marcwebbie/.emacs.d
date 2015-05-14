(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-M-SPC" . er/expand-region)
         ("C-+" . er/contract-region))
  )

(provide 'setup-expand-region)
