(require 'use-package)

(use-package visual-regexp
  :defer 3
  :bind (("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)
         ("C-c r" . vr/replace)
         ("C-q" . vr/query-replace)))

;; provide
(provide 'setup-search)
