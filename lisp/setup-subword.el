(require 'use-package)

(use-package subword
  :defer 5
  :diminish subword-mode
  :config
  (global-subword-mode 1)
  (defadvice subword-upcase (before upcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word)))

  (defadvice subword-downcase (before downcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word)))

  (defadvice subword-capitalize (before capitalize-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word))))


(provide 'setup-subword)
