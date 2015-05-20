(require 'use-package)

(use-package conf-mode
  :mode (
         ("\\.coveragerc" . conf-mode)
         ("\\.passpierc" . conf-mode)
         ("\\.tox" . conf-mode)
         )
  )

(provide 'setup-conf-mode)
