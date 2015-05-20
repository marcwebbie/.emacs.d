(require 'use-package)

(use-package shell
  :mode (("\\.bash" . shell-script-mode)
         ("\\.zsh" . shell-script-mode)
         ("\\.fish" . shell-script-mode)))

(provide 'setup-shell-mode)
