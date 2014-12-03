;;; osx.el --- OSX related configuration -*- lexical-binding: t; -*-

;;; Code:

;; ;; Set default spell program
;; ;; installed by: `brew install aspell --all`
(setq ispell-program-name "/usr/local/bin/aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; Switch the Cmd and Meta keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; Make the browser the OS X default
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Fix path for zsh in emacs shells
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))

(exec-path-from-shell-initialize)
