;;; osx.el --- OSX related configuration -*- lexical-binding: t; -*-

;;; Code:

;; Set default spell program
;; installed by: `brew install aspell --all`
(setq ispell-program-name "/usr/local/bin/aspell")

;; Switch the Cmd and Meta keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; OSX friendly font
;; (when window-system
;;   (set-face-attribute 'default nil :font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

;; Make the browser the OS X default
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f11]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")
