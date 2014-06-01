;;; osx.el --- OSX related configuration -*- lexical-binding: t; -*-

;;; Code:

;; Switch the Cmd and Meta keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; mac friendly font
(when window-system
  (setq default-font "-apple-Monaco-medium-normal-normal-*-15-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font default-font))

;; Make the browser the OS X default
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f11]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")
