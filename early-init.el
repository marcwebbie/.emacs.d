;;; Core configurations to run before package.el is initialized
(setq package-enable-at-startup nil)

;; Set GC threshold for faster startup (optional but recommended)
(setq gc-cons-threshold 10000000)
