;; Here is the root of your personal configs.
;; Either place config straight in here,
;; such as this colour theme (feel free to change it to your own favourite theme)
;;; --------------------------------------------------------------------------
;;; PACKAGES
;;; --------------------------------------------------------------------------
(defvar my-packages
  '(
  ;;color-theme-sanityinc-tomorrow
  ;;color-theme-sanityinc-solarized
  ;;starter-kit
  ;;starter-kit-bindings
    )
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; use blackbored colour theme
(load-file (concat dotfiles-lib-dir "blackbored.el"))
(color-theme-blackbored)

;;Or load external files such as this bindings file:
(load-dotfile "config/bindings.el")
