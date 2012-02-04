;;Color-theme
(add-live-lib-path "color-theme")
(require 'color-theme)

;; To use blackbored colour theme add the followign lines to your
;; config:
;;(load-file (concat dotfiles-lib-dir "blackbored.el"))
;;(color-theme-blackbored)

;;highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")

;;set cursor colour
(set-cursor-color "yellow")

;;make sure ansi colour character escapes are honoured
(ansi-color-for-comint-mode-on)

;get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
