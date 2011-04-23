;;Color-theme
(add-live-lib-path "color-theme")
(require 'color-theme)

;; Sam's colours **********************
;; use blackbored colour theme
(load-file (concat dotfiles-lib-dir "blackbored.el"))
(color-theme-blackbored)

;;highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")

;;set cursor colour
(set-cursor-color "yellow")

;; End Sam's colours ******************

;;;; Iain's colours *********************
;;;; use tg colour theme
;;(load-file (concat dotfiles-lib-dir "tg.el"))
;;(color-theme-tg)
;;
;;;;highlight current line
;;(global-hl-line-mode 1)
;;
;;;; End Iain's colours *****************

;;make sure ansi colour character escapes are honoured
(ansi-color-for-comint-mode-on)

;get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
