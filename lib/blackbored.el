;; Blackbored Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Blackboard colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/themes/color-theme-blackbored.el")
;;
;; And then (color-theme-blackboard) to activate it.
;;
;; MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;; Credits due to the excellent TextMate Blackboard theme
;;
;; All patches welcome

(require 'color-theme)

;;;###autoload
(defun color-theme-blackbored ()
  "Color theme by Sam Aaron, based off BlackBoard by JD Huntington based off the TextMate Blackboard theme, created 2008-11-27"
  (interactive)
  (color-theme-install
   '(color-theme-blackbored
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#96CBFE")
      (foreground-color . "#EDEDED")
      (mouse-color . "sienna1"))

     (default ((t (:background "black" :foreground "light gray"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))


     (font-lock-builtin-face ((t (:foreground "#FF6400"))))
     (font-lock-comment-face ((t (:italic t :foreground "#8B8989"))))
     (font-lock-constant-face ((t (:foreground "#4c83ff"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "deep pink"))))
     (font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
     (font-lock-preprocessor-face ((t (:foreground "gray57"))))
     (font-lock-reference-face ((t (:foreground "medium slate blue"))))
     (font-lock-reference-face ((t (:foreground "gray"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     (font-lock-string-face ((t (:foreground "#61CE3C"))))
     (font-lock-type-face ((t (:foreground "#D8FA3C"))))
     (font-lock-variable-name-face ((t (:foreground "#D8FA3C"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (gui-element ((t (:background "#333333" :foreground "#96CBFE"))))
;;     (region ((t (:foreground "black" :background "#7F073F"))))
 (region ((t ( :background "#7F073F"))))
     (mode-line ((t (:background "gray10" :foreground "#4c83ff"))))
     (mode-line-inactive ((t (:background "gray10" :foreground "gray30"))))

     (highlight ((t (:background "DarkOrange"))))
     (isearch ((t (:background "deep pink" :foreground "black"))))
     (isearch-fail ((t (:background "red4"))))
     (lazy-highlight ((t (:background "yellow" :foreground "black"))))
     (query-replace ((t (:background "#333333"))))
     (Highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (vertical-border ((t (:background "black" :foreground "#333333"))))
     (zmacs-region ((t (:background "snow" :foreground "ble"))))

     (diff-added ((t (:foreground "green"))))
     (diff-removed ((t (:foreground "red"))))

     (magit-diff-add ((t (:foreground "green"))))
     (magit-diff-del ((t (:foreground "red"))))
     (magit-item-highlight ((t (:background "gray15"))))
     (magit-section-title ((t (:foreground "deep pink"))))
     (magit-diff-hunk-header ((t (:foreground "orange"))))
     (magit-branch ((t (:foreground "gold"))))

     (eval-sexp-fu-flash ((t (:background "grey15" :foreground "DeepPink3"))))

     (ac-completion-face ((t (:foreground "darkgray" :underline t))))
     (ac-candidate-face ((t (:background "gray60" :foreground "black"))))
     (ac-selection-face ((t (:background "deep pink" :foreground "black"))))
     (popup-isearch-match ((t (:background "black" :foreground "deep pink"))))
     (popup-tip-face ((t (:background "#333333" :foreground "whitepnpnn"))))
     (popup-scroll-bar-foreground-face ((t (:background "#0A0A0A"))))
     (popup-scroll-bar-background-face ((t (:background "#333333"))))

     (window-number-face ((t (:background "grey10" :foreground "#4c83ff"))))

     (yas/field-highlight-face ((t (:background "deep pink" :foreground "black"))))

     (show-paren-match-face ((t (:background "deep pink" :foreground "black"))))

     (naeu-green-face ((t (:foreground "green" :background "black"))))
     (naeu-pink-face ((t (:foreground "deep pink" :background "black"))))
     (naeu-blue-face ((t (:foreground "medium slate blue" :background "black"))))
     (naeu-orange-face ((t (:foreground "#FBDE2D" :background "black"))))
     (naeu-red-face ((t (:foreground "orange" :background "black"))))
     (naeu-grey-face ((t (:foreground "gray30" :background "black"))))

     (ido-first-match ((t (:foreground "deep pink" :background "black"))))
     (ido-only-match ((t (:foreground "deep pink" :background "black"))))
     (ido-subdir ((t (:foreground "gray60" :background "black"))))
     (ido-indicator ((t (:foreground "black" :background "deep pink"))))

     (match ((t (:foreground "deep pink" :background "blackn"))))
     (minibuffer-prompt ((t (:foreground "#61CE3C" :background "black"))))
     (grep-match-face ((t (:foreground "black" :background "deep pink"))))
     (grep-hit-face ((t (:foreground "black" :background "red"))))
     (grep-context-face ((t (:foreground "black" :background "deep pink"))))

     (rainbow-delimiters-depth-1-face ((t (:foreground "dark red"))))
     (rainbow-delimiters-depth-2-face ((t (:foreground "dark green"))))
     (rainbow-delimiters-depth-3-face ((t (:foreground "deep pink"))))
     (rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
     (rainbow-delimiters-depth-5-face ((t (:foreground "green"))))
     (rainbow-delimiters-depth-6-face ((t (:foreground "light blue"))))
     (rainbow-delimiters-depth-7-face ((t (:foreground "orange"))))
     (rainbow-delimiters-depth-8-face ((t (:foreground "slate blue"))))
     (rainbow-delimiters-depth-9-face ((t (:foreground "light gray"))))
     (rainbow-delimiters-unmatched-face ((t (:foreground "white"))))

     (erc-notice-face ((t (:bold t :foreground "grey26"))))

     (erc-action-face ((t (:foreground "#FF6400"))))
;;     (erc-bold-face ((t (:bold t :weight bold))))
;;     (erc-button ((t (:bold t :weight bold))))
;;     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-current-nick-face ((t (:foreground "#FBDE2D"))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (:foreground "#61CE3C"))))
     (erc-direct-msg-face ((t (:foreground "orange"))))
     (erc-error-face ((t (:foreground "red"))))
     (erc-fool-face ((t (:foreground "dim gray"))))
     (erc-header-line ((t (:background "grey90" :foreground "grey20"))))
     (erc-input-face ((t (:foreground "#4c83ff"))))
     (erc-inverse-face ((t (:background "Black" :foreground "White"))))
     (erc-keyword-face ((t (:foreground "deep pink"))))
     (erc-my-nick-face ((t (:bold t :foreground "deep pink" ))))
     (erc-nick-default-face ((t (:foreground "grey57"))))
     (erc-nick-msg-face ((t (:foreground "deep pink"))))

     (erc-pal-face ((t (:bold t :foreground "Magenta" :weight bold))))
     (erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))
     (erc-timestamp-face ((t (:foreground "dim gray"))))
     (erc-underline-face ((t (:underline t))))


     (undo-tree-visualizer-active-branch-face ((t (:foreground "deep pink" :background "black")))))))



(custom-set-faces

 ;;nXhtml colours
 '(mumamo-background-chunk-major ((((class color) (background dark)) (:background "black"))))
 '(mumamo-background-chunk-submode1 ((((class color) (background dark)) (:background "black"))))

 ;;ediff
 '(ediff-even-diff-A ((((class color) (background dark)) (:background "dark green"))))
 '(ediff-odd-diff-A ((((class color) (background dark)) (:background "dark green"))))
 '(ediff-odd-diff-B ((((class color) (background dark)) (:background "dark red"))))
 '(ediff-even-diff-B ((((class color) (background dark)) (:background "dark red"))))
; '(ediff-current-diff-B ((((class color)) (:background "white"))))
; '(ediff-even-diff-A ((((class color)) nil)))
; '(ediff-even-diff-B ((((class color)) nil)))
; '(ediff-fine-diff-A ((((class color)) (:background "cyan"))))
; '(ediff-fine-diff-B ((((class color)) (:background "cyan"))))
; '(ediff-odd-diff-A ((((class color)) nil)))
; '(ediff-odd-diff-B ((((class color)) nil)))
 )
