(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; create autosaves and backups tmp dirs if necessary
(make-directory (concat dotfiles-tmp-dir "autosaves") t)
(make-directory (concat dotfiles-tmp-dir "backups") t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/tmp/
(setq auto-save-file-name-transforms `((".*" ,(concat dotfiles-tmp-dir "autosaves/\\1") t)))
(setq backup-directory-alist `((".*" . ,(concat dotfiles-tmp-dir "backups"))))
(setq auto-save-list-file-name (concat dotfiles-tmp-dir "autosaves/autosave-list"))

;;When you visit a file, point goes to the last place where it was when you previously visited
;;Save file is set to dotfiles-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)

;;enable cua-mode for rectangular selections
(require 'cua-base)
(require 'cua-gmrk)
(require 'cua-rect)
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;;enable winner mode for C-c-(<left>|<right>) to navigate the history
;;of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
      (winner-mode 1))

(setq redisplay-dont-pause t
      visible-bell t
      column-number-mode t
      echo-keystrokes 0.02
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      save-place-file (concat dotfiles-tmp-dir "places")
      fill-column 80)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

(setq diff-switches "-u")

(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

(setq confirm-nonexistent-file-or-buffer nil)

;;remove all trailing whitespace and trailing blank lines before saving the file
(add-hook 'before-save-hook 'whitespace-cleanup)
