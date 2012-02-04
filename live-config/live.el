;; Live Coding Config
;; Pulls in and configures all libs useful for live coding
;; with a specific focus on Clojure and Overtone

;; Assumes the presence of:
;; - dotfiles-lib-dir (usually bound to ~/.emacs.d/lib/)
;; - dotfiles-tmp-dir (usually bound to ~/.emacs.d/tmp/)
;; - dotfiles-etc-dir (usually bound to ~/.emacs.d/etc/)

;; Create a var for this live config dir
(setq live-config-dir (file-name-directory
                       (or (buffer-file-name) load-file-name)))

;; Define some helper fns for loading live config paths and files
(defun load-live-config-file (f)
  (load-file (concat live-config-dir f)))

(defun add-live-lib-path (p)
  (add-to-list 'load-path (concat dotfiles-lib-dir p)))

;; Ensure that the lib dir is on the load-path (safety first!)
(add-to-list 'load-path dotfiles-lib-dir)
(require 'dircolors)
(require 'smooth-scrolling)
(require 'rainbow-delimiters)
(require 'mwe-log-commands)
(require 'ace-jump-mode)

(load-live-config-file "key-chord-conf.el")
(load-live-config-file "util-fns.el")
(load-live-config-file "built-in.el")
(load-live-config-file "paredit-conf.el")
(load-live-config-file "lisps-conf.el")
(load-live-config-file "cosmetic.el")
(load-live-config-file "highlight-flash-conf.el")
(load-live-config-file "volatile-highlights-conf.el")
(load-live-config-file "ido-conf.el")
(load-live-config-file "clojure-conf.el")
(load-live-config-file "slime-conf.el")
(load-live-config-file "auto-complete-conf.el")
(load-live-config-file "smex-conf.el")
(load-live-config-file "yasnippet-conf.el")
(load-live-config-file "undo-tree-conf.el")
