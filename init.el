(setq custom-basedir (expand-file-name "~/.emacs.d/"))

(defun add-local-path (p)
  (add-to-list 'load-path (concat custom-basedir p)))

(defun load-local-file (p)
  (load-file (concat custom-basedir p)))

(add-local-path "lib")

(require 'dircolors)
(require 'smooth-scrolling)
(require 'rainbow-parens)
(require 'rainbow-delimiters)

;; The amazing undo tree
(add-local-path "lib/undo-tree/")
(require 'undo-tree)
(global-undo-tree-mode)

;;slime lets you connect to a swank server
(add-local-path "lib/slime")
(require 'slime)

(load-local-file "config/paredit-conf.el")
(load-local-file "config/cosmetic.el")
(load-local-file "config/bindings.el")
(load-local-file "config/highlight-flash-conf.el")
(load-local-file "config/ido-conf.el")
(load-local-file "config/clojure-conf.el")
(load-local-file "config/slime-conf.el")
(load-local-file "config/auto-complete-conf.el")
(load-local-file "config/durendal-conf.el")
(load-local-file "config/smex-conf.el")
(load-local-file "config/yasnippet-conf.el")
