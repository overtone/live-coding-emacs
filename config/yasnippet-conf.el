(add-local-path "lib/yasnippet")
(require 'yasnippet)
(yas/initialize)

(setq yas/root-directory (concat custom-basedir "etc/snippets"))

;; Load the snippets
(yas/load-directory yas/root-directory)

