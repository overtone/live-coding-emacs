(add-local-path "lib/yasnippet")
(require 'yasnippet)
(yas/initialize)

(setq yas/root-directory (concat dotfiles-dir "etc/snippets"))

;; Load the snippets
(yas/load-directory yas/root-directory)

