(add-live-lib-path "yasnippet")
(require 'yasnippet)
(yas/initialize)

(setq yas/root-directory (concat dotfiles-etc-dir "snippets"))

;; Load the snippets
(yas/load-directory yas/root-directory)

