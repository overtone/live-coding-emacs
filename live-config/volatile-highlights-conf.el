;; momentarily highlight changes made by commands such as undo, yank-pop, etc.
(add-live-lib-path "volatile-highlights")
(require 'volatile-highlights)
(volatile-highlights-mode t)
