## Live Coding Config

Configuration of libs useful for live coding with a specific focus on Clojure and Overtone.

### Prerequisites

Assumes the presence of `dotfiles-lib-dir` (usually bound to `~/.emacs.d/lib/`) and the following libraries in there:

* `eval-sexp-fu.el`
* `rainbow-delimiters.el`
* `highlight.el`
* `dircolors.el`
* `paredit.el`
* `blackbored.el`
* `smooth-scrolling.el`

And the following lib dirs:

* `align-cljlet`
* `ac-slime`
* `clojure-mode`
* `color-theme`
* `auto-complete`
* `undo-tree`
* `durendal` (note that this is a personalised version available at: https://github.com/samaaron/durendal)
* `smex`
* `slime`
* `yasnippet`

These may all be found in the lib dir here: https://github.com/overtone/live-coding-emacs

### Personalisations
If you wish to keep your live coding config in sync with others, don't edit the files in this folder directly and override stuff in your normal config files. Or, if you have something you think is valuable to others, send a pull-request with your new live coding goodies!
