## Live Coding Emacs setup for Overtone

An opinionated set of defaults for getting started with Emacs for use as a live coding tool for Overtone. These have been extracted from Sam Aaron's personal dot-emacs which is even more opinionated and probably not entirely fit for general consumption. This version pulls out the most useful elements in a format that's simple to install and get you live coding quickly.

### Prerequisites

This dot-emacs config has only been tested with a terminal hosted Emacs `23.2.1`. Issues and pull-requests for this and later versions will be happily accepted.

### Getting started

1. Download the source and move and rename the `dot-emacs` folder to `~/.emacs.d`
2. Launch Emacs
3. Live code your hat off!

### What's inside?

This config includes an exciting cornucopia of Emacs goodies set up and ready to go. The highlights are:

* Clojure Mode (with fancy `(λ [a] (+ a 5))` and `ƒ(+ % 5)` prettifications)
* Slime (for communicating with swank servers)
* Auto completion (configured to work with slime for inline auto completion of documentation)
* Tailor-made colour theme
* Rainbow parens and delimiters (to allow you to easily navigate a sea of parens)
* The amazing undo-tree (live-code with confidence!)
* Textmate-like snippets
* Fancy highlighting of sexps on eval
* REPL syntax highlighting

### Feedback

I'm very happy to hear any feedback regarding this config. The idea is for you to use it to get started and give you a platform to start editing it and turning it into something personal.


