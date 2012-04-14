<img src="https://github.com/downloads/overtone/live-coding-emacs/improcess-logo-2.png" alt="Fuzzy Improcess Logo" title="Improcess" align="right" />

## Live Coding Emacs setup for Overtone
An opinionated set of defaults for getting started with Emacs for use as a live coding tool for Overtone which also happens to make it a jolly good generic Clojure hacking config. This has been extracted from Sam Aaron's personal dot-emacs which is even more opinionated and probably not entirely fit for general consumption. This version pulls out the most useful elements in a format that's simple to install and get you live coding quickly.

### Prerequisites

This dot-emacs config has only been tested with a terminal hosted Emacs ` 24.1.50.1` (pre-release). Issues and pull-requests for this and later versions will be happily accepted.

### Getting started

1. Download the source and move and rename the `live-coding-emacs` folder to `~/.emacs.d`
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
* Refheap mode for pasting snippets to refheap.com
* Smex - for a much nicer fuzzy-matching M-x experience.

### Personalising the Config

Obviously, if you're an Emacs-whizz, you can take this config as a starting point and take it in your own direction, configuring as much as you wish. Fork away my friend!

However, if you're still learning, you might want to try and keep your copy of these configs pretty close to the original so that you may benefit from future updates. This is easily achieved with a couple of easy rules:

* Don't remove any of the libs
* Don't modify the files in live-config

If you do either of these, you're on your own :-)

However, this won't preclude you from making your own modifications in a non-conflicting manner. You may add new libs and you can add your own config files. Place your libs in the lib dir, and your configs in the config dir. The config is there for you to add your own personalisations, and it already contains a blank bindings file to place your own bindings. The files in the config dir will be loaded *after* the files in live-config, so you may override things to your own content. Start modifying and have at it!

### Screenshots

<img src="https://github.com/downloads/overtone/live-coding-emacs/live-coding-config-in-use.png" alt="Screenshot 1" title="Live Coding Config Screenshot 1" />

<img src="https://github.com/downloads/overtone/live-coding-emacs/live-coding-config-in-use-2.png" alt="Screenshot 2" title="Live Coding Config Screenshot 2" />

### Video

Here's a video showing the config in use: [Quick Intro to Live Programming with Overtone](http://vimeo.com/22798433)

### Feedback

I'm very happy to hear any feedback regarding this config. The idea is for you to use it to get started and give you a platform to start editing it and turning it into something personal.


