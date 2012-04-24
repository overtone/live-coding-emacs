;; This is where everything starts


;; Ensure package.el is available to install our packages.
;; Emacs 24 comes bundled with package.el.
;; If you can't use Emacs 24, you'll need to install package.el.
;; See http://tromey.com/elpa/install.html
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(if (not package-archive-contents)
  (package-refresh-contents))

;; Auto-install base packages!
(defvar base-packages
  '(
    mwe-log-commands
    rainbow-delimiters
    smex
    undo-tree
    volatile-highlights
    yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p base-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; The following included packages aren't (yet) available in package.el:
;; * auto-complete
;; * blackbored
;; * dircolors
;; * eval-sexp-fu
;; * highlight
;; * key-chord
;; * smooth-scrolling


;; The following packages are available in package.el but not
;; installed from there:
;; * color-theme (I leave it like LCE installed it for now.)


;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Create variables to store the path to this dotfile dir's lib etc and tmp directories
(setq dotfiles-lib-dir (concat dotfiles-dir "lib/"))
(setq dotfiles-tmp-dir (concat dotfiles-dir "tmp/"))
(setq dotfiles-etc-dir (concat dotfiles-dir "etc/"))

;; Create helper fns for loading dotfile paths and files
(defun add-dotfile-path (p)
  (add-to-list 'load-path (concat dotfiles-dir p)))

(defun add-lib-path (p)
  (add-to-list 'load-path (concat dotfiles-lib-dir p)))

(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))


;; Ensure the lib directory is on the load path
(add-dotfile-path "lib")


;; Pull in live-coding config (see https://github.com/overtone/live-coding-emacs)
(load-dotfile "live-config/live.el")


;; Pull in personalised config
(load-dotfile "config/core.el")

