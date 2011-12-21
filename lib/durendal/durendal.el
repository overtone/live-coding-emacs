;;; durendal.el --- A bucket of tricks for Clojure and Slime.

;; Copyright (C) 2010 Phil Hagelberg
;;
;; Author: Phil Hagelberg
;; URL: http://github.com/technomancy/durendal
;; Version: 0.2
;; Keywords: lisp clojure slime
;; Created: 2010-08-13
;; Package-Requires: ((clojure-mode "1.7") (slime "20100404") (paredit "22"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;   "Count Roland smites upon the marble stone;
;;   I cannot tell you how he hewed it and smote;
;;   Yet the blade breaks not nor splinters, though it groans;
;;   Upward to heaven it rebounds from the blow."
;;
;; –From The Song of Roland
;; (Translated by Dorothy Sayers, Viking Penguin, NY, NY, 1957)

;; Durendal is a bucket of tricks for Clojure that are too
;; implementation-specific to go in Slime proper but too fancy to go
;; in clojure-mode itself.

;;; Features:

;; * durendal-hide-successful-compile: Hide compilation buffer upon success.
;; * durendal-jack-in: Initiate a lein-swank + slime-connect.
;; * durendal-enable-auto-compile: Compile on save.
;; * durendal-slime-repl-paredit: Turn on paredit in the slime repl.
;; * durendal-enable-slime-repl-font-lock: Use clojure-mode's
;;                     font-lock settings in SLIME REPL (with tweaks
;;                     for prompts and printouts); turn off with
;;                     durendal-disable-slime-repl-font-lock.

;; Call durendal-enable to turn on all Durendal features. To enable
;; features one at a time, simply use the corresponding add-hook call:

;; (add-hook 'clojure-mode-hook 'durendal-enable-auto-compile)
;; (add-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
;; (add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
;; (add-hook 'slime-compilation-finished-hook 'durendal-hide-successful-compile)

;;; TODO:

;; * ns-unmap binding
;; * bury *SLIME Compilation* buffer on successful compile
;; * package and require paredit 22

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'slime)
(require 'slime-repl)
(require 'clojure-mode)

(defvar durendal-auto-compile? t
  "Automatically compile on save when applicable.")

;; launcher

;;;###autoload
(defun durendal-jack-in (&optional port-prompt)
  (interactive "P")
  (lexical-let ((root (locate-dominating-file default-directory "project.clj"))
                (port (if port-prompt
                          (string-to-number (read-string "Port: " nil nil slime-port))
                        (+ 1024 (* (random 64512))))))
    (message "Launching lein swank on %s..." port)
    (when (not root)
      (error "Not in a Leiningen project."))
    (shell-command (format "cd %s && lein swank %s &" root port)
                   "*lein-swank*")
    (message "Launching lein swank on %s..." port)
    (set-process-filter (get-buffer-process "*lein-swank*")
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (slime-connect "localhost" port)
                            (with-current-buffer (slime-output-buffer t)
                              (setq default-directory root))
                            (set-process-filter process nil))))
    (message "Starting swank server...")))

;; debugger

;;;###autoload
(defun durendal-dim-sldb-font-lock ()
  "Dim irrelevant lines in Clojure debugger buffers."
  (if (string-match "clojure" (buffer-name))
      (font-lock-add-keywords
       nil `((,(concat " [0-9]+: " (regexp-opt '("clojure.core"
                                                 "clojure.lang"
                                                 "swank." "java."))
                       ;; TODO: regexes ending in .* are ignored by
                       ;; font-lock; what gives?
                       "[a-zA-Z0-9\\._$]*")
              . font-lock-comment-face)) t)))

;; belongs in slime?
(put 'slime-lisp-host 'safe-local-variable 'stringp)
(put 'slime-port 'safe-local-variable 'integerp)

;; auto-compile

(defun durendal-in-current-project? (file)
  (let ((root (expand-file-name
               (read (cadr (slime-eval
                            '(swank:eval-and-grab-output
                              "(System/getProperty \"user.dir\")")))))))
    (string= (substring file 0 (length root)) root)))

(defun durendal-auto-compile ()
  (when (and slime-mode durendal-auto-compile?
             (slime-connected-p) (slime-current-package)
             (durendal-in-current-project? buffer-file-name))
    (slime-compile-and-load-file)))

;;;###autoload
(defun durendal-enable-auto-compile ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'durendal-auto-compile))

;; compile-buffer suppression

;;;###autoload
(defun durendal-hide-successful-compile (msg)
  (with-struct (slime-compilation-result. notes duration successp)
      slime-last-compilation-result
    (when successp
      (kill-buffer "*SLIME Compilation*"))))

;; repl

;;;###autoload
(defun durendal-slime-repl-paredit ()
  (require 'paredit)
  (define-key slime-repl-mode-map
    (kbd "DEL") 'paredit-backward-delete)
  (define-key slime-repl-mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key slime-repl-mode-map
    (kbd "}") 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")
  (modify-syntax-entry ?~ "'   ")
  (modify-syntax-entry ?, "    ")
  (modify-syntax-entry ?^ "'")
  (modify-syntax-entry ?= "'")
  (paredit-mode t))

(defadvice slime-repl-emit (after durendal-slime-repl-emit-ad)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after durendal-slime-repl-prompt-ad)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))
;;;###autoload
(defun durendal-enable-slime-repl-font-lock ()
  (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
  (ad-activate #'slime-repl-emit)
  (ad-activate #'slime-repl-insert-prompt))

;;;###autoload
(defun durendal-disable-slime-repl-font-lock ()
  (remove-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
  (ad-deactivate #'slime-repl-emit)
  (ad-deactivate #'slime-repl-insert-prompt))

;; entry point:

;;;###autoload
(defun durendal-enable (&optional preserve-proto-version-check-p)
  "Enable hooks for all durendal functionality."
  (when (not preserve-proto-version-check-p)
    (setq slime-protocol-version 'ignore))
  (add-hook 'slime-connected-hook
            (lambda ()
              (if (equal (slime-lisp-implementation-name) "clojure")
                  (progn
                    (eval-after-load 'clojure-mode
                      '(font-lock-add-keywords
                        'slime-repl-mode `(("(\\(fn\\)[\[[:space:]]"
                                         (0 (progn (compose-region (match-beginning 1)
                                                                   (match-end 1) "λ")
                                                   nil))))))

                    (eval-after-load 'clojure-mode
                      '(font-lock-add-keywords
                        'slime-repl-mode `(("\\(#\\)("
                                         (0 (progn (compose-region (match-beginning 1)
                                                                   (match-end 1) "ƒ")
                                                   nil))))))

                    (eval-after-load 'clojure-mode
                      '(font-lock-add-keywords
                        'slime-repl-mode `(("\\(#\\){"
                                         (0 (progn (compose-region (match-beginning 1)
                                                                   (match-end 1) "∈")
                                                   nil))))))

                    (add-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
                    (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
                    (add-hook 'slime-repl-mode-hook 'rainbow-paren-mode)
                    (add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
                    (durendal-enable-slime-repl-font-lock))


                  (progn
;;                    (remove-hook 'clojure-mode-hook 'durendal-enable-auto-compile)
                    (remove-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
                    (remove-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
                    (durendal-disable-slime-repl-font-lock))))))

(provide 'durendal)
;;; durendal.el ends here
