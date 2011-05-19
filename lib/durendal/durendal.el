;;; durendal.el --- A bucket of tricks for Clojure and Slime.

;; Copyright (C) 2010 Phil Hagelberg
;;
;; Author: Phil Hagelberg
;; URL: http://github.com/technomancy/durendal
;; Version: 0.1
;; Keywords: lisp clojure slime
;; Created: 2010-08-13
;; Package-Requires: ((clojure-mode "1.7") (slime "20100404"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; "Count Roland smites upon the marble stone;
;; I cannot tell you how he hewed it and smote;
;; Yet the blade breaks not nor splinters, though it groans;
;; Upward to heaven it rebounds from the blow."
;; --From The Song of Roland
;; (Translated by Dorothy Sayers, Viking Penguin, NY, NY, 1957)

;; Durendal is a bucket of tricks for Clojure that are too
;; implementation-specific to go in Slime proper but too fancy to go
;; in clojure-mode itself.

;;; Features:

;; * durendal-sort-ns: Sorts the :use, :require, :import, and
;;                     :refer sections of your ns call.
;; * durendal-jack-in: Initiate a lein-swank + slime-connect.
;; * durendal-enable-auto-compile: Compile on save.
;; * durendal-slime-repl-paredit: Turn on paredit in the slime repl.
;; * durendal-enable-slime-repl-font-lock: Use clojure-mode's
;;                     font-lock settings in SLIME REPL (with tweaks
;;                     for prompts and printouts); turn off with
;;                     durendal-disable-slime-repl-font-lock.

;; Call durendal-enable to turn on all Durendal features.

;;; TODO:

;; * ns-unmap binding
;; * ns-cleanup?
;; * when class-not-found, search classpath
;; * bury *SLIME Compilation* buffer on successful compile
;; * reload-all
;; * fix sort-ns to support wrapped lines
;; * search for vars that are referenced but not refered; offer to insert refer
;; * remove unused refers
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

(defvar durendal-port 4005
  "The port of the currently-launching swank server.
Terrible hack workaround for the fact that elisp lacks fscking closures.")

(defvar durendal-auto-compile? t
  "Automatically compile on save when applicable.")

;; ns stuff

(defmacro durendal-with-section (section &rest body)
  `(save-excursion
     (goto-char (point-min))
     (while (search-forward (concat ,section " ") nil t)
       (back-to-indentation)
       (mark-sexp)
       (save-restriction
         (narrow-to-region (point) (mark))
         ,@body
         (goto-char (point-max))))))

(defmacro durendal-within-sexp (&rest body)
  `(save-excursion
     (mark-sexp)
     (save-restriction
       (narrow-to-region (+ (point) 1) (- (mark) 1))
       ,@body)))

(defun durendal-sort-words (start end)
  (let ((sorted (sort (split-string (buffer-substring-no-properties start end)) 'string<)))
    (delete-region start end)
    (insert (mapconcat 'identity sorted " "))))

(defun durendal-sort-subsection (subsection)
  (goto-char (point-min))
  (when (search-forward (concat subsection " ") nil t)
    (durendal-within-sexp
     (durendal-sort-words (point-min) (point-max)))))

(defun durendal-sort-libspec ()
  (durendal-within-sexp
   (when (and (search-forward-regexp "\\s " nil t)
              (not (= (char-after) (string-to-char ":"))))
     (durendal-sort-words (point) (point-max)))))

(defun durendal-sort-libspecs ()
  (while (search-forward-regexp "[(\\[]\\S +\\." nil t)
    (search-backward-regexp "[(\\[]" nil t)
    (durendal-sort-libspec)
    (forward-sexp)))

(defun durendal-sort-section (section)
  (durendal-with-section
   section
   (mapc 'durendal-sort-subsection '(":only" ":exclude"))
   (goto-char (point-min))
   (search-forward-regexp "(\\S +\\S+ ")
   (durendal-sort-libspecs)
   (goto-char (point-min))
   ;;(forward-word) -- refer-clojure
   ;;(forward-symbol) -- bizarre-error
   (search-forward-regexp "(\\S +\\S+ ")
   (insert "\n")
   (indent-region (point-min) (point-max))
   ;; TODO: handle wrapped lines
   (sort-lines nil (point) (- (point-max) 1))
   (goto-char (point-min))
   (setq h (buffer-substring (point) (point-max)))
   (join-line t)))

;;;###autoload
(defun durendal-sort-ns ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "(ns ")
      (back-to-indentation)
      (durendal-within-sexp
       (mapc 'durendal-sort-section '(":use" ":require" ":import" ":refer" ":refer-clojure")))
      (mark-defun)
      (indent-region (point) (mark)))))

;; launcher

;;;###autoload
(defun durendal-jack-in (&optional port-prompt)
  (interactive "P")
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (setq durendal-port (if port-prompt
                            (string-to-number (read-string "Port: "
                                                           nil nil slime-port))
                          slime-port))
    (message "Launching lein swank on %s..." durendal-port)
    (when (not root)
      (error "Not in a Leiningen project."))
    (shell-command (format "cd %s && lein swank %s &" root durendal-port)
                   "*lein-swank*")
    (set-process-filter (get-buffer-process "*lein-swank*")
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (slime-connect "localhost" durendal-port)
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
                  (remove-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
                  (remove-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
                  (durendal-disable-slime-repl-font-lock))))))

(provide 'durendal) ;;; durendal.el ends here
