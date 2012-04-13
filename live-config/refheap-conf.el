;;Create a file called ~/.refheap-pass with the following (use your own credentials):
;;(custom-set-variables '(refheap-token "your token") '(refheap-user "your username"))

(add-live-lib-path "refheap")
(if (file-exists-p "~/.refheap-pass")
    (load "~/.refheap-pass"))

(require 'refheap)
