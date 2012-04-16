;;disable backups of files edited with tramp
(add-to-list 'bkup-backup-directory-info
             (list tramp-file-name-regexp ""))
(setq tramp-bkup-backup-directory-info  nil)
