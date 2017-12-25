;; recentf
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '("/.recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
(setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
(run-with-idle-timer 30 t '(lambda ()
                             (with-suppressed-message (recentf-save-list))))
(recentf-mode 1)

;; backup
;; https://www.emacswiki.org/emacs/BackupDirectory#toc3
(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.cache/emacs/backup"
                         (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

                                        ; history
(el-get-bundle! undohist
  (undohist-initialize)
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG"))
  )
