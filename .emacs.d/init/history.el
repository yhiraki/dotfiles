(setq recentf-save-file "~/.cache/emacs/recentf"
      recentf-max-saved-items 2000
      recentf-exclude '("/.recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/")
      recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
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

;; history
(use-package undohist
             :ensure t
             :config
             (undohist-initialize)
             (setq undohist-ignored-files '("COMMIT_EDITMSG")))
