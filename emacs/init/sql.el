(use-package sql-indent
  :init (el-get-bundle alex-hhh/emacs-sql-indent)
  :config
  (defvar my-sql-indentation-offsets-alist
    `((select-clause 0)
      (insert-clause 0)
      (delete-clause 0)
      (update-clause 0)
      (select-join-condition 0)
      (in-select-clause + sqlind-lineup-close-paren-to-open-indentation)
      (select-table-continuation + sqlind-lineup-close-paren-to-open-indentation)
      ,@sqlind-default-indentation-offsets-alist))
  (add-hook 'sqlind-minor-mode-hook
            (lambda ()
              (setq sqlind-indentation-offsets-alist
                    my-sql-indentation-offsets-alist)))

  (add-hook 'sql-mode-hook
            (lambda ()
              (sqlind-minor-mode)
              (setq-default sql-indent-offset 2
                            indent-tabs-mode nil
                            c-basic-offset 2
                            tab-width 2)
              (sql-set-product "postgres")
              ))

  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))

  ;; https://github.com/xlighting/happy-emacs.d/blob/12e8369cd7934600703b61bb1c278d77dab0c3a2/modules/init-sql.el
  (defun sql-add-newline-first (output)
    "In a SQLi buffer,The table formatting is ugly because the top boundary of the
    table is printed on the same row as the the prompt,This fixes it"
    (replace-regexp-in-string "\\(\\w+[ ]?\\[\\((?[[:alpha:]])?\\|_\\)+\\][#>][ ]?\\)\\(.*[#>] \\)?" "\\1\n" output))

  (defun sqli-add-hooks ()
    "Add hooks to `sql-interactive-mode-hook'."
    (add-hook 'comint-preoutput-filter-functions
              'sql-add-newline-first))

  (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks))
