(setq-default elscreen-tab-display-kill-screen nil ;; タブ全消しをしない
              elscreen-tab-display-control nil)

(el-get-bundle! elscreen
  (add-hook 'elscreen-screen-update-hook
            '(lambda ()
               (setq elscreen-display-tab (if (elscreen-one-screen-p) nil t))))

  (elscreen-start)
  )
