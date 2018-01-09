(use-package elscreen
             :ensure t
             :config
             (setq elscreen-tab-display-kill-screen nil ;; タブ全消しをしない
                   elscreen-tab-display-control nil)
             (add-hook 'elscreen-screen-update-hook
                       '(lambda ()
                          (setq elscreen-display-tab (if (elscreen-one-screen-p) nil t))))
             (elscreen-start))
