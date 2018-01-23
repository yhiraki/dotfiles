(use-package elscreen
  :ensure t
  :init
  (setq elscreen-tab-display-kill-screen nil ;; タブ全消しをしない
        elscreen-tab-display-control nil)
  :config
  (add-hook 'elscreen-screen-update-hook
            '(lambda ()
               (setq elscreen-display-tab (if (elscreen-one-screen-p) nil t))))
  (elscreen-start))
