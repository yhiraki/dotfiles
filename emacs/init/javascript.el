(use-package company-tern
  :ensure t
  :defer t
  :init
  (add-hook 'js-mode-hook 'tern-mode)
  )

(use-package js
  :init
  (setq js-indent-level 2)
  :defer t
  :config
  (add-to-list 'company-backends 'company-tern)
  )

(use-package add-node-modules-path
  :ensure t
  :hook (js-mode typescript-mode)
  )
