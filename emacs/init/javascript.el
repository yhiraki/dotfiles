(use-package company-tern
  :ensure t
  :defer t
  :init
  (add-hook 'js-mode-hook 'tern-mode)
  )

(use-package js
  :init
  :defer t
  :config
  (add-to-list 'company-backends 'company-tern)
  )
