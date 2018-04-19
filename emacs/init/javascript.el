(use-package company-tern
  :init
  (setq company-tern-property-marker "")
  (add-hook 'js-mode-hook 'tern-mode)
  :config
  (add-to-list 'company-backends 'company-tern) ; backendに追加
  )
