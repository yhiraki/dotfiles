(use-package tide
  :ensure t
  :defer t
  :hook (typescript-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        tide-completion-ignore-case t)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (company-mode 1)
  )

(use-package typescript-mode
  :ensure t
  :defer t
  :mode
  (("\\.ts\\'" . typescript-mode))
  )
