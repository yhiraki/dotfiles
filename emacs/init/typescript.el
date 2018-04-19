(use-package typescript-mode
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  )

(use-package tide
  :ensure t
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (flycheck-mode t)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode t)
              (company-mode-on)))
  )
