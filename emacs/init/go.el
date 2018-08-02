(use-package go-mode
  :ensure t
  :defer t
  :init
  (custom-set-variables
   '(company-go-insert-arguments nil))
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (add-to-list 'company-backends 'company-go)
)

(use-package company-go
  :ensure t
  :defer t
  )

(use-package go-eldoc
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  :defer t
)
