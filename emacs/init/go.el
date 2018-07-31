(use-package go-mode
  :ensure t
  :defer t
  :init
  (custom-set-variables
   '(company-go-insert-arguments nil))
  :config
  (add-to-list 'company-backends 'company-go)
  (add-hook 'before-save-hook 'gofmt-before-save)
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
