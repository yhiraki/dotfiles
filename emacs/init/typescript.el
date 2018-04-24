(use-package tide :ensure t)

(use-package typescript-mode
  :ensure t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode t)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode t)
    (setq tide-completion-ignore-case t)
    (company-mode-on))
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  :mode
  ("\\.ts\\'" . typescript-mode)
  )
