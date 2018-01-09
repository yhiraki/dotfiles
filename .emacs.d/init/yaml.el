(use-package yaml-mode
             :ensure t
             :config
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)
             :mode
             ("\\.yml\\'" . yaml-mode)
             ("\\.yaml\\'" . yaml-mode))
