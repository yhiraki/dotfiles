(use-package tern
  :ensure t
  :hook ((js2-mode web-mode) . tern-mode)
  :config
  (setq tern-command (append tern-command '("--no-port-file")))
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  )

(use-package company-tern
  :ensure t
  )

(use-package js2-mode
  :ensure t
  :defer t
  :init
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook
            '(lambda()
               (add-to-list 'company-backends 'company-tern)
               ))
  :mode (("\\.js\\'" . js2-mode))
  )

(use-package js2-refactor
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil)
  )

(use-package xref-js2
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook
            '(lambda ()
               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package add-node-modules-path
  :ensure t
  :hook (js2-mode typescript-mode)
  )

(use-package vue-mode
  :ensure t
  :mode (("\\.vue\\'" . vue-mode))
  )
