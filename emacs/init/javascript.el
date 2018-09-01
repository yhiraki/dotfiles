(use-package company-tern
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  )

(use-package js2-mode
  :ensure t
  :init
  (setq js2-basic-offset 2)
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (add-to-list 'company-backends 'company-tern)
  (tern-mode)
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
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
  (add-hook
   'js2-mode-hook
   (lambda ()
     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package add-node-modules-path
  :ensure t
  :hook (js-mode typescript-mode)
  )

(use-package vue-mode
  :ensure t
  :mode (("\\.vue\\'" . vue-mode))
  )
