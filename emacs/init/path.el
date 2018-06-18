;; required for counsel-ghq, etc.
(use-package exec-path-from-shell
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-startup-hook
            '(lambda()
               (when (memq window-system '(mac ns x))
                 (exec-path-from-shell-initialize)))
            )
  )
