  )

(use-package go-translate
  :quelpa (go-translate :fetcher github :repo "lorniu/go-translate")
  :custom
  (go-translate-token-current (cons 430675 2721866130)))

(use-package shell
  :hook
  (evil-after-load
   . (lambda () (evil-set-initial-state 'shell-mode 'emacs))))

(use-package counsel-tramp :ensure t)

(use-package annotate :ensure t)
