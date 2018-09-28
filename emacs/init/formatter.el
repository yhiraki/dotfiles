(use-package prettier-js
  :ensure t
  :defer t
  :hook ((js-mode typescript-mode markdown-mode) . prettier-js-mode)
  )
