(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (add-hook
   'after-init-hook
   '(lambda()
      (load-theme 'sanityinc-tomorrow-night t)
      ))
  )
