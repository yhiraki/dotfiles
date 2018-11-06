(use-package smartrep
  :ensure t
  :after evil
  :config
  (smartrep-define-key
      global-map "C-c" '(
      ("+" . 'evil-numbers/inc-at-pt)
      ("=" . 'evil-numbers/inc-at-pt)
      ("-" . 'evil-numbers/dec-at-pt)
      ))
  )
