(use-package direx
  :ensure t
  :init
  (setq direx:leaf-icon "  "
        direx:open-icon "▾ "
        direx:closed-icon "▸ "))

(use-package wdired
  :ensure t
  :init
  (setq wdired-allow-to-change-permissions t)
  (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
  )
