(use-package shackle
  :ensure t
  :init
  (setq shackle-rules
        '((compilation-mode :align below :ratio 0.2)
          ("*Help*" :align right)
          ("*Completions*" :align below :ratio 0.3)
          ("*quickrun*" :align below :select nil :ratio 0.3)
          ("*magit: \*" :regexp t :align below :ratio 0.3)
          ("*magit-diff: \*" :regexp t :align above :ratio 0.5)
          (direx:direx-mode :popup t :align left :ratio 32)
          ("*Warnings*" :popup t :align below :ratio 0.1)
          )
        shackle-lighter "")
  :config
  (shackle-mode 1)
  )
