(use-package shackle
  :ensure t
  :config
  (setq shackle-rules
        '((compilation-mode :align below :ratio 0.2)
          ("*Help*" :align right)
          ("*Completions*" :align below :ratio 0.3)
          ("*quickrun*" :align below :select nil :ratio 0.3)
          ("*magit: \*" :regexp t :align below :ratio 0.3)
          ("*magit-diff: \*" :regexp t :align above :ratio 0.5)
          (direx:direx-mode :popup t :align left :ratio 0.2)
          ))
  (shackle-mode 1)
  (setq shackle-lighter "")

  ;; C-zで直前のウィンドウ構成に戻す
  (winner-mode 1)
  (global-set-key (kbd "C-z") 'winner-undo))
