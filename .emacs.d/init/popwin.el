;; (use-package! popwin

;;   (setq display-buffer-alist 'popwin:display-buffer)
;;   (setq popwin:popup-window-position 'bottom)

;;   (add-hook 'dired-load-hook (lambda () (load "dired-x")))

;;   (push '("*quickrun*") popwin:special-display-config)
;;   (push '("*Warnings*") popwin:special-display-config)
;;   (push '("*el-get packages*") popwin:special-display-config)
;;   (push '("^\*helm[\- ].+\*$" :regexp t) popwin:special-display-config)
;;   (push '("^\*magit: .*$" :regexp t) popwin:special-display-config)
;;   (push '(direx:direx-mode :position top :dedicated t) popwin:special-display-config)
;;   (push '(direx:direx-project :position top :dedicated t) popwin:special-display-config)
;;   (push '(dired-mode :position top) popwin:special-display-config)
;;   )


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
