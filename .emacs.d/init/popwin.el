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
                   '(;; *compilation*は下部に2割の大きさで表示
                     (compilation-mode :align below :ratio 0.2)
                     ;; ヘルプバッファは右側に表示
                     ("*Help*" :align right)
                     ;; 補完バッファは下部に3割の大きさで表示
                     ("*Completions*" :align below :ratio 0.3)
                     ;; M-x helm-miniは下部に7割の大きさで表示
                     ("*helm mini*" :align below :ratio 0.7)
                     ("*quickrun*" :popup t :select nil)
                     ;; 他のhelmコマンドは右側に表示 (バッファ名の正規表現マッチ)
                     ("\*helm" :regexp t :align right)
                     ;; 上部に表示
                     ("foo" :align above)
                     ;; 別フレームで表示
                     ("bar" :frame t)
                     ;; 同じウィンドウで表示
                     ("baz" :same t)
                     ;; ポップアップで表示
                     ("hoge" :popup t)
                     ;; 選択する
                     ("abc" :select t)
                     ))
             (shackle-mode 1)
             (setq shackle-lighter "")

             ;; C-zで直前のウィンドウ構成に戻す
             (winner-mode 1)
             (global-set-key (kbd "C-z") 'winner-undo))
