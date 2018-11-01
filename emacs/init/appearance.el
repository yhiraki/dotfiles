;; (use-package symon
;;   :init (el-get-bundle zk-phi/symon)
;;   :config (symon-mode))

(when (memq window-system '(mac ns))
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t) ;; タイトルバーを透過
           (vertical-scroll-bars . nil) ;; スクロールバーを消す
           (ns-appearance . dark) ;; 26.1 {light, dark}
           (internal-border-width . 0))))) ;; 余白を消す
(setq default-frame-alist initial-frame-alist)

;; バッテリ残量をおしゃれに表示
;; https://qiita.com/zk_phi/items/76c950c89428a54ec67d
(add-to-list 'mode-line-format
             '(:eval (let ((bat (read (cdr (assoc ?p (funcall battery-status-function))))))
                       (cond ((> bat 87)  "█") ((> bat 75)  "▇")
                             ((> bat 62)  "▆") ((> bat 50)  "▅")
                             ((> bat 37)  "▄") ((> bat 25)  "▃")
                             ((> bat 12)  "▂") (t           "▁")))))

(use-package sky-color-clock
  :init
  (el-get-bundle zk-phi/sky-color-clock)
  (setq sky-color-clock-enable-emoji-icon nil
        sky-color-clock-format "%m/%d %H:%M")
  :config
  (sky-color-clock-initialize 35))

(use-package emojify
  :ensure t
  :init
  (global-emojify-mode)
  )

;; https://qiita.com/takaxp/items/6ec37f9717e362bef35f
;; カーソルのあるバッファを強調
(use-package dimmer
  :disabled
  :ensure t
  :init
  (setq dimmer-fraction 0.6)
  ;; (setq dimmer-exclusion-regexp "^\\*helm\\|^ \\*Minibuf\\|^\\*Calendar")
  :config
  (dimmer-mode 1)
  (defun dimmer-off ()
    (dimmer-mode -1)
    (dimmer-process-all))
  (defun dimmer-on ()
    (dimmer-mode 1)
    (dimmer-process-all))
  (add-hook 'focus-out-hook #'dimmer-off)
  (add-hook 'focus-in-hook #'dimmer-on)
  )
