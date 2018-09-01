;; (use-package symon
;;   :init (el-get-bundle zk-phi/symon)
;;   :config (symon-mode))

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
