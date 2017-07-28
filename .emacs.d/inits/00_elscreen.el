;; ElScreen
(require 'elscreen)
;; (setq elscreen-prefix-key (kbd "C-b"))      ;; これは任意
(setq elscreen-tab-display-kill-screen nil) ;; タブ全消しをしない
(setq elscreen-tab-display-control nil))
(elscreen-start)  ;; ElScreen開始
(elscreen-create) ;; ElScreenのスクリーン作成
