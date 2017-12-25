;; http://blog.ichiroc.in/entry/2013/09/06/075832

;; Google日本語入力をベースにする
;; これがないと(mac-toggle-input-method t) で、ことえりが有効になってしまう。
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
(add-hook 'evil-normal-state-entry-hook
          '(lambda ()
             (mac-toggle-input-method nil)))
(add-hook 'evil-normal-state-entry-hook 'mac-change-language-to-us)

;; ミニバッファを開いたときに英字にする（閉じてもモードは戻らない）
(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
