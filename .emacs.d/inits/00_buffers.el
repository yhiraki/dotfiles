;; rainbow delimiters
(require 'rainbow-delimiters)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)


;; Google日本語入力をベースにする
;; これがないと(mac-toggle-input-method t) で、ことえりが有効になってしまう。
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
(add-hook 'evil-normal-state-entry-hook
          '(lambda ()
             (mac-toggle-input-method nil)))
(add-hook 'evil-normal-state-entry-hook 'mac-change-language-to-us)

;; ミニバッファを開いたときに英字にする（閉じてもモードは戻らない）
(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)


;; whitespace
; http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; set lcs=extends:<,precedes:<
(set-display-table-slot standard-display-table 'truncation ?<)

;; set nbsp:%
(setcar (nthcdr 2 (assq 'space-mark whitespace-display-mappings)) [?%])

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
; (setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)
