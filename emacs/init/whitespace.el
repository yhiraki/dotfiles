;;; http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a

(use-package whitespace
  :init
  (setq whitespace-style '(face           ; faceで可視化
                           trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           empty          ; 先頭/末尾の空行
                           space-mark     ; 表示のマッピング
                           tab-mark))

  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  :config
  ;; set lcs=extends:<,precedes:<
  (set-display-table-slot standard-display-table 'truncation ?<)

  ;; set nbsp:%
  (setcar (nthcdr 2 (assq 'space-mark whitespace-display-mappings)) [?_])

  (set-face-attribute 'whitespace-trailing nil
                      :foreground "DeepPink"
                      :background nil
                      :inverse-video nil
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background nil)
  (set-face-attribute 'whitespace-space nil
                      :background nil
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background nil
                      :foreground "DeepPink"
                      :inverse-video nil
                      :underline t)

  (global-whitespace-mode 1))
