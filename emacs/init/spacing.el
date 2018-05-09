(use-package pangu-spacing
  :ensure t
  :init
  ;; http://onemoreduoa.phpapps.jp/emacs/org-mode
  ;; chinse-two-byte → japanese に置き換えるだけで日本語でも使える
  (defvar pangu-spacing-chinese-before-english-regexp)
  (setq pangu-spacing-chinese-before-english-regexp
        (rx (group-n 1 (category japanese))
            (group-n 2 (in "a-zA-Z0-9"))))
  (defvar pangu-spacing-chinese-after-english-regexp)
  (setq pangu-spacing-chinese-after-english-regexp
        (rx (group-n 1 (in "a-zA-Z0-9"))
            (group-n 2 (category japanese))))
  ;; 見た目ではなくて実際にスペースを入れる
  (defvar pangu-spacing-real-insert-separtor)
  (setq pangu-spacing-real-insert-separtor t)
  :config
  ;; text-mode やその派生モード(org-mode 等)のみに使いたいならこれ
  (add-hook 'text-mode-hook 'pangu-spacing-mode)
  (add-hook 'twittering-edit-mode-hook 'pangu-spacing-mode)
  )
