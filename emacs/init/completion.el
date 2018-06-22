;; (use-package company-flx)
;; (use-package company-racer)

;; http://qiita.com/sune2/items/b73037f9e85962f5AFB7
(use-package company
  :ensure t
  :defer t
  :init
  (setq company-auto-complete nil
        company-idle-delay 0
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-dabbrev-downcase nil)
  (add-hook 'after-init-hook
            '(lambda()
               (global-company-mode)
               ))
  :bind
  (:map company-active-map
        ("<tab>" . nil)
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        ("C-h" . nil) ;; C-hはバックスペース割当のため無効化
        ("C-S-h" . 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
        ))

(use-package company-statistics
  :ensure t
  :after company
  :init
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  :config
  ;; 候補のソート順
  (add-hook 'after-init-hook 'company-statistics-mode)
  (company-statistics-mode))
