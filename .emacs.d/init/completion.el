;; (use-package company-flx)
;; (use-package company-racer)

;; http://qiita.com/sune2/items/b73037f9e85962f5afb7
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-auto-complete nil
        company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-dabbrev-downcase nil)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<tab>") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)))

(use-package company-statistics
  :ensure t
  :config
  ;; 候補のソート順
  (add-hook 'after-init-hook 'company-statistics-mode)
  (company-statistics-mode)
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)))
