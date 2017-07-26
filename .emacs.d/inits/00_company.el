;; http://qiita.com/sune2/items/b73037f9e85962f5afb7

(require 'company)
(global-company-mode)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (company-flx-mode +1))

;; vars
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)

;; mappings
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; 候補のソート順
(add-hook 'after-init-hook 'company-statistics-mode)
(require 'company-statistics)
(company-statistics-mode)
(setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))

;; python
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
