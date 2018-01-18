(use-package magit
  :ensure t
  :config
  ;; magit-commmit 時に diff が開くのをやめる
  ;; https://qiita.com/egg_chicken/items/948f8df70069334e8296
  (remove-hook 'server-switch-hook 'magit-commit-diff))

(use-package git-gutter-fringe+
  :ensure t
  :config
  (global-git-gutter+-mode))
