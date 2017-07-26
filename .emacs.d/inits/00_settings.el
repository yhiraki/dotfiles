;; path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; git gutter
(global-git-gutter-mode +1)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
; (require 'yasnippet)
(yas-global-mode 1)

; ;; pyenv
; (defun projectile-pyenv-mode-set ()
;   "Set pyenv version matching project name."
;   (let ((project (projectile-project-name)))
;     (if (member project (pyenv-mode-versions))
;         (pyenv-mode-set project)
;       (pyenv-mode-unset))))
;
; (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

; ;; which key
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)

;; all-the-icons
;; (require 'all-the-icons)
