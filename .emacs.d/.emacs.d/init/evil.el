(setq-default evil-want-C-u-scroll t
              evil-want-fine-undo t
              evil-search-module 'evil-search
              evil-ex-search-vim-style-regexp t)

(el-get-bundle! evil)

(el-get-bundle evil-leader
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "bs" 'elscreen-start
    "bc" 'elscreen-create
    "bn" 'elscreen-next
    "bp" 'elscreen-previous
    "bk" 'elscreen-kill
    "fr" 'helm-recentf
    "fd" 'direx:jump-to-directory-other-window
    "fj" 'open-junk-file
    "fc" 'org-capture
    "fgh" 'helm-ghq
    "fgl" 'helm-ls-git-ls
    "fgg" 'helm-git-grep
    "fI" 'find-user-init-file
    "fb" 'helm-mini
    "gs" 'magit-status
    "r" 'quickrun
    "el" 'flycheck-error-list
    "\\r" 'restart-emacs
    )
  ;; Note: You should enable global-evil-leader-mode before you enable evil-mode
  (global-evil-leader-mode)
  (evil-mode 1)
  )

(el-get-bundle evil-surround
  (global-evil-surround-mode 1)

  ;; python
  (evil-define-key 'normal python-mode-map
    (kbd "gd") 'jedi:goto-definition
    (kbd "K") 'jedi:show-doc)
  )

(el-get-bundle evil-nerd-commenter)
(el-get-bundle evil-exchange)
(el-get-bundle evil-magit)
;; (el-get-bundle evil-args)
;; (el-get-bundle tarao-evil-plugins
;;   :type github :pkgname "tarao/evil-plugins")
(el-get-bundle! evil-commentary
  (evil-commentary-mode)
  )

(el-get-bundle! evil-matchit
  (global-evil-matchit-mode 1)
  )

;; ;; https://github.com/kluge/spacemacs.d/blob/264a3d3d3b6dc93e7e57212a149be396da79775f/layers/kluge/funcs.el#L12
;; (defun my-org-meta-return ()
;;   "org-meta-return and insert state"
;;   (interactive)
;;   (end-of-line)
;;   (org-meta-return)
;;   (evil-insert 1))

;; (defun my-org-insert-todo-heading ()
;;   (interactive)
;;   (end-of-line)
;;   (org-insert-todo-heading)
;;   (evil-insert 1))

;; (defun org-mode-hooks ()
;;   (evil-define-key 'normal evil-org-mode-map
;;     (kbd "M-<return>") 'my-org-meta-return
;;     (kbd "M-S-<return>") 'my-org-insert-todo-heading
;;     (kbd "\\x") 'org-toggle-checkbox
;;     )
;;   (linum-mode -1)
;;   )

;; (add-hook 'org-mode-hook 'org-mode-hooks)
