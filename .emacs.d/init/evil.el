(setq evil-want-C-u-scroll t
      evil-want-fine-undo t
      evil-search-module 'evil-search
      evil-ex-search-vim-style-regexp t)

(use-package evil
  :ensure t
  :config
  ;; global
  (define-key evil-normal-state-map (kbd "C-l") 'evil-ex-nohighlight)
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  ;; direx
  (evil-define-key 'normal direx:direx-mode-map
    (kbd "D") 'direx:do-delete-files
    (kbd "r") 'direx:do-rename-file
    (kbd "c") 'direx:do-copy-files
    (kbd "j") 'direx:next-item
    (kbd "k") 'direx:previous-item
    (kbd "C-j") 'direx:next-sibling-item
    (kbd "C-k") 'direx:previous-sibling-item
    (kbd "SPC") 'direx:toggle-item
    (kbd "o") 'direx:maybe-find-item
    (kbd "RET") 'direx:find-item
    (kbd "P") 'direx-project:jump-to-project-root)
  ;; python
  (evil-define-key 'normal python-mode-map
    (kbd "gd") 'jedi:goto-definition
    (kbd "K") 'jedi:show-doc))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "bs" 'elscreen-start
    "bc" 'elscreen-create
    "bn" 'elscreen-next
    "bp" 'elscreen-previous
    "bk" 'elscreen-kill
    "fd" 'direx:jump-to-directory-other-window
    "fj" 'my/open-junk-file
    "fc" 'org-capture
    "fI" 'find-user-init-file
    "gs" 'magit-status
    "r" 'quickrun
    "el" 'flycheck-list-errors
    "\\r" 'restart-emacs
    "ff" 'counsel-find-file
    "fb" 'ivy-switch-buffer
    "fr" 'counsel-recentf
    "us" 'counsel-unicode-char
    "gp" 'counsel-ghq
    "gf" 'counsel-git
    "gg" 'counsel-git-grep
    "ag" 'counsel-ag
    "lo" 'counsel-locate
    "df" 'counsel-describe-function
    "dv" 'counsel-describe-variable
    "ll" 'counsel-load-library
    "ls" 'counsel-info-lookup-symbol
    )
  ;; Note: You should enable global-evil-leader-mode before you enable evil-mode
  (global-evil-leader-mode)
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit :ensure t)

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

;; (use-package somelauw/evil-org)
