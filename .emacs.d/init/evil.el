(setq-default evil-want-C-u-scroll t
              evil-want-fine-undo t
              evil-search-module 'evil-search
              evil-ex-search-vim-style-regexp t)

(el-get-bundle evil
  ;; direx
  (evil-define-key 'normal direx:direx-mode-map (kbd "D") 'direx:do-delete-files)
  (evil-define-key 'normal direx:direx-mode-map (kbd "r") 'direx:do-rename-file)
  (evil-define-key 'normal direx:direx-mode-map (kbd "c") 'direx:do-copy-files)
  (evil-define-key 'normal direx:direx-mode-map (kbd "j") 'direx:next-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "k") 'direx:previous-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "C-j") 'direx:next-sibling-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "C-k") 'direx:previous-sibling-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "SPC") 'direx:toggle-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "o") 'direx:maybe-find-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "RET") 'direx:find-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "P") 'direx-project:jump-to-project-root)
  ;; swiper
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  )

(el-get-bundle evil-leader
  (require 'counsel)
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
  (evil-mode 1)
  )

(el-get-bundle evil-surround
  (global-evil-surround-mode 1)
  )

(el-get-bundle evil-exchange)
(el-get-bundle! evil-magit)
;; (el-get-bundle evil-args)
;; (el-get-bundle tarao-evil-plugins
;;   :type github :pkgname "tarao/evil-plugins")
(el-get-bundle! evil-commentary
  (evil-commentary-mode)
  )

(el-get-bundle! evil-matchit
  (global-evil-matchit-mode 1)
  )

;; python
(evil-define-key 'normal python-mode-map
  (kbd "gd") 'jedi:goto-definition
  (kbd "K") 'jedi:show-doc)

;; (el-get-bundle! somelauw/evil-org)
