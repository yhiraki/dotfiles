;; vim に近い操作モードを再現
;; http://tarao.hatenablog.com/entry/20130304/evil_config
(setq evil-want-C-u-scroll t
      evil-want-fine-undo t
      evil-search-module 'evil-search
      evil-ex-search-vim-style-regexp t)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(evil-commentary-mode)

;; (require 'evil-operator-moccur)
;; (global-evil-operator-moccur-mode 1)

;; (require 'evil-relative-linum)

;; (require 'evil-textobj-between)

(require 'open-junk-file)
(setq open-junk-file-format "~/Dropbox/memo/junk/%Y/%m/%Y-%m%d-%H%M%S.")

(require 'evil-magit)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "bs" 'elscreen-start
  "bc" 'elscreen-create
  "bn" 'elscreen-next
  "bp" 'elscreen-previous
  "bk" 'elscreen-kill
  "fr" 'helm-recentf
  "ft" 'neotree-toggle
  "fj" 'open-junk-file
  "fc" 'org-capture
  "fgh" 'helm-ghq
  "fgl" 'helm-ls-git-ls
  "fgg" 'helm-git-grep
  "fb" 'helm-mini
  "gs" 'magit-status
  "r" 'quickrun
  "el" 'flycheck-error-list)

;; neotree
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "gi") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd ".") 'neotree-hidden-file-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "N") 'neotree-create-node)

;; python
(evil-define-key 'normal python-mode-map
  (kbd "gd") 'jedi:goto-definition
  (kbd "K") 'jedi:show-doc))

;; ESC にキャンセルの意味をもたせる
(defun evil-escape-or-quit (&optional prompt)
  (interactive)
  (cond
   ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
        (evil-replace-state-p) (evil-visual-state-p)) [escape])
   (t (kbd "C-g"))))
;; (define-key key-translation-map (kbd "C-c") #'evil-escape-or-quit)
;; (define-key evil-operator-state-map (kbd "C-c") #'evil-escape-or-quit)
(define-key evil-normal-state-map [escape] #'keyboard-quit)

;; increment / decrement
(define-key evil-normal-state-map (kbd "C-c +") #'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") #'evil-numbers/dec-at-pt)

;; registers
(require 'evil-ex-registers)
(define-key evil-ex-search-keymap (kbd "C-r") #'evil-ex-paste-from-register)
(define-key evil-ex-completion-map (kbd "C-r") #'evil-ex-paste-from-register)

;; C-n C-p をハイブリッドに
(defadvice evil-paste-pop (around evil-paste-or-move-line activate)
  ;; evil-paste-popできなかったらprevious-lineする
  "If there is no just-yanked stretch of killed text, just move
to previous line."
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop)
               (call-interactively 'previous-line)
             (signal (car err) (cdr err))))))
(defadvice evil-paste-pop-next (around evil-paste-or-move-line activate)
  ;; evil-paste-pop-nextできなかったらnext-lineする
  "If there is no just-yanked stretch of killed text, just move
to next line."
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop-next)
               (call-interactively 'next-line)
             (signal (car err) (cdr err))))))
