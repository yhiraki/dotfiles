(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-fine-undo t
        evil-search-module 'evil-search
        evil-ex-search-vim-style-regexp t
        )
  (modify-syntax-entry ?_ "w" (standard-syntax-table))
  :bind
  (:map evil-normal-state-map
        ( "C-l" . 'evil-ex-nohighlight)
        ( "/" . 'swiper)
        ( "Y" . "y$"))
  (:map evil-insert-state-map
        ( "C-k" . 'company-yasnippet))
  (:map evil-visual-state-map
        ( "gs" . 'google-this-region))
  :config
  ;; direx
  (evil-define-key 'normal direx:direx-mode-map
    (kbd "q") 'evil-window-delete
    (kbd "D") 'direx:do-delete-files
    (kbd "r") 'direx:refresh-whole-tree
    (kbd "R") 'direx:do-rename-file
    (kbd "c") 'direx:do-copy-files
    (kbd "j") 'direx:next-item
    (kbd "k") 'direx:previous-item
    (kbd "C-j") 'direx:next-sibling-item
    (kbd "C-k") 'direx:previous-sibling-item
    (kbd "SPC") 'direx:toggle-item
    (kbd "o") 'direx:maybe-find-item
    (kbd "RET") 'direx:find-item
    (kbd "P") 'direx-project:jump-to-project-root)
  ;; quickrun
  (evil-define-key 'normal quickrun--mode-map
    (kbd "q") 'evil-window-delete)
  ;; python
  (evil-define-key 'normal python-mode-map
    (kbd "gd") 'jedi:goto-definition
    (kbd "gb") 'jedi:goto-definition-pop-marker
    (kbd "K") 'jedi:show-doc
    (kbd ",f") 'py-autopep8)
  (evil-define-key 'visual python-mode-map
    (kbd ",f") 'py-autopep8-region)
  ;; go
  (evil-define-key 'normal go-mode-map
    (kbd "gd") 'godef-jump)
  ;; markdown
  (evil-define-key 'normal markdown-mode-map
    (kbd ",1") 'markdown-insert-header-setext-1
    (kbd ",2") 'markdown-insert-header-setext-2
    (kbd ",-") 'markdown-insert-hr
    (kbd ",c") 'markdown-insert-gfm-code-block
    (kbd "zc") 'markdown-hide-subtree
    (kbd "zo") 'markdown-show-subtree
    (kbd "TAB") 'markdown-cycle
    )
  ;; org-evil
  (evil-define-key 'normal org-mode-map
    (kbd ",u.") (kbd "i C-u C-c .")  ; org-time-stamp with datetime
    (kbd ",u!") (kbd "i C-u C-c !")  ; org-time-stamp-inactive with datetime
    (kbd ",.") 'org-time-stamp
    (kbd ",!") 'org-time-stamp-inactive
    (kbd ",d") 'org-deadline
    (kbd ",s") 'org-schedule
    (kbd ",o") 'org-open-at-point
    (kbd ",p") 'org-priority
    (kbd ",q") 'org-set-tags-command
    (kbd ",t") 'org-todo
    (kbd ",x") 'org-toggle-checkbox
    )
  (evil-define-key 'insert org-mode-map
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-h") 'org-metaleft
    (kbd "M-l") 'org-metaright
    )
  (evil-define-key 'visual org-mode-map
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-h") 'org-metaleft
    (kbd "M-l") 'org-metaright
    )
  ;; js
  (evil-define-key 'normal js2-mode-map
    (kbd "zc") 'js2-mode-hide-element
    (kbd "zo") 'js2-mode-show-element
    )
  ;; web
  (evil-define-key 'normal web-mode-map
    (kbd "zc") 'web-mode-fold-or-unfold
    (kbd "zo") 'web-mode-fold-or-unfold
    )
  ;; org-agenda
  ;; https://gist.github.com/amirrajan/301e74dc844a4c9ffc3830dc4268f177
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "<RET>") 'org-agenda-switch-to
    (kbd "\t") 'org-agenda-goto
    (kbd "q") 'org-agenda-quit
    (kbd "r") 'org-agenda-redo
    (kbd "S") 'org-save-all-org-buffers
    (kbd "gj") 'org-agenda-goto-date
    (kbd "gJ") 'org-agenda-clock-goto
    (kbd "gm") 'org-agenda-bulk-mark
    (kbd "go") 'org-agenda-open-link
    (kbd "s") 'org-agenda-schedule
    (kbd "+") 'org-agenda-priority-up
    (kbd ",") 'org-agenda-priority
    (kbd "-") 'org-agenda-priority-down
    (kbd "y") 'org-agenda-todo-yesterday
    (kbd "n") 'org-agenda-add-note
    (kbd "t") 'org-agenda-todo
    (kbd ":") 'org-agenda-set-tags
    (kbd ";") 'org-timer-set-timer
    (kbd "i") 'org-agenda-clock-in-avy
    (kbd "O") 'org-agenda-clock-out-avy
    (kbd "u") 'org-agenda-bulk-unmark
    (kbd "x") 'org-agenda-exit
    (kbd "j")  'org-agenda-next-line
    (kbd "k")  'org-agenda-previous-line
    (kbd "vt") 'org-agenda-toggle-time-grid
    (kbd "va") 'org-agenda-archives-mode
    (kbd "vw") 'org-agenda-week-view
    (kbd "vl") 'org-agenda-log-mode
    (kbd "vd") 'org-agenda-day-view
    (kbd "vc") 'org-agenda-show-clocking-issues
    (kbd "g/") 'org-agenda-filter-by-tag
    (kbd "o") 'delete-other-windows
    (kbd "gh") 'org-agenda-holiday
    (kbd "gv") 'org-agenda-view-mode-dispatch
    (kbd "f") 'org-agenda-later
    (kbd "b") 'org-agenda-earlier
    (kbd "e") 'org-agenda-set-effort
    (kbd "n") nil  ; evil-search-next
    (kbd "{") 'org-agenda-manipulate-query-add-re
    (kbd "}") 'org-agenda-manipulate-query-subtract-re
    (kbd "A") 'org-agenda-toggle-archive-tag
    (kbd ".") 'org-agenda-goto-today
    (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line
    (kbd "<") 'org-agenda-filter-by-category
    (kbd ">") 'org-agenda-date-prompt
    (kbd "F") 'org-agenda-follow-mode
    (kbd "D") 'org-agenda-deadline
    (kbd "H") 'org-agenda-holidays
    (kbd "J") 'org-agenda-next-date-line
    (kbd "K") 'org-agenda-previous-date-line
    (kbd "L") 'org-agenda-recenter
    (kbd "P") 'org-agenda-show-priority
    (kbd "R") 'org-agenda-clockreport-mode
    (kbd "Z") 'org-agenda-sunrise-sunset
    (kbd "T") 'org-agenda-show-tags
    (kbd "X") 'org-agenda-clock-cancel
    (kbd "[") 'org-agenda-manipulate-query-add
    (kbd "g\\") 'org-agenda-filter-by-tag-refine
    (kbd "]") 'org-agenda-manipulate-query-subtract)
  ;; flycheck
  (evil-define-key 'normal prog-mode-map
    (kbd "[e") 'flycheck-previous-error
    (kbd "]e") 'flycheck-next-error)
  )

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    (kbd "ag") 'counsel-ag
    (kbd "bc") 'elscreen-create
    (kbd "bk") 'elscreen-kill
    (kbd "bn") 'elscreen-next
    (kbd "bp") 'elscreen-previous
    (kbd "df") 'counsel-describe-function
    (kbd "dv") 'counsel-describe-variable
    (kbd "el") 'flycheck-list-errors
    (kbd "fb") 'ivy-switch-buffer
    (kbd "fd") 'direx:jump-to-directory-other-window
    (kbd "ff") 'counsel-find-file
    (kbd "fj") 'my/open-junk-file
    (kbd "fr") 'counsel-recentf
    (kbd "gf") 'counsel-git
    (kbd "gg") 'counsel-git-grep
    (kbd "gp") 'counsel-ghq
    (kbd "gs") 'magit-status
    (kbd "ll") 'counsel-load-library
    (kbd "lo") 'counsel-locate
    (kbd "ls") 'counsel-info-lookup-symbol
    (kbd "oa") 'org-agenda
    (kbd "ob") 'org-switchb
    (kbd "oc") 'org-capture
    (kbd "ol") 'org-store-link
    (kbd "r") 'quickrun
    (kbd "th") 'twit
    (kbd "tu") 'twittering-update-status-interactive
    (kbd "us") 'counsel-unicode-char
    (kbd "x") 'counsel-M-x
    (kbd "ze") 'eval-buffer
    (kbd "zi") 'find-user-init-file
    (kbd "zr") 'restart-emacs)
  ;; Note: You should enable global-evil-leader-mode before you enable evil-mode
  (global-evil-leader-mode)
  (evil-mode 1)
  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit
  :ensure t
  :after magit
  )

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional todo return)))))

(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-escape
  :ensure t
  :after evil
  )

(use-package evil-numbers
  :ensure t
  :after evil
  )
