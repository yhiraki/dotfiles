(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-fine-undo t
        evil-search-module 'evil-search
        evil-ex-search-vim-style-regexp t)
  :bind
  (:map evil-normal-state-map
        ( "C-l" . 'evil-ex-nohighlight)
        ( "/" . 'swiper))
  (:map evil-insert-state-map
        ( "C-k" . 'company-yasnippet))
  :config
  ;; direx
  (evil-define-key 'normal direx:direx-mode-map
    "q" 'evil-window-delete
    "D" 'direx:do-delete-files
    "r" 'direx:refresh-whole-tree
    "R" 'direx:do-rename-file
    "c" 'direx:do-copy-files
    "j" 'direx:next-item
    "k" 'direx:previous-item
    "C-j" 'direx:next-sibling-item
    "C-k" 'direx:previous-sibling-item
    "SPC" 'direx:toggle-item
    "o" 'direx:maybe-find-item
    "RET" 'direx:find-item
    "P" 'direx-project:jump-to-project-root)
  ;; quickrun
  (evil-define-key 'normal quickrun--mode-map
    "q" 'evil-window-delete)
  ;; python
  (evil-define-key 'normal python-mode-map
    "gd" 'jedi:goto-definition
    "K" 'jedi:show-doc)
  ;; markdown
  (evil-define-key 'normal markdown-mode-map
    ",1" 'markdown-insert-header-setext-1
    ",2" 'markdown-insert-header-setext-2
    ",-" 'markdown-insert-hr
    ",c" 'markdown-insert-gfm-code-block)
  ;; org-evil
  (evil-define-key 'normal org-mode-map
    ",p" 'org-priority
    ",t" 'org-todo
    ",q" 'org-priority)
  ;; org-agenda
  ;; https://gist.github.com/amirrajan/301e74dc844a4c9ffc3830dc4268f177
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-define-key 'normal org-agenda-mode-map
    "<RET>" 'org-agenda-switch-to
    "\t" 'org-agenda-goto
    "q" 'org-agenda-quit
    "r" 'org-agenda-redo
    "S" 'org-save-all-org-buffers
    "gj" 'org-agenda-goto-date
    "gJ" 'org-agenda-clock-goto
    "gm" 'org-agenda-bulk-mark
    "go" 'org-agenda-open-link
    "s" 'org-agenda-schedule
    "+" 'org-agenda-priority-up
    "," 'org-agenda-priority
    "-" 'org-agenda-priority-down
    "y" 'org-agenda-todo-yesterday
    "n" 'org-agenda-add-note
    "t" 'org-agenda-todo
    ":" 'org-agenda-set-tags
    ";" 'org-timer-set-timer
    "I" 'helm-org-task-file-headings
    "i" 'org-agenda-clock-in-avy
    "O" 'org-agenda-clock-out-avy
    "u" 'org-agenda-bulk-unmark
    "x" 'org-agenda-exit
    "j"  'org-agenda-next-line
    "k"  'org-agenda-previous-line
    "vt" 'org-agenda-toggle-time-grid
    "va" 'org-agenda-archives-mode
    "vw" 'org-agenda-week-view
    "vl" 'org-agenda-log-mode
    "vd" 'org-agenda-day-view
    "vc" 'org-agenda-show-clocking-issues
    "g/" 'org-agenda-filter-by-tag
    "o" 'delete-other-windows
    "gh" 'org-agenda-holiday
    "gv" 'org-agenda-view-mode-dispatch
    "f" 'org-agenda-later
    "b" 'org-agenda-earlier
    "c" 'helm-org-capture-templates
    "e" 'org-agenda-set-effort
    "n" nil  ; evil-search-next
    "{" 'org-agenda-manipulate-query-add-re
    "}" 'org-agenda-manipulate-query-subtract-re
    "A" 'org-agenda-toggle-archive-tag
    "." 'org-agenda-goto-today
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    "<" 'org-agenda-filter-by-category
    ">" 'org-agenda-date-prompt
    "F" 'org-agenda-follow-mode
    "D" 'org-agenda-deadline
    "H" 'org-agenda-holidays
    "J" 'org-agenda-next-date-line
    "K" 'org-agenda-previous-date-line
    "L" 'org-agenda-recenter
    "P" 'org-agenda-show-priority
    "R" 'org-agenda-clockreport-mode
    "Z" 'org-agenda-sunrise-sunset
    "T" 'org-agenda-show-tags
    "X" 'org-agenda-clock-cancel
    "[" 'org-agenda-manipulate-query-add
    "g\\" 'org-agenda-filter-by-tag-refine
    "]" 'org-agenda-manipulate-query-subtract)
  )

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "ag" 'counsel-ag
    "bc" 'elscreen-create
    "bk" 'elscreen-kill
    "bn" 'elscreen-next
    "bp" 'elscreen-previous
    "bs" 'elscreen-start
    "df" 'counsel-describe-function
    "dv" 'counsel-describe-variable
    "el" 'flycheck-list-errors
    "fb" 'ivy-switch-buffer
    "fd" 'direx:jump-to-directory-other-window
    "ff" 'counsel-find-file
    "fj" 'my/open-junk-file
    "fr" 'counsel-recentf
    "gf" 'counsel-git
    "gg" 'counsel-git-grep
    "gp" 'counsel-ghq
    "gs" 'magit-status
    "ll" 'counsel-load-library
    "lo" 'counsel-locate
    "ls" 'counsel-info-lookup-symbol
    "oa" 'org-agenda
    "ob" 'org-switchb
    "oc" 'org-capture
    "ol" 'org-store-link
    "r" 'quickrun
    "th" 'twit
    "tu" 'twittering-update-status-interactive
    "us" 'counsel-unicode-char
    "ze" 'eval-buffer
    "zi" 'find-user-init-file
    "zr" 'restart-emacs)
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
  :config
  (evil-lion-mode))

(use-package evil-escape :ensure t)
