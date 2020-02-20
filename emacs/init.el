(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq load-path (cons
                 (locate-user-emacs-file "elisp")
                 load-path
                 ))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
        ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; load straight.el
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package global :no-require
  :config
  ;; system-type predicates
  ;; from http://d.hatena.ne.jp/tomoya/20090807/1249601308
  (setq darwin-p   (eq system-type 'darwin)
        linux-p    (eq system-type 'gnu/linux)
        carbon-p   (eq system-type 'mac)
        meadow-p   (featurep 'meadow))
  )

(use-package user-defined-functions :no-require
  :config
  (defun my-open-in-external-app (&optional @fname)
    "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
    (interactive)
    (let* (
           ($file-list
            (if @fname
                (progn (list @fname))
              (if (string-equal major-mode "dired-mode")
                  (dired-get-marked-files)
                (list (buffer-file-name)))))
           ($do-it-p (if (<= (length $file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when $do-it-p
        (cond
         ((string-equal system-type "windows-nt")
          (mapc
           (lambda ($fpath)
             (w32-shell-execute "open" $fpath)) $file-list))
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (shell-command
              (concat "open " (shell-quote-argument $fpath))))  $file-list))
         ((string-equal system-type "gnu/linux")
          (mapc
           (lambda ($fpath) (let ((process-connection-type nil))
                              (start-process "" nil "xdg-open" $fpath))) $file-list))))))
  )

(use-package user-defined-functions-macos :no-require
  :if darwin-p
  :config
  (defun my-open-current-dir ()
    (interactive)
    (shell-command "open ."))
  )

(use-package startup :no-require
  :custom
  (confirm-kill-emacs nil)
  :config
  (setq inhibit-startup-message t)
  (fset 'yes-or-no-p 'y-or-n-p)
  )

(use-package diminish :ensure t)

(use-package server
  :hook (after-init
         . (lambda ()
             (unless
                 (server-running-p)
               (server-start))))
  )

(use-package scroll :no-require
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
  (mouse-wheel-follow-mouse 't) ; scroll window under mouse
  :config
  (setq scroll-conservatively 1)
  (scroll-bar-mode 0)
  )

(use-package menu :no-require
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  )

(use-package cursor :no-require
  :config
  (show-paren-mode 1) ;; 対応する括弧を光らせる
  )

(use-package bell :no-require
  :config
  (setq ring-bell-function 'ignore)
  )

(use-package buffer :no-require
  :config
  (setq-default indicate-buffer-boundaries 'right) ;; バッファの終端を表示
  (setq-default indicate-empty-lines t) ;; バッファの終端以降を可視化
  ;; (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  )

(use-package tempbuf
  :hook ((dired-mode magit-mode) . turn-on-tempbuf-mode)
  )

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t) ; シンボリックリンクの読み込みを許可
  (auto-revert-check-vc-info t) ; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
  (large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
  (tags-revert-without-query 1) ; TAGS ファイルを自動で再読込
  )

(use-package autorevert
  :config
  (global-auto-revert-mode t)
  )

(use-package executable
  :config
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p) ; shegang を見て自動で +x する
  )

(use-package display-line-numbers
  ;; :hook (prog-mode . display-line-numbers-mode)
  ;; :config (setq display-line-numbers-type 'relative)
  )

(use-package cc-vars
  :custom (c-basic-offset 2)
  )

(use-package indent :no-require
  :config
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  )

(use-package syntax :no-require
  :config
  ;; http://tarao.hatenablog.com/entry/20130304/evil_config#vim-word
  (modify-syntax-entry ?_ "w" (standard-syntax-table)) ; 単語境界をvim風に
  )

(use-package meta-key :no-require
  :config
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta)
    )
  )

(use-package font :no-require
  :config
  ;; Osaka + Menlo
  ;; (when (eq system-type 'darwin)
  ;;   (set-face-attribute 'default nil
  ;;                       :family "Menlo"
  ;;                       :height 120)
  ;;   (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Osaka"))
  ;;   (push '("Osaka" . 1.2) face-font-rescale-alist) ; 全角文字を2文字幅に揃える
  ;;   )

  ;; Cica
  (set-face-attribute 'default nil
                      :family "Cica"
                      :height 140
                      )

  ;; http://misohena.jp/blog/2017-09-26-symbol-font-settings-for-emacs25.html
  ;; TODO: インデント可視化用のunicode文字は半角幅にしたいので無効化
  ;; (setq use-default-font-for-symbols nil) ; 記号をデフォルトのフォントにしない ○△□が全角幅になる

  ;; |あいうえお|かきくけこ|
  ;; |１２３４５|一二三四五|
  ;; |①②③④⑤|○△□☆…|
  ;; |　　　　　|😀😀😀😀😀😀| ; TODO: 絵文字の幅がおかしい
  ;; |abcdefghij|klmnopqrst|
  ;; |1234567890|1234567890|

  ;; unicode の一部を1文字幅として扱う
  ;; "┃" : git-gutter
  ;; "│" : highlight-indent-guides
  (set-language-environment "English")
  )

(use-package fira-code-mode :disabled
  :hook ((
         prog-mode
         gfm-mode
         markdown-mode
         org-mode
         ) . (lambda ()
               (when window-system
                 (fira-code-mode)
                 )
               ))
  )

(use-package encoding :no-require
  :config
  ;; magitでの文字化け対策
  (prefer-coding-system 'utf-8)
  )

(use-package files
  :custom
  (require-final-newline t)
  (find-file-visit-truename t)
  :config
  (setq save-silently t)
  )

(use-package find-large-file :no-require
  ;; 巨大なファイルを開いたときに fundamental mode にする
  :hook (find-file . conditional-disable-modes)
  :init
  (defun conditional-disable-modes ()
    (when (> (buffer-size) (* 500 1024))  ; 500KB
      (flycheck-mode -1)
      (font-lock-mode -1)
      (fundamental-mode)
      (which-function-mode -1)
      )
    )
  )

(use-package open-junk-file :ensure t
  :custom
  (open-junk-file-format "~/.cache/junkfile/%Y/%m/%Y-%m%d-%H%M%S.")
  )

(use-package volatile-highlights :ensure t
  :diminish volatile-highlights-mode
  :hook ((prog-mode org-mode) . volatile-highlights-mode)
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  )

(use-package highlight-indent-guides :ensure t
  :diminish highlight-indent-guides-mode
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive t)
  )

(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode-hook lisp-mode-hook) . eldoc-mode)
  )

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package which-key :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-allow-evil-operators t)
  )

(use-package smartparens :ensure t
  :diminish smartparens-mode
  :hook (after-init . smartparens-global-mode)

  :config
  (sp-pair "\{ " " \}")
  (sp-pair "\[ " " \]")

  (sp-with-modes '(lisp-mode emacs-lisp-mode lisp-interaction-mode slime-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil)
    )

  (sp-with-modes '(plantuml-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "% " " %")
    )

  (sp-with-modes '(web-mode html-mode)
    (sp-local-pair "\{% " " %\}")
    (sp-local-pair "\{# " " #\}")
    )

  (sp-with-modes '(sh-mode)
    (sp-local-pair "%\{" "%\}")
    )

  ;; https://github.com/Fuco1/smartparens/issues/80
  (defun my-open-block-c-mode (id action context)
    (when (eq action 'insert)
      (newline)
      (indent-according-to-mode)
      (previous-line)
      (indent-according-to-mode)))

  (sp-with-modes '(prog-mode vue-mode)
    (sp-local-pair  "{" nil :post-handlers '((my-open-block-c-mode "RET")))
    )
  )

(use-package restart-emacs :ensure t
  :commands restart-emacs
  )

(use-package elscreen :ensure t :disabled
  :hook (after-init . elscreen-start)
  :custom
  (elscreen-tab-display-kill-screen nil) ; タブ全消しをしない
  (elscreen-tab-display-control nil)
  )

(use-package s :ensure t)

(use-package f :ensure t)

;; mac port 版の emacs のみ
(use-package ime :no-require :disabled
  :init
  (add-hook 'evil-normal-state-entry-hook
            '(lambda ()
               (mac-toggle-input-method nil)))
  (add-hook 'evil-normal-state-entry-hook 'mac-change-language-to-us)
  ;; ミニバッファを開いたときに英字にする（閉じてもモードは戻らない）
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
  :config
  ;; http://blog.ichiroc.in/entry/2013/09/06/075832
  ;; Google日本語入力をベースにする
  ;; これがないと(mac-toggle-input-method t) で、ことえりが有効になってしまう。
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
  )

(use-package appearance :no-require
  :custom
  (initial-frame-alist
        (append
         '((ns-transparent-titlebar . t) ;; タイトルバーを透過
           (vertical-scroll-bars . nil) ;; スクロールバーを消す
           (ns-appearance . dark) ;; 26.1 {light, dark}
           (internal-border-width . 0) ;; 余白を消す
           ))
        )
  :config
  (setq default-frame-alist initial-frame-alist)
  )

(use-package emojify :ensure t :disabled
  :hook (after-init . global-emojify-mode)
  )

(use-package dired
  :after evil
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (add-hook 'dired-mode-hook
            '(lambda() (setq line-spacing 3)) t t)
  (evil-define-key 'normal dired-mode-map
    (kbd "C-j") 'dired-next-dirline
    (kbd "C-k") 'dired-prev-dirline
    (kbd "G") 'evil-goto-line
    (kbd "SPC") 'hydra-global-leader/body
    (kbd "gg") 'evil-goto-first-line
    (kbd "go") 'my-open-in-external-app
    )
  :bind
  (:map dired-mode-map
        ("G" . nil) ; 何故かハングアップするので無効化
        )
  )

(use-package dired-sidebar :ensure t
  :after evil
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'icons)
  :config
  (evil-define-key 'normal dired-sidebar-mode-map
    (kbd "l") '(lambda () (interactive) (dired-subtree-insert) (dired-sidebar-redisplay-icons))
    (kbd "h") '(lambda () (interactive) (dired-subtree-remove))
    )
  )

(use-package all-the-icons-dired :ensure t
  )

(use-package wdired :ensure t
  :commands (wdired-change-to-wdired-mode)
  :bind
  (:map dired-mode-map
        ("e" . wdired-change-to-wdired-mode))
  :custom (wdired-allow-to-change-permissions t)
  )

(use-package diredfl :ensure t :disabled
  :config (diredfl-global-mode 1)
  )

(use-package flymake
;;   :commands flymake-mode
;;   :custom
;;   (flymake-error-bitmap nil)
;;   (flymake-note-bitmap nil)
;;   (flymake-warning-bitmap nil)
;;   :config
;;   flymakeが起動していてもマーカーを見えなくする。 eglotで勝手にflymakeが起動してしまう対策。
;;   eglot--managed-mode-hook を設定したので様子見
;;   (set-face-underline 'flymake-error nil)
;;   (set-face-underline 'flymake-note nil)
;;   (set-face-underline 'flymake-warning nil)
  )

(use-package flycheck :ensure t
  :after evil
  :hook ((plantuml-mode
          sh-mode)
         . flycheck-mode)
  :custom
  (flycheck-python-flake8-executable "python3")
  (flycheck-python-pycompile-executable "python3")
  (flycheck-python-pylint-executable "python3")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode)

  (flycheck-define-checker c/c++-g++
    "A C/C++ checker using g++."
    :command ("g++" "-Wall" "-Wextra" "-std=c++14" source)
    :error-patterns  ((error line-start
                             (file-name) ":" line ":" column ":" " error: " (message)
                             line-end)
                      (warning line-start
                               (file-name) ":" line ":" column ":" " warning: " (message)
                               line-end))
    :modes (c-mode c++-mode))

  (push 'c/c++-g++ flycheck-checkers)

  ;; (flycheck-define-checker python-pycodestyle
  ;;   "A Python syntax and style checker using pycodestyle (former pep8)."

  ;;   :command ("pycodestyle" source-inplace)
  ;;   :error-patterns
  ;;   ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  ;;   :modes python-mode)

  ;; (push 'python-pycodestyle flycheck-checkers)

  (evil-define-key 'normal flycheck-error-list-mode-map
    (kbd "F") 'flycheck-error-list-reset-filter
    (kbd "RET") 'flycheck-error-list-goto-error
    (kbd "f") 'flycheck-error-list-set-filter
    (kbd "j") 'flycheck-error-list-next-error
    (kbd "k") 'flycheck-error-list-previous-error
    (kbd "n") 'flycheck-error-list-next-error
    (kbd "p") 'flycheck-error-list-previous-error
    (kbd "q") 'quit-window
    )
  )

(use-package flyspell
  :commands flyspell-mode
  :init
  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  (defun flyspell-detect-ispell-args (&optional run-together)
    "if RUN-TOGETHER is true, spell check the CamelCase words."
    (let (args)
      (cond
       ((string-match  "aspell$" ispell-program-name)
        ;; Force the English dictionary for aspell
        ;; Support Camel Case spelling check (tested with aspell 0.6)
        (setq args (list "--sug-mode=ultra" "--lang=en_US"))
        (if run-together
            (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
       ((string-match "hunspell$" ispell-program-name)
        ;; Force the English dictionary for hunspell
        (setq args "-d en_US")))
      args))

  ;; ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
  ;; ;; Please note when you use hunspell, ispell-extra-args will NOT be used.
  ;; ;; Hack ispell-local-dictionary-alist instead.
  ;; (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
  ;; ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
  ;; (defadvice ispell-word (around my-ispell-word activate)
  ;;   (let ((old-ispell-extra-args ispell-extra-args))
  ;;     (ispell-kill-ispell t)
  ;;     (setq ispell-extra-args (flyspell-detect-ispell-args))
  ;;     ad-do-it
  ;;     (setq ispell-extra-args old-ispell-extra-args)
  ;;     (ispell-kill-ispell t)
  ;;     ))

  (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; use emacs original arguments
      (setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      ;; restore our own ispell arguments
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)
      ))

  (defun text-mode-hook-setup ()
    ;; Turn off RUN-TOGETHER option when spell check text-mode
    (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
  (add-hook 'text-mode-hook 'text-mode-hook-setup)

  (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
  (autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
  (autoload 'tex-mode-flyspell-verify "flyspell" "" t)

  :config
  ;; http://keisanbutsuriya.hateblo.jp/entry/2015/02/10/152543
  (setq-default ispell-program-name "aspell")
  (eval-after-load "ispell"
    '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

  (setq-default ispell-extra-args (flyspell-detect-ispell-args t))

  (mapc
   (lambda (hook)
     (add-hook hook 'flyspell-prog-mode))
   '(
     ;; ここに書いたモードではコメント領域のところだけ flyspell-mode が有効になる
     ))
  (mapc
   (lambda (hook)
     (add-hook hook
               '(lambda () (flyspell-mode 1))))
   '(
     ;; ここに書いたモードでは flyspell-mode が有効になる
     ;; company との相性が悪いので一旦停止
     ;; text-mode-hook
     ;; prog-mode-hook
     twittering-edit-mode-hook
     ))
  )

(use-package flyspell-lazy :ensure t
  :hook (flyspell-mode . flyspell-lazy-mode)
  )

(use-package magit :ensure t
  :commands (magit-status)
  :custom (magit-save-repository-buffers nil)
  :config
  ;; magit-commit 時に diff が開くのをやめる
  ;; https://qiita.com/egg_chicken/items/948f8df70069334e8296
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  )

(use-package git-timemachine :ensure t
  :after evil
  :config
  (evil-define-key 'normal git-timemachine-mode-map
    ;; Navigate
    (kbd "p") 'git-timemachine-show-previous-revision
    (kbd "n") 'git-timemachine-show-next-revision
    (kbd "g") 'git-timemachine-show-nth-revision
    (kbd "t") 'git-timemachine-show-revision-fuzzy
    ;; Kill current revision
    (kbd "w") 'git-timemachine-kill-abbreviated-revision
    (kbd "W") 'git-timemachine-kill-revision
    ;; Misc
    (kbd "b") 'git-timemachine-blame
    (kbd "c") 'git-timemachine-show-commit
    (kbd "?") 'git-timemachine-help
    (kbd "q") 'git-timemachine-quit
    )
  )

(use-package git-gutter+ :ensure t
  :diminish
  )

(use-package git-gutter-fringe+ :ensure t
  :after git-gutter+
  :hook (after-init . global-git-gutter+-mode)
  :custom
  (git-gutter+-added-sign "┃")
  (git-gutter+-deleted-sign "▔")
  (git-gutter+-modified-sign "┃")
  :custom-face
  (git-gutter+-modified ((t (:italic nil))))
  (git-gutter+-deleted ((t (:italic nil))))
  (git-gutter+-added ((t (:italic nil))))
  )

(use-package gist :ensure
  :after evil
  :config
  (evil-define-key 'normal gist-list-menu-mode-map
    (kbd "RET") 'gist-fetch-current
    (kbd "*") 'gist-star
    (kbd "+") 'gist-add-buffer
    (kbd "-") 'gist-remove-file
    (kbd "^") 'gist-unstar
    (kbd "b") 'gist-browse-current-url
    (kbd "e") 'gist-edit-current-description
    (kbd "f") 'gist-fork
    (kbd "r") 'gist-list-reload
    (kbd "K") 'gist-kill-current
    (kbd "y") 'gist-print-current-url
    (kbd "<tab>") 'gist-fetch-current-noselect
    (kbd "q") 'quit-window
    ;; ("/p" gist-list-push-visibility-limit)
    ;; ("/t" gist-list-push-tag-limit)
    ;; ("/w" gist-list-pop-limit)
    )
  )

(use-package browse-at-remote :ensure t)

(use-package recentf
  :commands recentf-mode
  :custom
  (recentf-save-file "~/.cache/emacs/recentf")
  (recentf-max-saved-items 2000)
  (recentf-exclude '("/.recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
  (recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  :config
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
  (run-with-idle-timer 30 t '(lambda ()
                               (with-suppressed-message (recentf-save-list))))
  )

(use-package backup :no-require
  :config
  ;; backup
  ;; https://www.emacswiki.org/emacs/BackupDirectory#toc3
  (defun make-backup-file-name (FILE)
    (let ((dirname (concat "~/.cache/emacs/backup"
                           (format-time-string "%y/%m/%d/"))))
      (if (not (file-exists-p dirname))
          (make-directory dirname t))
      (concat dirname (file-name-nondirectory FILE))))
  )

(use-package undohist :ensure t
  :hook (after-init . undohist-initialize)
  :custom
  (undohist-ignored-files '("COMMIT_EDITMSG"))
  )

(use-package undo-tree :ensure t
  :diminish undo-tree-mode)

(use-package ivy :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  :bind (:map ivy-minibuffer-map ([escape] . 'minibuffer-keyboard-quit))
  )

(use-package ivy-hydra :ensure t
  :after ivy)

(use-package ivy-rich :ensure t
  :after (ivy counsel)
  :hook (ivy-mode . ivy-rich-mode)
  )

(use-package counsel :ensure t
  :after ivy
  :custom
  (counsel-yank-pop-separator "\n-------\n")
  )

(use-package swiper :ensure t
  :commands swiper
  )

(use-package ivy-ghq :straight
  (ivy-ghq :type git :host github :repo "analyticd/ivy-ghq")
  :commands (ivy-ghq-open)
  )

(use-package path :no-require
  :config
  (defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

  (set-exec-path-from-shell-PATH)
  )

(use-package pangu-spacing :ensure t
  :custom
  ;; http://onemoreduoa.phpapps.jp/emacs/org-mode
  ;; chinse-two-byte → japanese に置き換えるだけで日本語でも使える
  (pangu-spacing-chinese-before-english-regexp
        (rx (group-n 1 (category japanese))
            (group-n 2 (in "a-zA-Z0-9"))))
  (pangu-spacing-chinese-after-english-regexp
        (rx (group-n 1 (in "a-zA-Z0-9"))
            (group-n 2 (category japanese))))

  ;; 見た目ではなくて実際にスペースを入れる
  (pangu-spacing-real-insert-separtor t)

  ;; text-mode やその派生モード(org-mode 等)のみに使いたいならこれ
  ;; :hook
  ;; (text-mode-hook . pangu-spacing-mode)
  ;; (twittering-edit-mode . pangu-spacing-mode)
  )

(use-package twittering-mode :ensure t
  :commands (twit)
  :init
  ;; master-password を設定する際に注意すること
  ;; https://blog.web-apps.tech/emacs-mac-twittering-mode-every-asked-pin/
  (setq twittering-use-master-password t))

(use-package lsp-mode :ensure t
  :hook ((c++-mode
          go-mode
          js-mode
          python-mode
          ;; sh-mode
          typescript-mode
          vue-mode)
         . lsp)

  :custom
  (lsp-auto-guess-root t)
  (lsp-clients-go-server "gopls")
  (lsp-clients-javascript-typescript-server "typescript-language-server")
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 1)
  )

(use-package lsp-vetur
  :custom
  (lsp-vetur-format-default-formatter-ts "eslint")
  (lsp-vetur-format-default-formatter-js "eslint")
  )

(use-package lsp-pyls
  :custom
  (lsp-pyls-plugins-flake8-enabled t)
  (lsp-pyls-plugins-jedi-completion-include-params nil)
  (lsp-pyls-plugins-pylint-enabled nil)
  )

(use-package lsp-ui :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . #'lsp-ui-peek-find-references)
        )
  )

(use-package company :ensure t
  :diminish company-mode
  :hook (after-init
         . (lambda()
              (global-company-mode)
              (company-tng-configure-default)))

  :custom
  (company-auto-complete nil)
  (company-candidates-cache t)
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-etags-ignore-case t)
  (company-idle-delay 0.2)
  (company-lsp-cache-candidates 'auto)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-tooltip-idle-delay 0.1)
  (company-tooltip-limit 10)
  (completion-ignore-case t)

  :config
  ;; evil でも動くようにする
  ;; https://github.com/expez/company-quickhelp/issues/63
  (add-hook
   'company-completion-started-hook
   '(lambda (&rest ignore)
     (when evil-mode
       (when (evil-insert-state-p)
         (define-key evil-insert-state-map (kbd "C-n") nil)
         (define-key evil-insert-state-map (kbd "C-p") nil)
         ))))

  :bind
  (:map company-active-map
        ("C-S-h" . 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
        ("C-h" . nil) ;; C-hはバックスペース割当のため無効化
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        ("C-s" . 'company-filter-candidates)
        )
  (:map company-search-map
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)

        ;; tab and go のため再割り当て
        ;; ("RET" . 'company-complete)
        ;; ([return] . 'company-complete)
        )
  )

;; company tab and go が動かないのでしばらく無効化
;; https://github.com/company-mode/company-mode/pull/706
(use-package company-box :ensure t :disabled
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  )

(use-package company-lsp :ensure t
  :after (company yasnippet)
  :commands company-lsp
  :config
  ;; https://github.com/tigersoldier/company-lsp/issues/103
  (push '(gopls . nil) company-lsp-filter-candidates )
  (push 'company-lsp company-backends)
  )

(use-package company-statistics :ensure t
  :hook (company-mode . company-statistics-mode)
  :custom
  (company-statistics-file "~/.cache/emacs/company-statistics-cache.el")
  (company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  )

;; リソースを食うので停止
(use-package company-tabnine :ensure t :disabled
  :config
  (add-to-list 'company-backends #'company-tabnine)
  )

(use-package quickrun :ensure t
  :after evil
  :commands quickrun

  :custom
  (quickrun-timeout-seconds 30)

  :config
  (quickrun-set-default "c" "c/gcc")

  (quickrun-add-command "rust/script"
    '((:command . "cargo")
      (:exec    . ("%c script %o %s")))
    :default "rust")

  (quickrun-add-command "c++/g++"
    '((:exec . ("%c -std=c++14 %o -o %e %s" "%e %a"))
      (:compile-only . "%c -Wall -Wextra -std=c++14 %o -o %e %s"))
    :override t)
  (quickrun-set-default "c++" "c++/g++")

  (quickrun-add-command "python3"
    '((:command . "python3")
      (:compile-only . "flake8 %s")))
  (quickrun-set-default "python" "python3")

  (quickrun-add-command "typescript"
    '((:exec . ("%c --target es6 --module commonjs %o %s %a" "node %n.js")))
    :override t)

  (evil-define-key 'normal quickrun--mode-map
    (kbd "q") 'evil-window-delete
    )
  )

(use-package csharp-mode :ensure t
  :mode
  ("\\.cs\\'" . csharp-mode)
  )

(use-package dockerfile-mode :ensure t
  :mode
  ("Dockerfile\\'" . dockerfile-mode))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (emacs-lisp-mode . my/hs-minor-mode-hide-all)
  :config
  (defun my/hs-minor-mode-hide-all ()
    (hs-minor-mode)
    (hs-hide-all)
    )
  )

(use-package elisp-mode
  :mode
  ("\\.el\\'" . emacs-lisp-mode)
  )

(use-package clang-format :ensure t
  :commands clang-format-buffer)

(use-package go-mode :ensure t
  :after evil
  :mode ("\\.go\\'" . go-mode)
  :custom
  (gofmt-command "goimports")
  :config
  (evil-define-key 'normal go-mode-map
    (kbd "\\f") 'gofmt
    )
  (evil-define-key 'visual go-mode-map
    (kbd "\\f") 'gofmt
    )
)

(use-package go-eldoc :ensure t
  :hook (go-mode . go-eldoc-setup)
)

(use-package js
  :custom
  (js-indent-level 2)
  )

(use-package css-mode
  :custom
  (css-indent-offset 2)
  )

(use-package add-node-modules-path :ensure t
  :hook (
         js-mode
         typescript-mode
         web-mode
         vue-mode
         )
  )

(use-package eslint-fix :ensure t
  :commands eslint-fix
  )

(use-package prettierjs :no-require :disabled
  :config
  (defun my/prettier ()
    (interactive)
    (shell-command
     (format "%s --write %s"
             (shell-quote-argument (executable-find "prettier"))
             (shell-quote-argument (expand-file-name buffer-file-name))))
    (revert-buffer t t t))
  )

(use-package json-mode :ensure t
  :after evil
  :config
  (evil-define-key 'normal json-mode-map
    (kbd "\\f") 'json-pretty-print-buffer
    )
  )

(use-package markdown-mode :ensure t
  :after evil
  :custom
  (markdown-command "pandoc -s --self-contained -t html5 -c ~/.emacs.d/css/github.css")
  (markdown-gfm-use-electric-backquote nil)
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  :config
  (evil-define-key 'normal markdown-mode-map
    (kbd "zc") 'markdown-hide-subtree
    (kbd "zo") 'markdown-show-subtree
    (kbd "TAB") 'markdown-cycle
    )
  )

(use-package prog-mode
  :after evil
  :config
  (evil-define-key 'normal prog-mode-map
    (kbd "[e") 'flycheck-previous-error
    (kbd "]e") 'flycheck-next-error
    (kbd "\\f") 'lsp-format-buffer
    (kbd "\\m") 'lsp-ui-imenu
    (kbd "\\qa") 'quickrun-autorun-mode
    (kbd "\\qc") '(lambda () (interactive) (save-buffer) (quickrun-compile-only))
    (kbd "\\qr") '(lambda () (interactive) (save-buffer) (quickrun))
    (kbd "\\qs") '(lambda () (interactive) (save-buffer) (quickrun-shell))
    (kbd "\\r") '(lambda () (interactive) (save-buffer) (quickrun))
    (kbd "gd") 'xref-find-definitions
    (kbd "gr") 'xref-find-references
    ;; (kbd "K") 'eglot-help-at-point
    )
  (evil-define-key 'visual prog-mode-map
    (kbd "\\f") 'lsp-format-region
    (kbd "\\r") 'quickrun-region
    (kbd "\\qr") 'quickrun-region
    )
  )

(use-package view
  :after evil
  :config
  (evil-define-key 'normal view-mode-map
    (kbd "q") 'View-quit
    )
  )

(use-package edit-indirect :ensure t
  :commands edit-indirect-region
  )

(use-package org :ensure org-plus-contrib
  :after evil
  :custom
  (org-directory "~/org/")
  (org-startup-with-inline-images nil)
  (org-src-fontify-natively t)
  (org-hide-leading-stars t) ; 見出しの余分な*を消す
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-log-done 'time) ; DONEの時刻を記録

  :config
  ;; https://www.reddit.com/r/emacs/comments/4golh1/how_to_auto_export_html_when_saving_in_orgmode/?st=jeqpsmte&sh=3faa76e8
  (defun toggle-org-html-export-on-save ()
    (interactive)
    (if (memq 'org-html-export-to-html after-save-hook)
        (progn
          (remove-hook 'after-save-hook 'org-html-export-to-html t)
          (setq org-export-in-background nil)
          (message "Disabled org html export on save for current buffer..."))
      (add-hook 'after-save-hook 'org-html-export-to-html nil t)
      (setq org-export-in-background t)
      (message "Enabled org html export on save for current buffer...")))

  ;; https://emacs.stackexchange.com/questions/32473/edit-org-mode-tags-using-ido-or-ivy-completion
  ;; =C-M-m= to add/remove tag
  ;; =C-M-j= to fix tags
  (global-set-key [remap org-set-tags-command] #'counsel-org-tag)

  (evil-define-key 'normal org-mode-map
    (kbd "C-j") 'org-next-visible-heading
    (kbd "C-k") 'org-previous-visible-heading
    (kbd "M-h") 'org-metaleft
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-l") 'org-metaright
    (kbd "<M-return>") '(lambda () (interactive) (evil-append-line 1) (org-meta-return))
    (kbd "<C-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-heading-after-current))
    (kbd "<M-S-return>") '(lambda () (interactive) (evil-append-line 1) (org-insert-todo-heading 1))
    (kbd "<C-S-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-todo-heading-respect-content))
    (kbd "t") 'org-todo
    (kbd "<") 'org-metaleft
    (kbd ">") 'org-metaright
    (kbd "\\g") 'org-mac-grab-link
    (kbd "\\i") 'org-clock-in
    (kbd "\\p") 'org-priority
    (kbd "\\q") 'org-set-tags-command
    (kbd "\\s") 'org-schedule
    (kbd "\\t") 'org-todo
    (kbd "\\v") 'org-toggle-inline-images
    (kbd "\\xp") 'org-set-property
    (kbd "gh") 'outline-up-heading
    (kbd "gp") 'outline-previous-heading
    (kbd "\\ \\") 'hydra-outline/body
    )

  (evil-define-key 'insert org-mode-map
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-h") 'org-metaleft
    (kbd "M-l") 'org-metaright
    (kbd "RET") 'org-return-indent
    )

  (evil-define-key 'visual org-mode-map
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-h") 'org-metaleft
    (kbd "M-l") 'org-metaright
    )

  :mode (("\\.org\\'" . org-mode))
  )

(use-package org-faces
  :after org
  :custom
  (org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("SOMEDAY" :foreground "dark gray")
     ("DONE" :foreground "forest green" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ))
)

(use-package org-src
  :after org
  :custom
  (org-src-tab-acts-natively t) ; src block をインデントする
  )

(use-package org-agenda
  :after (evil org)
  :commands (org-agenda org-refile)
  :init
  (add-hook 'org-agenda-mode-hook
            '(lambda()
               (custom-set-variables '(org-agenda-files (list
                                       org-directory
                                       (concat org-directory "projects"))))
               ))
  :custom
  (org-agenda-current-time-string "← now")
  (org-agenda-time-grid ;; Format is changed from 9.1
   '((daily today require-timed)
     (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
     "-"
     "────────────────"))
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  :config
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "+") 'org-agenda-priority-up
    (kbd "-") 'org-agenda-priority-down
    (kbd ".") 'org-agenda-goto-today
    (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line
    (kbd ":") 'org-agenda-set-tags
    (kbd ";") 'org-timer-set-timer
    (kbd "<") 'org-agenda-filter-by-category
    (kbd "<RET>") 'org-agenda-switch-to
    (kbd ">") 'org-agenda-date-prompt
    (kbd "A") 'org-agenda-toggle-archive-tag
    (kbd "D") 'org-agenda-deadline
    (kbd "F") 'org-agenda-follow-mode
    (kbd "H") 'org-agenda-holidays
    (kbd "J") 'org-agenda-next-date-line
    (kbd "K") 'org-agenda-previous-date-line
    (kbd "L") 'org-agenda-recenter
    (kbd "O") 'org-agenda-clock-out
    (kbd "P") 'org-agenda-show-priority
    (kbd "R") 'org-agenda-clockreport-mode
    ;; (kbd "S") 'org-save-all-org-buffers
    (kbd "T") 'org-agenda-show-tags
    (kbd "X") 'org-agenda-clock-cancel
    (kbd "Z") 'org-agenda-sunrise-sunset
    (kbd "[") 'org-agenda-manipulate-query-add
    (kbd "\\t") 'org-agenda-goto
    (kbd "]") 'org-agenda-manipulate-query-subtract
    ;; (kbd "b") 'org-agenda-earlier
    ;; (kbd "e") 'org-agenda-set-effort
    ;; (kbd "f") 'org-agenda-later
    (kbd "g/") 'org-agenda-filter-by-tag
    (kbd "gJ") 'org-agenda-clock-goto
    (kbd "g\\") 'org-agenda-filter-by-tag-refine
    (kbd "gh") 'org-agenda-holiday
    (kbd "gj") 'org-agenda-goto-date
    (kbd "gm") 'org-agenda-bulk-mark
    (kbd "go") 'org-agenda-open-link
    (kbd "gv") 'org-agenda-view-mode-dispatch
    (kbd "I") 'org-agenda-clock-in
    (kbd "j") 'org-agenda-next-line
    (kbd "k") 'org-agenda-previous-line
    (kbd "N") 'org-agenda-add-note
    (kbd "o") 'delete-other-windows
    (kbd "p") 'org-agenda-priority
    (kbd "q") 'org-agenda-quit
    (kbd "r") 'org-agenda-redo
    (kbd "s") 'org-agenda-schedule
    (kbd "t") 'org-agenda-todo
    (kbd "u") 'org-agenda-bulk-unmark
    (kbd "va") 'org-agenda-archives-mode
    (kbd "vc") 'org-agenda-show-clocking-issues
    (kbd "vd") 'org-agenda-day-view
    (kbd "vl") 'org-agenda-log-mode
    (kbd "vt") 'org-agenda-toggle-time-grid
    (kbd "vw") 'org-agenda-week-view
    ;; (kbd "x") 'org-agenda-exit
    (kbd "y") 'org-agenda-todo-yesterday
    ;; (kbd "{") 'org-agenda-manipulate-query-add-re
    ;; (kbd "}") 'org-agenda-manipulate-query-subtract-re
    )
  )

(use-package japanese-holidays :ensure t
  :hook
  (calendar-today-visible . japanese-holiday-mark-weekend)
  (calendar-today-invisible . japanese-holiday-mark-weekend)
)

(use-package holidays
  :after japanese-holidays
  :custom
  (calendar-holidays
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  )

(use-package calendar
  :custom
  (calendar-mark-holidays-flag t)
  )

(use-package ob
  :custom
  (org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  :config
  ;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("python" "shell" "plantuml" "uml" "shell" "dot" "js" "C" "C++"))))

  (push '("ts" . typescript) org-src-lang-modes)
  (push '("console" . sh) org-src-lang-modes)
  (push '("uml" . plantuml) org-src-lang-modes)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (plantuml . t) (shell . t) (dot . t) (js . t) (C . t))
   )

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  )

(use-package ob-plantuml
  :after (ob plantuml-mode s)
  :custom
  (org-plantuml-jar-path plantuml-jar-path)
  (plantuml-server-url nil)
  :config
  (push (cons ':java plantuml-java-options) org-babel-default-header-args:plantuml)
  (push (cons ':cmdline (s-join " " plantuml-jar-args)) org-babel-default-header-args:plantuml)
  (push '(:async) org-babel-default-header-args:plantuml)
  (push '(:cache . "yes") org-babel-default-header-args:plantuml)
  )

(use-package ob-shell
  :after ob
  :config
  (push '(:async) org-babel-default-header-args:shell)
  )

(use-package ob-python
  :after ob
  :custom
  (org-babel-python-command "python3")
  :config
  (push '(:session . "default") org-babel-default-header-args:python)
  )

(use-package ob-C
  :after ob
  :custom
  (org-babel-default-header-args:C '((:async) (:cache . "yes")))
  (org-babel-default-header-args:C++
   (append org-babel-default-header-args:C '((:includes . "<iostream>"))))
  )

(use-package ob-async :ensure t
  :after ob
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
        '(lambda ()
           (setq org-plantuml-jar-path "~/lib/java/plantuml.jar")))
  )

(use-package ob-ipython :ensure t :disabled
  :after ob)

(use-package ob-js
  :config
  (setq org-babel-js-function-wrapper
        "require('util').inspect(function(){\n%s\n}());")
  )

(use-package org-capture
  :commands org-capture

  :config
  (setq org-capture-templates
        '(
          ("i" "Inbox\t\t- Add entry to Inbox"
           entry (file+headline "~/org/inbox.org" "Inbox")
           "** %?\n\t:PROPERTIES:\n\t:CREATED: %U\n\t:END:\n\t\n\t")

          ("n" "Note\t\t- Taking note to source code"
           entry (file+headline "~/org/notes.org" "Notes")
           "** %?\n\t:PROPERTIES:\n\t:CREATED: %U\n\t:END:\n\t%a\n\t")

          ("b" "Book\t\t- Books wish list"
           table-line (file+headline "~/org/books.org" "wish list")
           "|名前|価格|電子版|追加日|\n|%?|||%U|" :table-line-pos "II-1")

          ("j" "Journal\t- Short logs like Twitter"
           entry (file+olp+datetree "~/org/journal.org")
           "* %?\n\t:PROPERTIES:\n\t:CREATED: %U\n\t:END:\n\t%a\n\t")

          ("B" "Blog\t\t- Hugo post"
           plain (file+olp "~/org/blog.org" "Blog Ideas")
           "hugo%?")
          )
        )
  )

(use-package org-bullets :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-reveal :ensure t :disabled
  :init
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  )

(use-package org-download :ensure t
  :custom
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-method 'attach)
  )

(use-package org-mac-link
  :bind
  (:map org-mode-map
        ("C-c g" . org-mac-grab-link)
        ))

(use-package org-trello :ensure t
  ;; https://org-trello.github.io/usage.html#automatic-org-trello-files-in-emacs
  :hook
  (org-mode
   . (lambda ()
       (let ((filename (buffer-file-name (current-buffer))))
         (when (and filename (string= "trello" (file-name-extension filename)))
           (org-trello-mode)))))
  (org-trello-mode
   . (lambda ()
       (org-trello-sync-buffer t)
       ))
  :mode ("\\.trello$" . org-mode)
  )

(use-package ox-publish
  :after org
  :config
  (setq org-publish-directory "~/public_html/")
  )

(use-package ox-confluence
  :after ox
  )

(use-package ox-gfm :ensure t
  :after ox
  )

(use-package ox-hugo :ensure t
  :after ox
  )

(use-package ox-html
  :custom
  (org-html-htmlize-output-type 'css)
  )

(use-package ox-rst :ensure t
  :after ox
  )

(use-package electric
  :hook (python-mode . electric-indent-mode)
  )

(use-package cc-mode
  :after evil
  :mode (("\\.cpp\\'" . c++-mode))
  :config
  (setq-default sp-escape-quotes-after-insert nil)
  (evil-define-key 'normal c++-mode-map
    (kbd "\\f") 'clang-format-buffer
    )
  (evil-define-key 'visual c++-mode-map
    (kbd "\\f") 'clang-format-region
    )
  )

(use-package python :ensure t
  :after evil
  :mode (("\\.py\\'" . python-mode))
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-m IPython --simple-prompt -i")
  :config
  (evil-define-key 'normal python-mode-map
    (kbd "\\i") 'py-isort-buffer
    ;; (kbd "\\f") 'py-yapf-buffer
    )
  (evil-define-key 'visual python-mode-map
    (kbd "\\i") 'py-isort-region
    )
  )

(use-package py-yapf :ensure t
  :commands (py-yapf-buffer)
  )

(use-package py-isort :ensure t
  :commands (py-isort-buffer py-isort-region)
  )

(use-package sh-script
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+)
  :mode
  ("\\.?sh\\'" . shell-script-mode)
  )

(use-package sql
  :config
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  (setq sql-postgres-login-params (append sql-postgres-login-params '(port)))
  :mode
  ("\\.sql\\'" . sql-mode)
  )

(use-package sql-indent :ensure t
  :after sql
  :commands sqlind-setup
  :init
  (add-hook 'sqlind-minor-mode-hook
            '(lambda ()
              (setq sqlind-indentation-offsets-alist
                    my-sql-indentation-offsets-alist)))
  (add-hook 'sql-mode-hook
            '(lambda ()
              (sqlind-minor-mode)
              (sql-set-product "postgres")
              ))
  (add-hook 'sql-interactive-mode-hook
            '(lambda ()
              (toggle-truncate-lines t)))

  ;; https://github.com/xlighting/happy-emacs.d/blob/12e8369cd7934600703b61bb1c278d77dab0c3a2/modules/init-sql.el
  (defun sql-add-newline-first (output)
    "In a SQLi buffer,The table formatting is ugly because the top boundary of the
    table is printed on the same row as the the prompt,This fixes it"
    (replace-regexp-in-string "\\(\\w+[ ]?\\[\\((?[[:alpha:]])?\\|_\\)+\\][#>][ ]?\\)\\(.*[#>] \\)?" "\\1\n" output))
  (defun sqli-add-hooks ()
    "Add hooks to `sql-interactive-mode-hook'."
    (add-hook 'comint-preoutput-filter-functions
              'sql-add-newline-first))
  (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
  :config
  (setq sql-indent-offset 2)
  (defvar my-sql-indentation-offsets-alist
    `((select-clause)
      (insert-clause 0)
      (delete-clause 0)
      (update-clause 0)
      (select-column + sqlind-adjust-comma)
      (select-join-condition +)
      (in-select-clause + sqlind-lineup-close-paren-to-open-indentation)
      (select-table-continuation + sqlind-lineup-close-paren-to-open-indentation)
      ,@sqlind-default-indentation-offsets-alist))
  )

(use-package toml-mode :ensure t
  :mode ("\\.toml\\'")
  )

(use-package typescript-mode :ensure t
  :custom
  (typescript-indent-level 2)
  :mode ("\\.ts\\'")
  )

(use-package plantuml-mode :ensure t
  :init
  (setq plantuml-java-options "-Djava.awt.headless=true") ; plantuml-modeのdefaultになったけどob-plantumlで使う
  (setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar")) ; ob-plantumlで使う
  (setq plantuml-jar-args
        (list
         "-charset" "UTF-8"
         ;; "-config" (expand-file-name "~/.config/plantuml/color.uml")
         )) ; ob-plantumlで使う

  :custom
  (plantuml-default-exec-mode 'jar)

  :config
  ;; (setq plantuml-output-type "svg")

  ;; plantumlをpngで保存する関数
  (defun plantuml-save-png ()
    (interactive)
    (when (buffer-modified-p)
      (map-y-or-n-p "Save this buffer before executing PlantUML?"
                    'save-buffer (list (current-buffer))))
    (let ((code (buffer-string))
          out-file
          cmd)
      (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
        (setq out-file (match-string 1 code)))
      (setq cmd (concat
                 plantuml-java-options " "
                 (shell-quote-argument plantuml-jar-path) " "
                 (and out-file (concat "-t" (file-name-extension out-file))) " "
                 (s-join " " plantuml-jar-args) " "
                 (f-dirname (buffer-file-name))
                 ))
      (message cmd)
      (call-process-shell-command cmd nil 0)))

  :mode
  ("\\.uml\\'" . plantuml-mode)
  ("\\.plantuml\\'" . plantuml-mode)
  :bind
  ("C-c C-s" . 'plantuml-save-png)
  )

(use-package flycheck-plantuml :ensure t
  :hook (plantuml-mode . flycheck-plantuml-setup)
  )

(use-package web-mode :ensure t
  :after evil
  :custom
  (web-mode-attr-indent-offset nil)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  :config
  (evil-define-key 'normal web-mode-map
    (kbd "\\R") 'web-mode-element-rename
    (kbd "zc") 'web-mode-fold-or-unfold
    (kbd "zo") 'web-mode-fold-or-unfold
    )
  :mode
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ;; ("\\.vue\\'" . web-mode)
  )

(use-package vue-mode :ensure t
  :after evil
  :mode
  ("\\.vue\\'" . vue-mode)
  :hook
  (vue-mode
   . (lambda ()
       (setq syntax-ppss-table nil)
       (add-hook 'after-save-hook 'mmm-parse-buffer nil t)
       ))
  :config
  (evil-define-key 'normal vue-mode-map
    (kbd "\\f") 'eslint-fix
    )
  )

(use-package mmm-mode
  :diminish
  :custom
  (mmm-submode-decoration-level 0)
  )

(use-package yaml-mode :ensure t
  :bind
  (:map yaml-mode-map ("\C-m" . 'newline-and-indent))
  :mode
  ("\\.ya?ml\\'")
  )

(use-package vimrc-mode :ensure t
  :mode
  ("\\.vim\\(rc\\)?\\'" . vimrc-mode)
  )

(use-package image-mode
  :after evil
  :config
  (evil-define-key 'normal image-mode-map
    (kbd "q") 'evil-quit
    )
  )

(use-package emmet-mode :ensure t
  :hook (sgml-mode css-mode web-mode xml-mode)
  :custom
  (emmet-indent-after-insert nil)
)

(use-package gitignore-mode :ensure t
  :mode
  ("\\(\\.git\\|docker\\)ignore\\'" . gitignore-mode)
  )

(use-package smartrep :ensure t
  :after evil
  :config
  (smartrep-define-key evil-normal-state-map
      "C-c" '(("+" . 'evil-numbers/inc-at-pt)
              ("=" . 'evil-numbers/inc-at-pt)
              ("-" . 'evil-numbers/dec-at-pt)
              ))
  )

(use-package hydra :ensure t
  :config

  (defhydra hydra-file-open (:exit t)
    ("b" switch-to-buffer "buffer")
    ("d" dired-sidebar-toggle-sidebar "sidebar")
    ("f" counsel-find-file "find file")
    ("g" counsel-rg "grep")
    ("j" open-junk-file "junk file")
    ("o" my-open-current-dir "open dir")
    ("r" counsel-recentf "rencetf")
    )

  (defhydra hydra-git (:exit t)
    ("f" counsel-git "find")
    ("g" counsel-git-grep "grep")
    ("lb" gist-buffer "gist buffer")
    ("ll" gist-list "gist list")
    ("p" ivy-ghq-open "ghq")
    ("s" magit-status "status")
    ("t" git-timemachine "timemachine")
    ("o" browse-at-remote "browse at remote")
    )

  (defhydra hydra-help (:exit t)
    ("f" counsel-describe-function "function")
    ("k" counsel-descbinds "bind")
    ("v" counsel-describe-variable "variable")
    )

  (defhydra hydra-org (:exit t)
    ("a" org-agenda "agenda")
    ("b" org-switchb "buffer")
    ("c" org-capture "capture")
    ("l" org-store-link "store link")
    ;; org clock
    ("I" org-clock-in "clock in")
    ("O" org-clock-out "clock out")
    ("Q" org-clock-cancel "clock cancel")
    ("j" org-clock-goto "clock goto")
    )

  (defhydra hydra-twitter (:exit t)
    ("h" twit "home")
    ("m" twittering-mentions-timeline "mentions")
    ("u" twittering-update-status-interactive "update")
    )

  (defhydra hydra-emacs-operation (:exit t)
    ("e" eval-buffer "eval-buffer")
    ("i" (progn (interactive) (find-file user-init-file)) "init.el")
    ("k" kill-emacs "kill emacs")
    ("r" restart-emacs "restart emacs")
    )

  (defhydra hydra-narrow (:exit t)
    ("b" org-narrow-to-block "blodk")
    ("e" org-narrow-to-element "element")
    ("f" narrow-to-defun "defun")
    ("s" org-narrow-to-subtree "subtree")
    ("w" widen "widen")
    )

  (defhydra hydra-global-leader (:exit t)
    ("G" hydra-google/body "google")
    ("a" org-agenda "org-agenda")
    ("c" org-capture "org-cature")
    ("el" flycheck-list-errors "error")
    ("f" hydra-file-open/body "find file")
    ("g" hydra-git/body "git")
    ("h" hydra-help/body "help")
    ("k" kill-this-buffer "kill buffer")
    ("n" hydra-narrow/body "narrow")
    ("o" hydra-org/body "org")
    ("q" nil "quit")
    ("t" hydra-twitter/body "twitter")
    ("u" undo-tree-visualize "undotree")
    ("z" hydra-emacs-operation/body "emacs")
    )

;; Use smartrep instead
;;   (defhydra hydra-inc-dec-number ("C-c" :hint nil)
;;     "
;; decrement _-_ 41←42→43 _+_ increment
;; "
;;     ("+" evil-numbers/inc-at-pt)
;;     ("=" evil-numbers/inc-at-pt)
;;     ("-" evil-numbers/dec-at-pt))

  (defhydra hydra-operate-window (:hint nil)
    "
 ^command^ |^^^^^^  size   |^^^^^^^^  move
 ^^--------|^^^^^^---------|^^^^^^^^---------
  _s_plit  | ^ ^  _+_  ^ ^ | ^ ^  ^_j_^  ^ ^
  _v_plist | _<_  _=_  _>_ | _h_  ^^^ ^  _l_
  _c_lose  | ^ ^  _-_  ^ ^ | ^ ^  ^_k_^  ^ ^
  close _o_ther windows
"

    ;; size
    ("+" evil-window-increase-height)
    ("-" evil-window-decrease-height)
    ("<" evil-window-decrease-width)
    ("=" balance-windows)
    (">" evil-window-increase-width)

    ;; move
    ("j" (progn (evil-window-down 1) (hydra-operate-window-no-move/body)) :exit t)
    ("k" (progn (evil-window-up 1) (hydra-operate-window-no-move/body)) :exit t)
    ("h" (progn (evil-window-left 1) (hydra-operate-window-no-move/body)) :exit t)
    ("l" (progn (evil-window-right 1) (hydra-operate-window-no-move/body)) :exit t)

    ;; operate
    ("c" evil-window-delete :exit t)
    ("o" delete-other-windows :exit t)
    ("s" evil-window-split)
    ("v" evil-window-vsplit)

    ("q" nil)
    ("<ESC>" nil)
    )

  (defhydra hydra-operate-window-no-move (:hint nil)
    "
 ^command^ |^^^^^^  size
 ^^--------|^^^^^^---------
  _s_plit  | ^ ^  ^_+_
  _v_plist | _<_  _=_  _>_
  _c_lose  | ^ ^  ^_-_
  close _o_ther windows
"

    ;; size
    ("+" (progn (evil-window-increase-height 5) (hydra-operate-window/body)) :exit t)
    ("-" (progn (evil-window-decrease-height 5) (hydra-operate-window/body)) :exit t)
    ("<" (progn (evil-window-decrease-width 5) (hydra-operate-window/body)) :exit t)
    ("=" (progn (balance-windows) (hydra-operate-window/body)) :exit t )
    (">" (progn (evil-window-increase-width 5) (hydra-operate-window/body)) :exit t)

    ;; operate
    ("c" evil-window-delete :exit t)
    ("o" delete-other-windows :exit t)
    ("s" (progn (evil-window-split) (hydra-operate-window/body)) :exit t)
    ("v" (progn (evil-window-vsplit) (hydra-operate-window/body)) :exit t)

    ("q" nil)
    ("<ESC>" nil)
    )

  (defhydra hydra-elscreen (:hint nil)
    "
^^^^    move    |   ^modify^    |  ^preferance^   |
^^^^------------|^^-------------|^^---------------|----------------------
jump to _0_-_9_ | _C_lone       | _r_ename        | _f_ind file
_b_uffer    ^ ^ | _K_ill others | _T_ab hide/show | _F_ind file (RO)
_n_ext      ^ ^ | _c_reate      | ^ ^             |
_p_revious  ^ ^ | _d_elete      | ^ ^             |
^ ^         ^ ^ | _s_plit       | ^ ^             | _q_uit
"

    ;; move
    ("0" elscreen-jump)
    ("1" elscreen-jump)
    ("2" elscreen-jump)
    ("3" elscreen-jump)
    ("4" elscreen-jump)
    ("5" elscreen-jump)
    ("6" elscreen-jump)
    ("7" elscreen-jump)
    ("8" elscreen-jump)
    ("9" elscreen-jump)
    ("b" elscreen-find-and-goto-by-buffer)
    ("n" elscreen-next)
    ("p" elscreen-previous)

    ;; modify
    ("C" elscreen-clone)
    ("K" elscreen-kill-others)
    ("c" elscreen-create :exit t)
    ;; ("k" elscreen-kill)
    ("d" elscreen-kill)
    ("s" elscreen-split :exit t)

    ;; preferance
    ("T" elscreen-toggle-display-tab)
    ("r" elscreen-screen-nickname)

    ;; other
    ("f" elscreen-find-file)
    ("F" elscreen-find-file-read-only)

    ;; quit
    ("q" nil)

    ;; evil
    ("i" evil-insert-state :exit t)
    ("a" evil-append :exit t)
    ("j" evil-next-line :exit t)
    ("k" evil-previous-line :exit t)
    ("l" evil-forward-char :exit t)
    ("h" evil-backward-char :exit t)
    )

  (defhydra hydra-google (:exit t)
    ("g" google-this)
    ("t" google-translate-enja-or-jaen)
    )

  (defhydra hydra-outline (:hint nil)
    "
^^^^^^^^^^^^^^^^         move       |
^^^^^^^^^^^^^^^^--------------------|
  ^ ^  ^_j_^  ^ ^ | ^ ^  ^_J_^  ^ ^ | _O_ widen
  _h_  ^^^ ^  _l_ | _H_  ^^^ ^  _L_ | _o_ narrow
  ^ ^  ^_k_^  ^ ^ | ^ ^  ^_K_^  ^ ^ | _RET_ show
"

    ("j" outline-next-visible-heading)
    ("k" outline-previous-visible-heading)
    ("h" (progn (outline-up-heading 1) (outline-hide-subtree)))
    ("l" outline-show-children)

    ("J" outline-forward-same-level)
    ("K" outline-backward-same-level)
    ("H" outline-up-heading)
    ("L" (progn (outline-show-children) (outline-next-visible-heading 1)))

    ("o" narrow-to-defun)
    ("O" widen)
    ("RET" outline-show-entry)
    )

  )

(use-package evil :ensure t
  :after hydra
  :hook (after-init . evil-mode)

  :bind
  (:map evil-normal-state-map
        ("/" . 'swiper)
        ("C-a" . evil-numbers/inc-at-pt)
        ;; ("C-x" . evil-numbers/dec-at-pt)
        ("C-j" . 'evil-forward-paragraph)
        ("C-k" . 'evil-backward-paragraph)
        ("C-l" . 'evil-ex-nohighlight)
        ("S-C-j" . 'evil-forward-section-begin)
        ("S-C-k" . 'evil-backward-section-begin)
        ("Y" . "y$")
        ("<SPC>" . 'hydra-global-leader/body)
        ;; ("C-w" .'hydra-operate-window/body)
        ;; ("C-b" . 'hydra-elscreen/body)
        )

  (:map evil-insert-state-map
        ("C-k" . 'company-yasnippet)
        ("C-u" . (lambda ()
                   (interactive)
                   (evil-delete (point-at-bol) (point))))
        )

  (:map evil-visual-state-map
        ("C-a" . 'evil-numbers/inc-at-pt)
        ("C-x" . 'evil-numbers/dec-at-pt)
        ("SPC" . 'hydra-global-leader/body)
        ("g C-a" . 'evil-numbers/inc-at-pt-incremental)
        ("g C-x" . 'evil-numbers/dec-at-pt-incremental)
        )

  :custom
  (evil-ex-search-vim-style-regexp t)
  (evil-search-module 'evil-search)
  (evil-want-C-i-jump t)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo 'fine)
  (evil-toggle-key "C-M-z")

  :config
  (modify-syntax-entry ?_ "w" (standard-syntax-table))
  (evil-declare-change-repeat 'company-complete)

  ;; https://gist.github.com/amirrajan/301e74dc844a4c9ffc3830dc4268f177
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-set-initial-state 'snippet-mode 'insert)
  (evil-set-initial-state 'org-capture-mode 'insert)
  (evil-set-initial-state 'gist-list-mode 'insert)
  (evil-set-initial-state 'git-timemachine-mode 'insert)
  )

(use-package evil-jumps-push-on-find-file :no-require
  :after evil
  :init
  (add-hook 'dired-mode-hook 'my-rename-dired-buffer)
  :config
  ;; https://www.reddit.com/r/emacs/comments/8rg6zk/question_add_dired_buffers_to_evil_jump_list/
  (defun my-rename-dired-buffer ()
    (interactive)
    (unless (string-match-p "Dired:" (buffer-name))
      (rename-buffer (concat "Dired:" (buffer-name)))))

  (setq evil--jumps-buffer-targets "\\(\\*\\(\\new\\|scratch\\)\\*\\|Dired:.+\\)")
  (evil-add-command-properties #'dired-find-file :jump t)
  )

(use-package evil-surround :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-magit :ensure t
  :after (evil magit)

  ;; :config
  ;; (defun my/echo-disabled ()
  ;;   "Echo disabled."
  ;;   (interactive)
  ;;   (message "Disabled")
  ;;   )

  ;; (evil-define-key 'normal magit-mode-map
  ;;   (kbd "x") 'my/echo-disabled
  ;;   )
  ;; (evil-define-key 'visual magit-mode-map
  ;;   (kbd "x") 'my/echo-disabled
  ;;   )
  )

(use-package evil-commentary :ensure t
  :diminish evil-commentary-mode
  :after evil
  :config
  (evil-commentary-mode)
  )

(use-package evil-matchit :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1)
  )

(use-package evil-lion :ensure t
  :after evil
  :config
  (evil-lion-mode)
  )

(use-package evil-escape :ensure t
  :after evil
  )

(use-package evil-numbers :straight
  (evil-numbers :type git :host github :repo "janpath/evil-numbers")
  :after evil
  )

(use-package popwin :ensure t
  :hook (after-init . popwin-mode)
  :config
  (push '("*Error*") popwin:special-display-config)
  (push '("*Org Src" :regexp t) popwin:special-display-config)
  (push '("*Help*" :position right :width 0.5) popwin:special-display-config)
  (push '("*Org-Babel Error Output*") popwin:special-display-config)
  (push '("*quickrun*" :regexp t :position bottom :dedicated t) popwin:special-display-config)
  (push '("*xref*" :position bottom ) popwin:special-display-config)
  (push '("magit:*" :regexp t :position bottom :height 0.5) popwin:special-display-config)
  ;; (push '(image-mode) popwin:special-display-config)
  )

(use-package all-the-icons :ensure t)

(use-package theme :no-require
  :after (color-theme-sanityinc-tomorrow smart-mode-line)
  :hook
  (after-init
   . (lambda ()
       ;; (set-frame-parameter nil 'alpha 90)
       (load-theme 'sanityinc-tomorrow-bright t)
       (sml/setup)
       ))
  )

(use-package color-theme-sanityinc-tomorrow :ensure t)

(use-package smart-mode-line :ensure t)

(use-package hide-mode-line :ensure t
  :hook ((
          dired-mode
          dired-sidebar-mode
          gist-list-mode
          git-commit-mode
          image-mode
          inferior-python-mode
          magit-mode
          org-agenda-mode
          org-capture-mode
          org-export-stack-mode
          quickrun--mode
          ) . hide-mode-line-mode)
  )

(use-package beacon :ensure t
  :hook (after-init . beacon-mode)
  :diminish
  )

(use-package whitespace
  :diminish whitespace-mode
  :hook ((emacs-lisp-mode org-mode gfm-mode markdown-mode) . whitespace-mode)

  :custom
  ;; http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
  (whitespace-style '(face           ; faceで可視化
                      trailing       ; 行末
                      tabs           ; タブ
                      spaces         ; スペース
                      empty          ; 先頭/末尾の空行
                      space-mark     ; 表示のマッピング
                      tab-mark))
  (whitespace-display-mappings
   '((space-mark ?\u3000 [?\u25a1])
     ;; WARNING: the mapping below has a problem.
     ;; When a TAB occupies exactly one column, it will display the
     ;; character ?\xBB at that column followed by a TAB which goes to
     ;; the next TAB column.
     ;; If this is a problem for you, please, comment the line below.
     (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-space-regexp "\\(\u3000+\\)") ; スペースは全角のみを可視化
  ;; (whitespace-action '(auto-cleanup)) ; 保存時に自動でクリーンアップ

  :config
  (set-face-attribute 'whitespace-trailing nil
                      :background "controlBackgroundColor"
                      :foreground "DeepPink"
                      :inherit 'default
                      :underline t
                      )
  (set-face-attribute 'whitespace-tab nil
                      :background "controlBackgroundColor"
                      )
  (set-face-attribute 'whitespace-space nil
                      :background "controlBackgroundColor"
                      :foreground "GreenYellow"
                      :weight 'bold
                      )
  (set-face-attribute 'whitespace-empty nil
                      :background "controlBackgroundColor"
                      :foreground "DeepPink"
                      :underline t
                      )

  (set-display-table-slot standard-display-table 'truncation ?<) ; set lcs=extends:<,precedes:<
  (setcar (nthcdr 2 (assq 'space-mark whitespace-display-mappings)) [?_]) ; set nbsp:%
  )

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :hook
  (after-init . yas-global-mode)
  (yas-minor-mode
   . (lambda ()
       (setq yas-prompt-functions '(yas-x-prompt yas-completing-prompt yas-no-prompt))
       ))
  (yas-before-expand-snippet . evil-insert-state)

  :custom
  (require-final-newline nil)
  (yas-indent-line 'fixed)

  :config
  (setq yas-snippet-dirs (list (locate-user-emacs-file "snippets")))

  ;;; C-k で表示させるので無効化
  ;; YASnippet のスニペットを候補に表示するための設定
  ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backends.")
  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;       backend
  ;;     (append (if (consp backend) backend (list backend))
  ;;             '(:with company-yasnippet))))
  ;; (defun set-yas-as-company-backend ()
  ;;   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;;   )
  ;; (add-hook 'company-mode-hook 'set-yas-as-company-backend)

  ;; company tab and go 経由だと確定時に展開してくれないので return をバインドする
  ;; yas-maybe-expand を :bind で設定する方法がわからん…
  (define-key yas-minor-mode-map (kbd "RET") yas-maybe-expand)

  :bind
  (:map yas-keymap
        ("<tab>" . nil)
        ("RET" . yas-next-field-or-maybe-expand))
  )

(use-package google-this :ensure t
  :commands google-this
  )

(use-package google-translate :ensure t
  :commands google-translate-translate
  :config
  ;; http://emacs.rubikitch.com/google-translate/
  (defvar google-translate-english-chars "[:ascii:]’“”–"
    "これらの文字が含まれているときは英語とみなす")

  (defun google-translate-enja-or-jaen (&optional string)
    "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))
  )

(use-package yasnippet-snippets :ensure t :disabled)

(use-package popup :ensure t
  :commands (popup-next popup-previous)
  :init
  ;; use popup menu for yas-choose-value
  ;; https://www.emacswiki.org/emacs/Yasnippet
  (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
    (when (featurep 'popup)
      (popup-menu*
       (mapcar
        (lambda (choice)
          (popup-make-item
           (or (and display-fn (funcall display-fn choice))
               choice)
           :value choice))
        choices)
       :prompt prompt
       ;; start isearch mode immediately
       :isearch t
       )))
  :bind
  (:map popup-menu-keymap
        ("C-n" . popup-next)
        ("TAB" . popup-next)
        ("<tab>" . popup-next)
        ("<backtab>" . popup-previous)
        ("C-p" . popup-previous))
  )

(use-package autoinsert
  :hook (find-file . auto-insert)

  :custom
  (auto-insert-query nil)
  (auto-insert-directory (concat user-emacs-directory "templates/"))

  :config
  (defun my/autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas/expand-snippet (buffer-string) (point-min) (point-max)))

  ;; http://emacs.rubikitch.com/sd1602-autoinsert-yatemplate-yasnippet/
  (dolist (x auto-insert-alist)
    (when (equal "\\.el\\'" (car-safe (car x)))
      (setcar (car x) "/src/.+\\.el\\'")))

  (setq auto-insert-alist
        (append '(
                  (("test_.*\\.py\\'" "Python test") . ["test.py" my/autoinsert-yas-expand])
                  (("setup.py\\'" "Python setup file") . "setup.py")
                  (("setup.cfg\\'" "Python setup config") . ["setup.cfg" my/autoinsert-yas-expand])
                  (("\\.py\\'" "Python script") . ["template.py" my/autoinsert-yas-expand])
                  (("\\.vue\\'" "Vue") . ["template.vue" my/autoinsert-yas-expand])
                  ((plantuml-mode "Plantuml") . ["template.plantuml" my/autoinsert-yas-expand])
                  ((sh-mode "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                  )
                auto-insert-alist)
        )
  )

(use-package emacs-lock
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  )

(use-package key-binding :no-require
  :config
  ;; (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-\\") nil)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-z") 'universal-argument)
  )

(use-package cus-edit
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  :config
  (load custom-file)
  )
