(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

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

;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(when (not (package-installed-p 'el-get))
  (package-install 'el-get))
(require 'el-get)

;; use-package
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(use-package startup :no-require
  :config
  (setq inhibit-startup-message t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (fset 'yes-or-no-p 'y-or-n-p)
  )

(use-package scroll :no-require
  :config
  (setq scroll-conservatively 1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
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

(use-package vscode-icon :ensure t
  :commands (vscode-icon-for-file)
  :config
  ;; Install ImageMagick:
  ;; $ brew install ImageMagick
  ;; A simple example if I want 16x16 icons:
  ;; M-x vscode-icon-convert-and-copy
  ;; 16 RET
  (setq vscode-icon-size 16)
  )

(use-package file-open :no-require
  :config
  (setq vc-follow-symlinks t) ; シンボリックリンクの読み込みを許可
  (setq find-file-visit-truename t) ; 実体を開く
  (setq auto-revert-check-vc-info t) ; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
  (setq large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
  (setq tags-revert-without-query 1) ; TAGS ファイルを自動で再読込
  )

(use-package line-number :no-require
  :config
  (add-hook 'prog-mode-hook
            '(lambda ()
               (linum-mode)
               ;; linumに起因する高速化
               ;; http://d.hatena.ne.jp/daimatz/20120215/1329248780
               (setq linum-delay t)
               (defadvice linum-schedule (around my-linum-schedule () activate)
                 (run-with-idle-timer 0.2 nil #'linum-update-current))
               ))
  )

(use-package indent :no-require
  :config
  (setq-default c-basic-offset 2)
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  (setq-default require-final-newline t)
  )

(use-package syntax :no-require
  :config
  ;; http://tarao.hatenablog.com/entry/20130304/evil_config#vim-word
  (modify-syntax-entry ?_ "w" (standard-syntax-table)) ; 単語境界をvim風に
  )

(use-package messages :no-require
  :config
  ;; エコーエリアや *Messages* バッファにメッセージを表示させたくない
  ;; http://qiita.com/itiut@github/items/d917eafd6ab255629346
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
  )

(use-package open-junk-file :ensure t :defer t
  :commands (my/open-junk-file)
  :config
  (setq open-junk-file-format "~/.cache/junkfile/%Y/%m/%Y-%m%d-%H%M%S.")
  ;; https://github.com/yewton/.emacs.d
  (defun my/open-junk-file (&optional arg)
    "Open junk file using ivy.

When ARG is non-nil search in junk files."
    (interactive "P")
    (let* ((fname (format-time-string open-junk-file-format (current-time)))
           (rel-fname (file-name-nondirectory fname))
           (junk-dir (file-name-directory fname))
           (default-directory junk-dir))
      (cond (arg
             (counsel-ag nil junk-dir "" "[junk]"))
            (t
             (counsel-find-file rel-fname)))))
  )

(use-package volatile-highlights :ensure t :defer t
  :init
  (add-hook 'evil-mode-hook
            '(lambda()
               (volatile-highlights-mode t)
               (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                                     'evil-paste-pop 'evil-move)
               (vhl/install-extension 'evil)
               (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
               (vhl/install-extension 'undo-tree)
               ))
  )

(use-package eldoc :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-mode-hook 'eldoc-mode)
  )

(use-package rainbow-delimiters :ensure t :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package which-key :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  )

(use-package smartparens :ensure t
  :config
  (smartparens-global-mode t)
  )

(use-package restart-emacs :ensure t)

(use-package elscreen :ensure t
  :init
  (add-hook 'elscreen-screen-update-hook
            '(lambda ()
               (setq elscreen-display-tab (if (elscreen-one-screen-p) nil t))))
  (add-hook 'after-init-hook 'elscreen-start)
  :config
  (setq elscreen-tab-display-kill-screen nil) ; タブ全消しをしない
  (setq elscreen-tab-display-control nil)
  )

(use-package s :ensure t)

(use-package f :ensure t)

(use-package my/functions :no-require
  :config
  ;; path 連結
  ;; http://tototoshi.hatenablog.com/entry/20110520/1305906664
  (defun my/file-path-join (&rest paths)
    (reduce #'(lambda (x y) (concat (file-name-as-directory x) y)) paths))

  (defun find-user-init-file ()
    "Edit the `user-init-file', in another window."
    (interactive)
    (find-file-other-window user-init-file))
  )

(use-package ime :no-require
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
  :config
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t) ;; タイトルバーを透過
           (vertical-scroll-bars . nil) ;; スクロールバーを消す
           (ns-appearance . dark) ;; 26.1 {light, dark}
           (internal-border-width . 0) ;; 余白を消す
           ))
        )
  (setq default-frame-alist initial-frame-alist)
  ;; バッテリ残量をおしゃれに表示
  ;; https://qiita.com/zk_phi/items/76c950c89428a54ec67d
  ;; (add-to-list
  ;;  'mode-line-format
  ;;  '(:eval (let ((bat (read (cdr (assoc ?p (funcall battery-status-function))))))
  ;;            (cond ((> bat 87)  "█") ((> bat 75)  "▇")
  ;;                  ((> bat 62)  "▆") ((> bat 50)  "▅")
  ;;                  ((> bat 37)  "▄") ((> bat 25)  "▃")
  ;;                  ((> bat 12)  "▂") (t           "▁")))))
  )

(use-package emojify :ensure t :defer t
  :config
  (global-emojify-mode)
  )

(use-package dired-sidebar :ensure t
  :commands dired-sidebar-toggle-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  )

(use-package projectile :ensure t)

(use-package wdired :ensure t :defer t
  :bind
  (:map dired-mode-map
        ("e" . wdired-change-to-wdired-mode))
  :config
  (setq wdired-allow-to-change-permissions t)
  )

(use-package flycheck :ensure t :defer t
  :hook ((js2-mode python-mode web-mode plantuml-mode) . flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode)
  )

(use-package flyspell
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
     text-mode-hook
     prog-mode-hook
     twittering-edit-mode-hook
     ))
  )

(use-package flyspell-lazy :ensure t
  :config
  (flyspell-lazy-mode 1)
  )

(use-package magit :ensure t :defer t
  :commands (magit-status)
  :config
  ;; magit-commit 時に diff が開くのをやめる
  ;; https://qiita.com/egg_chicken/items/948f8df70069334e8296
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  )

(use-package git-gutter-fringe+ :ensure t :defer t
  :init
  (global-git-gutter+-mode)
  )

(use-package recentf :defer t
  :config
  (setq recentf-save-file "~/.cache/emacs/recentf")
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '("/.recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
  (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  (run-with-idle-timer 30 t '(lambda ()
                               (with-suppressed-message (recentf-save-list))))
  (recentf-mode 1)
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
  :config
  (setq undohist-ignored-files '("COMMIT_EDITMSG"))
  (undohist-initialize))

(use-package ivy :ensure t)

(use-package counsel :ensure t
  :after ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(use-package swiper :ensure t :defer t
  :commands (ivy-mode))

(use-package counsel-ghq :init (el-get-bundle windymelt/counsel-ghq) :defer t
  :commands (counsel-ghq)
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

;; (use-package pangu-spacing :ensure t
;;   :init
;;   ;; http://onemoreduoa.phpapps.jp/emacs/org-mode
;;   ;; chinse-two-byte → japanese に置き換えるだけで日本語でも使える
;;   (defvar pangu-spacing-chinese-before-english-regexp)
;;   (setq pangu-spacing-chinese-before-english-regexp
;;         (rx (group-n 1 (category japanese))
;;             (group-n 2 (in "a-zA-Z0-9"))))
;;   (defvar pangu-spacing-chinese-after-english-regexp)
;;   (setq pangu-spacing-chinese-after-english-regexp
;;         (rx (group-n 1 (in "a-zA-Z0-9"))
;;             (group-n 2 (category japanese))))
;;   ;; 見た目ではなくて実際にスペースを入れる
;;   (defvar pangu-spacing-real-insert-separtor)
;;   (setq pangu-spacing-real-insert-separtor t)
;;   ;; text-mode やその派生モード(org-mode 等)のみに使いたいならこれ
;;   (add-hook 'text-mode-hook 'pangu-spacing-mode)
;;   (add-hook 'twittering-edit-mode-hook 'pangu-spacing-mode)
;;   )

(use-package twittering-mode :ensure t :defer t
  :commands (twit)
  :config
  ;; master-password を設定する際に注意すること
  ;; https://blog.web-apps.tech/emacs-mac-twittering-mode-every-asked-pin/
  (setq twittering-use-master-password t))

(use-package eglot :ensure t :defer t
  )

(use-package company :ensure t :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; http://qiita.com/sune2/items/b73037f9e85962f5AFB7
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
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
        ("<tab>" . nil)
        ("C-S-h" . 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
        ("C-h" . nil) ;; C-hはバックスペース割当のため無効化
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        ("C-s" . 'company-filter-candidates)
        )
  (:map company-search-map
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        )
  )

(use-package company-box :ensure t
  :hook (company-mode . company-box-mode)
  ;; ~/.emacs.d/elpa//company-box-*/images
  ;; $ mogrify -resize 50% *.png
  )

(use-package company-lsp :ensure t :defer t
  :after (eglot company)
  :config
  (push 'company-lsp company-backends)
  )

(use-package company-statistics :ensure t :defer t
  :after (company)
  :init
  (add-hook 'company-mode-hook 'company-statistics-mode)
  :config
  ;; 候補のソート順
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  )

;; (use-package company-flx :disabled t)

;; (use-package company-racer :disabled t)

(use-package modeline :no-require
  :config
  (setq-default
   mode-line-format
   '(;; Position, including warning for 80 columns
     (:propertize "%4l:" face mode-line-position-face)
     (:eval (propertize "%3c" 'face
                        (if (>= (current-column) 80)
                            'mode-line-80col-face
                          'mode-line-position-face)))
     ;; emacsclient [default -- keep?]
     mode-line-client
     ;; read-only or modified status
     (:eval
      (concat "  "
              (cond (buffer-read-only
                     (propertize " RO " 'face 'mode-line-read-only-face))
                    ((buffer-modified-p)
                     (propertize " ** " 'face 'mode-line-modified-face)))))
     "  "
     ;; directory and buffer/file name
     (:propertize (:eval (shorten-directory default-directory 15))
                  face mode-line-folder-face)
     (:propertize "%b"
                  face mode-line-filename-face)
     ;; narrow [default -- keep?]
     " %n "
     ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
     (vc-mode vc-mode)
     "  %["
     (:propertize mode-name
                  face mode-line-mode-face)
     "%] "
     (:eval (propertize (format-mode-line minor-mode-alist)
                        'face 'mode-line-minor-mode-face))
     (:propertize mode-line-process
                  face mode-line-process-face)
     (global-mode-string global-mode-string)
     ))

  ;; Helper function
  (defun shorten-directory (dir max-length)
    "Show up to `max-length' characters of a directory name `dir'."
    (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
          (output ""))
      (when (and path (equal "" (car path)))
        (setq path (cdr path)))
      (while (and path (< (length output) (- max-length 4)))
        (setq output (concat (car path) "/" output))
        (setq path (cdr path)))
      (when path
        (setq output (concat ".../" output)))
      output))

  ;; Extra mode line faces
  (make-face 'mode-line-read-only-face)
  (make-face 'mode-line-modified-face)
  (make-face 'mode-line-folder-face)
  (make-face 'mode-line-filename-face)
  (make-face 'mode-line-position-face)
  (make-face 'mode-line-mode-face)
  (make-face 'mode-line-minor-mode-face)
  (make-face 'mode-line-process-face)
  (make-face 'mode-line-80col-face)

  (set-face-attribute 'mode-line nil
                      :foreground "gray60" :background "gray20"
                      :inverse-video nil
                      :box '(:line-width 6 :color "gray20" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "gray80" :background "gray40"
                      :inverse-video nil
                      :box '(:line-width 6 :color "gray40" :style nil))

  (set-face-attribute 'mode-line-read-only-face nil
                      :inherit 'mode-line-face
                      :foreground "#4271ae"
                      :box '(:line-width 2 :color "#4271ae"))
  (set-face-attribute 'mode-line-modified-face nil
                      :inherit 'mode-line-face
                      :foreground "#c82829"
                      :background "#ffffff"
                      :box '(:line-width 2 :color "#c82829"))
  (set-face-attribute 'mode-line-folder-face nil
                      :inherit 'mode-line-face
                      :foreground "gray60")
  (set-face-attribute 'mode-line-filename-face nil
                      :inherit 'mode-line-face
                      :foreground "#eab700"
                      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
                      :inherit 'mode-line-face
                      :family "Menlo" :height 100)
  (set-face-attribute 'mode-line-mode-face nil
                      :inherit 'mode-line-face
                      :foreground "gray80")
  (set-face-attribute 'mode-line-minor-mode-face nil
                      :inherit 'mode-line-mode-face
                      :foreground "gray40"
                      :height 110)
  (set-face-attribute 'mode-line-process-face nil
                      :inherit 'mode-line-face
                      :foreground "#718c00")
  (set-face-attribute 'mode-line-80col-face nil
                      :inherit 'mode-line-position-face
                      :foreground "black" :background "#eab700")
  )

(use-package quickrun :ensure t :defer t
  :config
  (setq quickrun-timeout-seconds 30)
  (quickrun-add-command "rust/script"
    '((:command . "cargo")
      (:exec    . ("%c script %o %s")))
    :default "rust")
  )

(use-package csharp-mode :ensure t :defer t
  :mode
  ("\\.cs\\'" . csharp-mode)
  )

(use-package dockerfile-mode :ensure t :defer t
  :mode
  ("Dockerfile\\'" . dockerfile-mode))

(use-package elisp-mode :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            '(lambda()
               (hs-minor-mode)
               (hs-hide-all)
               ))
  )

(use-package go-mode :ensure t :defer t
  :init
  (add-hook 'go-mode-hook
            '(lambda()
               (add-hook 'write-contents-functions 'gofmt-before-save)
               (add-to-list 'company-backends 'company-go)
               ))
  :config
  (setq company-go-insert-arguments nil)
  (setq gofmt-command "goimports")
)

(use-package company-go :ensure t :defer t)

(use-package go-eldoc :ensure t :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup)
)

(use-package js2-mode :ensure t :defer t
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook
            '(lambda()
               (add-to-list 'company-backends 'company-tern)
               ))
  :config
  (setq-default js2-basic-offset 2)
  (setq-default js2-highlight-external-variables nil)
  (setq-default js2-include-browser-externs nil)
  (setq-default js2-include-jslint-globals nil)
  (setq-default js2-mode-show-parse-errors nil)
  (setq-default js2-mode-show-strict-warnings nil)
  :mode (("\\.js\\'" . js2-mode))
  )

(use-package js2-refactor :ensure t :defer t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil)
  )

(use-package xref-js2 :ensure t :defer t
  :init
  (add-hook 'js2-mode-hook
            '(lambda ()
               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package tern :ensure t :defer t
  :hook ((js2-mode web-mode) . tern-mode)
  :config
  (setq tern-command (append tern-command '("--no-port-file")))
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  )

(use-package company-tern :ensure t)

(use-package add-node-modules-path :ensure t :defer t
  :hook (js-mode js2-mode typescript-mode web-mode)
  )

(use-package eslint-fix :ensure t :defer t)

(use-package json-mode :ensure t :defer t)

(use-package markdown-mode :ensure t :defer t
  :init
  (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.emacs.d/css/github.css")
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (hide-sublevels 1)
               ;; whitespace-cleanup を無効にする
               (set (make-local-variable 'whitespace-action) nil)
               ))
  :commands
  (markdown-mode gfm-mode)
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  )

(use-package edit-indirect :ensure t)

(use-package org :ensure org-plus-contrib :defer t
  :init
  ;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("python" "shell" "plantuml" "rust"))))

  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

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

  (defun my/get-org-agenda-files ()
    (concatenate
     'list
     (f-files "~/org"
              (lambda (f)
                (string= (f-ext f) "org"))
              'recursive))
    )

  (setq org-directory "~/org/")

  (defun my/get-org-agenda-files ()
    (concatenate
     'list
     (f-files org-directory
              (lambda (f)
                (string= (f-ext f) "org"))
              'recursive))
    )

  (defun my/update-org-agenda-files ()
    (interactive)
    (setq org-agenda-files (my/get-org-agenda-files))
    )
  (my/update-org-agenda-files)

  (setq org-startup-with-inline-images nil)
  (setq org-src-fontify-natively t)
  (setq org-plantuml-jar-path "~/lib/java/plantuml.jar")
  (setq org-default-notes-file "notes.org")
  (setq org-capture-templates
        '(("t" "Task\t\t- TODOs" entry (file+headline "~/org/task.org" "Todos") "** TODO %?%i\n  %a")
          ("m" "Mail\t\t- Mail or text message drafts" entry (file+olp+datetree "~/org/mail.org") "* %?\n  %c\n  %T")
          ("n" "Note\t\t- Notes" entry (file+headline "~/org/notes.org" "Notes") "** %?\n  %a\n  %T")
          ("r" "Reading\t- Web surfing" entry (file+olp+datetree "~/org/reading.org") "* %?\n  %c\n  %T")
          ("j" "Journal\t- Short logs like Twitter" entry (file+olp+datetree "~/org/journal.org") "* %?\n  %c\n  Entered on %U")
          ;; https://ox-hugo.scripter.co/doc/org-capture-setup
          ("b" "Blog\t\t- Hugo post" entry (file+olp "~/org/blog.org" "Blog Ideas")
           (function org-hugo-new-subtree-post-capture-template))
          )
        )
  (setq org-hide-leading-stars t) ; 見出しの余分な*を消す
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s@!)" "WAIT(w@/!)" "|" "DONE(d@!)" "CANCEL(c@/!)")))
  (setq org-log-done 'time) ; DONEの時刻を記録
  (setq org-html-htmlize-output-type 'css)
  (setq org-publish-directory "~/public_html/")
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook
            '(lambda()
               (setq company-minimum-prefix-length 1)
               (push 'company-capf 'company-backends)
               (push 'company-tempo 'company-backends)
               (add-hook 'completion-at-point-functions
                         'pcomplete-completions-at-point nil t)
               ))
  :config
  ;; https://github.com/skuro/plantuml-mode
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (plantuml . t) (shell . t) (dot . t))
   )
  :mode (("\\.org\\'" . org-mode))
  )

(use-package org-tempo
  )

;; (use-package org-reveal :ensure t :disabled t
;;   :init
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;;   )

(use-package ox-confluence
  :after ox
  )

(use-package ox-gfm :ensure t :defer t
  :after ox
  )

(use-package ox-hugo :ensure t :defer t
  :after ox
  )

(use-package ox-rst :ensure t :defer t
  :after ox
  )

(use-package python :ensure t :defer t
  :init
  (add-hook 'python-mode-hook 'electric-indent-mode)
  (add-hook 'python-mode-hook 'eglot-ensure)
  )

(use-package py-yapf :ensure t :defer t
  :after python
  :commands (py-yapf-buffer)
  )

(use-package py-isort :ensure t :defer t
  :after python
  :commands (py-isort-buffer py-isort-region)
  )

(use-package sh-script :defer t
  :init
  (add-hook 'sh-mode-hook 'eglot-ensure)
  :config
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+)
  :mode
  ("\\.zsh\\'" . shell-script-mode)
  )

(use-package sql :defer t
  :config
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  (setq sql-postgres-login-params (append sql-postgres-login-params '(port)))
  (setq sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq tab-width 2)
  )

(use-package sql-indent :ensure t
  :config
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
  )

(use-package toml-mode :ensure t :defer t
  :mode
  (("\\.toml\\'" . toml-mode))
  )

(use-package typescript-mode :ensure t :defer t
  :mode
  (("\\.ts\\'" . typescript-mode))
  )

(use-package tide :ensure t :defer t
  :after typescript-mode
  :init
  (add-hook 'typescript-mode-hook
            '(lambda()
               (tide-setup)
               (eldoc-mode +1)
               (company-mode 1)
               ))
  :hook (typescript-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq tide-completion-ignore-case t)
  )

(use-package plantuml-mode :ensure t :defer t
  :config
  (setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar"))
  (setq plantuml-java-options "-Djava.awt.headless=true")
  (setq plantuml-options "-charset UTF-8")
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
                 "java -Djava.awt.headless=true -jar " plantuml-java-options " "
                 (shell-quote-argument plantuml-jar-path) " "
                 (and out-file (concat "-t" (file-name-extension out-file))) " "
                 plantuml-options " "
                 (f-dirname (buffer-file-name))
                 ))
      (message cmd)
      (call-process-shell-command cmd nil 0)))
  :mode
  ("\\.pu\\'" . plantuml-mode)
  ("\\.uml\\'" . plantuml-mode)
  ("\\.puml\\'" . plantuml-mode)
  ("\\.plantuml\\'" . plantuml-mode)
  :bind
  ("C-c C-s" . 'plantuml-save-png)
  )

(use-package flycheck-plantuml :ensure t
  :commands (flycheck-plantuml-setup)
  :init
  (add-hook 'plantuml-mode-hook 'flycheck-plantuml-setup)
  )

(use-package web-mode :ensure t :defer t
  :init
  (add-hook 'web-mode-hook
            '(lambda()
               (push 'company-tern company-backends)
               ))
  :config
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  :mode
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  )

(use-package yaml-mode :ensure t :defer t
  :bind
  (:map yaml-mode-map ("\C-m" . 'newline-and-indent))
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  )

(use-package emmet-mode :ensure t :defer t
  :hook (sgml-mode css-mode web-mode xml-mode)
)

(use-package evil :ensure t
  :bind
  (:map evil-normal-state-map
        ( "C-l" . 'evil-ex-nohighlight)
        ( "/" . 'swiper)
        ( "Y" . "y$")
        )
  (:map evil-insert-state-map
        ( "C-k" . 'company-yasnippet))
  (:map evil-visual-state-map
        ( "gs" . 'google-this-region))
  :config
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-fine-undo 'fine)
  (modify-syntax-entry ?_ "w" (standard-syntax-table))
  (evil-define-key 'normal direx:direx-mode-map
    (kbd "C-j") 'direx:next-sibling-item
    (kbd "C-k") 'direx:previous-sibling-item
    (kbd "D") 'direx:do-delete-files
    (kbd "P") 'direx-project:jump-to-project-root
    (kbd "R") 'direx:do-rename-file
    (kbd "RET") 'direx:find-item
    (kbd "SPC") 'direx:toggle-item
    (kbd "c") 'direx:do-copy-files
    (kbd "j") 'direx:next-item
    (kbd "k") 'direx:previous-item
    (kbd "o") 'direx:maybe-find-item
    (kbd "q") 'evil-window-delete
    (kbd "r") 'direx:refresh-whole-tree
    )
  (evil-define-key 'normal dired-sidebar-mode-map
    (kbd "l") '(lambda () (interactive) (dired-subtree-insert) (dired-sidebar-redisplay-icons))
    (kbd "h") '(lambda () (interactive) (dired-subtree-remove))
    )
  (evil-define-key 'normal quickrun--mode-map
    (kbd "q") 'evil-window-delete
    )
  (evil-define-key 'normal sh-mode-map
    (kbd "K") 'eglot-help-at-point
    (kbd "gd") 'xref-find-definitions
    )
  (evil-define-key 'normal python-mode-map
    (kbd "K") 'eglot-help-at-point
    (kbd "\\f") 'eglot-format
    (kbd "\\i") 'py-isort-buffer
    (kbd "gd") 'xref-find-definitions
    (kbd "gr") 'eglot-rename
    )
  (evil-define-key 'visual python-mode-map
    (kbd "\\f") 'eglot-format
    (kbd "\\i") 'py-isort-region
    )
  (evil-define-key 'normal go-mode-map
    (kbd "gd") 'godef-jump)
  (evil-define-key 'normal markdown-mode-map
    (kbd "\\1") 'markdown-insert-header-setext-1
    (kbd "\\2") 'markdown-insert-header-setext-2
    (kbd "\\-") 'markdown-insert-hr
    (kbd "\\c") 'markdown-insert-gfm-code-block
    (kbd "zc") 'markdown-hide-subtree
    (kbd "zo") 'markdown-show-subtree
    (kbd "TAB") 'markdown-cycle
    )
  (evil-define-key 'normal org-mode-map
    (kbd "C-k") 'org-metaup
    (kbd "C-j") 'org-metadown
    (kbd "<M-return>") '(lambda () (interactive) (evil-append-line 1) (org-meta-return))
    (kbd "<C-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-heading-after-current))
    (kbd "<M-S-return>") '(lambda () (interactive) (evil-append-line 1) (org-insert-todo-heading 1))
    (kbd "<C-S-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-todo-heading-respect-content))
    (kbd "t") 'org-todo
    (kbd "\\.") 'org-time-stamp
    (kbd "\\!") 'org-time-stamp-inactive
    (kbd "\\d") 'org-deadline
    (kbd "\\s") 'org-schedule
    (kbd "\\o") 'org-open-at-point
    (kbd "\\p") 'org-priority
    (kbd "\\q") 'org-set-tags-command
    (kbd "\\t") 'org-todo
    (kbd "\\x") 'org-toggle-checkbox
    (kbd "<") 'org-metaleft
    (kbd ">") 'org-metaright
    (kbd "gh") 'outline-up-heading
    (kbd "gp") 'outline-previous-heading
    (kbd "}") (if (fboundp 'org-forward-same-level)
                  'org-forward-same-level
                'org-forward-heading-same-level)
    (kbd "{") (if (fboundp 'org-backward-same-level)
                  'org-backward-same-level
                'org-backward-heading-same-level)
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
  (evil-define-key 'normal js2-mode-map
    (kbd "zc") 'js2-mode-hide-element
    (kbd "zo") 'js2-mode-show-element
    (kbd "\\f") 'eslint-fix
    )
  (evil-define-key 'normal js-mode-map
    (kbd "zc") 'js2-mode-hide-element
    (kbd "zo") 'js2-mode-show-element
    (kbd "\\f") 'eslint-fix
    )
  (evil-define-key 'normal web-mode-map
    (kbd "zc") 'web-mode-fold-or-unfold
    (kbd "zo") 'web-mode-fold-or-unfold
    (kbd "\\f") 'eslint-fix
    )
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
    (kbd "O") 'org-agenda-clock-out-avy
    (kbd "P") 'org-agenda-show-priority
    (kbd "R") 'org-agenda-clockreport-mode
    (kbd "S") 'org-save-all-org-buffers
    (kbd "T") 'org-agenda-show-tags
    (kbd "X") 'org-agenda-clock-cancel
    (kbd "Z") 'org-agenda-sunrise-sunset
    (kbd "[") 'org-agenda-manipulate-query-add
    (kbd "\\t") 'org-agenda-goto
    (kbd "]") 'org-agenda-manipulate-query-subtract
    (kbd "b") 'org-agenda-earlier
    (kbd "e") 'org-agenda-set-effort
    (kbd "f") 'org-agenda-later
    (kbd "g/") 'org-agenda-filter-by-tag
    (kbd "gJ") 'org-agenda-clock-goto
    (kbd "g\\") 'org-agenda-filter-by-tag-refine
    (kbd "gh") 'org-agenda-holiday
    (kbd "gj") 'org-agenda-goto-date
    (kbd "gm") 'org-agenda-bulk-mark
    (kbd "go") 'org-agenda-open-link
    (kbd "gv") 'org-agenda-view-mode-dispatch
    (kbd "i") 'org-agenda-clock-in-avy
    (kbd "j")  'org-agenda-next-line
    (kbd "k")  'org-agenda-previous-line
    (kbd "n") 'org-agenda-add-note
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
    (kbd "x") 'org-agenda-exit
    (kbd "y") 'org-agenda-todo-yesterday
    (kbd "{") 'org-agenda-manipulate-query-add-re
    (kbd "}") 'org-agenda-manipulate-query-subtract-re
    )
  (evil-define-key 'normal prog-mode-map
    (kbd "[e") 'flycheck-previous-error
    (kbd "]e") 'flycheck-next-error)
  ;; https://gist.github.com/amirrajan/301e74dc844a4c9ffc3830dc4268f177
  (evil-set-initial-state 'org-agenda-mode 'normal)
  )

(use-package evil-leader :ensure t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    (kbd "ag") 'counsel-ag
    (kbd "bc") 'elscreen-create
    (kbd "bk") 'elscreen-kill
    (kbd "bn") 'elscreen-next
    (kbd "bp") 'elscreen-previous
    (kbd "df") 'counsel-describe-function
    (kbd "dk") 'describe-key
    (kbd "dv") 'counsel-describe-variable
    (kbd "el") 'flycheck-list-errors
    (kbd "fb") 'ivy-switch-buffer
    (kbd "fd") 'dired-sidebar-toggle-sidebar
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
    (kbd "tm") 'twittering-mentions-timeline
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

(use-package evil-surround :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-magit :ensure t
  :after (evil magit)
  )

(use-package evil-commentary :ensure t
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

(use-package evil-numbers :ensure t
  :after evil
  )

(use-package shackle :ensure t
  :config
  (setq shackle-rules
        '((compilation-mode :align below :ratio 0.2)
          ("*Help*" :align right)
          ("*Completions*" :align below :ratio 0.3)
          ("*quickrun*" :align below :select nil :ratio 0.3)
          ("*magit: *" :regexp t :align below :ratio 0.3)
          ("*magit-diff: *" :regexp t :align above :ratio 0.5)
          ("*Warnings*" :popup t :align below :ratio 0.1)
          )
        )
  (setq shackle-lighter "")
  (shackle-mode 1)
  )

(use-package smartrep :ensure t
  :after evil-numbers
  :config
  (smartrep-define-key global-map
      "C-c" '(("+" . 'evil-numbers/inc-at-pt)
              ("=" . 'evil-numbers/inc-at-pt)
              ("-" . 'evil-numbers/dec-at-pt)
              ))
  )

(use-package color-theme-sanityinc-tomorrow :ensure t
  :init
  ;; after emacsclient load
  (add-hook 'after-make-frame-functions
            '(lambda(frame)
               (load-theme 'sanityinc-tomorrow-night t)
               (my/init-whitespace-mode)
               ))
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  )

(use-package whitespace
  :config
  ;; http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
  (setq whitespace-style '(face           ; faceで可視化
                           trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           empty          ; 先頭/末尾の空行
                           space-mark     ; 表示のマッピング
                           tab-mark))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\u3000+\\)") ; スペースは全角のみを可視化
  (setq whitespace-action '(auto-cleanup)) ; 保存時に自動でクリーンアップ

  (set-display-table-slot standard-display-table 'truncation ?<) ; set lcs=extends:<,precedes:<
  (setcar (nthcdr 2 (assq 'space-mark whitespace-display-mappings)) [?_]) ; set nbsp:%

  (defun my/init-whitespace-mode ()
    (interactive)
    (set-face-attribute 'whitespace-trailing nil
                        :foreground "DeepPink"
                        :background nil
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :background nil)
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground "GreenYellow"
                        :weight 'bold)
    (set-face-attribute 'whitespace-empty nil
                        :background nil
                        :foreground "DeepPink"
                        :underline t)
    (global-whitespace-mode 1)
    )
  (my/init-whitespace-mode)
  )

(use-package yasnippet :ensure t :defer t
  :init
  (add-hook 'after-init-hook '(lambda() (yas-global-mode 1)))
  (setq yas-snippet-dirs (list
                          (locate-user-emacs-file "snippets")
                          "~/.yasnippet"
                          'yas-installed-snippets-dir))
  (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
  :bind
  (:map yas-keymap
        ("<tab>" . nil)
        ("RET" . yas-next-field-or-maybe-expand))
  )

(use-package yasnippet-snippets :ensure t :defer t
  :after yasnippet)

(use-package popup :ensure t
  :after yasnippet
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

(use-package yatemplate :ensure t
  :after yasnippet
  :config
  ;; http://emacs.rubikitch.com/sd1602-autoinsert-yatemplate-yasnippet/
  (yatemplate-fill-alist)
  (auto-insert-mode 1)
  )

(use-package key-binding :no-require
  :config
  (global-set-key "\C-h" (kbd "<backspace>"))
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
  )
