;;; init.el --- Emacs init file                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  yhiraki

;; Author: yhiraki <coffexpr at gmail.com>
;; Keywords: init

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


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

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package quelpa-use-package :ensure t
  :custom (quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice))

(defvar darwin-p (eq system-type 'darwin))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar carbon-p (eq system-type 'mac))
(defvar meadow-p (featurep 'meadow))

(use-package package-utils :ensure t)

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

(use-package yes-no :no-require
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  )

(use-package startup :no-require ; cannot require
  :config
  (setq inhibit-startup-message t)
  )

(use-package diminish :ensure t)

(use-package abbrev
  :diminish)

(use-package server
  :hook (after-init
         . (lambda ()
             (unless
                 (server-running-p)
               (server-start))))
  )

(use-package scroll :no-require
  :config
  (setq scroll-conservatively 1)
  (setq scroll-margin 0)
  )

(use-package scroll-bar
  :config
  (scroll-bar-mode 0)
  )

(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
  (mouse-wheel-follow-mouse 't) ; scroll window under mouse
  )

(use-package frame
  :custom
  (initial-frame-alist
        (append
         '((ns-transparent-titlebar . t) ;; タイトルバーを透過
           (vertical-scroll-bars . nil) ;; スクロールバーを消す
           (ns-appearance . dark) ;; 26.1 {light, dark}
           (internal-border-width . 0) ;; 余白を消す
           ))
        )
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  :config
  (setq default-frame-alist initial-frame-alist)
  )

(use-package paren
  :config
  (show-paren-mode 1) ;; 対応する括弧を光らせる
  )

(use-package bell :no-require
  :config
  (setq ring-bell-function 'ignore)
  )

(use-package buffer :no-require
  :config
  (setq-default indicate-buffer-boundaries 'left) ;; バッファの終端を表示
  (setq-default indicate-empty-lines t) ;; バッファの終端以降を可視化
  )

(use-package simple
  :hook
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'process-menu-mode 'emacs)
       ))
  :bind
  (:map process-menu-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ))

(use-package fringe
  :custom-face
  (fringe ((t (:background nil))))
 )

(use-package autosave :no-require
  :config
  (setq auto-save-timeout 10)
  (setq auto-save-interval 100)  ;; key typing count
  )

(use-package tempbuf
  :hook
  ((dired-mode magit-mode) . turn-on-tempbuf-mode)
  (find-file
   . (lambda ()
       (when (string-match "^/private/tmp\\|^/tmp" (buffer-file-name))
         (turn-on-tempbuf-mode))))
  :custom
  (tempbuf-kill-message nil)
  )

(use-package midnight)

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t) ; シンボリックリンクの読み込みを許可
  (auto-revert-check-vc-info t) ; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
  (large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
  (tags-revert-without-query 1) ; TAGS ファイルを自動で再読込
  )

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  )

(use-package executable
  ;; shegang を見て自動で +x する
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  )

(use-package cc-vars
  :custom (c-basic-offset 2)
  )

(use-package syntax :no-require
  :config
  ;; http://tarao.hatenablog.com/entry/20130304/evil_config#vim-word
  (modify-syntax-entry ?_ "w" (standard-syntax-table)) ; 単語境界をvim風に
  )

(use-package ns-win
  :if darwin-p
  :config
  (setq mac-option-modifier 'meta)
  )

(use-package faces
  :if darwin-p

  :hook
  (window-setup . my-reload-font)
  (find-file . set-apple-color-emoji)

  :config
  (defvar yhiraki-font 'cica)

  (defun my-reload-font (&optional frame)
    "reload my font settings"
    (interactive)

    ;; Osaka + Menlo
    (when (eq yhiraki-font 'osaka)
      (set-face-attribute 'default nil
                          :family "Menlo"
                          :height 120)
      (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Osaka"))
      (push '("Osaka" . 1.2) face-font-rescale-alist) ; 全角文字を2文字幅に揃える
      )

    ;; Cica
    (when (eq yhiraki-font 'cica)
      (set-face-attribute 'default nil
                          :family "Cica"
                          :height 160)
      ;; apple color emoji
      (push '("Apple color emoji" . 0.8) face-font-rescale-alist) ; 4文字幅に揃える
      )

    ;; Jetbrains mono
    (when (eq yhiraki-font 'jetbrains-mono)
      (set-face-attribute 'default nil
                          :family "Jetbrains Mono"
                          :height 140)
      ;; 日本語
      (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Osaka"))
      (push '("Osaka" . 1.2) face-font-rescale-alist) ; 全角文字を2文字幅に揃える
      ;; apple color emoji
      (push '("Apple color emoji" . 0.9) face-font-rescale-alist) ; 4文字幅に揃える
      )

    (remove-hook 'window-setup-hook #'my-reload-font)
    )

  (defun set-apple-color-emoji ()
    "set apple color emoji"
    (set-fontset-font nil '(#x1F000 . #x1FAFF) "Apple Color Emoji")
    (remove-hook 'find-file-hook #'set-apple-color-emoji))

  ;; http://misohena.jp/blog/2017-09-26-symbol- font-settings-for-emacs25.html
  ;; TODO: インデント可視化用のunicode文字は半角幅にしたいので無効化
  ;; (setq use-default-font-for-symbols nil) ; 記号をデフォルトのフォントにしない ○△□が全角幅になる

  ;; |あいうえお|かきくけこ|
  ;; |１２３４５|一二三四五|
  ;; |①②③④⑤|○△□☆…|
  ;; |😀😀😀😀😀|
  ;; |abcdefghij|klmnopqrst|
  ;; |1234567890|1234567890|
  )

(use-package mule-cmds :no-require ; cannot require
  :config
  ;; unicode の一部を1文字幅として扱う
  ;; "┃" : git-gutter
  ;; "│" : highlight-indent-guides
  (set-language-environment "English")
  ;; magitでの文字化け対策
  (prefer-coding-system 'utf-8)
  )

(use-package fira-code-mode :disabled
  :hook ((prog-mode
         gfm-mode
         markdown-mode
         org-mode)
         . (lambda ()
               (when window-system
                 (fira-code-mode)
                 )
               ))
  )

(use-package files
  :custom
  (confirm-kill-emacs nil)
  (find-file-visit-truename t)
  (require-final-newline t)
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
  (which-key-use-C-h-commands nil)
  (which-key-allow-evil-operators t)
  )

(use-package smartparens :ensure t
  :diminish smartparens-mode
  :hook (after-init . smartparens-global-mode)

  :config
  ;; (sp-pair "\{ " " \}")
  ;; (sp-pair "\[ " " \]")

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

  ;; {|}
  ;;  ↓
  ;; {
  ;;  |
  ;; }
  ;; https://github.com/Fuco1/smartparens/issues/80
  (defun my-open-block-c-mode (id action context)
    (when (eq action 'insert)
      (newline)
      (indent-according-to-mode)
      (forward-line)
      (indent-according-to-mode)))

  ;; (sp-with-modes '(prog-mode vue-mode)
  ;;   (sp-local-pair  "{" nil :post-handlers '((my-open-block-c-mode "RET")))
  ;;   )
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

(use-package emojify :ensure t :disabled
  :hook (after-init . global-emojify-mode)
  )

(use-package dired
  :hook
  (dired-mode
   . (lambda()
       (dired-hide-details-mode 1)
       (setq-local line-spacing 3)))
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'dired-mode 'emacs)))

  :bind
  (:map dired-mode-map
        (":"   . evil-ex)
        ("C-b" . evil-scroll-page-up)
        ("C-f" . evil-scroll-page-down)
        ("C-j" . dired-next-dirline)
        ("C-k" . dired-prev-dirline)
        ("G"   . evil-goto-line)
        ("SPC" . hydra-global-leader/body)
        ("r"   . revert-buffer)
        ("g"   . nil)
        ("gg"  . evil-goto-first-line)
        ("go"  . my-open-in-external-app)
        ("j"   . dired-next-line)
        ("k"   . dired-previous-line)))

(use-package dired-subtree :ensure t
  :after dired
  :commands (dired-subtree-insert dired-subtree-remove)

  :bind
  (:map dired-mode-map
	("l" . my-dired-subtree-insert)
	("h" . my-dired-subtree-remove))

  :config
  ;; for all-the-icons
  ;; https://github.com/Fuco1/dired-hacks/issues/155
  (defun my-dired-subtree-insert ()
    (interactive)
    (dired-subtree-insert)
    (revert-buffer))
  (defun my-dired-subtree-remove ()
    (interactive)
    (dired-subtree-remove)
    (revert-buffer))
  )

(use-package dired-sidebar :ensure t
  :after dired-subtree
  :commands (dired-sidebar-toggle-sidebar)
  )

(use-package dired-filter :ensure t)

(use-package all-the-icons-dired :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package wdired
  :commands (wdired-change-to-wdired-mode)

  :hook
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'wdired-mode 'normal)))

  :custom (wdired-allow-to-change-permissions t)

  :bind
  (:map dired-mode-map
        ("e"   . wdired-change-to-wdired-mode))
  )

(use-package flycheck :ensure t
  :hook
  ((prog-mode yaml-mode) . flycheck-mode)
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal flycheck-error-list-mode-map
         (kbd "F") 'flycheck-error-list-reset-filter
         (kbd "RET") 'flycheck-error-list-goto-error
         (kbd "f") 'flycheck-error-list-set-filter
         (kbd "j") 'flycheck-error-list-next-error
         (kbd "k") 'flycheck-error-list-previous-error
         (kbd "n") 'flycheck-error-list-next-error
         (kbd "p") 'flycheck-error-list-previous-error
         (kbd "q") 'quit-window
         )))
  :custom
  (flycheck-python-flake8-executable "python3")
  (flycheck-python-pycompile-executable "python3")
  (flycheck-python-pylint-executable "python3")
  (flycheck-deferred-syntax-check t)
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
  :commands (magit-status magit-stage)
  :custom
  (magit-save-repository-buffers nil)
  (magit-diff-refine-hunk 'all)
  )

(use-package git-timemachine :ensure t
  :hook
    (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'git-timemachine-mode 'emacs)
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
         )))
    )

(use-package git-gutter+ :ensure t
  :diminish

  :hook
  ((find-file
    after-save
    after-revert
    evil-insert-state-entry)
   . git-gutter+-turn-on)
  ;; (evil-normal-state-entry
  ;;  . git-gutter+-refresh)
  (before-save
   . git-gutter+-turn-off)

  :custom
  (git-gutter+-added-sign "┃")
  (git-gutter+-deleted-sign "▔")
  (git-gutter+-modified-sign "┃")

  :custom-face
  (git-gutter+-modified ((t (:italic nil :underline nil :foreground "orange"))))
  (git-gutter+-deleted ((t (:italic nil :underline nil :foreground "red"))))
  (git-gutter+-added ((t (:italic nil :underline nil :foreground "green"))))
  )

(use-package git-messenger :ensure t
  :commands git-messenger:popup-message)

(use-package gist :ensure
  :hook
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'gist-list-mode 'insert)
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
         )))
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

(use-package undo-fu :ensure t)

(use-package undo-fu-session :ensure t
  :after undo-fu
  :hook (after-init . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  )

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

(use-package all-the-icons-ivy :ensure t
  :hook (after-init . all-the-icons-ivy-setup))

(use-package ivy-hydra :ensure t
  :after (ivy hydra))

(use-package ivy-rich :ensure t
  :hook (ivy-mode . ivy-rich-mode)
  )

(use-package counsel :ensure t
  :after ivy
  :custom
  (counsel-yank-pop-separator "\n-------\n")
  )

(use-package swiper :ensure t :disabled  ; 遅いので無効化
  :commands (swiper-isearch swiper-isearch-backward)
  :after evil
  :bind
  (:map evil-normal-state-map
        ("*" . 'swiper-isearch-thing-at-point)
        ("/" . 'swiper-isearch)
        ("?" . 'swiper-isearch-backward))
  )

(use-package ivy-ghq
  :quelpa (ivy-ghq :fetcher github :repo "analyticd/ivy-ghq")
  :commands (ivy-ghq-open)
  )

(use-package exec-path-from-shell :ensure t
  :hook (after-init . exec-path-from-shell-initialize)
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
  ;; (pangu-spacing-real-insert-separtor t)

  ;; text-mode やその派生モード(org-mode 等)のみに使いたいならこれ
  ;; :hook
  ;; (text-mode-hook . pangu-spacing-mode)
  ;; (twittering-edit-mode . pangu-spacing-mode)
  )

(use-package twittering-mode :ensure t
  :commands (twit)

  :custom
  ;; master-password を設定する際に注意すること
  ;; https://blog.web-apps.tech/emacs-mac-twittering-mode-every-asked-pin/
  (twittering-use-master-password t)

  :config
  ;; https://github.com/hayamiz/twittering-mode/issues/154
  (when (>= 27 emacs-major-version)
    (defalias 'epa--decode-coding-string 'decode-coding-string))
  )

(use-package lsp-mode :ensure t
  :hook ((c++-mode
          js-mode
          ;; sh-mode
          typescript-mode)
         . lsp)

  :custom
  (lsp-auto-guess-root t)
  (lsp-clients-go-server "gopls")
  (lsp-clients-javascript-typescript-server "typescript-language-server")
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 1)

  :config
  ;; https://github.com/seagle0128/.emacs.d/blob/50d9de85ba4ff2aa5daa2603d366cde2f3e89242/lisp/init-lsp.el#L426-L458
  (defvar centaur-lsp 'lsp-mode)
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((filename (or (->> info caddr (alist-get :file))
                               buffer-file-name
                               "*org-src-lsp*")))
             (unless filename
               (user-error "LSP:: specify `:file' property to enable"))

             (setq buffer-file-name filename)
             (pcase centaur-lsp
               ('eglot
                (and (fboundp 'eglot) (eglot)))
               ('lsp-mode
                (and (fboundp 'lsp-deferred)
                     ;; `lsp-auto-guess-root' MUST be non-nil.
                     (setq lsp-buffer-uri (lsp--path-to-uri filename))
                     (lsp-deferred))))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      centaur-lsp (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (lsp-org-babel-enable "python")
  (lsp-org-babel-enable "cpp")
  (lsp-org-babel-enable "js")
  (lsp-org-babel-enable "typescript")
  )

(use-package lsp-vetur
  :hook (vue . lsp)
  :custom
  (lsp-vetur-format-default-formatter-ts "eslint")
  (lsp-vetur-format-default-formatter-js "eslint")
  )

(use-package lsp-pyls
  :hook (python-mode . lsp)
  :custom
  (lsp-pyls-plugins-flake8-enabled t)
  (lsp-pyls-plugins-jedi-completion-include-params nil)
  (lsp-pyls-plugins-pylint-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  (lsp-pyls-plugins-autopep8-enabled nil)
  )

(use-package lsp-python-ms :disabled  ; formatting providorが無いと言われるので様子見
  :ensure t
  :custom (lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(use-package lsp-go
  :hook (go . lsp))

(use-package lsp-ui :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)

  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-sideline-enable t)

  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . #'lsp-ui-peek-find-references))
  (:map lsp-ui-peek-mode-map
        ("j" . lsp-ui-peek--select-next)
        ("k" . lsp-ui-peek--select-prev)))

(use-package company :ensure t
  :diminish company-mode
  :hook
  (after-init . global-company-mode)
  (after-init . company-tng-mode)
  (TeX-mode . edit-category-table-for-company-dabbrev)

  :custom
  (company-auto-commit nil)
  (company-candidates-cache t)
  (company-dabbrev-char-regexp "\\cs")
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-etags-ignore-case t)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-tng-auto-configure nil) ; keymap(company-active-map)が変更されないようにoff
  (company-tooltip-idle-delay 0.1)
  (company-tooltip-limit 10)
  (completion-ignore-case t)

  :config
  ;; company-dabbrevで日本語を補完しない
  ;; https://qiita.com/wktkshn/items/3ac46671d1c242a59f7e
  (defun edit-category-table-for-company-dabbrev (&optional table)
    (define-category ?s "word constituents for company-dabbrev" table)
    (let ((i 0))
      (while (< i 128)
        (if (equal ?w (char-syntax i))
            (modify-category-entry i ?s table)
          (modify-category-entry i ?s table t))
        (setq i (1+ i)))))
  (edit-category-table-for-company-dabbrev)
  ;; (add-to-list 'company-frontends 'company-tng-frontend)

  :bind
  (:map company-active-map
        ("C-S-h" . 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
        ("C-h" . nil) ;; C-hはバックスペース割当のため無効化
        ("RET" . nil)
        ("<return>" . nil)
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        ("C-s" . 'company-filter-candidates)
        ("<tab>" . 'yas-expand-from-trigger-key)
        )
  (:map company-search-map
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        ("C-h" . 'company-search-delete-char)
        ("<space>" . nil)
        ("RET" . 'company-complete-selection)
        ("<return>" . 'company-complete-selection)
        )
  )

(use-package company-box :ensure t :disabled
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  )

(use-package company-statistics :ensure t
  :hook (company-mode . company-statistics-mode)
  :custom
  (company-statistics-file "~/.cache/emacs/company-statistics-cache.el")
  (company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  )

(use-package company-tabnine :ensure t :disabled  ; リソースを食うので停止
  :config
  (add-to-list 'company-backends #'company-tabnine)
  )

(use-package quickrun :ensure t
  :commands quickrun

  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal quickrun--mode-map
         (kbd "q") 'evil-window-delete
         )
       ))

  :custom
  (quickrun-timeout-seconds 30)

  :config
  (quickrun-set-default "c" "c/gcc")

  (quickrun-add-command "rust/script"
    '((:command . "cargo")
      (:exec    . ("%c script %o %s")))
    :default "rust")

  (quickrun-add-command "c++14/g++"
    '((:command . "g++")
      (:exec    . ("%c -x c++ %o -o %e %s" "%e %a"))
      (:compile-only . "%c %o -o %e %s")
      (:remove  . ("%e"))
      (:description . "Compile C++ file with g++ and execute")
      (:cmdopt . "-Wall -Wextra -pedantic -std=c++14 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlong-long -Wshift-overflow -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=address -fsanitize=undefined -fno-sanitize-recover=all -fstack-protector"))
    )
  (quickrun-set-default "c++" "c++14/g++")

  (quickrun-add-command "typescript"
    '((:exec . ("%c --target es6 --module commonjs %o %s %a" "node %n.js")))
    :override t)

  (quickrun-add-command "python"
    '((:command . "python3")
      (:compile-only . "flake8 %s"))
    :override t)
  )

(use-package csharp-mode :ensure t
  :mode
  ("\\.cs\\'" . csharp-mode)
  )

(use-package dockerfile-mode :ensure t)

(use-package docker-compose-mode :ensure t)

(use-package hideshow
  :diminish hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode)
  ;; ジャンプしたときに自動的に展開して特定の行に移動してくれるようにしたい
  ;; (prog-mode . my/hs-minor-mode-hide-all)
  ;; (xref-after-jump . (lambda () (hs-show-block)))
  :config
  (defun my/hs-minor-mode-hide-all ()
    (hs-minor-mode)
    (when (> (count-lines (point-min) (point-max)) (frame-height))
      (hs-hide-all)))
  )

(use-package elisp-mode
  :mode
  ("\\.el\\'" . emacs-lisp-mode)
  )

(use-package clang-format :ensure t
  :commands clang-format-buffer)

(use-package go-mode :ensure t
  :mode ("\\.go\\'" . go-mode)

  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal go-mode-map
         (kbd "\\f") 'gofmt
         )
       (evil-define-key 'visual go-mode-map
         (kbd "\\f") 'gofmt
         )
       ))

  :custom
  (gofmt-command "goimports")
  )

(use-package go-eldoc :ensure t
  :hook (go-mode . go-eldoc-setup)
)

(use-package protobuf-mode :ensure t)

(use-package js
  :custom
  (js-indent-level 2)
  :mode
  ("\\.js\\'" . js-mode)
  ("\\.jsx\\'" . js-jsx-mode)
  )

(use-package css-mode
  :custom
  (css-indent-offset 2)
  )

(use-package add-node-modules-path :ensure t
  :hook (js-mode
         typescript-mode
         web-mode
         vue-mode)
  )

(use-package eslint-fix :ensure t
  :commands eslint-fix
  )

(use-package prettier-js :ensure t
  :commands prettier-js)

(use-package json-mode :ensure t
  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal json-mode-map
         (kbd "\\f") 'json-pretty-print-buffer
         )
       ))
  )

(use-package markdown-mode :ensure t
  :hook
  ((markdown-mode gfm-mode) . outline-hide-subtree)
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal markdown-mode-map
         (kbd "zo") '(lambda () (interactive) (outline-show-children) (outline-show-entry))
         (kbd "zc") 'outline-hide-subtree
         (kbd "TAB") 'markdown-cycle
         (kbd "\\f") 'prettier-js
         )
       (evil-define-key 'normal gfm-mode-map
         (kbd "zo") '(lambda () (interactive) (outline-show-children) (outline-show-entry))
         (kbd "zc") 'outline-hide-subtree
         (kbd "TAB") 'markdown-cycle
         )
       ))

  :custom
  (markdown-command "pandoc -s -t html5 -c ~/.emacs.d/css/github.css")
  (markdown-gfm-use-electric-backquote nil)

  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  )

(use-package prog-mode
  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal prog-mode-map
         (kbd "[e") 'flycheck-previous-error
         (kbd "]e") 'flycheck-next-error

         (kbd "\\R")  'lsp-rename
         (kbd "\\e") 'flycheck-list-errors
         (kbd "\\f") 'lsp-format-buffer
         (kbd "\\m") 'lsp-ui-imenu
         (kbd "\\qa") 'quickrun-autorun-mode
         (kbd "\\qc") 'quickrun-compile-only
         (kbd "\\qr") 'quickrun
         (kbd "\\qs") 'quickrun-shell
         (kbd "\\r")  'quickrun

         (kbd "gd") 'xref-find-definitions
         (kbd "gr") 'xref-find-references
         )
       (evil-define-key 'visual prog-mode-map
         (kbd "[e") 'flycheck-previous-error
         (kbd "]e") 'flycheck-next-error

         (kbd "\\R")  'lsp-rename
         (kbd "\\e") 'flycheck-list-errors
         (kbd "\\f") 'lsp-format-region
         (kbd "\\m") 'lsp-ui-imenu
         (kbd "\\qa") 'quickrun-autorun-mode
         (kbd "\\qc") 'quickrun-compile-only
         (kbd "\\qr") 'quickrun-region
         (kbd "\\qs") 'quickrun-shell
         (kbd "\\r") 'quickrun-region

         (kbd "gd") 'xref-find-definitions
         (kbd "gr") 'xref-find-references
         )))
  )

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  )

(use-package xref
  :hook
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
       ))

  :bind
  (:map xref--xref-buffer-mode-map
        ("j" . #'xref-next-line)
        ("k" . #'xref-prev-line)
        ))

(use-package view
  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal view-mode-map
         (kbd "q") 'View-quit
         )))
  )

(use-package edit-indirect :ensure t
  :commands edit-indirect-region
  )

(use-package replace
  :bind
  (:map occur-mode-map
        ("j" . next-line)
        ("k" . previous-line)))

(use-package org :ensure org-plus-contrib
  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal org-mode-map
         (kbd "C-S-j") 'org-next-visible-heading
         (kbd "C-S-k") 'org-previous-visible-heading
         (kbd "M-h") 'org-metaleft
         (kbd "M-j") 'org-metadown
         (kbd "M-k") 'org-metaup
         (kbd "M-l") 'org-metaright
         (kbd "<M-return>") '(lambda () (interactive) (evil-append-line 1) (org-meta-return))
         (kbd "M-RET") '(lambda () (interactive) (evil-append-line 1) (org-meta-return))
         (kbd "<C-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-heading-after-current))
         (kbd "C-RET") '(lambda () (interactive) (evil-insert-state) (org-insert-heading-after-current))
         (kbd "<M-S-return>") '(lambda () (interactive) (evil-append-line 1) (org-insert-todo-heading 1))
         (kbd "<C-S-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-todo-heading-respect-content))
         (kbd "t") 'org-todo
         (kbd "T") '(lambda () (interactive) (org-call-with-arg 'org-todo 'right))
         (kbd "<") 'org-metaleft
         (kbd ">") 'org-metaright
         (kbd "\\g") 'org-mac-grab-link
         (kbd "\\i") 'org-clock-in
         (kbd "\\nb") 'org-narrow-to-block
         (kbd "\\ne") 'org-narrow-to-element
         (kbd "\\nf") 'narrow-to-defun
         (kbd "\\ns") 'org-narrow-to-subtree
         (kbd "\\nw") 'widen
         (kbd "\\p") 'org-priority
         (kbd "\\q") 'org-set-tags-command
         (kbd "\\s") 'org-schedule
         (kbd "\\t") 'org-todo
         (kbd "\\v") 'org-toggle-inline-images
         (kbd "\\xp") 'org-set-property
         (kbd "gh") 'outline-up-heading
         (kbd "gp") 'outline-previous-heading
         (kbd "\\ \\") 'hydra-outline/body
         (kbd "\\f") 'whitespace-cleanup
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
         )))

  (org-after-todo-statistics . org-summary-todo)
  (org-checkbox-statistics . my/org-checkbox-todo)
  (org-mode . org-indent-mode)

  :custom
  (org-directory "~/org/")
  (org-startup-with-inline-images nil)
  (org-src-fontify-natively t)
  (org-hide-leading-stars t) ; 見出しの余分な*を消す
  (org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
     (sequence "|" "CANCELLED(c)")
     (sequence "WAITING(w)" "STARTED(s)" "|")
     (sequence "SOMEDAY(S)" "|" "DONE(d)")
     (sequence "AGENDA(a)" "|" "MEETING(m)")))
  (org-log-done 'time) ; DONEの時刻を記録
  (org-hidden-keywords '(title))

  :config
  ;; https://emacs.stackexchange.com/questions/32473/edit-org-mode-tags-using-ido-or-ivy-completion
  ;; =C-M-m= to add/remove tag
  ;; =C-M-j= to fix tags
  (global-set-key [remap org-set-tags-command] #'counsel-org-tag)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; https://emacs.stackexchange.com/questions/19843/how-to-automatically-adjust-an-org-task-state-with-its-children-checkboxes
  (defun my/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (save-excursion
          (org-back-to-heading t)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (goto-char beg)
          (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                 end t)
              (if (match-end 1)
                  (if (equal (match-string 1) "100%")
                      (unless (string-equal todo-state "DONE")
                        (org-todo 'done))
                    (unless (string-equal todo-state "TODO")
                      (org-todo 'todo)))
                (if (and (> (match-end 2) (match-beginning 2))
                         (equal (match-string 2) (match-string 3)))
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done))
                  (unless (string-equal todo-state "TODO")
                    (org-todo 'todo)))))))))

  :mode (("\\.org\\'" . org-mode))
  )

(use-package org-faces
  :after org

  :custom
  (org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("STARTED" :foreground "orange red" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("SOMEDAY" :foreground "dark gray")
     ("DONE" :foreground "forest green" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("AGENDA" :foreground "sky blue" :weight bold)
     ("MEETING" :foreground "sky blue" :weight bold)))
  ;; Only use the first 4 styles and do not cycle.
  (org-cycle-level-faces nil)
  (org-n-level-faces 4)

  :config
  ;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
  (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1) ;\large
  (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2) ;\Large
  (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.4) ;\LARGE

  ;; Document Title, (\huge)
  (set-face-attribute 'org-document-title nil
		      :height 2.074
		      :foreground 'unspecified
		      :inherit 'org-level-8)
  )

(use-package org-agenda
  :after org
  :commands (org-agenda org-refile)
  :demand t

  :hook
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'org-agenda-mode 'emacs)
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
  (setq org-agenda-files (list org-directory))

  :bind
  (:map org-agenda-mode-map
        ("j" . org-agenda-next-item)
        ("k" . org-agenda-previous-item))
  )

(use-package org-clock
  :hook
  (kill-emacs . my:org-clock-out-and-save-when-exit)
  (org-after-todo-state-change . my:org-clock-in-if-starting)
  (org-after-todo-state-change . my:org-clock-out-if-waiting)

  :custom
  (org-clock-out-remove-zero-time-clocks t)

  :config
  ;; https://qiita.com/takaxp/items/6b2d1e05e7ce4517274d
  (defun my:org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (when (org-clocking-p)
      (org-clock-out)
      (save-some-buffers t)))

  ;; https://passingloop.tumblr.com/post/10150860851/org-clock-in-if-starting
  (defun my:org-clock-in-if-starting ()
    "Clock in when the task is marked STARTED."
    (when (and (string= org-state "STARTED")
               (not (string= org-last-state org-state)))
      (org-clock-in)))

  (defun my:org-clock-out-if-waiting ()
    "Clock in when the task is marked STARTED."
    (when (and (string= org-state "WAITING")
               (not (string= org-last-state org-state)))
      (org-clock-out)))
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
  :hook
  (org-babel-after-execute . org-display-inline-images)
  :custom
  (org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (org-babel-C++-compiler "g++ -Wall -Wextra -std=c++14")
  :config
  ;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("elisp" "python" "shell" "plantuml" "uml" "shell" "dot" "js" "C" "cpp" "typescript"))))

  (push '("ts" . typescript) org-src-lang-modes)
  (push '("console" . sh) org-src-lang-modes)
  (push '("uml" . plantuml) org-src-lang-modes)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (plantuml . t)
     (shell . t)
     (dot . t)
     (js . t)
     (typescript . t)
     (C . t))
   ))

(defvar my/plantuml-java-options "-Djava.awt.headless=true") ; plantuml-modeのdefaultになったけどob-plantumlで使う
(defvar my/plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar")) ; ob-plantumlで使う
(defvar my/plantuml-jar-args (list "-charset" "UTF-8" "-config" (expand-file-name "~/.config/plantuml/color.uml"))) ; ob-plantumlで使う

(use-package ob-plantuml
  :after (ob plantuml-mode s)
  :custom
  (org-plantuml-jar-path my/plantuml-jar-path)
  (plantuml-server-url nil)
  :config
  (push (cons ':java my/plantuml-java-options) org-babel-default-header-args:plantuml)
  (push (cons ':cmdline (s-join " " my/plantuml-jar-args)) org-babel-default-header-args:plantuml)
  (push '(:async) org-babel-default-header-args:plantuml)
  (push '(:cache . "yes") org-babel-default-header-args:plantuml)
  )

(use-package ob-shell
  :after ob
  :config
  (push '(:async) org-babel-default-header-args:shell)
  )

(use-package ob-python
  :hook (org-mode
         . (lambda ()
             (use-package auto-virtualenvwrapper
               :config
               (setq-local
                org-babel-python-command
                (find-virtualenv-executable "python3")))))
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
  :hook
  (ob-async-pre-execute-src-block
   . (lambda ()
       (setq org-plantuml-jar-path "~/lib/java/plantuml.jar")))
  )

(use-package ob-ipython :ensure t :disabled
  :after ob)

(use-package ob-js
  :config
  (setq org-babel-js-function-wrapper
        "require('util').inspect(function(){\n%s\n}());")
  )

(use-package ob-typescript :ensure t)

(use-package org-capture
  :commands org-capture

  :hook
  (org-capture-mode . evil-insert-state)

  :custom
  (org-capture-templates
        '(
          ("i" "Inbox\t\t- Add entry to Inbox"
           entry (file+headline "~/org/tasks.org" "Inbox")
           "** %?\n%T")

          ;; http://grugrut.hatenablog.jp/entry/2016/03/13/085417
          ("I" "Interrupt\t\t- Add an interrupt task"
           entry (file+headline "~/org/tasks.org" "Tasks")
           "** %?\n%T" :clock-in t :clock-resume t)

          ("b" "Book\t\t- Books wish list"
           table-line (file+headline "~/org/books.org" "wish list")
           "|名前|価格|電子版|追加日|\n|%?|||%U|" :table-line-pos "II-1")

          ("j" "Journal\t- Short logs like Twitter"
           entry (file+olp+datetree "~/org/journal.org" "Journal")
           "* %?\n%T")

          ("B" "Blog\t\t- Hugo post"
           plain (file+olp "~/org/blog.org" "Blog Ideas")
           "hugo%?")
          )
        )

  :bind
  (:map org-capture-mode-map
        ("C-c C-k" . (lambda () (interactive) (message "Abort is disabled")))
        )
  )

(use-package org-checklist
  :after org
  )

(use-package org-superstar :ensure t
  :hook (org-mode . org-superstar-mode)

  :custom
  ;; Set different bullets, with one getting a terminal fallback.
  (org-superstar-headline-bullets-list '("◉" ("🞛" ?◈) "○" "▷"))
  ;; Stop cycling bullets to emphasize hierarchy of headlines.
  (org-superstar-cycle-headline-bullets nil)
  ;; Hide away leading stars on terminal.
  (org-superstar-leading-fallback ?\s)

  :config
  (set-face-attribute 'org-superstar-item nil :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  (set-face-attribute 'org-superstar-leading nil :height 1.3)
  )

(use-package org-download :ensure t
  :custom
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir (concat org-directory "images"))
  (org-download-heading-lvl nil)
  )

(use-package org-mac-link
  :bind
  (:map org-mode-map
        ("C-c g" . org-mac-grab-link)
        ))

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
  (org-html-validation-link nil)
  (org-html-mathjax-options
   '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
     (scale "100")
     (align "center")
     (font "TeX")
     (linebreaks "false")
     (autonumber "AMS")
     (indent "0em")
     (multlinewidth "85%")
     (tagindent ".8em")
     (tagside "right"))
   ))

(use-package ox-reveal :ensure t
  :after ox
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  )

(use-package ox-rst :ensure t
  :after ox
  )

(use-package htmlize :ensure t)

(use-package electric
  :hook (python-mode . electric-indent-mode)
  )

(use-package cc-mode
  :mode (("\\.cpp\\'" . c++-mode))
  :config
  (setq-default sp-escape-quotes-after-insert nil)
  )

(use-package python :ensure t
  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal python-mode-map
         (kbd "\\i") 'py-isort-buffer
         ;; (kbd "\\f") 'py-yapf-buffer  ;; use lsp instead
         )
       (evil-define-key 'visual python-mode-map
         (kbd "\\i") 'py-isort-region
         )))

  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-m IPython --simple-prompt -i")
  )

(use-package auto-virtualenvwrapper :ensure t
  :commands auto-virtualenvwrapper-find-virtualenv-path

  :hook (python-mode . auto-virtualenvwrapper-activate)

  :init
  (defun find-virtualenv-executable (command)
    (let ((path (auto-virtualenvwrapper-find-virtualenv-path)))
      (if path
          (concat path "bin/" command)
        (let ((exe (executable-find command)))
              (if exe exe command)))))
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
  (sh-indent-for-case-alt '+))

(use-package shfmt
  :quelpa (shfmt :type git :fetcher github :repo "amake/shfmt.el")
  :if (executable-find "shfmt")

  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal sh-mode-map
         (kbd "\\f") 'shfmt-buffer)
       (evil-define-key 'visual sh-mode-map
         (kbd "\\f") 'shfmt-region)
       ))

  :custom
  (shfmt-arguments '("-i" "2" "-ci"))
  )

(use-package sql
  :config
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  (setq sql-postgres-login-params (append sql-postgres-login-params '(port))))

(use-package sql-indent :ensure t
  :after sql
  :commands sqlind-setup

  :hook
  (sqlind-minor-mode
   . (lambda ()
      (setq-local sqlind-indentation-offsets-alist
            my-sql-indentation-offsets-alist)))
  (sql-mode
   . (lambda ()
      (sqlind-minor-mode)
      (sql-set-product "postgres")))
  (sql-interactive-mode
   . (lambda ()
       (toggle-truncate-lines t)
       (sqli-add-hooks)))

  :init
  ;; https://github.com/xlighting/happy-emacs.d/blob/12e8369cd7934600703b61bb1c278d77dab0c3a2/modules/init-sql.el
  (defun sql-add-newline-first (output)
    "In a SQLi buffer,The table formatting is ugly because the top boundary of the
    table is printed on the same row as the the prompt,This fixes it"
    (replace-regexp-in-string "\\(\\w+[ ]?\\[\\((?[[:alpha:]])?\\|_\\)+\\][#>][ ]?\\)\\(.*[#>] \\)?" "\\1\n" output))
  (defun sqli-add-hooks ()
    "Add hooks to `sql-interactive-mode-hook'."
    (add-hook 'comint-preoutput-filter-functions
              'sql-add-newline-first))

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

(use-package toml-mode :ensure t)

(use-package typescript-mode :ensure t
  :custom
  (typescript-indent-level 2)
  :mode
  ("\\.tsx?\\'"))

(use-package plantuml-mode :ensure t
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 2)
  (plantuml-jar-args my/plantuml-jar-args)
  (plantuml-jar-path my/plantuml-jar-path)
  (plantuml-java-options my/plantuml-java-options)

  :config
  ;; (setq plantuml-output-type "svg")

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
  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal web-mode-map
         (kbd "\\R") 'web-mode-element-rename
         (kbd "zc") 'web-mode-fold-or-unfold
         (kbd "zo") 'web-mode-fold-or-unfold
         )))

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
  :hook
  (vue-mode
   . (lambda ()
       (setq syntax-ppss-table nil)
       (add-hook 'after-save-hook 'mmm-parse-buffer nil t)
       ))
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal vue-mode-map
         (kbd "\\f") 'eslint-fix
         )))
  (vue-mode . lsp)
  )

(use-package mmm-mode
  :diminish
  :custom
  (mmm-submode-decoration-level 0)
  )

(use-package yaml-mode :ensure t
  :hook (evil-after-load
         . (lambda ()
             (evil-define-key 'normal yaml-mode-map
               (kbd "\\e") 'flycheck-list-errors
               (kbd "C-m") 'newline-and-indent
               (kbd "\\f") 'prettier-js))))

(use-package flycheck-yamllint :ensure t
  :hook (yaml-mode . flycheck-yamllint-setup))

(use-package vimrc-mode :ensure t
  :mode
  ("\\.vim\\(rc\\)?\\'" . vimrc-mode)
  )

(use-package image-mode
  :hook
  (evil-after-load
   . (lambda ()
       (evil-define-key 'normal image-mode-map
         (kbd "q") 'evil-quit
         )))
  )

(use-package emmet-mode :ensure t
  :hook (sgml-mode css-mode web-mode xml-mode js-jsx-mode typescript-mode)
  :custom
  (emmet-indent-after-insert nil)
)

(use-package text-mode
  :mode
  ("\\.qrinput\\'" . text-mode)
  )

(use-package gitignore-mode :ensure t
  :mode
  ("\\(\\.git\\|docker\\)ignore\\'" . gitignore-mode)
  )

(use-package smartrep :ensure t
  :hook
  (evil-after-load
   . (lambda ()
       (smartrep-define-key evil-normal-state-map
           "C-c" '(("+" . 'evil-numbers/inc-at-pt)
                   ("=" . 'evil-numbers/inc-at-pt)
                   ("-" . 'evil-numbers/dec-at-pt)))
       (smartrep-define-key evil-normal-state-map
           "C-w" '(("+" . 'evil-window-increase-height)
                   ("-" . 'evil-window-decrease-height)
                   ("=" . 'balance-windows)
                   ("<" . 'evil-window-decrease-width)
                   (">" . 'evil-window-increase-width)))
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
    ("c" magit-commit "commit")
    ("f" counsel-git "find")
    ("g" counsel-git-grep "grep")
    ("lb" gist-buffer "gist buffer")
    ("ll" gist-list "gist list")
    ("m" git-messenger:popup-message "git messenger")
    ("o" browse-at-remote "browse at remote")
    ("p" ivy-ghq-open "ghq")
    ("s" magit-stage "stage")
    ("t" git-timemachine "timemachine")
    ("u" magit-unstage "unstage")
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
    ("r" rocket-chat-edit "rocket chat")
    )

  (defhydra hydra-emacs-operation (:exit t)
    ("e" eval-buffer "eval-buffer")
    ("k" save-buffers-kill-emacs "kill emacs")
    ("r" restart-emacs "restart emacs")
    )

  (defhydra hydra-global-leader (:exit t)
    ("G" hydra-google/body "google")
    ("a" org-agenda "org-agenda")
    ("b" counsel-bookmark "bookmarks")
    ("c" org-capture "org-cature")
    ("f" hydra-file-open/body "find file")
    ("g" hydra-git/body "git")
    ("h" hydra-help/body "help")
    ("k" (message "Disabled. use :bw") "kill buffer")
    ("o" hydra-org/body "org")
    ("q" nil "quit")
    ("s" magit-status "git status")
    ("t" hydra-twitter/body "twitter")
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
  :after (hydra undo-fu)

  :bind
  (:map evil-normal-state-map
        ("<SPC>" . 'hydra-global-leader/body)
        ("C-h" . 'evil-backward-char)
        ("C-j" . 'evil-forward-paragraph)
        ("C-k" . 'evil-backward-paragraph)
        ("C-l" . 'redraw-display)
        ("S-C-j" . 'evil-forward-section-begin)
        ("S-C-k" . 'evil-backward-section-begin)
        ("Y" . "y$")
        ("[g" . 'git-gutter+-previous-hunk)
        ("]g" . 'git-gutter+-next-hunk)
        ("\\gs" . 'git-gutter+-stage-hunks)
        ("\\gr" . 'git-gutter+-revert-hunks)
        )

  (:map evil-insert-state-map
        ("C-k" . 'company-yasnippet)
        ("C-u" . (lambda ()
                   (interactive)
                   (evil-delete (point-at-bol) (point))))
        )

  (:map evil-visual-state-map
        ("SPC" . 'hydra-global-leader/body)
        ("[g" . 'git-gutter+-previous-hunk)
        ("]g" . 'git-gutter+-next-hunk)
        ("\\gs" . 'git-gutter+-stage-hunks)
        ("\\gr" . 'git-gutter+-revert-hunks)
  )

  :custom
  (evil-ex-search-vim-style-regexp t)
  (evil-want-C-i-jump t)
  (evil-want-C-u-scroll t)
  (evil-toggle-key "C-M-z")
  (evil-undo-system 'undo-fu)

  :config
  (modify-syntax-entry ?_ "w" (standard-syntax-table))
  (evil-declare-change-repeat 'company-complete)
  (evil-add-command-properties #'find-file :jump t)
  (evil-add-command-properties #'xref-find-definitions :jump t)
  (evil-add-command-properties #'xref-find-references :jump t)
  (evil-mode 1)
  )

(use-package evil-surround :ensure t
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-magit :ensure t
  :hook (magit-mode . evil-magit-init))

(use-package evil-commentary :ensure t
  :diminish evil-commentary-mode
  :after evil
  :config (evil-commentary-mode))

(use-package evil-matchit :ensure t
  :after evil
  :config (global-evil-matchit-mode))

(use-package evil-lion :ensure t
  :after evil
  :config (evil-lion-mode))

(use-package evil-numbers
  :quelpa (evil-numbers :type git :fetcher github :repo "janpath/evil-numbers")
  :after evil
  :bind
  (:map evil-normal-state-map
        ("C-a" . evil-numbers/inc-at-pt)
        ("C-x" . evil-numbers/dec-at-pt)
        )
  (:map evil-visual-state-map
        ("C-a" . 'evil-numbers/inc-at-pt)
        ("C-x" . 'evil-numbers/dec-at-pt)
        ("g C-a" . 'evil-numbers/inc-at-pt-incremental)
        ("g C-x" . 'evil-numbers/dec-at-pt-incremental)
        ))

(use-package evil-goggles :ensure t
  :after evil
  :diminish
  :custom
  (evil-goggles-duration 0.050)
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

(use-package popwin :ensure t
  :hook (after-init . popwin-mode)
  :config
  (push '("*Error*") popwin:special-display-config)
  (push '("*Org-Babel Error Output*") popwin:special-display-config)
  (push '("*quickrun*" :regexp t :position bottom :dedicated t) popwin:special-display-config)
  (push '("*xref*" :position bottom ) popwin:special-display-config)
  )

(use-package all-the-icons :ensure t)

(use-package color-theme-sanityinc-tomorrow :ensure t :disabled
  :hook (after-init
         . (lambda ()
             (load-theme 'sanityinc-tomorrow-bright t))))

(use-package smart-mode-line :ensure t :disabled
  :custom
  (sml/no-confirm-load-theme t)
  )

(use-package doom-themes :ensure t
  :config (load-theme 'doom-sourcerer t))

(use-package doom-modeline :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (when (daemonp)
    (setq doom-modeline-icon t)))

(use-package hide-mode-line :ensure t
  :hook ((dired-mode
          dired-sidebar-mode
          gist-list-mode
          git-commit-mode
          image-mode
          inferior-python-mode
          magit-mode
          org-agenda-mode
          org-capture-mode
          org-export-stack-mode
          quickrun--mode)
         . hide-mode-line-mode)
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
       (setq-local yas-prompt-functions '(yas-x-prompt yas-completing-prompt yas-no-prompt))
       ))
  (yas-before-expand-snippet . evil-insert-state)
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'snippet-mode 'insert)
       ))

  :custom
  (require-final-newline nil)
  (yas-indent-line 'fixed)

  :config
  (setq yas-snippet-dirs (list (locate-user-emacs-file "snippets")))

  ;; YASnippet のスニペットを候補に表示するための設定
  ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (defun set-yas-as-company-backend ()
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    )
  (add-hook 'company-mode-hook 'set-yas-as-company-backend)

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

(use-package autoinsert
  :hook (find-file . auto-insert)

  :custom
  (auto-insert-query nil)
  (auto-insert-directory (concat user-emacs-directory "templates/"))

  :config
  (defun my/autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  ;; http://emacs.rubikitch.com/sd1602-autoinsert-yatemplate-yasnippet/
  (dolist (x auto-insert-alist)
    (when (equal "\\.el\\'" (car-safe (car x)))
      (setcar (car x) "/src/[^/]+\\.el\\'")))

  (setq auto-insert-alist
        (append '(
                  (("test_.*\\.py\\'" "Python test") . ["test.py" my/autoinsert-yas-expand])
                  (("setup.py\\'" "Python setup file") . "setup.py")
                  (("setup.cfg\\'" "Python setup config") . ["setup.cfg" my/autoinsert-yas-expand])
                  (("\\.cpp\\'" "C++ setup file") . ["template.cpp" my/autoinsert-yas-expand])
                  (("\\.py\\'" "Python script") . ["template.py" my/autoinsert-yas-expand])
                  (("\\.vue\\'" "Vue") . ["template.vue" my/autoinsert-yas-expand])
                  ((plantuml-mode "Plantuml") . ["template.plantuml" my/autoinsert-yas-expand])
                  ((sh-mode "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                  ((".github/workflows/.*\\.yml" "GitHub Actions") . ["actions.yml" my/autoinsert-yas-expand])
                  )
                auto-insert-alist)
        )
  )

(use-package oj :ensure t
  :custom
  (oj-default-online-judge 'atcoder))

(use-package emacs-lock
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  )

(use-package key-binding :no-require
  :config
  (define-key key-translation-map [?\C-h] [?\C-?])
  ;; (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-\\") nil)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-q") 'universal-argument)
  (global-set-key (kbd "C-s") 'Control-X-prefix)
  )

(use-package rocket-chat-post
  :commands rocket-chat-edit
  :hook
  (evil-after-load
   . (lambda ()
       (evil-set-initial-state 'rocket-chat-edit-mode 'insert)
       ))
  )

(use-package cus-edit
  :hook
  (after-init . (lambda () (load custom-file t)))
  (kill-emacs . (lambda () (delete-file custom-file)))
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  )

(use-package init-candidate :no-require
  :hook
  (after-init . (lambda () (load my/user-emacs-file-candidate t)))
  :config
  (defvar my/user-emacs-file-candidate (concat user-emacs-directory "init.candidate.el"))
  )

(use-package init-local :no-require
  :hook
  (after-init . (lambda () (load my/user-emacs-file-local t)))
  :config
  (defvar my/user-emacs-file-local (concat user-emacs-directory "init.local.el"))
  )


(provide 'init)
;;; init.el ends here
