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
(setq package-user-dir (locate-user-emacs-file "elpa"))
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
		("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "http://melpa.org/packages/")
        ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
        ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defvar darwin-p (eq system-type 'darwin))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar carbon-p (eq system-type 'mac))
(defvar meadow-p (featurep 'meadow))

(setq use-package-enable-imenu-support t)  ; Must be set before (require 'use-package)
(require 'use-package)

(use-package quelpa-use-package :ensure t
  :custom (quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice))

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

(use-package emacs
  :config

	;; autosave
  (setq auto-save-timeout 10)
  (setq auto-save-interval 100)  ;; key typing count

	;; startup
  (setq inhibit-startup-message t)

	;; subr
  (fset 'yes-or-no-p 'y-or-n-p)

	;; bell
  (setq ring-bell-function 'ignore)

	;; scroll
  (setq scroll-conservatively 1)
  (setq scroll-margin 0)

	;; buffer
  (setq-default tab-width 4)
  (setq-default indicate-buffer-boundaries 'left) ;; バッファの終端を表示
  (setq-default indicate-empty-lines t) ;; バッファの終端以降を可視化

	;; mule-cmds
  ;; unicode の一部を1文字幅として扱う
  ;; "┃" : git-gutter
  ;; "│" : highlight-indent-guides
  (set-language-environment "English")
  ;; magitでの文字化け対策
  (prefer-coding-system 'utf-8)

  :bind
  ("C-s" . Control-X-prefix)
  )

(use-package diminish :ensure t)

(use-package abbrev
  :diminish)

(use-package server
  :config
  (unless
	  (server-running-p)
	(server-start))
  )

(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  )

(use-package smooth-scrolling :ensure t
  :custom
  (smooth-scroll-margin 1)
  :config (smooth-scrolling-mode)
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
  :bind
  ("C-s-f" . toggle-frame-fullscreen)
  )

(use-package paren
  :config
  (show-paren-mode 1) ;; 対応する括弧を光らせる
  )

(use-package simple
  :bind
  ("C-q" . universal-argument) ; C-u -> C-q
  )

(use-package fringe
  :custom-face
  (fringe ((t (:background nil))))
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
  :config (global-auto-revert-mode)
  )

(use-package executable
  ;; shegang を見て自動で +x する
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  )

(use-package cc-vars
  :custom (c-basic-offset 2)
  )

(use-package syntax
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

  :custom-face
  (fringe ((t (:background nil))))

  :hook
  (window-setup . my-reload-font)
  (window-setup . set-apple-color-emoji)

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
  (trash-directory "~/.Trash")
  (confirm-kill-emacs nil)
  (find-file-visit-truename t)
  (require-final-newline t)
  :config
  (setq save-silently t)
  (let ((value
		 (list
		  "/bin"
		  "/opt/homebrew/bin"
		  "/sbin"
		  "/usr/bin"
		  "/usr/local/bin"
		  "/usr/sbin"
		  (expand-file-name "~/.local/bin")
		  (expand-file-name "~/bin")
		  )))
	(setq exec-path value)
	(setq eshell-path-env value)
	)
  )

;; (use-package osx-trash :ensure t
;;   :when darwin-p
;;   :config
;;   (setq delete-by-moving-to-trash t)
;;   ; also needs to set (trash-directory "~/.Trash") in files.el
;;   (osx-trash-setup))

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

(use-package rainbow-delimiters :ensure t :disabled
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package which-key :ensure t
  :diminish which-key-mode
  :custom
  (which-key-use-C-h-commands nil)
  (which-key-allow-evil-operators t)
  :config (which-key-mode)
  )

(use-package smartparens :ensure t
  :diminish smartparens-mode

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

  (smartparens-global-mode)
  )

(use-package restart-emacs :ensure t
  :commands restart-emacs
  )

(use-package s :ensure t)

(use-package f :ensure t)

(use-package dired
  :after evil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (evil-define-key '(normal visual) dired-mode-map
	(kbd "C-j") 'dired-next-dirline
	(kbd "C-k") 'dired-prev-dirline
	(kbd "C-c C-o") 'my-open-in-external-app
	(kbd "q") 'kill-current-buffer
	(kbd "r") 'revert-buffer
	(kbd "SPC") nil)
  )

(use-package dired-filter :ensure t)

(use-package dired-subtree :ensure t
  :after (dired evil)

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

  :config
  (evil-define-key '(normal visual) dired-mode-map
	(kbd "l") 'my-dired-subtree-insert
	(kbd "h") 'my-dired-subtree-remove)

  :custom-face
  (dired-subtree-depth-1-face ((t (:background nil))))
  (dired-subtree-depth-2-face ((t (:background nil))))
  (dired-subtree-depth-3-face ((t (:background nil))))
  (dired-subtree-depth-4-face ((t (:background nil))))
  (dired-subtree-depth-5-face ((t (:background nil))))
  (dired-subtree-depth-6-face ((t (:background nil))))
  )

(use-package dired-sidebar :ensure t
  :after dired-subtree
  :commands (dired-sidebar-toggle-sidebar)
  )

(use-package all-the-icons-dired :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package wdired
  :commands (wdired-change-to-wdired-mode)
  :custom (wdired-allow-to-change-permissions t)

  :bind
  (:map  dired-mode-map
		 ("e" . 'wdired-change-to-wdired-mode)
		 )
  )

(use-package vterm :ensure t
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "*vterm: %s*")
  :after evil
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-define-key 'emacs vterm-mode-map
	(kbd "C-x") 'vterm-send-C-x
	(kbd "C-c") 'vterm-send-C-c)
  )

(use-package vterm-toggle :ensure t
  :custom
  (vterm-toggle-scope 'project)

  :after evil
  :config
  (evil-define-key 'emacs vterm-mode-map
	(kbd "M-t") 'vterm-toggle-cd)
  (evil-define-key '(normal visual) 'global
	(kbd "M-t") 'vterm-toggle-cd)
  )

(use-package flycheck :ensure t
  :hook
  ((prog-mode yaml-mode) . flycheck-mode)
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

  ;; https://www.flycheck.org/en/28/_downloads/flycheck.html
  (flycheck-define-checker sh-shellcheck
	"A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
	:command ("shellcheck" "-f" "checkstyle"
			  "-s" (eval (symbol-name sh-shell))
			  source)
	:modes sh-mode
	:error-parser flycheck-parse-checkstyle)

  (push 'c/c++-g++ flycheck-checkers)

  ;; (flycheck-define-checker python-pycodestyle
  ;;   "A Python syntax and style checker using pycodestyle (former pep8)."

  ;;   :command ("pycodestyle" source-inplace)
  ;;   :error-patterns
  ;;   ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  ;;   :modes python-mode)

  ;; (push 'python-pycodestyle flycheck-checkers)
  )

(use-package posframe :ensure t
  :if window-system
  :config
  (use-package flycheck-posframe :ensure t
	:after flycheck
	:hook (flycheck-mode . flycheck-posframe-mode))
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

(use-package magit-todos :ensure t :disabled
  :hook (magit-mode . magit-todos-mode))

(use-package git-modes :ensure t)

(use-package git-timemachine :ensure t)

(use-package git-gutter+ :ensure t
  :diminish

  :custom
  (git-gutter+-added-sign "┃")
  (git-gutter+-deleted-sign "▔")
  (git-gutter+-modified-sign "┃")

  :custom-face
  (git-gutter+-modified ((t (:italic nil :underline nil :foreground "orange"))))
  (git-gutter+-deleted ((t (:italic nil :underline nil :foreground "red"))))
  (git-gutter+-added ((t (:italic nil :underline nil :foreground "green"))))

  :after evil
  :config
  (evil-define-key '(normal visual) 'global
	(kbd "<localleader>gg") 'git-gutter+-mode)
  (evil-define-key '(normal visual) 'git-gutter+-mode
	(kbd "[g") 'git-gutter+-previous-hunk
	(kbd "]g") 'git-gutter+-next-hunk
	(kbd "<localleader>gr") 'git-gutter+-revert-hunks
	(kbd "<localleader>gs") 'git-gutter+-stage-hunks)
  )

(use-package git-messenger :ensure t
  :commands git-messenger:popup-message)

(use-package gist :ensure
  :after evil
  :config
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
	)
  )

(use-package browse-at-remote :ensure t)

(use-package recentf
  :custom
  (recentf-save-file "~/.cache/emacs/recentf")
  (recentf-max-saved-items 2000)
  (recentf-exclude '("/.recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))

  :config
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
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

(use-package undo-fu :ensure t)

(use-package undo-fu-session :ensure t
  :after undo-fu
  :config (global-undo-fu-session-mode)
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  )

(use-package pangu-spacing :ensure t
  :hook
  ((text-mode-hook twittering-edit-mode) . pangu-spacing-mode)

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

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
  )

(use-package lsp-vetur
  :hook (vue . lsp)
  :custom
  (lsp-vetur-format-default-formatter-ts "eslint")
  (lsp-vetur-format-default-formatter-js "eslint")
  )

(use-package lsp-pyls :disabled
  :hook (python-mode . lsp)
  :custom
  (lsp-pyls-plugins-flake8-enabled t)
  (lsp-pyls-plugins-jedi-completion-include-params nil)
  (lsp-pyls-plugins-pylint-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  (lsp-pyls-plugins-autopep8-enabled nil)
  )

(use-package lsp-pyright :ensure t
  :custom
  (lsp-pyright-python-executable-cmd "python3")

  :hook
  (python-mode
   . (lambda ()
	   (require 'lsp-pyright)
	   (lsp)))

  :after evil
  :config
  (evil-define-key 'normal python-mode-map
	(kbd "<localleader>f") 'python-black-buffer)
  (evil-define-key 'visual python-mode-map
	(kbd "<localleader>f") 'python-black-region)
  )

(use-package lsp-go
  :hook (go-mode . lsp))

(use-package lsp-ui :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)

  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-sideline-enable nil)  ; instead of flycheck-posframe

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

  :after evil
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

  (global-company-mode)
  (company-tng-mode)

  (evil-define-key 'insert 'global
	(kbd "C-k") 'company-yasnippet)

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

(use-package company-restclient :ensure t
  :config
  (add-to-list 'company-backends #'company-restclient)
  )

(use-package quickrun :ensure t
  :commands quickrun

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
      (:compile-only . "%c -g %o -o a.out %s")
      (:remove  . ("%e"))
      (:description . "Compile C++ file with g++ and execute")
      (:cmdopt . "-Wall -Wextra -pedantic -std=c++14 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlong-long -Wshift-overflow -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=address -fsanitize=undefined -fno-sanitize-recover=all -fstack-protector"))
    )
  (quickrun-add-command "c++17/g++"
    '((:command . "g++")
      (:exec    . ("%c -x c++ %o -o %e %s" "%e %a"))
      (:compile-only . "%c -g %o -o a.out %s")
      (:remove  . ("%e"))
      (:description . "Compile C++ file with g++ and execute")
      (:cmdopt . "-Wall -Wextra -pedantic -std=c++17 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlong-long -Wshift-overflow -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=address -fsanitize=undefined -fno-sanitize-recover=all -fstack-protector"))
    )
  (quickrun-set-default "c++" "c++17/g++")

  (quickrun-add-command "typescript"
    '((:exec . ("%c --target es6 --module commonjs %o %s %a" "node %n.js")))
    :override t)

  (quickrun-add-command "python"
    '((:command . "python3")
      (:compile-only . "flake8 %s"))
    :override t)

  (defadvice quickrun (before quickrun-before-save ())
	"Save buffer before quickrun."
	(save-buffer))
  (defadvice quickrun-region (before quickrun-region-before-save ())
	"Save buffer before quickrun-region."
	(save-buffer))
  (ad-activate 'quickrun)
  (ad-activate 'quickrun-region)
  )

(use-package annotate :ensure t)

(use-package csharp-mode :ensure t)

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

  :custom
  (gofmt-command "goimports")

  :after evil
  :config
  (evil-define-key '(normal visual) go-mode-map
	(kbd "<localleader>f") 'gofmt)
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

(use-package tree-sitter :ensure t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config (global-tree-sitter-mode)
  )

(use-package tree-sitter-langs :ensure t)

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
  :after evil
  :config
  (evil-define-key '(normal visual) json-mode-map
	(kbd "<localleader>f") 'json-pretty-print-buffer)
  )

(use-package markdown-mode :ensure t
  :commands markdown-toggle-markup-hiding

  :custom
  (markdown-command "pandoc -s -t html5 -c ~/.emacs.d/css/github.css")
  (markdown-gfm-use-electric-backquote nil)

  :custom-face
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.4 :bold t :underline t))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2 :bold t :underline t))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1 :bold t :underline t))))

  :mode
  ("README\\.md\\'" . gfm-mode)

  :after evil
  :config
  (mapcar
   #'(lambda (map)
	   (evil-define-key '(normal visual) markdown-mode-map
		 (kbd "zo") '(lambda () (interactive) (outline-show-children) (outline-show-entry))
		 (kbd "zc") 'outline-hide-subtree
		 (kbd "TAB") 'markdown-cycle
		 (kbd "<localleader>f") 'prettier-js
		 ))
   (list markdown-mode-map gfm-mode-map))
  )

(use-package terraform-mode :ensure t)

(use-package prog-mode
  :after evil
  :config
  (evil-define-key '(normal visual) prog-mode-map
	(kbd "[e") 'flycheck-previous-error
	(kbd "]e") 'flycheck-next-error

	(kbd "gd") 'xref-find-definitions
	(kbd "gr") 'xref-find-references

	(kbd "<localleader>R") 'lsp-rename
	(kbd "<localleader>e") 'flycheck-list-errors
	(kbd "<localleader>f") 'lsp-format-buffer
	(kbd "<localleader>m") 'lsp-ui-imenu
	(kbd "<localleader>qa") 'quickrun-autorun-mode
	(kbd "<localleader>qc") 'quickrun-compile-only
	(kbd "<localleader>qs") 'quickrun-shell)
  (evil-define-key 'normal prog-mode-map
	(kbd "<localleader>qr") 'quickrun
	(kbd "<localleader>r") 'quickrun)
  (evil-define-key 'visual prog-mode-map
	(kbd "<localleader>qr") 'quickrun-region
	(kbd "<localleader>r") 'quickrun-region)
  )

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  )

(use-package edit-indirect :ensure t
  :commands edit-indirect-region
  )

(use-package org :ensure org-contrib
  :hook
  (org-after-todo-statistics . org-summary-todo)
  (org-checkbox-statistics . my/org-checkbox-todo)
  (org-mode . org-indent-mode)
  (org-mode . prettify-symbols-mode)
  (org-mode
   . (lambda ()
       (setq prettify-symbols-alist
	     '(("[ ]" . "☐")
	       ("[X]" . "☑")
	       ("[-]" . "◪")))))

  :custom
  (org-directory "~/org/")
  (org-startup-folded t)
  (org-startup-with-inline-images nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively nil)
  (org-hide-leading-stars t) ; 見出しの余分な*を消す
  (org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)" "STARTED" "|")
     (sequence "|" "SOMEDAY(S)")
     (sequence "|" "CANCELLED(c@)")
     (sequence "|" "MEETING(m)")))
  (org-log-done 'time) ; DONEの時刻を記録
  (org-hidden-keywords '(title))
  (org-image-actual-width nil)  ; to use #+ATTR_ORG: :width

  :after evil
  :config
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
                    (when (string-equal todo-state "DONE")
                      (org-todo 'todo)))
                (if (and (> (match-end 2) (match-beginning 2))
                         (equal (match-string 2) (match-string 3)))
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done))
                  (when (string-equal todo-state "DONE")
                    (org-todo 'todo)))))))))

  (defun my/org-todo-next ()
	"Org todo next cycle"
	(interactive) (org-call-with-arg 'org-todo 'right)
	)

  (evil-define-key '(normal visual) org-mode-map
	;; leader mapping
	(kbd "<leader>/") 'my-outline

	;; localleader mapping
	(kbd "<localleader>dc") 'org-download-clipboard
	(kbd "<localleader>dd") 'org-download-delete
	(kbd "<localleader>ds") 'org-download-screenshot
	(kbd "<localleader>f") 'whitespace-cleanup
	(kbd "<localleader>i") 'org-clock-in
	(kbd "<localleader>nb") 'org-narrow-to-block
	(kbd "<localleader>ne") 'org-narrow-to-element
	(kbd "<localleader>nf") 'narrow-to-defun
	(kbd "<localleader>ns") 'org-narrow-to-subtree
	(kbd "<localleader>nw") 'widen
	(kbd "<localleader>p") 'org-priority
	(kbd "<localleader>q") 'org-set-tags-command
	(kbd "<localleader>s") 'org-schedule
	(kbd "<localleader>t") 'org-todo
	(kbd "<localleader>v") 'org-toggle-inline-images
	(kbd "<localleader>xp") 'org-set-property

	(kbd "C-S-j") 'org-next-visible-heading
	(kbd "C-S-k") 'org-previous-visible-heading
	(kbd "<M-return>") '(lambda () (interactive) (evil-append-line 1) (org-meta-return))
	(kbd "M-RET") '(lambda () (interactive) (evil-append-line 1) (org-meta-return))
	(kbd "<C-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-heading-after-current))
	(kbd "C-RET") '(lambda () (interactive) (evil-insert-state) (org-insert-heading-after-current))
	(kbd "<M-S-return>") '(lambda () (interactive) (evil-append-line 1) (org-insert-todo-heading 1))
	(kbd "<C-S-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-todo-heading-respect-content))
	(kbd "T") 'org-todo
	(kbd "t") 'my/org-todo-next
	(kbd "<") 'org-metaleft
	(kbd ">") 'org-metaright
	(kbd "gh") 'outline-up-heading
	(kbd "gp") 'outline-previous-heading
	(kbd "TAB") 'org-cycle
	)

  (evil-define-key '(normal insert visual) org-mode-map
	(kbd "M-S-h") 'org-metashiftleft
	(kbd "M-S-j") 'org-metashiftdown
	(kbd "M-S-k") 'org-metashiftup
	(kbd "M-S-l") 'org-metashiftright
	(kbd "M-h") 'org-metaleft
	(kbd "M-j") 'org-metadown
	(kbd "M-k") 'org-metaup
	(kbd "M-l") 'org-metaright
	(kbd "RET") 'org-return
	)
  )

(use-package org-faces
  :after org

  :custom
  (org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("STARTED" :foreground "orange" :weight bold)
     ("DOING" :foreground "orange" :weight bold)
     ("WAITING" :foreground "light pink" :weight bold)
     ("SOMEDAY" :foreground "dark gray")
     ("DONE" :foreground "forest green" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("AGENDA" :foreground "sky blue" :weight bold)
     ("MEETING" :foreground "sky blue" :weight bold)))
  ;; Only use the first 4 styles and do not cycle.
  (org-cycle-level-faces nil)
  (org-n-level-faces 4)

  :custom-face
  ;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
  (org-level-1 ((t (:inherit 'outline-1 :height 1.4)))) ;\LARGE
  (org-level-2 ((t (:inherit 'outline-2 :height 1.2)))) ;\Large
  (org-level-3 ((t (:inherit 'outline-3 :height 1.1)))) ;\large
  ;; Document Title, (\huge)
  (org-document-title ((t (:height 2.074 :inherit 'org-level-8))))

  (org-block-begin-line ((t (:height 0.8 :foreground "gray40"))))
  (org-date ((t (:height 0.7 :foreground "gold4"))))
  (org-drawer ((t (:height 0.55 :foreground "gray40"))))
  (org-meta-line ((t (:height 0.8 :foreground "gray40"))))
  (org-property-value ((t (:height 0.8))))
  (org-special-keyword ((t (:height 0.7 :foreground "gray40"))))
  (org-sexp-date ((t (:height 0.7 :foreground "gray40"))))
  )

(use-package org-agenda
  :after (evil org)
  :commands (org-agenda org-refile)
  :demand t

  :custom
  (org-agenda-current-time-string "← now")
  (org-agenda-time-grid ;; Format is changed from 9.1
   '((daily today require-timed)
	 (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
	 "-"
	 "────────────────"))
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (org-agenda-files `(,org-directory))
  (org-agenda-span 'week)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t :tags t :hidefiles t))

  :config
  (defun my/org-agenda-todo-next ()
    "Org agenda todo next cycle"
    (interactive) (org-call-with-arg 'org-agenda-todo 'right)
    )
  (evil-set-initial-state 'org-agenda-mode 'emacs)

  :bind
  (:map org-agenda-mode-map
		(":" . evil-ex)
		("K" . org-capture)
		("T" . org-agenda-todo)
		("j" . org-agenda-next-item)
		("k" . org-agenda-previous-item)
		("t" . my/org-agenda-todo-next)
		))

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
	(when (and (or
				;; (string= org-state "TODO")
				(string= org-state "WAITING"))
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
  :after org
  :hook
  (org-babel-after-execute . org-display-inline-images)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-C++-compiler "g++ -Wall -Wextra -std=c++14")
  :config
  ;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
  ;; (defun my-org-confirm-babel-evaluate (lang body)
  ;; 	(not (member lang
  ;; 				 '(
  ;; 				   "C"
  ;; 				   "cpp"
  ;; 				   "dot"
  ;; 				   "elisp"
  ;; 				   "go"
  ;; 				   "js"
  ;; 				   "plantuml"
  ;; 				   "python"
  ;; 				   "restclient"
  ;; 				   "sh"
  ;; 				   "shell"
  ;; 				   "ts"
  ;; 				   "typescript"
  ;; 				   "uml"
  ;; 				   ))))

  (push '("ts" . typescript) org-src-lang-modes)
  (push '("uml" . plantuml) org-src-lang-modes)
  )

(use-package ob-restclient :ensure t)

(use-package ob-exp
  :custom
  (org-export-use-babel t))

(defvar my/plantuml-java-options "-Djava.awt.headless=true") ; plantuml-modeのdefaultになったけどob-plantumlで使う
(defvar my/plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar")) ; ob-plantumlで使う
(defvar my/plantuml-jar-args (list "-charset" "UTF-8" "-config" (expand-file-name "~/.config/plantuml/color.uml"))) ; ob-plantumlで使う

(use-package ob-plantuml
  :after ob
  :hook
  (ob-async-pre-execute-src-block
   . (lambda ()
       (setq org-plantuml-jar-path "~/lib/java/plantuml.jar")))
  :custom
  (org-plantuml-jar-path my/plantuml-jar-path)
  (plantuml-server-url nil)
  :config
  (push (cons ':java my/plantuml-java-options) org-babel-default-header-args:plantuml)
  (push (cons ':cmdline (s-join " " my/plantuml-jar-args)) org-babel-default-header-args:plantuml)
  ;; (push '(:async) org-babel-default-header-args:plantuml)
  (push '(:cache . "yes") org-babel-default-header-args:plantuml)
  )

(use-package ob-mermaid :ensure t
  :if darwin-p
  :after ob
  :custom
  (ob-mermaid-cli-path "/opt/homebrew/bin/mmdc"))

(use-package ob-shell
  :after ob)

(use-package ob-python
  :after auto-virtualenvwrapper
  :hook
  (org-mode
   . (lambda ()
	   (let ((path (auto-virtualenvwrapper-find-virtualenv-path)))
		 (when path
		   (setq-local org-babel-python-command (concat path "bin/python"))))))
  :custom
  (org-babel-python-command "python3"))

(use-package ob-C
  :after ob
  :custom
  (org-babel-default-header-args:C '((:async) (:cache . "yes")))
  (org-babel-default-header-args:C++
   (append org-babel-default-header-args:C '((:includes . "<iostream>"))))
  )

(use-package ob-async :ensure t
  :after ob)

(use-package ob-ipython :ensure t :disabled
  :after ob)

(use-package ob-js
  :config
  (setq org-babel-js-function-wrapper
        "require('util').inspect(function(){\n%s\n}());")
  )

(use-package ob-typescript :ensure t)

(use-package ob-go :ensure t)

(use-package org-capture
  :commands org-capture

  :hook
  (org-capture-mode . evil-insert-state)

  :custom
  (org-capture-templates
   `(
	 ("B" "Blog" plain (file+olp "blog.org" "Blog Ideas") "hugo%?")
	 ("I" "Interrupt - Add an interrupt task" entry (file+olp+datetree "journal.org") "** Interrupted task\n%T\n%?" :clock-in t :clock-resume t)
	 ("b" "Book" table-line (file+headline "books.org" "wish list") "|Name|Price|eBook?|Created|\n|%?|||%U|" :table-line-pos "II-1")
	 ("j" "Journal" entry (file+olp+datetree "journal.org") "** %?\n%T")
	 ("l" "Log Time" entry (file+olp+datetree "journal.org") "** %U %^{Log} :Time:" :immediate-finish t)
	 ("m" "Meeting" entry (file+olp+datetree "journal.org") "** %^{Title} :MEETING:\n%T%^{CATEGORY}p%?" :jump-to-captured t :clock-in t :clock-keep t :immediate-finish t)
	 ("n" "Note" entry (file+olp+datetree "journal.org") "** %? :Note:\n%T %a")
	 ("t" "Task" entry (file+olp+datetree "journal.org") "** TODO %?\nSCHEDULED: %^T\n%(org-mac-chrome-get-frontmost-url)")
	 ("s" "Start Task" entry (file+olp+datetree "journal.org") "** STARTED %(org-mac-chrome-get-frontmost-url)\n%T\n%?" :clock-in t :clock-resume t)
	 ))

  :after evil
  :config
  (defun my-org-daily-journal-file ()
	(concat
	 (file-name-as-directory org-directory)
	 "journals/"
	 (format-time-string "%Y-%m-%d.org" (current-time))))
  (defun my-open-todays-journal ()
	(interactive)
	(find-file (my-org-daily-journal-file)))

  (evil-define-key '(normal insert) 'global
	(kbd "<leader> oo") 'my-open-todays-journal)
  )

(use-package org-checklist
  :after org
  )

(use-package org-superstar :ensure t
  :hook (org-mode . org-superstar-mode)

  :custom
  ;; Set different bullets, with one getting a terminal fallback.
  ;; (org-superstar-headline-bullets-list '("◉" ("🞛" ?◈) "○" "▷"))
  ;; Stop cycling bullets to emphasize hierarchy of headlines.
  ;; (org-superstar-cycle-headline-bullets nil)
  ;; Hide away leading stars on terminal.
  (org-superstar-leading-fallback ?\s)

  :custom-face
  (org-superstar-item          ((t (:height 1.2))))
  (org-superstar-header-bullet ((t (:height 1.2))))
  (org-superstar-leading       ((t (:height 1.3))))
  )
;; (use-package org-modern :ensure t
;;   :quelpa (org-modern :fetcher github :repo "minad/org-modern")
;;   :hook (org-mode . org-modern-mode)
;;   )

(use-package org-download :ensure t
  :custom
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir (concat (file-name-as-directory org-directory) "images"))
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
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-m IPython --simple-prompt -i")
  )

(use-package auto-virtualenvwrapper :ensure t
  :commands auto-virtualenvwrapper-find-virtualenv-path
  :hook (python-mode . auto-virtualenvwrapper-activate))

(use-package py-yapf :ensure t
  :commands (py-yapf-buffer)
  )

(use-package python-black :ensure t
  :custom (python-black-extra-args '("--skip-string-normalization"))
  :commands (pyhton-black-buffer pyhton-black-region)
  )

(use-package py-isort :ensure t
  :after evil
  :config
  (evil-define-key 'normal python-mode-map
	(kbd "<localleader>i") 'py-isort-buffer)
  (evil-define-key 'visual python-mode-map
	(kbd "<localleader>i") 'py-isort-region)
  )

(use-package sh-script
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+))

(use-package shfmt :ensure t
  :after evil
  :config
  (evil-define-key 'normal sh-mode-map
	(kbd "<localleader>f") 'shfmt-buffer)
  (evil-define-key 'visual sh-mode-map
	(kbd "<localleader>f") 'shfmt-region)

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

(use-package mermaid-mode :ensure t)

(use-package plantuml-mode :ensure t
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 2)
  (plantuml-jar-args my/plantuml-jar-args)
  (plantuml-jar-path my/plantuml-jar-path)
  (plantuml-java-options my/plantuml-java-options)
  (plantuml-output-type 'svg)
  )

(use-package flycheck-plantuml :ensure t
  :hook (plantuml-mode . flycheck-plantuml-setup)
  )

(use-package web-mode :ensure t
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

  :after evil
  :config
  (evil-define-key 'normal web-mode-map
	(kbd "<localleader>R") 'web-mode-element-rename
	(kbd "zc") 'web-mode-fold-or-unfold
	(kbd "zo") 'web-mode-fold-or-unfold)
  )

(use-package vue-mode :ensure t
  :hook
  (vue-mode
   . (lambda ()
       (setq syntax-ppss-table nil)
       (add-hook 'after-save-hook 'mmm-parse-buffer nil t)
       ))
  (vue-mode . lsp)

  :after evil
  :config
  (evil-define-key '(normal visual) vue-mode-map
	(kbd "<localleader>f") 'eslint-fix)
  )

(use-package mmm-mode
  :diminish
  :custom
  (mmm-submode-decoration-level 0)
  )

(use-package yaml-mode :ensure t
  :after evil
  :config
  (evil-define-key '(normal visual) yaml-mode-map
	(kbd "C-m") 'newline-and-indent
	(kbd "<localleader>e") 'flycheck-list-errors)
  )

(use-package flycheck-yamllint :ensure t
  :hook (yaml-mode . flycheck-yamllint-setup))

(use-package vimrc-mode :ensure t)

(use-package emmet-mode :ensure t
  :diminish
  :hook (sgml-mode css-mode web-mode xml-mode js-jsx-mode typescript-mode)
  :custom
  (emmet-indent-after-insert nil)
)

(use-package text-mode
  :mode
  ("\\.qrinput\\'" . text-mode)
  )

(use-package smartrep :ensure t
  :after evil
  :config
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
  )

(use-package evil :ensure t

  :bind
  (:map evil-normal-state-map
		("C-h" . 'evil-backward-char)
		("C-j" . 'evil-forward-paragraph)
		("C-k" . 'evil-backward-paragraph)
		("C-l" . 'redraw-display)
		("S-C-j" . 'evil-forward-section-begin)
		("S-C-k" . 'evil-backward-section-begin)
		("Y" . "y$")
		)

  (:map evil-insert-state-map
		("C-u" . (lambda ()
				   (interactive)
				   (evil-delete (point-at-bol) (point))))
		)

  :init
  (setq evil-want-keybinding nil)

  :custom
  (evil-ex-search-vim-style-regexp t)
  (evil-toggle-key "C-M-z")
  (evil-undo-system 'undo-fu)
  (evil-want-C-i-jump t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)

  :config
  (modify-syntax-entry ?_ "w" (standard-syntax-table))
  (evil-declare-change-repeat 'company-complete)
  (evil-add-command-properties #'find-file :jump t)
  (evil-add-command-properties #'xref-find-definitions :jump t)
  (evil-add-command-properties #'xref-find-references :jump t)

  (evil-set-leader '(normal visual) (kbd "SPC"))
  (evil-set-leader '(normal visual) (kbd "\\") t) ; localleader

  (evil-define-key '(normal visual) 'global
	(kbd "<leader>/") 'imenu
	(kbd "<leader>G g") 'google-this
	(kbd "<leader>G t") 'google-translate-enja-or-jaen
	(kbd "<leader>a") 'org-agenda
	(kbd "<leader>b") 'bookmark-jump
	(kbd "<leader>c") 'org-capture
	(kbd "<leader>f b") 'switch-to-buffer
	(kbd "<leader>f d") 'dired-sidebar-toggle-sidebar
	(kbd "<leader>f f") 'find-file
	(kbd "<leader>f g") 'grep-find
	(kbd "<leader>f j") 'open-junk-file
	(kbd "<leader>f o") 'my-open-current-dir
	(kbd "<leader>f r") 'recentf-open-files
	(kbd "<leader>g c") 'magit-commit
	(kbd "<leader>g f") 'my-git-find
	(kbd "<leader>g g") 'vc-git-grep
	(kbd "<leader>g lb") 'gist-buffer
	(kbd "<leader>g ll") 'gist-list
	(kbd "<leader>g m") 'git-messenger:popup-message
	(kbd "<leader>g o") 'browse-at-remote
	(kbd "<leader>g p") 'my-ghq
	(kbd "<leader>g s") 'magit-stage
	(kbd "<leader>g t") 'git-timemachine
	(kbd "<leader>g u") 'magit-unstage
	(kbd "<leader>h f") 'describe-function
	(kbd "<leader>h k") 'describe-bindings
	(kbd "<leader>h v") 'describe-variable
	(kbd "<leader>o I") 'org-clock-in
	(kbd "<leader>o O") 'org-clock-out
	(kbd "<leader>o Q") 'org-clock-cancel
	(kbd "<leader>o a") 'org-agenda
	(kbd "<leader>o b") 'org-switchb
	(kbd "<leader>o c") 'org-capture
	(kbd "<leader>o j") 'org-clock-goto
	(kbd "<leader>o l") 'org-store-link
	(kbd "<leader>o s") 'org-save-all-org-buffers
	(kbd "<leader>s") 'magit-status
	(kbd "<leader>t h") 'twit
	(kbd "<leader>t m") 'twittering-mentions-timeline
	(kbd "<leader>t r") 'rocket-chat-edit
	(kbd "<leader>t u") 'twittering-update-status-interactive
	(kbd "<leader>z e") 'eval-buffer
	(kbd "<leader>z k") 'save-buffers-kill-emacs
	(kbd "<leader>z r") 'restart-emacs
	)

  (evil-mode)
  )

(use-package evil-surround :ensure t
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-collection :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init
   '(
	 (occur replace)
	 (package-menu package)
	 dired
	 flycheck
	 ibuffer
	 magit
	 magit-todos
	 quickrun
	 xref
	 )))

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

(use-package evil-numbers :ensure t
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
  :config
  (push '("*Error*") popwin:special-display-config)
  (push '("*Org-Babel Error Output*") popwin:special-display-config)
  (push '("*quickrun*" :regexp t :position bottom :dedicated t) popwin:special-display-config)
  (push '("*xref*" :position bottom ) popwin:special-display-config)
  (popwin-mode)
  )

(use-package all-the-icons :ensure t)

(use-package color-theme-sanityinc-tomorrow :ensure t :disabled)

(use-package doom-themes :ensure t :disabled)

(use-package doom-modeline :ensure t :disabled)

(use-package mini-modeline :ensure t :disabled t
  :diminish mini-modeline-mode
  :custom
  (mini-modeline-face-attr `(:background nil))
  :custom-face
  (mini-modeline-mode-line
   ((t (:background ,(face-attribute 'window-divider :foreground) :height 0.14 :box nil)))))

(use-package modus-themes :ensure t
  :custom
  (modus-themes-headings
   '((1 . (overline))
	 (2 . (rainbow overline))
	 (t . (no-bold))))
  (modus-themes-paren-match '(bold underline))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t))

(use-package my-theme :no-require
  :config
  (load-theme 'modus-vivendi t))

(use-package hide-mode-line :ensure t
  :hook ((
		  dired-mode
		  dired-sidebar-mode
		  gist-list-mode
		  git-commit-mode
		  image-mode
		  inferior-python-mode
		  lisp-interaction-mode
		  magit-mode
		  org-agenda-mode
		  org-capture-mode
		  org-export-stack-mode
		  quickrun--mode
		  )
		 . hide-mode-line-mode)
  )

(use-package beacon :ensure t
  :config (beacon-mode)
  :diminish
  )

(use-package clipetty :ensure t
  :diminish clipetty-mode
  :config (global-clipetty-mode))

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

  :custom-face
  (whitespace-empty    ((t (:background "controlBackgroundColor" :foreground "DeepPink" :underline t))))
  (whitespace-space    ((t (:background "controlBackgroundColor" :foreground "GreenYellow" :weight bold))))
  (whitespace-tab      ((t (:background "controlBackgroundColor"))))
  (whitespace-trailing ((t (:background "controlBackgroundColor" :foreground "DeepPink" :underline t :inherit 'default))))

  :config
  (set-display-table-slot standard-display-table 'truncation ?<) ; set lcs=extends:<,precedes:<
  (setcar (nthcdr 2 (assq 'space-mark whitespace-display-mappings)) [?_]) ; set nbsp:%
  )

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :hook
  (yas-minor-mode
   . (lambda ()
       (setq-local yas-prompt-functions '(yas-x-prompt yas-completing-prompt yas-no-prompt))
       ))
  (yas-before-expand-snippet . evil-insert-state)
  :custom
  (require-final-newline nil)
  (yas-indent-line 'fixed)

  :after evil
  :config
  (yas-global-mode)
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

  (evil-set-initial-state 'snippet-mode 'insert)

  :bind
  (:map yas-keymap
        ("<tab>" . nil)
        ("RET" . yas-next-field-or-maybe-expand))
  )

(use-package google-this :ensure t
  :commands google-this
  )

(use-package google-translate :ensure t
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

(use-package ol
  :bind ("C-c l" . org-store-link))

(use-package key-binding :no-require
  :config
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\C-\[] [?\C-?])
  ;; (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-\\") nil)
  )

(use-package rocket-chat-post
  :commands rocket-chat-edit
  :after evil
  :config
  (evil-set-initial-state 'rocket-chat-edit-mode 'insert)
  )

(use-package cus-edit
  :hook
  (kill-emacs . (lambda () (delete-file custom-file)))
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  :config (load custom-file t)
  )

(use-package init-candidate :no-require
  :config
  (defvar my/user-emacs-file-candidate (concat user-emacs-directory "init.candidate.el"))
  (load my/user-emacs-file-candidate t)
  )

(use-package init-local :no-require
  :config
  (defvar my/user-emacs-file-local (concat user-emacs-directory "init.local.el"))
  (load my/user-emacs-file-local t)
  )


(provide 'init)
;;; init.el ends here
