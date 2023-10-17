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


(defvar my/profiler-enabled nil)
(when my/profiler-enabled
  (require 'profiler)
  (profiler-start 'cpu)
  (add-hook 'after-init-hook
            #'(lambda ()
                (profiler-report)
                (profiler-stop)))
  )

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq load-path (cons
                 (locate-user-emacs-file "elisp")
                 load-path))

(require 'package)
(setq package-user-dir (locate-user-emacs-file "elpa"))
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
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
(defvar wsl-p
  (let* ((ver-file "/proc/version")
         (exists? (file-exists-p ver-file)))
    (when exists?
      (with-temp-buffer
        (insert-file-contents ver-file)
        (when (string-match "WSL" (buffer-string)) t)))))

(defvar use-package-enable-imenu-support t)  ; Must be set before (require 'use-package)
(require 'use-package)

(defun my/open-file-darwin (file-or-directory)
  "Open FILE-OR-DIRECTORY in darwin."
  (let* ((cmd (format "open %s" file-or-directory)))
    (shell-command cmd)))

(defun my/open-file-wsl (file-or-directory)
  "Open FILE-OR-DIRECTORY in wsl."
  (let* ((cmd (format "explorer.exe $(wslpath -w %s)" file-or-directory)))
    (shell-command cmd)))

(defvar my/open-file-function
  (or
   (when darwin-p #'my/open-file-darwin)
   (when wsl-p #'my/open-file-wsl)))

(defun my/open-file (file-or-directory)
  "Open FILE-OR-DIRECTORY."
  (funcall my/open-file-function file-or-directory))

(defun my/open-current-dir ()
  (interactive)
  (my/open-file "."))

(defun my/find-up-directory (filename basedir)
  "Find a FILENAME in upper level directories from BASEDIR."
  (let ((dir basedir)
        (file nil)
        (found-file nil))
    (while dir
      (setq file (f-join dir filename))
      (when (and (not found-file) (file-exists-p file))
        (setq found-file file))
      (setq dir (f-dirname dir)))
    found-file))

(defvar bootstrap-version)
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

(use-package general :ensure t)

(use-package emacs
  :hook
  ((ansible
    yaml-mode
    emacs-lisp-mode)
   . (lambda ()
       (setq indent-tabs-mode nil)))

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

  (set-language-environment "Japanese")
  ;; magitでの文字化け対策
  (prefer-coding-system 'utf-8)

  :bind-keymap
  ("C-s" . ctl-x-map)
  )

(use-package man
  ;; The 'nanual-entry' command locks Emacs for several seconds, which is annoying.
  :bind ("s-M" . nil)
  )

(use-package diminish :ensure t)

(use-package abbrev
  :diminish)

(use-package server
  :hook (after-init . server-start)
  )

(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  )

(use-package pixel-scroll
  :hook (mouse-wheel-mode . pixel-scroll-mode)
  :custom
  ;; https://www.reddit.com/r/emacs/comments/8sw3r0/finally_scrolling_over_large_images_with_pixel/
  (pixel-dead-time 0)
  (pixel-resolution-fine-flag t)
  )

(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1)) ; one line at a time
  (mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
  (mouse-wheel-follow-mouse 't) ; scroll window under mouse
  )

;; Mouse scrolling in terminal emacs
;; https://stackoverflow.com/questions/18198387/how-do-i-mouse-scroll-in-emacs-in-the-terminal-i-havent-gotten-mouse-wheel-mod
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(use-package paren
  :config
  (show-paren-mode 1) ;; 対応する括弧を光らせる
  )

(use-package simple
  :general
  (:states
   '(normal visual insert)
   "C-q" 'universal-argument ; C-u -> C-q
  ))

(use-package tempbuf :straight t
  :hook
  (find-file
   . (lambda ()
       (when (string-match "^/private/tmp\\|^/tmp" (buffer-file-name))
         (turn-on-tempbuf-mode))))
  :custom
  (tempbuf-kill-message nil)
  )

(use-package midnight
  :hook (find-file . midnight-mode))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t) ; シンボリックリンクの読み込みを許可
  (auto-revert-check-vc-info t) ; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
  (large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
  (tags-revert-without-query 1) ; TAGS ファイルを自動で再読込
  )

(use-package autorevert
  :config (global-auto-revert-mode t)
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

  :hook
  (window-setup . my/reload-font)
  (window-setup . set-apple-color-emoji)

  :config
  (defvar yhiraki-font 'cica)

  (defun my/reload-font (&optional frame)
    "reload my font settings"
    (interactive)

    ;; Osaka + Menlo
    (when (eq yhiraki-font 'osaka)
      (set-face-attribute 'default     nil :family "Menlo" :height 120)
      (set-face-attribute 'fixed-pitch nil :family "Menlo")
      (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Osaka"))
      (push '("Osaka" . 1.2) face-font-rescale-alist) ; 全角文字を2文字幅に揃える
      )

    ;; Cica
    (when (eq yhiraki-font 'cica)
      (set-face-attribute 'default     nil :family "Cica" :height 160)
      (set-face-attribute 'fixed-pitch nil :family "Cica")
      ;; apple color emoji
      (push '("Apple color emoji" . 0.8) face-font-rescale-alist) ; 4文字幅に揃える
      )

    ;; Jetbrains mono
    (when (eq yhiraki-font 'jetbrains-mono)
      (set-face-attribute 'default     nil :family "Jetbrains Mono" :height 140)
      (set-face-attribute 'fixed-pitch nil :family "Jetbrains Mono")
      ;; 日本語
      (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Osaka"))
      (push '("Osaka" . 1.2) face-font-rescale-alist) ; 全角文字を2文字幅に揃える
      ;; apple color emoji
      (push '("Apple color emoji" . 0.9) face-font-rescale-alist) ; 4文字幅に揃える
      )

    (remove-hook 'window-setup-hook #'my/reload-font)
    )

  (defun set-apple-color-emoji ()
    "set apple color emoji"
    (set-fontset-font nil '(#x1F000 . #x1FAFF) "Apple Color Emoji")
    (remove-hook 'find-file-hook #'set-apple-color-emoji))

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
                 (fira-code-mode))))
  )

(use-package files
  :custom
  (confirm-kill-emacs nil)
  (find-file-visit-truename t)
  (require-final-newline t)
  (safe-local-variable-values '((org-log-done)))
  (trash-directory "~/.Trash")
  :config
  (setq save-silently t)
  )

(use-package time-stamp
  :commands time-stamp
  :hook
  (before-save . time-stamp)
  :custom
  (time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S")
  )

(use-package exec-path-from-shell :ensure t
  :custom
  (exec-path-from-shell-variables
   '(
     "GOPATH"
     "MANPATH"
     "PATH"
     ))
  :config
  (exec-path-from-shell-initialize))

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

(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode-hook lisp-mode-hook) . eldoc-mode)
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
  (defun my/open-block-c-mode (id action context)
    (when (eq action 'insert)
      (newline)
      (indent-according-to-mode)
      (forward-line)
      (indent-according-to-mode)))

  ;; (sp-with-modes '(prog-mode vue-mode)
  ;;   (sp-local-pair  "{" nil :post-handlers '((my/open-block-c-mode "RET")))
  ;;   )

  (smartparens-global-mode)
  )

(use-package restart-emacs :ensure t
  :commands restart-emacs
  )

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)

  :init
  (defun my/dired-open-in-external-app ()
    "Open file in external app on dired."
    (interactive)
    (my/open-file (dired-get-filename)))

  :config
  (use-package dired-filter :ensure t)

  (use-package dired-subtree :ensure t
    :after dired

    :custom-face
    (dired-subtree-depth-1-face ((t (:background nil))))
    (dired-subtree-depth-2-face ((t (:background nil))))
    (dired-subtree-depth-3-face ((t (:background nil))))
    (dired-subtree-depth-4-face ((t (:background nil))))
    (dired-subtree-depth-5-face ((t (:background nil))))
    (dired-subtree-depth-6-face ((t (:background nil))))
    )

  (use-package dired-sidebar :ensure t
    :commands (dired-sidebar-toggle-sidebar)
    )

  (use-package all-the-icons-dired :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package wdired
    :commands (wdired-change-to-wdired-mode)
    :custom (wdired-allow-to-change-permissions t)
    )
  )

(use-package vterm :ensure t
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "*vterm: %s*")

  :config
  (defun my/get-tmux-prefix ()
    (let* ((out (shell-command-to-string "tmux show-options -g prefix")))
      (when out
        (string-match "prefix \\(.*\\)" out)
        (match-string 1 out))))

  (defvar my/tmux-prefix "C-t")

  (defun my/vterm-insert-tmux-detach ()
    (interactive)
    (unless my/tmux-prefix
      (error "Set tmux prefix key"))
    (let ((buf (current-buffer)))
      (vterm-send (kbd my/tmux-prefix))
      (vterm-send (kbd "d"))

      ;; unless TMUX buffer
      (sleep-for 0.2)
      (when (buffer-live-p buf)
        (quit-window))))

  :bind
  ("M-t" . #'vterm-other-window)

  :general
  (:keymaps 'vterm-mode-map
            :states '(insert emacs)
            "C-a" #'vterm--self-insert
            "C-c" #'vterm--self-insert
            "C-d" #'vterm--self-insert
            "C-e" #'vterm--self-insert
            "C-h" #'vterm--self-insert
            "C-k" #'vterm--self-insert
            "C-n" #'vterm--self-insert
            "C-p" #'vterm--self-insert
            "C-r" #'vterm--self-insert
            "C-x" #'vterm--self-insert
            "M-t" #'my/vterm-insert-tmux-detach
            "SPC" #'vterm--self-insert
            )
  )

(use-package flycheck :ensure t
  :hook
  ((
    markdown-mode
    org-mode
    prog-mode
    yaml-mode
    ) . flycheck-mode)
  :custom
  (flycheck-python-flake8-executable "python3")
  (flycheck-python-pycompile-executable "python3")
  (flycheck-python-pylint-executable "python3")
  (flycheck-deferred-syntax-check t)
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'textlint 'org-mode)
  (flycheck-add-next-checker 'markdown-markdownlint-cli 'textlint)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode)
  :general
  (:keymaps 'flycheck-mode-map
            :states '(normal visual)
            "<localleader>e" #'flycheck-list-errors
            "[e" #'flycheck-previous-error
            "]e" #'flycheck-next-error
            )
  )

(use-package posframe :ensure t
  :if window-system
  :config
  (use-package flycheck-posframe :ensure t
    :hook (flycheck-mode . flycheck-posframe-mode))
  )

(use-package magit :ensure t
  :commands (magit-status magit-stage)
  :custom
  (magit-save-repository-buffers nil)
  (magit-diff-refine-hunk 'all)

  :config
  (use-package magit-todos :ensure t
    :hook (magit-mode . magit-todos-mode)
    :custom
    (magit-todos-exclude-globs '(".git/" "*.js.map" "*.css.map")))

  (use-package git-modes :ensure t)

  (use-package git-timemachine :ensure t)

  (use-package git-messenger :ensure t
    :commands git-messenger:popup-message)

  )

(use-package blamer :ensure t
  :config
  (defvar my/blamer-enabled nil)
  (defun my/toggle-blamer ()
    (interactive)
    (setq my/blamer-enabled (not my/blamer-enabled))
    (unless my/blamer-enabled
      (my/blamer-turnoff)))
  (defun my/blamer-turnon ()
    (when my/blamer-enabled
      (blamer-mode 1)))
  (defun my/blamer-turnoff ()
    (blamer-mode -1))
  (defun my/setup-blamer-mode()
    (add-hook 'evil-insert-state-entry-hook #'my/blamer-turnoff)
    (add-hook 'evil-insert-state-exit-hook #'my/blamer-turnon))
  (my/setup-blamer-mode)
  :general
  (:states '(normal visual)
           "<localleader>gb" 'my/toggle-blamer)
  )

(use-package gist :ensure t
  :config
  (evil-set-initial-state 'gist-list-mode 'insert)
  :general
  (:keymaps 'gist-list-menu-mode-map
            :states '(normal visual)
            "RET" #'gist-fetch-current
            "*" #'gist-star
            "+" #'gist-add-buffer
            "-" #'gist-remove-file
            "^" #'gist-unstar
            "b" #'gist-browse-current-url
            "e" #'gist-edit-current-description
            "f" #'gist-fork
            "r" #'gist-list-reload
            "K" #'gist-kill-current
            "y" #'gist-print-current-url
            "<tab>" #'gist-fetch-current-noselect
            "q" #'quit-window)
  )

(use-package diff-hl :ensure t
  :general
  (:states '(normal visual)
           "<localleader>gg" #'diff-hl-mode
           "<localleader>gr" #'diff-hl-revert-hunk
           "<localleader>gs" #'diff-hl-stage-current-hunk)
  (:keymaps 'diff-hl-mode-map
            :states '(normal visual)
            "[g" #'diff-hl-previous-hunk
            "]g" #'diff-hl-next-hunk)
  )

(use-package browse-at-remote :ensure t
  :commands (browse-at-remote browse-at-remote-get-url)
  :custom (browse-at-remote-prefer-symbolic nil) ;; use hash instead of branch name
  )

(use-package recentf
  :commands (recentf-mode recentf-open-files)

  :init
  (defun start-recentf-mode-function()
    (recentf-mode)
    (remove-function after-focus-change-function 'start-recentf-mode-function))
  (add-function :after after-focus-change-function #'start-recentf-mode-function)

  :custom
  (recentf-save-file "~/.cache/emacs/recentf")
  (recentf-max-saved-items 2000)
  (recentf-exclude
   '("/.?TAGS"
     "/.recentf"
     "/\\.emacs\\.d/\\.cask/"
     "/\\.emacs\\.d/games/*-scores"
     "COMMIT_EDITMSG"
     "^/sudo:"))

  :config
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
  (run-with-idle-timer 30 t #'(lambda ()
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
  :custom
  (undohist-ignored-files '("\\.git/COMMIT_EDITMSG$"))
  :config (undohist-initialize))

(use-package pangu-spacing :ensure t
  :hook
  ((text-mode-hook) . pangu-spacing-mode)

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

(use-package lsp-mode :ensure t
  :hook ((c++-mode
          js-mode
          terraform-mode
          ;; sh-mode
          typescript-mode
          ) . lsp)

  :custom
  (lsp-auto-guess-root t)
  (lsp-clients-go-server "gopls")
  (lsp-clients-javascript-typescript-server "typescript-language-server")
  (lsp-enable-file-watchers nil)  ;; file watcher is too slow
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

  :general
  (:keymaps 'lsp-mode-map
            :states '(normal visual)
            "<localleader>R" #'lsp-rename)
  (:keymaps 'lsp-mode-map
            :states 'normal
            "<localleader>f" #'lsp-format-buffer)
  (:keymaps 'lsp-mode-map
            :states 'visual
            "<localleader>f" #'lsp-format-region)
  )

(use-package lsp-vetur
  :hook (vue . lsp)
  :custom
  (lsp-vetur-format-default-formatter-ts "eslint")
  (lsp-vetur-format-default-formatter-js "eslint")
  )

(use-package lsp-pyright :ensure t
  :hook
  (python-mode
   . (lambda ()
       (require 'lsp-pyright)
       (setq-local lsp-pyright-python-executable-cmd (my/find-venv-python-or-global))
       (evil-local-set-key 'normal (kbd "<localleader>f") #'python-black-buffer)
       (evil-local-set-key 'visual (kbd "<localleader>f") #'python-black-region)
       (lsp)))
  )

(use-package lsp-go
  :hook (go-mode . lsp)
  :custom (lsp-go-use-gofumpt t)
  )

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
        ([remap xref-find-references] . #'lsp-ui-peek-find-references)
        ("<localleader>m" . #'lsp-ui-imenu))
  (:map lsp-ui-peek-mode-map
        ("j" . lsp-ui-peek--select-next)
        ("k" . lsp-ui-peek--select-prev)))

(use-package corfu :ensure t
  :after evil
  :custom
  (corfu-cycle t)
  ;; (corfu-separator ?\s)
  (corfu-preselect-first nil)
  (lsp-completion-provider :none)  ; for corfu
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-SPC" . corfu-insert-separator)
        )
  :init
  (global-corfu-mode)
  :config
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  )

(use-package corfu-terminal :ensure t
  :if (not window-system)
  :config
  (corfu-terminal-mode))

(use-package cape :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

(use-package kind-icon :ensure t
  :after corfu
  :config
   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package company :ensure t :disabled
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

(use-package company-statistics :ensure t
  :hook (company-mode . company-statistics-mode)
  :custom
  (company-statistics-file "~/.cache/emacs/company-statistics-cache.el")
  (company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  )

(use-package company-restclient :ensure t
  :config
  (add-to-list 'company-backends #'company-restclient)
  )

(use-package quickrun :ensure t
  :commands quickrun

  :custom
  (quickrun-focus-p nil)
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

  (quickrun-add-command "python"
    '((:command . my/find-venv-python-or-global)
      (:compile-only . "flake8 %s")
      )
    :override t)

  (quickrun-add-command "python/pytest"
    '((:command . (lambda () (my/find-venv-executable "pytest")))
      (:exec    . ("%c %s"))))

  (quickrun-set-default "typescript" "typescript/deno")

  (quickrun-add-command "typescript/deno-test"
    '((:command . "deno")
      (:exec . "%c test -A %s")
      (:compile-only . "%c compile %s")
      (:compile-conf . ((:compilation-mode . nil) (:mode . js-mode)))
      (:remove  . ("%n.js"))
      (:description . "Test TypeScript script with deno")))

  (defadvice quickrun (before quickrun-before-save ())
    "Save buffer before quickrun."
    (save-buffer))
  (defadvice quickrun-region (before quickrun-region-before-save ())
    "Save buffer before quickrun-region."
    (save-buffer))
  (ad-activate 'quickrun)
  (ad-activate 'quickrun-region)

  :general
  (:keymaps 'prog-mode-map
            :states 'normal
            "<localleader>r" #'quickrun)
  (:keymaps 'prog-mode-map
            :states 'visual
            (kbd "<localleader>r") #'quickrun-region)
  )

(use-package annotate :ensure t)

(use-package csharp-mode
  :if (string< emacs-version "29")
  :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package ansible :ensure t
  :mode "\\/roles/[^/]+/[^/]+/[^/]+\\.yaml\\'")

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

(use-package clang-format :ensure t
  :commands clang-format-buffer)

(use-package go-mode :ensure t)

(use-package go-eldoc :ensure t
  :hook (go-mode . go-eldoc-setup)
)

(use-package protobuf-mode :ensure t)

(use-package js
  :hook
  (js-mode
   . (lambda ()
       (evil-define-key '(normal visual) 'local
         (kbd "<localleader>f") #'prettier-js)))
  :custom (js-indent-level 2)
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

(use-package tree-sitter-langs :ensure t
  :after tree-sitter)

(use-package add-node-modules-path :ensure t
  :hook ((js-mode
          typescript-mode
          web-mode
          vue-mode)
         . add-node-modules-path)
  )

(use-package eslint-fix :ensure t
  :commands eslint-fix
  )

(use-package prettier-js :ensure t
  :commands prettier-js)

(use-package json-mode :ensure t)

(use-package markdown-mode :ensure t
  :commands markdown-toggle-markup-hiding

  :hook
  ((markdown-mode gfm-mode)
   . (lambda () ()
       (setq-local indent-tabs-mode nil)
       (setq-local tab-width 4)))

  :custom
  (markdown-command "pandoc -s -t html5 -c ~/.emacs.d/css/github.css")
  (markdown-gfm-use-electric-backquote nil)

  :custom-face
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.4 :bold t :underline t))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2 :bold t :underline t))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1 :bold t :underline t))))

  :mode
  ("README\\.md\\'" . gfm-mode)

  :general
  (:keymaps '(markdown-mode-map gfm-mode-map)
            :states '(normal visual)
            "zo" #'(lambda () (interactive) (outline-show-children) (outline-show-entry))
            "zc" #'outline-hide-subtree
            "TAB" #'markdown-cycle
            "<localleader>f" #'prettier-js)

  :config
  (defun my/markdown-convert-backlog-wiki-link-region (begin end)
    (interactive "r")
    (save-excursion
      (replace-regexp "\\[\\[[A-Z_]+] \\(.*\\) | .* | .*\\](.*)" "[[\\1]]" nil begin end)))
  (defun my/markdown-convert-backlog-wiki-link ()
    (interactive)
    (my/markdown-convert-backlog-wiki-link-region (point-min) (point-max)))
  (defun my/markdown-convert-backlog-ticket-link-region (begin end)
    (interactive "r")
    (save-excursion
      (replace-regexp "\\[\\([A-Z_]+-[0-9]+\\)\\](.*)" "[[\\1]]" nil begin end)))
  (defun my/markdown-convert-backlog-ticket-link ()
    (interactive)
    (my/markdown-convert-backlog-ticket-link-region (point-min) (point-max)))
  (defun my/markdown-convert-todo-status-region (begin end)
    (interactive "r")
    (save-excursion
      (replace-regexp "# TODO" "# `未対応`" nil begin end)
      (replace-regexp "# STARTED" "# `処理中`" nil begin end)
      (replace-regexp "# WAITING" "# `待機`" nil begin end)
      (replace-regexp "# DONE ?\\(.*\\)" "# ~~`完了` \\1~~" nil begin end)
      ))
  (defun my/markdown-convert-todo-status ()
    (interactive)
    (my/markdown-convert-todo-status-region (point-min) (point-max))
    )
  (defun my/markdown-convert-backlog-img-link-region (begin end)
    (interactive "r")
    (save-excursion
      (replace-regexp "!\\[img\\](.*/+\\(.*\\))" "![img][\\1]" nil begin end)))
  (defun my/markdown-convert-backlog-img-link ()
    (interactive)
    (my/img-embed-convert-for-backlog-style-region (point-min) (point-max)))
  (defun my/markdown-convert-backlog-all ()
    (interactive)
    (my/remove-all-backslashes)
    (my/markdown-convert-todo-status)
    (my/markdown-convert-backlog-ticket-link)
    (my/markdown-convert-backlog-img-link)
    (my/markdown-convert-backlog-wiki-link))
  )

(use-package terraform-mode :ensure t)

(use-package prog-mode
  :custom
  (prettify-symbols-unprettify-at-point t)
  :general
  (:keymaps 'prog-mode-map
            :states '(normal visual)
            "gd" #'xref-find-definitions
            "gr" #'xref-find-references)
  )

(use-package edit-indirect :ensure t
  :commands edit-indirect-region
  )

(defvar my/plantuml-java-options "-Djava.awt.headless=true") ; plantuml-modeのdefaultになったけどob-plantumlで使う
(defvar my/plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar")) ; ob-plantumlで使う
(defvar my/plantuml-jar-args (list "-charset" "UTF-8" "-config" (expand-file-name "~/.config/plantuml/color.uml"))) ; ob-plantumlで使う

(use-package org :ensure org-contrib
  :diminish org-indent-mode

  :init
  (defun my/org-mode-update-time-stamp-date ()
    (let ((time-stamp-start "DATE:")
          (time-stamp-end "$")
          (time-stamp-format " %Y-%02m-%02d"))
      (time-stamp)))

  (defun my/org-mode-update-time-stamp-modified ()
    (let ((time-stamp-start ":MODIFIED:[ \t]+\\\\?[\"<]+"))
      (time-stamp)))

  (defun my/setup-org-mode-local-hooks ()
    (require 'time-stamp)
    (add-hook 'before-save-hook #'my/org-mode-update-time-stamp-date nil t)
    (add-hook 'before-save-hook #'my/org-mode-update-time-stamp-modified nil t)
    )

  (defun my/org-mode-set-prop-timestamp (prop &optional inactive)
    "Set timestamp PROP if it does not exists."
    (let* ((exists? (org-entry-get (point) prop))
           (str (if inactive
                    (format-time-string "[%Y-%m-%d %T]")
                  (format-time-string "<%Y-%m-%d %T>"))))
      (unless exists?
        (org-set-property prop str))))

  (defun my/org-mode-insert-time-stamp-created-heading ()
    (save-excursion
      (org-back-to-heading)
      (my/org-mode-set-prop-timestamp "CREATED")))

  (defun my/org-mode-insert-time-stamp-file ()
    (when (and (eq major-mode 'org-mode)
               (string= (buffer-string) ""))
      (save-excursion
        (point-min)
        (my/org-mode-set-prop-timestamp "CREATED")
        (my/org-mode-set-prop-timestamp "MODIFIED"))))

  (add-hook 'find-file-hook #'my/org-mode-insert-time-stamp-file)

  :hook
  (org-after-todo-statistics . my/org-summary-todo)
  (org-after-todo-state-change . my/org-add-date-for-book-state)
  (org-checkbox-statistics . my/org-checkbox-todo)
  (org-mode . org-indent-mode)
  (org-mode . prettify-symbols-mode)
  (org-mode
   . (lambda ()
       (setq prettify-symbols-alist
             '(
               ;; ("#+DOWNLOADED:" . "📥")
               ("#+RESULTS" . "▷")
               ("#+RESULTS:" . "▷")
               ("#+begin_src" . "⌟")
               ("#+end_src" . "⌝")
               ;; (":END:" . "⎺")
               ;; (":LOGBOOK:" . "📚")
               ;; (":PROPERTIES:" . "⚙")
               ;; ("CLOCK:" . "⏰")
               ;; ("CLOSED:" . "✅")
               ;; ("DEADLINE:" . "⛔️")
               ;; ("SCHEDULED:" . "📅")
               ("[ ]" . "☐")
               ("[-]" . "〼")
               ("[X]" . "☑")
               ))))
  (org-mode . my/setup-org-mode-local-hooks)
  (org-insert-heading . my/org-mode-insert-time-stamp-created-heading)

  :custom
  (org-directory "~/org/")
  (org-archive-location (f-join org-directory "archived" (format-time-string "%Y/%m") "%s::"))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-startup-folded t)
  (org-startup-with-inline-images nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively nil)
  (org-hide-leading-stars t) ; 見出しの余分な*を消す
  (org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
     (sequence "NEXT(n)" "STARTED" "|")
     (sequence "WAITING(w)" "STARTED" "|")
     (sequence "SOMEDAY(S)" "|")
     (sequence "|" "CANCELLED(c)")
     (sequence "|" "MEETING(m)")))
  (org-log-done 'time) ; DONEの時刻を記録
  (org-log-into-drawer "LOGBOOK")
  (org-image-actual-width nil)  ; to use #+ATTR_ORG: :width or fixed width

  :after evil
  :config
  (defun my/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (org-todo (if (= n-not-done 0) "DONE" "TODO")))

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

  (defun my/org-add-date-for-book-state ()
    "Set date string when book state changed."
    (when (string-equal org-state "BOUGHT")
      (org-entry-put (point) "bought_at" (format-time-string "[%Y-%m-%d]")))
    (when (string-equal org-state "READ")
      (org-entry-put (point) "read_at" (format-time-string "[%Y-%m-%d]")))
    )

  (defun my/org-todo-next ()
    "Org todo next cycle"
    (interactive) (org-call-with-arg 'org-todo 'right)
    )

  (evil-define-key '(normal visual) org-mode-map
    ;; leader mapping
    (kbd "<leader>/") 'consult-outline

    ;; localleader mapping
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
    )

  (use-package org-tempo
    :after org)

  (use-package org-faces
    :after org

    :custom
    (org-todo-keyword-faces
     '(("TODO"      :foreground "red"          :weight bold)
       ("NEXT"      :foreground "green"          :weight bold)
       ("STARTED"   :foreground "orange"       :weight bold)
       ("DOING"     :foreground "orange"       :weight bold)
       ("WAITING"   :foreground "light pink"   :weight bold)
       ("SOMEDAY"   :foreground "dark gray")
       ("DONE"      :foreground "forest green" :weight bold)
       ("CANCELLED" :foreground "forest green" :weight bold)
       ("AGENDA"    :foreground "sky blue"     :weight bold)
       ("MEETING"   :foreground "sky blue"     :weight bold)))
    ;; Only use the first 4 styles and do not cycle.
    (org-cycle-level-faces nil)
    (org-n-level-faces 4)

    :custom-face
    ;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
    (org-level-1 ((t (:inherit 'outline-1 :height 1.2)))) ;\LARGE
    (org-level-2 ((t (:inherit 'outline-2 :height 1.1)))) ;\Large
    (org-level-3 ((t (:inherit 'outline-3 :height 1.05)))) ;\large
    ;; Document Title, (\huge)
    (org-document-title ((t (:height 2.074 :inherit 'org-level-8))))

    (org-block            ((t (:height 0.9    :background "gray5"))))
    (org-code             ((t (:height 0.8    :background "gray8"))))
    (org-block-begin-line ((t (:height 0.7    :foreground "gray40" :background nil :underline "gray20"))))
    (org-block-end-line   ((t (:underline nil :overline "gray20"))))
    (org-date             ((t (:height 0.7    :foreground "gold4"))))
    (org-drawer           ((t (:height 0.7    :foreground "gray40"))))
    (org-meta-line        ((t (:height 0.7    :foreground "gray40"))))
    (org-property-value   ((t (:height 0.7))))
    (org-sexp-date        ((t (:height 0.7    :foreground "gray40"))))
    (org-table            ((t (:height 0.9))))
    )

  (use-package org-clock
    :hook
    ;; (kill-emacs . my:org-clock-out-and-save-when-exit) ;; use persistent clock instead
    (org-after-todo-state-change . my:org-clock-in-if-starting)
    (org-after-todo-state-change . my:org-clock-out-if-waiting)
    (org-mode . org-clock-persistence-insinuate)

    :custom
    (org-clock-clocked-in-display 'frame-title)
    (org-clock-idle-time 5)
    (org-clock-out-remove-zero-time-clocks t)
    (org-clock-persist t)

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
                 (not (string= org-last-state org-state))
                 (not (org-clocking-p)))
        (org-clock-in)))

    (defun my:org-clock-out-if-waiting ()
      "Clock out when the task is not marked STARTED."
      (when (and
             (and (not (string= org-state "STARTED"))
                  (string= (org-clock--mode-line-heading) org-clock-heading))
             (not (string= org-last-state org-state)))
        (org-clock-out)))
    )

  (use-package org-collector)

  (use-package org-habit)

  (use-package org-roam :ensure t
    :after evil
    :demand t  ; completion-at-point on plain org-mode
    :hook
    (org-mode
     . (lambda ()
         (org-roam--register-completion-functions-h)
         (org-roam--replace-roam-links-on-save-h)
         ))
    :custom
    (org-roam-completion-everywhere t)
    (org-roam-db-update-on-save t)
    (org-roam-directory (f-join org-directory "roam"))
    (org-roam-node-display-template "${title:*} ${tags:10}")
    (org-roam-capture-templates
     '(("d" "default" plain "%?"
        :target (file+head "nodes/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+DATE:")
        :unnarrowed t)
       ))
    (org-roam-db-node-include-function
     (lambda ()
       (not (member "ATTACH" (org-get-tags)))))
    ;; (org-roam-dailies-capture-templates
    ;;  `(("d" "default" entry
    ;;     "* %?"
    ;;     :target (file+head "%<%Y-%m-%d>.org"
    ;;                        "#+SETUPFILE: ../../tags.org\n#+OPTIONS: ^:{} H:5 toc:nil\n#+title: %<%Y-%m-%d>\n"))))
    ;; :bind-keymap
    ;; ("C-c n d" . org-roam-dailies-map)
    :bind
    ("C-c n f" . org-roam-node-find)
    (:map org-mode-map
          ("C-c n a" . org-roam-alias-add)
          ("C-c n b" . org-roam-buffer-display-dedicated)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n l" . org-roam-buffer-toggle)
          ("C-c n o" . org-id-get-create)
          ("C-c n t" . org-roam-tag-add)
          ("C-M-i" . completion-at-point))
    :general
    (:states '(normal visual)
             ;; "<leader> n" #'org-roam-dailies-map
             "<leader> nS" #'org-roam-db-sync
             "<leader> nb" #'org-roam-buffer-display-dedicated
             "<leader> n/" #'org-roam-node-find
             "<leader> nF" #'my/org-roam-fix-exported-markdown)
    :config
    ;; (require 'org-roam-dailies)

    (defun my/unlink-all-markdown-file-links-region (begin end)
      "MarkdownFile link to normal text from BEGIN to END"
      (interactive "r")
      (save-excursion
        (replace-regexp "\\[\\(.*\\)\\](.*.md)" "\\1" nil begin end)))

    (defun my/unlink-all-markdown-file-links ()
      "MarkdownFile link to normal text"
      (interactive)
      (my/unlink-all-markdown-file-links-region (point-min) (point-max)))

    (defun my/remove-all-backslashes ()
      (interactive)
      (my/remove-all-backslashes-region (point-min) (point-max)))

    (defun my/remove-all-backslashes-region (begin end)
      "Remove all backslashes from BEGIN to END"
      (interactive "r")
      (save-excursion
        (replace-regexp "\\\\" "" nil begin end)))

    (defun my/org-roam-fix-exported-markdown ()
      (interactive)
      (my/unlink-all-markdown-file-links)
      (my/remove-all-backslashes))

    ;; add org-daily files to org-agenda-files
    ;; (mapcar
    ;;  #'(lambda (i)
    ;;      (let
    ;;          ((filename
    ;;            (concat
    ;;             (file-name-as-directory org-roam-directory)
    ;;             (file-name-as-directory "daily")
    ;;             (format-time-string
    ;;              "%Y-%m-%d.org"
    ;;              (time-add (current-time) (* -1 i 60 60 24))))))
    ;;        (when (file-readable-p filename)
    ;;          (add-to-list 'org-agenda-files filename))))
    ;;  (number-sequence 0 13)  ; two weeks
    ;;  )
    )

  (use-package org-capture
    :commands org-capture
    :hook
    (org-capture-mode . (lambda () (evil-insert 0)))
    (org-capture-prepare-finalize . my/org-mode-insert-time-stamp-created-heading)

    :custom
    (org-capture-templates
     `(
       ("d" "Diary" entry (file+olp+datetree my/org-capture-file-this-year) "** %(format-time-string \"%H:%M\") - %?
:LOGBOOK:
- created at %U
:END:
")
       ("b" "Book" entry (file+headline "books.org" "Books") "** WISH %(my/book-templeate-from-url \"%c\")%?")
       ("t" "Task" entry (file+headline "todos.org" "Todos") "** TODO %?
:LOGBOOK:
- created at %U
:END:
")
       ("T" "Task (Interrupt)" entry (file+headline "todos.org" "Todos") "** DONE %?\nCLOSED: %U" :clock-in t :clock-resume t)
       ("c" "Item (Clocking)" item (clock) "%U %?")
       ("C" "Entry (Clocking)" entry (clock) "* %U %?")
       ("m" "Meeting Note" entry (file+headline "meetings.org" "Meetings") "** NEXT %?
:LOGBOOK:
- created at %U
:END:
")
       ("r" "Review" entry (file+headline "todos.org" "Todos") "\
** %(with-current-buffer (org-capture-get :original-buffer) (my/get-local-git-repo))\
 [[file:%F::%(with-current-buffer (org-capture-get :original-buffer) (format \"%s\" (line-number-at-pos)))][%f]]\
 on %(with-current-buffer (org-capture-get :original-buffer) (format \"%s\" (magit-get-current-branch)))
%K

%(with-current-buffer (org-capture-get :original-buffer) (browse-at-remote-get-url))

#+begin_src %(my/get-major-mode \"%F\")
%(org-escape-code-in-string \"%i\")
#+end_src

%?
")
       ))

    :config
    (defun my/org-capture-file-this-year ()
      (f-join
       org-directory
       "diary"
       (format-time-string "%Y.org")))

    (defun my/org-capture-file-today ()
      (f-join
       org-directory
       "diary"
       (format-time-string "%Y-%m-%d.org")))

     (defun my/get-major-mode (f)
       "get major-mode for F"
       (with-current-buffer (find-buffer-visiting f)
         (replace-regexp-in-string "-mode" "" (format "%s" major-mode))))

     (defun my/get-local-git-repo ()
       "returns github.com/repo/name"
       (let ((repo (magit-repository-local-repository)))
         (string-match "\\([^\\/]+\\/[^\\/]+\\/[^\\/]+\\)\\/$" repo)
         (match-string 1 repo)))

     (defun my/book-templeate-from-url (url)
       (unless (executable-find "bookinfo")
         (error "Requires \"bookinfo\" command"))
       (let ((info
              (ignore-errors
                (json-read-from-string
                 (shell-command-to-string
                  (s-join " "
                          (mapcar
                           #'shell-quote-argument
                           `("bookinfo" ,url))))))))
         (unless info
           (setq info `((url . ,url)(name . "%?"))))
         (format
          "%s
:PROPERTIES:
:price: %s
:name: %s
:author: %s
:isbn:
:added_at: %s
:bought_at:
:read_at:
:store_name: %s
:store_url: %s
:ebook: %s
:ebook_url:
:END:
"
          (cdr (assq 'name info))
          (cdr (assq 'price info))
          (cdr (assq 'name info))
          (cdr (assq 'author info))
          (format-time-string "[%Y-%m-%d]")
          (cdr (assq 'store info))
          (cdr (assq 'url info))
          (cdr (assq 'ebook info)))
         ))
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

  (use-package org-attach
    :custom
    (org-attach-directory (f-join org-directory "data"))

    :config
    (use-package org-attach-git)
    )

  (use-package org-download :ensure t
    :commands
    (org-download-clipboard
     org-download-delete
     org-download-screenshot)

    :custom
    (org-download-method 'attach)
    (org-download-screenshot-method "screencapture -i %s")

    :init
    (defvar org-download-map (make-sparse-keymap))
    (define-prefix-command 'org-download-map)
    (define-key org-download-map "c" #'org-download-clipboard)
    (define-key org-download-map "d" #'org-download-delete)
    (define-key org-download-map "s" #'org-download-screenshot)

    :general
    (:keymaps 'org-mode-map
              :states '(normal insert)
              "C-c d" #'org-download-map)
    (:states 'normal
             "<localleader>d" #'org-download-map)
    )

  (use-package org-mac-link
    :if darwin-p
    :straight (org-mac-link :host gitlab :repo "aimebertrand/org-mac-link")
    :commands org-mac-link-get-link
    :bind
    (:map org-mode-map
          ("C-c g" . org-mac-link-get-link)
          ))

  (use-package org-ai
    :straight
    (:host github :repo "rksm/org-ai" :files ("*.el" "README.md" "snippets"))
    :commands (org-ai-mode)
    :init
    (add-hook 'org-mode-hook #'org-ai-mode)
    :custom
    (org-ai-default-max-tokens 1024)
    ;; (org-ai-openai-api-token "openai-access-token")
    :config
    ;; if you are using yasnippet and want `ai` snippets
    (org-ai-install-yasnippets))

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

  (use-package org-web-tools :ensure t)

  (use-package org-appear :ensure t
    :hook
    (org-mode . org-appear-mode)
    (org-mode
     . (lambda ()
         (add-hook 'evil-insert-state-entry-hook
                   #'org-appear-manual-start
                   nil
                   t)
         (add-hook 'evil-insert-state-exit-hook
                   #'org-appear-manual-stop
                   nil
                   t)))
    :custom
    (org-appear-autolinks t)
    (org-appear-trigger 'manual)
    (org-hide-emphasis-markers t)
    )

  (use-package ob
    :after org
    :hook
    (org-babel-after-execute . org-display-inline-images)
    :custom
    (org-confirm-babel-evaluate nil)
    (org-babel-C++-compiler "g++ -Wall -Wextra -std=c++14")
    (org-babel-default-header-args
     (append org-babel-default-header-args '((:exports . "both") (:eval . "no-export"))))
    :config
    ;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
    ;; (defun my/org-confirm-babel-evaluate (lang body)
    ;;   (not (member lang
    ;;                '(
    ;;                  "C"
    ;;                  "cpp"
    ;;                  "dot"
    ;;                  "elisp"
    ;;                  "go"
    ;;                  "js"
    ;;                  "plantuml"
    ;;                  "python"
    ;;                  "restclient"
    ;;                  "sh"
    ;;                  "shell"
    ;;                  "ts"
    ;;                  "typescript"
    ;;                  "uml"
    ;;                  ))))

    ;; https://github.com/astahlman/ob-async/issues/61
    ;; for ob-async
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (chatgpt . t)
       (emacs-lisp . t)
       (mermaid . t)
       (plantuml . t)
       (python . t)
       (shell . t)
       )
     )

    (use-package ob-restclient :ensure t)

    (use-package ob-exp
      :custom
      (org-export-use-babel t))

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
      (push '(:cache . "yes") org-babel-default-header-args:plantuml)
      )

    (use-package ob-mermaid :ensure t
      :if darwin-p
      :after ob
      :custom
      (ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")
      (org-babel-default-header-args:mermaid
       (append org-babel-default-header-args:mermaid '((:cache . "yes") (:async . t))))
      :config
      (setq mermaid-config-file "~/.config/mermaid/config.json"))

    (use-package ob-python
      :after ob
      :hook
      (org-mode
       . (lambda ()
           (setq-local org-babel-python-command (my/find-venv-python-or-global))))
      :custom
      (org-babel-python-command "python3")
      (org-babel-default-header-args:python '((:cache . "yes") (:results . "output")))
      )

    (use-package ob-C
      :after ob
      :custom
      (org-babel-default-header-args:C '((:cache . "yes")))
      (org-babel-default-header-args:C++
       (append org-babel-default-header-args:C '((:includes . "<iostream>"))))
      )

    (use-package ob-async :ensure t
      :after ob
      :config
      ;; https://github.com/astahlman/ob-async/issues/75
      (defun no-hide-overlays (orig-fun &rest args)
        (setq org-babel-hide-result-overlays nil))
      (advice-add 'ob-async-org-babel-execute-src-block :before #'no-hide-overlays)
      )

    (use-package ob-js
      :config
      (setq org-babel-js-function-wrapper
            "require('util').inspect(function(){\n%s\n}());")
      )

    (use-package ob-typescript :ensure t
      :config
      (push '("ts" . typescript) org-src-lang-modes)
      (org-babel-make-language-alias "ts" "typescript")
      )

    (use-package ob-go :ensure t)

    (use-package ob-chatgpt
      ;; :straight (:host github :repo "yhiraki/ob-chatgpt" :files ("*.el")))
      :load-path "~/src/github.com/yhiraki/ob-chatgpt"
      :custom
      (org-babel-default-header-args:chatgpt
       (append org-babel-default-header-args:chatgpt
               '((:async . "yes") (:to-org . t) (:wrap . "quote"))))
      (org-babel-chatgpt-aliases '("ask"))
      )
    )

  (use-package ox
    :commands org-export-dispatch

    :custom
    (org-export-headline-levels 5)
    (org-export-use-babel t)
    (org-export-with-broken-links t)
    (org-export-with-section-numbers nil)
    (org-export-with-sub-superscripts '{})
    (org-export-with-toc nil)

    :config
    (use-package ox-gfm :ensure t)

    (use-package ox-hugo :ensure t)

    (use-package ox-html
      :custom
      (org-html-head-include-scripts nil)
      (org-html-head-include-default-style nil)
      (org-html-head "\
<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/tokyo-night-dark.min.css\" id=\"hljs-css\">
<link rel=\"stylesheet\" href=\"https://yhiraki.github.io/memo/static/css/main.css\">
")
      (org-html-postamble t)
      (org-html-postamble-format
       '(("en" "\
<p class=\"date\">Date: %d</p>
<p class=\"author\">Author: %a</p>
<link rel=\"stylesheet\" href=\"static/css/main.css\" >\
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js\"></script>
<script src=\"https://yhiraki.github.io/memo/static/js/main.js\"></script>
")))
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

    (use-package org-re-reveal :ensure t
      :custom
      (org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
      (org-re-reveal-revealjs-version "4")
      (org-re-reveal-theme "white")
      (org-re-reveal-transition "none")
      (org-re-reveal-hlevel 5)
      )

    (use-package ox-rst :ensure t)
    )
  )

(use-package org-agenda
  :commands (org-agenda org-refile)
  :hook (find-file . my/add-org-roam-to-agenda)
  :demand t

  :init
  (defvar my/org-sub-todo-progress-regexp "\\[\\([0-9]+/[0-9]+\\|[0-9]+%\\)\\]")
  (defvar my/org-agenda-entry-is-project
    '(org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp my/org-sub-todo-progress-regexp)))
  (defvar my/org-agenda-entry-is-not-project
    '(org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp my/org-sub-todo-progress-regexp)))

  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-current-time-string "← now")
  (org-agenda-time-grid ;; Format is changed from 9.1
   '((daily today require-timed)
     (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
     "-"
     "────────────────"))
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (org-agenda-files `(,org-directory))
  (org-agenda-span 'day)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t :tags t :hidefiles t))
  (org-agenda-custom-commands
   `(("t" "Tasks"
      ((agenda "" ((org-agenda-entry-types '(:deadline :scheduled))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   ))
       (todo "STARTED" (,my/org-agenda-entry-is-not-project
                        (org-agenda-overriding-header "On progress: ")))
       (todo "NEXT" (,my/org-agenda-entry-is-not-project
                     (org-agenda-overriding-header "Next Actions: ")))
       (todo "WAITING" (,my/org-agenda-entry-is-not-project
                        (org-agenda-overriding-header "Waitings: ")))
       (todo 'not-done (,my/org-agenda-entry-is-project
                        (org-agenda-overriding-header "Projects: ")))
       ))
     ("r" "GTD review"
      ((tags-todo "TAGS=\"\"+LEVEL=2"
                  ((org-agenda-files '("todos.org"))
                   (org-agenda-overriding-header "Untagged TODOs")))
       (todo "TODO" ((org-agenda-files '("todos.org"))
                     ,my/org-agenda-entry-is-not-project
                     (org-agenda-overriding-header "Todos: ")))
       (todo 'not-done (,my/org-agenda-entry-is-project
                        (org-agenda-overriding-header "Projects: ")))
       (todo "SOMEDAY" (,my/org-agenda-entry-is-not-project
                        (org-agenda-overriding-header "Someday: ")))
       ))
     ("A" "Done tasks to be archived"
      ((tags "CLOSED<=\"<-1w>\"+LEVEL=2"
             ((org-agenda-files '("todos.org"))))))
     ("b" "Books"
      ((tags "+LEVEL=2"
             ((org-agenda-files '("books.org"))))))
     ))

  :config
  (defun my/org-agenda-todo-next ()
    "Org agenda todo next cycle"
    (interactive) (org-call-with-arg 'org-agenda-todo 'right)
    )

  (defun my/add-org-roam-to-agenda ()
    (let ((orgdir (file-truename (expand-file-name org-directory))))
      ;; ~org/ 直下のファイル以外。直下のファイルはすでに追加されているため無視する
      (when
          (string-match (concat orgdir "roam/.*/.*\\.org$") buffer-file-name)
        (add-to-list 'org-agenda-files (string-replace orgdir org-directory buffer-file-name)))))

  :bind
  (:map org-agenda-mode-map
        (":" . evil-ex)
        ("G" . evil-goto-line)
        ("H" . org-agenda-do-date-earlier)
        ("K" . org-capture)
        ("L" . org-agenda-do-date-later)
        ("T" . org-agenda-todo)
        ("g" . nil)
        ("gg" . evil-goto-first-line)
        ("j" . org-agenda-next-item)
        ("k" . org-agenda-previous-item)
        ("t" . my/org-agenda-todo-next)
        ("C-b" . evil-scroll-page-up)
        ("C-f" . evil-scroll-page-down)
        )
  )

(use-package electric
  :hook (python-mode . electric-indent-mode)
  )

(use-package cc-mode
  :mode (("\\.cpp\\'" . c++-mode))
  :config
  (setq-default sp-escape-quotes-after-insert nil)
  )

(use-package python :ensure t
  :defer t

  :hook
  (python-mode
   . (lambda ()
       (when (and buffer-file-name
                  (string-match "test_.*\\.py$" buffer-file-name))
         (setq-local quickrun-option-cmdkey "python/pytest"))))

  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-m IPython --simple-prompt -i")

  :config
  (defun my/find-venv-dir ()
    (my/find-up-directory ".venv" "."))

  (defun my/find-venv-executable (cmd)
    (let ((venv (my/find-up-directory ".venv" ".")))
      (if venv
          (format "%s/bin/%s" venv cmd)
        cmd)))

  (defun my/find-venv-python-or-global ()
    (my/find-venv-executable "python3"))

  (use-package py-yapf :ensure t
    :commands (py-yapf-buffer)
    )

  (use-package python-black :ensure t
    :custom (python-black-extra-args '("--skip-string-normalization"))
    :commands (pyhton-black-buffer pyhton-black-region)
    )

  (use-package py-isort :ensure t
    :general
    (:keymaps 'python-mode-map
              :states 'normal
              "<localleader>i" #'py-isort-buffer)
    (:keymaps 'python-mode-map
              :states 'visual
              "<localleader>i" #'py-isort-region)
    )
  )

(use-package sh-script
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+)

  :config
  (use-package shfmt :ensure t
    :general
    (:keymaps 'sh-mode-map
              :states 'normal
              "<localleader>f" #'shfmt-buffer)
    (:keymaps 'sh-mode-map
              :states 'visual
              "<localleader>f" #'shfmt-region)
    :custom
    (shfmt-arguments '("-i" "2" "-ci"))
    )
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
            my/sql-indentation-offsets-alist)))
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
  (defvar my/sql-indentation-offsets-alist
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
  :mode
  ("\\Pipfile\\'"))

(use-package typescript-mode :ensure t
  :hook
  (typescript-mode
   . (lambda ()
       (if (my/find-up-directory "package.json" (buffer-file-name))
           (progn
             (setq-local lsp-enabled-clients '(ts-ls))
             (evil-define-key '(normal visual) 'local
               (kbd "<localleader>f") #'prettier-js))
         (progn
           (when (string-match "_test\\.ts$" (buffer-file-name))
             (setq-local quickrun-option-cmdkey "typescript/deno-test"))
           (setq-local lsp-enabled-clients '(deno-ls)))
         )))
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

(use-package vue-mode :ensure t
  :hook
  (vue-mode
   . (lambda ()
       (setq syntax-ppss-table nil)
       (add-hook 'after-save-hook 'mmm-parse-buffer nil t)
       ))
  (vue-mode . lsp)

  :general
  (:keymaps 'vue-mode-map
            :states '(normal visual)
            "<localleader>f" #'eslint-fix)
  )

(use-package mmm-mode
  :diminish
  :custom
  (mmm-submode-decoration-level 0)
  :commands mmm-mode
  )

(use-package yaml-mode :ensure t
  :commands yaml-mode)

(use-package vimrc-mode :ensure t
  :commands vimrc-mode)

(use-package emmet-mode :ensure t
  :diminish
  :hook (sgml-mode css-mode web-mode xml-mode js-jsx-mode typescript-mode)
  :custom
  (emmet-indent-after-insert nil)
)

(use-package smartrep :ensure t
  :after evil
  :config
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
        ("C-n" . 'completion-at-point)
        ("C-p" . 'completion-at-point)
        )

  :custom
  (evil-ex-search-vim-style-regexp t)
  (evil-toggle-key "C-M-z")
  (evil-undo-system 'undo-redo)
  (evil-want-C-i-jump t)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-keybinding nil)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  (modify-syntax-entry ?_ "w" (standard-syntax-table))
  (evil-declare-change-repeat 'company-complete)
  (evil-add-command-properties #'find-file :jump t)
  (evil-add-command-properties #'xref-find-definitions :jump t)
  (evil-add-command-properties #'xref-find-references :jump t)

  (evil-set-leader '(normal visual emacs) (kbd "SPC"))
  (evil-set-leader '(normal visual) (kbd "\\") t) ; localleader

  (defun my/find-user-emacs-init-file ()
    (interactive)
    (find-file (locate-user-emacs-file "init.el")))

  (defun my/find-org-todos ()
    (interactive)
    (find-file (f-join org-directory "todos.org")))

  (evil-define-key nil 'global
    (kbd "<leader>/") 'imenu
    (kbd "<leader>.") 'my/find-user-emacs-init-file
    (kbd "<leader>G g") 'google-this
    (kbd "<leader>a") 'org-agenda
    (kbd "<leader>b") 'bookmark-jump
    (kbd "<leader>c") 'org-capture
    (kbd "<leader>f b") 'switch-to-buffer
    (kbd "<leader>f d") 'dired-sidebar-toggle-sidebar
    (kbd "<leader>f f") 'find-file
    (kbd "<leader>f g") 'grep-find
    (kbd "<leader>f j") 'open-junk-file
    (kbd "<leader>f o") 'my/open-current-dir
    (kbd "<leader>f r") 'recentf-open-files
    (kbd "<leader>g c") 'magit-commit
    (kbd "<leader>g f") 'consult-ls-git
    (kbd "<leader>g g") 'vc-git-grep
    (kbd "<leader>g lb") 'gist-buffer
    (kbd "<leader>g ll") 'gist-list
    (kbd "<leader>g m") 'git-messenger:popup-message
    (kbd "<leader>g o") 'browse-at-remote
    (kbd "<leader>g p") 'consult-ghq-find
    (kbd "<leader>g s") 'magit-stage
    (kbd "<leader>g t") 'git-timemachine
    (kbd "<leader>g u") 'magit-unstage
    (kbd "<leader>h") 'help
    (kbd "<leader>o I") 'org-clock-in
    (kbd "<leader>o L") 'org-clock-in-last
    (kbd "<leader>o O") 'org-clock-out
    (kbd "<leader>o Q") 'org-clock-cancel
    (kbd "<leader>o a") 'org-agenda
    (kbd "<leader>o b") 'org-switchb
    (kbd "<leader>o c") 'org-capture
    (kbd "<leader>o i") 'org-id-get-create
    (kbd "<leader>o j") 'org-clock-goto
    (kbd "<leader>o l") 'org-store-link
    (kbd "<leader>o s") 'org-save-all-org-buffers
    (kbd "<leader>s") 'magit-status
    (kbd "<leader>t") 'my/find-org-todos
    (kbd "<leader>z e") 'eval-buffer
    (kbd "<leader>z k") 'save-buffers-kill-emacs
    (kbd "<leader>z r") 'restart-emacs
    )

  (evil-mode)

  (use-package evil-collection :ensure t
    :after evil
    :diminish evil-collection-unimpaired-mode
    :config

    (defun my/evil-collection-dired-setup-extra ()
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" 'dired-subtree-remove
        "l" 'dired-subtree-insert
        "r" 'revert-buffer
        (kbd "C-c C-o") 'my/dired-open-in-external-app
        (kbd "C-j") 'dired-next-dirline
        (kbd "C-k") 'dired-prev-dirline
        (kbd "SPC") 'nil  ; for evil leader key
        )
      (advice-remove 'evil-collection-dired-setup 'my/evil-collection-dired-setup-extra))
    (advice-add 'evil-collection-dired-setup :after 'my/evil-collection-dired-setup-extra)

    (defun my/evil-collection-org-setup-extra ()
      (evil-collection-define-key '(normal visual) 'org-mode-map
        (kbd "C-j") 'org-next-visible-heading
        (kbd "C-k") 'org-previous-visible-heading
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
        (kbd "C-<") 'org-shiftmetaleft
        (kbd "C->") 'org-shiftmetaright
        (kbd "gh") 'outline-up-heading
        (kbd "gp") 'outline-previous-heading
        )
      (evil-collection-define-key '(normal insert visual) 'org-mode-map
        (kbd "M-S-h") 'org-metashiftleft
        (kbd "M-S-j") 'org-metashiftdown
        (kbd "M-S-k") 'org-metashiftup
        (kbd "M-S-l") 'org-metashiftright
        (kbd "M-h") 'org-metaleft
        (kbd "M-j") 'org-metadown
        (kbd "M-k") 'org-metaup
        (kbd "M-l") 'org-metaright
        )
      (advice-remove 'evil-collection-org-setup 'my/evil-collection-org-setup-extra))
    (advice-add 'evil-collection-org-setup :after 'my/evil-collection-org-setup-extra)

    (setq evil-collection-mode-list (delete 'vterm evil-collection-mode-list))
    (evil-set-initial-state 'vterm-mode 'emacs)

    (evil-collection-init)
    )

  (use-package evil-surround :ensure t
    :after evil
    :config (global-evil-surround-mode))

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
    :general
    (:states '(normal visual)
             "C-a" #'evil-numbers/inc-at-pt
             "C-x" #'evil-numbers/dec-at-pt
             "g C-a" #'evil-numbers/inc-at-pt-incremental
             "g C-x" #'evil-numbers/dec-at-pt-incremental)
    )

  (use-package evil-goggles :ensure t
    :after evil
    :diminish
    :custom
    (evil-goggles-duration 0.050)
    :config
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode))
  )

(use-package window
  :custom
  (display-buffer-alist
   '(
     ;; Sidebar bottom
     ("\\*\\(quickrun\\|Org-Babel Error Output\\|Backtrace\\|xref\\)\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (reusable-frames)
      (side . bottom)
      (window-height . 10))
     ("\\*Org Select\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (reusable-frames)
      (side . bottom))
     ("\\*vterm\\*"
      (display-buffer-in-side-window))

     ;; Org agenda at top
     ("\\*Agenda Commands\\*"
      (display-buffer-reuse-window display-buffer-in-direction)
      (reusable-frames)
      (direction . top))
     ("\\*Org Agenda\\*"
      (display-buffer-reuse-window display-buffer-in-direction)
      (reusable-frames)
      (direction . top)
      (window-height . 0.5))

     ;; at bottom
     ("magit: "
      (display-buffer-reuse-window display-buffer-in-direction)
      (reusable-frames)
      (direction . bottom)
      (window-height . 0.5))
     ("CAPTURE-.*"
      (display-buffer-reuse-window display-buffer-in-direction)
      (reusable-frames)
      (direction . bottom)
      (window-height . 0.5))
     ))
  )

(use-package all-the-icons :ensure t)

(use-package color-theme-sanityinc-tomorrow :ensure t :disabled)

(use-package doom-themes :ensure t :disabled)

(use-package doom-modeline :ensure t :disabled)

(use-package modus-themes :ensure t
  :config
  (defun my/load-theme ()
    (load-theme 'modus-vivendi t))
  (my/load-theme)
  :custom
  (modus-themes-paren-match '(bold underline))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mode-line '(borderless))
  )

(use-package mini-modeline :ensure t :disabled t
  :diminish mini-modeline-mode
  :custom
  (mini-modeline-face-attr `(:background nil))
  :custom-face
  (mini-modeline-mode-line
   ((t (:background ,(face-attribute 'window-divider :foreground) :height 0.14 :box nil)))))

(use-package nano-modeline :ensure t :disabled)

(use-package mood-line :ensure t
  :config
  (mood-line-mode))

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

(use-package clipetty :ensure t
  :diminish clipetty-mode
  :config (global-clipetty-mode))

(use-package whitespace
  :diminish (whitespace-mode global-whitespace-mode)
  :hook (after-init . global-whitespace-mode)

  :custom
  ;; http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
  (whitespace-style '(face           ; faceで可視化
                      trailing       ; 行末
                      tabs           ; タブ
                      spaces         ; スペース
                      empty          ; 先頭/末尾の空行
                      space-mark
                      tab-mark
                      ))
  (whitespace-display-mappings
   '((space-mark ?\u3000 [?\□])
     ;; WARNING: the mapping below has a problem.
     ;; When a TAB occupies exactly one column, it will display the
     ;; character ?\xBB at that column followed by a TAB which goes to
     ;; the next TAB column.
     ;; If this is a problem for you, please, comment the line below.
     (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-space-regexp "\\(\u3000+\\)") ; スペースは全角のみを可視化
  ;; (whitespace-action '(auto-cleanup)) ; 保存時に自動でクリーンアップ
  (whitespace-global-modes
   '(not eww-mode
         term-mode
         vterm-mode
         eshell-mode
         org-agenda-mode
         magit-mode
         calendar-mode))

  :custom-face
  (whitespace-empty    ((t (:background nil :foreground "gray12" :strike-through t :inherit nil))))
  (whitespace-space    ((t (:background nil :foreground "GreenYellow" :inherit nil))))
  (whitespace-tab      ((t (:background nil :inherit nil))))
  (whitespace-trailing ((t (:background nil :underline (:color "DeepPink" :style wave) :inherit nil))))

  :config
  (set-display-table-slot standard-display-table 'truncation ?<) ; set lcs=extends:<,precedes:<
  (setcar (nthcdr 2 (assq 'space-mark whitespace-display-mappings)) [?_]) ; set nbsp:%

  (defun my/whitespace-zenkaku-to-hankaku-buffer ()
    "Japanese zenkaku space to hankaku"
    (interactive)
    (my/whitespace-zenkaku-to-hankaku-region (point-min) (point-max)))

  (defun my/whitespace-zenkaku-to-hankaku-region (begin end)
    "Japanese zenkaku space to hankaku"
    (interactive "r")
    (save-excursion
      (replace-string "\u3000" " " nil begin end)))

  (defun my/whitespace-zerowidth-cleanup-region (begin end)
    "remove zero space width"
    (interactive "r")
    (save-excursion
      (replace-string "\u200B" " " nil begin end)))

  (defun my/whitespace-cleanup ()
    "Whitespace cleanup inclues zenkaku space"
    (interactive)
    (my/whitespace-zenkaku-to-hankaku-buffer)
    (my/whitespace-zerowidth-cleanup-region (point-min) (point-max))
    (whitespace-cleanup)
    )

  (defun my/whitespace-cleanup-region (begin end)
    "Whitespace cleanup inclues zenkaku space"
    (interactive "r")
    (my/whitespace-zenkaku-to-hankaku-region begin end)
    (whitespace-cleanup-region)
    )

  :general
  (:states 'normal
           "<localleader>w" #'my/whitespace-cleanup)
  (:states 'visual
           "<localleader>w" #'my/whitespace-cleanup-region)
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
  (oj-default-online-judge 'atcoder)
  :config
  (defun my/oj-submit ()
    "Submit code."
    (interactive)
    (let ((alist (quickrun--command-info
                  (quickrun--command-key (buffer-file-name)))))
      (oj--exec-script (format "cd %s" default-directory))
      (oj--exec-script
       (concat
        (format "oj submit %s" (buffer-file-name))
        (when oj-submit-args
          (format " %s" (mapconcat #'identity oj-submit-args " ")))
        (when (or (string= "clang" oj-compiler-c)
                  (string-match "clang" (format "%s" (alist-get :command alist))))
          " --guess-cxx-compiler clang")
        (when (or (string= "pypy" oj-compiler-python)
                  (string-match "pypy" (format "%s" (alist-get :command alist))))
          " --guess-python-interpreter pypy")
        ))))
  (advice-add 'oj-submit :override #'my/oj-submit)
  )

(use-package emacs-lock
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  )

(use-package ol
  :bind ("C-c l" . org-store-link))

(use-package grep
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'grep-mode 'normal)))

(use-package wgrep :ensure t)

(use-package consult :ensure t
  :bind
  ([remap find-file] . #'consult-find)
  ([remap imenu] . #'consult-imenu)
  ([remap bookmark-jump] . #'consult-bookmark)
  ([remap recentf-open-files] . #'consult-recent-file)
  ([remap grep-find] .#'consult-ripgrep)
  ([remap vc-git-grep] .#'consult-git-grep)

  :custom
  (consult-project-root-function #'vc-root-dir)
  (consult-find-args "find . -not ( -name .venv -prune -o -name node_modules -prune -o -name .git -prune )")

  :general
  (:states '(normal visual)
           "g/" 'consult-line)
  )

(use-package affe :ensure t
  :custom
  (affe-find-command "rg --color=never --files --hidden -g !.git")
  )

(use-package consult-ghq :ensure t
  :commands (consult-ghq-find consult-ghq-grep)
  )

(use-package consult-ls-git :ensure t
  :commands consult-ls-git
  )

(use-package vertico :ensure t
  :hook (after-init . vertico-mode)
  :custom (vertico-count 20))

(setq enable-recursive-minibuffers t)

(use-package minibuffer
  :init
  (setq completion-category-defaults nil)
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles partial-completion))))
  )

(use-package orderless :ensure t)

(use-package marginalia :ensure t
  :hook (after-init . marginalia-mode))

(use-package embark-consult :ensure t
  :bind
  ("C-o" . 'embark-act))

(use-package key-binding :no-require
  :config
  (global-set-key (kbd "C-\\") nil)
  (global-set-key (kbd "s-t") nil)
  ;; (keyboard-translate ?\C-h ?\C-?)
  (global-set-key (kbd "C-h") 'delete-backward-char) ; use this for using terminal
  )

(use-package mozc :ensure t
  :custom
  (default-input-method "japanese-mozc"))

(use-package mozc-temp :ensure t
  :bind
  ("M-n" . mozc-temp-convert))

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
  (defvar my/user-emacs-file-candidate (locate-user-emacs-file "init.candidate.el"))
  (load my/user-emacs-file-candidate t)
  )

(use-package init-local :no-require
  :config
  (defvar my/user-emacs-file-local (locate-user-emacs-file "init.local.el"))
  (load my/user-emacs-file-local t)
  )


(provide 'init)
;;; init.el ends here
