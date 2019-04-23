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

(use-package file-open :no-require
  :config
  (setq vc-follow-symlinks t) ; シンボリックリンクの読み込みを許可
  (setq find-file-visit-truename t) ; 実体を開く
  (setq auto-revert-check-vc-info t) ; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
  (setq large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
  (setq tags-revert-without-query 1) ; TAGS ファイルを自動で再読込
  )

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  ;; :config (setq display-line-numbers-type 'relative)
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

(use-package files
  :config
  (setq save-silently t)
  )

(use-package open-junk-file :ensure t
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

(use-package volatile-highlights :ensure t
  :hook ((prog-mode org-mode) . volatile-highlights-mode)
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  )

(use-package highlight-indent-guides :ensure t
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive t)
  )

(use-package eldoc
  :hook ((emacs-lisp-mode-hook lisp-mode-hook) . eldoc-mode)
  )

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package which-key :ensure t
  :hook (after-init . which-key-mode)
  )

(use-package smartparens :ensure t
  :hook (after-init . smartparens-global-mode)
  )

(use-package restart-emacs :ensure t
  :commands restart-emacs
  )

(use-package elscreen :ensure t
  :hook (after-init . elscreen-start)
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
  )

;; (use-package emojify :ensure t
;;   :hook (after-init . global-emojify-mode)
;;   )

(use-package dired-sidebar :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'icons)
  )

(use-package all-the-icons-dired :ensure t
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  )

(use-package projectile :ensure t
  :commands projectile-mode
  )

(use-package wdired :ensure t
  :commands (wdired-change-to-wdired-mode)
  :bind
  (:map dired-mode-map
        ("e" . wdired-change-to-wdired-mode))
  :config
  (setq wdired-allow-to-change-permissions t)
  )

(use-package flymake
  :commands flymake-mode
  :config
  (set-face-underline 'flymake-error nil)
  (set-face-underline 'flymake-note nil)
  (set-face-underline 'flymake-warning nil)
  :custom
  (flymake-error-bitmap nil)
  (flymake-note-bitmap nil)
  (flymake-warning-bitmap nil)
  )

(use-package flycheck :ensure t
  :hook ((js2-mode python-mode web-mode plantuml-mode c++-mode) . flycheck-mode)

  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode)

  (flycheck-define-checker c/c++-g++
    "A C/C++ checker using g++."
    :command ("g++" "-Wall" "-Wextra" source)
    :error-patterns  ((error line-start
                             (file-name) ":" line ":" column ":" " error: " (message)
                             line-end)
                      (warning line-start
                               (file-name) ":" line ":" column ":" " warning: " (message)
                               line-end))
    :modes (c-mode c++-mode))

  (push 'c/c++-g++ flycheck-checkers)
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
  :config
  ;; magit-commit 時に diff が開くのをやめる
  ;; https://qiita.com/egg_chicken/items/948f8df70069334e8296
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  )

(use-package git-gutter-fringe+ :ensure t
  :hook (after-init . global-git-gutter+-mode)
  )

(use-package recentf
  :commands recentf-mode
  :config
  (setq recentf-save-file "~/.cache/emacs/recentf")
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '("/.recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
  (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
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
  :config
  (setq undohist-ignored-files '("COMMIT_EDITMSG"))
  )

(use-package ivy :ensure t
  :hook (after-init . ivy-mode)
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
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

(use-package counsel-ghq :init (el-get-bundle windymelt/counsel-ghq)
  :commands (counsel-ghq)
  )

;; (use-package path :no-require
;;   :config
;;   (defun set-exec-path-from-shell-PATH ()
;;     "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

;; This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
;;     (interactive)
;;     (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;       (setenv "PATH" path-from-shell)
;;       (setq exec-path (split-string path-from-shell path-separator))))

;;   (set-exec-path-from-shell-PATH)
;;   )

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

(use-package twittering-mode :ensure t
  :commands (twit)
  :config
  ;; master-password を設定する際に注意すること
  ;; https://blog.web-apps.tech/emacs-mac-twittering-mode-every-asked-pin/
  (setq twittering-use-master-password t))

(use-package eglot :ensure t
  :commands eglot-ensure
  :hook ((python-mode go-mode sh-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(go-mode . ("go-langserver" "-mode=stdio" "-gocodecompletion" "-func-snippet-enabled=false"))
               )
  )

;; (use-package lsp-mode :ensure t
;;   :hook ((python-mode go-mode sh-mode) . lsp)
;;   :config
;;   (setq lsp-enable-snippet nil)
;;   )

(use-package company :ensure t
  :hook (after-init . global-company-mode)
  :config
  ;; https://github.com/expez/company-quickhelp/issues/63
  (add-hook
   'company-completion-started-hook
   '(lambda (&rest ignore)
     (when evil-mode
       (when (evil-insert-state-p)
         (define-key evil-insert-state-map (kbd "C-n") nil)
         (define-key evil-insert-state-map (kbd "C-p") nil)
         ))))
  (setq company-auto-complete nil)
  (setq company-candidates-cache t)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-etags-ignore-case t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
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

;; (use-package company-box :ensure t
;;   :hook (company-mode . company-box-mode)
;;   ;; ~/.emacs.d/elpa//company-box-*/images
;;   ;; $ mogrify -resize 50% *.png
;;   )

(use-package company-lsp :ensure t
  :after (company yasnippet)
  :commands company-lsp
  :config
  ;; https://github.com/tigersoldier/company-lsp/issues/103
  (add-to-list 'company-lsp-filter-candidates '(gopls . nil))
  (push 'company-lsp company-backends)
  )

(use-package company-statistics :ensure t
  :hook (company-mode . company-statistics-mode)
  :config
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  )

(use-package quickrun :ensure t
  :commands quickrun
  :config
  (setq quickrun-timeout-seconds 30)

  (quickrun-add-command "rust/script"
    '((:command . "cargo")
      (:exec    . ("%c script %o %s")))
    :default "rust")

  (quickrun-add-command "c++/g++"
    '((:exec . ("%c -x c++ -std=c++17 --pedantic-errors %o -o %e %s" "%e %a")))
    :override t)
  (quickrun-set-default "c++" "c++/g++")
  )

(use-package csharp-mode :ensure t
  :mode
  ("\\.cs\\'" . csharp-mode)
  )

(use-package dockerfile-mode :ensure t
  :mode
  ("Dockerfile\\'" . dockerfile-mode))

(use-package hideshow
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

(use-package go-mode :ensure t
  :mode ("\\.go\\'" . go-mode)
)

(use-package go-eldoc :ensure t
  :hook (go-mode . go-eldoc-setup)
)

(use-package js2-mode :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  :config
  (setq js2-basic-offset 2)
  (setq js2-highlight-external-variables nil)
  (setq js2-include-browser-externs nil)
  (setq js2-include-jslint-globals nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  :mode (("\\.js\\'" . js2-mode))
  )

(use-package js2-refactor :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil)
  )

(use-package xref-js2 :ensure t
  :init
  (add-hook 'js2-mode-hook
            '(lambda ()
               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package company-tern :ensure t
  :commands company-tern
  :config
  (push 'company-tern company-backends)
  )

(use-package add-node-modules-path :ensure t
  :hook (js-mode js2-mode typescript-mode web-mode)
  )

(use-package eslint-fix :ensure t
  :commands eslint-fix
  )

;; (use-package prettierjs :no-require
;;   :config
;;   (defun my/prettier ()
;;     (interactive)
;;     (shell-command
;;      (format "%s --write %s"
;;              (shell-quote-argument (executable-find "prettier"))
;;              (shell-quote-argument (expand-file-name buffer-file-name))))
;;     (revert-buffer t t t))
;;   )

(use-package json-mode :ensure t
  :mode (("\\.json\\'" . json-mode))
  )

(use-package markdown-mode :ensure t
  :config
  (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.emacs.d/css/github.css")
  (setq markdown-gfm-use-electric-backquote nil)
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  )

(use-package edit-indirect :ensure t
  :commands edit-indirect-region
  )

(use-package org :ensure org-plus-contrib
  :init
  (add-hook 'org-mode-hook
            '(lambda()
               (setq company-minimum-prefix-length 1)
               (push 'company-capf company-backends)
               (add-hook 'completion-at-point-functions
                         'pcomplete-completions-at-point nil t)
               ))
  :config
  (setq org-directory "~/org/")
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-startup-with-inline-images nil)
  (setq org-src-fontify-natively t)
  (setq org-plantuml-jar-path "~/lib/java/plantuml.jar")
  (setq org-default-notes-file "notes.org")
  (setq org-hide-leading-stars t) ; 見出しの余分な*を消す
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s@!)" "WAIT(w@/!)" "|" "DONE(d@!)" "CANCEL(c@/!)")))
  (setq org-log-done 'time) ; DONEの時刻を記録
  (setq org-html-htmlize-output-type 'css)
  (setq org-publish-directory "~/public_html/")

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
  :mode (("\\.org\\'" . org-mode))
  )

(use-package org-agenda
  :commands (org-agenda org-refile)
  :config
  (setq org-agenda-files '("~/org/" "~/org/kb/"))
  (setq org-agenda-current-time-string "← now")
  (setq org-agenda-time-grid ;; Format is changed from 9.1
        '((daily today require-timed)
          (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
          "-"
          "────────────────"))
  )

(use-package ob
  :config
  ;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("python" "shell" "plantuml" "shell" "dot" "js" "C" "C++"))))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; https://github.com/skuro/plantuml-mode
  (push '("plantuml" . plantuml) org-src-lang-modes)
  (push '("js" . js2) org-src-lang-modes)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (plantuml . t) (shell . t) (dot . t) (js . t) (C . t))
   )

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
  )

(use-package ob-plantuml
  :after (ob plantuml-mode)
  :config
  (push (cons ':java plantuml-java-options) org-babel-default-header-args:plantuml)
  (push (cons ':cmdline plantuml-options) org-babel-default-header-args:plantuml)
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

;; (use-package ob-ipython :ensure t
;;   :after ob)

(use-package org-capture
  :commands org-capture
  :config
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
  (setq org-capture-templates
        '(("t" "Task\t\t- TODOs" entry (file+headline "~/org/task.org" "Todos") "** TODO %?%i\n  %a")
          ("m" "Memo\t\t- Text miscs" entry (file+headline "~/org/memo.org" "Memo") "** %?\n %U")
          ("n" "Note\t\t- Notes" entry (file+headline "~/org/notes.org" "Notes") "** %? %a\n %T\n")
          ("r" "Reading\t- Web surfing" entry (file+olp+datetree "~/org/reading.org") "* %?\n  %c\n  %T")
          ("j" "Journal\t- Short logs like Twitter" entry (file+olp+datetree "~/org/journal.org") "* %?\n  %c\n  Entered on %U")
          ;; https://ox-hugo.scripter.co/doc/org-capture-setup
          ("b" "Blog\t\t- Hugo post" entry (file+olp "~/org/blog.org" "Blog Ideas")
           (function org-hugo-new-subtree-post-capture-template))
          )
        )
  )

(use-package org-bullets :ensure t
  :hook (org-mode . org-bullets-mode))

;; (use-package org-reveal :ensure t :disabled t
;;   :init
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;;   )

(use-package ox-confluence
  :after ox
  )

(use-package ox-gfm :ensure t
  :after ox
  )

(use-package ox-hugo :ensure t
  :after ox
  )

(use-package ox-rst :ensure t
  :after ox
  )

(use-package electric
  :hook (python-mode . electric-indent-mode)
  )

(use-package cc-mode
  :mode (("\\.cpp\\'" . c++-mode))
  )

(use-package python :ensure t
  :mode (("\\.py\\'" . python-mode))
  )

(use-package py-yapf :ensure t
  :commands (py-yapf-buffer)
  )

(use-package py-isort :ensure t
  :commands (py-isort-buffer py-isort-region)
  )

(use-package sh-script
  :config
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+)
  :mode
  ("\\.?sh\\'" . shell-script-mode)
  )

(use-package sql
  :config
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  (setq sql-postgres-login-params (append sql-postgres-login-params '(port)))
  (setq sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq tab-width 2)
  :mode
  ("\\.sql\\'" . sql-mode)
  )

(use-package sql-indent :ensure t
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
  :mode ("\\.ts\\'")
  )

(use-package tide :ensure t
  :hook (typescript-mode . tide-setup)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq tide-completion-ignore-case t)
  )

(use-package plantuml-mode :ensure t
  :init
  (setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar"))
  (setq plantuml-java-options "-Djava.awt.headless=true")
  (setq plantuml-options "-charset UTF-8 -config ~/.config/plantuml/color.uml")
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

(use-package web-mode :ensure t
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

(use-package emmet-mode :ensure t
  :hook (sgml-mode css-mode web-mode xml-mode)
)

(use-package gitignore-mode :ensure t
  :mode
  ("\\(\\.git\\|docker\\)ignore\\'" . gitignore-mode)
  )

(use-package evil :ensure t
  :hook (org-capture-mode . evil-insert-state)
  :bind
  (:map evil-normal-state-map
        ( "C-l" . 'evil-ex-nohighlight)
        ( "C-j" . 'evil-forward-paragraph)
        ( "C-k" . 'evil-backward-paragraph)
        ( "S-C-j" . 'evil-forward-section-begin)
        ( "S-C-k" . 'evil-backward-section-begin)
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
  (evil-declare-change-repeat 'company-complete)
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
    (kbd "gg") 'evil-goto-first-line
    (kbd "G") 'evil-goto-line
    )
  ;; (evil-define-key 'normal prog-mode-map
  ;;   (kbd "\\f") 'lsp-format-region
  ;;   )
  (evil-define-key 'normal prog-mode-map
    (kbd "K") 'eglot-help-at-point
    ;; (kbd "K") 'lsp-describe-thing-at-point
    (kbd "\\f") 'eglot-format
    ;; (kbd "\\f") 'lsp-format-buffer
    (kbd "\\r") '(lambda () (interactive) (save-buffer) (quickrun))
    (kbd "gd") 'xref-find-definitions
    (kbd "gr") 'xref-find-references
    )
  (evil-define-key 'normal quickrun--mode-map
    (kbd "q") 'evil-window-delete
    )
  (evil-define-key 'normal python-mode-map
    (kbd "\\i") 'py-isort-buffer
    (kbd "\\f") 'py-yapf-buffer
    )
  (evil-define-key 'visual python-mode-map
    (kbd "\\i") 'py-isort-region
    )
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
    (kbd "M-k") 'org-metaup
    (kbd "M-j") 'org-metadown
    (kbd "<M-return>") '(lambda () (interactive) (evil-append-line 1) (org-meta-return))
    (kbd "<C-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-heading-after-current))
    (kbd "<M-S-return>") '(lambda () (interactive) (evil-append-line 1) (org-insert-todo-heading 1))
    (kbd "<C-S-return>") '(lambda () (interactive) (evil-insert-state) (org-insert-todo-heading-respect-content))
    (kbd "t") 'org-todo
    (kbd "<") 'org-metaleft
    (kbd ">") 'org-metaright
    (kbd "\\!") 'org-time-stamp-inactive
    (kbd "\\*") 'org-ctrl-c-star
    (kbd "\\.") 'org-time-stamp
    (kbd "\\d") 'org-deadline
    (kbd "\\i") 'org-clock-in
    (kbd "\\p") 'org-priority
    (kbd "\\q") 'org-set-tags-command
    (kbd "\\s") 'org-schedule
    (kbd "\\t") 'org-todo
    (kbd "\\x") 'org-toggle-checkbox
    (kbd "\\v") 'org-toggle-inline-images
    (kbd "gh") 'outline-up-heading
    (kbd "gp") 'outline-previous-heading
    ;; (kbd "}") (if (fboundp 'org-forward-same-level) 'org-forward-same-level 'org-forward-heading-same-level)
    ;; (kbd "{") (if (fboundp 'org-backward-same-level) 'org-backward-same-level 'org-backward-heading-same-level)
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
    (kbd "\\R") 'web-mode-element-rename
    (kbd "\\f") 'eslint-fix
    (kbd "zc") 'web-mode-fold-or-unfold
    (kbd "zo") 'web-mode-fold-or-unfold
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
  (evil-define-key 'normal prog-mode-map
    (kbd "[e") 'flycheck-previous-error
    (kbd "]e") 'flycheck-next-error)
  ;; https://gist.github.com/amirrajan/301e74dc844a4c9ffc3830dc4268f177
  (evil-set-initial-state 'org-agenda-mode 'normal)
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

(use-package evil-leader :ensure t
  :hook
  ;; Note: You should enable global-evil-leader-mode before you enable evil-mode
  (after-init . (lambda() (global-evil-leader-mode) (evil-mode 1)))
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    (kbd "ag") 'counsel-ag
    (kbd "bc") 'elscreen-create
    (kbd "bk") 'elscreen-kill
    (kbd "bn") 'elscreen-next
    (kbd "bp") 'elscreen-previous
    (kbd "c") 'org-capture
    (kbd "df") 'counsel-describe-function
    (kbd "dk") 'counsel-descbinds
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
    (kbd "k") 'kill-this-buffer
    (kbd "oa") 'org-agenda
    (kbd "ob") 'org-switchb
    (kbd "oc") 'org-capture
    (kbd "oi") 'org-clock-in
    (kbd "ol") 'org-store-link
    (kbd "oo") 'org-clock-out
    (kbd "th") 'twit
    (kbd "tm") 'twittering-mentions-timeline
    (kbd "tu") 'twittering-update-status-interactive
    (kbd "u") 'undo-tree-visualize
    (kbd "x") 'counsel-M-x
    (kbd "ze") 'eval-buffer
    (kbd "zi") 'find-user-init-file
    (kbd "zk") 'kill-emacs
    (kbd "zr") 'restart-emacs
    )
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

;; (use-package evil-collection :ensure t
;;   :after evil
;;   :config
;;   (evil-collection-init 'neotree)
;;   )

(use-package popwin :ensure t
  :hook (after-init . popwin-mode)
  :config
  (push '("*quickrun*" :regexp t :position bottom :dedicated t) popwin:special-display-config)
  (push '("*Help*" :position right :width 0.5) popwin:special-display-config)
  (push '("*Error*") popwin:special-display-config)
  (push '("magit:*" :regexp t :position bottom :height 0.5) popwin:special-display-config)
  (push '("*xref*" :position bottom ) popwin:special-display-config)
  (push '(image-mode) popwin:special-display-config)
  (push '("*Org-Babel Error Output*") popwin:special-display-config)
  )

(use-package smartrep :ensure t
  :config
  (smartrep-define-key global-map
      "C-c" '(("+" . 'evil-numbers/inc-at-pt)
              ("=" . 'evil-numbers/inc-at-pt)
              ("-" . 'evil-numbers/dec-at-pt)
              ))
  )

(use-package all-the-icons :ensure t)

(use-package doom-themes :ensure t
  :hook (after-make-frame-functions .
         (lambda (frame)
           (load-theme 'doom-one t)
           ))
  :config
  (load-theme 'doom-one t)
  ;; (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

(use-package doom-modeline :ensure t
  :hook (after-init . doom-modeline-mode)
  )

(use-package whitespace
  :commands whitespace-mode
  :config
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
  )

(use-package yasnippet :ensure t
  :hook (after-init . yas-global-mode)
  :init (add-hook 'yasnippet-mode-hook
                 '(lambda () (setq require-final-newline nil)))
  :config
  (setq yas-snippet-dirs (list (locate-user-emacs-file "snippets")))
  (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
  :bind
  (:map yas-keymap
        ("<tab>" . nil)
        ("RET" . yas-next-field-or-maybe-expand))
  )

;; (use-package yasnippet-snippets :ensure t)

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

;; (use-package yatemplate :ensure t
;;   :after yasnippet
;;   :config
;;   ;; http://emacs.rubikitch.com/sd1602-autoinsert-yatemplate-yasnippet/
;;   (yatemplate-fill-alist)
;;   (auto-insert-mode 1)
;;   )

(use-package key-binding :no-require
  :bind
  ("C-h" . "")
  ("<C-s-268632070>" . toggle-frame-fullscreen)
  ("C-\\" . nil)
  )

(use-package custom-file :no-require
  :custom
  (custom-file (my/file-path-join user-emacs-directory "custom.el"))
  :config
  (load custom-file)
  )
