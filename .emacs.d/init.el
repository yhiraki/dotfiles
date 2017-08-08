;; init.el --- Emacs configurations

(require 'cl)

;;;;;;;;;;;;
;; el-get ;;
;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'package)
(add-to-list 'package-archives
             '(("gnu" . "http://elpa.gnu.org/packages/")
               ("melpa" . "http://melpa.org/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages/")))

(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
; (el-get 'sync)

;; packages
(require 'el-get)
(el-get-bundle evil)
(el-get-bundle evil-surround)
(el-get-bundle evil-numbers)
(el-get-bundle evil-leader)
(el-get-bundle evil-nerd-commenter)
(el-get-bundle evil-matchit)
; (el-get-bundle evil-extra-operator)
(el-get-bundle evil-exchange)
(el-get-bundle evil-magit)
(el-get-bundle evil-args)
(el-get-bundle tarao-evil-plugins
  :type github :pkgname "tarao/evil-plugins")
(el-get-bundle tarao-elisps
  :type github :pkgname "tarao/elisp")
(el-get-bundle epc)
(el-get-bundle flx)
(el-get-bundle jedi-core)
(el-get-bundle color-moccur)
(el-get-bundle company-statistics)
(el-get-bundle company-flx)
(el-get-bundle company-racer)
(el-get-bundle company-jedi :depends (company-mode))
(el-get-bundle exec-path-from-shell)
(el-get-bundle git-gutter-fringe)
(el-get-bundle helm)
(el-get-bundle helm-fuzzier)
(el-get-bundle helm-flx)
(el-get-bundle helm-ag)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-ghq)
(el-get-bundle helm-git-grep)
(el-get-bundle helm-gtags)
(el-get-bundle flycheck)
(el-get-bundle flycheck-rust)
(el-get-bundle popup)
(el-get-bundle popup-pos-tip)
(el-get-bundle smartparens)
(el-get-bundle emmet-mode)
(el-get-bundle volatile-highlights)
(el-get-bundle yasnippet)
(el-get-bundle pyenv-mode)
(el-get-bundle which-key)
(el-get-bundle quickrun)
(el-get-bundle material-theme)
(el-get-bundle key-combo
  :type github :pkgname "uk-ar/key-combo")
(el-get-bundle init-loader)
(el-get-bundle open-junk-file)
(el-get-bundle neotree)
(el-get-bundle evil-commentary)
(el-get-bundle all-the-icons
  :type github :pkgname "domtronn/all-the-icons.el")
(el-get-bundle popwin)
(el-get-bundle magit)
(el-get-bundle git-modes)
(el-get-bundle open-junk-file)
(el-get-bundle markdown-mode)
(el-get-bundle web-mode)
(el-get-bundle restart-emacs)
(el-get-bundle rainbow-delimiters)
(el-get-bundle undohist)
(el-get-bundle pyenv-mode)
(el-get-bundle projectile)
(el-get-bundle elscreen)
(el-get-bundle plantuml-mode)
(el-get-bundle org-confluence
  :type github :pkgname "hgschmie/org-confluence")
(el-get-bundle ox-gfm)
(el-get-bundle rust-mode)
(el-get-bundle emacs-racer)
(el-get-bundle ob-rust
  :type github :pkgname  "micanzhang/ob-rust")
(el-get-bundle toml-mode)


;;;;;;;;;;;;;;;;;
;; basic setup ;;
;;;;;;;;;;;;;;;;;

;; visibility
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-linum-mode t)
(global-hl-line-mode t)
(show-paren-mode 1) ;; 対応する括弧を光らせる
(scroll-bar-mode 0)
(setq indicate-buffer-boundaries 'left) ;; バッファの終端を表示
(set-frame-font "ricty-13")

;; scroll
(setq scroll-conservatively 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(fset 'yes-or-no-p 'y-or-n-p)

;; editor
(setq-default tab-width 4 indent-tabs-mode nil) ; タブにスペースを使用
(setq require-final-newline t)
(modify-syntax-entry ?_ "w" (standard-syntax-table))


;;;;;;;;;;;
;; files ;;
;;;;;;;;;;;

;; エコーエリアや *Messages* バッファにメッセージを表示させたくない
;; http://qiita.com/itiut@github/items/d917eafd6ab255629346
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; recent
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '("/.recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
(setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
(run-with-idle-timer 30 t '(lambda ()
                             (with-suppressed-message (recentf-save-list))))
(recentf-mode 1)

;; backup
;; https://www.emacswiki.org/emacs/BackupDirectory#toc3
(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.backups/emacs/"
                         (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

; history
(require 'undohist)
(undohist-initialize)
(setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG"))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;;;;;;;;;
;; ime ;;
;;;;;;;;;

;; http://blog.ichiroc.in/entry/2013/09/06/075832

;; Google日本語入力をベースにする
;; これがないと(mac-toggle-input-method t) で、ことえりが有効になってしまう。
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
(add-hook 'evil-normal-state-entry-hook
          '(lambda ()
             (mac-toggle-input-method nil)))
(add-hook 'evil-normal-state-entry-hook 'mac-change-language-to-us)

;; ミニバッファを開いたときに英字にする（閉じてもモードは戻らない）
(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)


;;;;;;;;;;;;;;;;
;; whitespace ;;
;;;;;;;;;;;;;;;;

; http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a

(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
 ;;; racerやrustfmt、コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
;;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             ;;; この辺の設定はお好みで
                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))                        ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; set lcs=extends:<,precedes:<
(set-display-table-slot standard-display-table 'truncation ?<)

;; set nbsp:%
(setcar (nthcdr 2 (assq 'space-mark whitespace-display-mappings)) [?%])

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
; (setq whitespace-action '(auto-cleanup))

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)


;;;;;;;;;;;;;;;;;;
;; global modes ;;
;;;;;;;;;;;;;;;;;;

(defun my-global-mode-init-hooks ()
  (projectile-mode)
  (smartparens-global-mode t)
  (global-whitespace-mode 1)
  (global-company-mode)
  )

(add-hook 'after-init-hook 'my-global-mode-init-hooks)


;;;;;;;;;;;;;
;; company ;;
;;;;;;;;;;;;;

;; http://qiita.com/sune2/items/b73037f9e85962f5afb7

(defun company-mode-hooks ()
  (company-flx-mode +1)

  ;; vars
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)

  ;; mappings
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; 候補のソート順
  (add-hook 'after-init-hook 'company-statistics-mode)
  (require 'company-statistics)
  (company-statistics-mode)
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  )

(add-hook 'company-mode-hook 'company-mode-hooks)


;;;;;;;;;;;;
;; python ;;
;;;;;;;;;;;;

;; python
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  (rainbow-delimiters-mode)
  (pyenv-mode)
  (electric-indent-mode +1))
(add-hook 'python-mode-hook 'my/python-mode-hook)

(defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
    (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)


;;;;;;;;;;
;; rust ;;
;;;;;;;;;;

;; http://keens.github.io/blog/2016/12/29/kizuitararustnokankyoukouchikugakanarirakuninatteta/

;;; racerやrustfmt、コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
;;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             ;;; この辺の設定はお好みで
                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))

(defun my/rust-mode-hook ()
  (add-to-list 'company-backends 'company-racer)
  (racer-mode)
  (rustfmt-enable-on-save)
  (rainbow-delimiters-mode)
  )
(add-hook 'rust-mode-hook 'my/rust-mode-hook)


;;;;;;;;;;;;;;;;
;; emacs lisp ;;
;;;;;;;;;;;;;;;;

(defun my/emacs-lisp-mode-hook ()
  (rainbow-delimiters-mode)
  )
(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)


;;;;;;;;;;;;;;
;; elscreen ;;
;;;;;;;;;;;;;;

(require 'elscreen)
(setq elscreen-tab-display-kill-screen nil) ;; タブ全消しをしない
(setq elscreen-tab-display-control nil)


;;;;;;;;;;
;; evil ;;
;;;;;;;;;;

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

(require 'evil-relative-linum)

(require 'evil-textobj-between)

(require 'open-junk-file)
(setq open-junk-file-format "~/Dropbox/memo/junk/%Y/%m/%Y-%m%d-%H%M%S.")

(require 'evil-magit)

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

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
  "fI" 'find-user-init-file
  "fb" 'helm-mini
  "gs" 'magit-status
  "r" 'quickrun
  "el" 'flycheck-error-list
  "\\R" 'restart-emacs
  )

;; neotree
(evil-define-key 'normal neotree-mode-map
  (kbd "TAB") 'neotree-enter
  (kbd "RET") 'neotree-enter
  (kbd "gi") 'neotree-quick-look
  (kbd "q") 'neotree-hide
  (kbd ".") 'neotree-hidden-file-toggle
  (kbd "N") 'neotree-create-node
  (kbd "D") 'neotree-delete-node
  (kbd "R") 'neotree-rename-node
  (kbd "r") 'neotree-refresh
  )

;; python
(evil-define-key 'normal python-mode-map
  (kbd "gd") 'jedi:goto-definition
  (kbd "K") 'jedi:show-doc)

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

(setq fill-column 65)
(auto-fill-mode 1)

;; https://github.com/kluge/spacemacs.d/blob/264a3d3d3b6dc93e7e57212a149be396da79775f/layers/kluge/funcs.el#L12
(defun my-org-meta-return ()
  "org-meta-return and insert state"
  (interactive)
  (end-of-line)
  (org-meta-return)
  (evil-insert 1))

(defun my-org-insert-todo-heading ()
  (interactive)
  (end-of-line)
  (org-insert-todo-heading)
  (evil-insert 1))

(defun evil-org-mode-hooks ()
  (evil-define-key 'normal org-mode-map
    (kbd "M-<return>") 'my-org-meta-return
    (kbd "M-S-<return>") 'my-org-insert-todo-heading
    (kbd "\\x") 'org-toggle-checkbox
    ))

(add-hook 'org-mode-hook 'evil-org-mode-hooks)


;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

(require 'helm)
(require 'helm-fuzzier)

(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(helm-fuzzier-mode 1)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(helm-flx-mode +1)

;; popwin
(require 'popwin)

(add-to-list 'popwin:special-display-config '("^\\*helm.*\\*$" :regexp t))

(defun helm-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (popwin:display-buffer helm-buffer t)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))

(defun helm-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

(add-hook 'helm-after-initialize-hook #'helm-popwin-help-mode-off)
(add-hook 'helm-cleanup-hook #'helm-popwin-help-mode-on)

(when (featurep 'golden-ratio)
  (add-to-list 'golden-ratio-inhibit-functions 'helm-alive-p))


;;;;;;;;;;;;;
;; neotree ;;
;;;;;;;;;;;;;

;; http://kiririmode.hatenablog.jp/entry/20150806/1438786800

;; 隠しファイル
(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "__pycache__" "~$" "^#.*#$" "\\.elc$" "\\.qrinput$"))

;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
(setq neo-smart-open t)

;; ;; popwin との共存
;; (when neo-persist-show
;;   (add-hook 'popwin:before-popup-hook
;;             (lambda () (setq neo-persist-show nil)))
;;   (add-hook 'popwin:after-popup-hook
;;             (lambda () (setq neo-persist-show t))))


;;;;;;;;;
;; org ;;
;;;;;;;;;

(require 'org)

(add-to-list 'load-path "~/.emacs.d/el-get/org-mode/contrib/lisp")

(setq org-startup-with-inline-images t)
(setq org-src-fontify-natively t)

(setq org-plantuml-jar-path "~/lib/java/plantuml.jar")

;; (require 'ox-confluence)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (plantuml . t) (rust . t))
 )

;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "sh" "plantuml" "rust"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; org-default-notes-fileのディレクトリ
(setq org-directory "~/org/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
;; org-capture のテンプレート
(setq org-capture-templates
      '(("t" "Task" entry (file (expand-file-name (concat org-directory "/task.org")))
         "* TODO %?\n    %i\n   %a\n    %T")
        ("n" "Note" entry (file (expand-file-name (concat org-directory "/notes.org")))
         "* %?\n   %a\n    %T")
        ("r" "Reading" entry (file (expand-file-name (concat org-directory "/reading.org")))
         "* %?\n   %a\n    %T")
        ("j" "Journal" entry (file+datetree (expand-file-name (concat org-directory "/journal.org")))
         "* %?\nEntered on %U\n  %i\n  %a")))

;; ;; org-modeでの強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; ;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)

;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;; DONEの時刻を記録
(setq org-log-done 'time)

;;; ハイパーリンク
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(require 'ox-md nil t)
(require 'ox-gfm nil t)


;;;;;;;;;;;;;;
;; plantuml ;;
;;;;;;;;;;;;;;

(setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar"))
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))


;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(yas-global-mode 1)

(eval-after-load 'yasnippet
  '(progn
     (define-key yas-keymap (kbd "<tab>") nil)
     (define-key yas-keymap (kbd "TAB") nil)
     (define-key yas-keymap (kbd "<S-return>") 'yas-prev-field)
     (define-key yas-keymap (kbd "RET") 'yas-next-field-or-maybe-expand)))


;;;;;;;;;;;;;;
;; quickrun ;;
;;;;;;;;;;;;;;

(quickrun-add-command "rust/script"
  '((:command . "cargo")
    (:exec    . ("%c script %o %s")))
  :default "rust")


;;;;;;;;;;;;;;;;
;; confluence ;;
;;;;;;;;;;;;;;;;

;; (require 'confluence)
;; (require 'org-confluence)


;;;;;;;;;;;
;; miscs ;;
;;;;;;;;;;;

;; path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; git gutter
(global-git-gutter-mode +1)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; ;; which key
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)

;; all-the-icons
;; (require 'all-the-icons)


;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

(load-theme 'material t)


(provide 'init)
;;; init.el ends here
