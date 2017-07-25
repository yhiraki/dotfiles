;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
(tool-bar-mode -1)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)

;; カーソル行をハイライトする
(global-hl-line-mode t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; スクロールバーを非表示
(scroll-bar-mode 0)

;; 改行コードを表示する
; (setq eol-mnemonic-dos "(CRLF)")
; (setq eol-mnemonic-mac "(CR)")
; (setq eol-mnemonic-unix "(LF)")

;; スペース、タブなどを可視化する
; (global-whitespace-mode 1)

;; スクロールは１行ごとに
(setq scroll-conservatively 1)

;; Macのoptionをメタキーにする
(setq mac-option-modifier 'meta)

;; smartchr
; (require smartchr)
; (global-set-key (kbd "=") (smartchr '(" = " " == " " === ")))

;; Color theme
; (require 'color-theme-sanityinc-tomorrow)
; (color-theme-sanityinc-tomorrow-night)
(require 'color-theme-sanityinc-tomorrow)
 (color-theme-sanityinc-tomorrow-night)

;; path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; python
;; (setenv "PYTHONPATH" "~/.pyenv/shims/python")
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Enable evil
(setq recentf-max-saved-items 2000) ;; 2000ファイルまで履歴保存する
(setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
(setq recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(require 'evil-leader)
(evil-mode 1)

; (helm-flx-mode +1)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "fr" 'helm-recentf)

;; git gutter
(global-git-gutter-mode +1)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
; (require 'yasnippet)
(yas-global-mode 1)

;; key-combo
; (require 'key-combo)
; (global-key-combo-mode t)
; (key-combo-load-default)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

; ;; pyenv
; (defun projectile-pyenv-mode-set ()
;   "Set pyenv version matching project name."
;   (let ((project (projectile-project-name)))
;     (if (member project (pyenv-mode-versions))
;         (pyenv-mode-set project)
;       (pyenv-mode-unset))))
;
; (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

;; helm
(helm-mode 1)
; (setq helm-mode-fuzzy-match t)
; (setq helm-completion-in-region-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)
; (require 'helm-fuzzier)
; (helm-fuzzier-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

; ;; which key
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(package-selected-packages (quote (init-loader pyenv-mode jedi-core))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; whitespace
; http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
; (setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

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
