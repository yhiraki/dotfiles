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
;; (setq eol-mnemonic-dos "(CRLF)")
;; (setq eol-mnemonic-mac "(CR)")
;; (setq eol-mnemonic-unix "(LF)")

;; スペース、タブなどを可視化する
;; (global-whitespace-mode 1)

;; スクロール
(setq scroll-conservatively 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Macのoptionをメタキーにする
;; (setq mac-option-modifier 'meta)

;; (yes/no) を (y/n)に
(fset 'yes-or-no-p 'y-or-n-p)

;; バッファの終端を表示
(setq indicate-buffer-boundaries 'left)

;; 最終行で必ず改行
(setq require-final-newline t)

;; 単語の境界をvim風に
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; recent 関連
(setq recentf-max-saved-items 2000) ;; 2000ファイルまで履歴保存する
(setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
(setq recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1)

; backup ファイルの作成ディレクトリ
; https://www.emacswiki.org/emacs/BackupDirectory#toc3
(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.backups/emacs/"
                         (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

; history の永続化
(require 'undohist)
(undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("/tmp/" "COMMIT_EDITMSG"))

(projectile-mode)

(rainbow-delimiters-mode)

;; font
(set-frame-font "ricty-13")

;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)
