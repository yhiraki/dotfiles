(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; startup page disabled
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; scroll
(setq scroll-conservatively 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; one line at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-follow-mouse 't) ; scroll window under mouse
(scroll-bar-mode 0)

;; menu
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)

;; cursor
(global-hl-line-mode t)
(show-paren-mode 1) ;; 対応する括弧を光らせる

;; buffer
(setq-default indicate-buffer-boundaries 'right) ;; バッファの終端を表示
(setq-default indicate-empty-lines t) ;; バッファの終端以降を可視化

;; line number
(add-hook 'prog-mode-hook
          '(lambda ()
             (linum-mode)
             ;; linumに起因する高速化
             ;; http://d.hatena.ne.jp/daimatz/20120215/1329248780
             (setq linum-delay t)
             (defadvice linum-schedule (around my-linum-schedule () activate)
               (run-with-idle-timer 0.2 nil #'linum-update-current))
             ))

;; indent
(setq-default c-basic-offset 2        ;基本インデント量
              tab-width 2             ;タブ幅
              indent-tabs-mode nil    ;インデントをタブでするかスペースでするか
              require-final-newline t)

;; editor
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package restart-emacs
  :ensure t)

;; 単語境界をvim風に
;; http://tarao.hatenablog.com/entry/20130304/evil_config#vim-word
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; エコーエリアや *Messages* バッファにメッセージを表示させたくない
;; http://qiita.com/itiut@github/items/d917eafd6ab255629346
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;; TAGS ファイルを自動で再読込
(setq tags-revert-without-query 1)


;; junkfile
(use-package open-junk-file
  :ensure t
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


;; highlights
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))


;; eldoc
(use-package eldoc
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-mode-hook 'eldoc-mode))
