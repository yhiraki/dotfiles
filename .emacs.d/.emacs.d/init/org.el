(setq org-startup-with-inline-images t)
(setq org-src-fontify-natively t)

(setq org-plantuml-jar-path "~/lib/java/plantuml.jar")

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

(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
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
(require 'ox-confluence nil t)

(setq org-html-htmlize-output-type 'css)

(el-get-bundle org-reveal
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  )
