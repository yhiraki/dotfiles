(setq org-startup-with-inline-images t
      org-src-fontify-natively t)
(setq org-plantuml-jar-path "~/lib/java/plantuml.jar")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (plantuml . t) )
 )

;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "sh" "plantuml" "rust"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; org-default-notes-fileのディレクトリ
(setq org-directory "~/org/")
;; agenda ディレクトリ
;; https://www.reddit.com/r/orgmode/comments/6q6cdk/adding_files_to_the_agenda_list_recursively/
(setq org-agenda-files
      (f-files "~/org"
               (lambda (f)
                 (string= (f-ext f) "org"))
               'recursive))
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
;; org-capture のテンプレート
(setq org-capture-templates
      '(("t" "Task" entry (file+headline (expand-file-name (concat org-directory "/task.org")) "Tasks")
         "* TODO %?%i\n  %a\n  %T")
        ("m" "Mail" entry (file+datetree (expand-file-name (concat org-directory "/mail.org")) "Mails")
         "* %?\n  %c\n  %T")
        ("n" "Note" entry (file+headline (expand-file-name (concat org-directory "/notes.org")) "Notes")
         "* %?\n  %a\n  %T")
        ("r" "Reading" entry (file+headline (expand-file-name (concat org-directory "/reading.org")) "Readings")
         "* %?\n  %c\n  %T")
        ("j" "Journal" entry (file+datetree (expand-file-name (concat org-directory "/journal.org")))
         "* %?\n  %a\n  Entered on %T")))

(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)

;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;; DONEの時刻を記録
(setq org-log-done 'time)

(setq org-html-htmlize-output-type 'css)

;; syntax highlight
(setq org-src-fontify-natively t)

;; (use-package org-reveal
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;;   )

;; (use-package org
;;   :ensure org-plus-contrib
;;   :config
;;   (require 'ox-confluence))

(setq org-publish-project-alist
      '(
        ("all"
         ;; Path to org files
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "/tmp/org/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         )
        ))

