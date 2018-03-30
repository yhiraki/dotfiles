(setq org-startup-with-inline-images nil
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
      (concatenate
       'list
       (f-files "~/org"
                (lambda (f)
                  (string= (f-ext f) "org"))
                'recursive)
       (f-files "~/.cache/junkfile"
                (lambda (f)
                  (string= (f-ext f) "org"))
                'recursive))
      )
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
;; org-capture のテンプレート
(setq org-capture-templates
      '(("t" "Task" entry (file "~/org/task.org") "* TODO %?%i\n  %a")
        ("m" "Mail" entry (file+datetree "~/org/mail.org") "* %?\n  %c\n  %T")
        ("n" "Note" entry (file "~/org/notes.org") "* %?\n  %a\n  %T")
        ("r" "Reading" entry (file+datetree "~/org/reading.org") "* %?\n  %c\n  %T")
        ("j" "Journal" entry (file+datetree "~/org/journal.org") "* %?\n  %a\n  Entered on %U")))

(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)

;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s@!)" "WAIT(w@/!)" "|" "DONE(d@!)" "CANCEL(c@/!)")))
;; DONEの時刻を記録
(setq org-log-done 'time)

(setq org-html-htmlize-output-type 'css)

;; syntax highlight
(setq org-src-fontify-natively t)

;; (use-package org-reveal
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;;   )

(use-package org :ensure org-plus-contrib)
(use-package ox-confluence)

(use-package ox-gfm :ensure t)
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
