;; (use-package org-reveal
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;;   )

(use-package org
  :defer t
  :ensure org-plus-contrib
  :init
  ;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("python" "sh" "plantuml" "rust"))))

  (setq org-startup-with-inline-images nil
        org-src-fontify-natively t
        org-plantuml-jar-path "~/lib/java/plantuml.jar"
        org-default-notes-file "notes.org"
        org-capture-templates
        '(("t" "Task\t\t- TODOs" entry (file "~/org/task.org") "* TODO %?%i\n  %a")
          ("m" "Mail\t\t- Mail or text message drafts" entry (file+datetree "~/org/mail.org") "* %?\n  %c\n  %T")
          ("n" "Note\t\t- Notes" entry (file "~/org/notes.org") "* %?\n  %a\n  %T")
          ("r" "Reading\t- Web surfing" entry (file+datetree "~/org/reading.org") "* %?\n  %c\n  %T")
          ("j" "Journal\t- Short logs like Twitter" entry (file+datetree "~/org/journal.org") "* %?\n  %c\n  Entered on %U"))
        org-hide-leading-stars t ; 見出しの余分な*を消す
        org-todo-keywords ; TODO状態
        '((sequence "TODO(t)" "STARTED(s@!)" "WAIT(w@/!)" "|" "DONE(d@!)" "CANCEL(c@/!)"))
        org-log-done 'time ; DONEの時刻を記録
        org-html-htmlize-output-type 'css
        org-src-fontify-natively t
        org-publish-directory "~/public_html/"

        ;; org-default-notes-fileのディレクトリ
        org-directory "~/org/"

        ;; agenda ディレクトリ
        ;; https://www.reddit.com/r/orgmode/comments/6q6cdk/adding_files_to_the_agenda_list_recursively/
        org-agenda-files (concatenate
                          'list
                          (f-files "~/org"
                                   (lambda (f)
                                     (string= (f-ext f) "org"))
                                   'recursive)
                          (f-files "~/.cache/junkfile"
                                   (lambda (f)
                                     (string= (f-ext f) "org"))
                                   'recursive))
        org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (plantuml . t) )
   )
  :mode (("\\.org\\'" . org-mode))
  )

(use-package ox-confluence
  :after org
  )

(use-package ox-gfm
  :ensure t
  :after org
  )

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
