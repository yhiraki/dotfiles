(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar")
        plantuml-java-options "-Djava.awt.headless=true"
        plantuml-options "-charset UTF-8")
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
                 "java -Djava.awt.headless=true -jar " plantuml-java-options " "
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
  ("C-c C-s" . 'plantuml-save-png))

(use-package flycheck-plantuml
  :ensure t
  :after plantuml-mode
  :init
  (add-hook 'plantuml-mode-hook
            '(lambda()
               (flycheck-plantuml-setup)
               ))
  )
