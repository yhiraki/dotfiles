(el-get-bundle plantuml-mode
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  )

(setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar"))
(add-hook 'plantuml-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
          ))
