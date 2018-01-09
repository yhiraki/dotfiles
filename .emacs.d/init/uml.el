(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar"))
  (setq indent-tabs-mode nil
        tab-width 2)
  :mode
  ("\\.uml\\'" . plantuml-mode)
  ("\\.plantuml\\'" . plantuml-mode))
