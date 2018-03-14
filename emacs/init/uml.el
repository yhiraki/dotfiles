(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path (expand-file-name "~/lib/java/plantuml.jar"))
  :mode
  ("\\.pu\\'" . plantuml-mode)
  ("\\.uml\\'" . plantuml-mode)
  ("\\.puml\\'" . plantuml-mode)
  ("\\.plantuml\\'" . plantuml-mode))

(use-package flycheck-plantuml
  :ensure t
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup))
