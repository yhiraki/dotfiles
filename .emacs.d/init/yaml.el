(use-package yaml-mode
  :ensure t
  :bind
  (:map yaml-mode-map ("\C-m" . 'newline-and-indent))
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode))
