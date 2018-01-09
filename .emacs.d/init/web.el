(use-package web-mode
             :ensure t
             :config
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (rainbow-delimiters-mode)
             :mode
             ("\\.tpl\\.php\\'" . web-mode)
             ("\\.[agj]sp\\'" . web-mode)
             ("\\.as[cp]x\\'" . web-mode)
             ("\\.erb\\'" . web-mode)
             ("\\.mustache\\'" . web-mode)
             ("\\.djhtml\\'" . web-mode)
             ("\\.html?\\'" . web-mode))
