(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.emacs.d/css/github.css")
  :commands
  (markdown-mode gfm-mode)
  :config
  ;; fold level
  (hide-sublevels 3)
  :bind
  (:map markdown-mode-map
        ("<tab>" . 'markdown-cycle)
        ("<S-tab>" . 'markdown-shifttab))
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode))

(use-package edit-indirect :ensure t)
