(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.emacs.d/css/github.css")
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (hide-sublevels 1)))
  :commands
  (markdown-mode gfm-mode)
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  )

(use-package edit-indirect :ensure t)
