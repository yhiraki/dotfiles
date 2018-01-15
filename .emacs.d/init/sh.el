(use-package sh
  :mode
  ("\\.zsh\\'" . shell-script-mode)
  :config
  (setq sh-basic-offset 4
        sh-indentation 4
        sh-indent-for-case-label 0
        sh-indent-for-case-alt '+))
