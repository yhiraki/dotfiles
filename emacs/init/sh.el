(use-package sh
  :mode
  ("\\.zsh\\'" . shell-script-mode)
  :init
  (setq sh-basic-offset 2
        sh-indentation 2
        sh-indent-for-case-label 0
        sh-indent-for-case-alt '+))
