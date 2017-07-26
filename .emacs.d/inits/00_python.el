(require 'pyenv-mode)

(print "evil enter")

(add-hook 'python-mode-hook
          #'(lambda ()
              (pyenv-mode)))

(defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
    (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

(print "hello")
