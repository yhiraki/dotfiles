(use-package switch-buffer-functions
  :ensure t
  :init
  (defvar pyenv-dir "~/.anyenv/envs/pyenv/")
  (setq my/current-virtual-env (concat pyenv-dir "versions/" (s-trim (f-read-text (concat pyenv-dir "/version") 'utf-8)))
        jedi:server-args (list "--virtual-env" my/current-virtual-env))
  (defun switch-jedi-server ()
    "Automatically activates pyenv version if .python-version file exists."
    (f-traverse-upwards
     (lambda (path)
       (let ((pyenv-version-path (f-expand ".python-version" path)))
         (cond ((f-exists? pyenv-version-path)
                (setq-default my/current-virtual-env (concat pyenv-dir "versions/" (s-trim (f-read-text pyenv-version-path 'utf-8)))
                              jedi:server-args (list "--virtual-env" my/current-virtual-env))
                ))))))
  (add-hook 'switch-buffer-functions
            (lambda (prev cur) (switch-jedi-server)))
  )


(use-package python
  :init
  :defer t
  :config
  (add-to-list 'company-backends 'company-jedi)
  (electric-indent-mode +1)
  )

(use-package jedi-core
  :ensure t
  )

(use-package company-jedi
  :ensure t
  :init
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t)
  )

(use-package py-autopep8
  :ensure t
  )
