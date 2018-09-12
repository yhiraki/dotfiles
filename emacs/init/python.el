(use-package switch-buffer-functions
  :ensure t
  :init
  (setq pyenv-dir (expand-file-name "~/.anyenv/envs/pyenv/"))
  (setq pyenv-jedi-env-args (list))

  (defun switch-jedi-server ()
    "Automatically activates pyenv version if .python-version file exists."
    (f-traverse-upwards
     (lambda (path)
       (let ((pyenv-version-path (f-expand ".python-version" path)))
         (cond ((f-exists? pyenv-version-path)
                (let ((jedi-env (s-trim (f-read-text pyenv-version-path 'utf-8))))
                  (setq my/current-virtual-env (concat pyenv-dir "versions/" jedi-env))
                  (set (make-local-variable 'jedi:server-command) (list "python" jedi:server-script))
                  (set (make-local-variable 'jedi:server-args) (list "--virtual-env" my/current-virtual-env))
                  ;; python-environment-default-root-name jedi-env
                  ;; jedi:environment-root jedi-env
                  )
                  )
                ;; (jedi:stop-server)
                )))))
  (add-hook 'python-mode-hook 'switch-jedi-server)
  )


(use-package python
  :ensure t
  :defer t
  :init
  (setq python-environment-directory "~/.anyenv/envs/pyenv/versions/")
  :config
  (add-hook 'python-mode-hook
            '(lambda()
               (electric-indent-mode +1)
               (add-to-list 'company-backends 'company-jedi)
               (add-hook 'write-contents-functions
                         '(lambda()
                            (py-yapf-buffer)
                            (py-isort-buffer)
                            ))))
  )

(use-package jedi-core
  :ensure t
  :init
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t)
  )

(use-package company-jedi
  :ensure t
  )

(use-package py-yapf
  :ensure t
  :init
)

(use-package py-isort
  :ensure t
  )
