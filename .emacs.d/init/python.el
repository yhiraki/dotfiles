(use-package jedi-core :ensure t)
(use-package company-jedi :ensure t)
(use-package switch-buffer-functions :ensure t)

(defun switch-jedi-server ()
  "Automatically activates pyenv version if .python-version file exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (cond ((f-exists? pyenv-version-path)
              (setq-default my/current-virtual-env (concat "~/.pyenv/versions/" (s-trim (f-read-text pyenv-version-path 'utf-8)))
                            jedi:server-args (list "--virtual-env" my/current-virtual-env))
              ))))))
(add-hook 'switch-buffer-functions
          (lambda (prev cur) (switch-jedi-server)))

(setq-default my/current-virtual-env (concat "~/.pyenv/versions/" (s-trim (f-read-text "~/.pyenv/version" 'utf-8)))
              jedi:server-args (list "--virtual-env" my/current-virtual-env))

(use-package python
  :init
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t)
  :config
  (add-to-list 'company-backends 'company-jedi)
  (electric-indent-mode +1))
