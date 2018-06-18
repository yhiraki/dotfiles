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
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t)
  :defer t
  :config
  (add-to-list 'company-backends 'company-jedi)
  (electric-indent-mode +1))

(use-package jedi-core
  :ensure t
  :defer t
  :after python
  )

(use-package company-jedi
  :ensure t
  :defer t
  :after python
  )

(use-package py-autopep8
  :init (el-get-bundle fujimisakari/py-autopep8.el)
  :after python
  :defer t
  :ensure t
  ;; (define-key python-mode-map (kbd "C-c F") 'py-autopep8)          ; バッファ全体のコード整形
  ;; (define-key python-mode-map (kbd "C-c f") 'py-autopep8-region)   ; 選択リジョン内のコード整形
  ;; 保存時にバッファ全体を自動整形する
  ;; (add-hook 'before-save-hook 'py-autopep8-before-save)
  )
