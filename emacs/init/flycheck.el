(use-package flycheck
  :ensure t
  :init
  ;; https://github.com/lunaryorn/old-emacs-configuration/blob/master/lisp/flycheck-virtualenv.el
  (declare-function python-shell-calculate-exec-path "python")

  (defun flycheck-virtualenv-executable-find (executable)
    "Find an EXECUTABLE in the current virtualenv if any."
    (if (bound-and-true-p python-shell-virtualenv-root)
        (let ((exec-path (python-shell-calculate-exec-path)))
          (executable-find executable))
      (executable-find executable)))

  (defun flycheck-virtualenv-setup ()
    "Setup Flycheck for the current virtualenv."
    (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))

  :config
  (global-flycheck-mode))
