(use-package quickrun
  :ensure t
  :config
  (quickrun-add-command "rust/script"
    '((:command . "cargo")
      (:exec    . ("%c script %o %s")))
    :default "rust")
  (setq quickrun-timeout-seconds 30))
