(use-package quickrun
  :ensure t
  :init
  (setq quickrun-timeout-seconds 30)
  :config
  (quickrun-add-command "rust/script"
    '((:command . "cargo")
      (:exec    . ("%c script %o %s")))
    :default "rust"))
