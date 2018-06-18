(use-package counsel
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  )

(use-package swiper
  :ensure t
  :defer t
  :commands (ivy-mode))

(use-package counsel-ghq
  :init (el-get-bundle windymelt/counsel-ghq)
  :defer t
  :commands (counsel-ghq)
  )
