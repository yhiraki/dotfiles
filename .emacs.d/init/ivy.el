(use-package counsel :ensure t)
(use-package swiper :ensure t)
(use-package counsel-ghq
             :init (el-get-bundle windymelt/counsel-ghq))
(ivy-mode 1)
(setq ivy-use-virtual-buffers t
      enable-recursive-minibuffers t)
