(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;;;; add melpa and orgmode for packages
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
        ))

(unless package-archive-contents
  (package-refresh-contents))

;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(when (not (package-installed-p 'el-get))
  (package-install 'el-get))
(require 'el-get)

;; use-package
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

;; init-loader
(use-package init-loader
  :ensure t
  :config
  (init-loader-load (locate-user-emacs-file "init-loader")))
