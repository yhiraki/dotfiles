
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; packages
(require 'el-get)
(el-get-bundle evil)
(el-get-bundle evil-surround)
(el-get-bundle evil-numbers)
(el-get-bundle evil-leader)
(el-get-bundle evil-nerd-commenter)
(el-get-bundle evil-matchit)
; (el-get-bundle evil-extra-operator)
(el-get-bundle evil-exchange)
(el-get-bundle evil-args)
(el-get-bundle tarao/evil-plugins)
(el-get-bundle epc)
(el-get-bundle jedi-core)
(el-get-bundle company-jedi :depends (company-mode))
(el-get-bundle exec-path-from-shell)
(el-get-bundle git-gutter-fringe)
; (el-get-bundle key-combo)
(el-get-bundle helm)
; (el-get-bundle helm-fuzzier)
; (el-get-bundle helm-flx)
(el-get-bundle helm-ag)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-ghq)
(el-get-bundle flycheck)
(el-get-bundle popup)
(el-get-bundle smartparens)
; (el-get-bundle emmet-mode)
(el-get-bundle volatile-highlights)
(el-get-bundle yasnippet)
; (el-get-bundle pyenv-mode)
(el-get-bundle which-key)
; (el-get-bundle quickrun)
(el-get-bundle color-theme-sanityinc-tomorrow)
(el-get-bundle whitespace)
(el-get-bundle key-combo
  :type github :pkgname "uk-ar/key-combo")
(el-get-bundle init-loader)

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
