;; Mouse scrolling in terminal emacs
;; https://stackoverflow.com/questions/18198387/how-do-i-mouse-scroll-in-emacs-in-the-terminal-i-havent-gotten-mouse-wheel-mod
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(use-package shell
  :after evil
  :config
  (evil-set-initial-state 'shell-mode 'emacs))

(use-package counsel-tramp :ensure t)

(defconst my-completion-method 'consult)

(when (eq my-completion-method 'consult)
  (use-package consult :ensure t
	:bind
	([remap find-file] . #'consult-find)
	([remap imenu] . #'consult-imenu)
	([remap bookmark-jump] . #'consult-bookmark)
	([remap recentf-open-files] . #'consult-recent-file)
	([remap grep-find] .#'consult-ripgrep)
	([remap vc-git-grep] .#'consult-git-grep)

	:custom
	(consult-project-root-function #'vc-root-dir)

	:config
	(defalias 'my-outline #'consult-outline)

	:after evil
	:config
	(evil-define-key '(normal visual) 'global
	  (kbd "g/") 'consult-line)
	)

  (use-package consult-ghq :ensure t
	:commands (my-ghq consult-ghq-find consult-ghq-grep)
	:config
	(defalias 'my-ghq 'consult-ghq-find))

  (use-package consult-ls-git :ensure t
	:config
	(defalias 'my-git-find #'consult-ls-git)
	)

  (use-package vertico :ensure t
	:hook (after-init . vertico-mode)
	:custom (vertico-count 20))

  (setq enable-recursive-minibuffers t)

  (use-package minibuffer
	:init
	(setq completion-category-defaults nil)
	:custom
	(completion-styles '(orderless))
	(completion-category-overrides '((file (styles partial-completion))))
	)

  (use-package orderless :ensure t
	)

  (use-package marginalia :ensure t
	:hook (after-init . marginalia-mode))

  (use-package embark-consult :ensure t
	:bind
	("C-o" . 'embark-act))
  )

(when (eq my-completion-method 'ivy)
  (use-package ivy :ensure t
	:diminish ivy-mode
	:hook (after-init . ivy-mode)

	:custom
	(enable-recursive-minibuffers t)
	(ivy-count-format "(%d/%d) ")
	(ivy-use-selectable-prompt t)
	(ivy-use-virtual-buffers t)

	:bind
	(:map ivy-minibuffer-map ([escape] . 'minibuffer-keyboard-quit))
	)

  (use-package all-the-icons-ivy :ensure t
	:hook (after-init . all-the-icons-ivy-setup))

  (use-package ivy-rich :ensure t
	:hook (ivy-mode . ivy-rich-mode)
	)

  (use-package counsel :ensure t
	:after ivy
	:custom
	(counsel-yank-pop-separator "\n-------\n")
	:bind
	("M-x" . counsel-M-x)
	([remap org-set-tags-command] . #'counsel-org-tag)
	([remap find-file] . #'counsel-find-file)
	([remap describe-function] . #'counsel-describe-function)
	([remap describe-bindings] . #'counsel-descbinds)
	([remap describe-variable] . #'counsel-describe-variable)
	([remap imenu] . #'counsel-imenu)
	([remap bookmark-jump] . #'counsel-bookmark)
	([remap recentf-open-files] . #'counsel-recentf)
	([remap grep-find] .#'counsel-rg)
	([remap vc-git-grep] .#'counsel-git-grep)

	:config
	(defalias 'my-outline 'counsel-outline)
	(defalias 'my-git-find 'counsel-git)
	)

  (use-package swiper :ensure t
	:after evil
	:config
	(evil-define-key '(normal visual) 'global
	  "g/" #'swiper-isearch)
	)

  (use-package ivy-ghq
	:quelpa (ivy-ghq :fetcher github :repo "analyticd/ivy-ghq")
	:commands (ivy-ghq-open my-ghq)
	:config
	(defalias 'my-ghq 'ivy-ghq-open)
	)

  (use-package wgrep :ensure t)

  (use-package ivy-hydra :ensure t)
  )

(use-package diff-hl :ensure t)
