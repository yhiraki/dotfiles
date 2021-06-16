  )

(use-package go-translate
  :quelpa (go-translate :fetcher github :repo "lorniu/go-translate")
  :custom
  (go-translate-token-current (cons 430675 2721866130)))

(use-package shell
  :hook
  (evil-after-load
   . (lambda () (evil-set-initial-state 'shell-mode 'emacs))))

(use-package counsel-tramp :ensure t)

(defconst my-completion-method 'ivy)

(when (eq my-completion-method 'consult)
  (use-package consult :ensure t)

  (use-package vertico :ensure t
	:hook (after-init . vertico-mode)
	:custom (vertico-count 20))

  (use-package orderless :ensure t
	:custom (completion-styles '(orderless)))

  (use-package marginalia :ensure t
	:hook (after-init . marginalia-mode))
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

  (use-package ivy-hydra :ensure t
	:after (ivy hydra))

  (use-package ivy-rich :ensure t
	:hook (ivy-mode . ivy-rich-mode)
	)

  (use-package counsel :ensure t
	:after ivy
	:custom
	(counsel-yank-pop-separator "\n-------\n")
	:bind
	("M-x" . counsel-M-x)
	;; =C-M-m= to add/remove tag
	;; =C-M-j= to fix tags
	([remap org-set-tags-command] . #'counsel-org-tag)
	)

  (use-package swiper :ensure t :disabled
	:commands (swiper-isearch swiper-isearch-backward)
	:bind
	;; evil-search-next(n)が逆向き(N)になる
	([remap evil-search-forward] . 'swiper-isearch)
	([remap evil-search-forward] . 'swiper-isearch-backward)
	)

  (use-package ivy-ghq
	:quelpa (ivy-ghq :fetcher github :repo "analyticd/ivy-ghq")
	:commands (ivy-ghq-open)
	)
  )
