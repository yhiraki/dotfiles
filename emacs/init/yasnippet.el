(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs (list
                          (locate-user-emacs-file "snippets")
                          "~/.yasnippet"
                          'yas-installed-snippets-dir)
        yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
  (add-hook 'find-file-hook
            '(lambda()
               (yas-global-mode 1)
               ))
  :bind (:map yas-keymap
              ("<tab>" . popup-next)
              ("RET" . yas-next-field-or-maybe-expand))
  )

(use-package yasnippet-snippets
  :ensure t
  :defer t
  )

;; use popup menu for yas-choose-value
;; https://www.emacswiki.org/emacs/Yasnippet
(use-package popup
  :ensure t
  :after yasnippet
  :config
  (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
    (when (featurep 'popup)
      (popup-menu*
       (mapcar
        (lambda (choice)
          (popup-make-item
           (or (and display-fn (funcall display-fn choice))
               choice)
           :value choice))
        choices)
       :prompt prompt
       ;; start isearch mode immediately
       :isearch t
       )))
  :bind (:map popup-menu-keymap
              ("C-n" . popup-next)
              ("TAB" . popup-next)
              ("<tab>" . popup-next)
              ("<backtab>" . popup-previous)
              ("C-p" . popup-previous)))

;; http://emacs.rubikitch.com/sd1602-autoinsert-yatemplate-yasnippet/
(use-package yatemplate
  :ensure t
  :after yasnippet
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode 1)
  )
