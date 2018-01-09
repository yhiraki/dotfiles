(use-package yasnippet
             :ensure t
             :config
             (setq yas-snippet-dirs
                   (list
                    (locate-user-emacs-file "snippets")
                    "~/.yasnippet"
                    'yas-installed-snippets-dir
                    ))
             (yas-global-mode 1)

             (eval-after-load 'yasnippet
               '(progn
                  (define-key yas-keymap (kbd "RET") 'yas-next-field-or-maybe-expand)))

             (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
             )

;; use popup menu for yas-choose-value
;; https://www.emacswiki.org/emacs/Yasnippet
(use-package popup
             :ensure t
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
