(use-package org-trello :ensure t
  ;; https://org-trello.github.io/usage.html#automatic-org-trello-files-in-emacs
  :hook
  (org-mode
   . (lambda ()
       (let ((filename (buffer-file-name (current-buffer))))
         (when (and filename (string= "trello" (file-name-extension filename)))
           (org-trello-mode)))))
  (org-trello-mode
   . (lambda ()
       (org-trello-sync-buffer t)
       (evil-define-key 'normal org-trello-mode-map
	 (kbd "\\r") 'org-trello-sync-card
	 (kbd "\\s") 'org-trello-sync-buffer
	 )))

  :mode
  ("\\.trello\\'" . org-mode)
  )

(use-package go-translate
  :quelpa (go-translate :fetcher github :repo "lorniu/go-translate")
  :custom
  (go-translate-token-current (cons 430675 2721866130)))

(use-package shell
  :hook
  (evil-after-load
   . (lambda () (evil-set-initial-state 'shell-mode 'emacs))))

(use-package vterm :ensure t
  :custom
  (vterm-buffer-name-string "*vterm: %s*"))

(use-package vterm-toggle :ensure t
  :custom
  (vterm-toggle-scope 'project))

(use-package counsel-tramp :ensure t)
