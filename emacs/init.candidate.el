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
       ))
  :mode
  ("\\.trello\\'" . org-mode)
  )
