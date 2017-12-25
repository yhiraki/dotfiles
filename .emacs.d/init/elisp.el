(defun my/emacs-lisp-mode-hook ()
  (rainbow-delimiters-mode)
  )
(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)
