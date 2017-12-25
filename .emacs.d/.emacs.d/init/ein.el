;; (el-get-bundle ein)
;; (add-hook 'ein:notebook-mode-hook #'python-mode)

;; (defun user-ein-reply-callback (args content -metadata-not-used-)
;;     (let ((callback (plist-get args :callback))
;;             (candidates (plist-get content :matches)))
;;         (funcall callback candidates)))

;; (defun user-company-ein-callback (callback)
;;     (ein:kernel-complete
;;         (ein:get-kernel)
;;         (thing-at-point 'line)
;;         (current-column)
;;         (list :complete_reply
;;             (cons #'user-ein-reply-callback (list :callback callback))))
;;     )

;; (defun user-company-ein-backend (command &optional arg &rest ignored)
;;     (interactive (list 'interactive))
;;     (case command
;;         (interactive (company-begin-backend 'user-company-ein-backend))
;;         (prefix (company-anaconda-prefix))
;;         (candidates (cons :async #'user-company-ein-callback))
;;         (location nil)
;;         (sorted t)
;;         )
;;     )
