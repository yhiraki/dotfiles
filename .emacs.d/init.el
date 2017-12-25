 ;; init.el --- Emacs configurations

(require 'cl)

;; packages
(require 'el-get)
(el-get-bundle tarao-elisps
  :type github :pkgname "tarao/elisp")
(el-get-bundle epc)
(el-get-bundle flx)
(el-get-bundle color-moccur)
(el-get-bundle helm)
(el-get-bundle helm-fuzzier)
(el-get-bundle helm-flx)
(el-get-bundle helm-ag)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-ghq)
(el-get-bundle helm-git-grep)
(el-get-bundle helm-gtags)
(el-get-bundle popup-pos-tip)
(e-get-bundle key-combo
  :type github :pkgname "uk-ar/key-combo")
(el-get-bundle all-the-icons
  :type github :pkgname "domtronn/all-the-icons.el")
(el-get-bundle ox-gfm)
(el-get-bundle pig-mode)
(el-get-bundle ein :depends (skewer-mode))


;;;;;;;;;;;;;;;;;
;; basic setup ;;
;;;;;;;;;;;;;;;;;

;; visibility

(set-frame-font "ricty-13")

(modify-syntax-entry ?_ "w" (standard-syntax-table))


;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

(require 'helm)
(require 'helm-fuzzier)

(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(helm-fuzzier-mode 1)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(helm-flx-mode +1)

(when (featurep 'golden-ratio)
  (add-to-list 'golden-ratio-inhibit-functions 'helm-alive-p))





;;;;;;;;;
;; ein ;;
;;;;;;;;;

(add-hook 'ein:notebook-mode-hook #'python-mode)

(defun user-ein-reply-callback (args content -metadata-not-used-)
    (let ((callback (plist-get args :callback))
            (candidates (plist-get content :matches)))
        (funcall callback candidates)))

(defun user-company-ein-callback (callback)
    (ein:kernel-complete
        (ein:get-kernel)
        (thing-at-point 'line)
        (current-column)
        (list :complete_reply
            (cons #'user-ein-reply-callback (list :callback callback))))
    )

(defun user-company-ein-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (case command
        (interactive (company-begin-backend 'user-company-ein-backend))
        (prefix (company-anaconda-prefix))
        (candidates (cons :async #'user-company-ein-callback))
        (location nil)
        (sorted t)
        )
    )



;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;



(provide 'init)
;;; init.el ends here
