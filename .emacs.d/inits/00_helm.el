(require 'helm)
(require 'helm-fuzzier)

(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(helm-fuzzier-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(helm-flx-mode +1)


;; popwin
(require 'popwin)

(add-to-list 'popwin:special-display-config '("^\\*helm.*\\*$" :regexp t))

(defun helm-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (popwin:display-buffer helm-buffer t)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))

(defun helm-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

(add-hook 'helm-after-initialize-hook #'helm-popwin-help-mode-off)
(add-hook 'helm-cleanup-hook #'helm-popwin-help-mode-on)

(when (featurep 'golden-ratio)
  (add-to-list 'golden-ratio-inhibit-functions 'helm-alive-p))
