(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
        ))
(package-initialize)

(require 'evil)
(require 'magit)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<SPC>g") 'magit-status)
  (define-key evil-normal-state-map (kbd "<SPC>a") 'org-agenda)
  (define-key evil-normal-state-map (kbd "<SPC>c") 'org-capture)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)

  (with-eval-after-load 'magit
    (require 'evil-magit)
    (evil-magit-init)
    )
  )

(with-eval-after-load 'org
  (setq org-directory "~/org/")
  (setq org-hide-leading-stars t) ; 見出しの余分な*を消す
  (setq org-agenda-files (list org-directory))
  (setq org-capture-templates
        '(
          ("i" "Inbox\t\t- Add entry to Inbox"
           entry (file+headline "~/org/inbox.org" "Inbox")
           "** %?\n%T")

          ("j" "Journal\t- Short logs like Twitter"
           entry (file+olp+datetree "~/org/journal.org" "Journal")
           "* %?\n%T")
          ))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  )

(define-key key-translation-map [?\C-h] [?\C-?])
