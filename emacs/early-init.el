;;; early-init.el --- Early Initialization. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

;; Disable GC when start-up
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'after-init-hook
		  #'(lambda ()
			  (setq gc-cons-threshold (* 128 1024 1024))))

;; Default coding system
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; For Emacs 27+
(setq package-enable-at-startup nil)
;; Always load newest byte code
(setq load-prefer-newer t)

;; GUI appearance
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)

;; Inhibit splash screen & resizing frame
(setq inhibit-splash-screen t
      frame-inhibit-implied-resize t
      byte-compile-warnings '(cl-functions))

;; fringe
(custom-set-faces
 '(fringe ((t (:background nil)))))

;; https://jeffkreeftmeijer.com/emacs-native-comp-log/
(defvar native-comp-deferred-compilation-deny-list nil)

;; Avoid loading old bytecode instead of newer source.
;; Re: jka-compr: https://www.mattduck.com/2021-05-upgrading-to-emacs-28.html
;;
;; NOTE: uncomment the next 3 lines if seeing issues like:
;;
;;     Recursive load: "/Applications/Emacs.app/Contents/Resources/lisp/jka-compr.el.gz"
(setq load-prefer-newer nil)
(require 'jka-compr)
(require 'cc-fonts) ; emacs 29+
(setq load-prefer-newer t)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
