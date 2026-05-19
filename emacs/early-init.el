;;; early-init.el --- Early initialization.  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  yhiraki

;; Author: yhiraki
;; Keywords: init, performance

;;; Commentary:
;; This file is loaded before init.el. It is used to tune startup performance
;; and prevent early GUI flicker by disabling UI elements before the frame is created.

;;; Code:

;; 1. 起動時の GC（ガベージコレクション）閾値を一時的に 100MB に引き上げ、I/O 速度を最大化
(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.6)

;; 起動完了後に通常の 800KB に復元するフック
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 800 1024))
            (setq gc-cons-percentage 0.1)))

;; 2. 画面初期化のちらつきを防ぐため、UI 要素を最速で非表示化
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
