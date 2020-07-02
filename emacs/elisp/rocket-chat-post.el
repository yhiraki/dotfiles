;;; rocket-chat-post.el --- Post message to rocket.chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  yhiraki

;; Author: yhiraki <coffexpr@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar rocket-chat-post-executable "rocket-post")

(defun rocket-chat-post (msg &optional room)
  "Post message to rocket.chat."
  (when (executable-find rocket-chat-post-executable)
    (let ((cmd (list rocket-chat-post-executable msg)))
      (when room (nconc cmd (list "--room" room)))
      (message (mapconcat #'concat cmd " "))
      (call-process-shell-command
       (concat
        (mapconcat #'shell-quote-argument cmd " ")
        "&")))))

(defun rocket-chat-post-buffer ()
  "Post buffer to rocket.chat."
  (interactive)
  (rocket-chat-post-region (point-min) (point-max)))

(defun rocket-chat-post-region (begin end)
  "Post region to rocket.chat."
  (interactive "r")
  (rocket-chat-post
   (buffer-substring-no-properties begin end)))

(defvar rocket-chat-post-tmp-buffer-name "*tmp-rocket-chat*")

(defun open-new-rocket-chat-post-tmp-buffer ()
  "Open new rocket chat post buffer."
  (let* ((name rocket-chat-post-tmp-buffer-name)
         (buf (if (get-buffer name)
                  (get-buffer name)
                (let ((b (generate-new-buffer name)))
                  (set-buffer-major-mode b)
                  b))))
    (pop-to-buffer buf)
    (with-current-buffer rocket-chat-post-tmp-buffer-name
      (rocket-chat-edit-mode))
    )
  "")

(defun rocket-char-post-buffer-and-close ()
  "Post message from tmp buffer."
  (interactive)
  (when (string= (buffer-name) rocket-chat-post-tmp-buffer-name)
    (rocket-chat-post-buffer)
    (quit-window (buffer-name))))

(defvar rocket-chat-edit-mode-map (make-sparse-keymap))
(define-key rocket-chat-edit-mode-map (kbd "C-c C-c") 'rocket-char-post-buffer-and-close)
(define-key rocket-chat-edit-mode-map (kbd "C-c C-k") '(lambda () (interactive) (quit-window rocket-chat-post-tmp-buffer-name)))

(define-minor-mode rocket-chat-edit-mode
  "Rocket chat edit mode."
  :init-value nil
  :lighter " RChat"
  :keymap rocket-chat-edit-mode-map
  :group 'rocket-chat-edit
  )

(defun auto-enable-rocket-chat-edit-mode ()
  "Automatically set minor mode."
  (when (string= rocket-chat-post-tmp-buffer-name (buffer-name))
    (rocket-chat-edit-mode)))

(add-hook 'after-change-major-mode-hook 'auto-enable-rocket-chat-edit-mode)

(defun rocket-chat-edit ()
  "Rocket chat edit."
  (interactive)
  (open-new-rocket-chat-post-tmp-buffer))

(provide 'rocket-chat-post)
;;; rocket-chat-post.el ends here
