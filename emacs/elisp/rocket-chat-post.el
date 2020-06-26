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
  "Post message to rocket.chat"
  (when (executable-find rocket-chat-post-executable)
    (let ((cmd (list rocket-chat-post-executable msg)))
      (when room (nconc cmd (list "--room" room)))
      (add-to-list 'cmd "&" t)
      (message (mapconcat #'concat cmd " "))
      (call-process-shell-command
       (mapconcat #'shell-quote-argument cmd " ")))))

(defun rocket-chat-post-buffer ()
  "Post buffer to rocket.chat"
  (interactive)
  (rocket-chat-post-region (point-min) (point-max)))

(defun rocket-chat-post-region (begin end)
  "Post region to rocket.chat"
  (interactive "r")
  (rocket-chat-post
   (buffer-substring-no-properties begin end)))

(provide 'rocket-chat-post)
;;; rocket-chat-post.el ends here
