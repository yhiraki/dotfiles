(require 'llm)

(defcustom my/llm-output-language "Japanese" "LLM output language")

(defcustom my/llm-default-provider nil "Default LLM provider.")

(defun my/llm-chat-streaming-current-point (prompt)
  (llm-chat-streaming-to-point
   my/llm-default-provider
   prompt
   (current-buffer)
   (point)
   (lambda () (message "LLM processing is complete."))))

(defun my/llm-chat-streaming-new-buffer (prompt)
  (let ((buf (generate-new-buffer "*llm-streaming-output*")))
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf
      (funcall 'markdown-mode)
      (llm-chat-streaming
       my/llm-default-provider
       prompt
       (lambda (partial-response)
         (goto-char (point-max))
         (insert partial-response))
       (lambda (response) (message "LLM processing is complete."))
       (lambda (type message)
         (error (format "Error type: %s message: %s" type message)))
       ))))

(defcustom my/llm-prompt-template-generate-commit-message
  "[Instructions]
Refer to the log messages and generate a commit message from the Diffs.

[Logs]
%s

[Git Diffs]
%s
"
  "prompt template for `my/llm-generate-commit-message'")

(defun my/llm-generate-commit-message ()
  "Generate commit messages from Git diffs."
  (interactive)
  (let* ((logs (shell-command-to-string "git log -10 --oneline"))
         (diff (shell-command-to-string "git diff --cached"))
         (prompt (llm-make-chat-prompt
                  (format my/llm-prompt-template-generate-commit-message logs diff))))
    (when (string-empty-p diff)
      (error "No diffs."))
    (my/llm-chat-streaming-current-point prompt)))

(defcustom my/llm-prompt-template-translate
  "[Instruction]
Translate the following text into English if it is in Japanese, and into Japanese if it is in English.
Output only the translated text.

[Text]
%s
"
  "prompt template for `my/llm-translate-region'")

(defun my/llm-translate-region (begin end)
  "Translate region using llm."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties begin end))
         (prompt (llm-make-chat-prompt (format my/llm-prompt-template-translate text))))
    (kill-region begin end)
    (my/llm-chat-streaming-current-point prompt)))

(defcustom my/llm-prompt-code-completion-template
  "[instruction]
complete the following code snippet.
{%%?} is the current cursor position. output only the code.

[code snippet]
%s{%%?}%s
"
  "prompt template for `my/llm-code-completion-at-point'")

(defun my/llm-code-completion-at-point ()
  "Get code completion suggestions from llm at point."
  (interactive)
  (let* ((before-point (buffer-substring-no-properties (point-min) (point)))
         (after-point (buffer-substring-no-properties (point) (point-max)))
         (prompt (llm-make-chat-prompt
                  (format my/llm-prompt-code-completion-template
                          before-point after-point))))
    (my/llm-chat-streaming-current-point prompt)))

(provide 'init-llm)
;;; init.el ends here
