;;; test-my-org-ask-answered.el --- Test suite for my/org-ask-answered-by-checkbox  -*- lexical-binding: t; -*-

;; usage: emacs --batch -l emacs/tests/test-my-org-ask-answered.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)

(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (dolist (marker '("(defun my/org-ask-answered-by-checkbox"))
        (goto-char (point-min))
        (if (search-forward marker nil t)
            (progn (goto-char (match-beginning 0))
                   (eval (read (current-buffer)) t))
          (error "marker not found in init.el: %s" marker))))))

(load-target-functions)

(setq-default org-todo-keywords
              '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                (sequence "NEXT(n)" "STARTED" "|")
                (sequence "WAITING(w@)" "STARTED" "|")
                (sequence "ASK(k)" "|" "ANSWERED(a)" "CLOSED(x)")
                (sequence "|" "CANCELLED(c@)")))

(defconst test-my-org-ask-answered/sample "\
* STARTED 試験の WP [0/1]
:PROPERTIES:
:COOKIE_DATA: todo recursive
:END:
** やること
*** TODO 一つ目の行動
** 未決の問い
*** ASK 一つ選ぶ問い
どれにするか。1 つ選ぶ。
- [ ] 案その一（推奨）。理由
- [ ] 案その二。理由
- [ ] その他 →
*** ASK 自由記述の問い
何を聞くか。
- 聞くこと →
")

(defmacro test-my-org-ask-answered/with-sample (&rest body)
  `(with-temp-buffer
     (org-mode)
     (insert test-my-org-ask-answered/sample)
     (add-hook 'org-checkbox-statistics-hook #'my/org-ask-answered-by-checkbox nil t)
     (let ((org-log-done nil))
       ,@body)))

(defun test-my-org-ask-answered/toggle (text)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (org-toggle-checkbox))

(defun test-my-org-ask-answered/state (title)
  (goto-char (point-min))
  (search-forward title)
  (org-get-todo-state))

(ert-deftest test-my-org-ask-answered/first-check-answers ()
  (test-my-org-ask-answered/with-sample
   (test-my-org-ask-answered/toggle "案その一")
   (should (equal (test-my-org-ask-answered/state "一つ選ぶ問い") "ANSWERED"))))

(ert-deftest test-my-org-ask-answered/more-checks-keep-answered ()
  (test-my-org-ask-answered/with-sample
   (test-my-org-ask-answered/toggle "案その一")
   (test-my-org-ask-answered/toggle "案その二")
   (should (equal (test-my-org-ask-answered/state "一つ選ぶ問い") "ANSWERED"))))

(ert-deftest test-my-org-ask-answered/uncheck-all-reverts ()
  (test-my-org-ask-answered/with-sample
   (test-my-org-ask-answered/toggle "案その一")
   (test-my-org-ask-answered/toggle "案その一")
   (should (equal (test-my-org-ask-answered/state "一つ選ぶ問い") "ASK"))))

(ert-deftest test-my-org-ask-answered/other-headings-untouched ()
  (test-my-org-ask-answered/with-sample
   (test-my-org-ask-answered/toggle "案その一")
   (should (equal (test-my-org-ask-answered/state "自由記述の問い") "ASK"))
   (should (equal (test-my-org-ask-answered/state "一つ目の行動") "TODO"))))

(ert-deftest test-my-org-ask-answered/non-ask-heading-ignored ()
  (with-temp-buffer
    (org-mode)
    (insert "* TODO 普通の TODO\n- [ ] 手順\n")
    (add-hook 'org-checkbox-statistics-hook #'my/org-ask-answered-by-checkbox nil t)
    (test-my-org-ask-answered/toggle "手順")
    (should (equal (test-my-org-ask-answered/state "普通の TODO") "TODO"))))

(provide 'test-my-org-ask-answered)
;;; test-my-org-ask-answered.el ends here
