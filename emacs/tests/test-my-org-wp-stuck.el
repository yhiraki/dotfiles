;;; test-my-org-wp-stuck.el --- Test suite for my/org-wp-stuck-p  -*- lexical-binding: t; -*-

;; usage: emacs --batch -l emacs/tests/test-my-org-wp-stuck.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)

(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (dolist (marker '("(defvar my/org-sub-todo-progress-regexp"
                        "(defun my/org-wp-stuck-p"
                        "(defun my/org-agenda-skip-unless-stuck-wp"))
        (goto-char (point-min))
        (unless (search-forward marker nil t)
          (error "Not found in init.el: %s" marker))
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t)))))

(load-target-functions)

(defconst test-my-org-wp-stuck/sample "\
* STARTED 次の一手がある WP [1/3]
** やること
*** DONE 読む
*** NEXT 共有する
*** TODO 確かめる
* STARTED 着手中の行動がある WP [0/2]
** やること
*** STARTED 書く
*** TODO 送る
* STARTED 止まっている WP [1/3]
** やること
*** DONE 読む
*** TODO 共有する
*** WAITING 返事 :next:
** 経過
- NEXT を置く
* TODO 未着手の WP [0/1]
** やること
*** TODO 始める
* DONE 終わった WP [1/1]
** やること
*** DONE 済んだ
* SOMEDAY いつかの WP [0/1]
** やること
*** TODO 何か
* TODO 印の無い TODO
* 日報
** 朝 [0/1]
- [ ] チェック
")

(setq-default org-todo-keywords
              '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                (sequence "NEXT(n)" "STARTED" "|")
                (sequence "WAITING(w@)" "STARTED" "|")
                (sequence "SOMEDAY(S)" "|")
                (sequence "ASK(k)" "|" "ANSWERED(a)" "CLOSED(x)")
                (sequence "|" "CANCELLED(c@)")))

(defmacro test-my-org-wp-stuck/at (needle &rest body)
  "サンプルバッファの NEEDLE を含む見出しで BODY を評価する。"
  (declare (indent 1))
  `(with-temp-buffer
     (insert test-my-org-wp-stuck/sample)
     (org-mode)
     (goto-char (point-min))
     (search-forward ,needle)
     ,@body))

(ert-deftest test-my/org-wp-stuck-p/has-next ()
  (test-my-org-wp-stuck/at "次の一手がある WP"
    (should-not (my/org-wp-stuck-p))))

(ert-deftest test-my/org-wp-stuck-p/has-started ()
  (test-my-org-wp-stuck/at "着手中の行動がある WP"
    (should-not (my/org-wp-stuck-p))))

(ert-deftest test-my/org-wp-stuck-p/stuck ()
  "子孫に NEXT・STARTED の見出しが無ければ、本文の NEXT の字や WAITING があっても止まっている。"
  (test-my-org-wp-stuck/at "止まっている WP"
    (should (my/org-wp-stuck-p)))
  (test-my-org-wp-stuck/at "未着手の WP"
    (should (my/org-wp-stuck-p))))

(ert-deftest test-my/org-wp-stuck-p/not-in-progress ()
  "済んだ WP、SOMEDAY の WP、進捗の印の無い TODO、TODO でない見出しは対象外。"
  (dolist (needle '("終わった WP" "いつかの WP" "印の無い TODO" "朝 [0/1]"))
    (test-my-org-wp-stuck/at needle
      (should-not (my/org-wp-stuck-p)))))

(ert-deftest test-my/org-agenda-skip-unless-stuck-wp ()
  "止まっている WP では nil を返し、それ以外では次の見出しの位置を返す。"
  (test-my-org-wp-stuck/at "止まっている WP"
    (should-not (my/org-agenda-skip-unless-stuck-wp)))
  (test-my-org-wp-stuck/at "次の一手がある WP"
    (let ((next (my/org-agenda-skip-unless-stuck-wp)))
      (should (integerp next))
      (should (> next (point))))))

(provide 'test-my-org-wp-stuck)
;;; test-my-org-wp-stuck.el ends here
