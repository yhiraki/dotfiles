;;; test-my-org-in-project.el --- Test suite for my/org-in-project-p  -*- lexical-binding: t; -*-

;; usage: emacs --batch -l emacs/tests/test-my-org-in-project.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)

;; 1. テスト対象の関数を init.el から動的にロードする（相対パス対応）
(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (dolist (marker '("(defvar my/org-sub-todo-progress-regexp"
                        "(defun my/org-in-project-p"))
        (goto-char (point-min))
        (when (search-forward marker nil t)
          (goto-char (match-beginning 0))
          (eval (read (current-buffer)) t))))))

(load-target-functions)

(defconst test-my-org-in-project/sample "\
* akikai-board PMS申請 [4/7]
** TODO クラウド利用方針を書く
* STARTED 火災保険金の請求と修理 [1/3]
** TODO 玄関のタイル [0/5]
* 日報
** ルーティン
*** 朝
**** 出勤 [3/4]
***** TODO コミマシ
* DONE 完了プロジェクト [2/2]
** TODO 取り残し
* TODO 単独タスク
")

;; org-mode の起動時に取り込ませるため、バッファローカルではなく既定値に置く
(setq-default org-todo-keywords
              '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                (sequence "NEXT(n)" "STARTED" "|")
                (sequence "WAITING(w@)" "STARTED" "|")
                (sequence "SOMEDAY(S)" "|")
                (sequence "ASK(k)" "|" "ANSWERED(a)" "CLOSED(x)")
                (sequence "|" "CANCELLED(c@)")))

(defmacro test-my-org-in-project/at (needle &rest body)
  "サンプルバッファの NEEDLE の位置で BODY を評価する。"
  (declare (indent 1))
  `(with-temp-buffer
     (insert test-my-org-in-project/sample)
     (org-mode)
     (goto-char (point-min))
     (search-forward ,needle)
     ,@body))

(ert-deftest test-my-org-in-project/under-started-project ()
  "祖先が未完了 TODO かつ進捗クッキー付きならプロジェクト配下と判定する。"
  (test-my-org-in-project/at "TODO 玄関のタイル"
    (should (my/org-in-project-p))))

(ert-deftest test-my-org-in-project/under-keywordless-heading ()
  "進捗クッキーだけを持つキーワード無し見出しはプロジェクトとみなさない。"
  (test-my-org-in-project/at "TODO クラウド利用方針を書く"
    (should-not (my/org-in-project-p))))

(ert-deftest test-my-org-in-project/under-journal-routine ()
  "日報の集計見出し配下の TODO はプロジェクト配下とみなさない。"
  (test-my-org-in-project/at "TODO コミマシ"
    (should-not (my/org-in-project-p))))

(ert-deftest test-my-org-in-project/under-done-project ()
  "祖先が完了済みキーワードならプロジェクト配下とみなさない。"
  (test-my-org-in-project/at "TODO 取り残し"
    (should-not (my/org-in-project-p))))

(ert-deftest test-my-org-in-project/toplevel ()
  "祖先見出しが無ければ nil。"
  (test-my-org-in-project/at "TODO 単独タスク"
    (should-not (my/org-in-project-p))))

(provide 'test-my-org-in-project)
;;; test-my-org-in-project.el ends here
