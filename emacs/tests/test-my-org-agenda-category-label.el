;;; test-my-org-agenda-category-label.el --- Test suite for my/org-agenda-category-label  -*- lexical-binding: t; -*-

;; usage: emacs --batch -l emacs/tests/test-my-org-agenda-category-label.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)

(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (dolist (marker '("(defun my/org-agenda-category-label"))
        (goto-char (point-min))
        (unless (search-forward marker nil t)
          (error "Not found in init.el: %s" marker))
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t)))))

(load-target-functions)

(defun test-my-org-agenda-category-label/at (file content needle)
  "FILE を訪問したことにした CONTENT の NEEDLE の行で label を返す。"
  (with-temp-buffer
    (insert content)
    (org-mode)
    (setq buffer-file-name file)
    (goto-char (point-min))
    (search-forward needle)
    (prog1 (my/org-agenda-category-label)
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil))))

(ert-deftest test-my-org-agenda-category-label/wp-without-category ()
  "カテゴリー無しの WP は、最上位見出しの題名を詰めて出す。"
  (let ((s (test-my-org-agenda-category-label/at
            "/x/wp/20261116-doc.org"
            "#+TODO: TODO NEXT STARTED | DONE\n* STARTED 文書管理の再設計 [1/2]\n** やること\n*** NEXT 手を付ける\n"
            "手を付ける")))
    (should (string-prefix-p "文書管理" s))
    (should (<= (string-width (string-trim-right s)) 11))
    (should-not (string-match-p "STARTED\\|\\[" s))))

(ert-deftest test-my-org-agenda-category-label/wp-with-category ()
  "案件の WP は #+CATEGORY をそのまま出す。"
  (let ((s (test-my-org-agenda-category-label/at
            "/x/pj/a/wp/20261116-x.org"
            "#+TODO: TODO NEXT STARTED | DONE\n#+CATEGORY: しまむら\n* STARTED 技術レビュー [1/2]\n** やること\n*** NEXT 読む\n"
            "読む")))
    (should (string-prefix-p "しまむら:" s))
    (should (= (string-width s) 12))))

(ert-deftest test-my-org-agenda-category-label/non-wp ()
  "WP 以外はカテゴリーをそのまま出す。"
  (let ((s (test-my-org-agenda-category-label/at
            "/x/inbox/20261116-note.org"
            "* TODO 買う\n"
            "買う")))
    (should (string-prefix-p "20261116-n" s))
    (should (= (string-width s) 12))))
