;;; test-my-org-roam.el --- Test suite for my/org-roam helpers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; 1. テスト対象の関数を init.el から動的にロードする（相対パス対応）
(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (goto-char (point-min))
      (when (re-search-forward "(defun my/org-roam-node-created-date" nil t)
        (beginning-of-line)
        (eval (read (current-buffer))))
      (goto-char (point-min))
      (when (re-search-forward "(defun my/org-roam-find-recent-items" nil t)
        (beginning-of-line)
        (eval (read (current-buffer)))))))

(load-target-functions)

;; 2. テスト用の org-roam-node 構造体とアクセス関数の定義（モック）
(unless (cl-find-class 'org-roam-node)
  (cl-defstruct org-roam-node
    file file-title file-hash file-atime file-mtime id level point todo priority scheduled deadline title properties olp tags aliases refs))

;; 構造体のアクセサが未定義の場合の明示的モック定義
(unless (fboundp 'org-roam-node-properties)
  (defun org-roam-node-properties (node)
    (aref node 14)))

(unless (fboundp 'org-roam-node-file)
  (defun org-roam-node-file (node)
    (aref node 1)))

(unless (fboundp 'org-roam-node-tags)
  (defun org-roam-node-tags (node)
    (aref node 16)))

(unless (fboundp 'org-roam-node-category)
  (defun org-roam-node-category (node)
    "CATEGORY プロパティがあればそれを使用し、なければ 'Node とするモック"
    (or (cdr (assoc "CATEGORY" (org-roam-node-properties node)))
        "Node")))

(unless (fboundp 'org-roam-node-title)
  (defun org-roam-node-title (node)
    (aref node 13)))

;; 3. ERT 単体テストの記述
(ert-deftest test-my/org-roam-node-created-date ()
  "my/org-roam-node-created-date 関数の動作を検証する単体テスト"

  ;; テスト1: CREATED プロパティに括弧付きのタイムスタンプがある場合
  (let ((node-with-brackets
         (make-org-roam-node
          :file "/path/to/file.org"
          :title "Title"
          :properties '(("CREATED" . "[2023-11-10 19:07:52]")))))
    (should (equal (my/org-roam-node-created-date node-with-brackets) "2023-11-10")))

  ;; テスト2: CREATED プロパティに <...> 形式のタイムスタンプがある場合
  (let ((node-with-angles
         (make-org-roam-node
          :file "/path/to/file.org"
          :title "Title"
          :properties '(("CREATED" . "<2026-05-19 Tue>")))))
    (should (equal (my/org-roam-node-created-date node-with-angles) "2026-05-19")))

  ;; テスト3: CREATED がなく、ファイルパスが journal 形式の場合
  (let ((node-journal
         (make-org-roam-node
          :file "/home/yuta/org/roam/journal/2026/04/22/091250.org"
          :title "Title"
          :properties nil)))
    (should (equal (my/org-roam-node-created-date node-journal) "2026-04-22")))

  ;; テスト4: CREATED がなく、ファイルパスが nodes 形式の場合
  (let ((node-node
         (make-org-roam-node
          :file "/home/yuta/org/roam/nodes/20250424222752-famille.org"
          :title "Title"
          :properties nil)))
    (should (equal (my/org-roam-node-created-date node-node) "2025-04-24"))))

;; 4. my/org-roam-find-recent-items のフィルタ/ソート機能のテスト
(ert-deftest test-my/org-roam-find-recent-items ()
  "my/org-roam-find-recent-items のメイン処理が正しく動作するか検証するテスト"
  (cl-letf* (((symbol-function 'completing-read)
              (lambda (prompt choices &optional default)
                (car choices))) ; 最も日付が新しい最初の選択肢を自動選択
             ((symbol-function 'org-roam-node-visit)
              (lambda (node)
                node)) ; 訪問したノードオブジェクトを返す
             ((symbol-function 'org-roam-node-list)
              (lambda ()
                (list
                 ;; 1. 最近のジャーナル (2026-05-15) -> 抽出対象
                 (make-org-roam-node
                  :file "/home/yuta/org/roam/journal/2026/05/15/181832.org"
                  :title "Recent Journal"
                  :tags '("Journal")
                  :properties nil)
                 ;; 2. 古いジャーナル (2020-01-01) -> 対象外
                 (make-org-roam-node
                  :file "/home/yuta/org/roam/journal/2020/01/01/120000.org"
                  :title "Old Journal"
                  :tags '("Journal")
                  :properties nil)
                 ;; 3. 最近の一般メモ (2026-05-18) -> 対象外
                 (make-org-roam-node
                  :file "/home/yuta/org/roam/nodes/20260518120000-note.org"
                  :title "Recent Note"
                  :tags '("Note")
                  :properties nil)))))
    
    (let ((result-node (my/org-roam-find-recent-items)))
      (should result-node)
      (should (equal (org-roam-node-title result-node) "Recent Journal")))))

(provide 'test-my-org-roam)
;;; test-my-org-roam.el ends here
