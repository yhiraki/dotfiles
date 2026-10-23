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
        (eval (read (current-buffer))))
      (goto-char (point-min))
      (when (re-search-forward "(defun my/org-roam-filter-is-not-journals-or-tasks" nil t)
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
(defun test-my/org-roam--fixed-now ()
  "テストで固定する現在時刻（2026-05-20 12:00 ローカル時刻）を返す。"
  (encode-time (list 0 0 12 20 5 2026 nil -1 nil)))

(defun test-my/org-roam--journal-node (date title)
  "DATE（YYYY/MM/DD）のジャーナルノードを作る。"
  (make-org-roam-node
   :file (format "/home/yuta/org/roam/journal/%s/120000.org" date)
   :title title
   :tags '("Journal")
   :properties nil))

(ert-deftest test-my/org-roam-find-recent-items ()
  "my/org-roam-find-recent-items のメイン処理が正しく動作するか検証するテスト"
  (cl-letf* (((symbol-function 'current-time) #'test-my/org-roam--fixed-now)
             ((symbol-function 'completing-read)
              (lambda (prompt choices &optional default)
                (car choices))) ; 最も日付が新しい最初の選択肢を自動選択
             ((symbol-function 'org-roam-node-visit)
              (lambda (node)
                node)) ; 訪問したノードオブジェクトを返す
             ((symbol-function 'org-roam-node-list)
              (lambda ()
                (list
                 ;; 1. 最近のジャーナル (2026-05-15) -> 抽出対象
                 (test-my/org-roam--journal-node "2026/05/15" "Recent Journal")
                 ;; 2. 古いジャーナル (2020-01-01) -> 対象外
                 (test-my/org-roam--journal-node "2020/01/01" "Old Journal")
                 ;; 3. 最近の一般メモ (2026-05-18) -> 対象外
                 (make-org-roam-node
                  :file "/home/yuta/org/roam/nodes/20260518120000-note.org"
                  :title "Recent Note"
                  :tags '("Note")
                  :properties nil)))))

    (let ((result-node (my/org-roam-find-recent-items)))
      (should result-node)
      (should (equal (org-roam-node-title result-node) "Recent Journal")))))

(ert-deftest test-my/org-roam-find-recent-items-boundary ()
  "ちょうど14日前は候補に含み、15日前は含まないことを検証するテスト"
  (let (offered)
    (cl-letf* (((symbol-function 'current-time) #'test-my/org-roam--fixed-now)
               ((symbol-function 'completing-read)
                (lambda (prompt choices &optional default)
                  (setq offered choices)
                  (car choices)))
               ((symbol-function 'org-roam-node-visit) #'identity)
               ((symbol-function 'org-roam-node-list)
                (lambda ()
                  (list
                   (test-my/org-roam--journal-node "2026/05/06" "14 Days Ago")
                   (test-my/org-roam--journal-node "2026/05/05" "15 Days Ago")))))
      (my/org-roam-find-recent-items)
      (should (cl-some (lambda (c) (string-match-p "14 Days Ago" c)) offered))
      (should-not (cl-some (lambda (c) (string-match-p "15 Days Ago" c)) offered)))))

;; 5. my/org-roam-filter-is-not-journals-or-tasks の除外条件のテスト
(ert-deftest test-my/org-roam-filter-is-not-journals-or-tasks ()
  "ジャーナル・タスク・セッションブリーフを除外し、通常ノードを残すか検証するテスト"
  (cl-flet ((node (tags category)
              (make-org-roam-node
               :file "/path/to/file.org"
               :title "Title"
               :tags tags
               :properties (when category `(("CATEGORY" . ,category))))))
    (should (my/org-roam-filter-is-not-journals-or-tasks (node '("Note") nil)))
    (should-not (my/org-roam-filter-is-not-journals-or-tasks (node '("Journal") nil)))
    (should-not (my/org-roam-filter-is-not-journals-or-tasks (node nil "Journal")))
    (should-not (my/org-roam-filter-is-not-journals-or-tasks (node '("Task") nil)))
    (should-not (my/org-roam-filter-is-not-journals-or-tasks (node '("SessionBrief") nil)))
    (should-not (my/org-roam-filter-is-not-journals-or-tasks (node nil "Session")))))

(provide 'test-my-org-roam)
;;; test-my-org-roam.el ends here
