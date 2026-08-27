;;; test-my-list-agenda-files.el --- Test suite for my/list-agenda-files  -*- lexical-binding: t; -*-

;; usage: emacs --batch -l emacs/tests/test-my-list-agenda-files.el -f ert-run-tests-batch-and-exit

(require 'ert)

(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (dolist (marker '("(defvar my/rg-agenda-exclude-globs"
                        "(defvar my/org-agenda-files-cache-file"
                        "(defun my/list-agenda-files--command"
                        "(defun my/list-agenda-files--report"
                        "(defun my/list-agenda-files ("
                        "(defun my/list-agenda-files-async"
                        "(defun my/org-agenda-files--read-cache"
                        "(defun my/org-agenda-files--write-cache"))
        (goto-char (point-min))
        (if (search-forward marker nil t)
            (progn (goto-char (match-beginning 0))
                   (eval (read (current-buffer)) t))
          (error "marker not found in init.el: %s" marker))))))

(load-target-functions)

(defvar my/rg-org-directories nil)

(defun test-my-list-agenda-files/make-fixture ()
  "Create a temp org tree and return its path."
  (let ((dir (make-temp-file "agenda-files-test" t)))
    (dolist (spec '(("todo.org"          . "* TODO タスク\n")
                    ("meeting.org"       . "* 定例 :Meeting:\n")
                    ("plain.org"         . "* ただの見出し\n")
                    ("archived/old.org"  . "* TODO 昔のタスク\n")
                    ("refs/clip.org"     . "* TODO 参照クリップ\n")
                    ("data/att.org"      . "* TODO 添付\n")))
      (let ((path (expand-file-name (car spec) dir)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert (cdr spec)))))
    dir))

(defun test-my-list-agenda-files/names (files)
  (sort (mapcar #'file-name-nondirectory files) #'string<))

(defun test-my-list-agenda-files/await (regexes)
  "REGEXES で非同期取得し、完了を待って結果を返す。"
  (let (result done)
    (my/list-agenda-files-async regexes (lambda (files) (setq result files done t)))
    (with-timeout (20 (error "my/list-agenda-files-async timed out"))
      (while (not done) (accept-process-output nil 0.05)))
    result))

(ert-deftest test-my-list-agenda-files/single-regex ()
  (let* ((my/rg-org-directories (list (test-my-list-agenda-files/make-fixture))))
    (should (equal '("todo.org")
                   (test-my-list-agenda-files/names
                    (my/list-agenda-files "^\\*+ (TODO)\\b"))))))

(ert-deftest test-my-list-agenda-files/multiple-regexes-are-unioned ()
  "複数正規表現を渡すと、各正規表現の結果の和集合が返る。"
  (let* ((my/rg-org-directories (list (test-my-list-agenda-files/make-fixture))))
    (should (equal '("meeting.org" "todo.org")
                   (test-my-list-agenda-files/names
                    (my/list-agenda-files '("^\\*+ (TODO)\\b" "^\\*+.*(:Meeting:)")))))))

(ert-deftest test-my-list-agenda-files/excludes-archived-refs-and-data ()
  "archived / refs / data は agenda 対象を含まないので走査結果から外れる。"
  (let* ((my/rg-org-directories (list (test-my-list-agenda-files/make-fixture)))
         (names (test-my-list-agenda-files/names
                 (my/list-agenda-files "^\\*+ (TODO)\\b"))))
    (should (equal '("todo.org") names))))

(ert-deftest test-my-list-agenda-files/extra-filters ()
  (let* ((my/rg-org-directories (list (test-my-list-agenda-files/make-fixture))))
    (should-not (member "meeting.org"
                        (test-my-list-agenda-files/names
                         (my/list-agenda-files '("^\\*+ (TODO)\\b" "^\\*+.*(:Meeting:)")
                                               '("!**/meeting.org")))))))

(ert-deftest test-my-list-agenda-files/async-matches-sync ()
  "非同期版は同期版と同じファイル集合を返す。"
  (let* ((my/rg-org-directories (list (test-my-list-agenda-files/make-fixture)))
         (regexes '("^\\*+ (TODO)\\b" "^\\*+.*(:Meeting:)")))
    (should (equal (test-my-list-agenda-files/names (my/list-agenda-files regexes))
                   (test-my-list-agenda-files/names
                    (test-my-list-agenda-files/await regexes))))))

(ert-deftest test-my-list-agenda-files/async-excludes-stderr-noise ()
  "存在しないディレクトリを混ぜても、rg のエラー出力が結果に混入しない。"
  (let* ((my/rg-org-directories (list (test-my-list-agenda-files/make-fixture)
                                      "/nonexistent-dir-for-test"))
         (files (test-my-list-agenda-files/await "^\\*+ (TODO)\\b")))
    (should (equal '("todo.org") (test-my-list-agenda-files/names files)))
    (should (seq-every-p #'file-exists-p files))))

(ert-deftest test-my-list-agenda-files/cache-roundtrip ()
  "書き出したキャッシュがそのまま読み戻せる。"
  (let* ((my/org-agenda-files-cache-file
          (expand-file-name "cache.eld" (make-temp-file "agenda-cache" t)))
         (files '("~/org/a.org" "~/org/b.org")))
    (should-not (my/org-agenda-files--read-cache))
    (my/org-agenda-files--write-cache files)
    (should (equal files (my/org-agenda-files--read-cache)))))

(ert-deftest test-my-list-agenda-files/cache-read-tolerates-garbage ()
  "壊れたキャッシュは nil を返し、エラーにしない。"
  (let* ((my/org-agenda-files-cache-file
          (expand-file-name "cache.eld" (make-temp-file "agenda-cache" t))))
    (with-temp-file my/org-agenda-files-cache-file (insert "(((("))
    (should-not (my/org-agenda-files--read-cache))))

;;; test-my-list-agenda-files.el ends here
