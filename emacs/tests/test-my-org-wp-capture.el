;;; test-my-org-wp-capture.el --- Test suite for the WP capture helpers  -*- lexical-binding: t; -*-

;; usage: emacs --batch -l emacs/tests/test-my-org-wp-capture.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)

(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (dolist (marker '("(defun my/org-wp-file-path"
                        "(defun my/org-wp-read-project"
                        "(defun my/org-wp-template-string"))
        (goto-char (point-min))
        (unless (search-forward marker nil t)
          (error "Not found in init.el: %s" marker))
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t)))))

(load-target-functions)

(defconst test-my-org-wp-capture/time (encode-time (list 0 30 9 16 11 2026 nil -1 nil))
  "2026-11-16 月 09:30:00")

(ert-deftest test-my/org-wp-file-path ()
  (let ((root "/r/roam/"))
    (should (equal (my/org-wp-file-path root "shimamura" "proposal-review" test-my-org-wp-capture/time)
                   "/r/roam/secret/pj/shimamura/wp/20261116-proposal-review.org"))
    (should (equal (my/org-wp-file-path root 'secret "tax-return" test-my-org-wp-capture/time)
                   "/r/roam/secret/wp/20261116-tax-return.org"))
    (should (equal (my/org-wp-file-path root 'public "emacs-upgrade" test-my-org-wp-capture/time)
                   "/r/roam/wp/20261116-emacs-upgrade.org"))
    (should-error (my/org-wp-file-path root 'public "Bad Slug" test-my-org-wp-capture/time))))

(ert-deftest test-my/org-wp-read-project ()
  (let* ((root (make-temp-file "roam" t))
         (dir (expand-file-name "secret/pj/sample/" root)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file (expand-file-name "project.org" dir)
            (insert "#+DATE: 2026-10-15
* サンプル商事 案件                                   :pj@sample:
:PROPERTIES:
:ID:       EB84A803-FD77-4FAC-A8FA-391936769DC8
:CATEGORY: サンプル商事
:DIR:      files/
:END:
"))
          (should (equal (my/org-wp-read-project root "sample")
                         '(:slug "sample"
                           :id "EB84A803-FD77-4FAC-A8FA-391936769DC8"
                           :title "サンプル商事 案件"
                           :category "サンプル商事")))
          (should-not (my/org-wp-read-project root "missing")))
      (delete-directory root t))))

(ert-deftest test-my/org-wp-template-string/project ()
  (let ((s (my/org-wp-template-string
            '(:slug "sample" :id "PID" :title "サンプル商事 案件" :category "サンプル商事")
            "NEW-ID" test-my-org-wp-capture/time)))
    (should (equal s "#+DATE: 2026-11-16
#+CATEGORY: サンプル商事
#+filetags: :pj@sample:
* TODO %^{WP の題名} [/]
:PROPERTIES:
:ID:       NEW-ID
:COOKIE_DATA: todo recursive
:DIR:      ../files/
:MODIFIED: [2026-11-16 09:30:00]
:END:

[[id:PID][サンプル商事 案件]] の%?

** やること
*** NEXT
** 経過
- [2026-11-16 月] 着手した
"))))

(ert-deftest test-my/org-wp-template-string/no-project ()
  (let ((s (my/org-wp-template-string nil "NEW-ID" test-my-org-wp-capture/time)))
    (should (equal s "#+DATE: 2026-11-16
* TODO %^{WP の題名} [/]
:PROPERTIES:
:ID:       NEW-ID
:COOKIE_DATA: todo recursive
:MODIFIED: [2026-11-16 09:30:00]
:END:

%?

** やること
*** NEXT
** 経過
- [2026-11-16 月] 着手した
"))))

(provide 'test-my-org-wp-capture)
;;; test-my-org-wp-capture.el ends here
