;;; test-my-org-attach-secret.el --- Test suite for my/org-attach-secret-id-dir  -*- lexical-binding: t; -*-

;; usage: emacs --batch -l emacs/tests/test-my-org-attach-secret.el -f ert-run-tests-batch-and-exit

(require 'ert)

(defun load-target-functions ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-el-path (expand-file-name "../init.el" current-dir)))
    (with-temp-buffer
      (insert-file-contents init-el-path)
      (dolist (marker '("(defun my/org-attach-secret-id-dir"))
        (goto-char (point-min))
        (unless (search-forward marker nil t)
          (error "Not found in init.el: %s" marker))
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t)))))

(load-target-functions)

(ert-deftest test-my/org-attach-secret-id-dir ()
  (let* ((roam (file-name-as-directory (make-temp-file "roam" t)))
         (secret (expand-file-name "secret/" roam)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "journal" secret) t)
          (make-directory (expand-file-name "nodes" roam) t)
          (should (equal (my/org-attach-secret-id-dir
                          (expand-file-name "journal/2026/11/16/090000.org" secret) secret)
                         (file-name-as-directory (expand-file-name "data" (file-truename secret)))))
          (should (equal (my/org-attach-secret-id-dir
                          (expand-file-name "pj/shimamura/wp/20261112-x.org" secret) secret)
                         (file-name-as-directory (expand-file-name "data" (file-truename secret)))))
          (should-not (my/org-attach-secret-id-dir
                       (expand-file-name "nodes/20250424222752-famille.org" roam) secret))
          (should-not (my/org-attach-secret-id-dir
                       (expand-file-name "nodes/secret-santa.org" roam) secret))
          (should-not (my/org-attach-secret-id-dir nil secret)))
      (delete-directory roam t))))

(provide 'test-my-org-attach-secret)
;;; test-my-org-attach-secret.el ends here
