(use-package s :ensure t)
(use-package f :ensure t)

;; path 連結
;; http://tototoshi.hatenablog.com/entry/20110520/1305906664
(defun my/file-path-join (&rest paths)
  (reduce #'(lambda (x y) (concat (file-name-as-directory x) y)) paths))
