(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get
;; http://tarao.hatenablog.com/entry/20150221/1424518030
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; init-loader
(el-get-bundle init-loader
  (setq-default init-loader-show-log-after-init nil)
                ;; init-loader-byte-compile t)
  (init-loader-load (locate-user-emacs-file "init-loader")))
