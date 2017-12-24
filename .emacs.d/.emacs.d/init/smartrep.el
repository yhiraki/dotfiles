(el-get-bundle! smartrep
  (smartrep-define-key
   global-map "C-c" '(("+" . 'evil-numbers/inc-at-pt)
                      ("-" . 'evil-numbers/dec-at-pt)))
  )
