(require 'yasnippet)

(defvar plantuml-arrow-right-alist
      '(
        "->  ｜ Arrow"
        "-|> ｜ [Class] Extension"
        "-*  ｜ [Class] Composition"
        "-o  ｜ [Class] Aggretation"
        "-#  ｜ [Class]"
        "-x  ｜ [Class]"
        "-}  ｜ [Class]"
        "-+  ｜ [Class]"
        "-^  ｜ [Class]"
        ))

(defvar plantuml-description-delimiter "｜")

(defun plantuml-choose (alist)
  (string-trim-right
   (nth 0 (split-string
           (yas-choose-value alist) plantuml-description-delimiter))))
