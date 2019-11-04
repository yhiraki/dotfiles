(require 'yasnippet)

(defvar plantuml-component-group-keywords
  '(
    "package"
    "node"
    "folder"
    "frame"
    "cloud"
    "database"
    ))

(defvar plantuml-sequence-group-keywords
  '(
    "alt"
    "opt"
    "loop"
    "par"
    "break"
    "critical"
    "group"
    ))

(defvar plantuml-arrow-right-alist
      '(
        "->  ｜ Arrow"
        "->x ｜ [Sequence] Deleted"
        "->o ｜ [Sequence]"
        "->> ｜ [Sequence]"
        "-\\  ｜ [Sequence]"
        "-\\\\ ｜ [Sequence]"
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
