;(defpackage "ltk-utils"
; (:use "COMMON-LISP"
;        "LTK")
;  (:export
;   "widget-delete"))

;(in-package "ltk-utils")


(defgeneric widget-delete-all (w))
(defmethod widget-delete-all ((w widget))
  (format-wish "~A delete all" (widget-path w)))
