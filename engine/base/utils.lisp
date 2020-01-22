(in-package :lbge.utils)

(defun println (object &optional stream)
  (print object stream)
  (terpri))

(defun princln (object &optional stream)
  (princ object stream)
  (terpri))
