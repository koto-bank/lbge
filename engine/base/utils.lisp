(in-package :lbge.utils)

(defun println (object &optional stream)
  (print object stream)
  (terpri))