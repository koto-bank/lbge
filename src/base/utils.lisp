(in-package :lbge.utils)

(defun println (object &optional stream)
  (print object stream)
  (terpri))

(defun princln (object &optional stream)
  (princ object stream)
  (terpri))

(defun trim-newlines (string)
  (string-trim (list #\Newline #\Return) string))

(defun merge-lines (line-list)
  (reduce (lambda (a b)
            (concatenate 'string
                         a
                         b))
          (cdr line-list)
          :initial-value (car line-list)))
