(in-package :lbge.utils)

(defun println (object &optional (stream *standard-output*))
  (print object stream)
  (terpri stream))

(defun princln (object &optional (stream *standard-output*))
  (princ object stream)
  (terpri stream))

(defun trim-newlines (string)
  (string-trim (list #\Newline #\Return) string))

(defun merge-lines (line-list)
  (reduce (lambda (a b)
            (concatenate 'string
                         a
                         b))
          (cdr line-list)
          :initial-value (car line-list)))

(defmacro bind (fun &rest args)
  "Bind fun to provided args positionally.
Args named _ remain unbound and should be provided in
subsequent funcall, e.g:
   > (funcall (bind + 1 _ 2 _) 4 5)
   12"
  (let ((new-arglist (list))
        (lambda-arglist (list)))
    (dolist (a args)
      (if (string= (format nil "~A" a) "_")
          (let ((var (gensym)))
            (push var new-arglist)
            (push var lambda-arglist))
          (push a new-arglist)))
    `(lambda (,@(nreverse lambda-arglist))
       (,fun ,@(nreverse new-arglist)))))

(defun find-shortest (objects get-size)
  (when (null objects)
    (return-from find-shortest nil))
  (loop
    :with elem := nil
    :with shortest := (funcall get-size (first objects))
    :for obj :in (cdr objects)
    :while obj
    :for current-size := (funcall get-size obj)
    :if (< current-size shortest)
      :do
         (setf shortest current-size
               elem obj)
    :finally (return elem)))
