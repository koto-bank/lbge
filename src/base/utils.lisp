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
  (when (and (listp fun)
             (eq 'function (car fun)))
    (setf fun (cadr fun)))
  (let ((lambda-arglist (list)))
    (labels ((fill-arg-list (args arg-list)
               (cond
                 ((null args) arg-list)
                 ((atom (car args))
                  (let ((arg (car args)))
                    (if (string= (format nil "~A" arg) "_")
                      (let ((var (gensym)))
                        (push var arg-list)
                        (push var lambda-arglist))
                      (push arg arg-list)))
                  (fill-arg-list (cdr args) arg-list))
                 ((listp (car args))
                  (push (nreverse (fill-arg-list (car args) (list))) arg-list)
                  (fill-arg-list (cdr args) arg-list)))))
      (let ((new-arglist (fill-arg-list args (list))))
        `(lambda (,@(nreverse lambda-arglist))
           (,fun ,@(nreverse new-arglist)))))))

(defun find-shortest (objects get-size)
  (when (null objects)
    (return-from find-shortest nil))
  (when (= 1 (length objects))
    (return-from find-shortest (car objects)))
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

(defmacro ~ (object &rest slots)
  "Shorthand for slot access: (~ object slot)"
  (reduce (lambda (acc elem) `(slot-value ,acc ',elem))
          slots
          :initial-value object))

(defun metaclass-of (object)
  (class-of (class-of object)))
