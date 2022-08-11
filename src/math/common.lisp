(in-package :lbge.math)

(defvar +single-float-min-normal+ 1.175494351e-38)
(defvar *epsilon* 0.00001)

(defun eqfp (x y &optional (eps *epsilon*))
  (let ((abs-x (abs x))
        (abs-y (abs y))
        (diff (abs (- x y))))
    (when (= x y) (return-from eqfp t))
    (if (or (= x 0) (= y 0)
            (< (+ abs-x abs-y)
               +single-float-min-normal+))
        (< diff (* eps +single-float-min-normal+))
        (< (/ diff (min (+ abs-x abs-y) most-positive-single-float)) eps))))

(defun neqfp (x y &optional (eps *epsilon*))
  (not (eqfp x y eps)))

(defun round-to-eps (float &optional (eps *epsilon*))
  (* eps (floor float eps)))

(defun clamp (val min max)
  (cond ((> val max) max)
        ((< val min) min)
        (t val)))

(defmacro get-sign (n)
  `(if (< ,n 0)
       "-"
       "+"))

(defmacro append-to (l &rest v)
  `(setf ,l (append ,l (list ,@v))))

(defmacro macro-curry (func val &key (right nil))
  `(lambda (x) ,(if right `(,func x ,val) `(,func ,val x))))
