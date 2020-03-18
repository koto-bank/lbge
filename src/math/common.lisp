(in-package :lbge.math)

(defvar +single-float-min-normal+ 1.175494351e-38)
(defvar +single-float-max-value+ 3.402823466e+38)
(defvar +epsilon+ 0.00001)

(defun hand (x y)
  (and x y))

(defun eqfp (x y &optional (eps +epsilon+))
  (let ((abs-x (abs x))
        (abs-y (abs y))
        (diff (abs (- x y))))

    (if (= x y) (return-from eqfp t))
    (if (or (= x 0) (= y 0)
            (< (+ abs-x abs-y)
               +single-float-min-normal+))
        (< diff (* eps +single-float-min-normal+))
        (< (/ diff (min (+ abs-x abs-y) +single-float-max-value+)) eps))))

(defun neqfp (x y &optional (eps +epsilon+))
  (not (eqfp x y eps)))

(defun round-to-eps (float &optional (eps +epsilon+))
  (* eps (floor float eps)))

(defun clamp (val min max)
  (cond ((> val max) max)
        ((< val min) min)
        (t val)))

(defmacro get-sign (n)
  `(if (< ,n 0)
       "-"
       "+"))
