(in-package :lbge.math)


(defvar +single-float-min-normal+ 1.175494351e-38)
(defvar +single-float-max-value+ 3.402823466e+38)
;(defvar +double-float-min-normal+ 2.2250738585072014e-308)
;(defvar +double-float-max-value+ 1.7976931348623158e+308)


(defun hand (x y)
  (and x y))


(defun eqfp (x y &optional (eps 0.00001))
  (let ((absX (abs x))
        (absY (abs y))
        (diff (abs (- x y))))

    (if (= x y)
        T
        (if (or (= x 0)
                (= y 0)
                (< (+ absX absY)
                   +single-float-min-normal+))
            (< diff (* eps +single-float-min-normal+))
            (< (/ diff (min (+ absX absY) +single-float-max-value+)) eps)))))


(defun neqfp (x y &optional (eps 0.00001))
  (not (eqfp x y eps)))
