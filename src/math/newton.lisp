(in-package :lbge.math)

(defclass newton-interp ()
  ((divdiffs :initarg :divdiffs :accessor divdiffs)
   (xs :initarg :xs :accessor xs)
   (polynomial :accessor poly)))

(defmethod degree ((interp newton-interp))
  (length (xs interp)))

(defun div-diff (xs row start order)
  (let ((y0 (nth (1+ start) row))
        (y1 (nth start row))
        (x0 (nth (+ start order) xs))
        (x1 (nth start xs)))
      (/ (- y0 y1)
         (- x0 x1))))

(defun calc-div-diffs (xs ys)
  (let ((diffs (list ys))
        (len (length ys)))
     (loop for o from 1 below len
      do (append-to diffs
                    (loop for n from 0 below (- len o)
                          collect (div-diff xs (nth (1- o) diffs) n o))))
     diffs))

(defun calc-basis-polynomial (interp order)
  (let ((p (make-polynomial 1)))
       (loop for x in (subseq (xs interp) 0 order)
             do (setf p (mul p (make-polynomial (- x) 1))))
      p))

(defun calc-newton-polynomial (interp)
  (let ((p (make-polynomial 0)))
      (loop for i from 0 below (degree interp)
            do (setf p (add p
                            (mul (calc-basis-polynomial interp i)
                                 (first (nth i (divdiffs interp)))))))
      p))

(defun make-newton-raw (coords)
  (assert (>= (length coords) 3)
          nil
          "There must be at least 3 points to interpolate")
  (let ((xs '())
        (ys '()))
      (loop for c across coords
            do (progn
                (append-to xs (float2-x c))
                (append-to ys (float2-y c))))
      (make-instance 'newton-interp :xs xs :divdiffs (calc-div-diffs xs ys))))

(defun make-newton (coords)
  (let ((newt (make-newton-raw coords)))
      (setf (poly newt) (calc-newton-polynomial newt))
      newt))

(defmethod add-point ((interp newton-interp) (x real) (y real))
  (append-to (xs interp) x)
  (append-to (first (divdiffs interp)) y)
  (append-to (divdiffs interp) '())
  (loop for o from 1 below (degree interp)
        do (append-to (nth o (divdiffs interp))
                      (div-diff (xs interp)
                                (nth (1- o) (divdiffs interp))
                                (- (degree interp) o 1)
                                o)))
  (setf (poly interp) (add (poly interp) (mul (calc-basis-poly interp (1- (degree interp)))
                                              (caar (last (divdiffs interp))))))
  interp)

(defmethod call ((interp newton-interp) (x real))
  (call (poly interp) x))
