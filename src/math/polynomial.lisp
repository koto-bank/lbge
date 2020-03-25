(in-package :lbge.math)

(defclass polynomial ()
  ((coefficients
    :initarg :coeffs
    :accessor coeffs)))

(defun make-polynomial (a &rest as)
  (if (vectorp a)
      (make-instance 'polynomial :coeffs a)
      (make-instance 'polynomial :coeffs (concatenate 'vector (list a) as))))

(defmethod degree ((poly polynomial))
  (1- (length (coeffs poly))))

(defmethod print-object ((poly polynomial) out)
  (format out "~S" (coeffs poly)))

(defun raise-degree (poly degrees &optional (value 0.0f0))
  (if (<= degrees 0)
      poly
      (make-polynomial (concatenate 'vector
                                    (make-array (list degrees)
                                                :initial-element value)
                                    (coeffs poly)))))

(defun pad-poly (poly pad-by &optional (value 0.0f0))
  (if (<= pad-by 0)
      poly
      (make-polynomial (concatenate 'vector
                                    (coeffs poly)
                                    (make-array (list pad-by)
                                                :initial-element value)))))


(defmacro define-poly-op (name op)
  `(defmethod ,name ((poly1 polynomial) (poly2 polynomial))
     (let* ((deg1 (length (coeffs poly1)))
            (deg2 (length (coeffs poly2)))
            (maxdeg (max deg1 deg2))
            (np1 (pad-poly poly1 (- maxdeg deg1)))
            (np2 (pad-poly poly2 (- maxdeg deg2))))
       (make-polynomial (map 'vector ,op
                             (coeffs np1)
                             (coeffs np2))))))

(defmacro define-poly-num-strord-op (name map-fun)
  `(defmethod ,name ((poly polynomial) (value real))
     (make-instance 'polynomial :coeffs
                    (map 'vector (ax:rcurry ,map-fun value)
                         (coeffs poly)))))

(defmacro define-poly-num-revord-op (name map-fun)
  `(defmethod ,name ((value real) (poly polynomial))
     (make-instance 'polynomial :coeffs
                    (map 'vector (ax:curry ,map-fun value)
                         (coeffs poly)))))


(defmacro define-poly-num-op (name map-fun)
  `(progn
    (define-poly-num-strord-op ,name ,map-fun)
    (define-poly-num-revord-op ,name ,map-fun)))

(defmacro define-poly-unary-op (name map-fun)
  `(defmethod ,name ((poly polynomial))
     (make-instance 'polynomial :coeffs
                    (map 'vector ,map-fun
                         (coeffs poly)))))

(define-poly-op add #'+)
(define-poly-op sub #'-)
(define-poly-num-op mul #'*)
(define-poly-num-op div #'/)
(define-poly-unary-op negp #'-)
(define-poly-unary-op absp #'abs)

(defmethod mul ((poly1 polynomial) (poly2 polynomial))
  (let ((deg (length (coeffs poly1))))
    (reduce #'add
      (loop for c across (coeffs poly1)
            for n in (alexandria:iota deg)
            collect (pad-poly (mul (raise-degree poly2 n)
                                   c)
                              (- deg n 1))))))

(defmethod call ((poly polynomial) (x real))
  (reduce #'+
    (loop for c across (coeffs poly)
          for n in (alexandria:iota (length (coeffs poly)))
          collect (* c (expt x n)))))

(defun eqp (poly1 poly2 &optional (eps +epsilon+))
  "Test two polynomial for equality"
  (if (/= (length (coeffs poly1))
          (length (coeffs poly2)))
      nil
      (reduce #'hand
             (map 'vector (ax:rcurry #'eqfp eps)
                  (coeffs poly1)
                  (coeffs poly2)))))

(defun neqp (poly1 poly2 &optional (eps +epsilon+))
  (not (eqp poly1 poly2 eps)))
