(in-package :lbge.math)

(defclass polynomial ()
  ((coefficients
    :initarg :coefs
    :accessor coefs)))

(defun make-polynomial (a &rest as)
  (if as
      (make-instance 'polynomial :coefs (concatenate 'vector (list a) as))
      (make-instance 'polynomial :coefs a)))


(defmethod print-object ((poly polynomial) out)
  (format out "~S" (coefs poly)))


(defun raise-degree (poly degrees &optional (value 0.0f0))
  (if (<= degrees 0)
      poly
      (make-polynomial (concatenate 'vector
                                    (make-array (list degrees)
                                                :initial-element value)
                                    (coefs poly)))))

(defun pad-poly (poly pad-by &optional (value 0.0f0))
  (if (<= pad-by 0)
      poly
      (make-polynomial (concatenate 'vector
                                    (coefs poly)
                                    (make-array (list pad-by)
                                                :initial-element value)))))


(defmacro define-poly-op (name op)
  `(defmethod ,name ((poly1 polynomial) (poly2 polynomial))
     (let* ((deg1 (length (coefs poly1)))
            (deg2 (length (coefs poly2)))
            (diff1 (- deg1 deg2))
            (diff2 (- diff1))
            (np1 (pad-poly poly1 diff1))
            (np2 (pad-poly poly2 diff2)))
       (make-polynomial (map 'vector ,op
                             (coefs np1)
                             (coefs np2))))))

(defmacro define-poly-num-op (name map-fun)
  `(defmethod ,name ((poly polynomial) (value real))
     (make-instance 'polynomial :coefs
                    (map 'vector (ax:rcurry ,map-fun value)
                         (coefs poly)))))

(defmacro define-poly-num-revord-op (name map-fun)
  `(defmethod ,name ((value real) (poly polynomial))
     (make-instance 'polynomial :coefs
                    (map 'vector (ax:curry ,map-fun value)
                         (coefs poly)))))

(defmacro define-poly-unary-op (name map-fun)
  `(defmethod ,name ((poly polynomial))
     (make-instance 'polynomial :coefs
                    (map 'vector ,map-fun
                         (coefs poly)))))

(define-poly-op add #'+)
(define-poly-op sub #'-)
(define-poly-num-op mul #'*)
(define-poly-num-op div #'/)
(define-poly-num-revord-op mul #'*)
(define-poly-num-revord-op div #'/)
(define-poly-unary-op negp #'-)
(define-poly-unary-op absp #'abs)

(defmethod mul ((poly1 polynomial) (poly2 polynomial))
  (let ((deg (length (coefs poly1))))
    (reduce #'add
      (loop for c across (coefs poly1)
            for n in (alexandria:iota deg)
            collect (pad-poly (mul (raise-degree poly2 n)
                                   c)
                              (- deg n 1))))))

(defun call-at (poly x)
  (reduce #'+
    (loop for c across (coefs poly)
          for n in (alexandria:iota (length (coefs poly)))
          collect (* c (expt x n)))))

(defun eqp (poly1 poly2)
  "Test two polynomial for equality"
  (if (/= (length (coefs poly1))
          (length (coefs poly2)))
      nil
      (reduce #'hand
             (map 'vector #'eqfp
                  (in-vec poly1)
                  (in-vec poly2)))))

(defun neqp (poly1 poly2)
  (not (eqp poly1 poly2)))
