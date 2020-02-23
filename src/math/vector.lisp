(in-package :lbge.math)

(defclass float2 ()
  ((in-vec
    :initarg :in-vec
    :accessor in-vec))
  (:default-initargs :in-vec #(0 0)))

(defmacro float2-x (vec)
  `(aref (in-vec ,vec) 0))

(defmacro float2-y (vec)
  `(aref (in-vec ,vec) 1))

(defclass float3 ()
  ((in-vec
    :initarg :in-vec
    :accessor in-vec))
  (:default-initargs :in-vec #(0 0 0)))

(defmacro float3-x (vec)
  `(aref (in-vec ,vec) 0))

(defmacro float3-y (vec)
  `(aref (in-vec ,vec) 1))

(defmacro float3-z (vec)
  `(aref (in-vec ,vec) 2))

(defclass float4 ()
  ((in-vec
    :initarg :in-vec
    :accessor in-vec))
  (:default-initargs :in-vec #(0 0 0 0)))

(defmacro float4-x (vec)
  `(aref (in-vec ,vec) 0))

(defmacro float4-y (vec)
  `(aref (in-vec ,vec) 1))

(defmacro float4-z (vec)
  `(aref (in-vec ,vec) 2))

(defmacro float4-w (vec)
  `(aref (in-vec ,vec) 3))

(defgeneric get-size (vector)
  (:documentation "Return number of vector components"))

(defgeneric add (vector1 vector2)
  (:documentation "Add two vector2 to vector1"))

(defgeneric sub (vector1 vector2)
  (:documentation "Subtract vector2 from vector1"))

(defgeneric mul (vector value)
  (:documentation "Multiply vector by a scalar or each respective element"))

(defgeneric div (vector value)
  (:documentation "Divide vector by a scalar or each respective element"))

(defgeneric negv (vector)
  (:documentation "Negate a vector"))

(defgeneric absv (vector)
  (:documentation "Make all elemets of a vector absolute values"))

; doesn't work yet
(defgeneric swizzle (vector values &optional signs)
  (:documentation "Perform swizzling on a given vector with optional sign flips"))


(defun x (vec)
  (aref (in-vec vec) 0))

(defun y (vec)
  (aref (in-vec vec) 1))

(defun z (vec)
  (aref (in-vec vec) 2))

(defun w (vec)
  (aref (in-vec vec) 3))


(defun make-float2 (&optional a1 a2)
  (cond ((null a1)
         (make-instance
          'float2
          :in-vec #(0.0f0 0.0f0)))
        ((and a1 a2)
         (make-instance
          'float2
          :in-vec (vector a1 a2)))
        (a1 (make-instance 'float2 :in-vec (vector a1 a1)))))

(defun make-float3 (&optional a1 a2 a3)
  (cond
    ((null a1)
     (make-instance
      'float3
      :in-vec #(0.0f0 0.0f0 0.0f0)))
    ((and a2 a3)
     (make-instance
      'float3
      :in-vec (vector a1 a2 a3)))
    (a1 (make-instance 'float3 :in-vec (vector a1 a1 a1)))))

(defun make-float4 (&optional a1 a2 a3 a4)
  (let (in-vec)
    (cond
      ((null a1)
       (setf in-vec (vector 0.0f0 0.0f0 0.0f0 0.0f0)))
      ((and a1 a2 a3 a4)
       (setf in-vec (vector a1 a2 a3 a4)))
      (a1
       (setf in-vec (vector a1 a1 a1 a1))))
    (make-instance 'float4 :in-vec
                   (map 'vector (ax:rcurry #'coerce 'single-float)
                        in-vec))))

(defmethod print-object ((vec float4) out)
  (format out "~S" (in-vec vec)))

(defmethod print-object ((vec float3) out)
  (format out "~S" (in-vec vec)))

(defmethod get-size ((vec float2))
  2)

(defmethod get-size ((vec float3))
  3)

(defmethod get-size ((vec float4))
  4)

(defmacro define-vec-op (name result-type map-op)
  `(defmethod ,name ((vector1 ,result-type) vector2)
     (make-instance ',result-type :in-vec
                    (map 'vector ,map-op
                         (in-vec vector1)
                         (in-vec vector2)))))

(define-vec-op add float2 #'+)
(define-vec-op add float3 #'+)
(define-vec-op add float4 #'+)

(define-vec-op sub float2 #'-)
(define-vec-op sub float3 #'-)
(define-vec-op sub float4 #'-)

(define-vec-op mul float2 #'*)
(define-vec-op mul float3 #'*)
(define-vec-op mul float4 #'*)

(define-vec-op div float2 #'/)
(define-vec-op div float3 #'/)
(define-vec-op div float4 #'/)

(defmacro define-vec-num-op (name vec-type map-fun)
  `(defmethod ,name ((vector ,vec-type) (value real))
     (make-instance ',vec-type :in-vec
                    (map 'vector (ax:rcurry ,map-fun value)
                         (in-vec vector)))))

(define-vec-num-op mul float2 #'*)
(define-vec-num-op mul float3 #'*)
(define-vec-num-op mul float4 #'*)

(define-vec-num-op div float2 #'/)
(define-vec-num-op div float3 #'/)
(define-vec-num-op div float4 #'/)

(defun dot (vector1 vector2)
  "Dot product of vector1 and vector2"
  (reduce #'+
          (map 'vector #'*
               (in-vec vector1)
               (in-vec vector2))))

(defun norm (vector)
  "The Euclidian norm of a vector"
  (sqrt (reduce #'+
                (map 'vector
                     (ax:rcurry #'expt 2)
                     (in-vec vector)))))

(defmacro define-vec-unary-op (name vec-type map-fun)
  `(defmethod ,name ((vector ,vec-type))
     (make-instance ',vec-type :in-vec
                    (map 'vector ,map-fun
                         (in-vec vector)))))

(define-vec-unary-op negv float2 #'-)
(define-vec-unary-op negv float3 #'-)
(define-vec-unary-op negv float4 #'-)

(define-vec-unary-op absv float2 #'abs)
(define-vec-unary-op absv float3 #'abs)
(define-vec-unary-op absv float4 #'abs)

(defun eqv (vector1 vector2)
  "Test two vectors for equality"
  (reduce #'hand
          (map 'vector #'eqfp
               (in-vec vector1)
               (in-vec vector2))))


(defun neqv (vector1 vector2)
  "Test two vectors for inequality"
  (not (eqv vector1 vector2)))


(defun cross (vector1 vector2)
  (make-float3 (- (* (y vector1) (z vector2))
                  (* (z vector1) (y vector2)))
               (- (* (z vector1) (x vector2))
                  (* (x vector1) (z vector2)))
               (- (* (x vector1) (y vector2))
                  (* (y vector1) (x vector2)))))


(defun normalize (vector)
  "Return a normalized vector"
  (div vector (norm vector)))

(defun angle (vector1 vector2)
  "Angle between two vectors in radians"
  (acos (/ (dot vector1 vector2)
           (* (norm vector1)
              (norm vector2)))))

(defun project (vector1 vector2)
  "Return the projection of vector1 onto vector2"
  (mul (normalize vector2) (dot vector1 (normalize vector2))))

; doesnt work yet
(defmethod swizzle ((vector float2) values &optional signs)
    (with-slots (x y) vector
    (map nil
     (lambda (c s)
       (ecase c
         (#\X (funcall (intern (string s)) x))
         (#\Y (funcall (intern (string s)) y))))
     (symbol-name values)
     (if (null signs)
         "++"
         (symbol-name signs)))))


(defun float2-zero () (make-float2 0 0))
(defun float3-zero () (make-float3 0 0 0))
(defun float4-zero () (make-float4 0 0 0 0))

(defun float2-one () (make-float2 1 1))
(defun float3-one () (make-float3 1 1 1))
(defun float4-one () (make-float4 1 1 1 1))
