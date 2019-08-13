(in-package :lbge.math)

(defclass float2 ()
  ((in-list
    :initarg :in-list
    :accessor in-list))
  (:default-initargs :in-list #(0 0)))

(defclass float3 ()
  ((in-list
    :initarg :in-list
    :accessor in-list))
  (:default-initargs :in-list #(0 0 0)))

(defclass float4 ()
  ((in-list
    :initarg :in-list
    :accessor in-list))
  (:default-initargs :in-list #(0 0 0 0)))


(defgeneric add (vector1 vector2)
  (:documentation "Add two vector2 to vector1"))

(defgeneric sub (vector1 vector2)
  (:documentation "Subtract vector2 from vector1"))

(defgeneric mul (vector value)
  (:documentation "Multiply vector by a scalar or each respective element"))

(defgeneric div (vector value)
  (:documentation "Divide vector by a scalar or each respective element"))

(defgeneric dot (vector1 vector2)
  (:documentation "Dot product of vector1 and vector2"))

(defgeneric norm (vector)
  (:documentation "The Euclidian norm of a vector"))

(defgeneric normalize (vector)
  (:documentation "Return a normalized vector"))

(defgeneric angle (vector1 vector2)
  (:documentation "Angle between two vectors in radians"))

(defgeneric negate (vector)
  (:documentation "Negate a vector"))

(defgeneric absv (vector)
  (:documentation "Make all elemets of a vector absolute values"))

(defgeneric project (vector1 vector2)
  (:documentation "Projects vector1 onto vector2"))

(defgeneric eqv (vector1 vector2)
  (:documentation "Test two vectors for equality"))

(defgeneric neqv (vector1 vector2)
  (:documentation "Test two vectors for inequality"))

; doesn't work yet
(defgeneric swizzle (vector values &optional signs)
  (:documentation "Perform swizzling on a given vector with optional sign flips"))


(defun x (vec)
  (aref (in-list vec) 0))

(defun y (vec)
  (aref (in-list vec) 1))

(defun z (vec)
  (aref (in-list vec) 2))

(defun w (vec)
  (aref (in-list vec) 3))


(defun make-float2 (a1 &optional a2)
  (if a2
      (make-instance 'float2 :in-list (make-array '(2)
						  :initial-contents (list a1 a2)))
      (make-instance 'float2 :in-list a1)))

(defun make-float3 (a1 &optional a2 a3)
  (if (and a2 a3)
      (make-instance 'float3 :in-list (make-array '(3)
						  :initial-contents (list a1 a2 a3)))
      (make-instance 'float3 :in-list a1)))

(defun make-float4 (a1 &optional a2 a3 a4)
  (if (and a2 a3 a4)
      (make-instance 'float4 :in-list (make-array '(4)
						  :initial-contents (list a1 a2 a3 a4)))
      (make-instance 'float4 :in-list a1)))


(defmethod add ((vector1 float2) vector2)
  (make-float2 (map 'vector #'+
		    (in-list vector1)
		    (in-list vector2))))

(defmethod add ((vector1 float3) vector2)
  (make-float3 (map 'vector #'+
		    (in-list vector1)
		    (in-list vector2))))

(defmethod add ((vector1 float4) vector2)
  (make-float4 (map 'vector #'+
		    (in-list vector1)
		    (in-list vector2))))


(defmethod sub ((vector1 float2) vector2)
  (make-float2 (map 'vector #'-
		    (in-list vector1)
		    (in-list vector2))))

(defmethod sub ((vector1 float3) vector2)
  (make-float3 (map 'vector #'-
		    (in-list vector1)
		    (in-list vector2))))

(defmethod sub ((vector1 float4) vector2)
  (make-float4 (map 'vector #'-
		    (in-list vector1)
		    (in-list vector2))))


(defmethod mul ((vector float2) (value real))
  (make-float2 (map 'vector
		    (lambda (x)
		      (* x value))
		    (in-list vector))))

(defmethod mul ((vector float3) (value real))
  (make-float3 (map 'vector
		    (lambda (x)
		      (* x value))
		    (in-list vector))))

(defmethod mul ((vector float4) (value real))
  (make-float4 (map 'vector
		    (lambda (x)
		      (* x value))
		    (in-list vector))))


(defmethod mul ((vector float2) (value float2))
  (make-float2 (map 'vector #'*
		    (in-list vector)
		    (in-list value))))

(defmethod mul ((vector float3) (value float3))
  (make-float3 (map 'vector #'*
		    (in-list vector)
		    (in-list value))))

(defmethod mul ((vector float4) (value float3))
  (make-float4 (map 'vector #'*
		    (in-list vector)
		    (in-list value))))


(defmethod div ((vector float2) (value real))
  (make-float2 (map 'vector
		    (lambda (x)
		      (/ x value))
		    (in-list vector))))

(defmethod div ((vector float3) (value real))
  (make-float3 (map 'vector
		    (lambda (x)
		      (/ x value))
		    (in-list vector))))

(defmethod div ((vector float4) (value real))
  (make-float4 (map 'vector
		    (lambda (x)
		      (/ x value))
		    (in-list vector))))


(defmethod div ((vector float2) (value float2))
  (make-float2 (map 'vector #'/
		    (in-list vector)
		    (in-list value))))

(defmethod div ((vector float3) (value float3))
  (make-float3 (map 'vector #'/
		    (in-list vector)
		    (in-list value))))

(defmethod div ((vector float4) (value float3))
  (make-float4 (map 'vector #'/
		    (in-list vector)
		    (in-list value))))


(defmethod dot (vector1 vector2)
  (reduce #'+
	  (map 'list #'*
	       (in-list vector1)
	       (in-list vector2))))


(defmethod norm (vector)
  (sqrt (reduce #'+
		(map 'list
		     (lambda (x)
		       (expt x 2))
		     (in-list vector)))))


(defmethod negate ((vector float2))
  (make-float2 (map 'vector #'-
		    (in-list vector))))

(defmethod negate ((vector float2))
  (make-float3 (map 'vector #'-
		    (in-list vector))))

(defmethod negate ((vector float2))
  (make-float4 (map 'vector #'-
		    (in-list vector))))


(defmethod absv ((vector float2))
  (make-float2 (map 'vector #'abs
		    (in-list vector))))


(defmethod absv ((vector float3))
  (make-float3 (map 'vector #'abs
		    (in-list vector))))

(defmethod absv ((vector float4))
  (make-float4 (map 'vector #'abs
		    (in-list vector))))

; a small helper function for eqv
(defun hand (x y)
  (and x y))


(defmethod eqv (vector1 vector2)
  (reduce #'hand
	  (map 'vector #'=
	       (in-list vector1)
	       (in-list vector2))))


(defmethod neqv (vector1 vector2)
  (not (eqv vector1 vector2)))


(defun cross (vector1 vector2)
  (make-float3 (- (* (y vector1) (z vector2))
		  (* (z vector1) (y vector2)))
	       (- (* (z vector1) (x vector2))
		  (* (x vector1) (z vector2)))
	       (- (* (x vector1) (y vector2))
		  (* (y vector1) (x vector2)))))


(defmethod normalize (vector)
  (div vector (norm vector)))


(defmethod angle (vector1 vector2)
  (acos (/ (dot vector1 vector2)
	   (* (norm vector1)
	      (norm vector2)))))


(defmethod project (vector1 vector2)
  (dot vector1 (normalize vector2)))

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

(defun float2-x () (make-float2 1 0))
(defun float2-y () (make-float2 0 1))

(defun float3-x () (make-float3 1 0 0))
(defun float3-y () (make-float3 0 1 0))
(defun float3-z () (make-float3 0 0 1))

(defun float4-x () (make-float4 1 0 0 0))
(defun float4-y () (make-float4 0 1 0 0))
(defun float4-z () (make-float4 0 0 1 0))
(defun float4-w () (make-float4 0 0 0 1))
