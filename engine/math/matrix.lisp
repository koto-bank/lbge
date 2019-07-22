;(in-package :lbge.math)

(defclass float2x2 ()
  ((in-list
    :initarg :in-list
    :accessor in-list
    :initform #(0 0
		0 0))))

(defclass float3x3 ()
  ((in-list
    :initarg :in-list
    :accessor in-list
    :initform #(0 0 0
		  0 0 0
		  0 0 0))))

(defclass float4x4 ()
  ((in-list
    :initarg :in-list
    :accessor in-list
    :initform #(0 0 0 0
		 0 0 0 0
		 0 0 0 0
		 0 0 0 0))))

(defgeneric get-at (matrix i j)
  (:documentation "Get element from the matrix at row i column j starting from (0,0)"))

(defgeneric set-at (matrix i j v)
  (:documentation "Set element to the matrix at row i column j starting from (0,0)"))

(defgeneric add (matrix1 matrix2)
  (:documentation "Add two matrices"))

(defgeneric sub (matrix1 matrix2)
  (:documentation "Subtract two matrices"))

(defgeneric mul (matrix scalar)
  (:documentation "Multiply matrix by a scalar"))

(defgeneric div (matrix scalar)
  (:documentation "Divide matrix by a scalar"))

(defgeneric prod (matrix1 matrix2)
  (:documentation "Product of two matrices"))

(defgeneric vprod (vector matrix)
  (:documentation "Multiply a vector by a matrix"))

(defgeneric det (matrix)
  (:documentation "Determinant of a matrix"))

(defgeneric transpose (matrix)
  (:documentation "Transpose a matrix"))


(defmethod get-at ((matrix float2x2) i j)
  (aref (in-list matrix) (+ i (* j 2))))

(defmethod get-at ((matrix float3x3) i j)
  (aref (in-list matrix) (+ i (* j 3))))

(defmethod get-at ((matrix float4x4) i j)
  (aref (in-list matrix) (+ i (* j 4))))


(defmethod set-at ((matrix float2x2) i j v)
  (setf (aref (in-list matrix) (+ i (* j 2))) v))

(defmethod set-at ((matrix float3x3) i j v)
  (setf (aref (in-list matrix) (+ i (* j 3))) v))

(defmethod set-at ((matrix float4x4) i j v)
  (setf (aref (in-list matrix) (+ i (* j 4))) v))


(defmethod add ((matrix1 float2x2) (matrix2 float2x2))
  (make-instance
   'float2x2
   :in-list (map 'vector
	     (lambda (x y)
	       (+ x y))
	     (in-list matrix1)
	     (in-list matrix2))))

(defmethod add ((matrix1 float3x3) (matrix2 float3x3))
  (make-instance
   'float3x3
   :in-list (map 'vector
	     (lambda (x y)
	       (+ x y))
	     (in-list matrix1)
	     (in-list matrix2))))

(defmethod add ((matrix1 float4x4) (matrix2 float4x4))
  (make-instance
   'float4x4
   :in-list (map 'vector
	     (lambda (x y)
	       (+ x y))
	     (in-list matrix1)
	     (in-list matrix2))))


(defmethod sub ((matrix1 float2x2) (matrix2 float2x2))
  (make-instance
   'float2x2
   :in-list (map 'vector
	     (lambda (x y)
	       (- x y))
	     (in-list matrix1)
	     (in-list matrix2))))

(defmethod sub ((matrix1 float3x3) (matrix2 float3x3))
  (make-instance
   'float3x3
   :in-list (map 'vector
	     (lambda (x y)
	       (- x y))
	     (in-list matrix1)
	     (in-list matrix2))))

(defmethod sub ((matrix1 float4x4) (matrix2 float4x4))
  (make-instance
   'float4x4
   :in-list (map 'vector
	     (lambda (x y)
	       (- x y))
	     (in-list matrix1)
	     (in-list matrix2))))


(defmethod mul ((matrix float2x2) scalar)
  (make-instance
   'float2x2
   :in-list (map 'vector
	     (lambda (x)
	       (* x scalar))
	     (in-list matrix))))

(defmethod mul ((matrix float3x3) scalar)
  (make-instance
   'float3x3
   :in-list (map 'vector
	     (lambda (x)
	       (* x scalar))
	     (in-list matrix))))

(defmethod mul ((matrix float4x4) scalar)
  (make-instance
   'float4x4
   :in-list (map 'vector
	     (lambda (x)
	       (* x scalar))
	     (in-list matrix))))


(defmethod div ((matrix float2x2) scalar)
  (make-instance
   'float2x2
   :in-list (map 'vector
	     (lambda (x)
	       (/ x scalar))
	     (in-list matrix))))

(defmethod div ((matrix float3x3) scalar)
  (make-instance
   'float3x3
   :in-list (map 'vector
	     (lambda (x)
	       (/ x scalar))
	     (in-list matrix))))

(defmethod div ((matrix float4x4) scalar)
  (make-instance
   'float4x4
   :in-list (map 'vector
	     (lambda (x)
	       (/ x scalar))
	     (in-list matrix))))


(defmethod prod ((matrix1 float2x2) (matrix2 float2x2))
  (let ((outm (make-instance 'float2x2)))
    (loop
      for i
      from 0
	to 1 do
	  (loop
	    for j
	    from 0
	      to 1 do (set-at outm i j
			      (reduce #'+
				      (loop
					for k
					from 0
					  to 1 collect (* (get-at matrix1 i k)
							  (get-at matrix2 k j)))))))
    outm))


(defmethod prod ((matrix1 float3x3) (matrix2 float3x3))
  (let ((outm (make-instance 'float3x3)))
    (loop
      for i
      from 0
	to 2 do
	  (loop
	    for j
	    from 0
	      to 2 do (set-at outm i j
			      (reduce #'+
				      (loop
					for k
					from 0
					  to 2 collect (* (get-at matrix1 i k)
							  (get-at matrix2 k j)))))))
    outm))


(defmethod prod ((matrix1 float4x4) (matrix2 float4x4))
  (let ((outm (make-instance 'float4x4)))
    (loop
      for i
      from 0
	to 4 do
	  (loop
	    for j
	    from 0
	      to 4 do (set-at outm i j
			      (reduce #'+
				      (loop
					for k
					from 0
					  to 4 collect (* (get-at matrix1 i k)
							  (get-at matrix2 k j)))))))
    outm))
