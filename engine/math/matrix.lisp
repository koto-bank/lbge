(in-package :lbge.math)

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

(defgeneric mul (matrix value)
  (:documentation "Multiply matrix by a scalar, vector or other matrix"))

(defgeneric div (matrix scalar)
  (:documentation "Divide matrix by a scalar"))

(defgeneric det (matrix)
  (:documentation "Determinant of a matrix"))

(defgeneric transpose (matrix)
  (:documentation "Transpose a matrix"))

(defgeneric absm (matrix)
  (:documentation "Matrix with absolute values of each element"))

(defgeneric negm (matrix)
  (:documentation "Matrix with each element inverted"))

; should probably be a macro
(defmethod get-at ((matrix float2x2) i j)
  (aref (in-list matrix) (+ i (* j 2))))

(defmethod get-at ((matrix float3x3) i j)
  (aref (in-list matrix) (+ i (* j 3))))

(defmethod get-at ((matrix float4x4) i j)
  (aref (in-list matrix) (+ i (* j 4))))

; should probably be a macro
(defmethod set-at ((matrix float2x2) i j v)
  (setf (aref (in-list matrix) (+ i (* j 2))) v))

(defmethod set-at ((matrix float3x3) i j v)
  (setf (aref (in-list matrix) (+ i (* j 3))) v))

(defmethod set-at ((matrix float4x4) i j v)
  (setf (aref (in-list matrix) (+ i (* j 4))) v))


(defmethod add ((matrix1 float2x2) (matrix2 float2x2))
  (make-instance 'float2x2 :in-list
		 (map 'vector
		      (lambda (x y)
			(+ x y))
		      (in-list matrix1)
		      (in-list matrix2))))

(defmethod add ((matrix1 float3x3) (matrix2 float3x3))
  (make-instance 'float3x3 :in-list
		 (map 'vector
		      (lambda (x y)
			(+ x y))
		      (in-list matrix1)
		      (in-list matrix2))))

(defmethod add ((matrix1 float4x4) (matrix2 float4x4))
  (make-instance 'float4x4 :in-list
		 (map 'vector
		      (lambda (x y)
			(+ x y))
		      (in-list matrix1)
		      (in-list matrix2))))


(defmethod sub ((matrix1 float2x2) (matrix2 float2x2))
  (make-instance 'float2x2 :in-list
		 (map 'vector
		      (lambda (x y)
			(- x y))
		      (in-list matrix1)
		      (in-list matrix2))))

(defmethod sub ((matrix1 float3x3) (matrix2 float3x3))
  (make-instance 'float3x3 :in-list
		 (map 'vector
		      (lambda (x y)
			(- x y))
		      (in-list matrix1)
		      (in-list matrix2))))

(defmethod sub ((matrix1 float4x4) (matrix2 float4x4))
  (make-instance 'float4x4 :in-list
		 (map 'vector
		      (lambda (x y)
			(- x y))
		      (in-list matrix1)
		      (in-list matrix2))))


(defmethod mul ((matrix float2x2) (value real))
  (make-instance 'float2x2 :in-list
		 (map 'vector
		      (lambda (x)
			(* x value))
		      (in-list matrix))))

(defmethod mul ((matrix float3x3) (value real))
  (make-instance 'float3x3 :in-list
		 (map 'vector
		      (lambda (x)
			(* x value))
		      (in-list matrix))))

(defmethod mul ((matrix float4x4) (value real))
  (make-instance 'float4x4 :in-list
		 (map 'vector
		      (lambda (x)
			(* x value))
		      (in-list matrix))))


(defmethod div ((matrix float2x2) scalar)
  (make-instance 'float2x2 :in-list
		 (map 'vector
		      (lambda (x)
			(/ x scalar))
		      (in-list matrix))))

(defmethod div ((matrix float3x3) scalar)
  (make-instance 'float3x3 :in-list
		 (map 'vector
		      (lambda (x)
			(/ x scalar))
		      (in-list matrix))))

(defmethod div ((matrix float4x4) scalar)
  (make-instance 'float4x4 :in-list
		 (map 'vector
		      (lambda (x)
			(/ x scalar))
		      (in-list matrix))))

; definitely need to be macros
(defun get-row (matrix j)
  (let ((x (isqrt (length (in-list matrix)))))
    (loop
       for i
       from 0
       below x collect
	 (get-at matrix i j))))

(defun get-col (matrix i)
  (let ((x (isqrt (length (in-list matrix)))))
    (loop
       for j
       from 0
       below x collect
	 (get-at matrix i j))))

; also need to be macros
(defmethod mul ((matrix float2x2) (value float2x2))
  (let ((outm (make-instance 'float2x2)))
    (loop
       for i
       from 0
       to 1 do
	 (loop
	    for j
	    from 0
	    to 1 do
	      (set-at outm i j
		      (reduce #'+
			      (mapcar #'*
				      (get-row matrix i)
				      (get-col value j))))))
    outm))

(defmethod mul ((matrix float3x3) (value float3x3))
  (let ((outm (make-instance 'float3x3)))
    (loop
       for i
       from 0
       to 2 do
	 (loop
	    for j
	    from 0
	    to 2 do
	      (set-at outm i j
		      (reduce #'+
			      (mapcar #'*
				      (get-row matrix i)
				      (get-col value j))))))
    outm))

(defmethod mul ((matrix float4x4) (value float4x4))
  (let ((outm (make-instance 'float2x2)))
    (loop
       for i
       from 0
       to 3 do
	 (loop
	    for j
	    from 0
	    to 3 do
	      (set-at outm i j
		      (reduce #'+
			      (mapcar #'*
				      (get-row matrix i)
				      (get-col value j))))))
    outm))

; probably should be macros
(defmethod mul ((matrix float2x2) (value float2))
  (make-float2
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 1 collect
		(* (get-at matrix 0 i)
		   (x value))))
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 1 collect
		(* (get-at matrix 1 i)
		   (y value))))))

(defmethod mul ((matrix float3x3) (value float3))
  (make-float3
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 2 collect
		(* (get-at matrix 0 i)
		   (x value))))
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 2 collect
		(* (get-at matrix 1 i)
		   (y value))))
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 2 collect
		(* (get-at matrix 2 i)
		   (z value))))))

(defmethod mul ((matrix float4x4) (value float4))
  (make-float4
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 3 collect
		(* (get-at matrix 0 i)
		   (x value))))
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 3 collect
		(* (get-at matrix 1 i)
		   (y value))))
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 3 collect
		(* (get-at matrix 2 i)
		   (z value))))
   (reduce #'+
	   (loop
	      for i
	      from 0
	      to 3 collect
		(* (get-at matrix 3 i)
		   (w value))))))


; small helper functions for determinants, maybe should be macros
(defun det2x2 (a11 a12 a21 a22)
  (- (* a11 a22)
     (* a12 a21)))

(defun det3x3 (a11 a12 a13 a21 a22 a23 a31 a32 a33)
  (- (+ (* a11
	   (det2x2 a22 a23
		   a32 a33))
	(* a13
	   (det2x2 a21 a22
		   a31 a32)))
     (* a12
	(det2x2 a21 a23
		a31 a33))))

(defun det4x4 (a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44)
  (- (+ (* a11
	   (det3x3 a22 a23 a24
		   a32 a33 a34
		   a42 a43 a44))
	(* a13
	   (det3x3 a21 a22 a24
		   a31 a32 a34
		   a41 a42 a44)))
     (+ (* a12
	   (det3x3 a21 a23 a24
		   a31 a33 a34
		   a41 a43 a44))
	(* a14
	   (det3x3 a21 a22 a23
		   a31 a32 a33
		   a41 a42 a43)))))

; should be macros too I think
(defmethod det ((matrix float2x2))
  (det2x2 (get-at matrix 0 0)
	  (get-at matrix 0 1)
	  (get-at matrix 1 0)
	  (get-at matrix 1 1)))

(defmethod det ((matrix float3x3))
  (det3x3 (get-at matrix 0 0)
	  (get-at matrix 0 1)
	  (get-at matrix 0 2)
	  (get-at matrix 1 0)
	  (get-at matrix 1 1)
	  (get-at matrix 1 2)
	  (get-at matrix 2 0)
	  (get-at matrix 2 1)
	  (get-at matrix 2 2)))

(defmethod det ((matrix float4x4))
  (det4x4 (get-at matrix 0 0)
	  (get-at matrix 0 1)
	  (get-at matrix 0 2)
	  (get-at matrix 0 3)
	  (get-at matrix 1 0)
	  (get-at matrix 1 1)
	  (get-at matrix 1 2)
	  (get-at matrix 1 3)
	  (get-at matrix 2 0)
	  (get-at matrix 2 1)
	  (get-at matrix 2 2)
	  (get-at matrix 2 3)
	  (get-at matrix 3 0)
	  (get-at matrix 3 1)
	  (get-at matrix 3 2)
	  (get-at matrix 3 3)))


(defmethod absm ((matrix float2x2))
  (make-instance 'float2x2 :in-list
		 (map 'vector
		      #'abs
		      (in-list matrix))))

(defmethod absm ((matrix float3x3))
  (make-instance 'float3x3 :in-list
		 (map 'vector
		      #'abs
		      (in-list matrix))))

(defmethod absm ((matrix float4x4))
  (make-instance 'float4x4 :in-list
		 (map 'vector
		      #'abs
		      (in-list matrix))))


(defmethod negm ((matrix float2x2))
  (make-instance 'float2x2 :in-list
		 (map 'vector
		      #'-
		      (in-list matrix))))

(defmethod negm ((matrix float3x3))
  (make-instance 'float3x3 :in-list
		 (map 'vector
		      #'-
		      (in-list matrix))))

(defmethod negm ((matrix float4x4))
  (make-instance 'float4x4 :in-list
		 (map 'vector
		      #'-
		      (in-list matrix))))


; helper function for transpose
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))


(defmethod transpose ((matrix float2x2))
  (make-instance 'float2x2 :in-list
		 (make-array '(4) :initial-contents
		      (flatten
		       (loop
			  for i
			  from 0
			  to 1 collect
			    (loop
			       for j
			       from 0
			       to 1 collect
				 (get-at matrix i j)))))))

(defmethod transpose ((matrix float3x3))
  (make-instance 'float3x3 :in-list
		 (make-array '(9) :initial-contents
		      (flatten
		       (loop
			  for i
			  from 0
			  to 2 collect
			    (loop
			       for j
			       from 0
			       to 2 collect
				 (get-at matrix i j)))))))

(defmethod transpose ((matrix float4x4))
  (make-instance 'float4x4 :in-list
		 (make-array '(16) :initial-contents
		      (flatten
		       (loop
			  for i
			  from 0
			  to 3 collect
			    (loop
			       for j
			       from 0
			       to 3 collect
				 (get-at matrix i j)))))))
