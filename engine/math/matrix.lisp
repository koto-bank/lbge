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

(defgeneric eqm (matrix1 matrix2)
  (:documentation "Test two matrices for equality"))

(defgeneric neqm (matrix1 matrix2)
  (:documentation "Test two matrices for inequality"))


(defun make-float2x2 (a00 &optional a01 a10 a11)
  (if a11
      (make-instance 'float2x2
                     :in-list (make-array '(4) :initial-contents
                                          (list a00 a01
                                                a10 a11)))
      (make-instance 'float2x2
                     :in-list a00)))

(defun make-float3x3 (a00 &optional a01 a02 a10 a11 a12 a20 a21 a22)
  (if a22
      (make-instance 'float3x3
                     :in-list (make-array '(9) :initial-contents
                                          (list a00 a01 a02
                                                a10 a11 a12
                                                a20 a21 a22)))
      (make-instance 'float3x3
                     :in-list a00)))

(defun make-float4x4 (a00 &optional a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33)
  (if a33
      (make-instance 'float4x4
                     :in-list (make-array '(16) :initial-contents
                                          (list a00 a01 a02 a03
                                                a10 a11 a12 a13
                                                a20 a21 a22 a23
                                                a30 a31 a32 a33)))
      (make-instance 'float4x4
                        :in-list a00)))


(defun float2x2-zero () (make-instance 'float2x2))
(defun float3x3-zero () (make-instance 'float3x3))
(defun float4x4-zero () (make-instance 'float4x4))

(defun float2x2-one () (make-float2x2 1 1
                                      1 1))

(defun float3x3-one () (make-float3x3 1 1 1
                                      1 1 1
                                      1 1 1))

(defun float4x4-one () (make-float4x4 1 1 1 1
                                      1 1 1 1
                                      1 1 1 1
                                      1 1 1 1))


(defun float2x2-iden () (make-float2x2 1 0
                                       0 1))

(defun float3x3-iden () (make-float3x3 1 0 0
                                       0 1 0
                                       0 0 1))

(defun float4x4-iden () (make-float4x4 1 0 0 0
                                       0 1 0 0
                                       0 0 1 0
                                       0 0 0 1))


(defmacro mat-size (matrix)
  `(isqrt (length (in-list ,matrix))))

(defmacro get-at (matrix i j)
  `(aref (in-list ,matrix)
        (+ ,j (* ,i (mat-size ,matrix)))))

(defmacro set-at (matrix i j v)
  `(setf (aref (in-list ,matrix)
          (+ ,j (* ,i (mat-size ,matrix))))
         ,v))


(defmethod add ((matrix1 float2x2) (matrix2 float2x2))
  (make-float2x2
   (map 'vector #'+
        (in-list matrix1)
        (in-list matrix2))))

(defmethod add ((matrix1 float3x3) (matrix2 float3x3))
  (make-float3x3
   (map 'vector #'+
        (in-list matrix1)
        (in-list matrix2))))

(defmethod add ((matrix1 float4x4) (matrix2 float4x4))
  (make-float4x4
   (map 'vector #'+
        (in-list matrix1)
        (in-list matrix2))))


(defmethod sub ((matrix1 float2x2) (matrix2 float2x2))
  (make-float2x2
   (map 'vector #'-
        (in-list matrix1)
        (in-list matrix2))))

(defmethod sub ((matrix1 float3x3) (matrix2 float3x3))
  (make-float3x3
   (map 'vector #'-
        (in-list matrix1)
        (in-list matrix2))))

(defmethod sub ((matrix1 float4x4) (matrix2 float4x4))
  (make-float4x4
   (map 'vector #'-
        (in-list matrix1)
        (in-list matrix2))))


(defmethod mul ((matrix float2x2) (value real))
  (make-float2x2
   (map 'vector
        (lambda (x)
          (* x value))
        (in-list matrix))))

(defmethod mul ((matrix float3x3) (value real))
  (make-float3x3
   (map 'vector
        (lambda (x)
          (* x value))
        (in-list matrix))))

(defmethod mul ((matrix float4x4) (value real))
  (make-float4x4
   (map 'vector
        (lambda (x)
          (* x value))
        (in-list matrix))))


(defmethod div ((matrix float2x2) scalar)
  (make-float2x2
   (map 'vector
        (lambda (x)
          (/ x scalar))
        (in-list matrix))))

(defmethod div ((matrix float3x3) scalar)
  (make-float3x3
   (map 'vector
        (lambda (x)
          (/ x scalar))
        (in-list matrix))))

(defmethod div ((matrix float4x4) scalar)
  (make-float4x4
   (map 'vector
        (lambda (x)
          (/ x scalar))
        (in-list matrix))))


(defmacro get-row (matrix j)
  `(loop for i from 0 below (mat-size ,matrix) collect
        (get-at ,matrix i ,j)))

(defmacro get-col (matrix i)
  `(loop for j from 0 below (mat-size ,matrix) collect
        (get-at ,matrix ,i j)))


(defmethod mul ((matrix float2x2) (value float2x2))
  (let ((outm (float2x2-zero)))
    (dotimes (i 2)
      (dotimes (j 2)
       (set-at outm i j
               (reduce #'+
                       (mapcar #'*
                               (get-col matrix i)
                               (get-row value j))))))
    outm))

(defmethod mul ((matrix float3x3) (value float3x3))
  (let ((outm (float3x3-zero)))
    (dotimes (i 3)
      (dotimes (j 3)
        (set-at outm i j
                (reduce #'+
                        (mapcar #'*
                                (get-col matrix i)
                                (get-row value j))))))
    outm))

(defmethod mul ((matrix float4x4) (value float4x4))
  (let ((outm (float4x4-zero)))
    (dotimes (i 4)
      (dotimes (j 4)
        (set-at outm i j
                (reduce #'+
                        (mapcar #'*
                                (get-col matrix i)
                                (get-row value j))))))
    outm))


(defmethod mul ((matrix float2x2) (value float2))
  (make-float2
   (reduce #'+
           (loop for i from 0 to 1 collect
                 (* (get-at matrix 0 i)
                    (x value))))
   (reduce #'+
           (loop for i from 0 to 1 collect
                 (* (get-at matrix 1 i)
                    (y value))))))

(defmethod mul ((matrix float3x3) (value float3))
  (make-float3
   (reduce #'+
           (loop for i from 0 to 2 collect
                 (* (get-at matrix 0 i)
                    (x value))))
   (reduce #'+
           (loop for i from 0 to 2 collect
                 (* (get-at matrix 1 i)
                    (y value))))
   (reduce #'+
           (loop for i from 0 to 2 collect
                 (* (get-at matrix 2 i)
                    (z value))))))

(defmethod mul ((matrix float4x4) (value float4))
  (make-float4
   (reduce #'+
           (loop for i from 0 to 3 collect
                 (* (get-at matrix 0 i)
                    (x value))))
   (reduce #'+
           (loop for i from 0 to 3 collect
                 (* (get-at matrix 1 i)
                    (y value))))
   (reduce #'+
           (loop for i from 0 to 3 collect
                 (* (get-at matrix 2 i)
                    (z value))))
   (reduce #'+
           (loop for i from 0 to 3 collect
                 (* (get-at matrix 3 i)
                    (w value))))))


(defmacro det2x2 (a11 a12 a21 a22)
  `(- (* ,a11 ,a22)
     (* ,a12 ,a21)))

(defmacro det3x3 (a11 a12 a13 a21 a22 a23 a31 a32 a33)
  `(- (+ (* ,a11
           (det2x2 ,a22 ,a23
                   ,a32 ,a33))
        (* ,a13
           (det2x2 ,a21 ,a22
                   ,a31 ,a32)))
     (* ,a12
        (det2x2 ,a21 ,a23
                ,a31 ,a33))))

(defmacro det4x4 (a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44)
  `(- (+ (* ,a11
           (det3x3 ,a22 ,a23 ,a24
                   ,a32 ,a33 ,a34
                   ,a42 ,a43 ,a44))
        (* ,a13
           (det3x3 ,a21 ,a22 ,a24
                   ,a31 ,a32 ,a34
                   ,a41 ,a42 ,a44)))
     (+ (* ,a12
           (det3x3 ,a21 ,a23 ,a24
                   ,a31 ,a33 ,a34
                   ,a41 ,a43 ,a44))
        (* ,a14
           (det3x3 ,a21 ,a22 ,a23
                   ,a31 ,a32 ,a33
                   ,a41 ,a42 ,a43)))))

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
  (make-float2x2
   (map 'vector #'abs
        (in-list matrix))))

(defmethod absm ((matrix float3x3))
  (make-float3x3
   (map 'vector #'abs
        (in-list matrix))))

(defmethod absm ((matrix float4x4))
  (make-float4x4
   (map 'vector #'abs
        (in-list matrix))))


(defmethod negm ((matrix float2x2))
  (make-float2x2
   (map 'vector #'-
        (in-list matrix))))

(defmethod negm ((matrix float3x3))
  (make-float3x3
   (map 'vector #'-
        (in-list matrix))))

(defmethod negm ((matrix float4x4))
  (make-float4x4
   (map 'vector #'-
        (in-list matrix))))


(defmethod transpose ((matrix float2x2))
  (make-float2x2
   (make-array '(4) :initial-contents
               (list (get-at matrix 0 0) (get-at matrix 1 0)
                     (get-at matrix 0 1) (get-at matrix 1 1)))))

(defmethod transpose ((matrix float3x3))
  (make-float3x3
   (make-array '(9) :initial-contents
               (list (get-at matrix 0 0) (get-at matrix 1 0) (get-at matrix 2 0)
                     (get-at matrix 0 1) (get-at matrix 1 1) (get-at matrix 2 1)
                     (get-at matrix 0 2) (get-at matrix 1 2) (get-at matrix 2 2)))))

(defmethod transpose ((matrix float4x4))
  (make-float4x4
   (make-array '(16) :initial-contents
               (list (get-at matrix 0 0) (get-at matrix 1 0) (get-at matrix 2 0) (get-at matrix 3 0)
                     (get-at matrix 0 1) (get-at matrix 1 1) (get-at matrix 2 1) (get-at matrix 3 1)
                     (get-at matrix 0 2) (get-at matrix 1 2) (get-at matrix 2 2) (get-at matrix 3 2)
                     (get-at matrix 0 3) (get-at matrix 1 3) (get-at matrix 2 3) (get-at matrix 3 3)))))


(defmethod eqm (matrix1 matrix2)
  (reduce #'hand
          (map 'list #'eqfp
               (in-list matrix1)
               (in-list matrix2))))


(defmethod neqm (matrix1 matrix2)
  (not (eqm matrix1 matrix2)))

(defun make-ortho-projection (&keyword left right top bottom near far)
  (let ((l left)
        (r right)
        (tp top)
        (b bottom)
        (n near)
        (f far))
    (make-float4x4 (/ 2 (- r l)) 0 0 (- (/ (+ r l) (- r l)))
                   0 (/ 2 (- tp b)) 0 (- (/ (+ tp b) (- tp b)))
                   0 0 (/ -2 (- n f)) (- (/ (+ f n) (- f n)))
                   0 0 0 1)))
