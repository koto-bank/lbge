(in-package :lbge.math)

(defclass floatnxn ()
  ((vector
    :initarg :in-vec
    :accessor in-vec)))

(defclass float2x2 (floatnxn)
  ((vector
    :initarg :in-vec
    :accessor in-vec
    :initform #(0 0
                0 0))))

(defclass float3x3 (floatnxn)
  ((vector
    :initarg :in-vec
    :accessor in-vec
    :initform #(0 0 0
                0 0 0
                0 0 0))))

(defclass float4x4 (floatnxn)
  ((vector
    :initarg :in-vec
    :accessor in-vec
    :initform #(0 0 0 0
                0 0 0 0
                0 0 0 0
                0 0 0 0))))

(defmethod print-object ((mat float4x4) stream)
  (loop
    :for (a b c d) :on (coerce (in-vec mat) 'list)
    :by #'cddddr
    :do (format stream "~A ~A ~A ~A~%" a b c d)))

(defun make-float2x2 (a00 &optional a01 a10 a11)
  (if a11
      (make-instance 'float2x2
                     :in-vec (make-array '(4) :initial-contents
                                         (vector a00 a01
                                                 a10 a11)))
      (make-instance 'float2x2
                     :in-vec a00)))

(defun make-float3x3 (a00 &optional a01 a02 a10 a11 a12 a20 a21 a22)
  (if a22
      (make-instance 'float3x3
                     :in-vec (make-array '(9) :initial-contents
                                         (vector a00 a01 a02
                                                 a10 a11 a12
                                                 a20 a21 a22)))
      (make-instance 'float3x3
                     :in-vec a00)))

(defun make-float4x4 (a00 &optional a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33)
  (if a33
      (make-instance 'float4x4
                     :in-vec (make-array '(16) :initial-contents
                                         (vector a00 a01 a02 a03
                                                 a10 a11 a12 a13
                                                 a20 a21 a22 a23
                                                 a30 a31 a32 a33)))
      (make-instance 'float4x4
                     :in-vec a00)))


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
  `(isqrt (length (in-vec ,matrix))))

(defmacro get-at (matrix i j)
  `(aref (in-vec ,matrix)
        (+ ,j (* ,i (mat-size ,matrix)))))

(defmacro set-at (matrix i j v)
  `(setf (aref (in-vec ,matrix)
          (+ ,j (* ,i (mat-size ,matrix))))
         ,v))


(defmethod add ((matrix1 float2x2) (matrix2 float2x2))
  (make-float2x2
   (map 'vector #'+
        (in-vec matrix1)
        (in-vec matrix2))))

(defmethod add ((matrix1 float3x3) (matrix2 float3x3))
  (make-float3x3
   (map 'vector #'+
        (in-vec matrix1)
        (in-vec matrix2))))

(defmethod add ((matrix1 float4x4) (matrix2 float4x4))
  (make-float4x4
   (map 'vector #'+
        (in-vec matrix1)
        (in-vec matrix2))))


(defmethod sub ((matrix1 float2x2) (matrix2 float2x2))
  (make-float2x2
   (map 'vector #'-
        (in-vec matrix1)
        (in-vec matrix2))))

(defmethod sub ((matrix1 float3x3) (matrix2 float3x3))
  (make-float3x3
   (map 'vector #'-
        (in-vec matrix1)
        (in-vec matrix2))))

(defmethod sub ((matrix1 float4x4) (matrix2 float4x4))
  (make-float4x4
   (map 'vector #'-
        (in-vec matrix1)
        (in-vec matrix2))))


(defmethod mul ((matrix float2x2) (value real))
  (make-float2x2
   (map 'vector
        (ax:curry #'* value)
        (in-vec matrix))))

(defmethod mul ((matrix float3x3) (value real))
  (make-float3x3
   (map 'vector
        (ax:curry #'* value)
        (in-vec matrix))))

(defmethod mul ((matrix float4x4) (value real))
  (make-float4x4
   (map 'vector
        (ax:curry #'* value)
        (in-vec matrix))))


(defmethod div ((matrix float2x2) scalar)
  (make-float2x2
   (map 'vector
        (ax:rcurry #'/ scalar)
        (in-vec matrix))))

(defmethod div ((matrix float3x3) scalar)
  (make-float3x3
   (map 'vector
        (ax:rcurry #'/ scalar)
        (in-vec matrix))))

(defmethod div ((matrix float4x4) scalar)
  (make-float4x4
   (map 'vector
        (ax:rcurry #'/ scalar)
        (in-vec matrix))))


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
        (in-vec matrix))))

(defmethod absm ((matrix float3x3))
  (make-float3x3
   (map 'vector #'abs
        (in-vec matrix))))

(defmethod absm ((matrix float4x4))
  (make-float4x4
   (map 'vector #'abs
        (in-vec matrix))))


(defmethod negm ((matrix float2x2))
  (make-float2x2
   (map 'vector #'-
        (in-vec matrix))))

(defmethod negm ((matrix float3x3))
  (make-float3x3
   (map 'vector #'-
        (in-vec matrix))))

(defmethod negm ((matrix float4x4))
  (make-float4x4
   (map 'vector #'-
        (in-vec matrix))))


(defmethod transpose ((matrix float2x2))
  (make-float2x2
   (make-array '(4) :initial-contents
               (vector (get-at matrix 0 0) (get-at matrix 1 0)
                       (get-at matrix 0 1) (get-at matrix 1 1)))))

(defmethod transpose ((matrix float3x3))
  (make-float3x3
   (make-array '(9) :initial-contents
               (vector (get-at matrix 0 0) (get-at matrix 1 0) (get-at matrix 2 0)
                       (get-at matrix 0 1) (get-at matrix 1 1) (get-at matrix 2 1)
                       (get-at matrix 0 2) (get-at matrix 1 2) (get-at matrix 2 2)))))

(defmethod transpose ((matrix float4x4))
  (make-float4x4
   (make-array '(16) :initial-contents
               (vector (get-at matrix 0 0) (get-at matrix 1 0) (get-at matrix 2 0) (get-at matrix 3 0)
                       (get-at matrix 0 1) (get-at matrix 1 1) (get-at matrix 2 1) (get-at matrix 3 1)
                       (get-at matrix 0 2) (get-at matrix 1 2) (get-at matrix 2 2) (get-at matrix 3 2)
                       (get-at matrix 0 3) (get-at matrix 1 3) (get-at matrix 2 3) (get-at matrix 3 3)))))


(defun eqm (matrix1 matrix2)
  (reduce #'hand
          (map 'vector #'eqfp
               (in-vec matrix1)
               (in-vec matrix2))))

(defun neqm (matrix1 matrix2)
  (not (eqm matrix1 matrix2)))


(defun make-ortho-projection (&key left right top bottom near far)
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

(defun make-look-at (position target up)
  (let* ((dir (normalize (sub target position)))
         (right (normalize (cross dir up)))
         (norm-up (normalize (cross right dir))))
    (make-float4x4
     (float4-x right)
     (float4-y right)
     (float4-z right)
     (dot right (negv position))

     (float4-x norm-up)
     (float4-y norm-up)
     (float4-z norm-up)
     (dot norm-up (negv position))

     (float4-x dir)
     (float4-y dir)
     (float4-z dir)
     (dot dir (negv position))

     0.0
     0.0
     0.0
     1.0)))
