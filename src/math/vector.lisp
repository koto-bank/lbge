(in-package :lbge.math)

(defun make-float-vec (length &rest contents)
  (if contents
      (make-array (list length) :element-type 'single-float
                                :initial-contents (mapcar (lambda (e) (coerce e 'single-float)) contents))
      (make-array (list length) :element-type 'single-float :initial-element 0.0f0)))

(defclass floatn ()
  ((%in-vec :initarg :in-vec
            :reader in-vec
            :type (vector single-float)
            :documentation "The container vector for the data")))

(defclass float2 (floatn) ()
  (:default-initargs :in-vec (make-float-vec 2)))

(defclass float3 (floatn) ()
  (:default-initargs :in-vec (make-float-vec 3)))

(defclass float4 (floatn) ()
  (:default-initargs :in-vec (make-float-vec 4)))

(defmacro define-element-accessor (name idx type)
  `(progn
     (defmethod ,name ((vec ,type))
       (aref (in-vec vec) ,idx))
     (defmethod (setf ,name) (new-value (vec ,type))
       (setf (aref (in-vec vec) ,idx) new-value))))

(define-element-accessor x 0 float2)
(define-element-accessor y 1 float2)

(define-element-accessor x 0 float3)
(define-element-accessor y 1 float3)
(define-element-accessor z 2 float3)

(define-element-accessor x 0 float4)
(define-element-accessor y 1 float4)
(define-element-accessor z 2 float4)
(define-element-accessor w 3 float4)

(defun make-float2 (&optional (x 0.0f0) (y 0.0f0))
  (make-instance 'float2 :in-vec (make-float-vec 2 x y)))

(defun make-float3 (&optional (x 0.0f0) (y 0.0f0) (z 0.0f0))
  (make-instance 'float3 :in-vec (make-float-vec 3 x y z)))

(defun make-float4 (&optional (x 0.0f0) (y 0.0f0) (z 0.0f0) (w 0.0f0))
  (make-instance 'float4 :in-vec (make-float-vec 4 x y z w)))

(defun float2-zero () (make-float2))
(defun float3-zero () (make-float3))
(defun float4-zero () (make-float4))

(defun float2-one () (make-float2 1 1))
(defun float3-one () (make-float3 1 1 1))
(defun float4-one () (make-float4 1 1 1 1))

(defmethod print-object ((vec floatn) out)
  (print-unreadable-object (vec out :type t :identity t)
    (format out "~{~s ~s~^ ~}" (mapcan #'list '(:x :y :z :w) (coerce (in-vec vec) 'list)))))

(defmethod size ((vec float2)) 2)

(defmethod size ((vec float3)) 3)

(defmethod size ((vec float4)) 4)

(defmacro define-vec-op (name result-type map-op)
  `(defmethod ,name ((vector1 ,result-type) (vector2 ,result-type))
     (make-instance ',result-type :in-vec
                    (map '(vector single-float) ,map-op
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
  (flet ((body (func type)
              `(make-instance ',type :in-vec
                              (map '(vector single-float) ,func
                                   (in-vec vec)))))
     `(progn
       (defmethod ,name ((value real) (vector ,vec-type))
         ,(body `(ax:curry ,map-fun value) vec-type))
       (defmethod ,name ((vector ,vec-type) (value real))
         ,(body `(ax:rcurry ,map-fun value) vec-type)))))

(define-vec-num-op mul float2 #'*)
(define-vec-num-op mul float3 #'*)
(define-vec-num-op mul float4 #'*)

(define-vec-num-op div float2 #'/)
(define-vec-num-op div float3 #'/)
(define-vec-num-op div float4 #'/)

(defun dot (vector1 vector2)
  "Dot product of vector1 and vector2"
  (assert (typep vector1 (type-of vector2)))
  (loop :for v1 :across (in-vec vector1)
        :for v2 :across (in-vec vector2)
        :sum (* v1 v2)))

(defmethod norm2 ((vector floatn))
  (loop :for v :across (in-vec vector)
        :sum (expt v 2)))

(defmethod norm ((vector floatn))
  (declare (inline norm2))
  (sqrt (norm2 vector)))

(defmacro define-vec-unary-op (name vec-type map-fun)
  `(defmethod ,name ((vector ,vec-type))
     (make-instance ',vec-type :in-vec
                    (map 'vector ,map-fun
                         (in-vec vector)))))

(define-vec-unary-op negg float2 #'-)
(define-vec-unary-op negg float3 #'-)
(define-vec-unary-op negg float4 #'-)

(define-vec-unary-op absg float2 #'abs)
(define-vec-unary-op absg float3 #'abs)
(define-vec-unary-op absg float4 #'abs)

(defmethod eqg ((vector1 floatn) (vector2 floatn) &key (eps *epsilon*) &allow-other-keys)
  "Test two vectors for equality"
  (and (typep vector1 (type-of vector2))
       (every (ax:rcurry #'eqfp eps) (in-vec vector1) (in-vec vector2))))

(defmethod neqg ((vector1 floatn) (vector2 floatn) &key (eps *epsilon*) &allow-other-keys)
  "Test two vectors for inequality"
  (not (eqv vector1 vector2 eps)))

(defun cross (vector1 vector2)
  (assert (and (typep vector1 'float3) (typep vector2 'float3)))
  (make-float3 (- (* (y vector1) (z vector2))
                  (* (z vector1) (y vector2)))
               (- (* (z vector1) (x vector2))
                  (* (x vector1) (z vector2)))
               (- (* (x vector1) (y vector2))
                  (* (y vector1) (x vector2)))))

(defun zero-vector-p (vector)
  "Test if vector is zero"
  (every #'zerop (in-vec vector)))

(defmethod normalize ((vector floatn))
  "Return the normalized vector of input vector"
  (if (zero-vector-p vector)
      vector
      (div vector (norm vector))))

(defun angle (vector1 vector2)
  "Angle between two vectors in radians"
  (acos (/ (dot vector1 vector2)
           (* (norm vector1)
              (norm vector2)))))

(defun project (vector1 vector2)
  "Return the projection of vector1 onto vector2"
  (let ((normalized (normalize vector2)))
    (mul normalized (dot vector1 normalized))))

(ax:define-constant +float2-components+ '(x y)
  :test #'equal)

(ax:define-constant +float3-components+ '(x y z)
  :test #'equal)

(ax:define-constant +float4-components+ '(x y z w)
  :test #'equal)

(defmacro define-swizzle-method (type length)
  `(defmethod swizzle ((vec ,type) components)
     (assert (= (length components) ,length))
     (make-instance ',type :in-vec
                    (loop :for c :in components
                          :for a := (intern (symbol-name c) :lbge.math)
                          :do (assert (member a ,(ax:symbolicate #\+ type :-components+)))
                          :collect (funcall a vec) :into res
                          :finally (return (coerce res '(vector single-float)))))))

(define-swizzle-method float2 2)
(define-swizzle-method float3 3)
(define-swizzle-method float4 4)
