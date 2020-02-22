(in-package :lbge.math)

(defclass transform ()
  ((translation :initform (make-float4 0.0f0)
                :initarg :translation
                :accessor translation)
   (rotation :initform (make-quaternion)
             :accessor rotation)
   (scale :initform (make-float3 1.0f0)
          :accessor scale))
  (:documentation "Contains transformation parameters"))

(defun make-transform (&key (pos (make-float4 0.0f0)))
  (make-instance 'transform :translation pos))

(defmethod mul ((t1 transform) (t2 transform))
  (let ((r (make-transform)))
    (with-slots (translation rotation scale) r
      (setf rotation (mul (rotation t2) (rotation t1))
            scale (mul (scale t1) (scale t2))
            translation (add (mul (mul (translation t1) (rotation t2))
                                  (scale t2))
                             (translation t2))))
    r))

(defun transform-matrix (transform)
  (with-slots ((r rotation) (s scale) (tr translation)) transform
    (let ((xx (* (quaternion-x r) (quaternion-x r)))
          (xy (* (quaternion-x r) (quaternion-y r)))
          (xz (* (quaternion-x r) (quaternion-z r)))
          (xw (* (quaternion-x r) (quaternion-w r)))
          (yy (* (quaternion-y r) (quaternion-y r)))
          (yz (* (quaternion-y r) (quaternion-z r)))
          (yw (* (quaternion-y r) (quaternion-w r)))
          (zz (* (quaternion-z r) (quaternion-z r)))
          (zw (* (quaternion-z r) (quaternion-w r))))
      (make-float4x4
       (* (float3-x s)
          (- 1.0f0 (* 2.0f0 (+ yy zz))))
       (* (float3-y s)
          (* 2.0f0 (- xy zw)))
       (* (float3-z s)
          (* 2.0f0 (+ xz yw)))
       (float3-x tr)

       (* (float3-x s)
          (* 2.0f0 (+ xy zw)))
       (* (float3-y s)
          (- 1.0f0 (* 2.0f0 (+ xx zz))))
       (* (float3-z s)
          (* 2.0f0 (- yz xw)))
       (float3-y tr)

       (* (float3-x s)
          (* 2.0f0 (- xz yw)))
       (* (float3-y s)
          (* 2.0f0 (+ yz xw)))
       (* (float3-z s)
          (- 1.0f0 (* 2.0f0 (+ xx yy))))
       (float3-z tr)

       0.0f0
       0.0f0
       0.0f0
       1.0f0))))
