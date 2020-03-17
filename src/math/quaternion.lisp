(in-package :lbge.math)

(defclass quaternion ()
  ((x :accessor quaternion-x :initarg :x)
   (y :accessor quaternion-y :initarg :y)
   (z :accessor quaternion-z :initarg :z)
   (w :accessor quaternion-w :initarg :w)))

(defun make-quaternion (&key (x 0.0f0) (y 0.0f0) (z 0.0f0) (w 1.0f0))
  (make-instance 'quaternion :x x :y y :z z :w w))

(defun quaternion-zero () (make-quaternion :x 0 :y 0 :z 0 :w 0))
(defun quaternion-one () (make-quaternion :x 1 :y 1 :z 1 :w 1))

(defmacro quaternion-v (q)
  `(make-instance 'float3 :in-vec #((quaternion-x ,q)
                                    (quaternion-y ,q)
                                    (quaternion-z ,q))))

(defmacro quaternion-a (q)
  `(quaternion-w ,q))

(defmacro define-quaternion-op (name op)
  `(defmethod ,name ((q1 quaternion) (q2 quaternion))
    (make-quaternion
      :x (funcall ,op (quaternion-x q1) (quaternion-x q2))
      :y (funcall ,op (quaternion-y q1) (quaternion-y q2))
      :z (funcall ,op (quaternion-z q1) (quaternion-z q2))
      :w (funcall ,op (quaternion-w q1) (quaternion-w q2)))))

(defmacro define-quaternion-num-op (name op)
  `(defmethod ,name ((q quaternion) (num real))
    (make-quaternion
      :x (funcall ,op (quaternion-x q) num)
      :y (funcall ,op (quaternion-y q) num)
      :z (funcall ,op (quaternion-z q) num)
      :w (funcall ,op (quaternion-w q) num))))

(defmacro define-quaternion-num-op-revord (name op)
  `(defmethod ,name ((num real) (q quaternion))
    (make-quaternion
     :x (funcall ,op num (quaternion-x q))
     :y (funcall ,op num (quaternion-y q))
     :z (funcall ,op num (quaternion-z q))
     :w (funcall ,op num (quaternion-w q)))))

(defmacro define-quaternion-unary-op (name op)
  `(defmethod ,name ((q quaternion))
    (make-quaternion
     :x (funcall ,op (quaternion-x q))
     :y (funcall ,op (quaternion-y q))
     :z (funcall ,op (quaternion-z q))
     :w (funcall ,op (quaternion-w q)))))

(define-quaternion-op add #'+)
(define-quaternion-op sub #'-)

(define-quaternion-num-op mul #'*)
(define-quaternion-num-op div #'/)

(define-quaternion-num-op-revord mul #'*)
(define-quaternion-num-op-revord div #'/)

(define-quaternion-unary-op absq #'abs)
(define-quaternion-unary-op negq #'-)

(defun eqq (q1 q2)
  "Test two quaternions for equality"
  (and
    (eqfp (quaternion-x q1) (quaternion-x q2))
    (eqfp (quaternion-y q1) (quaternion-y q2))
    (eqfp (quaternion-z q1) (quaternion-z q2))
    (eqfp (quaternion-w q1) (quaternion-w q2))))

(defun neqq (q1 q2)
  "Test two quaternions for equality"
  (not (eqq q1 q2)))

(defmethod mul ((q1 quaternion) (q2 quaternion))
  (let ((a (* (+ (quaternion-w q1) (quaternion-x q1))
              (+ (quaternion-w q2) (quaternion-x q2))))
        (b (* (- (quaternion-z q1) (quaternion-y q1))
              (- (quaternion-y q2) (quaternion-z q2))))
        (c (* (- (quaternion-x q1) (quaternion-w q1))
              (+ (quaternion-y q2) (quaternion-z q2))))
        (d (* (+ (quaternion-y q1) (quaternion-z q1))
              (- (quaternion-x q2) (quaternion-w q2))))
        (e (* (+ (quaternion-x q1) (quaternion-z q1))
              (+ (quaternion-x q2) (quaternion-y q2))))
        (f (* (- (quaternion-x q1) (quaternion-z q1))
              (- (quaternion-x q2) (quaternion-y q2))))
        (g (* (+ (quaternion-w q1) (quaternion-y q1))
              (- (quaternion-w q2) (quaternion-z q2))))
        (h (* (- (quaternion-w q1) (quaternion-y q1))
              (+ (quaternion-w q2) (quaternion-z q2)))))
    (make-quaternion :x (- a (* 0.5f0 (+ e f g h)))
                     :y (+ (- c) (* 0.5f0 (+ e (- f) g (- h))))
                     :z (+ (- d) (* 0.5f0 (+ e (- f) (- g) h)))
                     :w (+ b (* 0.5f0 (+ (- e) (- f) g h))))))

(defmethod mul ((v float3) (q quaternion))
  (let* ((q-v (make-float3 (quaternion-x q) (quaternion-y q) (quaternion-z q)))
         (c (mul (cross q-v v) 2.0f0)))
    (add v (add (mul c (quaternion-w q)) (cross q-v c)))))

(defmethod norm2 ((q quaternion))
  (expt (+ (quaternion-x q)) 2)
  (expt (+ (quaternion-y q)) 2)
  (expt (+ (quaternion-z q)) 2)
  (expt (+ (quaternion-w q)) 2))

(defmethod norm ((q quaternion))
  (sqrt (norm2 q)))

(defmethod versor ((q quaternion))
  (if (eqq q (quaternion-zero))
    (quaternion-zero)
    (div q (norm q))))

(defmethod conj ((q quaternion))
  (make-quaternion
    (- (quaternion-x q))
    (- (quaternion-y q))
    (- (quaternion-z q))
    (quaternion-w q)))

(defmethod inv ((q quaternion))
  (div (conj q) (norm2 q)))

(defmethod div (q quaternion) (num quaternion)
  (mul q (inv num)))

(defmethod expq ((q quaternion))
  (let ((a (quaternion-a q))
        (v (quaternion-v q)))
      (* (exp a)
         (+ (cos (norm v))
            (* (normalize v)
               (sin (norm v)))))))

(defmethod logq ((q quaternion))
  (let ((a (quaternion-a q))
        (v (quaternion-v v)))
      (+ (log (norm q))
         (* (normalize v)
            (acos (/ a (norm q)))))))

(defmethod exptq ((q1 quaternion) (q2 quaternion))
  (expq
    (mul q2
         (logq q1))))

(defmethod exptq ((q1 quaternion) (q2 real))
  (expq
    (mul q2
         (logq q1))))

(defun from-euler (angle)
  (let* ((cz (round-to-eps (cos (* 0.5 (float3-z angle)))))
         (sz (round-to-eps (sin (* 0.5 (float3-z angle)))))
         (cx (round-to-eps (cos (* 0.5 (float3-x angle)))))
         (sx (round-to-eps (sin (* 0.5 (float3-x angle)))))
         (cy (round-to-eps (cos (* 0.5 (float3-y angle)))))
         (sy (round-to-eps (sin (* 0.5 (float3-y angle))))))
    (make-quaternion :x (- (* sx cy cz) (* cx sy sz))
                     :y (+ (* cx sy cz) (* sx cy sz))
                     :z (- (* cx cy sz) (* sx sy cz))
                     :w (+ (* cx cy cz) (* sx sy sz)))))

(defun to-euler (quat)
  (flet ((x (q) (quaternion-x q))
         (y (q) (quaternion-y q))
         (z (q) (quaternion-z q))
         (w (q) (quaternion-w q)))
    (let* ((y (* 2.0f0 (+ (* (y quat) (z quat))
                          (* (w quat) (x quat)))))
           (x (- (+ (* (w quat) (w quat))
                    (* (z quat) (z quat)))
                 (* (x quat) (x quat))
                 (* (y quat) (y quat))))
           (f.x (if (and (eqfp x 0.0f0) (eqfp y 0.0f0))
                  (* 2.0f0 (atan (x quat) (w quat)))
                  (atan y x)))
           (f.y (asin (clamp (* -2.0f0 (- (* (x quat) (z quat))
                                          (* (w quat) (y quat))))
                             -1.0f0 1.0f0)))
           (f.z (atan (* 2.0f0 (+ (* (x quat) (y quat))
                                  (* (w quat) (z quat))))
                      (- (+ (* (w quat) (w quat))
                            (* (x quat) (x quat)))
                         (* (z quat) (z quat))
                         (* (y quat) (y quat))))))
      (make-float3 f.x f.y f.z))))
