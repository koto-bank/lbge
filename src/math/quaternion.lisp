(in-package :lbge.math)

(defclass quaternion ()
  ((x :accessor quaternion-x :initarg :x)
   (y :accessor quaternion-y :initarg :y)
   (z :accessor quaternion-z :initarg :z)
   (w :accessor quaternion-w :initarg :w)))

(defun make-quaternion (&key (x 0.0f0) (y 0.0f0) (z 0.0f0) (w 1.0f0))
  (make-instance 'quaternion :x x :y y :z z :w w))

(defmacro make-quaternion-f2 (f2)
  `(make-quaternion :w 0 :x (float2-x ,f2) :y (float2-y ,f2) :z 0))

(defmacro make-quaternion-f3 (f3)
  `(make-quaternion :w 0 :x (float3-x ,f3) :y (float3-y ,f3) :z (float3-z ,f3)))

(defun quaternion-zero () (make-quaternion :x 0 :y 0 :z 0 :w 0))
(defun quaternion-one () (make-quaternion :x 1 :y 1 :z 1 :w 1))

(defmacro quaternion-v (q)
  `(make-float3 (quaternion-x ,q)
                (quaternion-y ,q)
                (quaternion-z ,q)))

(defmacro quaternion-a (q)
  `(quaternion-w ,q))

(defmethod print-object ((q quaternion) out)
  (let* ((w (quaternion-w q))
         (x (quaternion-x q))
         (y (quaternion-y q))
         (z (quaternion-z q))
         (xsgn (get-sign x))
         (ysgn (get-sign y))
         (zsgn (get-sign z)))
      (format out "~a ~a ~ai ~a ~aj ~a ~ak" w xsgn (abs x) ysgn (abs y) zsgn (abs z))))

(defmacro define-quaternion-op (name op)
  `(defmethod ,name ((q1 quaternion) (q2 quaternion))
    (make-quaternion
      :x (,op (quaternion-x q1) (quaternion-x q2))
      :y (,op (quaternion-y q1) (quaternion-y q2))
      :z (,op (quaternion-z q1) (quaternion-z q2))
      :w (,op (quaternion-w q1) (quaternion-w q2)))))

(defmacro define-quaternion-num-strord-op (name op)
  `(defmethod ,name ((q quaternion) (num real))
    (make-quaternion
      :x (,op (quaternion-x q) num)
      :y (,op (quaternion-y q) num)
      :z (,op (quaternion-z q) num)
      :w (,op (quaternion-w q) num))))

(defmacro define-quaternion-num-revord-op (name op)
  `(defmethod ,name ((num real) (q quaternion))
    (make-quaternion
     :x (,op num (quaternion-x q))
     :y (,op num (quaternion-y q))
     :z (,op num (quaternion-z q))
     :w (,op num (quaternion-w q)))))

(defmacro define-quaternion-num-op (name op)
  `(progn
    (define-quaternion-num-strord-op ,name ,op)
    (define-quaternion-num-revord-op ,name ,op)))

(defmacro define-quaternion-unary-op (name op)
  `(defmethod ,name ((q quaternion))
    (make-quaternion
     :x (,op (quaternion-x q))
     :y (,op (quaternion-y q))
     :z (,op (quaternion-z q))
     :w (,op (quaternion-w q)))))

(define-quaternion-op add +)
(define-quaternion-op sub -)

(define-quaternion-num-op mul *)
(define-quaternion-num-op div /)

(define-quaternion-unary-op absq abs)
(define-quaternion-unary-op negq -)

(defmacro quat-add-real (q value)
  `(make-quaternion
     :w (+ (quaternion-w ,q) ,value)
     :x (quaternion-x ,q)
     :y (quaternion-y ,q)
     :z (quaternion-z ,q)))

(defmethod add ((value real) (q quaternion))
  (quat-add-real q value))

(defmethod add ((q quaternion) (value real))
  (quat-add-real q value))

(defun eqq (q1 q2 &optional (eps +epsilon+))
  "Test two quaternions for equality"
  (and
    (eqfp (quaternion-x q1) (quaternion-x q2) eps)
    (eqfp (quaternion-y q1) (quaternion-y q2) eps)
    (eqfp (quaternion-z q1) (quaternion-z q2) eps)
    (eqfp (quaternion-w q1) (quaternion-w q2) eps)))

(defun neqq (q1 q2 &optional (eps +epsilon+))
  "Test two quaternions for equality"
  (not (eqq q1 q2 eps)))

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
  (+ (expt (quaternion-x q) 2)
     (expt (quaternion-y q) 2)
     (expt (quaternion-z q) 2)
     (expt (quaternion-w q) 2)))

(defmethod norm ((q quaternion))
  (sqrt (norm2 q)))

(defmethod versor ((q quaternion))
  (if (eqq q (quaternion-zero))
    (quaternion-zero)
    (div q (norm q))))

(defmethod conj ((q quaternion))
  (make-quaternion
    :x (- (quaternion-x q))
    :y (- (quaternion-y q))
    :z (- (quaternion-z q))
    :w (quaternion-w q)))

(defmethod inv ((q quaternion))
  (div (conj q) (norm2 q)))

(defmethod div ((q quaternion) (num quaternion))
  (mul q (inv num)))

(defmethod expq ((q quaternion))
  (let ((a (quaternion-a q))
        (v (quaternion-v q)))
      (mul (exp a)
           (add (cos (norm v))
               (make-quaternion-f3
                 (mul (normalize v)
                      (sin (norm v))))))))

(defmethod logq ((q quaternion))
  (let ((a (quaternion-a q))
        (v (quaternion-v q)))
      (add (log (norm q))
           (make-quaternion-f3
             (mul (normalize v)
                  (acos (/ a (norm q))))))))

(defmethod exptq ((q1 quaternion) (q2 quaternion))
  (expq
    (mul (logq q1)
         q2)))

(defmethod exptq ((q1 quaternion) (q2 real))
  (expq
    (mul (logq q1)
         q2)))

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
