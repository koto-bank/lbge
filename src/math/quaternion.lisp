(in-package :lbge.math)

(defclass quaternion ()
  ((x :accessor quaternion-x :initarg :x)
   (y :accessor quaternion-y :initarg :y)
   (z :accessor quaternion-z :initarg :z)
   (w :accessor quaternion-w :initarg :w)))

(defun make-quaternion (&key (x 0.0f0) (y 0.0f0) (z 0.0f0) (w 1.0f0))
  (make-instance 'quaternion :x x :y y :z z :w w))

(defun quaternion-zero () (make-quaternion 0 0 0 0))
(defun quaternion-one () (make-quaternion 1 1 1 1))

(defmacro quaternion-v (q)
  `(make-instance 'float3 :in-vec #((quaternion-y ,q)
                                    (quaternion-z ,q)
                                    (quaternion-w ,q))))

(defmacro quaternion-a (q)
  `(quaternion-x ,q))

(defgeneric norm2 (q)
  (:documentation "Get the squared euclidean norm of a quaternion"))

(defgeneric norm (q)
  (:documentation "Get the euclidean norm of a quaternion"))

(defgeneric conj (q)
  (:documentation "Get the conjugate quaternion of q"))

(defgeneric inv (q)
  (:documentation "Get the inverse quaternion of q"))

(defgeneric versor (q)
  (:documentation "Return the versor (unit quaterion) of q"))

(defgeneric expq (q)
  (:documentation "Raise e to the power of q"))

(defgeneric logq (q)
  (:documentation "Get the natural logarithm of q"))

(defgeneric exptq (q1 q2)
  (:documentation "Raise q1 to the power of q2"))

(defmacro define-quaternion-op (name op docstring)
  `(progn
    (defgeneric ,name (q1 q2)
     (:documentation ,docstring))
    (defmethod ,name ((q1 quaternion) (q2 quaternion))
     (make-quaternion
       (op (quaternion-x q1) (quaternion-x q2))
       (op (quaternion-y q1) (quaternion-y q2))
       (op (quaternion-z q1) (quaternion-z q2))
       (op (quaternion-w q1) (quaternion-w q2))))))

(defmacro define-quaternion-num-op (name op docstring)
  `(progn
    (defgeneric ,name (q num)
     (:documentation ,docstring))
    (defmethod ,name ((q quaternion) (num real))
     (make-quaternion
       (op (quaternion-x q) num)
       (op (quaternion-y q) num)
       (op (quaternion-z q) num)
       (op (quaternion-w q) num)))))

(defmacro define-quaternion-num-op-revord (name op docstring)
  `(progn
    (defgeneric ,name (q num)
     (:documentation ,docstring))
    (defmethod ,name ((num real) (q quaternion))
     (make-quaternion
       (op num (quaternion-x q))
       (op num (quaternion-y q))
       (op num (quaternion-z q))
       (op num (quaternion-w q))))))

(defmacro define-quaternion-unary-op (name op)
  `(progn
    (defgeneric ,name (q)
     (:documentation ,docstring))
    (defmethod ,name ((q quaternion))
     (make-quaternion
       (op (quaternion-x q))
       (op (quaternion-y q))
       (op (quaternion-z q))
       (op (quaternion-w q))))))

(define-quaternion-op add #'+ "Add quaternion q1 to q2")
(define-quaternion-op sub #'- "Subtract quaternion q2 from q1")

(define-quaternion-num-op mul #'* "Multiply each element of q by num if num is real, perform quaternion multiplication if num is a quaternion")
(define-quaternion-num-op div #'/ "Divide each element of q by num if num is real, multiply q1 by inverse of num if num is a quaternion")

(define-quaternion-num-op-revord mul #'* "Multiply each element of q by num if num is real, perform quaternion multiplication if num is a quaternion")
(define-quaternion-num-op-revord div #'/ "Divide num by each element of q if num is real, multiply num by inverse of q1 if num is a quaternion")

(define-quaternion-unary-op absq #'abs "Get the absolute value f each element of the quaternion")
(define-quaternion-unary-op negq #'- "Flip the sign of each element of the quaternion")

(defun eqq (q1 q2)
  (and
    (eqfp (quaternion-x q1) (quaternion-x q2))
    (eqfp (quaternion-y q1) (quaternion-y q2))
    (eqfp (quaternion-z q1) (quaternion-z q2))
    (eqfp (quaternion-w q1) (quaternion-w q2))))

(demacro neqq (q1 q2)
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
    (div q (norm q))
    (defmethod conj ((q quaternion))))

  (make-quaternion
    (quaternion-x q)
    (- (quaternion-y q))
    (- (quaternion-z q))
    (- (quaternion-w q))))

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
