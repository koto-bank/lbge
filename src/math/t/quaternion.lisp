(define-test add-test
    (:lbge.math)
  (testing "Quaternion addition"
    (ok (eqq (add (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w 6 :x 8 :y 10 :z 12)))))

(define-test sub-test
    (:lbge.math)
  (testing "Quaternion subtraction"
    (ok (eqq (sub (make-quaternion :w 5 :x 6 :y 7 :z 8)
                  (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 4 :x 4 :y 4 :z 4)))))

(define-test mul-test
    (:lbge.math)
  (testing "Quaternion multiplication by real"
    (ok (eqq (mul (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  2)
             (make-quaternion :w 2 :x 4 :y 6 :z 8))))

  (testing "Quaternion multiplication by quaternion"
    (ok (eqq (mul (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w -60 :x 12 :y 30 :z 24)))))

(define-test div-test
    (:lbge.math)
  (testing "Quaternion division by real"
    (ok (eqq (div (make-quaternion :w 2 :x 4 :y 6 :z 8)
                  2)
             (make-quaternion :w 1 :x 2 :y 3 :z 4))))

  (testing "Quaternion division by quaternion (multiplication by inverse)"
    (ok (eqq (div (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w 0.4023 :x 0.04598 :y 0 :z 0.09195)))))

(define-test norm-test
    (:lbge.math)
  (testing "Quaternion norm"
    (ok (= (norm (make-quaternion :w 1 :x 2 :y 3 :z 4))
           5.4772))))

(define-test versor-test
    (:lbge.math)
  (testing "Zero quaternion norm"
    (ok (eqq (versor (quaternion-zero))
             (quaternion-zero))))

  (testing "Quaternion norm"
    (ok (eqq (versor (quaternion-one))
             (make-quaternion :x 1/4 :y 1/4 :z 1/4 :w 1/4)))))

(define-test conj-test
    (:lbge.math)
  (testing "Conjugate of a quaternion"
    (ok (eqq (conj (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion-nord :w 1 :x -2 :y -3 :z -4)))))

(define-test inv-test
    (:lbge.math)
  (testing "Inverse of a quaternion"
    (ok (eqq (inv (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 1/30 :x 2/30 :y 3/30 :z 4/30)))))

(define-test expq-test
    (:lbge.math)
  (testing "Exponential of a quaternion"
    (ok (eqq (expq (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w -4.026 :x -5.369 :y -1.342 :z 2.786)))))

(define-test logq-test
    (:lbge.math)
  (testing "Logarithm of a quaternion"
    (ok (eqq (logq (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 0.7043 :x 0.939 :y 0.2348 :z 1.701)))))

(define-test exptq-test
    (:lbge.math)
  (testing "Quaternion to the real power"
    (ok (eqq (exptq (make-quaternion :w 1 :x 2 :y 3 :z 4)
                    2)
             (make-quaternion :w -28 :x 4 :y 6 :z 8))))
  (testing "Quaternion to the quaternion power"
    (ok (eqq (exptq (make-quaternion :w 1 :x 2 :y 3 :z 4)
                    (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w -0.0002284 :x 5.577e-05 :y 8.463e-05 :z 8.414e-05)))))

; TODO: to-euler and from-euler tests
