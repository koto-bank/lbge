(define-test quaternion-add-test
    (:lbge.math)
  (testing "Quaternion addition"
    (ok (eqg (add (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w 6 :x 8 :y 10 :z 12)))))

(define-test quaternion-sub-test
    (:lbge.math)
  (testing "Quaternion subtraction"
    (ok (eqg (sub (make-quaternion :w 5 :x 6 :y 7 :z 8)
                  (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 4 :x 4 :y 4 :z 4)))))

(define-test quaternion-mul-test
    (:lbge.math)
  (testing "Quaternion multiplication by real"
    (ok (eqg (mul (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  2)
             (make-quaternion :w 2 :x 4 :y 6 :z 8))))

  (testing "Quaternion multiplication by quaternion"
    (ok (eqg (mul (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w -60 :x 12 :y 30 :z 24)))))

(define-test quaternion-div-test
    (:lbge.math)
  (testing "Quaternion division by real"
    (ok (eqg (div (make-quaternion :w 2 :x 4 :y 6 :z 8)
                  2)
             (make-quaternion :w 1 :x 2 :y 3 :z 4))))

  (testing "Quaternion division by quaternion (multiplication by inverse)"
    (ok (eqg (div (make-quaternion :w 1 :x 2 :y 3 :z 4)
                  (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w 0.40229887
                              :x 0.04597701
                              :z 0.09195402)))))
(define-test quaternion-norm-test
    (:lbge.math)
  (testing "Quaternion norm"
    (ok (= (norm (make-quaternion :w 1 :x 2 :y 3 :z 4))
           5.4772256))))

(define-test quaternion-versor-test
    (:lbge.math)
  (testing "Zero quaternion versor"
    (ok (eqg (versor (quaternion-zero))
             (quaternion-zero))))

  (testing "Quaternion versor"
    (ok (eqg (versor (quaternion-one))
             (make-quaternion :x 1/2 :y 1/2 :z 1/2 :w 1/2)))))

(define-test quaternion-conj-test
    (:lbge.math)
  (testing "Conjugate of a quaternion"
    (ok (eqg (conj (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 1 :x -2 :y -3 :z -4)))))

(define-test quaternion-inv-test
    (:lbge.math)
  (testing "Inverse of a quaternion"
    (ok (eqg (inv (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 1/30 :x -2/30 :y -3/30 :z -4/30)))))

(define-test expq-test
    (:lbge.math)
  (testing "Exponential of a quaternion"
    (ok (eqg (expq (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 1.6939225
                              :x -0.7895596
                              :y -1.1843395
                              :z -1.5791192))))) ; values computed with GNU Octave

(define-test logq-test
    (:lbge.math)
  (testing "Logarithm of a quaternion"
    (ok (eqg (logq (make-quaternion :w 1 :x 2 :y 3 :z 4))
             (make-quaternion :w 1.7005987
                              :x 0.5151903
                              :y 0.77278554
                              :z 1.0303806))))) ; values computed with GNU Octave

(define-test exptq-test
    (:lbge.math)
  (testing "Quaternion to the real power"
    (ok (eqg (exptq (make-quaternion :w 1 :x 2 :y 3 :z 4)
                    2)
             (make-quaternion :w -28 :x 4 :y 6 :z 8))))

  (testing "Quaternion to the quaternion power"
    (ok (eqg (exptq (make-quaternion :w 1 :x 2 :y 3 :z 4)
                    (make-quaternion :w 5 :x 6 :y 7 :z 8))
             (make-quaternion :w -2.2844537e-04
                              :x 5.5768814e-05
                              :y 8.462691e-05
                              :z 8.4140054e-05)))))  ; values computed with GNU Octave

; TODO: to-euler and from-euler tests
