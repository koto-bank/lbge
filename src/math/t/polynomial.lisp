(define-test polynomial-add-test
    (:lbge.math)
  (testing "Polynomial addition"
    (ok (eqg (add (make-polynomial 1 2 3 4)
                  (make-polynomial 5 6 7))
             (make-polynomial 6 8 10 4)))))

(define-test polynomial-sub-test
    (:lbge.math)
  (testing "Polynomial subtraction"
    (ok (eqg (sub (make-polynomial 5 6 7 8)
                  (make-polynomial 1 2 3))
             (make-polynomial 4 4 4 8)))))

(define-test polynomial-mul-test
    (:lbge.math)
  (testing "Polynomial multiplication by real"
    (ok (eqg (mul (make-polynomial 1 2 3 4)
                  2)
             (make-polynomial 2 4 6 8))))

  (testing "Polynomial multiplication by polynomial"
    (ok (eqg (mul (make-polynomial 1 2 3 4)
                  (make-polynomial 5 6 7 8))
             (make-polynomial 5 16 34 60 61 52 32)))))

(define-test polynomial-div-test
    (:lbge.math)
  (testing "Polynomial division by real"
    (ok (eqg (div (make-polynomial 2 4 6 8)
                  2)
             (make-polynomial 1 2 3 4)))))

(define-test polynomial-pad-poly-test
    (:lbge.math)
  (testing "Polynomial padding"
    (ok (eqg (pad-poly (make-polynomial 1 2 3 4)
                       2)
             (make-polynomial 1 2 3 4 0 0)))))

(define-test polynomial-raise-degree-test
    (:lbge.math)
  (testing "Polynomial degree increase"
    (ok (eqg (raise-degree (make-polynomial 1 2 3 4)
                           2)
             (make-polynomial 0 0 1 2 3 4)))))

(define-test polynomial-call-test
    (:lbge.math)
  (testing "Polynomial evaluation at a point"
    (ok (= (call (make-polynomial 1 2 3 4)
                 1)
           10))))
