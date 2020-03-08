(define-test hash-get-test
    (:lbge.hash)
  (testing "hash-get"
    (let ((h (make-hash :a 1)))
      (ok (null (hash-get h :b)))
      (ok (= (hash-get h :a) 1))
      (ok (= (hash-get h :b 2) 2))))
  (testing "equal-hash"
    (let ((h1 (make-hash :a 1 :b 2))
          (h2 (make-hash :c 3 :d 4)))
      (ok (hash-equal h1 (make-hash :a 1 :b 2)))
      (ng (hash-equal h1 h2)))))

(define-test hash-set-test
    (:lbge.hash)
  (testing "hash-set"
    (let ((h (make-hash)))
      (hash-set h :a 1)
      (ok (= (hash-get h :a) 1)))))
