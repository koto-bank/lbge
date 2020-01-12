(defpackage :lbge.test.hash
  (:use :cl :rove :lbge.hash))

(in-package :lbge.test.hash)

(deftest hash-test
  (testing "hash-get"
    (let ((h (make-hash '((:a 1)))))
      (ok (null (hash-get h :b)))
      (ok (= (hash-get h :a) 1))
      (ok (= (hash-get h :b 2) 2))))
  (testing "hash-set"
    (let ((h (make-hash)))
      (hash-set h '((:a 1)))
      (ok (= (hash-get h :a) 1))))
  (testing "equal-hash"
    (let ((h1 (make-hash '((:a 1) (:b 2))))
          (h2 (make-hash '((:c 3) (:d 4)))))
      (ok (equal-hash h1 (make-hash '((:a 1) (:b 2)))))
      (ng (equal-hash h1 h2)))))
