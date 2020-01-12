(defpackage :lbge.test.hash
  (:use :cl :rove))

(in-package :lbge.test.hash)

(deftest hash-test
  (testing "get"
    (let ((h (hash:make :a 1)))
      (ok (null (hash:get h :b)))
      (ok (= (hash:get h :a) 1))
      (ok (= (hash:get h :b 2) 2))))
  (testing "set"
    (let ((h (hash:make)))
      (hash:set h :a '((:a 1)))
      (ok (= (hash:get h :a) 1))))
  (testing "equalp"
    (let ((h1 (hash:make '((:a 1) (:b 2))))
          (h2 (hash:make '((:c 3) (:d 4)))))
      (ok (hash:equalp h1 (hash:make '((:a 1) (:b 2)))))
      (ng (hash:equalp h1 h2)))))
