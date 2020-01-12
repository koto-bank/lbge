(defpackage :lbge.test.hash
  (:use :cl :rove :lbge.hash))

(in-package :lbge.test.hash)

(deftest hash-test
    (let ((h (hash-make '((:a 1) (:b 2)))))
      (ok (eq (hash-get h :a) 1))
      (ok (null (hash-get h :c)))
      (ok (eq (hash-get h :c 3) 3))
      (ok (hash-equal (hash-set (hash-make) '((:a 1) (:b 2))) h))
      (ng (hash-equal (hash-set (hash-make) '((:c 3))) h))))
