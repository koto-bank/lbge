(defpackage :lbge.test.hash
  (:use :cl :rove :lbge.hash))

(in-package :lbge.test.hash)

(deftest hash-make-equal-test
    (ok (eq (equalp (make-hash-table) (make-hash-table))
            (equal-hash (make-hash) (make-hash)))))

(deftest hash-set-test
  (let ((h1 (set-hash (make-hash) '((:a 1) (:b 2)))))
    (ok (equal-hash h1
                    (set-hash (make-hash) '((:a 1) (:b 2)))))))

(deftest get-hash-test
  (let ((h1 (set-hash (make-hash) '((:a 1)))))
    (ok (eq 1 (get-hash h1 :a)))))
