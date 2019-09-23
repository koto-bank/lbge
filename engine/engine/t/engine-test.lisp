(defpackage :lbge.test.engine
  (:use :cl :rove :lbge.engine))

(in-package :lbge.test.engine)

(deftest engine-test
  (delete-engine)
  (testing "Engine creation"
    (signals (get-engine))
    (let ((engine (make-engine)))
      (ok engine)
      (pass (check-type engine engine))
      (ok (eq (get-engine) engine)))))
