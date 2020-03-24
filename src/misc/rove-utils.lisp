(defpackage :lbge.rove-utils
  (:use :cl)
  (:export :report-results))

(in-package :lbge.rove-utils)

(defun report-results ()
  (let ((failed (rove/core/stats:stats-failed rove:*stats*))
        (passed (rove/core/stats:stats-passed rove:*stats*)))

    (when (> (length passed) 0)
      (format t "~%-------------~%Passed tests:~%")
      (loop :for test :across (rove/core/stats:stats-passed rove:*stats*) :do
        (format t "* ~A~%" (rove/core/result:test-name test))))
    (when (> (length failed) 0)
      (format t "~%-------------~%Failed tests:~%")
      (loop :for test :across failed :do
        (format t "* ~A~%" (rove/core/result:test-name test))))
    (when (= (length failed) 0)
      (format t "~%All tests passed successfully!~%"))
    (= 0 (length failed))))
