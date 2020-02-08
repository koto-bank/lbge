(defpackage :lbge-unit-tests-system
  (:use :cl))

(in-package :lbge-unit-tests-system)

(asdf:defsystem :lbge-unit-tests
  :depends-on (:rove :lbge)
  :components
  ((:file "main")))
