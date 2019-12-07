(defpackage :lbge-engine-test-system
  (:use :cl))

(in-package :lbge-engine-test-system)

(asdf:defsystem :lbge-engine-test
  :depends-on (:lbge)
  :components ((:file "main")))
