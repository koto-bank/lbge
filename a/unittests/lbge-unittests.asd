(defpackage :lbge-unittests-system
  (:use :cl))

(in-package :lbge-unittests-system)

(asdf:defsystem :lbge-unittests
  :depends-on (:rove :lbge)
  :components
  ((:file "main")))
