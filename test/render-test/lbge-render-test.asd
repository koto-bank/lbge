(defpackage :lbge-render-test-system
  (:use :cl))

(in-package :lbge-render-test-system)

(asdf:defsystem :lbge-render-test
  :depends-on (:lbge :lbge-render)
  :components ((:file "main")))
