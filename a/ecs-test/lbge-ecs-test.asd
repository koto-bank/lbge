(defpackage :lbge-ecs-test-system
  (:use :cl))

(in-package :lbge-ecs-test-system)

(asdf:defsystem :lbge-ecs-test
  :depends-on (:lbge :rove)
  :components ((:file "main")))
