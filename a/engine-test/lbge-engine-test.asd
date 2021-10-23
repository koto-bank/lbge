(defpackage :lbge-engine-test-system
  (:use :cl))

(in-package :lbge-engine-test-system)

(asdf:defsystem :lbge-engine-test
  :depends-on (:lbge :objective-cl)
  :components ((:file "main"))
  :around-compile (lambda (next)
                    (uiop:symbol-call '#:objective-cl '#:enable)
                    (unwind-protect (funcall next)
                      (uiop:symbol-call '#:objective-cl '#:disable))))
