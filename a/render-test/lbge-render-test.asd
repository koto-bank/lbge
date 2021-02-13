(defpackage :lbge-render-test-system
  (:use :cl))

(in-package :lbge-render-test-system)

(asdf:defsystem :lbge-render-test
  :depends-on (:lbge :lbge-render :objective-cl)
  :components ((:file "main"))
  :around-compile (lambda (next)
                    (uiop:symbol-call '#:objective-cl '#:enable)
                    (unwind-protect (funcall next)
                      (uiop:symbol-call '#:objective-cl '#:disable))))
