(defpackage satellites-system
  (:use :cl))

(in-package :satellites-system)

(asdf:defsystem :satellites
  (:depends-on (:lbge :lbge-render :objective-cl))
  (:components ((:file "main")))
  (:around-compile (lambda (next)
                    (uiop:symbol-call '#:objective-cl '#:enable)
                    (unwind-protect (funcall next)
                      (uiop:symbol-call '#:objective-cl '#:disable)))))
