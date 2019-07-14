(defpackage :lbge-test
  (:use :cl))

(in-package :lbge-test)

(asdf:defsystem :lbge-test
  :components
  #.(let (l) (uiop:collect-sub*directories
              (asdf:system-source-directory :lbge) t t
              (lambda (dir)
                (let ((dir-name (pathname-directory dir)))
                  (if (string-equal (last dir-name) "t")
                      (uiop:collect-sub*directories
                       ())))))))
