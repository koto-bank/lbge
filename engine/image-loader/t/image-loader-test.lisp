(defpackage lbge.test.image-loader
  (:use :cl :rove :lbge.image-loader :lbge.image-loader.image))

(in-package :lbge.test.image-loader)

(defvar *lbge-skip-test*)

(defun instance-of-p (class obj)
  (eq class (type-of obj)))

(deftest load-image-test
  (ok (instance-of-p
       'lbge.image-loader.image::image
       (load-image "test-file.tga")))
  ;; While png isn't support
  (ok (eq NIL
          (load-image "test-file.png")))
  (testing "Error throws"
    (ok (signals (load-image 1)))
    (ok (signals (load-image 'a)))
    (ok (signals (load-image '())))
    (ok (signals (load-image '(a b c))))
    (ok (signals (load-image :a)))))
