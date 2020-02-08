(defpackage :lbge.test.image
  (:use :cl :rove :lbge.image-loader.image))

(in-package :lbge.test.image)

(defvar *lbge-skip-test*)

(defun instance-of-p (class obj)
  (eq class (type-of obj)))

(deftest image-class-test
  (let ((image (make-image :width 50
                           :height 50
                           :channels "rgba8"
                           :data #(1 2 3 4))))
    (ok (instance-of-p 'lbge.image-loader.image::image image))
    (testing "image class readers"
      (ok (eq (width image) 50))
      (ok (eq (height image) 50))
      (ok (string= (channels image) "rgba8"))
      (ok (equalp (data image) #(1 2 3 4))))))
