(defpackage :lbge.test.tga
  (:use :cl :rove :lbge.image-loader))

(in-package :lbge.test.tga)

(defvar *lbge-skip-test*)

(deftest tga-test
    (let ((image (tga "test-file.tga")))
      (ok (instance-of-p 'lbge.image-loader::image image))
      (ok (and (= 128 (width  image))
               (= 128 (height image))
               (= "rgb8" (channels image))))))

(defun instance-of-p (class obj)
  (eq class (type-of obj)))
