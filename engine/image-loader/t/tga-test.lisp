(defpackage :lbge.test.tga
  (:use :cl :rove :lbge.image-loader.image))

(in-package :lbge.test.tga)

(defvar *lbge-skip-test*)

(defun instance-of-p (class obj)
  (eq class (type-of obj)))

(deftest tga-test
    (let ((image (lbge.image-loader.tga:tga "test-file.tga")))
      (ok (instance-of-p 'lbge.image-loader.image::image image)())
      (ok (= 128 (width  image)))
      (ok (= 128 (height image)))
      (ok (string= "rgba8" (channels image)))))
