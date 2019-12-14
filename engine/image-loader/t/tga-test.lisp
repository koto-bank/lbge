(defpackage :lbge.test.tga
  (:use :cl :rove :lbge.image-loader.image)
  (:import-from :alexandria))

(in-package :lbge.test.tga)

(defun instance-of-p (class obj)
  (eq class (type-of obj)))

(deftest tga-test
    (let ((image (lbge.image-loader.tga:tga "test-file.tga"))
          (test-data (alexandria:read-file-into-byte-vector)))
      (ok (instance-of-p 'lbge.image-loader.image::image image)())
      (ok (= 128 (width  image)))
      (ok (= 128 (height image)))
      (ok (string= "rgba8" (channels image)))
      (ok (= (data image) (subseq test-data 0 (- (* 128 128) 1))))))
