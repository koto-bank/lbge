(defpackage :lbge.test.tga
  (:use :cl :rove :lbge.image-loader.image)
  (:import-from :alexandria))

(in-package :lbge.test.tga)

(defun instance-of-p (class obj)
  (eq class (type-of obj)))

(deftest tga-test
    (let ((image (lbge.image-loader.tga:tga "t/test-file-no-rsa.tga"))
          (test-data (alexandria:read-file-into-byte-vector
                      "t/test-file-no-rsa.tga.image-data")))
      (ok (instance-of-p 'lbge.image-loader.image::image image)())
      (ok (= 1419 (width  image)))
      (ok (= 1001 (height image)))
      (ok (string= "rgba8" (channels image)))
      (ok (= (data image) test-data))))
