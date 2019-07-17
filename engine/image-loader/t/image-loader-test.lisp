(defpackage lbge.image-loader-test
  (:use :cl :rove))
(in-package lbge.image-loader-test)

(deftest make-image-test
  (ok (instance-of-p
          'lbge.image-loader::image
         (make-image :width 50 :height 50 :channels "rgb8" :data "raw-data"))))

(deftest load-image-test
  (ok (instance-of-p
          'lbge.image-loader::image
          (load-image "test-file.tga")))
  (testing "Error throws"
    (ok (signals (load-image 1)))
    (ok (signals (load-image 'a)))
    (ok (signals (load-image '())))
    (ok (signals (load-image '(a b c))))
    (ok (signals (load-image :a)))
    (ok (signals (load-image "test-file.png"))))) ;; While png isn't support

(deftest cond-extension
    (ok (expands
         '(lbge.image-loader::cond-extension "/path/to/file" (".ext" (some code)))
         '(if (search ".ext" path :from-end t)
            (some code)
            (lbge.image-loader::cond-extension path nil)))))


(defun instance-of-p (class obj)
  (eq class (type-of obj)))
