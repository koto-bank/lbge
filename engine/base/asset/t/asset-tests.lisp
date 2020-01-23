(defpackage :lbge.test.assets
  (:use :cl :rove :lbge.asset :lbge.filesystem))

(in-package :lbge.test.assets)

(deftest asset-path-test
  (testing "Asset path parsing"
    (let* ((path ":texture/path/to/file.tga")
           (reference-path (parse-namestring "path/to/file.tga"))
           (parsed-path (parse-asset-path path)))
      (ok (eql :texture (car parsed-path)))
      (ok (path-eq (cdr parsed-path) reference-path)))))

(deftest asset-manager-test
  (let ((a (make-instance 'asset-manager)))
    (add-root a :tmp (make-path :dir "/tmp"))
    (testing "Asset manager roots"
      (let ((roots (asset-roots a)))
        (ok (assoc :tmp roots))
        (ok (path-eq (cdr (assoc :tmp roots))
                     (make-path :dir "/tmp/")))))
    (add-root a :test-dir (root-relative-path "base/asset/t"))
    (add-handler a :sexp-data (make-instance 'sexp-asset-handler))
    (testing "Asset manager loading sexp assets"
      (let* ((sexp-data-key (make-asset-key :sexp-data :disk ":test-dir/test-data.data"
                                            (find-package :lbge.test.assets)))
             (test-asset (get-asset a sexp-data-key)))
        (ok test-asset)
        (ok (eq (asset-state test-asset) :loaded))
        (ok (equal (asset-data test-asset) (list 'henlo 'world)))))))
