(defpackage :lbge-unit-tests
  (:use :cl :rove)
  (:export :run :collect-test-packages))

(in-package :lbge-unit-tests)

(defun collect-test-files (root)
  (let (files)
    (uiop:collect-sub*directories
     root t t
     (lambda (dir)
       (if (uiop:string-suffix-p (directory-namestring dir) "/t/")
           (uiop:collect-sub*directories
            dir t t
            (lambda (dir)
              (setf files (nconc files (uiop:directory-files dir))))))))
    files))

(defun load-test-files ()
  (let* ((root-dir (asdf:system-source-directory :lbge))
         (files (collect-test-files root-dir)))
    (mapcar #'load
            (remove-if (lambda (path)
                         (not (string= (pathname-type path) "lisp")))
                       files))))

(defun test-package-p (package)
  "Test if provided package has `lbge.test.' prefix.
If it does, then it is a test package."
  (let ((name (package-name package)))
    (if (< (length name) 10)
        nil
        (string= "LBGE.TEST." (subseq name 0 10)))))

(defun collect-test-packages ()
  (let ((packages (list-all-packages)))
    (delete-if-not #'test-package-p packages)))

(defun run ()
  (load-test-files)
  (let ((test-packages (collect-test-packages)))
    (mapcar #'rove:run-suite test-packages)))
