(defpackage :lbge-unit-tests
  (:use :cl :rove)
  (:export :run :collect-test-packages :run-on-travis-agent))

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

(defun collect-all-packages (&optional selected-package)
  (let* ((packages (list-all-packages))
         (lbge-packages (delete-if-not #'test-package-p packages)))
    (if selected-package
        (let ((s (string selected-package)))
          (delete-if-not (lambda (package)
                           (string= s (subseq (package-name package)
                                              10
                                              (+ 10 (length s)))))
                         lbge-packages))
        lbge-packages)))

(defun delete-test-packages ()
  (let ((all-packages (collect-all-packages)))
    (mapcar #'delete-package all-packages)))

(defun collect-test-packages (&optional selected-package)
  (filter-disabled-packages (collect-all-packages selected-package)))

(defun test-package-disabled-p (package)
  (find-symbol "*LBGE-SKIP-TEST*" package))

(defun filter-disabled-packages (package-list)
  (delete-if #'test-package-disabled-p package-list))

(defun run (&key (reporter :spec) selected-package)
  (delete-test-packages)
  (load-test-files)
  (rove:use-reporter reporter)
  (let* ((all-packages (collect-test-packages selected-package))
         (filtered-packages (filter-disabled-packages all-packages)))
    (format t "filtered packages: ~S~%" filtered-packages)
    (mapcar #'rove:run-suite filtered-packages)
    ;; if all tests passed?
    (= 0 (length (rove/core/stats:stats-failed rove:*stats*)))))

(defun run-on-travis-agent ()
  (unless (run)
    (sb-ext::quit :unix-status 1)))
