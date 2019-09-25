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

(defun collect-all-packages ()
  (delete-if-not #'test-package-p (list-all-packages)))

(defun delete-test-packages ()
  "Find and delete all test packages"
  (let ((all-test-packages (collect-all-packages)))
    (mapcar #'delete-package all-test-packages)))

(defun collect-active-tests ()
  (filter-disabled-packages (collect-all-packages)))

(defun test-package-disabled-p (package)
  (find-symbol "*LBGE-SKIP-TEST*" package))

(defun filter-disabled-packages (package-list)
  (delete-if #'test-package-disabled-p package-list))

(defun run-selected-test-suite (packages selected-test-suite)
  (let ((suite (find-package selected-test-suite)))
    (if suite
      (rove:run-suite suite)
      (format t "Could not run test suite: package ~A not found~%" selected-test-suite))))

(defun print-start-message ()
  (terpri)
  (terpri)
  (terpri)
  (princ ",---------------------------------------.") (terpri)
  (princ "| :~-<==={ Starting unit tests }===>-~: |") (terpri)
  (princ "`---------------------------------------'") (terpri)
  (terpri))

(defun run (&key (reporter :spec) selected-test-suite)
  "Run unit tests and report the result.
`reporter`: rove reporter. Default is :spec
`selected-test-suite`: if present, only test suite with this name will run.
Name must be string designator of the full suite name, e.g. `:lbge.test.engine'"
  (delete-test-packages)
  (load-test-files)
  (rove:use-reporter reporter)
  (let* ((all-packages (collect-active-tests))
         (filtered-packages (filter-disabled-packages all-packages)))
    (when selected-test-suite
        (run-selected-test-suite filtered-packages selected-test-suite)
        (return-from run (report-results)))
    (print-start-message)
    (mapcar #'rove:run-suite filtered-packages)
    ;; if all tests passed?
    (= 0 (length (rove/core/stats:stats-failed rove:*stats*)))))

(defun run-on-travis-agent ()
  (unless (run)
    (sb-ext::quit :unix-status 1)))
