(defpackage :lbge-unit-tests
  (:use :cl :rove :lbge.utils)
  (:local-nicknames (:ax :alexandria))
  (:export :run :collect-test-packages :run-on-travis-agent))

(in-package :lbge-unit-tests)

(defmacro define-test (name use-packages &body body)
  (let ((actual-name (make-test-package-name (symbol-name name))))
    `(deftest ,actual-name ,@body)))

(defun load-test-file (file-spec)
  (let ((saved-package *package*))
    (with-open-file (source file-spec)
      (uiop:with-temporary-file (:stream temp-file
                                 :pathname temp-path)
        (prepare-temp-file source temp-file)
        (finish-output temp-file)
        (file-position temp-file :start)
        (loop
          :for form := (read temp-file nil nil) :while form
          :do (eval form))))
    (setf *package* saved-package)))

(defun prepare-uses (form)
  (loop :for n :in form :if (keywordp n) :collect n))

(defun prepare-nicknames (form)
  (ax:when-let (nicks (loop :for n :in form :if (listp n) :collect n))
    (list (cons :local-nicknames nicks))))

(defun get-form (source temp-packages &aux (pos (file-position source)))
  "One huge hack to read form robustly, i.e. create non-existing packages,
intern symbols not yet present in them, and reread the form"
  (restart-case
      (handler-bind
          ((sb-int:simple-reader-package-error
             (lambda (c &aux (l (slot-value c 'sb-kernel::format-arguments)))
               (if (= 2 (length l))
                 (let ((pkg (find-package (second l)))
                       (sym-name (first l)))
                   (export (intern sym-name pkg)
                           pkg))
                 (push
                  (make-package
                   (let ()
                     (if (= 2 (length l))
                       (second l)
                       (first l))))
                  temp-packages))
               (invoke-restart 'try-again)))
           (type-error
             (lambda (c)
               (declare (ignore c))
               (invoke-restart 'try-again))))
        (read source nil nil))
    (try-again ()
      (file-position source pos)
      (get-form source temp-packages))))

(defun prepare-temp-file (source temp-file)
  (let ((temp-packages (list)))
    (loop
      :for form := (get-form source temp-packages) :while form
      :do (progn
            (if (string= "DEFINE-TEST" (symbol-name (first form)))
              (let ((package-name (make-test-package-name (second form)))
                    (use-form (prepare-uses (third form)))
                    (nicknames-form (prepare-nicknames (third form))))
                (println (append (list 'defpackage package-name)
                                 (list (append '(:use :cl :rove :lbge-unit-tests)
                                               use-form))
                                 nicknames-form)
                         temp-file)
                (println (list 'in-package package-name) temp-file)
                (println (macroexpand (rplaca form 'define-test)) temp-file))
              (println form temp-file))))
    (mapcar #'delete-package temp-packages)))

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
    (let ((*package* (find-package :lbge-unit-tests)))
      (mapcar #'load-test-file
              (remove-if (lambda (path)
                           (not (string= (pathname-type path) "lisp")))
                         files)))))

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

(defun make-test-package-name (test-name)
  (ax:make-keyword
   (concatenate 'string "LBGE.TEST."
                (string-upcase test-name))))

(defun run-selected-tests (selected-tests)
  (let ((processed-names
          (mapcar #'make-test-package-name selected-tests)))
    (mapcar #'run-selected-test processed-names)))

(defun run-selected-test (selected-test)
  (let ((suite (find-package selected-test)))
    (if suite
      (rove:run-suite suite)
      (format t "Could not run test suite: package ~A not found~%" selected-test))))

(defun report-results ()
  (let ((failed (rove/core/stats:stats-failed rove:*stats*))
        (passed (rove/core/stats:stats-passed rove:*stats*)))

    (when (> (length passed) 0)
      (format t "~%-------------~%Passed tests:~%")
      (loop :for test :across (rove/core/stats:stats-passed rove:*stats*) :do
        (format t "* ~A~%" (rove/core/result:test-name test))))
    (when (> (length failed) 0)
      (format t "~%-------------~%Failed tests:~%")
      (loop :for test :across failed :do
        (format t "* ~A~%" (rove/core/result:test-name test))))
    (when (= (length failed) 0)
      (format t "~%All tests passed successfully!~%"))
    (= 0 (length failed))))

(defun print-start-message ()
  (terpri)
  (terpri)
  (terpri)
  (princ ",---------------------------------------.") (terpri)
  (princ "| :~-<==={ Starting unit tests }===>-~: |") (terpri)
  (princ "`---------------------------------------'") (terpri)
  (terpri))

(defun run (&rest selected-tests)
  "Run unit tests and report the result.
`reporter`: rove reporter. Default is :spec
`selected-test-suite`: if present, only test suite with this name will run.
Name must be string designator of the full suite name, e.g. `:lbge.test.engine'"
  (delete-test-packages)
  (load-test-files)
  (rove:use-reporter :spec)
  (let* ((all-packages (collect-active-tests))
         (filtered-packages (filter-disabled-packages all-packages)))
    (when selected-tests
      (run-selected-tests selected-tests)
      (return-from run (report-results)))
    (print-start-message)
    (mapcar #'rove:run-suite filtered-packages)
    (report-results)))

(defun run-on-travis-agent ()
  (unless (run)
    (sb-ext::quit :unix-status 1)))
