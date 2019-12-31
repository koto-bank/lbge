(in-package :lbge.filesystem)

(defun get-app-root ()
  (let ((app-root-arg
          (lbge.application:get-arg "--app-root")))
    (if (null app-root-arg)
      (asdf:system-source-directory 'lbge)
      (make-pathname :directory app-root-arg))))
