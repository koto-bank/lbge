(in-package :lbge.application)

;;; Engine supports a number of command line options you can
;;; pass to the sbcl interpreter.
;;; --app-root - absolute path to the application root.
;;; Various components (e.g. asset facility) depend on it.

(defun get-arg-helper (arg args)
  (if (or (null args)
          (null (cdr args)))
    nil
    (if (string= arg (car args))
      (cadr args)
      (get-arg-helper arg (cdr args)))))

(defun get-arg (arg)
  "Find runtime option with name `arg'"
  (get-arg-helper arg sb-ext:*posix-argv*))
