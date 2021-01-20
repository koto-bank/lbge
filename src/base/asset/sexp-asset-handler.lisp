(in-package :lbge.asset)

(define-asset sexp () ((sexp :initarg :sexp)))

(define-asset-handler sexp (asset-manager key)
  (let ((package-to-intern [key.options])
        (file-path (find-path-by-path-key asset-manager key)))
    (unless file-path
      (return (make-instance 'sexp :state :error)))
    (let (data)
      (let ((*package* (or package-to-intern *package*)))
        (setf data (read (open file-path))))
      (make-instance 'sexp :sexp data :state :loaded))))
