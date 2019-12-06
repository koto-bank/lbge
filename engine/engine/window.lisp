(in-package :lbge.engine)

(defclass window ()
  ()
  (:documentation "Base window interface"))

(defgeneric initialize-backend (window backend))
(defgeneric finalize-window (window))
