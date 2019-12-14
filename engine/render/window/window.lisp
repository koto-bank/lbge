(in-package :lbge.render)

(defclass window ()
  ((w :type :integer)
   (h :type :inteder))
  (:documentation "Base window"))

(defun create-window (renderer)
  (push (make-window) (slot-value renderer 'windows)))
