(in-package :lbge.beacon)

;;; Beacon can be linked to an unlimited number of callbacks,
;;; which will be called when someone orders beacon to blink.
;;; Some restrictions apply: e.g. you cannot connect the same
;;; callback twice.
;;; Technically, a beacon is a (:<name> callback-list) pair.

(defun make (name)
  (assert (eql (type-of name) 'keyword) nil
          "Beacon name should be a keyword")
  (cons name (list)))

(defun name (beacon)
  (car beacon))

(defun links (beacon)
  (cdr beacon))

(defun (setf links) (new-value beacon)
  (rplacd beacon new-value))

(defun link (beacon callback)
  (assert (not (find callback (links beacon))) nil
          "Callback ~S already linked to beacon ~S"
          callback (name beacon))
  (push callback (links beacon)))

(defun unlink (beacon callback)
  (setf (links beacon)
        (delete callback (links beacon))))

(defun unlink-all (beacon)
  (rplacd beacon nil))

(defun blink (beacon &rest args)
  "Call all linked callbacks with `args'"
  (mapcar (ax:rcurry #'apply args)
          (reverse (links beacon))))
