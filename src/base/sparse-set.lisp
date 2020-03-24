(in-package :lbge.sparse-set)

(defmacro id (elem)
  `(ldb (byte 48 0) ,elem))

(defmacro gen (elem)
  `(ldb (byte 16 48) ,elem))

(defun existsp (elem sparse packed)
  "Check if the element exists in the set"
  (declare ((unsigned-byte 64) elem))
  (let ((id (id elem)))
    (when (>= id (array-total-size sparse))
      (return-from existsp nil))
    (= elem (aref packed
                  (aref sparse id)))))

(defun get-extend-size (vector)
  (if (= 0 (array-total-size vector))
    8
    (array-total-size vector)))

(defun insert (elem sparse packed &optional data packed-data)
  "Insert new element to the set.
Sparse and packed vectors are extended, if necessary.
If data and packed-data are provided, data is pushed to
the packed-data array"
  (declare ((unsigned-byte 64) elem))
  (let ((id (id elem)))
    (when (>= id (array-total-size sparse))
      (adjust-array sparse (list (1+ id))))
    (setf (aref sparse id)
          (vector-push-extend elem packed (get-extend-size packed)))
    (when (and data packed-data)
      (vector-push-extend data packed-data (get-extend-size packed-data)))))

(defun remove-swap-last (vector index)
  "Swaps element at index with last, shrinks vector and returns the swapped element"
  (let ((last-index (1- (fill-pointer vector))))
    (setf (aref vector index) (aref vector last-index))
    (setf (fill-pointer vector) last-index)
    (aref vector index)))

(defun get (elem sparse packed &optional data)
  "If data provided, get assosiated data, when element is present in
the set.
If data is not provided, return elem, if it is present in the set.
Otherwise return nil"
  (declare ((unsigned-byte 64) elem))
  (let ((id (id elem)))
    (when (>= id (array-total-size sparse))
      (return-from get nil))
    (unless (existsp elem sparse packed)
      (return-from get nil))
    (when data
      (return-from get (aref data (aref sparse id))))
    elem))

(defun remove (elem sparse packed &optional packed-data)
  "Remove an element with id from the set"
  (declare ((unsigned-byte 64) elem))
  (unless (existsp elem sparse packed)
    (return-from remove nil))
  (let* ((packed-index (aref sparse (id elem)))
         (swapped (remove-swap-last packed packed-index)))
    (when packed-data
      (remove-swap-last packed-data packed-index))
    (setf (aref sparse (id swapped))
          packed-index)))

(defun clear (sparse packed &optional data)
  (adjust-array sparse '(0))
  (adjust-array packed '(0) :fill-pointer 0)
  (when data
    (adjust-array data '(0) :fill-pointer 0)))
