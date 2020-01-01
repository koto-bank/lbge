(in-package :lbge.asset)

(defclass sexp-asset-handler (asset-handler)
  ())

(defmethod handler-get-asset ((handler sexp-asset-handler) asset-manager key)
  (assert (eq (slot-value key 'asset-type) :sexp-data) nil)
  (let ((asset (make-instance 'asset))
        (package-to-intern (slot-value key 'options))
        (file-path (find-asset-file-by-path
                    (asset-roots asset-manager)
                    (slot-value key 'path))))
    (if file-path
      (let (data)
        (let ((*package* (or package-to-intern *package*)))
          (setf data (read (open file-path))))
        (setf (asset-data asset)
              data
              (asset-state asset) :loaded))
      (setf (asset-data asset) "Error loading"
            (asset-state asset) :error))
    asset))
