(in-package :lbge.image)

(defclass image-asset-handler (lbge.asset:asset-handler) ())

(defun image-asset-type ()
  :image)

(defmethod lbge.asset:handler-get-type ((h image-asset-handler))
  (image-asset-type))

(defmethod lbge.asset:handler-get-asset ((handler image-asset-handler) asset-manager key)
  (assert (eq (slot-value key 'lbge.asset:asset-type) (image-asset-type)) nil)
  (let ((asset (make-instance 'lbge.asset:asset))
        (file-path (lbge.asset:find-asset-file-by-path
                    (lbge.asset:asset-roots asset-manager)
                    (slot-value key 'lbge.asset:path))))
    (log:debug "File path ~S" file-path)
    (log:debug "Asset key ~S" key)
    (if file-path
      (let ((image (load-image file-path)))
        (if image
          (setf (lbge.asset:asset-data asset) image
                (lbge.asset:asset-state asset) :loaded)
          (setf (lbge.asset:asset-state asset) :loaded
                (lbge.asset:asset-data asset) (format nil "Failed to load image '~A'" (slot-value key 'lbge.asset:path)))))
      ;; Else
      (setf (lbge.asset:asset-data asset) (format nil "File '~A' not found" (slot-value key 'lbge.asset:path))
            (lbge.asset:asset-state asset) :error))
    asset))
