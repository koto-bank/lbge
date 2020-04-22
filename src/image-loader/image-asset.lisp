(in-package :lbge.image)

(defclass image-asset-handler (a:asset-handler) ())

(defun image-asset-type ()
  :image)

(defmethod a:handler-get-type ((h image-asset-handler))
  (:image))

(defmethod a:handler-get-asset ((handler image-asset-handler) asset-manager key)
  (assert (eq (slot-value key 'a:asset-type) (image-asset-type)) nil)
  (let ((asset (make-instance 'a:asset))
        (file-path (a:find-asset-file-by-path
                    (a:asset-roots asset-manager)
                    (slot-value key 'a:path))))
    (log:debug "File path ~S" file-path)
    (log:debug "Asset key ~S" key)
    (if file-path
      (let ((image (load-image file-path)))
        (if image
          (setf (a:asset-data asset) image
                (a:asset-state asset) :loaded)
          (setf (a:asset-state asset) :loaded
                (a:asset-data asset) (format nil "Failed to load image '~A'" (slot-value key 'a:path)))))
      ;; Else
      (setf (a:asset-data asset) (format nil "File '~A' not found" (slot-value key 'a:path))
            (a:asset-state asset) :error)))
  asset)
