(in-package :lbge.asset)

(defvar *asset-manager*)

(defclass asset-manager ()
  ((roots
    :initform (list))
   (loaded-assets
    :initform (make-hash))
   (asset-handlers
    :initform (list))))

(defun add-root (asset-manager new-root)
  (push new-root (slot-value asset-manager 'asset-roots)))

(defun get-asset (asset-manager asset-key)
  (let* ((loaded-assets (slot-value *asset-manager* 'loaded-assets))
         (asset (get-hash loaded-assets asset-key)))
    (if (not (null asset))
        assetp
        (load-asset asset-manager asset-key))))

(defun load-asset (asset-manager asset-key)
  "Load asset by key.
Asset handler for appropriate asset type must be registered.
After load it will be stored in the loaded assets storage."
  (let ((handler (assoc
                  (key-type asset-key)
                  (slot-value *asset-manager* asset-handlers))))
    (assert handler nil "Asset handler for type ~S not found"
            (key-type asset-key))
    (load-asset handler asset-key)))
