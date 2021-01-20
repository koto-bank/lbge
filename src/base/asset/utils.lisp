(in-package :lbge.asset)

(defmacro define-asset-handler (asset-class (asset-manager-var asset-key-var) &body get-asset-body)
  "Syntax: class of the asset type to handle, must be a symbol
get-asset-body - function accepting asset manager and asset key.
Must return the asset in a proper state"
  (flet ((make-handler-class-name (asset-name)
           (intern (concatenate 'string (string asset-name) "-ASSET-HANDLER") 'lbge.asset)))
    (assert (symbolp asset-class) nil "Asset type name must be a symbol, but got ~A" (type-of asset-class))
    (let ((class-name (make-handler-class-name asset-class)))
      `(progn
         (defclass ,class-name (asset-handler)
           ((type :initform ',asset-class :accessor asset-type)))

         (defmethod handler-get-asset ((,(gensym) ,class-name) ,asset-manager-var ,asset-key-var)
           (assert (eq (find-class ',asset-class)
                       (find-class (slot-value ,asset-key-var 'asset-type)))
                   nil
                   "Asset key type ~A is incompatible with current asset handler, which has type ~A"
                   (slot-value ,asset-key-var 'asset-type)
                   ',asset-class)
           (block nil
             ,@get-asset-body))))))
