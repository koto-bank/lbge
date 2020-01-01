(in-package :lbge.asset)

(defclass asset-manager ()
  ((roots
    :documentation
    "List of asset roots.
An asset root is a pair (name . path), where name is a keyword, and
path is a path to some location (on disk), absolute or relative.
E.g.: (:texture . #P\"assets/textures\")"
    :initform (list)
    :accessor asset-roots)
   (loaded-assets
    :documentation "Hash map, holding currently loaded assets"
    :initform (make-hash))
   (asset-handlers
    :documentation "An alist of pairs (handler-type . asset-handler)"
    :initform (list))))

(defun add-root (asset-manager root-key new-root)
  (push (cons root-key new-root)
        (asset-roots asset-manager)))

(defun get-asset (asset-manager asset-key)
  "Get already loaded asset or attempt to load/create it by provided key"
  (with-slots (loaded-assets) asset-manager
    (if (null (get-hash loaded-assets asset-key))
      (let ((new-asset (load-asset asset-manager asset-key)))
        (push new-asset loaded-assets)
        new-asset)
      asset)))

(defun add-handler (asset-manager type handler)
"Add an asset handler
`type' is keyword denoting for what asset key types handler will be used (e.g. texture)
`handler' is a constructed handler instance"
  (assert (closer-mop:subclassp (type-of handler)
                                (find-class 'asset-handler))
          nil "Handler must be a subclass of `asset-handler' class")
  (with-slots ((handlers asset-handlers)) asset-manager
    (assert (not (assoc type handlers))
            nil
            "Handler for ~S already registered" type)
    (push (cons type handler) handlers)))

(defun load-asset (asset-manager asset-key)
  "Load asset by key.
Asset handler for appropriate asset type must be registered.
After load it will be stored in the loaded assets storage."
  (let ((handler (assoc
                  (slot-value asset-key 'asset-type)
                  (slot-value asset-manager 'asset-handlers))))
    (assert handler nil "Asset handler for type ~S not found"
            (slot-value asset-key 'asset-type))
    (handler-get-asset (cdr handler) asset-manager asset-key)))
