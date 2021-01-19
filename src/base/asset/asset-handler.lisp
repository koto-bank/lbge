(in-package :lbge.asset)

(defclass asset-handler ()
  ()
  (:documentation "Base class for all asset handlers"))

(defgeneric handler-get-asset (handler asset-manager key)
  (:documentation "Create or load assets by key.
Implementation decides which asset keys are supported, what options
are needed for loading, and so on"))

(defgeneric handler-get-type (handler)
  (:documentation "Return type of the asset, e.g. :sexp-data, :glsl-source
and so on. Each handler must define this method"))

(defmethod handler-get-type ((h asset-handler))
  (error "Method handler-get-type must be defined for every asset-handler"))

(defun find-asset-file-by-path (asset-roots path)
  "Find asset file by asset path"
  (let* ((parsed-path (f:parse-asset-path path))
         (root-key (car parsed-path))
         (path (cdr parsed-path))
         (root (cdr (assoc root-key asset-roots))))
    (assert root nil "Root with key ~S (from asset path ~S) is not registered" root-key path)
    (let* ((merged-path (f:merge-paths root path)))
      (log:debug "Parsed asset path: ~S" parsed-path)
      (log:debug "Root path: ~S" root)
      (log:debug "Merged path ~S" merged-path)
      (f:is-file merged-path))))

(defun find-path-by-path-key (asset-manager asset-key)
  (assert (eq [asset-key.key-type] :disk)
          nil "Can find asset file path only for :disk assets.
Got ~A key type instead" [asset-key.key-type])
  (find-asset-file-by-path
   (asset-roots asset-manager)
   [asset-key.path]))
