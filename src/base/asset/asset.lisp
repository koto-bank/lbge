(in-package :lbge.asset)

(defclass asset-key ()
  ((asset-type
    :documentation
    "Type of the asset.
E.g. image, geometry, effect"
    :initform nil
    :initarg :type)
   (key-type
    :documentation
    "Common key types for various purposes:
:disk for assets to be loaded from/saved to disk
:memory for assets to be created in-memory"
    :initform nil
    :initarg :key-type)
   (path
    :documentation
    "For :disk asset keys should denote path to file.
For :memory may be just an asset name or whatever"
    :initform nil
    :initarg :path)
   (options
    :documentation
    "Options for use by asset handler. Implementation specific"
    :initarg :options
    :initform nil))
  (:documentation
   "Asset key is unique asset identifier."))

(defun make-asset-key (asset-type key-type path &optional options)
  (make-instance 'asset-key :type asset-type
                            :key-type key-type
                            :path path
                            :options options))

(defclass asset ()
  ((state
    :documentation
    "Current asset state:
:void - not loaded/created
:loaded - successfully loaded/created
:error - an error has occured during loading/creation"
    :initform :void
    :accessor asset-state)
   (data
    :documentation "Asset data. Type dependent, so may be whatever"
    :accessor asset-data)
   (key
    :documentation "Asset key"
    :type 'asset-key
    :accessor asset-key))
  (:documentation "Base class for all assets"))
