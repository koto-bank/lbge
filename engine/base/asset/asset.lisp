(in-package :lbge.asset)

(defclass asset-key ()
  ((asset-type
    :documentation
    "Type of the asset.
E.g. image, geometry, effect"
    :initform nil)
   (key-type
    :documentation
    "Common key types for various purposes:
:disk for assets to be loaded from/saved to disk
:memory for assets to be created in-memory"
    :initform nil)
   (path
    :documentation
    "For :disk asset keys should denote path to file.
For :memory may be just an asset name or whatever"
    :initform nil))
  (:documentation
   "Asset key is unique asset identifier."))

(defclass asset ()
  ()
  (:documentation "Base class for all assets"))
