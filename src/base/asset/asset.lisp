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

;;; Metaclass for asset class
;;; for :dep slot option support, so we can easily get
;;; all slots containing dependencies for every asset class
(defclass asset-class (standard-class) ())

(defclass asset-direct-slot (closer-mop:standard-direct-slot-definition)
  ((dep :initarg :dep
        :initform nil
        :accessor direct-slot-asset-dep)))

(defclass asset-effective-slot (closer-mop:standard-effective-slot-definition)
  ((dep :initarg :dep
        :initform nil
        :accessor effective-slot-asset-dep)))

(defmethod closer-mop:direct-slot-definition-class ((class asset-class) &rest initargs)
  (find-class 'asset-direct-slot))

(defmethod closer-mop:effective-slot-definition-class ((class asset-class) &rest initargs)
  (find-class 'asset-effective-slot))

(defun metaclass-of (object)
  (class-of (class-of object)))

(defun is-asset (dependency-slot)
  (unless dependency-slot
    (return-from is-asset nil))
  (eq (class-of
       (find-class
        (closer-mop:slot-definition-type dependency-slot)))
      (find-class 'asset-class)))

(defun get-invalid-deps (dependencies)
  "Find dependencies which don't have type or it is not an asset class
defined with `define-asset'"
  (mapcar (lambda (dep)
            (let ((type (closer-mop:slot-definition-type dep)))
              (list (closer-mop:slot-definition-name dep)
                    type
                    (if type
                      (class-of (closer-mop:slot-definition-type dep))
                      'n/a))))
          (remove-if #'is-asset dependencies)))

(defmethod closer-mop:compute-effective-slot-definition ((class asset-class) name direct-slots)
  (let ((slot (call-next-method))
        (direct-slot (first direct-slots)))
    (when (eq (class-of direct-slots)
              (find-class 'asset-direct-slot))
      (setf (effective-slot-asset-dep slot)
            (direct-slot-asset-dep direct-slot))) ; override the dep
    slot))

(defmethod closer-mop:compute-slots ((class asset-class))
  (let* ((all-slots (call-next-method))
         (deps (remove-if-not #'identity all-slots :key #'effective-slot-asset-dep)))
    (assert (every #'is-asset deps)
            nil "Every asset dependence should have :type which must be defined with `define-asset'
Dependencies in question:~:{ ~A, type: ~A, class: ~A~}"
            (get-invalid-deps deps))
    (cons (make-instance 'asset-effective-slot
                         :allocation :class
                         :allocation-class class
                         :name '%dependencies
                         :initfunction #'(lambda () deps)
                         :documentation "Asset dependencies"
                         :initform deps)
          all-slots)))

(defmethod closer-mop:validate-superclass ((class asset-class)
                                           (super standard-class))
  t)

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
