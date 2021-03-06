(in-package :lbge.asset)

(defclass asset-key (s:serializable)
  ((asset-type
    :documentation
    "Type of the asset.
E.g. image, geometry, effect"
    :initform nil
    :initarg :asset-type
    :reader asset-type
    :serialize t)
   (type
    :documentation
    "Common key types for various purposes:
:disk for assets to be loaded from/saved to disk
:memory for assets to be created in-memory"
    :initform nil
    :initarg :type
    :serialize :t
    :reader key-type)
   (path
    :documentation
    "For :disk asset keys should denote path to file.
For :memory may be just an asset name or whatever"
    :initform nil
    :initarg :path
    :serialize t)
   (options
    :documentation
    "Options for use by asset handler. Implementation specific"
    :initarg :options
    :initform nil
    :serialize t))
  (:documentation
   "Asset key is unique asset identifier.")
  (:metaclass s:serializable-class))

(defun make-asset-key (asset-type key-type path &optional options)
  (make-instance 'asset-key :asset-type asset-type
                            :type key-type
                            :path path
                            :options options))

;;; Metaclass for asset class
;;; for :dep slot option support, so we can easily get
;;; all slots containing dependencies for every asset class
(defclass asset-class (s:serializable-class) ())

(defclass asset-direct-slot (s:serializable-direct-slot)
  ((dep :initarg :dep
        :initform nil
        :accessor direct-slot-asset-dep)))

(defclass asset-effective-slot (s:serializable-effective-slot)
  ((dep :initarg :dep
        :initform nil
        :accessor effective-slot-asset-dep)))

(defun asset-effective-slot-p (slot)
  (eq 'asset-effective-slot (type-of slot)))

(defmethod closer-mop:direct-slot-definition-class ((class asset-class) &rest initargs)
  (find-class 'asset-direct-slot))

(defmethod closer-mop:effective-slot-definition-class ((class asset-class) &rest initargs)
  (find-class 'asset-effective-slot))

(defun is-dep-an-asset (dependency-slot)
  (unless dependency-slot
    (return-from is-dep-an-asset nil))
  (eq (class-of
       (find-class
        (closer-mop:slot-definition-type dependency-slot)))
      (find-class 'asset-class)))

(defun is-asset (object)
  (eq (find-class 'asset-class)
      (u:metaclass-of object)))

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
          (remove-if #'is-dep-an-asset dependencies)))

(defmethod closer-mop:compute-effective-slot-definition ((class asset-class) name direct-slots)
  (let ((slot (call-next-method))
        (direct-slot (first direct-slots)))
    (when (closer-mop:subclassp (class-of direct-slot)
                                (find-class 'asset-direct-slot))
      (setf (effective-slot-asset-dep slot)
            (direct-slot-asset-dep direct-slot))) ; override the dep
    slot))

(defmethod closer-mop:compute-slots ((class asset-class))
  (let* ((all-slots (call-next-method))
         (asset-slots (remove-if-not #'asset-effective-slot-p all-slots))
         (deps (remove-if-not #'identity asset-slots :key #'effective-slot-asset-dep)))
    (assert (every #'is-dep-an-asset deps)
            nil "Error while defining asset class ~A
Every asset dependence should have :type which must be defined with `define-asset'
Dependencies in question:~:{ ~A, type: ~A, class: ~A~}"
            class
            (get-invalid-deps deps))
    (cons (make-instance 'asset-effective-slot
                         :allocation :class
                         :allocation-class class
                         :name '%dependencies
                         :initfunction #'(lambda () deps)
                         :documentation "Asset dependencies"
                         :initform deps)
          all-slots)))

(defclass asset (s:serializable)
  ((state
    :documentation
    "Current asset state:
:void - not loaded/created
:loaded - successfully loaded/created
:error - an error has occured during loading/creation"
    :initform :void
    :initarg :state
    :accessor asset-state)
   (key
    :documentation "Asset key"
    :type 'asset-key
    :initarg :asset-key
    :accessor asset-key))
  (:documentation "Base class for all assets")
  (:metaclass asset-class))

(defmacro define-asset (name &body body)
  "Define an asset, which is a subclass of the asset class, and can
depend on other assets.

When loading an asset, dependenctes will be loaded automatically.

To mark slot as a dependent asset, you must add `:dep t' argument
to it  and specify its type, which must also be inherited from
asset. Type will be checked at the moment of class definition.

The slot may contain also a collection of there dependencies. In such
case they will be processed individually"
  (let ((base-class-list (car body)))
    (unless (some (u:bind #'closer-mop:subclassp
                          (find-class _)
                          (find-class 'lbge.asset:asset))
                  base-class-list)
      (setf base-class-list (cons 'asset base-class-list)))
    `(defclass ,name ,base-class-list
       ,@(cdr body)
       (:metaclass asset-class))))

(defun asset-deps (asset)
  (mapcar (lambda (dep-slot)
            (cons
             (closer-mop:slot-definition-name dep-slot)
             (when (slot-boundp asset
                                (closer-mop:slot-definition-name dep-slot))
               (slot-value asset
                           (closer-mop:slot-definition-name dep-slot)))))
          (slot-value asset '%dependencies)))

(defun dependency-type (asset dep-slot)
  (closer-mop:slot-definition-type
   (find dep-slot
         (slot-value asset '%dependencies)
         :key #'closer-mop:slot-definition-name)))

(defgeneric make-asset (asset-key &rest rest))

;;; Default method for all asset classes (they are instances of the
;;; asset-class metaclass)
(defmethod make-asset ((key asset-key) &rest rest)
  ;; If the :state is provided in :rest arguments,
  ;; it will override the default value listed here
  (apply #'make-instance (append (list [key.asset-type] :asset-key key) rest)))


;;; Asset serialization
(defmethod s:serialize ((asset asset))
  "Default serialization for asset: dump all dependencies as ~
their asset keys."
  (append
   (call-next-method)
   (for-each-dependency asset (slot-name dep)
     do (unless (null dep)
          (assert (is-asset dep) nil "Dependency ~A of ~A is not an asset (how did this even happened?)" dep asset))
     if (slot-boundp asset slot-name)
     collect (list slot-name [dep.key s:serialize])
     else collect (list slot-name nil))))

(defmethod s:deserialize ((asset asset) form &optional options)
  (call-next-method)
  (for-each-dependency asset (dep-slot dep)
    do (let* ((dep-type (dependency-type asset dep-slot))
              (dep-data (find dep-slot form :key #'car)))
         (assert dep-data
                 nil "Data for slot ~S not found in deps" dep-slot)
         ;; dep currently is an asset key. We need to create an asset
         ;; of appropriate type with this key and set it as the slot
         ;; value
         (assert dep nil "Can't create asset without an asset key")
         (setf (slot-value asset dep-slot)
               (make-asset dep))))
  asset)
