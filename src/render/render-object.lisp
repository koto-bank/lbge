(in-package :lbge.render)

(defclass batch ()
  ((vertices :documentation "Vertex attrubute array. A simple-vector.
Contents depend on semantics and can vary.  E.g. may include vertex
position, color, texture coordinates, etc."
             :initform (vector) :initarg :vertices)
   (indices :documentation "Index array. A simple-vector"
            :initform (vector) :initarg :indices)))

(defclass semantics ()
  ((attributes-num :documentation "Number of attributes"
                   :initarg :attributes-num)
   (stride :documentation "Attribute stride"
           :initarg :stride)
   (attribute-types :documentation "List of attribute types"
                    :initarg :attribute-types)
   (attribute-sizes :documentation "List of attribyte sizes (in types)"
                    :initarg :attribute-sizes)
   (attribute-offsets :documentation "List of attribute offsets"
                      :initarg :attribute-offsets)))

(defmethod print-object ((sem semantics) stream)
  (with-slots (stride attribute-types attribute-sizes attribute-offsets)
      sem
      (format stream "~
#<Stride: ~S~%~
  types:  ~S~%~
  sizes:  ~S~%~
  offsets: ~S>"
              stride
              attribute-types
              attribute-sizes
              attribute-offsets)))

(defun literal-semantics-definition-p (definition)
  (let ((allowed-attributes '(:vertex :color :texcoord))
        (allowed-types '(:float)))
    (and
     (loop
       :for attr :in definition
       :always (listp attr)
       :always (= 3 (length attr)))
     (loop
       :for (attr type size) :in definition
       :always (member attr allowed-attributes)
       :always (member type allowed-types)))))

(defun parse-semantics-def (definition)
  (let ((allowed-attributes '(:vertex :color :texcoord))
        (allowed-types '(:float))
        (type-byte-size '((:float 4))))
    (mapcar (lambda (def)
              (assert (= 3 (length def))
                      nil "Malformed attribute definition ~S" def)
              (let ((attr (first def))
                    (type (second def))
                    (component-length (third def)))
                (assert (find attr allowed-attributes) nil
                        "Attribute ~S not recognized" attr)
                (assert (find type allowed-types) nil
                        "Attribute type ~S not recognized" type)))
            definition)
    (let ((stride 0)
          (offsets (list))
          (types (list))
          (sizes (list)))
      (mapcar (lambda (def)
                (let* ((type (second def))
                       (component-length (third def))
                       (byte-size (second (assoc type type-byte-size))))
                  (push stride offsets)
                  (push type types)
                  (push component-length sizes)
                  (setf stride (+ stride (* component-length byte-size)))))
              definition)
      (values stride
              (nreverse types)
              (nreverse sizes)
              (nreverse offsets)))))

(defun make-semantics-fn (definition)
  (multiple-value-bind (stride types sizes offsets)
      (parse-semantics-def definition)
    (make-instance 'semantics :attributes-num (length definition)
                              :stride stride
                              :attribute-types types
                              :attribute-sizes sizes
                              :attribute-offsets offsets)))

(defmacro make-semantics (definition)
  (unless (literal-semantics-definition-p definition)
    (return-from make-semantics `(make-semantics-fn ,definition)))
  (multiple-value-bind (stride types sizes offsets)
      (parse-semantics-def definition)
    `(make-instance 'semantics :attributes-num ,(length definition)
                               :stride ,stride
                               :attribute-types ',types
                               :attribute-sizes ',sizes
                               :attribute-offsets ',offsets)))

(defun semantics= (sem-1 sem-2)
  (and (= (slot-value sem-1 'attributes-num)
          (slot-value sem-2 'attributes-num))
       (= (slot-value sem-1 'stride)
          (slot-value sem-2 'stride))
       (equal (slot-value sem-1 'attribute-offsets)
              (slot-value sem-2 'attribute-offsets))))

(defclass render-object ()
  ((batches :documentation "Adjustable vector of batches"
            :initform (make-arrray 0 :adjustable t
                                     :fill-pointer 0)
            :initarg :batches)
   (semantics :documentation "Vertex array semantics descriptor"
              :initarg :semantics)
   (transform :documentation "Transform matrix"
              :initarg :transform
              :accessor transform)
   (material :documentation "Material assigned to this render object"
             :initarg :material
             :accessor material)
   (backend-data :documentation "Backend-dependent data. Opaque"))
  (:documentation "Render object. The thing that contains backend-independent info for rendering"))

(defun make-render-batch (&key indices vertices)
  (make-instance 'batch :vertices vertices :indices indices))

(defun make-render-object (batches semantics material
                           &optional (transform (m:make-transform)))
  (make-instance 'render-object
                 :semantics semantics
                 :batches (make-array (length batches)
                                      :initial-contents batches
                                      :adjustable t
                                      :fill-pointer (length batches))
                 :material material
                 :transform transform))

(defun add-batch (object batch)
  (vector-push-extend batch (slot-value 'batches object)))
