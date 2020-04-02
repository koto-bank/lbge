(in-package :lbge.ecs)

(defclass world ()
  ((systems :accessor systems
            :initform (list)
            :documentation "List of all registered systems")
   (entity-storage
    :initform (make-instance 'entity-storage)
    :documentation "Sparse set based storage for all entities")
   (component-storage-map
    :accessor component-storage-map
    :initform (h:make-hash)
    :documentation "Map of component type to system that stores it"))
  (:documentation "The world. Contains all entities and systems"))

(defun make-world ()
  (make-instance 'world))

(defun add-system (world system-class)
  "Add system to the world. Order of adding matters"
  (assert (null (find system-class (systems world) :key #'type-of))
          nil "System ~A already added to the world" (system-name system))
  (let ((system (make-instance system-class)))
    (setf (systems world)
          (nconc (systems world) (list system)))
    (loop
      :for comp-storage :in (component-storages system)
      :do (h:hash-set (component-storage-map world)
                      (storage-component-type comp-storage)
                      comp-storage))))

(defun add-systems (world &rest system-classes)
  (mapcar (ax:curry #'add-system world) system-classes))

(defun get-system (world system-type)
  (find system-type (systems world) :key #'type-of))

(defun update-world (world dt)
  "Updates all systems present in the world"
  (mapcar (ax:rcurry #'update-system dt)
          (systems world)))

;;; Entities

(defun create-entity (world &rest components)
  (with-slots (entity-storage) world
    (let ((e (storage-create-entity entity-storage)))
      (loop
        :for c :in components
        :do (add-component (get-component-storage world c)
                           e
                           (make-instance c)))
      e)))

;;; Component interface

(defun get-comp-types (group)
  "Parse component group to type lists.
! means component must be present, ? means its optional.
By default all components in group considered obligatory"
  (let ((obligaroty-comp-types (list))
        (optional-comp-types (list)))
    (loop :for form :in group
          :for maybe-predicate := (symbol-name (first form))
          :do
             (cond ((string= "!" maybe-predicate)
                    (push (third form) obligaroty-comp-types))
                   ((string= "?" maybe-predicate)
                    (push (third form) optional-comp-types))
                   (t (push (second form) obligaroty-comp-types))))
    (values obligaroty-comp-types optional-comp-types)))

(defun get-entities-with-all-comps (world storages)
  "Get vector of entities with all required components"
  (let* ((shortest (u:find-shortest storages #'storage-size))
         (entities (get-entities shortest)))
    (loop
      :for e :across entities
      :if (every (lambda (storage)
                   (get-component storage e))
                 storages)
        :collect e)))

(defun get-component-storage (world component)
  (get-component-storage-by-type world (type-of component)))

(defun get-component-storage-by-type (world component-type)
  (with-slots (component-storage-map) world
    (h:hash-get component-storage-map component-type)))

(defun get-var-type (form)
  "(! var type)/var type -> (var type)"
  (let ((f (symbol-name (first form))))
    (cond ((or (string= f "!") (string= f "?"))
           (list (second form) (third form)))
          (t (list (first form) (second form))))))

(defun group-to-let-bindings (entity world group)
  (loop
    :for form :in group
    :collect
    (let ((var-type (get-var-type form)))
      `(,(first var-type)
        (get-component (get-component-storage ,world ,(second var-type))
                       ,entity)))))

(defmacro iterate-comp (world-form group &body body)
  (multiple-value-bind (must-have may-have)
      (get-comp-types group)
    (ax:with-gensyms (e entities storages)
      `(let* ((,storages (mapcar
                          (ax:curry #'get-component-storage-by-type ,world-form)
                          ',must-have))
              (,entities (get-entities-with-all-comps
                          ,world-form ,storages)))
         (loop
           :for ,e :in ,entities
           :do (let ,(group-to-let-bindings e world-form group)
                 ,@body))))))
