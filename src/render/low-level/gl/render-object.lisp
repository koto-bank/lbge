(in-package :lbge.render.gl)

(defclass gl-render-object-data ()
  ((vao :documentation "VAO which stores this render object data")
   (base-vertex :documentation "Index of the first vertex in vertex buffer")
   (index-size :documentation "Byte size of indices")
   (index-offset :documentation "Index of the first index in index buffer")
   (num-instances :documentation "Number of instances"))
  (:documentation "GL specific render object data"))

(defun vertices-to-gl-array (float-vector semantics)
  (let* ((vector-len (length float-vector))
         (elt-sizes (slot-value semantics 'r:attribute-sizes))
         (elt-size (reduce #'+ (slot-value semantics 'r:attribute-sizes)))
         (attr-num (slot-value semantics 'r:attributes-num))
         (array (gl:alloc-gl-array :float (* vector-len elt-size)))
         (gl-index 0))
    (dotimes (i vector-len)
      (let ((current-attr (mod i attr-num))
            (current (m:in-vec (aref float-vector i))))
        (dotimes (j (nth current-attr elt-sizes))
          (setf (gl:glaref array gl-index)
                (aref current j))
          (incf gl-index))))
    array))

(defun indices-to-gl-array (index-vector)
  (let* ((vector-len (length index-vector))
         (array (gl:alloc-gl-array :unsigned-short vector-len)))
    (dotimes (i vector-len)
      (setf (gl:glaref array i)
            (aref index-vector i)))
    array))

(defun ensure-buffer-data (backend render-object)
  "Check if the object has internal data, create if not, store data"
  (when (slot-boundp render-object 'r:backend-data)
    (return-from ensure-buffer-data))

  (let ((gl-data (make-instance 'gl-render-object-data))
        (buffer-storage (ensure-buffer-storage backend render-object)))
    (with-slots (vao vertex-bo index-bo semantics
                 last-index-index last-vertex-index
                 total-index-size total-vertex-size)
        buffer-storage
      (gl:bind-vertex-array vao)
      (loop
        :for batch :across (slot-value render-object 'r:batches)
        :do
           (with-slots (r:vertices
                        r:indices)
               batch
             (let ((verts (vertices-to-gl-array r:vertices semantics))
                   (inds (indices-to-gl-array r:indices)))
               (gl:bind-buffer :array-buffer vertex-bo)
               (log:debug "Filling VBO, offset: ~A count: ~A" total-vertex-size
                          (gl:gl-array-byte-size verts))
               (gl:buffer-sub-data :array-buffer verts
                                   :buffer-offset total-vertex-size)

               (gl:bind-buffer :element-array-buffer index-bo)
               (log:debug "Filling IBO, offset: ~A count: ~A" total-index-size
                          (gl:gl-array-byte-size inds))
               (gl:buffer-sub-data :element-array-buffer inds
                                   :buffer-offset total-index-size)
               (with-slots (base-vertex index-size index-offset)
                   gl-data
                 (setf (slot-value gl-data 'vao) vao)
                 (setf base-vertex last-vertex-index
                       index-size (length r:indices)
                       index-offset last-index-index)

                 (setf last-vertex-index
                       (+ last-vertex-index (length r:vertices))
                       last-index-index
                       (+ last-index-index index-size)
                       total-index-size
                       (+ total-index-size (gl:gl-array-byte-size inds))
                       total-vertex-size
                       (+ total-vertex-size (gl:gl-array-byte-size verts))))
               (gl:free-gl-array verts)
               (gl:free-gl-array inds))))
      (gl:bind-vertex-array 0))
    (setf (slot-value render-object 'r:backend-data) gl-data)))

(defun draw-object (camera backend render-object)
  (with-slots ((gl-data r:backend-data)
               (transform r:transform))
      render-object
    (with-slots (active-shader) backend
      (s:set-uniform-matrix active-shader :model-view
                            (m:mul
                             (r:camera-view-matrix camera)
                             (m:transform-matrix transform)))
      (s:set-uniform-matrix active-shader :projection
                            (r:camera-projection-matrix camera))
      (with-slots (base-vertex index-size index-offset vao)
          gl-data
        (gl:bind-vertex-array vao)
        (gl:draw-elements-base-vertex :triangles
                                      (gl:make-null-gl-array :unsigned-short)
                                      base-vertex
                                      :count index-size
                                      :offset index-offset)
        (gl:bind-vertex-array 0)))))
