(in-package :lbge.render.gl)

(defclass gl-render-object-data ()
  ((vertex-vbo :documentation "VBO for vertices")
   (index-vbo :documentation "VBO for indices")
   (indices :documentation "Index array"))
  (:documentation "GL specific render object data"))

(defun vertices-to-gl-array (float-vector)
  (let* ((vector-len (length float-vector))
         (elt-size (m:get-size (aref float-vector 0)))
         (array (gl:alloc-gl-array :float (* vector-len elt-size))))
    (log:debug "Alloc array of size ~A" (* vector-len elt-size))
    (dotimes (i vector-len)
      (let ((current (m:in-list (aref float-vector i))))
        (log:debug current)
        (dotimes (j elt-size)
          (setf (gl:glaref array (+ (* i elt-size) j))
                (aref current j)))))
    array))

(defun indices-to-gl-array (index-vector)
  (let* ((vector-len (length index-vector))
         (array (gl:alloc-gl-array :unsigned-int vector-len)))
    (dotimes (i vector-len)
      (setf (gl:glaref array i)
            (aref index-vector i)))
    (log:debug index-vector)
    array))

(defun ensure-buffers (render-object)
  "Check if the object has internal data, create if not"
  (unless (slot-boundp render-object 'r:backend-data)
    (let ((gl-data (make-instance 'gl-render-object-data)))
      (loop
        :for batch :across (slot-value render-object 'r:batches)
        :do
           (with-slots (r:vertices
                        r:indices)
               batch
             (let ((verts (vertices-to-gl-array r:vertices))
                   (inds (indices-to-gl-array r:indices))
                   (vertex-buf (gl:gen-buffer))
                   (index-buf (gl:gen-buffer)))
               (gl:enable-vertex-attrib-array 0) ; vertices

               ;; Load vertices
               (gl:bind-buffer :array-buffer vertex-buf)
               (gl:vertex-attrib-pointer 0 4 :float nil 0 0)
               (gl:buffer-data :array-buffer :static-draw verts)
               (gl:free-gl-array verts)

               ;; Load indices
               (gl:bind-buffer :element-array-buffer index-buf)
               (gl:buffer-data :element-array-buffer :static-draw inds)

               ;; Initialize render object gl data
               (with-slots (vertex-vbo index-vbo
                            index-count indices)
                   gl-data
                 (setf indices inds)
                 (setf vertex-vbo vertex-buf)
                 (setf index-vbo index-buf)))))
      (setf (slot-value render-object 'r:backend-data) gl-data))))

(defun draw-object (render-object)
  (with-slots ((gl-data r:backend-data)) render-object
    (gl:bind-buffer :array-buffer (slot-value gl-data 'vertex-vbo))
    (gl:vertex-attrib-pointer 0 4 :float nil 0 0)
    (gl:draw-arrays :triangle-strip 0 4)
    (gl:draw-elements :triangles (slot-value gl-data 'indices))))
