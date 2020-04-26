(in-package :lbge.render.gl)

(defclass gl-texture (t:texture)
  ((handle :documentation "GL handle"
           :initform -1)))

(defmethod t:texture-initialize ((tex gl-texture))
  (setf (slot-value tex 'handle)
        (gl:gen-texture))
  (log:debug "Generated texture with id ~A" (slot-value tex 'handle)))

(defun gl-texture-target (target)
  (ax:switch (target)
    (:texture-2d :texture-2d)))

(defun gl-image-format (image)
  (ax:switch ((i:channels image))
    (:r8 :r8)
    (:rgb8 :rgb8)
    (:rgba8 :rgba8)))

(defun gl-texture-format (format)
  (ax:switch (format)
    (:r8 :red)
    (:rgb8 :rgb)
    (:rgba8 :rgba)))

(defmethod t:texture-load ((tex gl-texture))
  (with-slots (t:image t:target handle t:format)
      tex
    (let ((target (gl-texture-target t:target)))
      (gl:bind-texture target handle)
      (gl:tex-parameter target :texture-min-filter :nearest)
      (gl:tex-parameter target :texture-mag-filter :linear)
      (gl:tex-parameter target :texture-wrap-s :clamp-to-border)
      (gl:tex-parameter target :texture-wrap-t :clamp-to-border)
      ;; Currently only 2D textures are supported
      (gl:tex-image-2d target 0 (gl-image-format t:image)
                       (i:width t:image)
                       (i:height t:image)
                       0
                       (gl-texture-format t:format)
                       :unsigned-byte
                       (i:data t:image)))))
