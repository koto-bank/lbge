(defpackage :lbge-render-test
  (:use :cl)
  (:local-nicknames (:b :lbge.render.backend)
                    (:m :lbge.math))
  (:export :run))

(in-package :lbge-render-test)

(defun run ()
  (lbge.engine:delete-engine)
  (lbge.engine:make-engine)
  (lbge.engine.events:add-event-handlers
    (:keyup
     (:keysym keysym)
     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
       (sdl2:push-event :quit))
     (format t "Pressed ~S key~%" (sdl2:scancode keysym))))
  (let ((r (lbge.render:make-renderer :gl))
        (a (lbge.asset:make-asset-manager))
        (c (lbge.render:make-ortho-camera :left 0.0f0 :right 1.0f0
                                          :top 1.0f0 :bottom 0.0f0
                                          :near 0.1f0 :far 1.0f0)))
    ;; Setup asset manager
    (lbge.asset:add-root a :root "a/render-test")
    (lbge.asset:add-handler a :glsl-source (make-instance 'lbge.asset:glsl-asset-handler))
    (lbge.engine:add-manager a)

    ;; Install
    (let ((frag-shader-asset
            (lbge.asset:get-asset a (lbge.asset:make-asset-key :glsl-source :disk ":root/frag.glsl")))
          (vert-shader-asset
            (lbge.asset:get-asset a (lbge.asset:make-asset-key :glsl-source :disk ":root/vert.glsl"))))
      (lbge.utils:println "Fragment shader:")
      (lbge.utils:println (lbge.asset:asset-data frag-shader-asset))
      (lbge.utils:println "Vertex shader:")
      (lbge.utils:println (lbge.asset:asset-data vert-shader-asset)))
    (lbge.engine:install-renderer r)
    (lbge.render:add-camera r c)
    (lbge.render:set-current-camera r c)
    (lbge.render:add-objects
     r
     (list (lbge.render:make-rect :w 0.1f0 :h 0.1f0
                                  :transform
                                  (m:make-transform :pos (m:make-float3 0.3f0 0.3f0 0.0f0)))
           (lbge.render:make-triangle :size 0.1f0
                                      :transform
                                      (m:make-transform :pos (m:make-float3 0.3f0 0.6f0 0.0f0)))
           (lbge.render:make-ellipse :w 0.1f0 :h 0.1f0
                                     :transform
                                     (m:make-transform :pos (m:make-float3 0.6f0 0.6f0 0.0f0)))
           (lbge.render:make-ring :w 0.1f0 :h 0.1f0 :thickness 0.02f0
                                  :transform
                                  (m:make-transform :pos (m:make-float3 0.6f0 0.3f0 0.0f0)))))
    (lbge.engine:link :before-start
                      (lambda ()
                        (b:init (lbge.render:get-backend r)
                                (lbge.engine:get-main-window))

                        (format t "OpenGL version string: ~a~%" (gl:gl-version))
                        (format t "GLSL version string: ~a~%" (gl:glsl-version))
                        (gl:clear-color 0.02f0 0.05f0 0.05f0 1.0f0)))
    (lbge.engine:link :on-loop
                      (lambda ()
                        (gl:clear :color-buffer-bit)
                        (lbge.render.backend:present (lbge.render:get-backend r)))))
  (lbge.engine:start))
