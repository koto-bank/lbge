(defpackage :lbge-render-test
  (:use :cl)
  (:local-nicknames (:m :lbge.math)
                    (:le :lbge.engine)
                    (:e :lbge.engine.events)
                    (:f :lbge.filesystem)
                    (:i :lbge.image)
                    (:a :lbge.asset)
                    (:an :lbge.animation)
                    (:r :lbge.render)
                    (:b :lbge.render.backend)
                    (:s :lbge.render.shader)
                    (:t :lbge.render.texture)
                    (:timer :lbge.timer)
                    (:u :lbge.utils))
  (:export :run))

(in-package :lbge-render-test)

(defun run ()
  (log:config :debug)
  (le:delete-engine)
  (le:make-engine)
  (le:init-engine (le:make-engine-options
                   :window-w 1440 :window-h 900))
  (e:add-event-handlers
    (:keyup
     (:keysym keysym)
     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
       (sdl2:push-event :quit))
     (format t "Pressed ~S key~%" (sdl2:scancode keysym))))
  (let* ((r (le:get-renderer))
         (a (le:get-manager 'a:asset-manager))
         (c (r:make-ortho-camera :left -1.0f0 :right 1.0f0
                                 :top 0.75f0 :bottom -0.75f0
                                 :near -0.1f0 :far 3.0f0
                                 :view (m:make-look-at
                                        (m:make-float3 0.0 0.0 1.0)
                                        (m:make-float3 0.0 0.0 0.0)
                                        (m:make-float3 0.0 1.0 0.0))))
         ;; primitives
         (rect (r:make-rect :w 0.45f0 :h 0.63f0
                            :transform
                            (m:make-transform :pos (m:make-float4 0.3f0 0.3f0 0.0f0 1.0f0))
                            :additional-attributes
                            (list '(:color :float 3)
                                  (list (m:make-float3 1.0 0.0 1.0)
                                        (m:make-float3 0.0 0.0 1.0)
                                        (m:make-float3 0.0 1.0 0.0)
                                        (m:make-float3 1.0 0.0 0.0))
                                  '(:texcoord :float 2)
                                  (list (m:make-float2 0.0 1.0)
                                        (m:make-float2 1.0 1.0)
                                        (m:make-float2 1.0 0.0)
                                        (m:make-float2 0.0 0.0)))))
         (tri  (r:make-triangle :size 0.3f0
                                :transform
                                (m:make-transform :pos (m:make-float4 -0.3f0 0.3f0 0.0f0 1.0f0))
                                :additional-attributes
                                (list '(:color :float 3)
                                      (list (m:make-float3 0.0 0.0 1.0)
                                            (m:make-float3 0.0 1.0 0.0)
                                            (m:make-float3 1.0 0.0 0.0)))))
         (ellipse (r:make-ellipse :r-x 0.15f0 :r-y 0.15f0
                                  :transform
                                  (m:make-transform :pos (m:make-float4 -0.3f0 -0.3f0 0.0f0 1.0f0))))
         (ring (r:make-ring :out-r 0.15f0 :in-r 0.1f0
                            :transform
                            (m:make-transform :pos (m:make-float4 0.3f0 -0.3f0 0.0f0 1.0f0))))
         ;; animations
         (keyframes (list (cons 0 (m:make-float2 0.1f0 0.1f0))
                          (cons 1000 (m:make-float2 -0.1f0 0.1f0))
                          (cons 2000 (m:make-float2 -0.1f0 -0.1f0))
                          (cons 3000 (m:make-float2 0.1f0 -0.1f0))
                          (cons 4000 (m:make-float2 0.1f0 0.1f0))))
         (animation (an:make keyframes
                             (let* ((init-rect (m:translation (r:transform rect)))
                                    (init-tri (m:translation (r:transform tri)))
                                    (init-ellipse (m:translation (r:transform ellipse)))
                                    (init-ring (m:translation (r:transform ring))))
                               (lambda (dp)
                                 (setf (m:translation (r:transform rect))
                                       (m:add init-rect (m:make-float4 (m:float2-x dp)
                                                                       (m:float2-y dp)
                                                                       0.0f0 1.0f0))
                                       (m:translation (r:transform tri))
                                       (m:add init-tri (m:make-float4 (- (m:float2-y dp))
                                                                      (m:float2-x dp)
                                                                      0.0f0 1.0f0))
                                       (m:translation (r:transform ellipse))
                                       (m:add init-ellipse (m:make-float4 (- (m:float2-x dp))
                                                                          (- (m:float2-y dp))
                                                                          0.0f0 1.0f0))
                                       (m:translation (r:transform ring))
                                       (m:add init-ring (m:make-float4 (m:float2-y dp)
                                                                       (- (m:float2-x dp))
                                                                       0.0f0 1.0f0)))))
                             #'an:linear-interpolation))
         ;; timer
         (timer (timer:make 5000 :one-shot t))

         ;; shader
         (frag-shader-asset
           (a:get-asset a (a:make-asset-key :glsl-source :disk ":root/frag.glsl")))
         (vert-shader-asset
           (a:get-asset a (a:make-asset-key :glsl-source :disk ":root/vert.glsl")))
         (shader (b:make-shader (r:renderer-backend r) "simple-shader"))

         ;; texture
         (image (a:get-asset a (a:make-asset-key :image :disk ":root/umalico-0.tga")))
         (image-test (a:get-asset a (a:make-asset-key :image :disk ":root/test2.tga")))
         (texture (b:make-texture (r:renderer-backend r)
                                  :image (a:asset-data image)
                                  :target :texture-2d
                                  :format :rgba8)))
    ;; Install
    (r:add-camera r c)
    (r:set-current-camera r c)
    (r:add-objects
     r
     rect
     tri
     ellipse
     ring)
    (le:link :before-start
             (lambda ()
               (log:info "Fragment shader:")
               (let ((lines (u:merge-lines (a:asset-data frag-shader-asset))))
                 (log:info lines))
               (log:info "Vertex shader:")
               (let ((lines (u:merge-lines (a:asset-data vert-shader-asset))))
                 (log:info lines))
               (s:add-stage shader (list :vertex (a:asset-data vert-shader-asset)
                                         :fragment (a:asset-data frag-shader-asset)))
               (s:compile-shader shader)
               (when (eq (s:get-status shader)
                         :error)
                 (log:info "Shader compilation failed")
                 (log:info (s:get-compile-log shader)))
               (when (eq (s:get-status shader)
                         :compiled)
                 (log:info "Shader successfully compiled and linked!"))
               (let ((log (s:get-compile-log shader)))
                 (when (> (length log) 0)
                   (log:info "Compilation log: ~A" log)))
               (b:use-shader (r:renderer-backend r) shader)
               ;; Texture
               (t:texture-load texture)
               (setf (slot-value (r:renderer-backend r) 'lbge.render.gl::active-texture)
                     texture)
               (gl:polygon-mode :front-and-back :fill))) ; change to :line to view wireframe
    (timer:link timer (lambda ()
                        (b:print-statistics (r:renderer-backend r))))
    (le:link :on-loop
             (lambda (dt)
               (let ((backend (r:renderer-backend r)))
                 (an:update animation dt)
                 (b:clear backend)
                 (b:render backend r)
                 (b:present backend)
                 (timer:update timer dt)))))
  (le:start))
