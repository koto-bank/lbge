(defpackage :lbge-render-test
  (:use :cl)
  (:local-nicknames (:m :lbge.math)
                    (:le :lbge.engine)
                    (:e :lbge.engine.events)
                    (:f :lbge.filesystem)
                    (:a :lbge.asset)
                    (:r :lbge.render)
                    (:b :lbge.render.backend)
                    (:s :lbge.render.shader)
                    (:u :lbge.utils))
  (:export :run))

(in-package :lbge-render-test)

(defun run ()
  (log:config :debug)
  (le:delete-engine)
  (le:make-engine)
  (e:add-event-handlers
    (:keyup
     (:keysym keysym)
     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
       (sdl2:push-event :quit))
     (format t "Pressed ~S key~%" (sdl2:scancode keysym))))
  (let ((r (r:make-renderer :gl))
        (a (a:make-asset-manager))
        (c (r:make-ortho-camera :left -1.0f0 :right 1.0f0
                                :top 0.75f0 :bottom -0.75f0
                                :near -0.1f0 :far 3.0f0
                                :view (m:make-look-at
                                       (m:make-float3 0.0 0.0 1.0)
                                       (m:make-float3 0.0 0.0 0.0)
                                       (m:make-float3 0.0 1.0 0.0)))))
    (f:set-app-root-to-system 'lbge-render-test)
    ;; Setup asset manager
    (a:add-root a :root ".")
    (a:add-handler a (make-instance 'lbge.asset:glsl-asset-handler))
    (le:add-manager a)

    ;; Install
    (le:install-renderer r)
    (r:add-camera r c)
    (r:set-current-camera r c)
    (r:add-objects
     r
     (r:make-rect :w 0.3f0 :h 0.3f0
                  :transform
                  (m:make-transform :pos (m:make-float4 0.3f0 0.3f0 0.0f0 1.0f0)))
     (r:make-triangle :size 0.3f0
                      :transform
                      (m:make-transform :pos (m:make-float4 -0.3f0 0.3f0 0.0f0 1.0f0)))
     (r:make-ellipse :r-x 0.15f0 :r-y 0.15f0
                     :transform
                     (m:make-transform :pos (m:make-float4 -0.3f0 -0.3f0 0.0f0 1.0f0)))
     (r:make-ring :out-r 0.15f0 :in-r 0.1f0
                  :transform
                  (m:make-transform :pos (m:make-float4 0.3f0 -0.3f0 0.0f0 1.0f0))))
    (le:link :before-start
             (lambda ()
               (b:init (r:get-backend r)
                       (le:get-main-window)
                       '((:gl-version (4 . 1))))
               (format t "OpenGL version string: ~a~%" (gl:gl-version))
               (format t "GLSL version string: ~a~%" (gl:glsl-version))
               (gl:clear-color 0.02f0 0.05f0 0.05f0 1.0f0)
               (let ((frag-shader-asset
                       (a:get-asset a (a:make-asset-key :glsl-source :disk ":root/frag.glsl")))
                     (vert-shader-asset
                       (a:get-asset a (lbge.asset:make-asset-key :glsl-source :disk ":root/vert.glsl")))
                     (shader (b:make-shader (r:get-backend r) "simple-shader")))
                 (log:info "Fragment shader:")
                 (let ((lines (u:merge-lines (lbge.asset:asset-data frag-shader-asset))))
                   (log:info lines))
                 (log:info "Vertex shader:")
                 (let ((lines (u:merge-lines (lbge.asset:asset-data vert-shader-asset))))
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
                 (b:use-shader (r:get-backend r) shader)
                 (gl:polygon-mode :front-and-back :fill)))) ; change to line to view wireframe
    (le:link :on-loop
             (lambda ()
               (let ((backend (r:get-backend r)))
                 (b:clear backend)
                 (b:render backend r)
                 (b:present backend)))))
  (le:start))
