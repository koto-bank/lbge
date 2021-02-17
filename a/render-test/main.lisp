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
                    (:se :lbge.serialization)
                    (:t :lbge.render.texture)
                    (:mat :lbge.render.material)
                    (:timer :lbge.timer)
                    (:u :lbge.utils))
  (:export :run))

(in-package :lbge-render-test)

(a:define-asset plain-material (a:material-asset)
  ((color :accessor color :serialize t)))

(a:define-asset textured-material (a:material-asset)
  ((texture :dep t :type a:texture-asset
            :accessor texture)))

(defun run ()
  (f:set-app-root-to-system 'lbge-render-test)
  (log:config :debug)
  (le:delete-engine)
  (le:make-engine)
  (le:init-engine (le:make-engine-options
                   :opengl-version '(4 . 1)
                   :window-title "Render testbed"))

  (e:add-event-handlers
    (:keyup
     (:keysym keysym)
     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
       (sdl2:push-event :quit))
     (format t "Pressed ~S key~%" (sdl2:scancode keysym))))

  (let ((a (le:get-manager 'a:asset-manager)))
    (a:add-handler a (make-instance 'a:shader-source-handler))
    (a:add-handler a (make-instance 'a:image-asset-handler))
    (a:add-handler a (make-instance 'a:texture-asset-handler))
    (a:add-handler a (make-instance 'a:material-asset-handler)))
  (let* ((r (le:get-renderer))
         (backend (r:renderer-backend r))
         (a (le:get-manager 'a:asset-manager))
         (c (r:make-ortho-camera :left -1.0f0 :right 1.0f0
                                 :top 0.75f0 :bottom -0.75f0
                                 :near -0.1f0 :far 3.0f0
                                 :view (m:make-look-at
                                        (m:make-float3 0.0 0.0 1.0)
                                        (m:make-float3 0.0 0.0 0.0)
                                        (m:make-float3 0.0 1.0 0.0))))
         ;; material
         (plain (a:build-material
                 backend
                 (a:load-dependencies
                  a
                  (a:get-asset a (a:make-asset-key 'plain-material :disk ":root/plain.mat")))
                 (list)
                 (list :in-color 'color)))
         (umalico (a:build-material
                   backend
                   (a:load-dependencies
                    a
                    (a:get-asset a (a:make-asset-key 'textured-material :disk ":root/umalico.mat")))
                   (list 'texture)
                   (list)))
         ;; primitives
         (rect (r:make-rect :w 0.45f0 :h 0.63f0
                            :material (a:backend-material umalico)
                            :transform
                            (m:make-transform :pos (m:make-float4 0.3f0 0.3f0 0.0f0 1.0f0))))
         (tri  (r:make-triangle :size 0.3f0
                                :material (a:backend-material plain)
                                :transform
                                (m:make-transform :pos (m:make-float4 -0.3f0 0.3f0 0.0f0 1.0f0))))
         (ellipse (r:make-ellipse :r-x 0.15f0 :r-y 0.15f0
                                  :material (a:backend-material plain)
                                  :transform
                                  (m:make-transform :pos (m:make-float4 -0.3f0 -0.3f0 0.0f0 1.0f0))))
         (ring (r:make-ring :out-r 0.15f0 :in-r 0.1f0
                            :material (a:backend-material plain)
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
         (total-dt 0)
         (timer (timer:make 5000)))
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
               (gl:enable :blend)
               (gl:blend-func :src-alpha :one-minus-src-alpha)
               (gl:polygon-mode :front-and-back :fill))) ; change to :line to view wireframe
    (timer:link timer (lambda ()
                        (log:debug "FPS ~A (~A frames ~A seconds)"
                                   (/ (b:get-total-frames backend)
                                      (/ total-dt 1000.0))
                                   (b:get-total-frames backend)
                                   (/ total-dt 1000.0))
                        (b:print-statistics backend)))
    (le:link :on-loop
             (let ((dr 0.02)
                   (dg 0.01)
                   (db 0.03))
               (lambda (dt)
                 (let ((color (color plain)))
                   (incf (m:x color) dr)
                   (incf (m:y color) dg)
                   (incf (m:z color) db)
                   (when (> (m:x color) 1.0)
                     (setf
                      dr (- dr)
                      (m:x color) (+ 1.0 dr)))
                   (when (> (m:y color) 1.0)
                     (setf
                      dg (- dg)
                      (m:y color) (+ 1.0 dg)))
                   (when (> (m:z color) 1.0)
                     (setf
                      db (- db)
                      (m:z color) (+ 1.0 db)))
                   (when (< (m:x color) 0.0)
                     (setf
                      dr (- dr)
                      (m:x color) (+ 0.0 dr)))
                   (when (< (m:y color) 0.0)
                     (setf
                      dg (- dg)
                      (m:y color) (+ 0.0 dg)))
                   (when (< (m:z color) 0.0)
                     (setf
                      db (- db)
                      (m:z color) (+ 0.0 db))))
                 (incf total-dt dt)
                 (an:update animation dt)
                 (b:clear backend)
                 (b:render backend r)
                 (b:present backend)
                 (timer:update timer dt)))))
  (le:start))
