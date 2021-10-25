(defpackage :lbge-ecs-test
  (:use :cl)
  (:local-nicknames (:m :lbge.math)
                    (:le :lbge.engine)
                    (:ev :lbge.engine.events)
                    (:e :lbge.ecs)
                    (:f :lbge.filesystem)
                    (:a :lbge.asset)
                    (:an :lbge.animation)
                    (:r :lbge.render)
                    (:b :lbge.render.backend)
                    (:s :lbge.render.shader)
                    (:u :lbge.utils))
  (:export :run))

(defclass cursor-follow-system (e:system)
  (cursor-pos :documentation "Current cursor position"
              :initform (m:make-float2))
  (satellites :documentation "List of satellite entities"
              :initform (list)))

(defmethod initialize-instance :after ((sys cursor-follow-system))
  ())

(defmethod e:update ((sys cursor-follow-system) dt)
  (let ((mouse-move-event (ev:get-event :mouse-move))
        (increase-satellites (ev:get-event :satellites-changed)))
    (with-slots (cursor-pos) sys
      ())))

(defun create-entities (world)
  (let ((e1 (e:create-entity world))
        (e2 (e:create-entity world))
        (e3 (e:create-entity world))
        (e4 (e:create-entity world)))))

(defun run ()
  (log:config :debug)
  (le:make-engine)
  (let ((world (e:make-world)))
    (e:add-system world (e:make-system 'e:input-system))
    (e:add-system world (e:make-system 'e:transform-system))
    (e:add-system world (e:make-system 'cursor-follow-system))
    (e:add-system world (e:make-system 'e:render-system))))
