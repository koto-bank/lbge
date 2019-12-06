(in-package :lbge.engine)

;;; From cl-sdl2.
;;; Since it doesn't let us access and destructure separate events,
;;; we write our implementation here.
;;; But it's nice to have compatibility
(eval-when (:compile-toplevel)
  (defparameter *event-type-to-accessor*
    '((:controlleraxismotion . :caxis)
      (:controllerbuttondown . :cbutton)
      (:controllerbuttonup . :cbutton)
      (:controllerdeviceadded . :cdevice)
      (:controllerdeviceremapped . :cdevice)
      (:controllerdeviceremoved . :cdevice)
      (:dollargesture . :dgesture)
      (:dropfile . :drop)
      (:fingermotion . :tfinger)
      (:fingerdown . :tfinger)
      (:fingerup . :tfinger)
      (:joyaxismotion . :jaxis)
      (:joyballmotion . :jball)
      (:joybuttondown . :jbutton)
      (:joybuttonup . :jbutton)
      (:joydeviceadded . :jdevice)
      (:joydeviceremoved . :jdevice)
      (:joyhatmotion . :jhat)
      (:keydown . :key)
      (:keyup . :key)
      (:mousebuttondown . :button)
      (:mousebuttonup . :button)
      (:mousemotion . :motion)
      (:mousewheel . :wheel)
      (:multigesture . :mgesture)
      (:syswmevent . :syswm)
      (:textediting . :edit)
      (:textinput . :text)
      (:userevent . :user)
      (:lisp-message . :user)
      (:windowevent . :window))))

(defun process-events (engine sdl-event)
  (loop :as rc = (sdl2:next-event sdl-event :poll)
        :until (= 0 rc)
        :do (let ((event-type (sdl2:get-event-type sdl-event)))
              (if (eq event-type :quit)
                  (setf (slot-value engine 'state) :stopping)
                  (process-event engine event-type sdl-event)))))

(defmacro expand-accessor (field event-type event)
  (let ((ref (cdr (assoc event-type *event-type-to-accessor*))))
    `(plus-c:c-ref ,event sdl2-ffi:sdl-event ,ref ,field)))

(defmacro case-event (event-type event &rest binds)
  `(case ,event-type
     ,@(mapcar (lambda (elem)
                 (let ((type (first elem))
                       (params (second elem))
                       (forms (cddr elem))
                       (mapping nil))
                   (do ((keyword params (if (cdr keyword)
                                            (cddr keyword)
                                            nil)))
                       ((null keyword))
                     (push (list (second keyword) `(expand-accessor ,(first keyword) ,type ,event)) mapping))
                   (princ mapping)
                   `(,type
                     (let (,@mapping) ,@forms))))
        binds)))

(defun process-event (engine event-type sdl-event)
  ;; TODO: collect events and process to sdl-independent more fine-grained
  ;; (e.g. respecting :keyup :keydown events, possible to use in the ECS
  (case-event event-type sdl-event
    (:keyup
     (:keysym keysym)
     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
       (sdl2:push-event :quit)))
    (:mousemotion
     (:x x :y y)
     (format t "Mouse moved: ~S ~S" x y))))
