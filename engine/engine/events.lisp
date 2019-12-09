(in-package :lbge.engine.events)

;;; From cl-sdl2.
;;; Since it doesn't let us access and destructure separate events,
;;; we write our implementation here.
;;; But it's nice to have compatibility
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
    (:windowevent . :window)))

(defmacro expand-accessor (field event-type event)
  (let ((ref (cdr (assoc event-type *event-type-to-accessor*))))
    `(plus-c:c-ref ,event sdl2-ffi:sdl-event ,ref ,field)))

(defvar *event-handlers* (list) "Storage for registered handlers")

(defmacro expand-handler (handler-form)
  (let ((type (first handler-form))
        (params (second handler-form))
        (forms (cddr handler-form))
        (event-var (gensym "SDL-EVENT-"))
        (mapping nil))
    (do ((keyword params (if (cdr keyword)
                             (cddr keyword)
                             nil)))
        ((null keyword))
      (push (list (second keyword) `(expand-accessor ,(first keyword) ,type ,event-var)) mapping))
    `(cons ,type (lambda (,event-var)
                   (let (,@mapping) ,@forms)))))

(defmacro add-event-handlers (&body handlers)
  (let ((forms (mapcar
                (lambda (handler)
                  (list 'add-event-handler handler))
                handlers)))
    `(progn ,@forms)))

(defmacro add-event-handler (handler-form)
  `(push (expand-handler ,handler-form) *event-handlers*))

(defun process-event (event-type sdl-event)
  ;; TODO: collect events and process to sdl-independent more fine-grained
  ;; (e.g. respecting :keyup :keydown events, possible to use in the ECS
  (let ((handler (assoc event-type *event-handlers*)))
    (when handler
      (assert (cdr handler) nil "Event handler cannot be empty")
      (funcall (cdr handler) sdl-event))))

(defun process-events (engine sdl-event)
  (loop :as rc = (sb-int:with-float-traps-masked (:invalid :overflow)
                   (sdl2:next-event sdl-event :poll))
        :until (= 0 rc)
        :do (let ((event-type (sdl2:get-event-type sdl-event)))
              (if (eq event-type :quit)
                  (setf (slot-value engine 'lbge.engine::state) :stopping)
                  (process-event event-type sdl-event)))))
