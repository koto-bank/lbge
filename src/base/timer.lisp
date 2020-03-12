(in-package :lbge.timer)

(defclass timer ()
  ((timeout :initarg :timeout
            :accessor timeout
            :documentation "Timer timeout in ms")
   (startedp :initform nil
             :accessor startedp
             :documentation "If the timer has started")
   (passed-time :accessor passed-time
                :initform 0
                :documentation "Time passed after timer has started")
   (beacon :accessor beacon
           :initform (beacon:make :inner-beacon)
           :documentation "Timer beacon. Blinks when timer times
           out"))
  (:documentation "Class for firing events periodically or after some
  time. Requires manually updating timer"))

(defun make (timeout)
  (make-instance 'timer :timeout timeout))

(defun start (timer &optional timeout)
  "Starts the timer if not already started"
  (when (startedp timer)
    (return-from start))
  (when timeout
    (setf (timeout timer) timeout))
  (setf (startedp timer) t
        (passed-time timer) 0))

(defun stop (timer)
  "Stop the timer"
  (setf (startedp timer) nil))

(defun link (timer callback)
  (funcall #'beacon:link (beacon timer) callback))

(defun unlink (timer callback)
  (funcall #'beacon:unlink (beacon timer) callback))

(defun unlink-all (timer)
  (funcall #'beacon:unlink-all (beacon timer)))

(defun restart (timer)
  (setf (passed-time timer) 0))

(defun update (timer passed-time)
  (unless (startedp timer)
    (return-from update))
  (incf (passed-time timer) passed-time)
  (let ((passed (passed-time timer))
        (timeout (timeout timer))
        (beacon (beacon timer)))
    (when (>= passed timeout)
      (setf (passed-time timer)
            (- passed timeout))
      (beacon:blink beacon))))
