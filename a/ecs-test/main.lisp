(defpackage :lbge-ecs-test
  (:use :cl :rove :lbge.rove-utils)
  (:local-nicknames
   (:ecs :lbge.ecs)
   (:u :lbge.utils))
  (:export :run))

(in-package :lbge-ecs-test)

(defclass my-comp (ecs:component)
  ((comp-tag :documentation "A certain data"
             :accessor comp-tag
             :initform 0)))

(defclass my-system (ecs:system)
  ((passed-time :documentation "Time (in ms) passed since the start"
                :accessor passed-time
                :initform 0)))

(ecs:bind-components 'my-system 'my-comp)

(defmethod ecs:update ((sys my-system) dt)
  (with-slots (passed-time (world ecs:world)) sys
    (incf passed-time dt)
    (when (> passed-time 1000)
      (decf passed-time 1000)
      (ecs:iterate-comp world ((comp my-comp)
                               (? other-comp other-comp))
        (incf (comp-tag comp))
        ;; If the entity has both components
        ;; and the value in the my-comp > 2,
        ;; raise the flag in the other-comp
        (when (>= (comp-tag comp) 2)
          (when other-comp
            (setf (other-comp-flag other-comp) t)))))))

(defclass other-comp (ecs:component)
  ((flag :accessor other-comp-flag
         :initform nil)))

(defclass other-system (ecs:system)
  ((data :documentation "Components with flags"
         :accessor sys-data
         :initform (list))))

(ecs:bind-components 'other-system 'other-comp)

(defmethod ecs:update ((sys other-system) dt)
  (with-slots (data (world ecs:world)) sys
    (ecs:iterate-comp world ((c other-comp))
      (when (other-comp-flag c)
        (push 34 data)))))

(defun create-entities (world)
  (values
   ;; 1st has my-comp and other-comp
   (ecs:create-entity world 'my-comp 'other-comp)
   ;; 2nd only my-comp
   (ecs:create-entity world 'my-comp)
   ;; 3d and 4th only other-comp
   (ecs:create-entity world 'other-comp)
   (ecs:create-entity world 'other-comp)))

(defun run ()
  (log:config :debug)
  (rove:run-suite :lbge-ecs-test))

(defun run-on-agent ()
  (run)
  (unless (lbge.rove-utils:report-results)
    (sb-ext:quit :unix-status 1)))

(defun run2 ()
  (log:config :debug)
  (let ((world (ecs:make-world))
        my-system other-system)
    (ecs:add-systems world 'my-system 'other-system)
    (multiple-value-bind (e1 e2 e3 e4)
        (create-entities world)

      ;; Begin tests!
      (log:info "World initialized, testing begins")
      (setf my-system (ecs:get-system world 'my-system))
      (setf other-system (ecs:get-system world 'other-system))
      (print (= 0 (passed-time my-system)))
      (print (= 0 (length (sys-data other-system))))

      (ecs:get-component world e1 'my-comp)
      (print (= 0 (comp-tag (ecs:get-component world e1 'my-comp))))
      (ecs:get-component world e1 'other-comp)

      (ecs:get-component world e2 'my-comp)
      (print (= 0 (comp-tag (ecs:get-component world e2 'my-comp))))
      (print (null (ecs:get-component world e2 'other-comp)))

      (print (null (ecs:get-component world e3 'my-comp)))
      (ecs:get-component world e3 'other-comp)
      (print (null (other-comp-flag (ecs:get-component world e3 'other-comp))))

      (print (null (ecs:get-component world e4 'my-comp)))
      (ecs:get-component world e4 'other-comp)
      (print (null (other-comp-flag (ecs:get-component world e4 'other-comp))))

      (print (= 0 (passed-time my-system)))
      (print (= 0 (length (sys-data other-system))))

      (ecs:update-world world 999)
      (print (= 999 (passed-time my-system)))
      (print (= 0 (comp-tag (ecs:get-component world e1 'my-comp))))
      (print (= 0 (comp-tag (ecs:get-component world e2 'my-comp)))))))

;;; Tests
(deftest ecs-test
  (let ((world (ecs:make-world))
        my-system other-system)
    (ecs:add-systems world 'my-system 'other-system)
    (multiple-value-bind (e1 e2 e3 e4)
        (create-entities world)

      ;; Begin tests!
      (testing "System creation and initialization"
        (ok (setf my-system (ecs:get-system world 'my-system)))
        (ok (setf other-system (ecs:get-system world 'other-system)))
        (ok (= 0 (passed-time my-system)))
        (ok (= 0 (length (sys-data other-system)))))

      (testing "Component retrieval"
        (ok (ecs:get-component world e1 'my-comp))
        (ok (= 0 (comp-tag (ecs:get-component world e1 'my-comp))))
        (ok (ecs:get-component world e1 'other-comp))

        (ok (ecs:get-component world e2 'my-comp))
        (ok (= 0 (comp-tag (ecs:get-component world e2 'my-comp))))
        (ok (null (ecs:get-component world e2 'other-comp)))

        (ok (null (ecs:get-component world e3 'my-comp)))
        (ok (ecs:get-component world e3 'other-comp))
        (ok (null (other-comp-flag (ecs:get-component world e3 'other-comp))))

        (ok (null (ecs:get-component world e4 'my-comp)))
        (ok (ecs:get-component world e4 'other-comp))
        (ok (null (other-comp-flag (ecs:get-component world e4 'other-comp))))

        (ok (= 0 (passed-time my-system)))
        (ok (= 0 (length (sys-data other-system)))))

      (testing "World updates"
        (ecs:update-world world 999)
        (ok (= 999 (passed-time my-system)))
        (ok (= 0 (comp-tag (ecs:get-component world e1 'my-comp))))
        (ok (= 0 (comp-tag (ecs:get-component world e2 'my-comp))))

        (ecs:update-world world 2)
        ;; 1001 - 1000 = 1
        (ok (= 1 (passed-time my-system)))
        (ok (= 1 (comp-tag (ecs:get-component world e1 'my-comp))))
        (ok (= 1 (comp-tag (ecs:get-component world e2 'my-comp))))
        (ok (null (other-comp-flag (ecs:get-component world e1 'other-comp))))
        (ok (null (other-comp-flag (ecs:get-component world e3 'other-comp))))
        (ok (null (other-comp-flag (ecs:get-component world e4 'other-comp))))
        (ok (= 0 (length (sys-data other-system))))

        ;; One more 1s update
        ;; Now on e1 flag in other-comp should be raised,
        ;; and since other-system updates after my-system,
        ;; its data should also be updated
        (ecs:update-world world 1000)
        (ok (= 1 (passed-time my-system)))
        (ok (= 2 (comp-tag (ecs:get-component world e1 'my-comp))))
        (ok (= 2 (comp-tag (ecs:get-component world e2 'my-comp))))
        (ok (other-comp-flag (ecs:get-component world e1 'other-comp)))
        ;; e3 and e4 don't have my-comp, so their other-comp is unaffected
        (ok (null (other-comp-flag (ecs:get-component world e3 'other-comp))))
        (ok (null (other-comp-flag (ecs:get-component world e4 'other-comp))))
        (ok (= 1 (length (sys-data other-system))))
        (ok (equal '(34) (sys-data other-system)))))))
