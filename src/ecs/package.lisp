(defpackage :lbge.ecs.entity-storage
  (:use :cl)
  (:local-nicknames
   (:ss :lbge.sparse-set))
  (:export
   :entity-storage
   :delete-entity
   :create-entity
   :entity-existsp))

(defpackage :lbge.ecs
  (:use
   :cl)
  (:local-nicknames
   (:ax :alexandria))
  (:export
   :world
   :entity
   :component
   :system

   :defsystem
   :defcomponent

   :create-entity
   :delte-entity
   :existsp))
