(defpackage :lbge.ecs
  (:use :cl)
  (:local-nicknames
   (:ax :alexandria)
   (:ss :lbge.sparse-set)
   (:u :lbge.utils)
   (:h :lbge.hash))
  (:export
   :world
   :make-world
   :update-world

   ;; System exports
   :system
   :bind-components
   :update
   :add-systems
   :get-system

   ;; Component exports
   :component
   :iterate-comp
   :get-component

   :entity
   :create-entity
   :delte-entity
   :existsp))
