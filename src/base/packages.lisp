(in-package :cl)

(defpackage :lbge.hash
  (:use :cl)
  (:export :make-hash
           :hash-get
           :hash-set
           :get-internal
           :hash-equal))

(defpackage :lbge.beacon
  (:use :cl)
  (:local-nicknames (:ax :alexandria))
  (:export :make
           :name
           :links
           :link
           :unlink
           :unlink-all
           :blink))

(defpackage :lbge.utils
  (:use :cl)
  (:export
   :println
   :trim-newlines
   :merge-lines
   :princln
   :bind))

(defpackage :lbge.timer
  (:use :cl)
  (:shadow :restart)
  (:local-nicknames (:beacon :lbge.beacon))
  (:export
   :timer
   :make
   :start
   :link
   :unlink
   :unlink-all
   :stop
   :update
   :restart))

(defpackage :lbge.sparse-set
  (:use :cl)
  (:shadow :remove :get)
  (:export
   :make
   :id
   :gen
   :existsp
   :get
   :insert
   :remove
   :clear))
