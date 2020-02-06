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
   :princln))
