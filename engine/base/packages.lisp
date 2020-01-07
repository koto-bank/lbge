(in-package :cl)

(defpackage :lbge.hash
  (:use :cl)
  (:export :make-hash
           :get-hash
           :set-hash))

(defpackage :lbge.beacon
  (:use :cl)
  (:nicknames :beacon)
  (:export :make
           :name
           :links
           :link
           :unlink
           :unlink-all
           :blink))

(defpackage :lbge.utils
  (:use :cl)
  (:export :println))
