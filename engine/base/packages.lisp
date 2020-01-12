(in-package :cl)

(defpackage :lbge.hash
  (:use :cl)
  (:nicknames :hash)
  (:export :make
           :get
           :set
           :equalp))

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
