(in-package :cl)

(defpackage :lbge.hash
  (:use :cl)
  (:export :make-hash
           :hash-get
           :hash-set
           :hash-equal))

(defpackage :lbge.beacon
  (:use :cl)
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
