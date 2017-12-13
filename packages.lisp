;;;; package.lisp

(defpackage :number-theory
  (:use :cl)
  (:export exptmod
           generate-fixed-size-number
           generate-group-element
           prime?
           generate-prime))

(defpackage :bellare-miner
  (:use :cl :number-theory)
  (:export generate-key
           update-key))