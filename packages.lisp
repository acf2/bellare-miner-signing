;;;; package.lisp

(defpackage :number-theory
  (:use :cl)
  (:export exptmod
           generate-fixed-size-number
           prime?
           generate-prime))

(defpackage :bellare-miner
  (:use :cl :number-theory))
