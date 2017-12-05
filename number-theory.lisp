(in-package :number-theory)

;; Quickly finds b^e (mod m).
(defun exptmod (b e m)
  (if (zerop e) 1
    (mod (if (evenp e) 
             ((lambda (x) (* x x)) (exptmod b (/ e 2) m))
             (* b (exptmod b (1- e) m)))
         m)))

(defparameter *package-random-state* (make-random-state t))

(defun generate-fixed-size-number (bits)
  (random (expt 2 bits) *package-random-state*))

(defun prime? (x)
  (cond
    ((= x 1) nil)
    ((= x 2) t)
    ((= (mod x 2) 0) nil)
    (t (let ((bound (ceiling (sqrt x))))
         (loop for a from 3 to bound by 2
               never (= (mod x a) 0))))))

(defun generate-prime (bits)
  (loop for x = (logior (ash 1 (1- bits)) 1 (random (ash 1 bits)))
        when (prime? x)
        return x))

