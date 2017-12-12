(in-package :number-theory)

(defparameter *package-random-state* (make-random-state t))

(defun generate-fixed-size-number (bits)
  (logior (ash 1 (1- bits)) (random (ash 1 bits) *package-random-state*)))

(defun generate-group-element (base)
  (loop for result = (random base *package-random-state*) if (= (gcd result base) 1) return result))

;; makes a binary expansion of an integer in big-endian format
;; returns binary-array
(defun binary-expansion (value)
  (loop with arry = (make-array (integer-length value) :element-type 'bit)
        for i from 0 to (1- (integer-length value))
        do (setf (bit arry i) (ldb (byte 1 i) value))
        finally (return arry)))

;; log(n) power function
;; can offer reuse of exponent binary expansion
(defun exptmod (base exponent modulo &key (binary-exponent nil binary-exponent-p))
  (let ((exponent (if (and binary-exponent-p binary-exponent)
                    exponent
                    (binary-expansion exponent))))
    (loop with result = 1
          for i from (1- (length exponent)) downto 0
          do (setf result (* result result))
          if (eq (bit exponent i) 1) do (setf result (* result base))
          do (setf result (mod result modulo))
          finally (return result))))

;; binary search for extracting all powers of 2 from number
;; let z be the number of tail zeros in given number
;; first loop: find such t that (< (expt 2 t) z (expt 2 (1+ t))) is T
;; second loop: for every s from t-1 downto 0, if (<= (+ result (ash 1 s)) z) is T, then add (ash 1 s) to result
(defun tail-zeros (num)
  "With binary search find number of all tailing zeros for binary representation of given integer"
  (let ((zero-tail-upper-bound-power-of-2 0))
    (loop while (eq (ldb (byte (ash 1 (1+ zero-tail-upper-bound-power-of-2)) 0) num) 0)
          do (setf zero-tail-upper-bound-power-of-2
                   (1+ zero-tail-upper-bound-power-of-2)))
    (loop with result = (ash 1 zero-tail-upper-bound-power-of-2)
          for test-power-of-2 from (1- zero-tail-upper-bound-power-of-2) downto 0
          if (eq (ldb (byte (+ result (ash 1 test-power-of-2)) 0) num) 0)
          do (setf result (+ result (ash 1 test-power-of-2)))
          finally (return result))))

(defun prime? (test-number &optional (reliability 20))
  "Miller-Rabin primality test
NIL return means \"composite\" and is true for all occurances
T return means \"prime\" and can be error with probability (expt 1/4 reliability)"
  (if (eq (mod test-number 2) 0)
    (return-from prime? nil)
    (let* ((power-of-2 (tail-zeros (1- test-number)))
           (even-part (ash (1- test-number) (- power-of-2))))
      (loop for i from 1 to reliability
            with test-base and chain-part
            do (setf test-base (+ (random (- test-number 3) *package-random-state*) 2))
            do (setf chain-part (exptmod test-base even-part test-number))
            if (not (or (= chain-part 1) (= chain-part (1- test-number))))
            do (loop with j = 1
                     while (and (not (= chain-part (1- test-number))) (< j power-of-2))
                     do (setf chain-part (exptmod chain-part 2 test-number))
                     if (= chain-part 1) do  (return-from prime? nil)
                     do (setf j (1+ j))
                     finally (when (not (= chain-part (1- test-number)))
                               (return-from prime? nil)))
            finally (return-from prime? t)))))

(defun generate-prime (bits)
  (loop for x = (logior (ash 1 (1- bits)) 1 (random (ash 1 bits)))
        when (prime? x)
        return x))

