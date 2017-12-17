;;;; forward-secure-signing.lisp
;;;; This package implements Bellare-Miner scheme

(in-package :bellare-miner)

; TODO make it 4096
(defparameter *boundary* 256)
(defparameter *challenge-length* 3)

(defun random-oracle (msg-bytes &optional (bits *challenge-length*))
  (defun extract-bits (bytes bits)
    (let ((result (list)))
      (loop for i from 0 to (1- (floor bits 8))
            do (loop for j from 0 to 7
                     do (push (ldb (byte 1 j) (aref bytes i)) result)))
      (loop for j from 0 to (1- (mod bits 8))
            do (push (ldb (byte 1 j) (aref bytes (1+ (floor bits 8)))) result))
      result))
  (let ((rnd-arr (ironclad:digest-sequence :sha512 msg-bytes)))
    (append
      (loop for bl from 0 to (1- (floor bits 512))
            append (extract-bits rnd-arr 512)
            do (setf rnd-arr (ironclad:digest-sequence :sha512 msg-bytes)))
      (extract-bits rnd-arr (mod bits 512)))))

(defun generate-blum-number ()
  (loop for num = (generate-prime (/ *boundary* 2)) when (= (mod num 4) 3) return num))

(defun generate-key (time-periods)
  (let* ((p (generate-blum-number))
         (q (loop for atpt = (generate-blum-number) if (not (= atpt p)) return atpt))
         (N (* p q))
         (fi-N (* (1- p) (1- q)))
         (private-key (list :mod N :time-periods time-periods :current-state 0 :key (list)))
         (public-key (list :mod N :time-periods time-periods :key (list))))
    (loop for i from 1 to *challenge-length*
          for key-point = (generate-group-element N)
          do (push key-point (getf private-key :key))
          do (push (exptmod key-point (mod (ash 1 (1+ time-periods)) fi-N) N) (getf public-key :key)))
    (list :private-key private-key :public-key public-key)))

(defun update-key (private-key)
  (if (= (getf private-key :current-state) (getf private-key :time-periods))
    nil
    (list :mod (getf private-key :mod)
          :time-periods (getf private-key :time-periods)
          :current-state (1+ (getf private-key :current-state))
          :key (loop for key-point in (getf private-key :key)
                     collect (exptmod key-point 2 (getf private-key :mod))))))

(defun sign (message-bytes private-key)
  (let* ((random-inversible (generate-group-element (getf private-key :mod)))
         (commitment (exptmod random-inversible
                              (ash 1 (- (1+ (getf private-key :time-periods))
                                        (getf private-key :current-state)))
                              (getf private-key :mod)))
         (challenge (random-oracle (concatenate '(vector (unsigned-byte 8))
                                                (ironclad:integer-to-octets (getf private-key :current-state))
                                                (ironclad:integer-to-octets commitment)
                                                message-bytes)))
         (response (reduce (lambda (num1 num2)
                             (mod (* num1 num2) (getf private-key :mod)))
                           (cons random-inversible
                                 (loop for key-point in (getf private-key :key)
                                       for is-present in challenge
                                       collect (if (= is-present 1) key-point 1))))))
    (list :timestamp (getf private-key :current-state)
          :commitment commitment
          :response response)))

(defun verify (message-bytes signature public-key)
  (let ((challenge (random-oracle (concatenate '(vector (unsigned-byte 8))
                                               (ironclad:integer-to-octets (getf signature :timestamp))
                                               (ironclad:integer-to-octets (getf signature :commitment))
                                               message-bytes))))
    (= (exptmod (getf signature :response)
                (ash 1 (- (1+ (getf public-key :time-periods)) (getf signature :timestamp)))
                (getf public-key :mod))
       (reduce (lambda (num1 num2)
                 (mod (* num1 num2) (getf public-key :mod)))
               (cons (getf signature :commitment)
                     (loop for key-point in (getf public-key :key)
                           for is-present in challenge
                           collect (if (= is-present 1) key-point 1)))))))

