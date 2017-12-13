;;;; forward-secure-signing.lisp
;;;; This package implements Bellare-Miner scheme

(in-package :bellare-miner)

(defun decode-base64 (str)
  (with-input-from-string (in str)
    (s-base64:decode-base64-bytes in)))

(defun encode-base64 (arr)
  (with-output-to-string (out)
    (s-base64:encode-base64-bytes arr out)))

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
    (format t "BITS: ~A~%" bits)
    (append
      (loop for bl from 0 to (1- (floor bits 512))
            append (extract-bits rnd-arr 512)
            do (setf rnd-arr (ironclad:digest-sequence :sha512 msg-bytes)))
      (extract-bits rnd-arr (mod bits 512)))))

(defun integer-to-bytes (int)
  (let ((result (make-array 1 :adjustable t :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop with num = int
          for i from 0 to (1- (ceiling (integer-length num) 8))
          for bite = (ldb (byte 8 (* i 8)) num)
          do (vector-push-extend bite result))
    result))

(defun generate-blum-number ()
  (loop for num = (generate-prime (/ *boundary* 2)) when (= (mod num 4) 3) return num))

(defun generate-key (time-periods)
  (let* ((p (generate-blum-number))
         (q (loop for atpt = (generate-blum-number) if (not (= atpt p)) return atpt))
         (N (* p q))
         (fi-N (* (1- p) (1- q)))
         (secret-key (list :mod N :time-periods time-periods :current-state 0 :key (list)))
         (public-key (list :mod N :time-periods time-periods :key (list))))
    (loop for i from 1 to *challenge-length*
          for key-part = (generate-group-element N)
          do (push key-part (getf secret-key :key))
          do (push (exptmod key-part (mod (ash 1 (1+ time-periods)) fi-N) N) (getf public-key :key)))
    (list :secret-key secret-key :public-key public-key)))

(defun update-key (secret-key)
  (if (= (getf secret-key :current-state) (getf secret-key :time-periods))
    nil
    (list :mod (getf secret-key :mod)
          :time-periods (getf secret-key :time-periods)
          :current-state (1+ (getf secret-key :current-state))
          :key (loop for key-part in (getf secret-key :key)
                     collect (exptmod key-part
                                      2
                                      (getf secret-key :mod))))))


