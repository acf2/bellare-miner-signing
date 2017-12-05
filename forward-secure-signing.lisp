;;;; forward-secure-signing.lisp
;;;; This package implements Bellare-Miner scheme

(in-package :bellare-miner)

(defun decode-base64 (str)
  (with-input-from-string (in str)
    (s-base64:decode-base64-bytes in)))

(defun encode-base64 (arr)
  (with-output-to-string (out)
    (s-base64:encode-base64-bytes arr out)))

; TODO make it 4096 when you implement miller-rabin
(defparameter *boundary* 64)

(defun generate-blum-number ()
  (loop for num = (number-theory:generate-prime (/ *boundary* 2)) when (eql (mod num 4) 3) return num))

(defun generate-key ()
  (let* ((p (generate-blum-number))
         (q (generate-blum-number))
         (N (* p q)))
    (format t "~A * ~A = ~A~%" p q N)
    (format t "~%~%~A ~A~%" (mod p 4) (mod q 4))))
