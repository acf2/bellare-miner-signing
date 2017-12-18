;;;; cli.lisp
;;;; Command-line interface for Bellare-Miner signing scheme

(defparameter argv (apply-argv:parse-argv (loop for elem in (rest *posix-argv*)
                                                collect (coerce elem 'string))))
(defparameter current-dir (pathname-directory *default-pathname-defaults*))

(defun getf-int (p-list key)
  (when (getf p-list key)
    (parse-integer (getf p-list key) :junk-allowed t)))

(defmacro setf-if-not-nil (place value)
  `(when (not (null ,value))
     (setf ,place ,value)))

(defmacro fail-quit (&rest symbol-quitmsg-list)
  (let ((fail (gensym)))
    `(let ((,fail nil))
       ,@(loop for symbol-quitmsg-pair in symbol-quitmsg-list
               collect `(unless ,(first symbol-quitmsg-pair)
                          (format t ,(second symbol-quitmsg-pair))
                          (setf ,fail t)))
       (when ,fail (sb-ext:exit)))))

(defun force-write-to-file (filename value)
  (with-standard-io-syntax
    (with-open-file (f filename :direction :output :if-exists :supersede)
      (format f "~S~%" value))))

(defun read-from-file-if-exists (filename)
  (with-standard-io-syntax
    (with-open-file (f filename :direction :input :if-does-not-exist nil)
      (and f (read f)))))

(defun read-bytes-from-file-if-exists (filename)
  (with-standard-io-syntax
    (with-open-file (f filename :element-type '(unsigned-byte 8) :direction :input :if-does-not-exist nil)
      (loop with result = nil
            with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
            with bytes
            do (setf bytes (read-sequence buffer f))
            do (setf result (concatenate '(vector (unsigned-byte 8)) result (subseq buffer 0 bytes)))
            while (= bytes 4096)
            finally (return result)))))

(setf-if-not-nil bm-signing:*boundary* (getf-int argv :bits))
(setf-if-not-nil bm-signing:*challenge-length* (getf-int argv :points))

(defun generate-key (private-key-file public-key-file time-periods)
  (fail-quit (private-key-file "You should specify private key filename for generation of the key~%")
             (public-key-file "You should specify public key filename for generation of the key~%")
             (time-periods "You should specify number of time periods for generation of the key~%"))
  (let ((key (bm-signing:generate-key time-periods)))
    (force-write-to-file public-key-file (getf key :public-key))
    (force-write-to-file private-key-file (getf key :private-key))))

(defun update-key (private-key-file)
  (fail-quit (private-key-file "Please specify private key for update~%"))
  (let ((private-key (read-from-file-if-exists private-key-file)))
    (fail-quit (private-key "Incorrect private key~%"))
    (setf private-key (bm-signing:update-key private-key))
    (force-write-to-file private-key-file private-key)))

(defun sign (message-file private-key-file signature-file)
  (fail-quit (private-key-file "You should specify a private key for signing~%")
             (message-file "You can not sign nothing: please specify a file to sign~%")
             (signature-file "Please, provide a signature file name~%"))
  (let ((private-key (read-from-file-if-exists private-key-file))
        (message (read-bytes-from-file-if-exists message-file)))
      (fail-quit (private-key "Incorrect private key~%"))
      (force-write-to-file signature-file (bm-signing:sign message private-key))))

(defun verify (message-file signature-file public-key-file)
  (fail-quit (public-key-file "You should specify a public key for verifying~%")
             (message-file "You can not verify nothing: please specify a file~%")
             (signature-file "You should provide a signature to check~%"))
  (let ((public-key (read-from-file-if-exists public-key-file))
        (message (read-bytes-from-file-if-exists message-file))
        (signature (read-from-file-if-exists signature-file)))
    (fail-quit (public-key "Incorrect public key~%")
               (signature "Incorrect signature~%"))
    (if (bm-signing:verify message signature public-key)
      (format t "~A: valid signature for '~A'~%" signature-file message-file)
      (format t "~A: INVALID SIGNATURE FOR '~A'~%ATTENTION: MESSAGE OR SIGNATURE WERE MODIFIED~%" signature-file message-file))))

(cond
  ((getf argv :generate-key)
   (generate-key (getf argv :private-key)
                 (getf argv :public-key)
                 (getf-int argv :time-periods)))
  ((getf argv :update-key)
   (update-key (getf argv :private-key)))
  ((getf argv :sign)
   (sign (getf argv :message)
         (getf argv :private-key)
         (getf argv :signature)))
  ((getf argv :verify)
   (verify (getf argv :message)
           (getf argv :signature)
           (getf argv :public-key))))
