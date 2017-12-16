(defparameter argv (apply-argv:parse-argv (loop for elem in (rest *posix-argv*)
                                                collect (coerce elem 'string))))
(defparameter current-dir (pathname-directory *default-pathname-defaults*))

(format t "~S~%" *posix-argv*)
(format t "~S~%" argv)
(format t "~S~%" current-dir)

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

(setf-if-not-nil bellare-miner:*boundary* (getf-int argv :bits))
(setf-if-not-nil bellare-miner:*challenge-length* (getf-int argv :points))

(let ((private-key (getf argv :private-key))
      (public-key (getf argv :public-key))
      (time-periods (getf-int argv :time-periods)))
  (when (getf argv :generate-key)
    (fail-quit (private-key "You should specify secret key filename for generation of the key~%")
               (public-key "You should specify public key filename for generation of the key~%")
               (time-periods "You should specify number of time periods for generation of the key~%"))
    (let* ((key (bellare-miner:generate-key time-periods)))
      (with-standard-io-syntax
        (with-open-file (pub-out (getf argv :public-key) :direction :output :if-exists :supersede)
          (format pub-out "~S~%" (getf key :public-key)))
        (with-open-file (prv-out (getf argv :private-key) :direction :output :if-exists :supersede)
          (format prv-out "~S~%" (getf key :private-key)))))))
