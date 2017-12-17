;;;; load.lisp

(ql:quickload :ironclad :silent t)
(ql:quickload :babel :silent t)
(ql:quickload :apply-argv :silent t)

(defparameter work-dir (directory-namestring (or *load-truename* *default-pathname-defaults*)))

(pushnew work-dir
         asdf:*central-registry*
         :test #'equal)

(asdf:load-system :bellare-miner-signing)
