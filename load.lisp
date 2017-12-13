;;;; load.lisp

(ql:quickload :s-base64 :silent t)
(ql:quickload :ironclad :silent t)
(ql:quickload :babel :silent t)

(pushnew (directory-namestring (or *load-truename*
                                   *default-pathname-defaults*))
         asdf:*central-registry*
         :test #'equal)

(asdf:load-system :bellare-miner-signing)