;;;; load.lisp

(ql:quickload :s-base64)
(ql:quickload :ironclad)
(ql:quickload :babel)

(pushnew (directory-namestring (or *load-truename*
                                   *default-pathname-defaults*))
         asdf:*central-registry*
         :test #'equal)
(asdf:load-system :bellare-miner-signing)