;;;; bm-signing.asd

(asdf:defsystem bm-signing
  :description "bm-signing: implementation of Bellare-Miner forward secure signing algorithm"
  :version "0.0.2"
  :author "Дмитрий Киселёв <acrfnv@gmail.com>"
  :depends-on (:ironclad :babel)
  :components ((:file "packages")
               (:file "number-theory" :depends-on ("packages"))
               (:file "bm-signing" :depends-on ("number-theory" "packages"))))

