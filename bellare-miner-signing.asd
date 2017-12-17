;;;; bellare-miner-signing.asd

(asdf:defsystem bellare-miner-signing
  :description "bellare-miner-signing: implementation of forward secure signing algorithm"
  :version "0.0.2"
  :author "Дмитрий Киселёв <acrfnv@gmail.com>"
  :depends-on (:ironclad :babel)
  :components ((:file "packages")
               (:file "number-theory" :depends-on ("packages"))
               (:file "bellare-miner" :depends-on ("number-theory" "packages"))))

