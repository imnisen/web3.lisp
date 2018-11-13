#|
  This file is a part of web3 project.
  Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

#|
  Author: Nisen (imnisen@gmail.com)
|#

(defsystem "web3"
  :version "0.1.0"
  :author "Nisen"
  :license ""
  :depends-on (#:cl-json
               #:drakma
               #:flexi-streams
               #:iolib)
  :components ((:file "package")
               (:file "src/providers/base" :depends-on ("package"))
               (:file "src/providers/http" :depends-on ("package" "src/providers/base"))
               (:file "src/providers/ipc" :depends-on ("package" "src/providers/base"))
               (:file "src/web3" :depends-on ("package"))
               )
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "web3-test"))))
