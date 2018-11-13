#|
  This file is a part of web3 project.
  Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

(defsystem "web3-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Nisen"
  :license ""
  :depends-on ("web3"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "web3"))))
  :description "Test system for web3"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
