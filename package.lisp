(defpackage web3
  (:use :cl)

  ;; special variable
  (:export #:*provider*)

  ;; providers
  (:export #:HTTPProvider
           #:IPCProvider)

  ;; interfaces
  (:export #:web3/client-version
           #:web3/sha3
           #:net/version)

  )
