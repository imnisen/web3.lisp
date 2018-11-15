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

           #:net/version
           #:net/listening
           #:net/peer-count

           #:eth/protocol-version
           #:eth/syncing
           #:eth/coinbase
           #:eth/mining
           #:eth/hashrate
           #:eth/gas-price
           #:eth/accounts
           #:eth/block-number
           #:eth/get-balance
           #:eth/get-storage-at
           #:eth/get-transaction-count
           )

  )
