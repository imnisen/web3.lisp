(defpackage web3
  (:use :cl)

  ;; special variable
  (:export #:*provider*)

  ;; providers
  (:export #:HTTPProvider
           #:IPCProvider)

  ;; maker function  ;; todo maybe the function name need rethink
  (:export #:make-transaction-object
           #:make-transaction-object2
           #:make-transaction-object3
           #:make-filter-object
           #:make-filter-object2
           #:make-filter-object3
           #:make-whisper-object)  

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
           #:eth/get-block-transaction-count-by-hash
           #:eth/get-block-transaction-count-by-number
           #:eth/get-uncle-count-by-block-hash
           #:eth/get-uncle-count-by-block-number
           #:eth/get-code
           #:eth/sign
           #:eth/send-transaction
           #:eth/send-raw-transaction
           #:eth/call
           #:eth/estimate-gas
           #:eth/get-block-by-hash
           #:eth/get-block-by-number
           #:eth/get-transaction-by-hash
           #:eth/get-transaction-by-block-hash-and-index
           #:eth/get-transaction-by-block-number-and-index
           #:eth/get-transaction-receipt
           #:eth/get-uncle-by-block-hash-and-index
           #:eth/get-uncle-by-block-number-and-index
           #:eth/new-filter
           #:eth/new-block-filter
           #:eth/new-pending-transaction-filter
           #:eth/uninstall-filter
           #:eth/get-filter-changes
           #:eth/get-filter-logs
           #:eth/get-logs
           #:eth/get-work
           #:eth/submit-work
           #:eth/submit-hashrate
           #:db/put-string
           #:db/get-string
           #:db/put-hex
           #:db/get-hex
           #:shh/version
           #:shh/post
           #:shh/new-identity
           #:shh/has-identity
           #:shh/new-group
           #:shh/add-togroup
           #:shh/new-filter
           #:shh/uninstall-filter
           #:shh/get-filter-changes
           #:shh/get-messages

           ))
