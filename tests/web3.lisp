(defpackage web3-test
  (:use :cl :web3
        :prove))
(in-package :web3-test)

;; NOTE: To run this test file, execute `(asdf:test-system :web3)' in your Lisp.

;; configure prove
(setf prove:*debug-on-error* t)
(setf prove:*enable-colors* nil)

;;; helper functions

(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun starts-with-hex-p (str)
  "Determine whether `str` starts with `0x`"
  (starts-with-p str "0x"))

(defun hex-and-of-length (str length)
  (and (starts-with-hex-p str)
       (= (- (length str) 2) length)))

(defun hex-and-of-bytes-length (str length)
  (and (starts-with-hex-p str)
       (= (- (length str) 2) (* 2 length))))

(defun transaction-hash-p (str)
  (hex-and-of-length str 64))

(defun booleanp (value)
  "Determine, if the value is t or nil, return t, otherwise return nil"
  (typep value 'boolean))

(defun block-p (b)
  (or (not b)
      (and b (listp b)
           (every #'(lambda (key) (assoc key b))
                  '(:number :hash :parent-hash :nonce
                    :sha-3-*uncles :logs-bloom :transactions-root
                    :state-root :receipts-root :miner :difficulty
                    :total-difficulty :extra-data :size :gas-limit
                    :gas-used :timestamp :transactions :uncles)))))

(defun transaction-p (tr)
  (or (not tr)
      (and tr (listp tr)
           (every #'(lambda (key) (assoc key tr))
                  '(:block-hash :block-number :from :gas :gas-price
                    :hash :input :nonce :to :transaction-index :value
                    :v :r :s)))))

(defun either (predicate lst)
  (and (listp lst)
       (= 2 (length lst))
       (let ((first-r (funcall predicate (first lst)))
             (second-r (funcall predicate (second lst))))
         (cond ((and first-r (not second-r)) t)
               ((and (not first-r) second-r) t)
               (t nil)))))

(defun transaction-receipt-p (tr)
  (or (not tr)
      (and tr (listp tr)
           (every #'(lambda (key) (assoc key tr))
                  '(:transaction-hash :transaction-index :block-hash :block-number :from :to :cumulative-gas-used :gas-used :contract-address :logs :logs-bloom))
           (either #'(lambda (key) (assoc key tr)) '(:root :status)))))


(defmacro deftest (test-fn &rest test-form)
  ;; Http provider test
  `(subtest ,(format nil "~%===Test pack: ~a===~%" test-fn)

     (let ((*result* (let ((*provider* (make-instance 'HTTPprovider :uri *http-uri*)))
                       ,test-fn)))
       ,@test-form)

     (let ((*result* ,(append test-fn '(:provider (make-instance 'HTTPprovider :uri *http-uri*)))))
       ,@test-form)

     (let ((*result* (let ((*provider* (make-instance 'IPCProvider :uri *ipc-uri*)))
                       ,test-fn)))
       ,@test-form)

     (let ((*result* ,(append test-fn '(:provider (make-instance 'IPCProvider :uri *ipc-uri*)))))
       ,@test-form)

     ))

;;; tests
(plan nil)

;; test used variables
(defvar *http-uri* "http://localhost:8545")
(defvar *ipc-uri* (namestring (merge-pathnames "client-data/geth.ipc" (directory-namestring *load-truename*))))
(defvar *result* nil)

;; web3
(deftest (web3/client-version)
    (ok (starts-with-p *result* "Geth")
        "Can give a client version back"))

(deftest (web3/sha3 "0x68656c6c6f20776f726c64")
    (is *result*
        "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
        "Can give a sha3 of data"))

;; net
(deftest (net/version)
    (is *result*
        "555" ; check out the t/genesis.json file where it is specified
        "Can give the net version"))

(deftest (net/listening)
    (is *result*
        t
        "Can give actively listening status back"))

(deftest (net/peer-count)
    (ok (starts-with-hex-p *result*)
        "Can give the peer count"))

;; eth
(deftest (eth/protocol-version)
    (ok (starts-with-hex-p *result*)
        "Can give the protocol version"))

;; TODO check is it right to use listp as a checker?
(deftest (eth/syncing)
    (ok (listp *result*)
        "Can check if geth is syncing"))

(deftest (eth/coinbase)
    (ok (hex-and-of-bytes-length *result* 20)
        "Can check the coinbase address"))

(deftest (eth/mining)
    (ok (booleanp *result*)
        "Can check the mining status"))

(deftest (eth/hashrate)
    (ok (starts-with-hex-p *result*)
        "Can check what is the hashrate of the mining operation"))

(deftest (eth/gas-price)
    (ok (starts-with-hex-p *result*)
        "Can return the current price per gas in wei"))

(deftest (eth/accounts)
    (ok (listp *result*)
        "Can return all accounts associated with the node"))

(deftest (eth/block-number)
    (ok (starts-with-hex-p *result*)
        "Can return the most recent block"))

(defvar *account-1* "0x76bb8a9c121513a26c20af5342c9a926ffea5885") ;; from genesis file
(deftest (eth/get-balance *account-1* "latest")
    (ok (starts-with-hex-p *result*)
        "Can return the balance of an address"))

(deftest (eth/get-storage-at "0x295a70b2de5e3953354a6a8344e616ed314d7251" "0x0" "latest")
    (ok (starts-with-hex-p *result*)
        "Can return the value from a storage position at a given address"))

(deftest (eth/get-transaction-count *account-1*  "latest")
    (ok (starts-with-hex-p *result*)
        "Can return number of transactions from a given address"))

(defvar *block-hash* (cdr (assoc :hash (eth/get-block-by-number "0x1" nil :provider (make-instance 'HTTPprovider :uri *http-uri*)))))
(deftest (eth/get-block-transaction-count-by-hash *block-hash*)
    (ok (starts-with-hex-p *result*)
        "Can return number of block transactions from a block hash"))

(deftest (eth/get-block-transaction-count-by-number "latest")
    (ok (starts-with-hex-p *result*)
        "Can return number of block transactions from a block number"))

(deftest (eth/get-uncle-count-by-block-hash *block-hash*)
    (ok (starts-with-hex-p *result*)
        "Can return the number of uncles in a block from a block matching the given block hash."))

(deftest (eth/get-uncle-count-by-block-number "latest")
    (ok (starts-with-hex-p *result*)
        "Can return the number of uncles in a block from a block matching the given block number."))

(deftest (eth/get-code "0xa94f5374fce5edbc8e2a8697c15331677e6ebf0b" "latest")
    (ok (starts-with-hex-p *result*)
        "Can return code at a given address."))

(deftest (eth/sign *account-1* "0xdeadbeaf")
    (ok (starts-with-hex-p *result*)
        "Can sign data"))

(defvar *account-2* "0x7fde6bc5d0560b624609d50e72a736493e3f0e70") ;; from genesis file
(defvar *transaction-object* (web3:make-transaction-object :from *account-1* :to *account-2*  :value "0x9184e72a" :data "") )
(deftest (eth/send-transaction *transaction-object*)
    (ok (transaction-hash-p *result*)
        "Can send transaction"))

;; TODO
;; when request with params in https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_sendrawtransaction
;; geth return {"jsonrpc":"2.0","id":1,"error":{"code":-32000,"message":"rlp: element is larger than containing list"}}
;; firgure out what's wrong latter
;; (deftest (eth/send-raw-transaction "0xd46e8dd67c5d32be8d46e8dd67c5d32be8058bb8eb970870f072445675058bb8eb970870f072445675")
;;     (ok (transaction-hash-p *result*)
;;         "Can send raw transaction"))

(defvar *transaction-object2* (web3:make-transaction-object2 :to *account-2* ))
(deftest (eth/call *transaction-object2*  "latest")
    (ok (starts-with-hex-p *result*)
        "Can execute a new message call immediately without creating a transaction"))

(defvar *transaction-object3* (web3:make-transaction-object3 :from *account-1* :to *account-2* ))
(deftest (eth/estimate-gas *transaction-object3*)
    (ok (starts-with-hex-p *result*)
        "Can make a call or transaction, which won't be added to the blockchain and returns the used gas, which can be used for estimating the used gas"))


(deftest (eth/get-block-by-hash *block-hash* t)
    (ok (block-p *result*)
        "Can return information about a block by hash"))

(deftest (eth/get-block-by-number "0x1" t)
    (ok (block-p *result*)
        "Can return information about a block by number"))

(defvar *transaction-hash* (eth/send-transaction *transaction-object* :provider (make-instance 'HTTPprovider :uri *http-uri*)))
(deftest (eth/get-transaction-by-hash *transaction-hash*)
    (ok (transaction-p *result*)
        "Can return the information about a transaction requested by transaction hash."))

(deftest (eth/get-transaction-by-block-hash-and-index *block-hash* "0x0")
    (ok (transaction-p *result*)
        "Can return information about a transaction by block hash and transaction index position."))

(deftest (eth/get-transaction-by-block-number-and-index "0x1" "0x0")
    (ok (transaction-p *result*)
        "Can return information about a transaction by block hash and transaction index position."))

(deftest (eth/get-transaction-receipt *transaction-hash*)
    (ok (transaction-receipt-p *result*)
        "Can return the receipt of a transaction by transaction hash."))

(deftest (eth/get-uncle-by-block-hash-and-index *block-hash* "0x0")
    (ok (block-p *result*)
        "Can return information about a uncle of a block by hash and uncle index position."))

(deftest (eth/get-uncle-by-block-number-and-index "0x1" "0x0")
    (ok (block-p *result*)
        "Can return information about a uncle of a block by block number  and uncle index position."))


































;; blah blah blah.

(finalize)
