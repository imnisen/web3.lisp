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
       (= (- (length str) 2) (* 2  length))))

(defun transaction-hash-p (str)
  (hex-and-of-length str 64))

(defun transaction-p (obj)
  ());; TODO


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

;; special variables
(defvar *http-uri* "http://localhost:8545")
(defvar *ipc-uri* (namestring (merge-pathnames "client-data/geth.ipc" (directory-namestring *load-truename*))))
(defvar *result* nil)
(defvar *account-1* "0x76bb8a9c121513a26c20af5342c9a926ffea5885")
(defvar *account-2* "7fde6bc5d0560b624609d50e72a736493e3f0e70")
(defvar *noexist-account* "0x267be1c1d684f78cb4f6a176c4911b741e4ffdc0")
(defvar *address-1* "0xa94f5374fce5edbc8e2a8697c15331677e6ebf0b")

(defvar *block-hash* '()) ;; todo will be get from another function
(defvar *signed-transaction-data* ("0xd46e8dd67c5d32be8d46e8dd67c5d32be8058bb8eb970870f072445675058bb8eb970870f072445675"))
(defvar *full-tx-p-t* t)
(defvar *full-tx-p-nil* nil)
(defvar *transaction-object* (make-transaction-object  :from *account-1* :to *account-2*  :value "0x9184e72a" :data "") )

(defvar *block-number* "0x1")
(defvar *transaction-hash* "0x0000000000000000000000000000000000000000000000000000000000000000")
(defvar *transaction-index* "0x0")




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

(deftest (eth/syncing)
    (ok (listp *result*)
        "Can check if geth is syncing"))

(deftest (eth/coinbase)
    (ok (starts-with-hex-p *result*)
        "Can check the coinbase address")
  (ok (hex-and-of-bytes-length *result* 20)
      "Can check the coinbase address"))

(deftest (eth/mining)
    (ok (typep *result* 'boolean)
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

(deftest (eth/get-balance *account-1* "latest")
    (ok (starts-with-hex-p *result*)
        "Can return the balance of an address"))

(deftest (eth/get-balance *noexist-account* "latest")
    (is *result*
        "0x0"
        "Can return the balance of an address"))

(deftest (eth/get-storage-at *noexist-account* "0x0" "latest")
    (ok (starts-with-hex-p *result*)
        "Can return the value from a storage position at a given address"))

(deftest (eth/get-transaction-count *account-1*  "latest")
    (ok (starts-with-hex-p *result*)
        "Can return number of transactions from a given address"))

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

(deftest (eth/get-code *address-1* "latest")
    (ok (starts-with-hex-p *result*)
        "Can return code at a given address."))

(deftest (eth/sign *account-1* "0xdeadbeaf")
    (ok (starts-with-hex-p *result*)
        "Can sign data"))

(deftest (eth/send-transaction *transaction-object*)
    (ok (transaction-hash-p *result*)
        "Can send transaction"))

(deftest (eth/send-raw-transaction *signed-transaction-data*)
    (ok (transaction-hash-p *result*)
        "Can send raw transaction"))

(deftest (eth/call *transaction-object*  "latest")
    (ok (starts-with-hex-p *result*)
        "Can execute a new message call immediately without creating a transaction"))

(deftest (eth/estimate-gas *transaction-object*)
    (ok (starts-with-hex-p *result*)
        "Can make a call or transaction, which won't be added to the blockchain and returns the used gas, which can be used for estimating the used gas"))

(deftest (eth/get-block-by-hash *block-hash* *full-tx-p-t*)
    (ok (starts-with-hex-p (cdr (assoc :hash *result*)))
        "Can Returns information about a block by hash."))

(deftest (eth/get-block-by-number *block-number* *full-tx-p-nil*)
    (ok (starts-with-hex-p (cdr (assoc :hash *result*))))
  "Can Returns information about a block by number.")

(deftest (eth/get-transaction-by-hash *transaction-hash*)
    (ok (transaction-p *result*)
        "Can return the information about a transaction requested by transaction hash."))

(deftest (eth/get-transaction-by-hash-and-index *transaction-hash* *transaction-index*)
    (ok (transaction-p *result*)
        "Can return information about a transaction by block hash and transaction index position."))

(deftest (eth/get-transaction-by-block-number-and-index *block-number* *transaction-index*)
    (ok (transaction-p *result*)
        "Can return information about a transaction by block hash and transaction index position."))



(deftest (eth/get-transaction-receipt *transaction-hash*)
    (ok (transaction-p *result*)
        "Can return the receipt of a transaction by transaction hash."))

(deftest (eth/get-uncle-by-block-hash-and-index *block-hash* "0x0")
    (ok (block-p *result*)
        "Can return information about a uncle of a block by hash and uncle index position."))

(deftest (eth/get-uncle-by-block-hash-and-index *block-number* "0x0")
    (ok (block-p *result*)
        "Can return information about a uncle of a block by block number  and uncle index position."))


































;; blah blah blah.

(finalize)
