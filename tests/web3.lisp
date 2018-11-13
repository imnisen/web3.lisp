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


(defmacro define-test-pack (test-fn &rest test-form)
  ;; Http provider test
  `(subtest ,(format nil "~%===Test pack: ~a===~%" test-fn)
     (let ((*test-result* (let ((*provider* (make-instance 'HTTPprovider :uri *http-uri*)))
                            ,test-fn)))
       ,@test-form)

     (let ((*test-result* ,(append test-fn '(:provider (make-instance 'HTTPprovider :uri *http-uri*)))))
       ,@test-form)

     (let ((*test-result* (let ((*provider* (make-instance 'IPCProvider :uri *ipc-uri*)))
                            ,test-fn)))
       ,@test-form)

     (let ((*test-result* ,(append test-fn '(:provider (make-instance 'IPCProvider :uri *ipc-uri*)))))
       ,@test-form)

     ))



;;; tests
(plan nil)

;; special variables
(defvar *http-uri* "http://localhost:8545")
(defvar *ipc-uri* (namestring (merge-pathnames "client-data/geth.ipc" (directory-namestring *load-truename*))))
(defvar *test-result* nil)
(defvar *account-1* "0x76bb8a9c121513a26c20af5342c9a926ffea5885")
(defvar *noexist-account* "0x267be1c1d684f78cb4f6a176c4911b741e4ffdc0")


;; web3
(define-test-pack (web3/client-version)
    (ok (starts-with-p *test-result* "Geth")
        "Can give a client version back"))

(define-test-pack (web3/sha3 "0x68656c6c6f20776f726c64")
    (is *test-result*
        "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
        "Can give a sha3 of data"))

;; net
(define-test-pack (net/version)
    (is *test-result*
        "555" ; check out the t/genesis.json file where it is specified
        "Can give the net version"))

(define-test-pack (net/listening)
    (is *test-result*
        t
        "Can give actively listening status back"))

(define-test-pack (net/peer-count)
    (ok (starts-with-hex-p *test-result*)
        "Can give the peer count"))

;; eth
(define-test-pack (eth/protocol-version)
    (ok (starts-with-hex-p *test-result*)
        "Can give the protocol version"))

(define-test-pack (eth/syncing)
    (ok (listp *test-result*)
        "Can check if geth is syncing"))

(define-test-pack (eth/coinbase)
    (ok (starts-with-hex-p *test-result*)
        "Can check the coinbase address")
  (ok (hex-and-of-bytes-length *test-result* 20)
      "Can check the coinbase address"))

(define-test-pack (eth/mining)
    (ok (typep *test-result* 'boolean)
        "Can check the mining status"))

(define-test-pack (eth/hashrate)
    (ok (starts-with-hex-p *test-result*)
        "Can check what is the hashrate of the mining operation"))

(define-test-pack (eth/gas-price)
    (ok (starts-with-hex-p *test-result*)
        "Can return the current price per gas in wei"))

(define-test-pack (eth/accounts)
    (ok (listp *test-result*)
        "Can return all accounts associated with the node"))

(define-test-pack (eth/block-number)
    (ok (starts-with-hex-p *test-result*)
        "Can return the most recent block"))

(define-test-pack (eth/get-balance *account-1* "latest")
    (ok (starts-with-hex-p *test-result*)
        "Can return the balance of an address"))

(define-test-pack (eth/get-balance *noexist-account* "latest")
    (is *test-result*
        "0x0"
        "Can return the balance of an address"))

(define-test-pack (eth/get-storage-at *noexist-account* "0x0" "latest")
    (ok (starts-with-hex-p *test-result*)
        "Can return the balance of an address"))









;; blah blah blah.

(finalize)
