(in-package :web3)

;;; ------------------------------------
;;; Utils
;;; ------------------------------------

(defun response-error (response)
  (cdr (assoc :error response)))


(defun make-transaction-object (&key from to gas gas-price value data nonce)
  (if (not (and from to data))
      (error "`from`, `to` and `data` are not optional")
      (remove nil
              `((:from . ,from)
                (:to . ,to)
                ,(when gas `(:gas . ,gas))
                ,(when gas-price `(:gasPrice . ,gas-price))
                ,(when value `(:value . ,value))
                (:data . ,data)
                ,(when nonce `(:nonce . ,nonce))))))

;; ;; not used
;; ;; read-sequence
;; (defun read-all-char-by-char (stream)
;;   (with-output-to-string (out)
;;     (loop for char = (read-char stream nil nil)
;;           until (equal char nil)
;;           do (write-char char out))))

;; (defun read-all-byte-by-byte (stream)
;;   (with-output-to-string (out)
;;     (loop for byte = (read-byte stream nil nil)
;;           until (equal byte nil)
;;           do (write-byte byte out))))


;;; ------------------------------------
;;; Endpoints
;;; ------------------------------------
(defvar *provider* nil)

(defmacro declare-endpoint (method &rest params)
  (labels ((camel-case-to-kebab-case (str)
             (with-output-to-string (out)
               (loop for c across str
                     if (upper-case-p c)
                     do (format out "-~A" c)
                     else
                     do (format out "~A" (char-upcase c)))))
           (geth-method-to-cl-method (geth-method)
             (let* (;; this will deal with the namespace: _ -> /
                    (cl-method (substitute #\/ #\_ geth-method))
                    ;; this will deal with the case: fooBar -> foo-bar
                    (cl-method (camel-case-to-kebab-case cl-method)))
               cl-method)))

    ;; The macro itself is really simple.
    `(defun ,(intern (geth-method-to-cl-method method)) ,(append params '(&key (provider nil provider-p)))
       (let ((p (or (and provider-p provider) *provider*)))
         (handle-response p
                          (make-request p ,method (list ,@params)))))) )

;;; web3
(declare-endpoint "web3_clientVersion")
(declare-endpoint "web3_sha3" data)

;;; net
(declare-endpoint "net_version")
(declare-endpoint "net_listening")
(declare-endpoint "net_peerCount")  ; returns:  QUANTITY - integer of the number of connected peers.

;;; eth
(declare-endpoint "eth_protocolVersion")
(declare-endpoint "eth_syncing")
(declare-endpoint "eth_coinbase")
(declare-endpoint "eth_mining")
(declare-endpoint "eth_hashrate")
(declare-endpoint "eth_gasPrice")
(declare-endpoint "eth_accounts")
(declare-endpoint "eth_blockNumber")
(declare-endpoint "eth_getBalance" address quantity/tag)
(declare-endpoint "eth_getStorageAt" address quantity quantity/tag)
(declare-endpoint "eth_getTransactionCount" address quantity/tag)
(declare-endpoint "eth_getBlockTransactionCountByHash" block-hash)
(declare-endpoint "eth_getBlockTransactionCountByNumber" quantity/tag)
(declare-endpoint "eth_getUncleCountByBlockHash" block-hash)
(declare-endpoint "eth_getUncleCountByBlockNumber" quantity/tag)
(declare-endpoint "eth_getCode" address quantity/tag)
(declare-endpoint "eth_sign" address data)
(declare-endpoint "eth_sendTransaction" transaction-object)
(declare-endpoint "eth_sendRawTransaction" signed-transaction-data)
(declare-endpoint "eth_call" transaction-object quantity/tag)

(declare-endpoint "eth_estimateGas" transaction-block)
(declare-endpoint "eth_getBlockByHash" block-hash full-tx-p)

(declare-endpoint "eth_getBlockByNumber" quantity/tag full-tx-p)

(declare-endpoint "eth_getTransactionByHash" transaction-hash)

(declare-endpoint "eth_getTransactionByBlockHashAndIndex" transaction-hash transaction-index)

(declare-endpoint "eth_getTransactionByBlockNumberAndIndex" quantity/tag transaction-index)










;; usecase
;; set provider first
;; (setf web3:*provider* (make-instance 'web3:HTTPprovider :uri "http://localhost:8545"))
;; (setf web3:*provider* (make-instance 'web3:IPCProvider :uri "/Users/nisen/quicklisp/local-projects/ethi/t/client-data/geth.ipc"))
;; (web3:web3/client-version)
;; (let ((web3:*provider* (make-instance 'web3:HTTPprovider :uri "http://localhost:8545")))
;;   (web3:web3/client-version))

;; (let ((web3:*provider* (make-instance 'web3:IPCProvider :uri "/Users/nisen/quicklisp/local-projects/ethi/t/client-data/geth.ipc")))
;;   (web3:web3/client-version))
