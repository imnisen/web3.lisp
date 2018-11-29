(in-package :web3)

;;; ------------------------------------
;;; Utils
;;; ------------------------------------
(defun response-error (response)
  (cdr (assoc :error response)))

;; Help set fileds to hash table
;; generate code like below, according to required fields or optional fileds
;; (LET (#:G767 (MAKE-HASH-TABLE :TEST 'EQUAL))
;;   (SETF (GETHASH "from" #:G767) FROM)
;;   (SETF (GETHASH "to" #:G767) TO)
;;   (SETF (GETHASH "data" #:G767) DATA)
;;   (WHEN GAS (SETF (GETHASH "gas" #:G767) GAS))
;;   (WHEN GAS-PRICE (SETF (GETHASH "gasPrice" #:G767) GAS-PRICE))
;;   (WHEN VALUE (SETF (GETHASH "value" #:G767) VALUE))
;;   (WHEN NONCE (SETF (GETHASH "nonce" #:G767) NONCE))
;;   #:G767)
;; The reason to use hash table to collect values (for cl-json encode to string)
;; instead list is because:
;; when I want to make some conditon like {"a":1, "b" : [{"c": [1,2,3]}]} very fuzzy:
;; > (cl-json:encode-json-to-string '(("a" . 1) ("b" . ("c" . ( 1 2 3)))))
;; "{\"a\":1,\"b\":[\"c\",1,2,3]}"
(defmacro generate-hash (require-fileds optional-fields)
  (labels ((kebab-case-to-camel-case (str)
             (with-output-to-string (out)
               (loop
                 :with offset := 1
                 :for i := 0 :then (+ i offset)
                 :while (< i (length str))
                 :if (char-equal (char str i) #\-)
                 :do (progn (format out "~a" (char-upcase (char str (+ 1 i)))) (setf offset 2))
                 :else
                 :do (progn (format out "~a" (char-downcase (char str i))) (setf offset 1))))))
    (with-gensyms (g)
      `(let ((,g (make-hash-table :test 'equal)))
         ,@(mapcar #'(lambda (x) `(setf (gethash ,(kebab-case-to-camel-case (string x)) ,g) ,x)) require-fileds)
         ,@(mapcar #'(lambda (x) `(when ,x (setf (gethash ,(kebab-case-to-camel-case (string x)) ,g) ,x))) optional-fields)
         ,g
         ))))

;; Refer https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_sendtransaction parameters description
(defun make-transaction-object (&key from to gas gas-price value data nonce)
  (if (not (and from to data))
      (error "`from`, `to` and `data` are not optional")
      (generate-hash (from to data) (gas gas-price value nonce))))


;; Refer https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_call parameters description
(defun make-transaction-object2 (&key from to gas gas-price value data nonce)
  (if (not to)
      (error "to are not optional")
      (generate-hash (to) (from gas gas-price value data nonce))))

;; Refer https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_estimategas parameters description
(defun make-transaction-object3 (&key from to gas gas-price value data nonce)
  (generate-hash () (from to gas gas-price value data nonce)))


;; Refer https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_newfilter parameters description
(defun make-filter-object (&key from-block to-block address topics)
  (generate-hash () (from-block to-block address topics)))

;; Refer https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_getlogs parameters description
(defun make-filter-object2 (&key from-block to-block address topics blockhash)
  (generate-hash () (from-block to-block address topics blockhash)))


;; Refer https://github.com/ethereum/wiki/wiki/JSON-RPC#shh_newfilter parameters description
(defun make-filter-object3 (&key to topics)
  (if (not topics)
      (error "topics is not optional")
      (generate-hash (topics) (to))))

;; Refer https://github.com/ethereum/wiki/wiki/JSON-RPC#shh_post parameters description
(defun make-whisper-object (&key from to topics payload priority ttl)
  (if (not (and topics payload priority ttl))
      (error "`topics`, `payload`, `priority,  and `ttl` are not optional")
      (generate-hash (topics payload priority ttl) (from to))))

(defvar *provider* nil)

(defmacro defendpoint (method &rest params)
  (labels ((camel-case-to-kebab-case (str)
             (with-output-to-string (out)
               (loop for c across str
                     if (upper-case-p c)
                     do (format out "-~A" c)
                     else
                     do (format out "~A" (char-upcase c)))))
           (geth-method-to-cl-method (geth-method)
             (let* ((cl-method (substitute #\/ #\_ geth-method))
                    (cl-method (camel-case-to-kebab-case cl-method)))
               cl-method)))

    `(defun ,(intern (geth-method-to-cl-method method)) ,(append params '(&key (provider nil provider-p)))
       (let ((p (or (and provider-p provider) *provider*)))
         (handle-response p (make-request p ,method (list ,@params)))))) )

;;; ------------------------------------
;;; Endpoints
;;; ------------------------------------

;; Refer to  https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding,
;; there are two key data types: quantity and unformatted data,
;; quantity is used for integers and numbers,
;; unformatted data is for byte arrays, account addresses, hashes, bytecode arrays
;; they will be denote as `quanity` and `udata` in the follwing endpoints.

;; There is also string type

;; As https://github.com/ethereum/wiki/wiki/JSON-RPC#the-default-block-parameter, there is another
;; type: tag, whose value is one of following: (String "earliest, String "latest", String "pending", HEX String integer)

;; There are some addition types:
;; transaction-object, made from `make-transaction-object`
;; transaction-object2, made from `make-transaction-object2`
;; transaction-object3, made from `make-transaction-object3`
;; filter-object, made from `make-filter-object`
;; filter-object2, made from `make-filter-object2`
;; filter-object3, made from `make-filter-object3`
;; whisper-object, made from `make-whisper-object`

;; The follwing endpoints parameters use ""semantic/type1/type2"" fomart.
;; so `block-address/udata` means this paramter is a block address , whose format is a unformatted data,
;; sometimes, we will add number of bytes behind type, like: udata-20bytes

;;; web3
(defendpoint "web3_clientVersion")
(defendpoint "web3_sha3" data/udata)

;;; net
(defendpoint "net_version")
(defendpoint "net_listening")
(defendpoint "net_peerCount")

;;; eth
(defendpoint "eth_protocolVersion")
(defendpoint "eth_syncing")
(defendpoint "eth_coinbase")
(defendpoint "eth_mining")
(defendpoint "eth_hashrate")
(defendpoint "eth_gasPrice")
(defendpoint "eth_accounts")
(defendpoint "eth_blockNumber")
(defendpoint "eth_getBalance" address/udata-20bytes block/quantity/tag)
(defendpoint "eth_getStorageAt" address/udata-20bytes position/quantity block/quantity/tag)
(defendpoint "eth_getTransactionCount" address/udata-20bytes block/quantity/tag)
(defendpoint "eth_getBlockTransactionCountByHash" blockhash/udata-32bytes)
(defendpoint "eth_getBlockTransactionCountByNumber" block/quantity/tag)
(defendpoint "eth_getUncleCountByBlockHash" blockhash/udata-32bytes)
(defendpoint "eth_getUncleCountByBlockNumber" block/quantity/tag)
(defendpoint "eth_getCode" address/udata-20bytes block/quantity/tag)
(defendpoint "eth_sign" address/udata-20bytes message/udata-nbytes)
(defendpoint "eth_sendTransaction" transaction-object)
(defendpoint "eth_sendRawTransaction" signed-transaction-data/udata)
(defendpoint "eth_call" transaction-object2 block/quantity/tag)
(defendpoint "eth_estimateGas" transaction-object3)
(defendpoint "eth_getBlockByHash" blockhash/udata-32bytes full-tx/boolean)
(defendpoint "eth_getBlockByNumber" block/quantity/tag full-tx/boolean)
(defendpoint "eth_getTransactionByHash" transaction-hash/udata-32bytes)
(defendpoint "eth_getTransactionByBlockHashAndIndex" blockhash/udata-32bytes transaction-index/quantity)
(defendpoint "eth_getTransactionByBlockNumberAndIndex" block/quantity/tag transaction-index/quantity)
(defendpoint "eth_getTransactionReceipt" transaction-hash/udata-32bytes)
(defendpoint "eth_getUncleByBlockHashAndIndex" blockhash/udata-32bytes uncle-index/quantity)
(defendpoint "eth_getUncleByBlockNumberAndIndex" block/quantity/tag uncle-index/quantity)

;; Note, not include some deprecated apis:
;; eth_getCompilers, eth_compileSolidity, eth_compileLLL, eth_compileSerpent

(defendpoint "eth_newFilter" filter-object)
(defendpoint "eth_newBlockFilter")
(defendpoint "eth_newPendingTransactionFilter")
(defendpoint "eth_uninstallFilter" filterid/quantity)
(defendpoint "eth_getFilterChanges" filterid/quantity)
(defendpoint "eth_getFilterLogs" filterid/quantity)
(defendpoint "eth_getLogs" filter-object2)
(defendpoint "eth_getWork")
(defendpoint "eth_submitWork" nonce/udata-8bytes header-pow-hash/udata-32bytes mix-digest/udata-32bytes)
(defendpoint "eth_submitHashrate" hashrate/udata-32bytes id/udata-32bytes)
(defendpoint "eth_getProof" address/udata-20bytes keys/array-32bytes block/quantity/tag)

;; Deprecated
;; db
;; (defendpoint "db_putString" dbname/string keyname/string string-to-store/string)
;; (defendpoint "db_getString" dbname/string keyname/string)
;; (defendpoint "db_putHex"  dbname/string keyname/string data-to-sore/udata)
;; (defendpoint "db_getHex"  dbname/string keyname/string)

;; shh
(defendpoint "shh_version")
(defendpoint "shh_post" whisper-object)
(defendpoint "shh_newIdentity")
(defendpoint "shh_hasIdentity" identity-address/udata-60bytes)
(defendpoint "shh_newGroup")
(defendpoint "shh_addTogroup" )
(defendpoint "shh_newFilter" filter-object3)
(defendpoint "shh_uninstallFilter" filterid/quantity)
(defendpoint "shh_getFilterChanges" filterid/quantity)
(defendpoint "shh_getMessages" filterid/quantity)
