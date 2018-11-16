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
         (handle-response p
                          (make-request p ,method (list ,@params)))))) )

;;; ------------------------------------
;;; Endpoints
;;; ------------------------------------

;;; web3
(defendpoint "web3_clientVersion")
(defendpoint "web3_sha3" data)

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
(defendpoint "eth_getBalance" address quantity/tag)
(defendpoint "eth_getStorageAt" address quantity quantity/tag)
(defendpoint "eth_getTransactionCount" address quantity/tag)
(defendpoint "eth_getBlockTransactionCountByHash" block-hash)
(defendpoint "eth_getBlockTransactionCountByNumber" quantity/tag)
(defendpoint "eth_getUncleCountByBlockHash" block-hash)
(defendpoint "eth_getUncleCountByBlockNumber" quantity/tag)
(defendpoint "eth_getCode" address quantity/tag)
(defendpoint "eth_sign" address data)
(defendpoint "eth_sendTransaction" transaction-object)
(defendpoint "eth_sendRawTransaction" signed-transaction-data)
(defendpoint "eth_call" transaction-object quantity/tag)
(defendpoint "eth_estimateGas" transaction-block)
(defendpoint "eth_getBlockByHash" block-hash full-tx-p)
(defendpoint "eth_getBlockByNumber" quantity/tag full-tx-p)
(defendpoint "eth_getTransactionByHash" transaction-hash)
(defendpoint "eth_getTransactionByBlockHashAndIndex" transaction-hash transaction-index)
(defendpoint "eth_getTransactionByBlockNumberAndIndex" quantity/tag transaction-index)
(defendpoint "eth_getTransactionReceipt" transaction-hash)
;; -> Here start use "semantic/types*" style of params
(defendpoint "eth_getUncleByBlockHashAndIndex" block-hash uncle-index/quanity)
(defendpoint "eth_getUncleByBlockNumberAndIndex" block-number/quanity/tag uncle-index/quanity)
;;; Note, not include some deprecated apis
(defendpoint "eth_newFilter" filter-options/filter-object)
;; -> Here start use new def style, need to modify defendpoint latter
(defendpoint "eth_newBlockFilter")
(defendpoint "eth_newPendingTransactionFilter")
(defendpoint "eth_uninstallFilter" filter-id/quantity)
(defendpoint "eth_getFilterChanges" filter-id/quantity)
(defendpoint "eth_getFilterLogs" filter-id/quantity)
(defendpoint "eth_getLogs" filter-options/filter-object)
(defendpoint "eth_getWork")
(defendpoint "eth_submitWork" nonce/hexstring-8bytes header-pow-hash/hexstring-32bytes mix-digest/hexstring-32bytes)
(defendpoint "eth_submitHashrate" hashrate/hexstring-32bytes id/hexstring-32bytes)

;; db
(defendpoint "db_putString" database-name/string key-name/string string-to-store/string)
(defendpoint "db_getString" database-name/string key-name/string)
(defendpoint "db_putHex"  database-name/string key-name/string data-to-sore/hexstring)
(defendpoint "db_getHex"  database-name/string key-name/string )

;; ssh
(defendpoint "ssh_version")
(defendpoint "ssh_post" whisper-object/whisper-object)
(defendpoint "ssh_newIdentity" )
(defendpoint "ssh_hasIndentity" identity-address/hexstring-60bytes)
(defendpoint "ssh_newGroup")
(defendpoint "ssh_addTogroup" )
(defendpoint "ssh_newFilter" filter-options/filter-object2)
(defendpoint "ssh_uninstallFilter" filter-id/quantity)
(defendpoint "shh_getFilterChanges" filter-id/quantity)
(defendpoint "shh_getMessages" filter-id/quantity)
