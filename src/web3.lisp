(in-package :web3)

;;; ------------------------------------
;;; Utils
;;; ------------------------------------

(defun response-error (response)
  (cdr (assoc :error response)))

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


(declare-endpoint "web3_clientVersion")
(declare-endpoint "web3_sha3" data)

(declare-endpoint "net_version")







;; usecase
;; set provider first
;; (setf web3:*provider* (make-instance 'web3:HTTPprovider :uri "http://localhost:8545"))
;; (setf web3:*provider* (make-instance 'web3:IPCProvider :uri "/Users/nisen/quicklisp/local-projects/ethi/t/client-data/geth.ipc"))
;; (web3:web3/client-version)
;; (let ((web3:*provider* (make-instance 'web3:HTTPprovider :uri "http://localhost:8545")))
;;   (web3:web3/client-version))

;; (let ((web3:*provider* (make-instance 'web3:IPCProvider :uri "/Users/nisen/quicklisp/local-projects/ethi/t/client-data/geth.ipc")))
;;   (web3:web3/client-version))
