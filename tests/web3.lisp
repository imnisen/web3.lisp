(defpackage web3-test
  (:use :cl
        :web3
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

(defun transaction-hash-p (str)
  (hex-and-of-length str 64))


(defmacro define-test-pack (test-fn test-form)
  ;; Http provider test
  `(subtest ,(format nil "~%===Test pack: ~a===~%" test-fn)
     (let ((*test-result* (let ((*provider* (make-instance 'HTTPprovider :uri *http-uri*)))
                            ,test-fn)))
       ,test-form)

     (let ((*test-result* ,(append test-fn '(:provider (make-instance 'HTTPprovider :uri *http-uri*)))))
       ,test-form)

     (let ((*test-result* (let ((*provider* (make-instance 'IPCProvider :uri *ipc-uri*)))
                            ,test-fn)))
       ,test-form)

     (let ((*test-result* ,(append test-fn '(:provider (make-instance 'IPCProvider :uri *ipc-uri*)))))
       ,test-form)

     ))



;; tests
(defvar *http-uri* "http://localhost:8545")

(defvar *ipc-uri* (namestring (merge-pathnames "tests/client-data/geth.ipc")))  ;; TOCHECK
;; (defvar *ipc-uri* "/Users/nisen/quicklisp/local-projects/web3.lisp/tests/client-data/geth.ipc")
(defvar *test-result* nil)


(define-test-pack (web3/client-version)
    (ok (starts-with-p *test-result* "Geth")
        "Can give a client version back"))

(define-test-pack (web3/sha3 "0x68656c6c6f20776f726c64")
    (is *test-result*
        "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
        "Can give a sha3 of data"))

(define-test-pack (net/version)
    (is *test-result*
        "555" ; check out the t/genesis.json file where it is specified
        "Can give the net version"))






(plan nil)

;; blah blah blah.

(finalize)
