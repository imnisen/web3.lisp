(in-package :web3)


;;; ------------------------------------
;;; Provider
;;; ------------------------------------

;;; Base provider
(defclass BaseProvider ()
  ((uri :initarg :uri
        :accessor provider-uri)))

(defgeneric make-request (provider method params)
  (:documentation ""))

(defgeneric handle-response (provider response)
  (:documentation ""))

(defgeneric construct-body (provider method params)

  (:documentation ""))

(defgeneric destructure-response (provider response)
  (:documentation ""))


(defclass JSONBaseProvider (BaseProvider)
  ())

(defmethod construct-body ((provider JSONBaseProvider) method params)
  (cl-json:encode-json-to-string `(("jsonrpc" . "2.0")
                                   ("method" . ,method)
                                   ("params" . ,params)
                                   ("id" . 1))))  ;; assume id 1 here, but will be moved to JSONbaseprovider and set auto

(defmethod destructure-response ((provider JSONBaseProvider) (response string))
  (cl-json:decode-json-from-string response))

(defmethod destructure-response ((provider JSONBaseProvider) (response vector)) ;; todo unsigned-byte
  (cl-json:decode-json-from-string (flexi-streams:octets-to-string response)))
