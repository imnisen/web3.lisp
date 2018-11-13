(in-package :web3)


;;; Http Provider
(defclass HTTPProvider (JSONBaseProvider)
  ())

(defmethod make-request ((provider HTTPProvider) method params)
  ;; (format t "~%request by http, params:~a ~%" params)

  (let ((raw-body (construct-body provider method params)))

    (drakma:http-request (provider-uri provider)
                         :method :post
                         :content-type "application/json"
                         :content raw-body)))

(defmethod handle-response ((provider HTTPProvider) response)
  (let ((decoded-response (destructure-response provider response)))
    ;; check for errors
    (if (response-error decoded-response)
        (error (response-error decoded-response))
        ;; ignore the rest of the response and return the result
        (cdr (assoc :result decoded-response)))))
