(in-package :web3)


(defclass IPCProvider (JSONBaseProvider)
  ())

(defmethod make-request ((provider IPCProvider) method params)
  ;; (format t "~%request by ipc~%")
  (iolib:with-open-socket (socket :address-family :local
                                  :remote-filename (provider-uri provider))
    (iolib:send-to socket (flexi-streams:string-to-octets
                           (construct-body provider method params)))
    (read-line socket))
  )

(defmethod handle-response  ((provider IPCProvider) response)
  (let ((decoded-response (destructure-response provider response)))
    ;; check for errors
    (if (response-error decoded-response)
        (error (response-error decoded-response))
        ;; ignore the rest of the response and return the result
        (cdr (assoc :result decoded-response)))))
