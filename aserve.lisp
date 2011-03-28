(in-package :qxl)

;;;; Backend for allegroserve

(defclass aserve-backend (qooxlisp-server-backend)
  ())

(defmethod shutdown-backend ((backend aserve-backend))
  (when *wserver* 
     (shutdown)))

(defmethod backend-debug-off ((backend aserve-backend))
  (net.aserve::debug-off :all))

(defmethod start-backend ((backend aserve-backend) &key port)
  (net.aserve:start :debug nil :port port))

(defmethod backend-publish-file ((backend aserve-backend)
                                 &key port path file)
  (publish-file :port port
                :path path
                :file file))

(defmethod backend-publish-directory ((backend aserve-backend)
                                      &key port prefix destination)
  (publish-directory :port port
                     :prefix prefix
                     :destination destination))

(defmethod backend-publish-function ((backend aserve-backend)
                                     &key path function)
  ;; Function takes two args, request and ent
  (publish :path path
           :function function))

(defmethod render-unescaped-response ((backend aserve-backend) string)
  (catch 'excl::printer-error
     (net.html.generator:html (:princ string))))

(defmethod backend-js-response ((backend aserve-backend) req ent function)
  (net.aserve:with-http-response (req ent :content-type "text/javascript")
    (net.aserve:with-http-body (req ent)
      (render-unescaped-response backend (funcall function))))
  nil)

(defmethod backend-json-response ((backend aserve-backend) req ent function)
  (net.aserve:with-http-response (req ent :content-type "application/json")
    (net.aserve:with-http-body (req ent)
      (funcall function)))
  nil)

(defmethod backend-request-value ((backend aserve-backend) request tag)
  (net.aserve:request-query-value tag request))

(defmethod backend-get-raw-request ((backend aserve-backend) request)
  (request-raw-request request))

(unless *default-backend-classname*
  (setf *default-backend-classname* 'aserve-backend)
  (initialize-backend))
