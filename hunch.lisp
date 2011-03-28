(in-package :qxl)

(defclass hunchentoot-backend (qooxlisp-server-backend)
  ((acceptor :accessor acceptor :initform nil)))

(defvar *default-hunchentoot-dispatch-table* hunchentoot:*dispatch-table*)

(defmethod shutdown-backend ((backend hunchentoot-backend))
  (let ((ac (acceptor backend)))
    (when ac
      (hunchentoot:stop ac)
      (setf hunchentoot:*dispatch-table* *default-hunchentoot-dispatch-table*))))

(defun add-dispatcher (dispatcher)
  (push dispatcher hunchentoot:*dispatch-table*))

(defmethod backend-debug-off ((backend hunchentoot-backend)))

(defmethod start-backend ((backend hunchentoot-backend) &key port)
  (let ((ac (make-instance 'hunchentoot:acceptor :port port)))
    (hunchentoot:start ac)
    (setf (acceptor backend) ac)))

(defmethod backend-publish-file ((backend hunchentoot-backend)
                                 &key port path file)
  (declare (ignore port)) ;; We expect that we only use one port
  (add-dispatcher
   (hunchentoot:create-static-file-dispatcher-and-handler path file)))

(defmethod backend-publish-directory ((backend hunchentoot-backend)
                                      &key port prefix destination)
  (declare (ignore port)) ;; We expect that we only use one port
  (add-dispatcher
   (hunchentoot:create-folder-dispatcher-and-handler prefix destination)))

;;; Allegroserve code often returns nil when hunchentoot excepts string data,
;;; for safety we save strings during processing of callback functions.
(defvar *response-strings*)

(defmethod backend-publish-function ((backend hunchentoot-backend)
                                     &key path function)
  (add-dispatcher
   (hunchentoot:create-prefix-dispatcher
    path
    (lambda ()
      (let ((*response-strings* nil))
        (funcall function hunchentoot:*request* 'ent-unused)
        (if (= 1 (length *response-strings*))
            (first *response-strings*) ; this is probably almost always the case
            (apply #'concatenate 'string (nreverse *response-strings*))))))))

(defmethod backend-get-raw-request ((backend hunchentoot-backend) request)
  (hunchentoot:raw-post-data :request request :force-text t))

(defmethod render-unescaped-response ((backend hunchentoot-backend) string)
  (push string *response-strings*)
  string)

(defmethod backend-js-response ((backend hunchentoot-backend) req ent function)
  (declare (ignore req ent))
  (setf (hunchentoot:content-type*) "text/javascript")
  (render-unescaped-response backend (funcall function)))

(defmethod backend-json-response ((backend hunchentoot-backend) req ent function)
  (declare (ignore req ent))  
  (setf (hunchentoot:content-type*) "application/json")
  (render-unescaped-response backend (funcall function)))

(defmethod backend-request-value ((backend hunchentoot-backend) request tag)
  (hunchentoot:get-parameter tag request))

(unless *default-backend-classname*
  (setf *default-backend-classname* 'hunchentoot-backend)
  (initialize-backend))
