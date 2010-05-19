(in-package :cqx)

(defun k-word (s)
  (when s (if (consp s) (mapcar 'k-word s)
            (intern (string-upcase s) :keyword))))

(defmacro whtml (&body body)
  `(catch 'excl::printer-error
     (net.html.generator:html ,@body)))

(defun req-val (req tag)
  (net.aserve:request-query-value tag req))

(defmacro with-plain-text-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "text/plain")
       (net.aserve:with-http-body (,req ,ent)
         (let* ((ws (net.aserve:websession-from-req ,req)))
           (declare (ignorable ws ns))
           ,@body)))))

(defmacro with-html-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "text/html")
       (net.aserve:with-http-body (,req ,ent)
         (let ((ws (net.aserve:websession-from-req ,req)))
           (declare (ignorable ws))
           ,@body)))))

(defmacro with-js-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "text/javascript")
       (net.aserve:with-http-body (,req ,ent)
         (let ((ws (net.aserve:websession-from-req ,req)))
           (declare (ignorable ws))
           ,@body)))))

(defmacro with-json-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "application/json")
       (net.aserve:with-http-body (,req ,ent)
         (let ((ws (net.aserve:websession-from-req ,req)))
           (declare (ignorable ws))
           ,@body)))))

(defmacro ml$ (&rest x)
  (let ((s (gensym)))
    `(with-output-to-string (,s)
       (net.html.generator:html-stream ,s
         ,@x))))

(defun js-prep (&rest lists)
  (format nil "(~{~a~})"
    (loop for list in lists
        collecting (format nil "(~{~(~a~)~^ {~a}~})" list))))

(defun json$ (x) (json:encode-json-to-string x))

#+test
(json$ (list (cons :one 1)))
