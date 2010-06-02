(in-package :qooxlisp)


(defun serve-apropos ()
  (let ((port 8000))
    (trace qx-begin qx-getdatacount qx-getdata qx-callback qx-sortdata)
    (when *wserver* (shutdown))
    (qx-reset)
    (net.aserve:start :debug t :port port)
    (flet ((pfl (p f)
             (publish-file :port port
               :path p
               :file f))
           (pdr (p d)
             (publish-directory :port port
               :prefix p
               :destination d))
           (pfn (p fn)
             (publish :path p :function fn)))
      
      (pdr "/qx/" "/devel/qx/")
      (pfn "/begin" 'qx-begin)
      (pfn "/callback" 'qx-callback)
      (pfn "/getdata" 'qx-getdata)
      (pfn "/sortdata" 'qx-sortdata)
      (pfn "/getdatacount" 'qx-getdatacount)
      
      (let* ((app-root "/devel/qooxlisp/ide") ;; <=== just change this
             (app-source (format nil "~a/source/" app-root)))
        (flet ((src-ext (x)
                 (format nil "~a~a" app-source x)))
          (pfl "/" (src-ext "index.html"))
          (pdr "/source/" app-source)
          (pdr "/script/" (src-ext "script/")))))))
  
(defvar qdoc)


(defun qx-begin (req ent)
  (ukt::stop-check :qx-begin)
  
  (with-js-response (req ent)
    (print :beginning-request)
    (with-integrity ()
      (let ((session
             (setf *qxdoc*
               #+notthis
               (make-instance 'apropos-session ;; ACL version
                 :theme "qx.theme.Classic")
               (make-instance
                   'apropos-session-makeover ;; kenny's makeover, step one
                 :theme "qx.theme.Modern")
               #+notthis
               (make-instance
                   'apropos-session-kt ;; kenny's makeover, step one
                 :theme "qx.theme.Modern"))))
        (qxfmt "
clDict[0] = qx.core.Init.getApplication().getRoot();
sessId=~a;" (session-id session))))))

