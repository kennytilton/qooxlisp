(in-package :apropos-qx)


(defun serve-apropos ()
  (let ((port 8000))
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
  
(defun qx-begin (req ent)
  (ukt::stop-check :qx-begin)
  (trace md-awaken)
  (with-qx-js-response (req ent)
    (print :beginning-request)
    (with-integrity ()
      (qxfmt "
sessId=~a;
console.log('sessid='+sessId);" (session-id (make-instance 'apropos-session) )))))

(defmd apropos-session (qxl-session)
  (sym-seg (c-in nil))
  (syms-unfiltered (c? (b-when seg (^sym-seg)
                         (symbol-info-raw seg))))
  (sym-info (c? (symbol-info-filtered (^syms-unfiltered)
                  (value (fm-other :type-filter))
                  (value (fm-other :exported-only))
                  (value (fm-other :all-packages))
                  (value (fm-other :selected-pkg)))))
  :kids (c? (the-kids
             (vbox (:spacing 6) 
               (:add '(:left 0 :top 0 :width "100%" :height "100%")
                 :padding 6)
               (search-panel self)
               (hbox (:spacing 6)()
                 (exported-pkg-filter self)
                 (type-filter self))
               (symbols-found self)))))

(defobserver sym-info ()
  (with-integrity (:client `(:post-make-qx ,self))
    (let ((tbl (fm-other :sym-info-table)))
      (assert tbl)
      (when (oid (table-model tbl))
        (qxfmt "clDict[~a].reloadData();" (oid (table-model tbl)))))))


