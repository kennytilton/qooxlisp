(in-package :qooxlisp)


(defun serve-apropos (&optional (port 8000))
  
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
    (pfn "/begin" 'qx-begin) ;; <=== qx-begin (below) gets customized
    (pfn "/callback" 'qx-callback-js)
    (pfn "/cbjson" 'qx-callback-json)
    
    (let* ((src-build "build")
           (app-root "/devel/qooxlisp/ide") ;; <=== change this to point to your qooxdoo app
           (app-source (format nil "~a/~a/" app-root src-build)))
      (flet ((src-ext (x)
               (format nil "~a~a" app-source x)))
        (pfl "/" (src-ext "index.html"))
        (pdr (format nil "/~a/" src-build) app-source)
        (pdr "/script/" (src-ext "script/"))
        (pdr "/resource/" (src-ext "resource/"))
        (format t "~&Now serving apropos on port ~a, index ~a" port (src-ext "index.html"))))))

(defun qx-begin (req ent)
  (ukt::stop-check :qx-begin)
  ;(trace md-awaken make-qx-instance)
  (with-js-response (req ent)
    (print :beginning-request)
    (with-integrity ()
      (qxfmt "console.log('starting...');")
      (qxfmt "
clDict[0] = qx.core.Init.getApplication().getRoot();
sessId=~a;" (session-id  
             ;; this is awkward: it might seem like there is no
             ;; point in assigning to *qxdoc*, but with-integrity 
             ;; runs its form then /with *qxdoc* set/ finishes up 
             ;; the deferred queue where a response gets built.
             ;;
             (setf *qxdoc*
               #+notthis(make-instance 'apropos-session-classic ;; ACL version
                 :theme #+xxxxxx "qx.theme.Modern" "qx.theme.Classic")
               #+notthis  (make-instance
                             'apropos-session-makeover ;; kenny's makeover, step one
                           :theme "qx.theme.Modern")
               (make-instance
                   'apropos-session-kt ;; kenny's makeover, step two
                 :theme "qx.theme.Modern")))))))


(defmd apropos-session (qxl-session) ;; abstract class
  (sym-seg (c-in nil))
  (syms-unfiltered (c? (b-when seg (^sym-seg)
                         (symbol-info-raw seg))))
  selected-pkg-p ;; supplied by subclasses
  (syms-filtered (c? (symbol-info-filtered (^syms-unfiltered)
                       (value (fm-other :type-filter))
                       (value (fm-other :exported-only))
                       (^selected-pkg-p)
                       (value (fm-other :selected-pkg)))))
  (sym-sort-spec (c-in nil))
  (sym-info (c? (let ((si (^syms-filtered)))
                  (mprt :sym-info-fires (length si))
                  (b-if sort (^sym-sort-spec)
                    (destructuring-bind (sort-key order) sort
                      (sort (copy-list si)
                        (if (equal order "asc")
                            'string-lessp 'string-greaterp)
                        :key (if (equal sort-key "pkg")
                                 (lambda (si) (package-name (symbol-info-pkg si)))
                               (qxl-sym (conc$ 'symbol-info- sort-key)))))
                    si)))))

(defobserver sym-info ()
  (mprt :sym-info-observer-fires)
  (with-integrity (:client `(:post-make-qx ,self))
    (mprt :sym-info-observer-runs (fm-other :sym-info-table) (oid (table-model (fm-other :sym-info-table))))
    (let ((tbl (fm-other :sym-info-table)))
      (assert tbl)
      (b-when oid (oid (table-model tbl))
        (qxfmt "clDict[~a].reloadData();" oid)))))