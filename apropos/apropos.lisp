(in-package :qooxlisp)

(defun serve-apropos (&optional (port 8000))
  (shutdown-backend *backend*)
  (qx-reset)
  (start-backend *backend* :port port)
  (backend-debug-off *backend*)
  (flet ((pfl (p f)
           (backend-publish-file *backend*
             :port port
             :path p
             :file f))
         (pdr (p d)
           (backend-publish-directory *backend*
             :port port
             :prefix p
             :destination d))

         (pfn (p fn)
           (backend-publish-function *backend*
            :path p :function fn)))
    
    (pdr "/qx/" "/devel/qx/")
    (pfn "/begin" 'qx-begin) ;; <=== qx-begin (below) gets customized
    (pfn "/callback" 'qx-callback-js)
    (pfn "/cbjson" 'qx-callback-json)

    (let* ((src-build "build")
           (app-root "/devel/qooxlisp/ide") ;; <=== just change this
           (app-source (format nil "~a/~a/" app-root src-build)))
      (flet ((src-ext (x)
               (format nil "~a~a" app-source x)))
        (pfl "/" (src-ext "index.html"))
        (pdr (format nil "/~a/" src-build) app-source)
        (pdr "/script/" (src-ext "script/"))
        (pdr "/resource/" (src-ext "resource/")) ;;>>> move this to qxl-session and figure out how to combine
        (format t "~&Now serving port ~a." port)))))

(defun qx-begin (req ent)
  (ukt::stop-check :qx-begin)
  ;(trace md-awaken make-qx-instance)
  (let ((*ekojs* nil)) ;; qx-begin
    (with-js-response (req ent)
      (top-level.debug::with-auto-zoom-and-exit ("aabegin-zoo.txt" :exit nil)
        (let ((*web-session* nil))
          (with-integrity ()
            (qxfmt "
function cbjs (oid,opcode,data) {
	var req = new qx.io.remote.Request('/callback','GET', 'text/javascript');
	req.setParameter('sessId', sessId);
	req.setParameter('oid', oid);
	req.setParameter('opcode', opcode);
	req.setParameter('data', data);
	req.send();
}
clDict[0] = qx.core.Init.getApplication().getRoot();
sessId=~a;" (session-id (setf *web-session*
                          (make-instance 'apropos-session-kt))))))))))


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
                  (trcx :sym-info-fires (length si))
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
  (trcx :sym-info-observer-fires)
  (with-integrity (:client `(:post-make-qx ,self))
    (trcx :sym-info-observer-runs (fm-other :sym-info-table) (oid (table-model (fm-other :sym-info-table))))
    (let ((tbl (fm-other :sym-info-table)))
      (assert tbl)
      (b-when oid (oid (table-model tbl))
        (qxfmt "clDict[~a].reloadData();" oid)))))