(in-package :qooxlisp)

(defun serve-apropos (&optional (port 8000))
  (trcx :serve-apropos port)
  (when *wserver* (shutdown))
  (qx-reset)
  (net.aserve:start :debug nil :port port)
  (net.aserve::debug-off :all)
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
    
    (pdr "/qx/" "/qooxdoo-5.0.2-sdk/")
    (pdr "/qooxdoo-5.0.2-sdk/" "/qooxdoo-5.0.2-sdk/")
    (pfn "/begin" 'qx-begin) ;; <=== qx-begin (below) gets customized
    (pfn "/callback" 'qx-callback-js)
    (pfn "/cbjson" 'qx-callback-json)

    (let* ((src-build "build")
           (app-root "/devel/qooxlisp/ide") ;; <=== just change this
           (app-source (format nil "~a/~a/" app-root src-build)))
      (flet ((src-ext (x)
               (format nil "~a~a" app-source x)))
        (trcx :index-html!!! (src-ext "index.html"))
        (pfl "/" (src-ext "index.html"))
        (pdr (format nil "/~a/" src-build) app-source)
        (pdr "/script/" (src-ext "script/"))
        (pdr "/resource/" (src-ext "resource/")) ;;>>> move this to qxl-session and figure out how to combine
        (format t "~&Now serving port ~a." port)))))

(defun qx-begin (req ent)
  (trcx :begin!!!!!!!!!!!!!)
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
                          (make-instance 'apropos-sampler))))))))))

(defparameter apropos-variants
  '((apropos-classic "Classic")
    (apropos-makeover "Makeover")
    (apropos-ala-kenny "a la Kenny")))

(defmd apropos-sampler (qxl-session)
  :kids (c? (the-kids
             (hbox (:spacing 6)(:add '(:left 0 :top 0 :width "100%" :height "100%") :padding 6)
               (groupbox (:spacing 10)(:legend "Variant")
                 (radiobuttongroup :ux-variant (:value (c-in 'apropos-classic))
                   (qx-vbox :spacing 6)
                   (loop for av in apropos-variants
                       collecting (radiobutton (car av) (cadr av)))))
               (vbox ()()
                 (make-kid (value (fm^ :ux-variant))))))))

(defmd apropos-variant (qxl-column) ;; abstract class
  (sym-seg (c-in nil))
  (syms-unfiltered (c? (b-when seg (^sym-seg)
                         (trcx :calcing-symunfiltered! seg)
                         (symbol-info-raw seg :pkg (value (fm-other :selected-pkg))))))
  (syms-filtered (c? (symbol-info-filtered (^syms-unfiltered)
                       (value (fm-other :type-filter))
                       (value (fm-other :exported-only)))))
  (sym-sort-spec (c-in nil))
  (sym-info (c? (b-if sort (^sym-sort-spec)
                  (destructuring-bind (sort-key order) sort
                    (sort (copy-list (^syms-filtered))
                      (if (equal order "asc")
                          'string-lessp 'string-greaterp)
                      :key (if (equal sort-key "pkg")
                               (lambda (si) (package-name (symbol-info-pkg si)))
                             (qxl-sym (conc$ 'symbol-info- sort-key)))))
                  (^syms-filtered)))))

(defobserver sym-info ()
  (trcx :sym-info-observer-fires)
  (with-integrity (:client `(:post-make-qx ,self))
    (trcx :sym-info-observer-runs (fm-other :sym-info-table) (oid (table-model (fm-other :sym-info-table))))
    (let ((tbl (fm-other :sym-info-table)))
      (assert tbl)
      (b-when oid (oid (table-model tbl))
        (trcx :reloading!!!!)
        (qxfmt "clDict[~a].reloadData();" oid)))))