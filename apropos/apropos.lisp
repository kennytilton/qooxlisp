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
  (let ((*ekojs* t)) ;; qx-begin
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
  '((av-classic "Classic")
    (av-classic-plus "Classic++")
    (apropos-ala-kenny "a la Kenny")))

(defmd apropos-sampler (qxl-session)
  :kids (c? (the-kids
             (hbox (:spacing 6)(:add '(:left 0 :top 0 :width "100%" :height "100%") :padding 6)
               (groupbox (:spacing 10)(:legend "Variant")
                 (radiobuttongroup :ux-variant (:value (c-in 'av-classic-plus))
                   (qx-vbox :spacing 6)
                   (loop for av in apropos-variants
                       collecting (radiobutton (car av) (cadr av)))))
               (vbox ()(:background-color 'yellow :add `(:flex 1))
                 (make-kid (value (fm^ :ux-variant))))))))