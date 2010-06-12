(in-package :qxl)


(defmd qx-object ()
  (oid nil :cell nil)
  constructor-args)

(defun session-focus (req ent)
  (with-js-response (req ent)
    (with-integrity ()
      (b-when session (b-if sessId (parse-integer (req-val req "sessId") :junk-allowed t)
                        (or (gethash sessId *qx-sessions*)
                          (dwarn "session-focus: Unknown session ID ~a in ~s" sessId (request-raw-request req)))
                        (dwarn "session-focus: No sessId parameter: ~s" (request-raw-request req)))
        (b-when new-focus (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                          (or (gethash oid (dictionary session))
                            (dwarn "session-focus: oid ~s not in dictionary" oid))
                          (dwarn "session-focus: No oid parameter: ~s" (request-raw-request req)))
          (setf (focus session) new-focus))))))



(defmethod initialize-instance :after ((self qx-object) &key oid fm-parent)
  (unless (typep self 'qxl-session)
    (assert (or oid fm-parent) () "No fm-parent at i-i for ~a" self)))

(defmethod md-awaken :before ((self qx-object))
  (unless (oid self)
    (let ((s (session self)))
      (assert s () "No session for ~a, par ~a, usess ~a" self (fm-parent self) (u^ qxl-session))
      (setf (oid self) (get-next-oid s))
      (setf (gethash (oid self) (dictionary s)) self)))
  (make-qx-instance self))

(defmethod make-qx-instance ((self qx-object))
  (with-integrity (:client `(:make-qx ,self))
    (when (qx-class self)
      (qxfmt "clDict[~a] = new ~a(~{~a~^,~});" 
        (oid self) (qx-class self)
        (constructor-args self))
      (b-when cfg (qx-configurations self)
        (qxfmt "clDict[~a].set(~a);" (oid self)(json$ cfg))))))
          
(defgeneric qx-configurations (self)
  (:method-combination append)
  (:method append (self)
    (declare (ignore self))))

(defun add-ops (self &aux (add (add self)))
  (typecase add
    (string add)
    (cons (apply 'jsk$ add))))

(defmd qx-widget (qx-layout-item)
  decorator
  background-color
  onkeypress
  (enabled t))

(defmethod make-qx-instance :after ((self qx-widget))
  (qxfmt "
clDict[~a].addListener('focus', function (e) {
    console.log('focus cb this is '+this+' oid '+~:*~d);
    this.setBackgroundColor('blue');
    var rq = new qx.io.remote.Request('/focusOn?sessId='+sessId+'&oid=~:*~a'
                                      ,'GET'
                                      , 'text/javascript');
    rq.send();
    });" (oid self)))

(defobserver enabled ()
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].setEnabled(~a);" (oid self) (if new-value "true" "false"))))

(defobserver decorator () ;; this one is not known to work yet
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setDecorator(new qx.ui.decoration.~a);" (oid self)(decorator self)))))
       
(defmethod qx-configurations append ((self qx-widget))
  (nconc
   (b-when x (background-color self)
     (list (cons :background-color x)
       ))))

(defmd qooxlisp-control () ;; qooxlisp- indicates this is a Lisp-side only class
  onexecute)

(defobserver onexecute ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('execute', function(e) {
    //consolelog('executing ~:*~a');
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onexecute&oid=~:*~a','GET', 'text/javascript');
    rq.send();
});" 
                  (oid self))))))

(defobserver onkeypress ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('keypress', function(e) {
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onkeypress&oid=~:*~a&keyId='+e.getKeyIdentifier()+'&value='+this.getValue(),'GET', 'text/javascript');
    rq.send();
});" 
                  (oid self)))
     #+chill-youneedtobespecific
     (old-value
      ;;untested
      (qxfmt "clDict[~a].removeListener('keypress');" (oid self))))))

(defmd qooxlisp-layouter (qx-widget qooxlisp-family)
  (layout nil :owning t))

(defobserver layout ()
  (when new-value
    (with-integrity (:client `(:layout ,self))
      (qxfmt "clDict[~a].setLayout(clDict[~a]);~%" (oid self)(oid (layout self))))))

(defmd qx-composite (qooxlisp-layouter)
  (qx-class "qx.ui.container.Composite" :allocation :class :cell nil))

(export! onappear)

(defmd qx-html (qx-widget)
  (qx-class "qx.ui.embed.Html" :allocation :class :cell nil)
  html
  onappear)

(defobserver html ()
  #+badidea
  (with-integrity (:client `(:post-make-qx ,self))
    #+nodice
    (qxfmt "clDict[~a].set(~a);"
      (oid self)
      (json$ (list :html
               "<div style='font-size:18pt'>Problem #1: <span class='math'>\\sqrt{x^2-4ac}</span></div>\");")))
    (qxfmt "var ce=clDict[~a].getContentElement();
var de=ce.getDomElement();
de.innerHtml=\"<div style='font-size:18pt'>Problem #1: <span class='math'>\\\\sqrt{x^2-4ac}</span></div>\";"
      (oid self))
    #+firefoxonly
    (qxfmt "clDict[~a].setHtml(\"<div style='font-size:18pt'>Problem #1: <span class='math'>\\\\sqrt{x^2-4ac}</span></div>\");
jsMath.ProcessBeforeShowing();"
      (oid self))
    #+not 
    (if new-value
        (qxfmtd "clDict[~a].setHtml('~s');" (oid self) new-value)
      (qxfmt "clDict[~a].setHtml(\"\");" (oid self)))
    (mprt :post-html qxl::*js-response*)))

#+test
(json$ (list :html "<span>\\frac{2}{3}</span>"))

;;;(defobserver html ()
;;;  (with-integrity (:client `(:post-make-qx ,self))
;;;    (qxfmt "clDict[~a].setHtml(~a);
;;;/*
;;;var ce = clDict[~a].getContentElement();
;;;this.debug('Html qx content ' + ce);
;;;var de = ce.getDomElement();
;;;this.debug('Html dom elt ' + de);
;;;jsMath.ProcessBeforeShowing(de);
;;;*/
;;;" (oid self)(or new-value "null")(oid self) (oid self))))
  
