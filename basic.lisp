;; -*- mode: Lisp; Syntax: Common-Lisp; Package: qooxlisp; -*-
#|

    basic -- core widget support

(See package.lisp for license and copyright notigification)

|#

(in-package :qxl)

(defmd qx-object ()
  (oid nil :cell nil)
  constructor-args)

(defmethod print-object ((self qx-object) s)
  (format s "~a[~a]" (md-name self)(slot-value self 'oid)))

#-its-alive!
(defmethod initialize-instance :after ((self qx-object) &key oid fm-parent)
  (unless (or (typep self 'qxl-session)
            (typep self 'qx-table-model-abstract))
    (assert (or oid fm-parent) () "No fm-parent at i-i for ~a" self)))

(defmethod md-awaken :before ((self qx-object))
  (unless (slot-value self 'oid)
    (let ((s (session self)))
      (assert s () "No session for ~a, par ~a, usess ~a" self (fm-parent self) (u^ qxl-session))
      (setf (oid self) (get-next-oid s))
      (setf (gethash (oid self) (dictionary s)) self)))
  (make-qx-instance self))

(defmethod make-qx-instance ((self qx-object))
  (with-integrity (:client `(:make-qx ,self))
    (when (qx-class self)
      (qxfmt "clDict[~a] = new ~a(~{~a~^,~}); clDict[~@*~a].oid = ~@*~a;" 
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
  (onkeydown nil #+chill (lambda (self req)
               
               (let* ((key (req-val req "keyId"))
                      (mods (parse-integer (req-val req "mods"))))
                 (print (list :onkeydown self key mods :fini)))))
  onkeypress
  onkeyinput
  onclick
  ondblclick
  onappear
  onfocusin 
  (onblur nil #+overkillthatmessedupsomething (lambda (self req)
            (declare (ignorable self req))
            (b-if s (n^ qxl-session)
              (progn
                ;(trcx qx-widget-onblur self)
                )
              (trcx :blur-no-sess self))))
  (enabled t)
  (focusable nil :cell nil) ;; qx default is false for widget so no need for init value :js-false
  (focus-grab nil :cell nil)
  selectedp
  selected-key
  kb-selector
  onexecute ;; likely to need refactoring, or kb-selector moves to qooxlisp-control
  (timeout 5000 :cell nil)
  tool-tip-text
  ps3 ;; persistence; name held over from S3
  selectable
  )

(defmd kb-manager ()
  (kb-controls (make-hash-table :test 'equal) :cell nil))

(defmethod not-to-be :after ((self qx-widget))
  (kb-manager-checkout self))

(defun kb-control-match (self key mods)
  (let ((chord (list key mods)))
    (b-when m (n^ kb-manager)
      (gethash chord (kb-controls m)))))

(defun kb-manager-checkout (self)
  (b-when chord (kb-selector self)
    (b-when m (n^ kb-manager)
      (remhash chord (kb-controls m)))))

(defun selected-match (sought sel &key (test 'eql))
  (if (consp sel)
      (member sought sel :test test)
    (funcall test sought sel)))

(export! ps3 qx-widget tool-tip-text selectedp ^selectedp selected-match selected-key ^selected-key
  background-color ^background-color kb-manager kb-control-match)

(defmethod qx-configurations append ((self qx-widget))
  (nconc
   (cfg background-color)
   (cfg focusable)
   (cfg enabled)
   (cfg selectable)
   (cfg tool-tip-text)))

(defmethod make-qx-instance :after ((self qx-widget))
  ;;>>> Make this dependent on some focusable flag, prolly a non-cell
  (with-integrity (:client `(:post-make-qx ,self))
    (b-when s (kb-selector self)
      (b-when m (n^ kb-manager)
        (b-if x (gethash s (kb-controls m))
            (break "chord ~a for ~a already registered as ~a in manager ~a" s self x m)
          (setf (gethash s (kb-controls m)) self))))

    (when (focusable self)
      (qxfmt "
clDict[~a].addListener('focus', function (e) {
    var rq = new qx.io.remote.Request('/focusOn?sessId='+sessId+'&oid=~:*~a'
                                      ,'GET'
                                      , 'text/javascript');
    rq.send();
    });" (oid self)(oid self))))
  (with-integrity (:client `(:post-assembly ,self))
    (when (focus-grab self)
      (focus-on self)))
    )

(defobserver enabled ()
  (when (or (null new-value) old-value-boundp) ;; only needed if off or was off
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setEnabled(~a);" (oid self) (if new-value "true" "false")))))

(defobserver visibility ()
  (when old-value-boundp
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setVisibility('~a');" (oid self) new-value))))

(defobserver decorator () 
  (when (or new-value old-value)
    (with-integrity (:client `(:post-make-qx ,self))
      (if new-value
          (qxfmt "clDict[~a].setDecorator('~a');" (oid self) new-value)
        (qxfmt "clDict[~a].setDecorator(null);" (oid self) )))))
       
(defobserver background-color ()
  (when old-value
    (if new-value
        (qxfmt "clDict[~a].setBackgroundColor('~(~a~)');" (oid self) new-value)
      (qxfmt "clDict[~a].setBackgroundColor(null);" (oid self) ))))

(export! onexecute)

(defmd qooxlisp-control () ;; qooxlisp- indicates this is a Lisp-side only class
  onexecute
  (timeout 5000 :cell nil))

(defobserver onexecute ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('execute', function(e) {
    var rq = new qx.io.remote.Request('/callback','GET', 'text/javascript');
    rq.setParameter('sessId',sessId);
    rq.setParameter('oid',~:*~a);
    rq.setParameter('opcode','onexecute');
    rq.setTimeout(~a);
    rq.addListener('timeout',function () {alert('Timeout waiting on server');});
    console.log('send onexecute');
    rq.send();
});" 
                  (oid self)  (timeout self))))))
#|
    
    rq.setTimeout(~a);
    rq.addListener('timeout',function () {alert('Timeout waiting on server');});

    console.log('send onexecute');
|#
(defobserver onclick ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('click', function(e) {
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onclick&oid=~:*~a','GET', 'text/javascript');
    rq.send();
});" (oid self))))))

(defobserver ondblclick ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('dblclick', function(e) {
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=ondblclick&oid=~:*~a','GET', 'text/javascript');
    rq.setTimeout(~a);
    rq.addListener('timeout',function () {alert('Timeout waiting on server');});
    rq.send();
});" (oid self)(timeout self))))))

(defobserver onappear ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (unless old-value
                  (qxfmt "
clDict[~a].addListener('appear', function(e) {
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onappear&oid=~:*~a','GET', 'text/javascript');
    rq.send();
});" (oid self)))))))

(defobserver onfocusin ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (unless old-value
                  (qxfmt "
clDict[~a].addListener('focusin', function(e) {
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onfocusin&oid=~:*~a','GET', 'text/javascript');
    rq.send();
});" (oid self)(oid self)))))))

(defobserver onblur ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (unless old-value
                  (qxfmt "
clDict[~a].addListener('blur', function(e) {
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onblur&oid=~:*~a','GET', 'text/javascript');
    rq.send();
});" (oid self)(oid self)))))))


(defobserver onkeydown ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('keydown', function(e) {
    var oid = ~:*~a;
    var k = e.getKeyIdentifier();
    e.stopPropagation();
    var rq = new qx.io.remote.Request('/callback?opcode=onkeydown','GET', 'text/javascript');
    rq.setParameter('sessId',sessId);
    rq.setParameter('oid',oid);
    rq.setParameter('keyId', k);
    rq.setParameter('mods', e.getModifiers());
        if (jsMath.TeX==undefined)
           rq.setParameter('tex','no');
        else
           rq.setParameter('tex','yes');
    if (k=='Enter') {
       if (clDict[oid].timeoutset==undefined) {
           clDict[oid].timeoutset=true;
           rq.addListener('timeout',function () {alert('Timeout by widget '+oid+' on keyDown waiting on server');});
       }
    }
    console.log('send onkeydown '+k);
    rq.send();
});" (oid self))))))

;;     console.log('send onkeydown '+k);

(defobserver onkeypress ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     ((functionp new-value) (qxfmt "
clDict[~a].addListener('keypress', function(e) {
    var k = e.getKeyIdentifier();
    // e.preventDefault();
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onkeypress&oid=~:*~a','GET', 'text/javascript');
    rq.setParameter('keyId', k);
    rq.setParameter('mods', e.getModifiers());
    console.log('keypress listener 1 sends '+ k);
    rq.send();
});" (oid self)))

     ;; Next guy keeps firefox from popping up a search prompt
     ;; and ie from browsing back on backspace
     ;; but then possibly made it necessary to hit Enter or PageDown
     ;; twice before beg noticed on IE... 
     
     (new-value (qxfmt "
clDict[~a].addListener('keypress', function(e) {
     e.preventDefault();
    console.log('keypress just prevents default on keypress '+ e.getKeyIdentifier());
});" (oid self)))

     #+chill-youneedtobespecificremovinglisteners
     (old-value
      ;;untested
      (qxfmt "clDict[~a].removeListener('keypress');" (oid self))))))

;;     console.log('send onkeypress '+k);
;;      console.log('keypress just prevent default '+e.getKeyIdentifier());

(defobserver onkeyinput ()
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "
clDict[~a].addListener('keyinput', function (e) {
    if (e.getChar()=='/') e.preventDefault();
    var rq = new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onkeyinput&oid=~:*~a','GET','text/javascript');
    rq.setParameter('char', e.getChar());
    rq.setParameter('code', e.getCharCode());
    rq.setParameter('mods', e.getModifiers());
    console.log('keyinput listener sends '+ e.getChar());
    rq.send();
});" (oid self)))))

;;     console.log('send onkeyinput '+ e.getChar());

(defmd qooxlisp-layouter (qx-widget qooxlisp-family)
  (layout nil :owning t))

(defobserver layout ()
  (when new-value
    (with-integrity (:client `(:layout ,self))
      (qxfmt "clDict[~a].setLayout(clDict[~a]);~%" (oid self)(oid (layout self))))))

(defmd qx-composite (qooxlisp-layouter)
  (qx-class "qx.ui.container.Composite" :allocation :class :cell nil))

(export! onappear html)


(defparameter *set-html* "
if (clDict[~a]!==undefined) {
   var de = clDict[~:*~a].keepDE;
   var ih = ~s;
   if (de!==undefined) {
      de.innerHTML = ih;}}")

(export! set-html)
(defmethod set-html (self)
  (when (html self)
    (qxfmt *set-html* (oid self) (html self))))

(defmethod gethtml (self) ;; for now assumed unvarying callback
  (declare (ignore self))
  (lambda (self req)
    (declare (ignore req))
    (set-html self)))

(defmd qx-html (qx-widget)
  (qx-class "qx.ui.embed.Html" :allocation :class :cell nil)
  html)

(defobserver html ()
  (with-integrity (:client `(:post-make-qx ,self))
    ;; in case anyone is asking
    (set-html self)
    ))

(defmethod qxl::make-qx-instance :after ((self qx-html))
  (with-integrity (:client `(:post-make-qx ,self))
    ;; --- appear : pick up inner entities ---
    (qxfmt "clDict[~a].addListener('appear', function(e) {
       var oid = ~:*~a;
       var ce = clDict[oid].keepCE = clDict[oid].getContentElement();
       var de = clDict[oid].keepDE = ce.getDomElement();
       cbjs(~:*~d,'gethtml','appeared');
});"  (oid self))))

  
