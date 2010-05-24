(in-package :qxl)

(defmd qooxlisp-family (family))

(defobserver .kids ((self qooxlisp-family))
  (progn
    (with-integrity (:client `(:post-make-qx ,self))
      ;(print (list "kidsing!!!!!!!!!!!!!!!!!" self old-value new-value))
      (loop for k in (set-difference old-value new-value)
          do (qxfmt "clDict[~a].remove(clDict[~a]);" (oid self)(oid k)))
      (loop for k in (set-difference new-value old-value) do 
            ;;(qxfmt "console.log('adding: to '+ ~a + ' the new ' + ~a);" pa new)
            (b-if ao (add-ops k)
              (qxfmt "clDict[~a].add(clDict[~a],~a);" (oid self) (oid k) ao)
              (qxfmt "clDict[~a].add(clDict[~a]);" (oid self) (oid k)))))))

(defparameter *session-ct* 0)
(defparameter *qx-sessions* (make-hash-table))

(defmd qxl-session (qooxlisp-family)
  (session-id (incf *session-ct*) :cell nil)
  (oid 0 :cell nil)
  (dictionary (make-hash-table) :cell nil)
  (next-oid 1 :cell nil :allocation :class)
  )

(defun qxl-request-session (req)
  (gethash (parse-integer (req-val req "sessId") :junk-allowed t) *qx-sessions*))

(defmethod initialize-instance :after ((self qxl-session) &key)
  (assert (null (gethash (session-id self) *qx-sessions*)))
  (setf (gethash (session-id self) *qx-sessions*) self))
      
(defun get-next-oid (doc)
  (prog1
      (next-oid doc)
    (incf (next-oid doc))))

(defmd qx-object ()
  (session nil :cell nil)
  (oid nil :cell nil))

(defmethod initialize-instance :after ((self qx-object) &rest iargs &key)
  (unless (session self)
    (when (fm-parent self)
      (setf (session self) (nearest (fm-parent self) qxl-session))))
  (assert (session self) () "No session for ~a: keys ~a" self iargs)
  (setf (oid self) (get-next-oid (session self)))
  (setf (gethash (oid self) (dictionary (session self))) self))



(defmethod md-awaken :before ((self qx-object))
  (make-qx-instance self))

(defmethod make-qx-instance ((self qx-object))
  (with-integrity (:client `(:make-qx ,self))
    (when (qx-class self)
      (b-if cfg (qx-configurations self)
        (qxfmt "clDict[~a] = new ~a().set(~a);" (oid self)(qx-class self)(json$ cfg))
        (qxfmt "clDict[~a] = new ~a();" (oid self)(qx-class self)))
      #+shh (qxfmt "console.log('stored new oid/obj ~a '+ clDict[~:*~a]);" (oid self)))))
          
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

(defobserver enabled ()
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].setEnabled(~a);" (oid self) (if new-value 'true 'false))))

(defobserver decorator () ;; this one is not known to work yet
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setDecorator(new qx.ui.decoration.~a);" (oid self)(decorator self)))))
       
(defmethod qx-configurations append ((self qx-widget))
  (nconc
   (b-when x (background-color self)
     (list (cons :background-color x)))))

(defmd qooxlisp-control () ;; qooxlisp- indicates this is a Lisp-side only class
  onexecute)

(defobserver onexecute ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('execute', function(e) {
    //console.log('executing ~:*~a');
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
     (old-value
      ;;untested
      (qxfmt "clDict[~a].removeListener('keypress');" (oid self))))))

(defmd qooxlisp-layouter (qx-widget qooxlisp-family)
  layout)

(defobserver layout ()
  (when new-value
    (with-integrity (:client `(:layout ,self))
      (qxfmt "clDict[~a].setLayout(clDict[~a]);~%" (oid self)(oid (layout self))))))

(defmd qx-composite (qooxlisp-layouter)
  (qx-class "qx.ui.container.Composite" :allocation :class :cell nil))

(defmd qx-group-box (qooxlisp-layouter)
  (qx-class "qx.ui.groupbox.GroupBox" :allocation :class :cell nil)
  legend)

(defmethod qx-configurations append ((self qx-group-box))
  (nconc
   (b-when x (legend self)
     (list (cons :legend x)))))

(defmd qx-check-group-box (qooxlisp-layouter)
  (qx-class "qx.ui.groupbox.CheckGroupBox" :allocation :class :cell nil))
  
