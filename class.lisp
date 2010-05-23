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

(defmd qx-document (qooxlisp-family)
  (oid 0 :cell nil)
  (doc-hash nil :cell nil)
  (dictionary (make-hash-table) :cell nil)
  (next-id 1 :cell nil :allocation :class)
  )

(defmethod initialize-instance :after ((self qx-document) &key)
  (setf *qxdoc* self))
      
(defun get-next-id (doc)
  (prog1
      (next-id doc)
    (incf (next-id doc))))

(define-symbol-macro .doc *qxdoc*) ;; eventually to be held in a session variable
(define-symbol-macro .dict (dictionary .doc))

(defmd qx-object ()
  (oid nil :cell nil)
  (jsoid nil :cell nil))

(defmethod md-awaken :before ((self qx-object))
  (make-qx-instance self))

(defmethod make-qx-instance ((self qx-object) &aux (doc .doc))
  (with-integrity (:client `(:make-qx ,self))
    (setf (oid self) (get-next-id doc))
    (setf (gethash (oid self) (dictionary doc)) self)
    
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
  background-color
  onkeypress
  (enabled t))

(defobserver enabled ()
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].setEnabled(~a);" (oid self) (if new-value 'true 'false))))
       
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
    var rq = new qx.io.remote.Request('/callback?opcode=onexecute&oid=~:*~a','GET', 'text/javascript');
    rq.send();
});" 
                  (oid self))))))

(defobserver onkeypress ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('keypress', function(e) {
    var rq = new qx.io.remote.Request('/callback?opcode=onkeypress&oid=~:*~a&keyId='+e.getKeyIdentifier()+'&value='+this.getValue(),'GET', 'text/javascript');
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

;; in cells "owned" handling? (defobserver .kids ((self qx-composite))
  
(defmd qx-atom (qx-widget)
  label)

(defmethod qx-configurations append ((self qx-atom))
  (nconc
   (b-when x (label self)
     (list (cons :label x)))))

;;; --- qx-combo-box --------------------------------------

(defmd qx-abstract-select-box (qooxlisp-control qx-widget qooxlisp-family))

(defmd qx-combo-box (qx-abstract-select-box)
  (qx-class "qx.ui.form.ComboBox" :allocation :class :cell nil)
  (onchangevalue (lambda (self req)
                   (print :onchangevalue-fires)
                   (let ((nv (req-val req "value")))
                     (setf (^value) nv)
                     (trc "combo-box ~a changed to ~a')"
                       (oid self) nv)
                     (qxfmt "console.log('ack chgval');"))))
  :value (c-in nil))

(defobserver onchangevalue ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('changeValue', function(e) {
    (new qx.io.remote.Request('/callback?opcode=onchangevalue&oid=~:*~a&value='+e.getData(),'GET', 'text/javascript')).send();
});" (oid self))))))

(defmd qx-list-item (qx-atom)
  (qx-class "qx.ui.form.ListItem" :allocation :class :cell nil)
  model)

(defmethod qx-configurations append ((self qx-list-item))
  (nconc
   (b-when x (model self)
     (list (cons :model x)))))

;;; --- button --------------------------------------

(defmd qx-button (qooxlisp-control qx-atom)
  (qx-class "qx.ui.form.Button" :allocation :class :cell nil))

;;; --- label --------------------------------------

(defmd qx-label (qx-widget)
  (qx-class "qx.ui.basic.Label" :allocation :class :cell nil)
  value
  (allow-grow-x :js-false)
  (allow-grow-y :js-false))

(defmethod qx-configurations append ((self qx-label))
  (nconc
   (b-when x (value self)
     (list (cons :value x)))))


;;; --- radio buttons --------------------------------

(defmd qx-radio-button-group (qooxlisp-control qooxlisp-layouter)
  (qx-class "qx.ui.form.RadioButtonGroup" :allocation :class :cell nil)
  (onchangeselection (lambda (self req)
                       (let ((nv (req-val req "value")))
                         (print `(:rbgroup ,nv))
                         (setf (^value) (intern nv :keyword))
                         (qxfmt "console.log('nada');")))))

(defobserver onchangeselection ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('changeSelection', function(e) {
    var rb = e.getData()[0];
    console.log('new sel='+rb+ ' listen '+clDict[~a]);
    var md = 'null';
    if (rb) md = rb.getModel();
    //console.log('new rb md ='+md);
    (new qx.io.remote.Request('/callback?opcode=onchangeselection&oid=~:*~a&value='+md,'GET', 'text/javascript')).send();
});" (oid self)(oid self))))))

(defmd qx-radio-button (qx-button qooxlisp-control )
  (qx-class "qx.ui.form.RadioButton" :allocation :class :cell nil)
  model)

(defmethod qx-configurations append ((self qx-radio-button))
  (nconc
   (b-when x (model self)
     (list (cons :model x)))))

(defmd qx-toggle-button (qx-atom qooxlisp-control )
  (value (c-in nil))
  (onchangevalue (lambda (self req)
                   (print :onchangevalue-fires)
                   (let ((nv (cvtjs (req-val req "value"))))
                     (setf (^value) nv)
                     (trc "qx-toggle-button ~a changed to ~a')"
                       self nv)
                     (qxfmt "console.log('nada');")))))

(defmethod qx-configurations append ((self qx-toggle-button))
  (nconc
   (b-when x (value self)
     (list (cons :value x)))))
  
(defmd qx-check-box (qx-toggle-button )
  (qx-class "qx.ui.form.CheckBox" :allocation :class :cell nil))

(defmd qx-select-box (qx-abstract-select-box)
  (qx-class "qx.ui.form.SelectBox" :allocation :class :cell nil)
  (onchangeselection (lambda (self req)
                       (let ((nv (req-val req "value")))
                         (print `(:qx-select-box ,nv))
                         (setf (^value) (find-package nv))
                         (qxfmt "console.log('naack chg sel');"))))
  :value (c-in nil))
