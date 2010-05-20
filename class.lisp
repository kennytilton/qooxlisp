(in-package :qxl)

(defmd qooxlisp-family (family))

(defobserver .kids ((self qooxlisp-family))
  (progn
    (trc "kidsing outer!!!!!!!!!!!!!!!!!!!!!" self old-value new-value)
    (with-integrity (:client `(:post-make-qx ,self))
      (print (list "kidsing!!!!!!!!!!!!!!!!!" self old-value new-value))
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
  (trc "mqi !!!!!!!!!!!!!!! outer" *qxdoc* .doc doc)
  (with-integrity (:client `(:make-qx ,self))
    (trc "mqi !!!!!!!!!!!!!!! inner" *qxdoc* .doc doc)
    (setf (oid self) (get-next-id doc))
    (setf (gethash (oid self) (dictionary doc)) self)
    
    (when (qx-class self)
      (b-if cfg (qx-configurations self)
        (qxfmt "clDict[~a] = new ~a().set(~a);" (oid self)(qx-class self)(json$ cfg))
        (qxfmt "clDict[~a] = new ~a();" (oid self)(qx-class self)))
      (qxfmt "console.log('stored new oid/obj ~a '+ clDict[~:*~a]);" (oid self)))))
          
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
    console.log('executing ~:*~a');
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

(defmd qx-container (qx-widget qooxlisp-family)
  layout)

(defobserver layout ()
  (when new-value
    (with-integrity (:client `(:layout ,self))
      (qxfmt "clDict[~a].setLayout(clDict[~a]);~%" (oid self)(oid (layout self))))))

(defmd qx-composite (qx-container)
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
                     (qxfmt "console.log('nada');"))))
  :value (c-in nil))

(defmd qx-list-item (qx-atom)
  (qx-class "qx.ui.form.ListItem" :allocation :class :cell nil))

(defobserver onchangevalue ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('changeValue', function(e) {
    (new qx.io.remote.Request('/callback?opcode=onchangevalue&oid=~:*~a&value='+e.getData(),'GET', 'text/javascript')).send();
});" (oid self))))))

;;; --- button --------------------------------------

(defmd qx-button (qooxlisp-control qx-atom)
  (qx-class "qx.ui.form.Button" :allocation :class :cell nil)
  (allow-grow-x :js-false)
  (allow-grow-y :js-false))

(defmethod qx-configurations append ((self qx-button))
  (nconc
   
   (b-when x (allow-grow-x self)
     (list (cons :allow-grow-x x)))
   (b-when x (allow-grow-y self)
     (list (cons :allow-grow-y x)))))

(defmacro new-button ((&rest add-plist) &rest inits)
  `(make-instance 'qx-button
     :fm-parent *parent*
     :add (list ,@add-plist)
     ,@inits))

;;; --- label --------------------------------------

(defmd qx-label (qx-widget)
  (qx-class "qx.ui.basic.Label" :allocation :class :cell nil)
  value
  (allow-grow-x :js-false)
  (allow-grow-y :js-false))

(defmethod qx-configurations append ((self qx-label))
  (nconc
   (b-when x (value self)
     (list (cons :value x)))
   (b-when x (allow-grow-x self)
     (list (cons :allow-grow-x x)))
   (b-when x (allow-grow-y self)
     (list (cons :allow-grow-y x)))))