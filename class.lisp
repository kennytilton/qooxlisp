(in-package :qxl)

(defmd qx-family (family))

(defobserver .kids ((self qx-family))
  (with-integrity (:client `(:post-make-qx ,self))
    (trc "kidsing" self old-value new-value)
    (loop for k in (set-difference old-value new-value)
        do (qxfmt "clDict[~a].remove(clDict[~a]);" (oid self)(oid k)))
    (loop for k in (set-difference new-value old-value) do 
          ;;(qxfmt "console.log('adding: to '+ ~a + ' the new ' + ~a);" pa new)
          (qxfmt "clDict[~a].add(clDict[~a],~a);" (oid self) (oid k) (add-ops k)))))

(defmd qx-document (qx-family)
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
  (trc "mqi" *qxdoc* .doc doc)
  (with-integrity (:client `(:make-qx ,self))
    (setf (oid self) (get-next-id doc))
    (setf (gethash (oid self) (dictionary doc)) self)
    
    (when (qx-class self)
      (qxfmt "clDict[~a] = new ~a().set(~a);"
        (oid self)
        (qx-class self)
        (json::encode-json-to-string (qx-configurations self)))
      (qxfmt "console.log('stored new oid/obj ~a '+ clDict[~:*~a]);" (oid self)))))
          
(defmethod qx-configurations (self)
            (declare (ignore self)))

(defmd qx-layout-item (qx-object)
  add)

(defun add-ops (self &aux (add (add self)))
  (typecase add
    (string add)
    (cons (apply 'jsk$ add))))


(defmd qx-widget (qx-layout-item))

(defmd qx-control ()
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

(defmd qx-container (qx-widget qx-family)
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

;;; --- qx-combo-box --------------------------------------

(defmd qx-combo-box (qx-control qx-atom)
  (qx-class "qx.ui.form.ComboBox" :allocation :class :cell nil)
  onchangevalue)

(defobserver onchangevalue ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('changeValue', function(e) {
    (new qx.io.remote.Request('/callback?opcode=onchangevalue&oid=~:*~a&value='+e.getData(),'GET', 'text/javascript')).send();
});" (oid self))))))

;;; --- qx-button --------------------------------------

(defmd qx-button (qx-control qx-atom)
  (qx-class "qx.ui.form.Button" :allocation :class :cell nil))

(defmethod qx-configurations ((self qx-button))
  (nconc
   (bwhen (x (label self))
     (list (cons :label x)))))

(defmacro new-button ((&rest add-plist) &rest inits)
  `(make-instance 'qx-button
     :fm-parent *parent*
     :add (list ,@add-plist)
     ,@inits))

;;; --- qx-label --------------------------------------

(defmd qx-label (qx-widget)
  (qx-class "qx.ui.basic.Label" :allocation :class :cell nil)
  value)

(defmethod qx-configurations ((self qx-label))
  (nconc
   (bwhen (x (value self))
     (list (cons :value x)))))

(defmd qx-layout (qx-object))
(defmd qx-layout-abstract (qx-layout))
(defmd qx-hbox (qx-layout-abstract)
  (qx-class "qx.ui.layout.HBox" :allocation :class :cell nil) 
  spacing)

(defmethod qx-configurations ((self qx-hbox))
  (nconc
   (bwhen (x (spacing self))
     (list (cons :spacing x)))))

;;;(defmacro new-hbox ((&rest inits) &rest kids)
;;;  `(make-instance 'qx-hbox
;;;     ,@inits
;;;     :kids (c? (the-kids ,@kids))))

(defmd qx-vbox (qx-layout-abstract)
  (qx-class "qx.ui.layout.VBox" :allocation :class :cell nil) 
  spacing)

(defmethod qx-configurations ((self qx-vbox))
  (nconc
   (bwhen (x (spacing self))
     (list (cons :spacing x)))))

(defmacro new-vbox ((&rest inits) &rest kids)
  `(make-instance 'qx-layout-vbox
     ,@inits
     :kids (c? (the-kids ,@kids))))
