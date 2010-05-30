(in-package :qxl)

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
                     (mprt :onchgvalu self :now nv))))
  :value (c-in nil))

(defobserver onchangevalue ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('changeValue', function(e) {
    (new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onchangevalue&oid=~:*~a&value='+e.getData(),'GET', 'text/javascript')).send();
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

(defobserver value ((self qx-label))
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].setValue('~a');" (oid self) (or new-value ""))))

;;; --- radio buttons --------------------------------

(defmd qx-radio-button-group (qooxlisp-control qooxlisp-layouter)
  (qx-class "qx.ui.form.RadioButtonGroup" :allocation :class :cell nil)
  (onchangeselection (lambda (self req)
                       (let ((nv (req-val req "value")))
                         (print `(:rbgroup ,nv))
                         (setf (^value) (intern nv :keyword))
                         ;;(qxfmt "null" #+not "consolelog('nada');")
                         ))))

(defobserver onchangeselection ()
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('changeSelection', function(e) {
    var rb = e.getData()[0];
    //consolelog('new sel='+rb+ ' listen '+clDict[~a]);
    var md = 'null';
    if (rb) md = rb.getModel();
    //consolelog('new rb md ='+md);
    (new qx.io.remote.Request('/callback?sessId='+sessId+'&opcode=onchangeselection&oid=~:*~a&value='+md,'GET', 'text/javascript')).send();
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
                       self nv)))))

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
                         (setf (^value) (find-package nv)))))
  :value (c-in nil))

;;; --- group boxes ------------------

(defmd qx-group-box (qooxlisp-layouter)
  (qx-class "qx.ui.groupbox.GroupBox" :allocation :class :cell nil)
  legend)

(defmethod qx-configurations append ((self qx-group-box))
  (nconc
   (b-when x (legend self)
     (list (cons :legend x)))))

(defmd qx-check-group-box (qx-group-box)
  (qx-class "qx.ui.groupbox.CheckGroupBox" :allocation :class :cell nil)
  value
  (onchangevalue (lambda (self req)
                   (print :onchangevalue-fires)
                   (let ((nv (req-val req "value")))
                     (setf (^value) (cvtjs nv))))))

(defmethod qx-configurations append ((self qx-check-group-box))
  (nconc
             (b-when x (value self)
               (list (cons :value x)))))

(defobserver value ((self qx-check-group-box))
  (mprt :qx-check-group-box-observes-value new-value old-value)
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].setValue(~a);"
      (oid self) (if new-value "true" "false"))))

(defobserver legend ((self qx-check-group-box))
  )