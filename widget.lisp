;; -*- mode: Lisp; Syntax: Common-Lisp; Package: qooxlisp; -*-
#|

    widget

(See package.lisp for license and copyright notigification)

|#

(in-package :qxl)

(defmd qx-atom (qx-widget)
  label)

(defmethod qx-configurations append ((self qx-atom))
  (nconc
   (b-when x (label self)
     (list (cons :label x)))))

(defmd qx-list (qooxlisp-control qx-widget qooxlisp-family)
  :value (c-in nil)
  (qx-class "qx.ui.form.List" :allocation :class :cell nil)
  selection-mode ;; single (default), multi, additive, or one
  (onchangeselection (lambda (self req)
                       (print :generic-list-changesel-fires (req-val req "value"))
                       (print :generic-list-changesel-fires )
                       (let* ((nv (req-val req "value"))
                              (nvs (split-sequence #\! nv)))
                         (mprt :onchgsel-a self :now nvs)
                         (setf (^value) nvs)
                         (mprt :onchgsel self :now nvs))))
  spacing)

(split-sequence #\! "aaa!bbb")

(defobserver .kids ((self qx-list))
  (with-cc :newlist
    (setf (^value) nil)))

(defobserver onchangeselection () ;; unspecialized, hoping all selections are lists
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "
clDict[~a].addListener('changeSelection', function(e) {
    var items = e.getData();
    var sel = '';
    for (i = 0; i < items.length; ++i) {
       if (i > 0) sel = sel + '!';
       sel = sel + items[i].getModel();
    }
    var req = new qx.io.remote.Request('/callback','GET', 'text/javascript');
    req.setParameter('sessId', sessId);
    req.setParameter('oid', ~a);
    req.setParameter('opcode', 'onchangeselection');
    req.setParameter('value',sel);
    req.send();
});" (oid self)(oid self))))))

(defmethod qx-configurations append ((self qx-list))
  (nconc
   (cfg spacing)
   (cfg selection-mode)))

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

(defmd qx-abstract-field (qx-widget)
  (qx-class "qx.ui.form.AbstractField" :allocation :class :cell nil)
  (onchangevalue (lambda (self req)
                   (print :onchangevalue-fires)
                   (let ((nv (req-val req "value")))
                     (setf (^value) nv)
                     (mprt :onchgvalu self :now nv))))
  :value (c-in nil))

(defmd qx-text-field (qx-abstract-field)
  (qx-class "qx.ui.form.TextField" :allocation :class :cell nil))



(defmd qx-list-item (qx-atom)
  (qx-class "qx.ui.form.ListItem" :allocation :class :cell nil)
  model)

(defmethod qx-configurations append ((self qx-list-item))
  (nconc
   (b-when x (model self)
     (list (cons :model x)))))

(defmacro listitem (label-form &optional model)
  (let ((label (gensym)))
    `(let ((,label ,label-form))
       (make-kid 'qx-list-item
         :label ,label
         :model (or ,model ,label)))))

;;; --- button --------------------------------------

(defmd qx-button (qooxlisp-control qx-atom)
  (qx-class "qx.ui.form.Button" :allocation :class :cell nil))

;;; --- label --------------------------------------

(defmd qx-label (qx-widget)
  (qx-class "qx.ui.basic.Label" :allocation :class :cell nil)
  value
  rich
  :allow-grow-x :js-false
  :allow-grow-y :js-false)

(defmethod qx-configurations append ((self qx-label))
  (nconc
   (cfg rich)
   (cfg value)))

(defobserver value ((self qx-label))
  ;; I think we need this for when value changes vs. during image creation
  ;; >>> prolly a good idea to figure this out
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].setValue('~a');" (oid self) (or new-value ""))))

(defmd qx-image (qx-widget)
  (qx-class "qx.ui.basic.Image" :allocation :class :cell nil)
  source
  scale)

(defmethod qx-configurations append ((self qx-image))
  (nconc
   (cfg source)
   (cfg scale)))

;;; --- radio buttons --------------------------------

(defmd qx-radio-button-group (qooxlisp-control qooxlisp-layouter)
  (qx-class "qx.ui.form.RadioButtonGroup" :allocation :class :cell nil)
  (onchangeselection (lambda (self req)
                       (let ((nv (req-val req "value")))
                         (print `(:rbgroup ,nv))
                         (setf (^value) nv)
                         ;;(qxfmt "null" #+not "consolelog('nada');")
                         ))))

(defobserver onchangeselection ((self qx-radio-button-group))
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
                       (mprt :default-qx-select-boxonchangeselection (req-val req "value"))
                       (let ((nv (req-val req "value")))
                         (setf (^value) nv))))
  :value (c-in nil))

(defmd qx-scroll (qx-widget qooxlisp-family)
  (qx-class "qx.ui.container.Scroll" :allocation :class :cell nil)
  )

(defmd qx-tab-view (qx-widget qooxlisp-family)
  (qx-class "qx.ui.tabview.TabView" :allocation :class :cell nil)
  bar-position)

(defmethod qx-configurations append ((self qx-tab-view))
  (nconc (cfg bar-position)))

(defmd qx-tab-page (qx-composite)
  (qx-class "qx.ui.tabview.Page" :allocation :class :cell nil)
  label icon)

(defmethod qx-configurations append ((self qx-tab-page))
  (nconc (cfg label)(cfg icon)))

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