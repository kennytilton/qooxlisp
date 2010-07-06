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
                         (setf (^value) nvs))))
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
       sel = sel + items[i].oid;
    }
    var req = new qx.io.remote.Request('/callback','GET', 'text/javascript');
    req.setParameter('sessId', sessId);
    req.setParameter('oid', ~@*~a);
    req.setParameter('opcode', 'onchangeselection');
    req.setParameter('value',sel);
    req.send();
});" (oid self))))))

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
                     (setf (^value) nv))))
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
                     (setf (^value) nv))))
  :value (c-in nil))

(defmd qx-text-field (qx-abstract-field)
  (qx-class "qx.ui.form.TextField" :allocation :class :cell nil))



(defmd qx-list-item (qx-atom)
  (qx-class "qx.ui.form.ListItem" :allocation :class :cell nil)
  model) ;; >>> bad idea? Use value? Why is model sent to client? Most Lisp objects won't go there

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
  text$
  rich
  :allow-grow-x :js-false
  :allow-grow-y :js-false)

(defmethod qx-configurations append ((self qx-label))
  (nconc
   (cfg rich)
   (cfg value text$)))

(defobserver text$ ((self qx-label))
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
                         (b-if oid (parse-integer nv :junk-allowed t)
                           (let ((sel (gethash oid (dictionary *web-session*))))
                             (assert sel () "unknown oid in changesel ~a" oid)
                             (unless (equal (^value) (model sel))
                               (setf (^value) (model sel))))
                           (warn "Invalid oid parameter ~s in onchgsel callback"  (req-val req "value")))))))

(defobserver .value ((self qx-radio-button-group))
  ;;; >>> this needs work to allow a multiple selection, which some of the code allows
  (unless old-value-boundp
    (with-integrity (:client `(:post-assembly ,self))
      (block nil
        (fm-traverse self (lambda (k)
                            (when (typep k 'qxl-radio-item)
                              (when (equal new-value (model k))
                                (qxfmt "
var rg = clDict[~a];
var oldsel = rg.getSelection()[0];
var rb = clDict[~a];

if (rb !== oldsel) {
   var sel = [];
   sel.push(rb);
   rg.setSelection(sel);
}" (oid self)(oid k))
                              (return))))
        :global-search nil :skip-node self :opaque nil)))))


(defmd qxl-radio-item (qooxlisp-control))

(defmd qx-radio-button (qx-button qxl-radio-item)
  (qx-class "qx.ui.form.RadioButton" :allocation :class :cell nil)
  model)

(defmethod qxl-model ((self qx-radio-button))
  (model self))

(defmethod qx-configurations append ((self qx-radio-button))
  (nconc (cfg model)))

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
                         (setf (^value) nv))))
  :value (c-in nil))

(defmd qx-scroll (qx-widget qooxlisp-family)
  (qx-class "qx.ui.container.Scroll" :allocation :class :cell nil)
  )

(defmd qx-stack (qx-widget qooxlisp-family) ;; oops, too close to qxl-stack, which is really a vbox
  ;; this is actually an overlay
  (qx-class "qx.ui.container.Stack" :allocation :class :cell nil)
  selection
  )

(defobserver selection ((self qx-stack))
  (with-integrity (:client `(:post-make-qx ,self))
    (cond
     (new-value (qxfmt "clDict[~a].setSelection([clDict[~a]]);" (oid self)(oid new-value)))
     (old-value (qxfmt "clDict[~a].setSelection(null);" (oid self)(oid new-value))))))
                        
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

;;; --------------- selector ------------------------------------

(export! qxl-selector selection ^selection .selector .selection
  selected-key ^selected-key multiple-choice? toggle?)

(defmd qxl-selector ()
  selection
  selected-key
  multiple-choice?
  toggle?)

(define-symbol-macro .selector (n^ qxl-selector))
(define-symbol-macro .selection (selection .selector))

