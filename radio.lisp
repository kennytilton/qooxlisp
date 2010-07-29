(in-package :qxl)

(defmacro radiogroupbox ((&rest layo-iargs)(&rest iargs) &rest kids)
  ;;; unfinished....
  `(make-kid 'qx-radio-group-box
     ,@iargs
     :layout (c? (mk-layout self 'qx-hbox ,@layo-iargs))
     :kids (c? (the-kids ,@kids))))

(defmacro radiobuttongroup (name (&rest iargs)(layout-class &rest layout-iargs) &rest kids)
  `(make-kid 'qx-radio-button-group
     :md-name ,name
     ,@iargs
     :layout (mk-layout self ',layout-class ,@layout-iargs)
     :kids (c? (the-kids ,@kids))))

(defmacro radiobutton (model label &rest rbiargs)
  `(make-kid 'qx-radio-button
     :label ,label
     :model ,model
     ,@rbiargs))

;;; --- radio buttons --------------------------------

(defmd qxl-radio-group-abstract (qooxlisp-control qooxlisp-layouter)
  :value (c-in nil)
  (onchangeselection (lambda (self req)
                       (let ((nv (req-val req "value")))
                         (b-if oid (parse-integer nv :junk-allowed t)
                           (let ((sel (gethash oid (dictionary *web-session*))))
                             (assert sel () "unknown oid in changesel ~a" oid)
                             (unless (equal (^value) (model sel))
                               (setf (^value) (model sel))))
                           (warn "Invalid oid parameter ~s in onchgsel callback"  nv))))))

(defobserver .value ((self qxl-radio-group-abstract))
  ;;; >>> this needs work to allow a multiple selection, which some of the code allows
  (unless old-value-boundp
    (with-integrity (:client `(:post-assembly ,self))
      (block nil
        (fm-traverse self (lambda (k)
                            (when (typep k 'qxl-radio-item)
                              (when (equal new-value (model k))
                                (qxfmt "
var rg = clDict[~a].grouper;
var oldsel = rg.getSelection()[0];
var rb = clDict[~a];

if (rb !== oldsel) {
   var sel = [];
   sel.push(rb);
   rg.setSelection(sel);
}" (oid self)(oid k))
                              (return))))
        :global-search nil :skip-node self :opaque nil)))))

(defmd qx-radio-button-group (qxl-radio-group-abstract)
  (qx-class "qx.ui.form.RadioButtonGroup" :allocation :class :cell nil))

(defmethod make-qx-instance :after ((self qx-radio-button-group))
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].grouper = clDict[~@*~a];"
      (oid self))))

(defmd qx-radio-button-group-ex (qxl-radio-group-abstract)
  (allow-empty-p t) 
  (qx-class "qx.ui.container.Composite" :allocation :class :cell nil))

(defmethod make-qx-instance :after ((self qx-radio-button-group-ex))
  (b-when items (fm-collect-if self (lambda (x) (typep x 'qxl-radio-item)))
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "
clDict[~a].grouper = new qx.ui.form.RadioGroup(~{clDict[~a]~^,~});
clDict[~a].grouper.setAllowEmptySelection(~a);
clDict[~@*~a].grouper.setSelection([]);
"
        (oid self) (mapcar 'oid items)(oid self)(if (allow-empty-p self) "true" "false")))))

(export! qx-radio-button-group-ex legend)

;;; clDict[~a].grouper.setAllowEmptySelection(true);

(defmd qxl-radio-item (qooxlisp-control))

(defmd qx-radio-button (qx-button qxl-radio-item)
  (qx-class "qx.ui.form.RadioButton" :allocation :class :cell nil)
  model)

(defmethod qx-configurations append ((self qx-radio-button))
  (nconc (cfg model)))

(defmd qx-radio-group-box (qxl-radio-item qx-group-box)
  (qx-class "qx.ui.groupbox.RadioGroupBox" :allocation :class :cell nil)
  (model (c? (^value))) ;; trying to fix qooxddo radio group box
  appearance) 

(defmethod qx-configurations append ((self qx-radio-group-box))
  (nconc
   (cfg appearance)
   (cfg model)))

(export! qx-radio-group-box appearance)

(defobserver legend ((self qx-radio-group-box))
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setLegend('~a');"
        (oid self) new-value))))
