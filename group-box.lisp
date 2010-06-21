(in-package :qooxlisp)

;;; --- group boxes ------------------

(defmd qx-group-box (qooxlisp-layouter)
  (qx-class "qx.ui.groupbox.GroupBox" :allocation :class :cell nil)
  legend)



;;;(defmethod qx-configurations append ((self qx-group-box))
;;;  )

(defmd qx-check-group-box (qx-group-box)
  (qx-class "qx.ui.groupbox.CheckGroupBox" :allocation :class :cell nil)
  value
  (onchangevalue (lambda (self req)
                   (print :onchangevalue-fires)
                   (let ((nv (req-val req "value")))
                     (setf (^value) (cvtjs nv))))))

(defmethod qx-configurations append ((self qx-check-group-box))
  (nconc
   (cfg value)))

(defobserver value ((self qx-check-group-box))
  (mprt :qx-check-group-box-observes-value new-value old-value)
  (with-integrity (:client `(:post-make-qx ,self))
    (qxfmt "clDict[~a].setValue(~a);"
      (oid self) (if new-value "true" "false"))))

(defmd qx-radio-group-box (qxl-radio-item qx-group-box)
  (qx-class "qx.ui.groupbox.RadioGroupBox" :allocation :class :cell nil)
  model) ;; trying to fix qooxddo radio group box

(defmethod qxl-model ((self qx-radio-group-box))
  (model self))

(defobserver legend ((self qx-radio-group-box))
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setLegend('~a');"
        (oid self) new-value))))