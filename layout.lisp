(in-package :qxl)

(defmd qx-layout-item (qx-object) ;; better name: qx-laid-out-item
  add
  height width
  margin margin-left margin-top margin-right margin-bottom
  padding padding-left padding-top padding-right padding-bottom
  align-x align-y 
  allow-grow-x
  allow-grow-y
  allow-shrink-x allow-shrink-y
  allow-stretch-x allow-stretch-y)

(defun kwd (x) (intern x :keyword))

(defmacro cfg (f)
  (let ((x (gensym)))
    `(b-when ,x (,f self)
       (list (cons ,(kwd f) ,x)))))

(defmethod qx-configurations append ((self qx-layout-item))
  (nconc
   (cfg padding)
   (cfg align-x)
   (cfg align-y)
   (cfg allow-grow-y)
   (cfg allow-grow-x)
   (cfg allow-shrink-y)
   (cfg allow-shrink-y)))

(defmd qx-layout (qx-object))
(defmd qx-layout-abstract (qx-layout))

(defmd qx-hv-box (qx-layout-abstract)
  ; anstract class on lisp side only
  align-x align-y reversed separator spacing)

(defmethod qx-configurations append ((self qx-hv-box))
  (nconc
   (b-when x (align-x self)
     (list (cons :align-x x)))
   (b-when x (align-y self)
     (list (cons :align-y x)))
   (b-when x (reversed self)
     (list (cons :reversed x)))
   (b-when x (separator self)
     (list (cons :separator x)))
   (b-when x (spacing self)
     (list (cons :spacing x)))
   ))

(defmd qx-hbox (qx-hv-box)
  (qx-class "qx.ui.layout.HBox" :allocation :class :cell nil))

(defmd qx-vbox (qx-hv-box)
  (qx-class "qx.ui.layout.VBox" :allocation :class :cell nil))

