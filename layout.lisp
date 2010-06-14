;; -*- mode: Lisp; Syntax: Common-Lisp; Package: qooxlisp; -*-
#|

    layout -- qooxdoo layout

(See package.lisp for license and copyright notigification)

|#

(in-package :qxl)

(defmd qx-layout-item (qx-object) ;; better name: qx-laid-out-item
  add
  height min-height max-height
  width min-width max-width
  margin margin-left margin-top margin-right margin-bottom
  padding padding-left padding-top padding-right padding-bottom
  align-x align-y 
  allow-grow-x
  allow-grow-y
  allow-shrink-x allow-shrink-y
  allow-stretch-x allow-stretch-y)

(defmacro cfg (f)
  (let ((x (gensym)))
    `(b-when ,x (,f self)
       (list (cons ,(intern f :keyword) ,x)))))

(defmethod qx-configurations append ((self qx-layout-item))
  (nconc
   (cfg height)(cfg min-height)(cfg max-height)
   (cfg width)(cfg min-width)(cfg max-width)
   (cfg margin)(cfg margin-left)(cfg margin-top)(cfg margin-right)(cfg margin-bottom)
   (cfg padding)(cfg padding-left)(cfg padding-top)(cfg padding-right)(cfg padding-bottom)
   (cfg align-x)
   (cfg align-y)
   (cfg allow-grow-y)
   (cfg allow-grow-x)
   (cfg allow-shrink-y)
   (cfg allow-shrink-y)))

(defmd qx-layout (qx-object))
(defmd qx-layout-abstract (qx-layout))

(defmd qx-grid (qx-layout-abstract)
  (qx-class "qx.ui.layout.Grid" :allocation :class :cell nil)
  spacing-x spacing-y)

(defmethod qx-configurations append ((self qx-grid))
  (nconc
   (cfg spacing-x)(cfg spacing-y)))

(defmd qx-hv-box (qx-layout-abstract)
  ; anstract class on lisp side only
  align-x align-y reversed separator spacing)

(defmethod qx-configurations append ((self qx-hv-box))
  (nconc
   (cfg align-x)(cfg align-y)(cfg reversed)(cfg separator)(cfg spacing)))

(defmd qx-hbox (qx-hv-box)
  (qx-class "qx.ui.layout.HBox" :allocation :class :cell nil))

(defmd qx-vbox (qx-hv-box)
  (qx-class "qx.ui.layout.VBox" :allocation :class :cell nil))

