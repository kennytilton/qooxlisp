;; -*- mode: Lisp; Syntax: Common-Lisp; Package: qooxlisp; -*-
#|

    layout -- qooxdoo layout

(See package.lisp for license and copyright notigification)

|#

(in-package :qxl)

(defmd qx-layout-item (qx-object) ;; better name: qx-laid-out-item
  (visibility "visible")
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

(export! height ^height ^min-height min-height ^max-height max-height visibility ^visibility
   ^min-height min-height ^max-height max-height)

(export! width ^width ^min-width min-width ^max-width max-width  
  ^min-width min-width ^max-width max-width)

(defobserver min-height ()
  (when old-value-boundp
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setMinHeight(~a);" (oid self) new-value))))

(defobserver max-height ()
  (when old-value-boundp
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setMaxHeight(~a);" (oid self) new-value))))

(defun visible (self)
  (assert self () "visi self")
  (equal (visibility self) "visible"))

(defmacro ^visible ()
  `(visible self))

(defun hidden (self)
  (assert self () "hid self")
  (equal (visibility self) "hidden"))

(defun collapsed (self)
  (assert self () "collaps self")
  (equal (visibility self) "excluded"))

(defun vis/not (b)
  (if b "visible" "hidden"))

(defmacro vis/collapsed (b)
  `(if ,b "visible" "excluded"))

(export! visible ^visible collapsed hidden vis/not vis/collapsed)


(defmethod qx-configurations append ((self qx-layout-item))
  (nconc
   (cfg visibility)
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

(export! qx-flow)
(defmd qx-flow (qx-layout-abstract)
  (qx-class "qx.ui.layout.Flow" :allocation :class :cell nil)
  spacing-x spacing-y align-x align-y reversed)

(defmethod qx-configurations append ((self qx-flow))
  (nconc
   (cfg align-x)(cfg align-y)
   (cfg reversed)
   (cfg spacing-x)(cfg spacing-y)))


;;;      var scroller = new qx.ui.container.Scroll();
;;;      win.add(scroller);
;;;
;;;      var container = new qx.ui.container.Composite(new qx.ui.layout.Flow()).set({
;;;        allowShrinkY: false
;;;      });
;;;      scroller.add(container);
;;;
;;;      for (var i=0; i<30; i++)
;;;      {
;;;        container.add(new qx.ui.basic.Atom("item #" + (i+1), "icon/48/devices/computer.png").set({
;;;          iconPosition: "top",
;;;          width: 60,
;;;          padding: 5
;;;        }));
;;;      }
