;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
#|

Copyright (C) 2004 by Kenneth William Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :qxl)

;------------------------------

(defmethod turn-edit-active (self new-value)
  (declare (ignorable self new-value)))

(defmethod focus-shared-by (f1 f2)
  (declare (ignore f1 f2))
  nil)


(defobserver textual-focus (self new-value old-value)
  (when new-value
    (setf (insertion-pt self) 0)
    (setf (sel-end self) (bwhen (t$ (text$ new-value))
                           (length t$)))))


(defmethod focus-starting ((self focus-minder))
  (or (focus-minded self)
      (focus-find-first self)
      (focus-find-first self :tab-stop-only nil)))

(export! focus-on)

(defmethod focus-on (self &optional focuser)
  (c-assert (or self focuser))
  #+xxx (trc "focus.on self, focuser" self focuser .focuser (focus-state .focuser))
  ;; (break "focus.on self, focuser")
  (setf (focus (or focuser .focuser)) self))

(defgeneric focus-gain (self)
  (:method (self) (declare (ignore self)))
  (:method ((self focus))
    (trc nil "setting focused-on true" self)
    (setf (^focused-on) t)))

(defgeneric focus-lose (self new-focus)
  (:method (self new-focus) (if self
                      (focus-lose (fm-parent self) new-focus)
                    t))
  (:method :around ((self focus) new-focus)
    (declare (ignore new-focus))
    (when (call-next-method)
      (setf (^focused-on) nil))))

;________________________________ I d l i n g _______________________
;
(defmethod focus-idle (other) (declare (ignorable other)))

(defmethod focus-idle ((list list))
  (dolist (f list)
    (focus-idle f)))

;_____________________ I n t o - V i e w _____________________
; 
; 990329 /// kt Resurrect eventually
;
(defmethod focus-scroll-into-view ((focus focus))
  ;;  temp to get going (view-scroll-into-view focus)
  )

(defmethod focus-scroll-into-view (other)
  (declare (ignore other)))

(defmethod focus-scroll-into-view ((focii list))
  (dolist (focus focii)
    (focus-scroll-into-view focus)))

(defun focusable? (focus &optional (test #'true) (tab-stop-only t))
  (and (typep focus 'focus)
    (visible focus)
    (fully-enabled focus)
    (or (not tab-stop-only)
      (tabstopp focus))
    (funcall test focus)))

(export! focus-find-first focus-find-next focus-find-prior)

(defun focus-find-first (self &key (test #'true) (tab-stop-only t))
  (fm-find-if self (lambda (x)
                      (focusable? x test tab-stop-only))))

(defun focus-find-next (self &key (test #'true) (tab-stop-only t))
  (fm-find-next self (lambda (x)
                      (focusable? x test tab-stop-only))))

(defun focus-find-prior (self &key (test #'true) (tab-stop-only t))
  (fm-find-prior self (lambda (x)
                        (focusable? x test tab-stop-only))))


