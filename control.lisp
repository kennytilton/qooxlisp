;; -*- mode: Lisp; Syntax: Common-Lisp; Package: qooxlisp; -*-
#|

    control -- abstract control handling

(See package.lisp for license and copyright notigification)

|#


(in-package :qxl)
(export! control enabled ^enabled ct-action-lambda sound ^sound
  tool-tip tool-tip-show? click-evt ^click-evt ^mouse-over? mouse-over?)

(defmd control ()
  (ct-proxy (c? self))
  
  (title$ (c? (format nil "~:(~a~)" ;; this is just a poor default-- really s.b. supplied by form author
                (string-downcase (substitute #\space #\- (string (md-name self)))))))
  (ct-action nil :cell nil)
  sound
  click-repeat-p

  (mouse-up-handler nil :documentation "Menus use this")
  (click-evt (c-in nil))
  (double-click-evt (c-in nil))
  (double-click-action (c-in nil))
  
  (key-evt nil :cell :ephemeral)
  (enabled t)
  
  tool-tip
  
  (kb-selector nil :cell nil))



(defmethod do-double-click ((self control) )
  (b-when a (^double-click-action)
    (trc "control sees defmethod" self a)
    (funcall a self)
    t)) ;; ie, handled

(export! control-do-action)

(defmethod tool-tip-show? (other)
  (declare (ignore other))
  nil)

(defmethod tool-tip (other)
  (declare (ignore other))
  nil)

(defmacro ct-action-lambda (&body body)
  `(lambda (self req)
     (declare (ignorable self req))
     ,@body))

(defmethod kb-selector (other) (declare (ignore other)) nil)

(defmethod enabled (other)(assert other) nil)

(defmethod do-cello-keydown ((self control) k event)
  (declare (ignorable event))
  (when (control-triggered-by self k event)
    (funcall (ct-action self) self event)
    t)) ;; handled

; ----------------------------------------------------------

(defmethod do-cello-keydown :around (self key-char event)
  (declare (ignorable key-char))
  (typecase self
    (null)
    (window (ctl-notify-keydown self self key-char event)
      (call-next-method))
    (otherwise
     (when (ctl-notify-keydown .parent self key-char event)
       (unless (call-next-method)
         (do-cello-keydown .parent key-char event))))))

(defmethod ctl-notify-keydown (self target key-char click)
  (ctl-notify-keydown (fm-parent self) target key-char click))



(defmethod control-triggered-by (control k event)
  (declare (ignorable event))
  (eql k (kb-selector control))) ;; this is lame--to be enhanced

(defmethod ctl-disabled (other)
  (declare (ignore other))
  nil)

(defmethod ctl-disabled ((self control))
  (not (enabled self)))

(export! fully-enabled)
(defmethod fully-enabled (self)
  (declare (ignore self))
  nil)

(defmethod fully-enabled ((self control))
  "Test if self and all ascendant controls are enabled"
  (labels ((no-disabled-up (node)
             (unless (ctl-disabled node)
               (bif (p (fm-parent node))
                 (no-disabled-up p)
                 t ;; reached top without hitting un-enabled control
                 ))))
    (no-disabled-up self)))


;
; /// m/b odd combo of customizable parameter 'controlAction and
;     generic function 'control-do-action. we like instance-oriented
;     programming, so keep 'controlAction, maybe just coordinate better
;     by establishing a rule: call 'controlAction first, if supplied,
;     and if it returns t that indicates "handled"? ugh
;
(defun control-do-action (ct trigger-evt &optional force)
  (when (ct-action ct)
    (if (or force (fully-enabled ct))
        (progn
          ;(trc "Control-do-action calling" ct trigger-evt)
          (clock :calling-ct-action)
          (funcall (ct-action ct) ct trigger-evt)
          (clock :called-ct-action)
          ;(trc "Control-do-action triggering FINIS" ct)
          )
      (when (enabled ct)
        (trc "control enabled but neither forced nor fully enabled, so not acting" ct )))))

(export! control-trigger)

(defun control-trigger (self &key even-if-disabled)
  ;;(when (mdead self) (bgo wtf?))
  (if (or even-if-disabled (^enabled))
      (progn
        (clock :control-trigger)
        (control-do-action self nil even-if-disabled)
        (clock :control-did-action))
    (trc "not actually triggering disabled" self)))


