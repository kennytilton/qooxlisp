(in-package :qxl)

(defun k-word (s)
  (when s (if (consp s) (mapcar 'k-word s)
            (intern s :keyword))))

(let ((case (if (string= "x" (symbol-name 'x)) :modern :ansi)))
  (defun qxl-sym (s)
    (intern (ecase case (:modern s)(:ansi (string-upcase s))) :qxl)))

(defmacro whtml (&body body)
  `(catch 'excl::printer-error
     (net.html.generator:html ,@body)))

(defun req-val (req tag)
  (net.aserve:request-query-value tag req))

(defmacro with-plain-text-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "text/plain")
       (net.aserve:with-http-body (,req ,ent)
         (let* ((ws (net.aserve:websession-from-req ,req)))
           (declare (ignorable ws ns))
           ,@body)))))

(defmacro with-html-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "text/html")
       (net.aserve:with-http-body (,req ,ent)
         (let ((ws (net.aserve:websession-from-req ,req)))
           (declare (ignorable ws))
           ,@body)))))

(defparameter *js-response* nil)

(defmacro with-js-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "text/javascript")
       (net.aserve:with-http-body (,req ,ent)
         (let ((ws (net.aserve:websession-from-req ,req))
               (*qxdoc* (qxl-request-session ,req)))
           (declare (ignorable ws))
           #+notonbegin!!!
           (progn
             (assert *qxdoc* () "with-js-response sees no session ~a. Known: ~a"
               (req-val ,req "sessId") (loop for id being the hash-keys of qxl::*qx-sessions*
                                           collecting id))
             (assert (typep *qxdoc* 'qxl-session)))
           (setf *js-response* nil)
           ,@body
           (print `(responding ,*js-response*))
           (push *js-response* (responses *qxdoc*))
           (qxl:whtml (:princ (format nil "(function () {~a})()" (or *js-response* "null;")))))))))

(export! rq-raw)
(defun rq-raw (r) (request-raw-request req))

#+check
(print *js-response*)
(defmacro with-json-response ((req ent) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (,req ,ent :content-type "application/json")
       (net.aserve:with-http-body (,req ,ent)
         (let ((ws nil #+nahhh (net.aserve:websession-from-req ,req)))
           (declare (ignorable ws))
           ,@body)))))

(defun qxfmt (fs &rest fa)
  (progn ;; print 
   (setf *js-response*
     (conc$ *js-response* (apply 'format nil (conc$ "~&" fs "~%") fa)))))

(defun qxfmtd (fs &rest fa)
  (let ((x (apply 'format nil (conc$ "~&" fs "~%") fa)))
    (mprt :qxfmtd-adds x)
    (setf *js-response*
      (conc$ *js-response* x))))

(defmacro ml$ (&rest x)
  (let ((s (gensym)))
    `(with-output-to-string (,s)
       (net.html.generator:html-stream ,s
         ,@x))))

(defun js-prep (&rest lists)
  (format nil "(~{~a~})"
    (loop for list in lists
        collecting (format nil "(~{~(~a~)~^ {~a}~})" list))))

(defun json$ (x) (json:encode-json-to-string x))

(defun jsk$ (&rest plist)
  (json$ (loop for (a b) on plist by #'cddr
               collecting (cons a b))))

(defun cvtjs (x)
  (cond
   ((string-equal x "null") nil)
   ((string-equal x "true") t)
   ((string-equal x "false") nil)
   (t x)))

(defmacro mk-layout (model class &rest initargs) 
  "NOP for now"
  `(make-instance ,class
     :oid (get-next-oid (session ,model))
     ,@initargs))

#+xxxx
(jsk$ :left 2 :top 3)

#+test
(json$ (list (cons 'aa-bb t)))

(defmacro groupbox ((&rest layo-iargs)(&rest iargs) &rest kids)
  `(make-kid 'qx-group-box
     ,@iargs
     :layout (c? (mk-layout self 'qx-vbox ,@layo-iargs))
     :kids (c? (the-kids ,@kids))))

(defmacro checkgroupbox ((&rest layo-iargs)(&rest iargs) &rest kids)
  ;;; unfinished....
  `(make-kid 'qx-check-group-box
     ,@iargs
     :layout (c? (mk-layout self 'qx-vbox ,@layo-iargs))
     :kids (c? (the-kids ,@kids))))

(defmacro vbox ((&rest box-iargs)(&rest compo-iargs) &rest kids)
  `(make-kid 'qx-composite
     ,@compo-iargs
     :layout (c? (mk-layout self 'qx-vbox ,@box-iargs))
     :kids (c? (the-kids ,@kids))))

(defmacro hbox ((&rest box-iargs)(&rest compo-iargs) &rest kids)
  `(make-kid 'qx-composite
     ,@compo-iargs
     :layout (c? (mk-layout self 'qx-hbox ,@box-iargs))
     :kids (c? (the-kids ,@kids))))

(defmacro lbl (label-form &rest iargs)
  `(make-kid 'qx-label
     :value ,label-form
     ,@iargs))

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

(defmacro checkbox (model label &rest iargs)
  `(make-kid 'qx-check-box
     :md-name ,model
     :label ,label
     ,@iargs))

(defmacro selectbox (name (&rest iargs) &body kids)
  `(make-kid 'qx-select-box
    :md-name ,name
     ,@iargs
     :kids (c? (the-kids ,@kids))))

(defmacro qxlist (name (&rest iargs) &body kids)
  `(make-kid 'qx-list
    :md-name ,name
     ,@iargs
     :kids (c? (the-kids ,@kids))))

(defmacro combobox (name (&rest iargs) &rest kids)
  `(make-kid 'qx-combo-box
     :md-name ,name
     ,@iargs
     :onkeypress (lambda (self req)
                   (let* ((key (req-val req "keyId"))
                          (jsv (req-val req "value"))
                          (v (cvtjs jsv)))
                     (setf (^value) (cond
                                     ((= 1 (length key))
                                      (conc$ v key))
                                     ((string-equal key "Backspace")
                                      (subseq v 0 (max 0 (1- (length v)))))
                                     (t v)))))
     :kids (c? (the-kids ,@kids))))

(defmacro textfield (name &rest iargs)
  `(make-kid 'qx-text-field
     :md-name ,name
     ,@iargs))

(defmacro button (label (&rest iargs) &key onexec)
  `(make-kid 'qx-button
     :label ,label
     ,@iargs
     :onexecute (lambda (self req)
                  (declare (ignorable self req))
                  ,onexec)))
