(in-package :apropos-qx)

(defun serve-apropos ()
  (let ((port 8000))
    (when *wserver* (shutdown))
    (net.aserve:start :debug t :port port)
    (flet ((pfl (p f)
             (publish-file :port port
               :path p
               :file f))
           (pdr (p d)
             (publish-directory :port port
               :prefix p
               :destination d))
           (pfn (p fn)
             (publish :path p :function fn)))

      (pfl "/" "/devel/qooxlisp/ide/source/index.html")
      (pdr "/source/" "/devel/qooxlisp/ide/source/")
      (pdr "/script/" "/devel/qooxlisp/ide/source/script/")

;;;      (pfl "/" "/devel/qooxlisp/apropos/aproposqx/source/index.html")
;;;      (pdr "/source/" "/devel/qooxlisp/apropos/aproposqx/source/")
;;;      (pdr "/script/" "/devel/qooxlisp/apropos/aproposqx/source/script/")

;;;      (pfl "/" "/devel/qooxlisp/apropos/aproposqx/source/index.html")
;;;      (pdr "/source/" "/devel/qooxlisp/apropos/aproposqx/source/")
;;;      (pdr "/script/" "/devel/qooxlisp/apropos/aproposqx/source/script/")

      (pdr "/qx/" "/devel/qx/")
      (pfn "/begin" 'qx-begin)
      ;; wow (pfn "/execute" 'qx-execute)
      (pfn "/callback" 'qx-callback)
      (pfn "/getdata" 'qx-getdata)
      (pfn "/sortdata" 'qx-sortdata)
      (pfn "/getdatacount" 'qx-getdatacount))))

(defun qx-begin (req ent)
  (with-qx-js-response (req ent)
    (qx-reset)
    (print :beginning-request)
    (with-integrity ()
      (qxfmt "console.log('hi mom');")
      (make-instance 'apropos-document))
    (print `(:done-root-id ,*qxdoc*))))

(defmd apropos-document (qx-document)
  (sym-seg (c? (value (fm-other :symbol-string))))
  (sym-info (c? (b-when seg (^sym-seg)
                  (symbol-info-load seg))))
  :kids (c? (the-kids
             (make-kid 'qx-composite
               :add '(:left 0 :top 0 :width "100%" :height "100%")
               :layout (make-instance 'qx-vbox :spacing 6)
               :background-color  "#F5F5F5"
               :kids (c? (the-kids
                          (search-panel self)
                          (symbols-found self)))))))

(defobserver sym-info ()
  (with-integrity (:client `(:post-make-qx ,self))
    (let ((tbl (fm-other :sym-info-table)))
      (assert tbl)
      (when (oid (table-model tbl))
        (qxfmt "clDict[~a].reloadData();" (oid (table-model tbl)))))))

(defun symbols-found (self)
  (make-kid 'qx-composite
    :add '(:flex 1)
    :layout (c? (make-instance 'qx-vbox :spacing 6))
    :kids (c? (the-kids
               (make-kid 'qx-label
                 :add '(:flex 0)
                 :value "Symbols Found:")
               
               (make-kid 'qx-table
                 :md-name :sym-info-table
                 :add '(:flex 1)
                 :allow-grow-x t
                 :allow-grow-y t
                 :table-model (make-instance 'qx-table-model-remote
                                :column-name-ids '(("Symbol Name" name)
                                                   ("Package" pkg)
                                                   ("Function" fntype)
                                                   ("Setf" setf?)
                                                   ("Var" var?)
                                                   ("Class" class?)
                                                   ("Exp" exported?))))
               ))))

(defun search-panel (self)
  (make-kid 'qx-composite
    :add '(:flex 0)
    ;:allow-grow-x t
    :allow-grow-y :js-false
    :padding 4
    :layout (c? (make-instance 'qx-hbox
                  :align-y 'middle
                  :spacing 12))
    :kids (c? (the-kids
               (make-kid 'qx-label
                 :add '(:flex 0)
                 :value "String:")
               
               (make-kid 'qx-combo-box
                 :md-name :symbol-string
                 :add '(:flex 1)
                 :allow-grow-x t
                 :onchangevalue
                 (lambda (self req)
                   (let ((nv (req-val req "value")))
                     (setf (^value) nv)
                     (trc ":onchangevalue-fires" (oid self) nv)
                     (pushnew (make-kid 'qx-list-item
                                :label nv) (^kids)
                       :key 'label :test 'string-equal)
                     (trc ":onchangevalue-fired" (oid self) nv (length (^kids))
                       (mapcar 'label (^kids)))))
                 :onkeypress
                 (lambda (self req)
                   (let* ((key (req-val req "keyId"))
                          (jsv (req-val req "value"))
                          (v (cvtjs jsv)))
                     (setf (^value) (cond
                                     ((= 1 (length key))
                                      (conc$ v key))
                                     ((string-equal key "Backspace")
                                      (subseq v 0 (max 0 (1- (length v)))))
                                     (t v)))
                     (qxfmt "console.log('ackkeypress');" ))))
               
               (make-kid 'qx-button
                 :add '(:flex 0)
                 :label "Search"
                 :enabled (c? (trc nil "enabled rule sees" (value (psib)))
                            (> (length (value (psib))) 1))
                 :onexecute (lambda (self req)
                              (declare (ignorable req self))
                              (trc nil "sympart" (value (psib)))
                              (b-if sympart (value (psib))
                                (progn
                                  (print `(:sympart-onexec ,sympart))
                                  (setf (sym-seg (u^ qx-document)) sympart))
                                (qxfmt "alert('Disable me!!!')" ))))
               ))))




