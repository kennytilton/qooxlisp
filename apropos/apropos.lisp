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
  (sym-seg (c-in nil))
  (syms-unfiltered (c? (b-when seg (^sym-seg)
                         (symbol-info-raw seg))))
  (sym-info (c? (symbol-info-filtered (^syms-unfiltered)
                  (value (fm-other :type-filter))
                  (value (fm-other :exported-only))
                  (value (fm-other :all-packages))
                  (value (fm-other :selected-pkg)))))
  :kids (c? (the-kids
             (make-kid 'qx-composite
               :add '(:left 0 :top 0 :width "100%" :height "100%")
               :layout (make-instance 'qx-vbox :spacing 6)
               :background-color  "#F5F5F5"
               :kids (c? (the-kids
                          (search-panel self)
                          (make-kid 'qx-composite
                            :padding 6
                            :layout (c? (make-instance 'qx-hbox :spacing 6))
                            :kids (c? (the-kids
                                       (exported-pkg-filter self)
                                       (type-filter self))))
                          (symbols-found self)))))))

(defobserver sym-info ()
  (with-integrity (:client `(:post-make-qx ,self))
    (let ((tbl (fm-other :sym-info-table)))
      (assert tbl)
      (when (oid (table-model tbl))
        (qxfmt "clDict[~a].reloadData();" (oid (table-model tbl)))))))

(defun exported-pkg-filter (self)
  (make-kid 'qx-composite
    :add '(:flex 1)
    :layout (c? (make-instance 'qx-vbox :spacing 6))
    :kids (c? (the-kids
               (make-kid 'qx-check-box
                 :md-name :exported-only
                 :label "Exported symbols only")
               (make-kid 'qx-composite
                 :layout (c? (make-instance 'qx-vbox :spacing 2))
                 :kids (c? (the-kids
                            (make-kid 'qx-label
                              :value "Packages to search:")
                            (make-kid 'qx-composite
                              :layout (c? (make-instance 'qx-hbox :spacing 2))
                              :kids (c? (the-kids
                                         (make-kid 'qx-check-box
                                           :md-name :all-packages
                                           :value (c-in nil)
                                           :label "All")
                                         (make-kid 'qx-select-box
                                           :md-name :selected-pkg
                                           :enabled (c? (not (value (fm-other :all-packages))))
                                           :kids (c? (the-kids
                                                      (b-if syms (syms-unfiltered (u^ qx-document))
                                                        (loop with pkgs
                                                            for symi in syms
                                                            do (pushnew (symbol-info-pkg symi) pkgs)
                                                            finally (return (loop for pkg in pkgs
                                                                                collecting
                                                                                  (make-kid 'qx-list-item
                                                                                    :model (package-name pkg)
                                                                                    :label (package-name pkg)))))
                                                        (loop for pkg in (subseq (list-all-packages) 0 10)
                                                            collecting
                                                              (make-kid 'qx-list-item
                                                                :model (package-name pkg)
                                                                :label (package-name pkg)))))))))))))))))


(defun type-filter (self)
  (make-kid 'qx-composite
    :layout (c? (make-instance 'qx-vbox :spacing 6))
    :kids (c? (the-kids
               (make-kid 'qx-label
                 :value "Show:")
               
               (make-kid 'qx-radio-button-group
                 :md-name :type-filter
                 :value (c-in :all)
                 
                 :layout (make-instance 'qx-grid :spacing-x 12 :spacing-y 6)
                 :kids (c? (the-kids
                            (make-kid 'qx-radio-button
                              :add '(:row 0 :column 0)
                              :label "All" :model 'all)
                            (make-kid 'qx-radio-button
                              :add '(:row 0 :column 1)
                              :label "Variables" :model 'var)
                            (make-kid 'qx-radio-button
                              :add '(:row 1 :column 0)
                              :label "Functions" :model 'fn)
                            (make-kid 'qx-radio-button
                              :add '(:row 1 :column 1)
                              :label "Classes" :model 'class))))))))

(defun symbols-found (self)
  (make-kid 'qx-composite
    :add '(:flex 1)
    :layout (c? (make-instance 'qx-vbox :spacing 6))
    :kids (c? (the-kids
               (make-kid 'qx-label
                 ;;:add '(:flex 0)
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
                                                   ("Exp" exported?))))))))

(defun search-panel (self)
  (make-kid 'qx-composite
    :allow-grow-y :js-false
    :padding 4
    :layout (c? (make-instance 'qx-hbox
                  :align-y 'middle
                  :spacing 12))
    :kids (c? (the-kids
               (make-kid 'qx-label
                 :value "String:")
               
               (make-kid 'qx-combo-box
                 :md-name :symbol-string
                 :add '(:flex 1)
                 :allow-grow-x t
                 :onchangevalue (lambda (self req)
                                  (let ((sympart (req-val req "value")))
                                    (setf (sym-seg (u^ qx-document)) sympart)))
                 :onkeypress (lambda (self req)
                               (let* ((key (req-val req "keyId"))
                                      (jsv (req-val req "value"))
                                      (v (cvtjs jsv)))
                                 (setf (^value) (cond
                                                 ((= 1 (length key))
                                                  (conc$ v key))
                                                 ((string-equal key "Backspace")
                                                  (subseq v 0 (max 0 (1- (length v)))))
                                                 (t v)))
                                 (qxfmt "console.log('ackkeypress');" )))
                 :kids (c? (let ((sympart (sym-seg (u^ qx-document))))
                             (if (plusp (length sympart))
                                 (pushnew (make-kid 'qx-list-item
                                            :label sympart) .cache
                                   :key 'label :test 'string-equal)
                             .cache))))
                                 
               
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



