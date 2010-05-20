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

      (pfl "/" "/devel/qooxlisp/apropos/aproposqx/build/index.html")
      (pdr "/build/" "/devel/qooxlisp/apropos/aproposqx/build/")
      (pdr "/script/" "/devel/qooxlisp/apropos/aproposqx/build/script/")

;;;      (pfl "/" "/devel/qooxlisp/apropos/aproposqx/source/index.html")
;;;      (pdr "/source/" "/devel/qooxlisp/apropos/aproposqx/source/")
;;;      (pdr "/script/" "/devel/qooxlisp/apropos/aproposqx/source/script/")

      (pdr "/qx/" "/devel/qx/")
      (pfn "/begin" 'qx-begin)
      ;; wow (pfn "/execute" 'qx-execute)
      (pfn "/callback" 'qx-callback))))

(defun cvtjs (x)
  (cond
   ((string-equal x "null") nil)
   ((string-equal x "true") t)
   ((string-equal x "false") nil)
   (t x)))

(defun qx-begin (req ent)
  (with-qx-js-response (req ent)
    (qx-reset)
    (print :beginning-request)
    (with-integrity ()
      (qxfmt "console.log('hi mom');")
      (make-instance 'qx-document
        :kids (c? (the-kids
                   (make-kid 'qx-composite
                     :add '(:left 0 :top 0 :width "100%" :height "100%")
                     :layout (c? (make-instance 'qx-vbox
                                   ;;:align-y 'middle
                                   :spacing 6))
                     :background-color "#F5F5F5"
                     :kids (c? (the-kids
                                (make-kid 'qx-composite
                                  :add '(:flex 1)
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
                                               :add '(:flex 1)
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
                                                              (qxfmt "alert('Searching for symbols like ~a')" (value (psib)))
                                                              (qxfmt "alert('Disable me!!!')" ))))
                                             )))
                                )))
                   
                   ))))
    (print `(:done-root-id ,*qxdoc*))))

(defun search-elements (self)
  (make-kid 'qx-composite
    :add '(:left 200 :top 100) ;; "{left: 100, top: 50}"
    :layout (c? (make-instance 'qx-hbox :spacing 6))
    :kids (c? (the-kids
               (make-kid 'qx-label
                 :add '(:flex 0)
                 :value "String:")
               (make-kid 'qx-combo-box
                 :add '(:flex 1)
                 :onchangevalue (lambda (self req)
                                  (print :onchangevalue-fires)
                                
                                  (let ((nv (req-val req "value")))
                                    (setf (^value) nv)
                                    (trc "alert('thingy ~a changed to ~a')" (oid self) nv))))
               (make-kid 'qx-button
                 :add '(:flex 0)
                 :label "Search"
                 :onexecute (lambda (self req)
                              (declare (ignorable req self))
                              (b-if sympart (value (psib))
                                (qxfmt "alert('Searching for symbols like ~a')" (value (psib)))
                                (qxfmt "alert('Disable me!!!')" ))))))))



