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
      (pfl "/" "/devel/qooxlisp/apropos/aproposqx/source/index.html")
      (pdr "/source/" "/devel/qooxlisp/apropos/aproposqx/source/")
      (pdr "/script/" "/devel/qooxlisp/apropos/aproposqx/source/script/")

      (pdr "/qx/" "/qx/")
      (pfn "/begin" 'qx-begin)
      (pfn "/execute" 'qx-execute)
      (pfn "/callback" 'qx-callback))))

(defun qx-begin (req ent)
  (with-qx-js-response (req ent)
    (qx-reset)
    (print :beginning-request)
    (with-integrity ()
      (qxfmt "console.log('hi mom');")
      (make-instance 'qx-document
        :kids (c? (the-kids 
                   (make-kid 'qx-button
                     :add '(:left 200 :top 100)
                     :label "Knock-knock"
                     :onexecute (lambda (self req)
                                  (declare (ignorable req self))
                                  (qxfmt "alert('Hello, world!!! from oid ~a')" (oid self))))
                   #+chill
                   (make-kid 'qx-composite
                     :add '(:left 200 :top 100) ;; "{left: 100, top: 50}"
                     :layout (c? (make-instance 'qx-vbox :spacing 20))
                     :kids (c? (the-kids
                                (search-elements self)
                                (make-kid 'qx-label
                                  :add '(:flex 0)
                                  :value "Symbols Found:")
                                )))))))
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
                                    (qxfmt "alert('thingy ~a changed to ~a')" (oid self) nv))))
               (make-kid 'qx-button
                 :add '(:flex 0)
                 :label "Search"
                 :onexecute (lambda (self req)
                              (declare (ignorable req self))
                              (qxfmt "alert('Hello, world!!! from oid ~a')" (oid self))))))))

#|

      (cqx:whtml
       (:princ
        (let* ((b1 (make-instance 'qx-button
                     :on-execute "alert('Hello, world!!!')"))
              (js (format nil "
var doc = qx.core.ObjectRegistry.fromHashCode('~a');
var b1 = new qx.ui.form.Button('~a Button','aproposqx/test.png');
var bId = b1.toHashCode();
doc.add(b1, {left: 100, top: 50});
var rq = new qx.io.remote.Request('/rtnjsoid?oid=~:*~a&jsoid='+bId);
rq.send();
b1.addListener('execute', function(e) {
    var rq = new qx.io.remote.Request('/execute?oid=~:*~a','GET', 'text/javascript');
    rq.addListener('completed', function(r) {
       rq.dispose();
    });
    rq.send();
});" doc (oid b1))))
          (setf (gethash (oid b1) *qxcu*) b1)
          ;(print `(:js-send ,js))
          js))))))

|#

