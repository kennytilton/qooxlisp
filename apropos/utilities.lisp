(in-package :cqx)

(defun k-word (s)
  (when s (if (consp s) (mapcar 'k-word s)
            (intern (string-upcase s) :keyword))))

(defmacro pqhtml (&body body)
  `(catch 'excl::printer-error
     (net.html.generator:html ,@body)))

(defun req-val (req tag)
  (net.aserve:request-query-value tag req))

(defmacro mrq^ (form)
  `(make-server-request (req$ ',form)))

(defmacro mrq (form)
  `(make-server-request (req$ '(prog1 nil ,form))))

(defmacro wsv (&rest x)
  `(net.aserve:websession-variable ws (ns ,@x)))

(defmacro nsfmt (f ns)
  `(format nil ,f ,ns))

(defmacro nsfmt^ (f)
  `(nsfmt ,f ns))

(defmacro wsv^ (&rest x)
  `(wsv ns ,@x))

(defun ns (&rest x)
  (format nil "~(~{~a~^-~}~)" x))

(defun pqsym (x) (intern (string-upcase x) :pq))

(defmacro ns^ (&rest x)
  `(ns ns ,@x))

(defmacro with-session (&body body)
  `(let* ((ws (net.aserve:websession-from-req req))
          (ns (req-val req "ns")))
     (declare (ignorable ws ns))
     ,@body))

(defmacro with-response (&body body)
  `(prog1 nil
     (net.aserve:with-http-response (req ent)
       (net.aserve:with-http-body (req ent)
         (let* ((ws (net.aserve:websession-from-req req))
                (ns (req-val req "ns")))
           (declare (ignorable ws ns))
           ,@body)))))

(defmacro with-plain-text-response (&body body)
  `(prog1 nil
     (net.aserve:with-http-response (req ent :content-type "text/plain")
       (net.aserve:with-http-body (req ent)
         (let* ((ws (net.aserve:websession-from-req req))
                (ns (req-val req "ns")))
           (declare (ignorable ws ns))
           ,@body)))))

(defmacro with-html-response (&body body)
  `(prog1 nil
     (net.aserve:with-http-response (req ent :content-type "text/html")
       (net.aserve:with-http-body (req ent)
         (let* ((ws (net.aserve:websession-from-req req))
                (ns (req-val req "ns")))
           (declare (ignorable ws ns))
           ,@body)))))

(defmacro with-typed-response ((type) &body body)
  `(prog1 nil
     (net.aserve:with-http-response (req ent :content-type ,type)
       (net.aserve:with-http-body (req ent)
         (let* ((ws (net.aserve:websession-from-req req))
                (ns (req-val req "ns")))
           (declare (ignorable ws ns))
           ,@body)))))

(defmacro with-js-response (&body body)
  `(prog1 nil
     (net.aserve:with-http-response (req ent :content-type "text/javascript")
       (net.aserve:with-http-body (req ent)
         (let* ((ws (net.aserve:websession-from-req req))
                (ns (req-val req "ns")))
           (declare (ignorable ws ns))
           ,@body)))))

(defmacro with-json-response (&body body)
  `(prog1 nil
     (net.aserve:with-http-response (req ent :content-type "application/json")
       (net.aserve:with-http-body (req ent)
         (let* ((ws (net.aserve:websession-from-req req))
                (ns (req-val req "ns")))
           (declare (ignorable ws ns))
           ,@body)))))


(defmacro ml$ (&rest x)
  (let ((s (gensym)))
    `(with-output-to-string (,s)
       (net.html.generator:html-stream ,s
         ,@x))))

(defun js-prep (&rest lists)
  (format nil "(~{~a~})"
    (loop for list in lists
        collecting (format nil "(~{~(~a~)~^ {~a}~})" list))))

(define-symbol-macro nskey
    (k-word ns))

(defun ws-ns-key (ws ns)
  (conc$ (net.aserve:websession-key ws) ns))

(define-symbol-macro wskey
    (ws-ns-key ws ns))

(defun req$ (symbolic-form) 
  (let ((r (substitute #\space #\newline
             (let ((*print-readably* t))
               (prin1-to-string symbolic-form )))))
    (let ((bad (position #\# r)))
      (if bad
          (progn
            (mprint `(bad form!!!!!! ,symbolic-form))
            (mprint `(bad r!!!! ,r))
            (mprint `(trouble!!!!!!!!!!!!!!!!!!!!!! ,bad))
            (mprint `(starting ,(subseq r bad (+ bad 32)))))
        #+shhh (mprint `(no sharps in command))))
    r))

(defun mrqe^ (form &optional show?)
  (let ((r$ (req$ form)))
    (when show?
      (let ((*print-level* 10))
        (mprint `(form ,form))
        (mprint `(request ,r$))))
    (make-server-request r$)))

(defun mrqe (form &optional show?)
  (let ((r$ (req$ `(prog1 nil ,form))))
    (when show?
      (let ((*print-level* 10))
        (mprint `(form ,form))
        (mprint `(request ,r$))))
    (make-server-request r$)))

;;;#+test
;;;(time (set-up-remote-image))
;;;

(defun set-up-remote-image () ;; 3msec if already done, so we do it many places
  (unless (make-server-request
           (req$ '(boundp '*ws-stores*)))
    (actually-set-up-remote-image-working)))

#+test
(actually-set-up-remote-image)

#+test
(actually-set-up-remote-image-working)

(defun actually-set-up-remote-image-working ()
  (assert (is-pq-running?))
  ;(mrq (load "../split-sequence/split-sequence.lisp"))
  ;(vim-backend-extend)
  )

#+test
(actually-set-up-remote-image-working)

(defvar *common-server*)
(defparameter *browser-backend-initialized* nil)
(defvar *websource-connection*)

(defun be-def (be-port &optional (be-host "localhost"))
  (setf *pq-host-name* be-host
    *pq-port-number* be-port))

(defun backend-connect (&key sp data-host dp websource-host ws-name wp)
  (assert (and sp dp))
  
  (be-def dp data-host)
  (handler-case
      (setf *websource-connection* (websource-connect ws-name :host websource-host :port wp))
    (t (c)
      (mprint `(:error-connecting-to-websource ,c))
      (setf *websource-connection* nil)
      nil))
  (if (is-pq-running?)
      (progn
        ;(actually-set-up-remote-image)
        (mrq (unless *browser-backend-initialized*
               (asp-backend-init)))
        (if (not (mrq^ *browser-backend-initialized*))
            (mprint "******************************************
******************************************
*******  ASP not started!!!!       *******
*******  Backend Initialization    *******
*******  is not happening.         *******
******************************************
******************************************")
          (progn
            (actually-set-up-remote-image-working)
            (setf *common-server* (net.aserve:start :port sp))
            (mprint "******************************************
******************************************
******** ASP successfully started  *******
******************************************
******************************************"))))
          (mprint "******************************************
******************************************
*******  ASP not started!!!!!!!!   *******
******************************************
******************************************

The PQ back end is not running.  Take these steps 
in a second Unix session on USNAM:

1. At the unix prompt on USNAM:
  $ alisp -I /usr/local/Images/DE-Test-08-28-Resource-Update-1.dxl
[Note: Any other image capable of running as a BE should do as well.]

2. At the resulting Lisp prompt:
  cl-user: (pq::start-pq-server :port 8000)
[Note: 8000 is different than the port the browser comes in by.]

3. Leave the Lisp image running in Session #2 and back in this
Unix session #1, confirm step #2 by evaluating at the Lisp prompt:
  cl-user: (is-pq-running?)
The output should be T. If not, check all work above.

4. Now try again with:
  cl-user: (asp-start) 
...to publish ASP on port 9000 or (asp-start NNNN) to use
some other port.

Now access ASP Browser from the Safari browser:
  http://localhost:9000 [or NNNN if 9000 not used in Step #4}.

[Scroll Up to see what went wrong (the data server is not running)]

"
      ))
    (values))

;;; ---- without-repeating ----------------------------------------------

;; Returns a function that generates an elements from ALL each time it
;; is called. When a certain element is generated it will take at
;; least DECENT-INTERVAL calls before it is generated again.  
;;
;; note: order of ALL is important for first few calls, could be fixed

(defun without-repeating-generator (decent-interval all)
  (let ((len (length all))
        (head (let ((v (shuffle all)))
                (nconc v v))))
    (lambda ()
      (if (< len 2)
          (car all)
        (prog2
          (rotatef (car head)
            (car (nthcdr (random (- len decent-interval))
                   head)))
            (car head)
          (setf head (cdr head)))))))

(defun shuffle (list &key (test 'identity))
  (if (cdr list)
      (loop thereis
            (funcall test
              (mapcar 'cdr
                (sort (loop for e in list collecting (cons (random most-positive-fixnum) e))
                  '< :key 'car))))
    (copy-list list)))

(defparameter *without-repeating-generators* nil)

(defun reset-without-repeating ()
  (if *without-repeating-generators*
      (clrhash *without-repeating-generators*)
    (setf *without-repeating-generators* (make-hash-table :test 'equalp))))

(defun without-repeating (key all &optional (decent-interval (floor (length all) 2)))
  (unless *without-repeating-generators*
    (reset-without-repeating))
  (funcall (or (gethash key *without-repeating-generators*)
             (progn
               (setf (gethash key *without-repeating-generators*)
                 (without-repeating-generator decent-interval all))))))

;; already ported to JS:
#+teststststs
(defun parse-1 ()
  (mprint (let* ((s (with-output-to-string (j)
              (net.aserve::html-stream j
                (pqhtml 
                 (:princ
                  (list '(vim-items)
                    (list 'client-sel-id
                      (format nil "~s"
                        (with-output-to-string (s)
                          (net.aserve::html-stream s
                            (:div (:p "hi")(:br)(:p "mom ('is') mom"))
                            )))))))))))
    (mprint s)
    
    (loop with state = 'find-start
        with pairs = (make-array 0 :adjustable t :fill-pointer t)
        and car-start and car-end and cdr-start and cdr-end
        for ch across s
        for cn upfrom 0
        do ;(mprint (list state ch))
          (case state
            (find-start (when (eql ch #\()
                          (setf state 'find-sub-start)))
            (find-sub-start 
             (cond
              ((eql ch #\()
               (setf car-start (1+ cn)
                 state 'find-car-end))
              ((eql ch #\)) (loop-finish))))
            (find-car-end (cond
                           ((eql ch #\space) (setf car-end cn
                                               state 'find-cdr-start))
                           ((eql ch #\)) (setf car-end cn
                                           state 'collect-sub))))
            (find-cdr-start (when (eql ch #\")
                              (setf cdr-start (1+ cn) ; tricky
                                state 'find-cdr-end)))
            (find-cdr-end (cond
                           ((eql ch #\\) (setf state 'ignore-car))
                           ((eql ch #\") (setf cdr-end cn
                                           state 'collect-sub))))
            (ignore-car (setf state 'find-cdr-end))
            (collect-sub 
             (vector-push-extend (vector (subseq s car-start car-end)
                                   (when cdr-start
                                     (subseq s cdr-start cdr-end))) 
               pairs)
             (setf car-start nil car-end nil cdr-start nil cdr-end nil)
             (setf state 'find-sub-start)))
          finally (return pairs)))))

