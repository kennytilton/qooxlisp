;; -*- mode: Lisp; Syntax: Common-Lisp; Package: qooxlisp; -*-
#|

    session -- session management

(See package.lisp for license and copyright notigification)

|#

(in-package :qxl)

(defmd qooxlisp-family (family))

(defobserver .kids ((self qooxlisp-family))
  ;; the problem here is that as each item gets deleted from, say, a select box,
  ;; a changeSelection event fires. Might need to muck with qooxdoo itself to
  ;; grok setf of children. todo: m/b
  (let ((why cz::*observe-why*))
    (when new-value (assert (fm-parent (car new-value))() "pa 1"))
    (when new-value (assert (eq self (fm-parent (car new-value)))() "pa 1.a self ~a pa ~a" self (fm-parent (car new-value))))
    (with-integrity (:client `(:post-make-qx ,self))
      (when new-value 
        (unless (fm-parent (car new-value))
          ;(describe (car new-value))
          (trcx :no-new-par why new-value old-value)
          (trc "heritage self" (fm-heritage self))
          (break "no parent for kid ~a of self ~a" (car new-value) self)))
      (when new-value (assert (eq self (fm-parent (car new-value)))() "pa 2.a"))
      (loop for k in (set-difference old-value new-value)
          when (oid k) ;; possibly dumped very early? can't hurt
          do ;; (trcx :remove self k cz::*data-pulse-id*)
            (qxfmt "clDict[~a].remove(clDict[~a]);" (oid self)(oid k))
            (qxfmt "clDict[~a].dispose();" (oid k))
            )
      (loop for k in (set-difference new-value old-value)
          do 
            ;;(trcx :add self k cz::*data-pulse-id*)
            (unless (oid k)
              (trc ":no-oid!!!" why cz::*just-do-it-q* new-value old-value)
              (loop for p = (fm-parent k) then (fm-parent p)
                  while p do (trc "No OID parent" p (oid p)))
              (describe k)
              
              (error "null oid from ~a oid-sv ~a ascendants ~a justdoit: ~a observe-cuz: ~a"
                k (slot-value k 'oid) (parentage k) cz::*just-do-it-q* why)
              )
            (assert (oid k) () "No OID for k ~a of fam ~a" k self)
            
            (b-if ao (add-ops k)
              (qxfmt "clDict[~a].add(clDict[~a],~a);" (oid self) (oid k) ao)
              (qxfmt "clDict[~a].add(clDict[~a]);" (oid self) (oid k)))))))

(defmd qxl-session (focuser qooxlisp-family)
  (session-id (incf *session-ct*) :cell nil)
  (oid 0 :cell nil)
  (dictionary (make-hash-table) :cell nil)
  :registry? t
  (next-oid 1 :cell nil)
  (theme "qx.theme.Modern")
  (responses nil :cell nil)
  (focus (c-in nil))
  keyboard-modifiers ;; not sure if this holdover gets kept
  (engine nil :cell nil)
  (browser nil :cell nil)
  (stopped nil :cell nil)
  (touched (get-universal-time) :cell nil)
  (cb-timeout (lambda (self)(print `(,self :timeout))) :cell nil)
  )

(defun qxl-session-stopped ()
  (and *web-session* (stopped *web-session*)))


(defun qxl-session-stop (why)
  (if *web-session*
      (progn
        (setf (stopped *web-session*) why)
        (print `(:qxl-session-stopped-because ,why)))
    (print `(:qxl-session-stop-sees-no-*web-session* ,why))))

(defmethod oid :around (self)
  (assert self)
  (or (call-next-method)
    (progn
      ;;(describe self)
      (warn "null oid from ~a oid-sv ~a ascendants ~a justdoit: ~a observe-cuz: ~a"
        self (slot-value self 'oid) (parentage self) cz::*just-do-it-q* cz::*observe-why*)
      nil)))

(defun parentage (self)
  (when self
    (cons (fm-parent self)
      (parentage (fm-parent self)))))

(defmethod initialize-instance :after ((self qxl-session) &key)
  (assert (null (gethash (session-id self) *qx-sessions*)))
  (trc "new session!!!" (session-id self) self (hash-table-count *qx-sessions*))
  (setf (gethash (session-id self) *qx-sessions*) self))

(export! .focus .focused *web-session* ^session engine browser qxl-session-stopped qxl-session-stop)

(define-symbol-macro ^session (n^ qxl-session))
(define-symbol-macro .focus (focus ^session))
(define-symbol-macro .focused (^focused-on))

(defmethod session ((self qxl-session)) self)

(defmethod session (self)
  (u^ qxl-session))


(defmethod make-qx-instance :after ((self qxl-session))
  (qxfmt "
clDict[0] = qx.core.Init.getApplication().getRoot();
sessId=~a;" (session-id self)))

(defobserver theme ()
  (when new-value
    (qxfmt "qx.theme.manager.Meta.getInstance().setTheme(~a);" new-value)))

(defun qxl-request-session (req)
  (gethash (parse-integer (req-val req "sessId") :junk-allowed t) *qx-sessions*))

(defun get-next-oid (doc)
  (prog1
      (next-oid doc)
    (incf (next-oid doc))))

(export! session-focus)

(defparameter *untouched-max* 1800)

(defmacro watching-stopped (session-form &body body)
  (let ((session (gensym)))
    `(let ((,session ,session-form))
       (setf (touched ,session) (get-universal-time))
       (loop for sess being the hash-values of *qx-sessions*
           for touched = (touched sess)
           if (null touched) do (setf (touched sess) (get-universal-time))
           else when (> (- (get-universal-time) (touched sess)) *untouched-max*)
           do (trc "timeout" (get-universal-time) :vs (touched sess))
             (remhash (session-id sess) *qx-sessions*)
             (funcall (cb-timeout sess) sess)
             
             (not-to-be sess))
       (case (stopped ,session)
         ((nil)
          ,@body)
         (otherwise
          (qx-alert "server session has aborted. Please reload the page."))))))

(defun session-focus (req ent)
  ;; this guy handles focusOn event from qooxdoo so it is cool to setf the focus
  (with-js-response (req ent)
    (with-integrity ()
      (b-when *web-session* (b-if sessId (parse-integer (req-val req "sessId") :junk-allowed t)
                              (or (gethash sessId *qx-sessions*)
                                (dfail "sessionfocus: Unknown session ID ~a in ~s" sessId (rq-raw req)))
                              (dfail "sessionfocus: No sessId parameter: ~s" (rq-raw req)))
        (watching-stopped *web-session*
          (b-when new-focus (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                              (or (gethash oid (dictionary *web-session*))
                                (dfail "sessionfocus: oid ~s not in dictionary" oid))
                              (dfail "sessionfocus: No oid parameter: ~s" (rq-raw req)))
            ;(trcx session-focus new-focus (type-of new-focus))
            (setf (focus *web-session*) new-focus)))))))

(export! qx-callback-js qx-callback-json make-qx-instance qx-alert) ;;>>> maybe not once start-up inherits

(defun qx-callback-js (req ent)
  (let ((*ekojs* nil)) ;; qx-callback-js
    (with-js-response (req ent) 
      (top-level.debug::with-auto-zoom-and-exit ("aa-callback-js.txt" :exit nil)
        ;;(trc "qx-callback-js sees req" (req-val req "sessId") (req-val req "opcode") (req-val req "oid"))
        (let ((sessId (parse-integer (req-val req "sessId") :junk-allowed t)))
          (b-if *web-session* (if sessId
                                  (gethash sessId *qx-sessions*)
                                (warn "Invalid sessId parameter ~s in callback req: ~a" (req-val req "sessId")
                                  (list (req-val req "opcode") (req-val req "oid"))))
            (watching-stopped *web-session*
              (b-if self (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                           (gethash oid (dictionary *web-session*))
                           (warn "Invalid oid parameter ~s in callback req: ~a" (req-val req "oid")
                             (list (req-val req "sessId")(req-val req "opcode"))))
                (let ((opcode (qxl-sym (req-val req "opcode")))
                      (ukt::*stopped* 'qxl-session-stopped)
                      (cz::*c-stopper* 'qxl-session-stop))
                  ;;(trcx qx-callback-js *web-session*  opcode  self  (request-raw-request req))
                  (with-integrity ()
                    (b-if cb (funcall opcode self)
                      (funcall cb self req)
                      (dwarn "Widget ~a oid ~a in session ~a has no handler for ~a callback " self (oid self) (session-id *web-session*) opcode))))
                (dwarn "Widget not found for oid ~a in session ~a for ~a callback" (oid self) (session-id *web-session*) (req-val req "opcode"))))
            (flet ((do-warn ()
                     (setf (gethash sessId *warned-dead*) (get-universal-time))
                     (dwarn "Session not found for oid ~a" sessId )
                     (qx-alert "Algebra session no longer active. Please reload page")))
              (b-if warn-time (gethash sessId *warned-dead*)
                (when (> (- (get-universal-time) warn-time) 10)
                  (do-warn))
                (do-warn)))))))))
  
(defun qx-alert (s)
  (trcx qx-alert s )
  (qxfmt "alert('~a');" (js-escape s)))

(defun js-escape (s)
  (with-output-to-string (js)
    (loop for c across s
          when (char-equal c #\')
          do (princ "\\" js)
          do (princ c js))))

(export! gethtml)

(defun qx-callback-json (req ent)
  (with-integrity ()
    (with-json-response (req ent)
      (b-if *web-session* (b-if sessId (parse-integer (req-val req "sessId") :junk-allowed t)
                            (gethash sessId *qx-sessions*)
                            (error "Invalid sessId parameter ~s in callback req: ~a" (req-val req "sessId")
                              (list (req-val req "opcode") (req-val req "oid"))))
        (b-if self (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                     (gethash oid (dictionary *web-session*))
                     (error "Invalid oid parameter ~s in callback req: ~a" (req-val req "oid")
                       (list (req-val req "sessId")(req-val req "opcode"))))
          (funcall (qxl-sym (req-val req "opcode")) self req)
          (error "Widget not found for oid ~a in session ~a for ~a callback" (req-val req "oid")
            (session-id *web-session*) (req-val req "opcode")))
        (dwarn "Unknown session ID ~a in callback: ~a" (req-val req "sessId") 
          (list (req-val req "opcode") (req-val req "oid")))))))
