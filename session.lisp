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
  
  (with-integrity (:client `(:post-make-qx ,self))
    (loop for k in (set-difference old-value new-value)
        do (qxfmt "clDict[~a].remove(clDict[~a]);" (oid self)(oid k)))
    (loop for k in (set-difference new-value old-value)
        do 
          ;(qxfmt "consolelog('adding: to '+ ~a + ' the new ' + ~a);" pa new)
          (assert (oid k) () "No OID for k ~a of fam ~a" k self)
          (b-if ao (add-ops k)
            (qxfmt "clDict[~a].add(clDict[~a],~a);" (oid self) (oid k) ao)
            (qxfmt "clDict[~a].add(clDict[~a]);" (oid self) (oid k))))))

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
  )

(defmethod oid :around (self)
  (or (call-next-method)
    (progn
      (describe self)
      (warn "null oid from ~a oid-sv ~a ascendants ~a " self (slot-value self 'oid) (parentage self))
      nil)))

(defun parentage (self)
  (when self
    (cons (fm-parent self)
      (parentage (fm-parent self)))))

(defmethod initialize-instance :after ((self qxl-session) &key)
  (assert (null (gethash (session-id self) *qx-sessions*)))
  (setf (gethash (session-id self) *qx-sessions*) self))

(export! .focus .focused *web-session* ^session engine browser)

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


(defun session-focus (req ent)
  ;; this guy handles focusOn event from qooxdoo so it is cool to setf the focus
  (trcx :session-focus-entry!!!!!!! (req-val req "sessId") (req-val req "oid"))
  (with-js-response (req ent)
    (with-integrity ()
      (b-when session (b-if sessId (parse-integer (req-val req "sessId") :junk-allowed t)
                        (or (gethash sessId *qx-sessions*)
                          (dfail "session-focus: Unknown session ID ~a in ~s" sessId (rq-raw req)))
                        (dfail "session-focus: No sessId parameter: ~s" (rq-raw req)))
        (b-when new-focus (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                            (or (gethash oid (dictionary session))
                              (dfail "session-focus: oid ~s not in dictionary" oid))
                            (dfail "session-focus: No oid parameter: ~s" (rq-raw req)))
          (trcx :focusOn-sets-session-focus session new-focus)
          (setf (focus session) new-focus))))))

(export! qx-callback-js qx-callback-json make-qx-instance) ;;>>> maybe not once start-up inherits

(defun qx-callback-js (req ent)
  (let ((*ekojs* nil)) ;; qx-callback-js
    (with-js-response (req ent) 
      (top-level.debug::with-auto-zoom-and-exit ("aa-callback-js.txt" :exit nil)
        (b-if *web-session* (b-if sessId (parse-integer (req-val req "sessId") :junk-allowed t)
                              (gethash sessId *qx-sessions*)
                              (warn "Invalid sessId parameter ~s in callback req: ~a" (req-val req "sessId")
                                (list (req-val req "opcode") (req-val req "oid"))))
          (b-if self (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                       (gethash oid (dictionary *web-session*))
                       (warn "Invalid oid parameter ~s in callback req: ~a" (req-val req "oid")
                         (list (req-val req "sessId")(req-val req "opcode"))))
            (let ((opcode (qxl-sym (req-val req "opcode"))))
              ;(trcx :s *web-session* :callback opcode :self self :req (request-raw-request req))
              (with-integrity ()
                (b-if cb (funcall opcode self)
                  (funcall cb self req)
                  (dwarn "Widget ~a oid ~a in session ~a has no handler for ~a callback " self (oid self) (session-id *web-session*) opcode))))
            (dwarn "Widget not found for oid ~a in session ~a for ~a callback" (oid self) (session-id *web-session*) (req-val req "opcode")))
          (dwarn "Unknown session ID ~a in callback: ~a" (req-val req "sessId") 
            (list (req-val req "opcode") (req-val req "oid"))))))))

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
