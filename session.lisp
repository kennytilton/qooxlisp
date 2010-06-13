(in-package :qxl)

(defmd qooxlisp-family (family))

(defobserver .kids ((self qooxlisp-family))
  ;; the problem here is that as each item gets deleted from, say, a select box,
  ;; a changeSelection event fires. Might need to hack qooxdoo itself to
  ;; grok setf of children. todo: m/b
  
  (with-integrity (:client `(:post-make-qx ,self))
    ;(print (list "kidsing!!!!!!!!!!!!!!!!!" self old-value new-value))
    (loop for k in (set-difference old-value new-value)
        do (qxfmt "clDict[~a].remove(clDict[~a]);" (oid self)(oid k)))
    (loop for k in (set-difference new-value old-value) do 
          ;;(qxfmt "consolelog('adding: to '+ ~a + ' the new ' + ~a);" pa new)
          (b-if ao (add-ops k)
            (qxfmt "clDict[~a].add(clDict[~a],~a);" (oid self) (oid k) ao)
            (qxfmt "clDict[~a].add(clDict[~a]);" (oid self) (oid k))))))

(defparameter *session-ct* 0)
(defparameter *qx-sessions* (make-hash-table))

(defmd qxl-session (qooxlisp-family)
  (session-id (incf *session-ct*) :cell nil)
  (oid 0 :cell nil)
  (dictionary (make-hash-table) :cell nil)
  (next-oid 1 :cell nil)
  (theme "qx.theme.Modern")
  (responses nil :cell nil)
  (focus (c-in nil))
  )

(defmethod initialize-instance :after ((self qxl-session) &key)
  (assert (null (gethash (session-id self) *qx-sessions*)))
  (setf (gethash (session-id self) *qx-sessions*) self))

(export! .focus .focused)
(define-symbol-macro .focus (focus (n^ qxl-session)))
(define-symbol-macro .focused (eq self (focus (n^ qxl-session))))

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
          (setf (focus session) new-focus))))))
