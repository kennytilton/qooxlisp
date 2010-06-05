(in-package :qxl)


(defparameter *qxdocs* nil)
(defparameter *qxdoc* nil)

(defun dwarn (&rest x)(print (apply 'format nil x)))

(defun qx-callback-js (req ent)
  (with-js-response (req ent) 
    (with-integrity ()
      (b-if session (b-if sessId (parse-integer (req-val req "sessId") :junk-allowed t)
                      (gethash sessId *qx-sessions*)
                      (warn "Invalid sessId parameter ~s in callback req: ~a" (req-val req "sessId")
                        (list (req-val req "opcode") (req-val req "oid"))))
        (b-if self (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                     (gethash oid (dictionary session))
                     (warn "Invalid oid parameter ~s in callback req: ~a" (req-val req "oid")
                       (list (req-val req "sessId")(req-val req "opcode"))))
          (let ((opcode (qxl-sym (req-val req "opcode"))))
            (mprt :callback opcode)
            (b-if cb (funcall opcode self)
              (funcall cb self req)
              (dwarn "Widget ~a oid ~a in session ~a has no handler for ~a callback " self (oid self) (session-id session) opcode)))
          (dwarn "Widget not found for oid ~a in session ~a for ~a callback" (oid self) (session-id session) (req-val req "opcode")))
        (dwarn "Unknown session ID ~a in callback: ~a" (req-val req "sessId") 
          (list (req-val req "opcode") (req-val req "oid")))))))

(defun qx-callback-json (req ent)
  (with-integrity ()
    (with-json-response (req ent)
      (b-if session (b-if sessId (parse-integer (req-val req "sessId") :junk-allowed t)
                      (gethash sessId *qx-sessions*)
                      (warn "Invalid sessId parameter ~s in callback req: ~a" (req-val req "sessId")
                        (list (req-val req "opcode") (req-val req "oid"))))
        (b-if self (b-if oid (parse-integer (req-val req "oid") :junk-allowed t)
                     (gethash oid (dictionary session))
                     (warn "Invalid oid parameter ~s in callback req: ~a" (req-val req "oid")
                       (list (req-val req "sessId")(req-val req "opcode"))))
          (funcall (qxl-sym (req-val req "opcode")) self req)
          (dwarn "Widget not found for oid ~a in session ~a for ~a callback" (oid self) (session-id session) (req-val req "opcode")))
        (dwarn "Unknown session ID ~a in callback: ~a" (req-val req "sessId") 
          (list (req-val req "opcode") (req-val req "oid")))))))

(defun qx-reset ()
  (cells-reset 'qxl-user-queue-handler)
  (setf *qx-sessions* (make-hash-table)))

(defparameter *qxl-client-task-priority*
    '(:make-qx :layout :post-make-qx))

(defun qxl-user-queue-handler (user-q)
  (loop for (defer-info . nil) in (fifo-data user-q)
      unless (find (car defer-info) *qxl-client-task-priority*)
        do (error "unknown qxl client task type ~a in task: ~a " (car defer-info) defer-info))

  (loop for (defer-info . task) in (prog1
                                       (stable-sort (fifo-data user-q) 'qxl-user-queue-sort :key 'car)
                                     (fifo-clear user-q))
        do
        ;(trc "!!! --- qxl-user-queue-handler dispatching" defer-info)
        (funcall task :user-q defer-info)))

(defun qxl-user-queue-sort (task1 task2)
  "Intended for use as user queue sorter, to make qxl happy by giving it stuff in the order it needs to work properly."
  (destructuring-bind (type1 self1 &rest dbg) task1
      (declare (ignorable dbg))
      (destructuring-bind (type2 self2 &rest dbg) task2
        (declare (ignorable dbg self1 self2))
        (let ((p1 (position type1 *qxl-client-task-priority*))
              (p2 (position type2 *qxl-client-task-priority*)))
          (cond
           ((< p1 p2) t)
           ((< p2 p1) nil)
           (t nil #+nahhh (fm-ordered-p self1 self2)))))))