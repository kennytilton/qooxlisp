(in-package :qxl)

(defparameter *oid* 0)
(defparameter *cl-session* nil)
(defparameter *qxdoc* nil)

(defun qx-callback (req ent)
  (with-qx-js-response (req ent)
    (with-integrity ()
      (let* ((opcode (qxl-sym (req-val req "opcode")))
             (oid (parse-integer (req-val req "oid")))
             (self (gethash oid .dict)))
        ;;(print `(:callback2 ,opcode ,oid ,self))
        (bwhen (cb (funcall opcode self))
          (funcall cb self req))))))

(defun qx-reset ()
  (cells-reset 'qxl-user-queue-handler))

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