;; -*- mode: Lisp; Syntax: Common-Lisp; Package: qooxlisp; -*-
#|

    qooxlisp -- cells and callbacks

(See package.lisp for license and copyright notigification)

|#

(in-package :qxl)


(defun dwarn (&rest x)(print (apply 'format nil x))(values))

(defun dfail (&rest x) (apply 'error x))

(defparameter *session-ct* 0)

(defvar *web-session*)

(defparameter *qx-sessions* (make-hash-table))

(defun qx-reset ()
  (cells-reset 'qxl-user-queue-handler)
  (setf *qx-sessions* (make-hash-table)))

(defparameter *qxl-client-task-priority*
    '(:make-qx :layout :post-make-qx :post-assembly))

(defun qxl-user-queue-handler (user-q)
  #+qxldebug
  (loop for (defer-info . nil) in (fifo-data user-q)
      unless (find (car defer-info) *qxl-client-task-priority*)
        do (error "unknown qxl client task type ~a in task: ~a " (car defer-info) defer-info))

  (loop for (defer-info . task) in (prog1
                                       (stable-sort (fifo-data user-q) 'qxl-user-queue-sort :key 'car)
                                     (fifo-clear user-q))
        do
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