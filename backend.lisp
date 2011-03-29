(in-package :qxl)

;;;; Backend protocol so both hunchentooth and allegroserve will work

(defvar *backend*)

(defclass qooxlisp-server-backend ()
  ())

(defvar *default-backend-classname* nil)

(defun make-backend ()
  (make-instance *default-backend-classname*))

(defun initialize-backend ()
  (setf *backend* (make-backend)))

(defgeneric start-backend (backend &key port))

;;TODO more generic fns
