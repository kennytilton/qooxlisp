;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or allegro lispworks cmu mcl clisp cormanlisp sbcl scl)
(progn
  (declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0))))

(asdf:defsystem :apropos
    :name "apropos"
  :author "Kenny Tilton <kentilton@gmail.com>"
  :maintainer "Kenny Tilton <kentilton@gmail.com>"
  :licence "MIT"
  :description "apropos"
  :long-description "demo/starter-app for qooxlisp"
  :version "1.0"
  :serial t
  :depends-on (:qooxlisp)
  :components ((:file "apropos")
               (:file "apropos-data")
               (:file "classic")
               (:file "makeover")
               (:file "rethought")))

(defmethod perform ((o load-op) (c (eql (find-system :cells))))
  (pushnew :cells *features*))
