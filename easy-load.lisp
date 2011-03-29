
;; Hopefully an easy start..
;; Run this on the commandline:
;;
;; sbcl --load easy-load.lisp

(defvar *here* *load-truename*)

(progn
  (let ((base (namestring (make-pathname :directory
                                         (pathname-directory *here*)))))
    
    (flet ((add (relative)
             (pushnew (parse-namestring (format nil "~a/~a" base relative))
                      asdf:*central-registry*)))
      (mapcar #'add
              '(""
                "cl-json/"
                "../cells/"
                "../utils-kt/"
                "apropos/")))
    (assert (asdf:find-system :hunchentoot nil)
              nil
              "You need hunchentoot, install for example with quicklisp")
    (asdf:oos 'asdf:load-op :qooxlisp)
    (asdf:oos 'asdf:load-op :apropos)
    (eval (read-from-string "(qooxlisp::serve-apropos 8000)"))
    (format t "~%Look at localhost:8000 with your browser. If you have firefox, install firebug first.")))
