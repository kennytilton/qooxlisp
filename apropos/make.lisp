;;
;; following assumes PQ/Source/nuviaghx is pwd on Unix, NOT <same>/Common
;;
(eval-when (compile load eval)
  (require :asdf))

(in-package :user)

(defun register-common (p)
  (unless (find (namestring p) (butlast asdf:*central-registry*)
            :test 'string-equal :key 'namestring)
    (push p asdf:*central-registry*)))

(setf asdf:*central-registry* (last asdf:*central-registry*))

(print `(load pathname ,*load-pathname*))
(print `(load pathname dirs ,(pathname-directory *load-pathname*)))

(register-common (path-pathname *load-pathname*))

(loop with root = (make-pathname
                   :directory (butlast (pathname-directory *load-pathname*) 2))
    for kw in '(:utils-kt :cells :cl-json :qooxlisp)
    do (register-common
        (merge-pathnames
         (make-pathname :directory `(:relative ,(string-downcase (symbol-name kw))))
         root)))

(loop for p in asdf:*central-registry*
      do (print `(asdf-registered ,p)))

(defun oos-load (x &optional force)
  (asdf:oos 'asdf:load-op x :force force))

#+test
(cl-user::oos-load :apropos t)

(print "Now (user::oos-load :apropos) and (qxl::serve-apropos)")
