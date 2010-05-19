(defpackage #:apropos-qx 
  (:use #:common-lisp
    #:qooxlisp
    #:cells
    #:utils-kt
    #:net.aserve))


(unintern 'apropos-qx::qx-callback :apropos-qx)