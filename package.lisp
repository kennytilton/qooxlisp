(eval-when (compile load eval)
  (require :aserve)
  (require :webactions)
  (require :pxml)
  )

(defpackage #:qooxlisp
  (:nicknames :qxl)
  (:use #:cells #:utils-kt #:cl #:excl)
  (:export #:*qxdoc* #:oid
    #:k-word #:whtml #:req-val
    #:mrq^ #:mrq #:wsv #:nsfmt
    #:nsfmt^ #:wsv^ #:ns #:pqsym
    #:ns^ #:with-session #:with-response
    #:with-plain-text-response
    #:with-html-response
    #:with-typed-response
    #:with-js-response #:with-qx-js-response
    #:with-json-response #:table-model
    #:json$ #:jsk$ #:cvtjs
    #:qx-document #:qx-callback
    #:qx-composite #:qx-table-model-remote
    #:qx-combo-box #:qx-table
    #:qx-button #:qx-label #:label
    #:qx-hbox #:qx-vbox #:qx-list-item
    #:qxfmt #:qx-reset))

#+xxx
(unintern 'apropos-qx::cvtjs :apropos-qx)

