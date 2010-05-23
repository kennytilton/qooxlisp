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
    #:qx-document #:qx-callback #:qx-select-box
    #:qx-composite #:qx-table-model-remote
    #:qx-combo-box #:qx-table #:qx-grid #:qx-radio-button-group #:qx-radio-button
    #:qx-button #:qx-label #:label
    #:qx-hbox #:qx-vbox #:qx-list-item #:qx-check-box
    #:qxfmt #:qx-reset))

#+xxx
(unintern 'apropos-qx::qx-select-box :apropos-qx)

