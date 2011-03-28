;;;
;;; Copyright (c) 1995,2010 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

#+allegro
(eval-when (compile load eval)
  (require :aserve)
  (require :webactions)
  (require :pxml))

(defpackage #:qooxlisp
  (:nicknames :qxl)
  #-allegro
  (:use #:cells #:utils-kt #:cl)
  #+allegro
  (:use #:cells #:utils-kt #:cl #:excl #:net.aserve)  
  (:export #:oid
    #:k-word #:whtml #:req-val
    #:with-plain-text-response
    #:with-html-response #:qxl-request-session
    #:with-js-response #:mk-layout
    #:with-json-response #:table-model
    #:json$ #:jsk$ #:cvtjs #:session-id
    #:qxl-session #:qx-callback #:qx-select-box
    #:qx-composite #:qx-table-model-remote
    #:qx-combo-box #:qx-table #:qx-grid #:qx-radio-button-group #:qx-radio-button
    #:qx-button #:qx-label #:label #:button #:qx-html #:qx-html-math
    #:qx-hbox #:qx-vbox #:qx-list-item #:qx-check-box #:combobox
    #:qxfmt #:qx-reset #:vbox #:hbox #:lbl #:radiobuttongroup #:radiobutton #:checkbox
    #:groupbox #:checkgroupbox #:radiogroupbox #:selectbox))

#+adhoc
(unintern 'mathx::qx-html-math :mathx)



