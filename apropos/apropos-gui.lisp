(in-package :apropos-qx)

(defun exported-pkg-filter (self)
  (vbox (:spacing 6)(:add '(:flex 1))
    (checkbox :exported-only "Show Exported Symbols Only")
    (groupbox (:spacing 2)(:legend "Package(s) to Search")
      (hbox (:spacing 20)()
        (checkbox :all-packages "All"
          :value (c-in t))
        (make-kid 'qx-select-box
          :md-name :selected-pkg
          :enabled (c? (not (value (fm-other :all-packages))))
          :kids (c? (the-kids
                     (b-if syms (syms-unfiltered (u^ qx-document))
                       (loop with pkgs
                           for symi in syms
                           do (pushnew (symbol-info-pkg symi) pkgs)
                           finally (return (loop for pkg in pkgs
                                               collecting
                                                 (make-kid 'qx-list-item
                                                   :model (package-name pkg)
                                                   :label (package-name pkg)))))
                       (loop for pkg in (subseq (list-all-packages) 0 10)
                           collecting
                             (make-kid 'qx-list-item
                               :model (package-name pkg)
                               :label (package-name pkg)))))))))))


(defun type-filter (self)
  (groupbox ()(:legend "Show")
    (radiobuttongroup :type-filter (:value (c-in :all))
      (qx-grid :spacing-x 12 :spacing-y 6)
      (radiobutton 'all "All"
        :add '(:row 0 :column 0))
      (radiobutton 'var "Variables"
        :add '(:row 0 :column 1))
      (radiobutton 'fn "Functions"
        :add '(:row 1 :column 0))
      (radiobutton 'class "Classes"
        :add '(:row 1 :column 1)))))

(defun symbols-found (self)
  (vbox (:spacing 6)
    (:add '(:flex 1))
    (lbl (c? (let ((sym-seg (sym-seg (u^ qx-document))))
               (if (plusp (length sym-seg))
                   (format nil "Symbols containing ~s" sym-seg)
                 "Symbols Found:"))))
    (make-kid 'qx-table
      :md-name :sym-info-table
      :add '(:flex 1)
      :allow-grow-x t
      :allow-grow-y t
      :table-model (make-instance 'qx-table-model-remote
                     :column-name-ids '(("Symbol Name" name)
                                        ("Package" pkg)
                                        ("Function" fntype)
                                        ("Setf" setf?)
                                        ("Var" var?)
                                        ("Class" class?)
                                        ("Exp" exported?))))))

(defun search-panel (self)
  (hbox (:align-y 'middle :spacing 12)
    (:allow-grow-y :js-false
      :padding 4)
    (lbl "String:")
    (make-kid 'qx-combo-box
      :md-name :symbol-string
      :add '(:flex 1)
      :allow-grow-x t
      :onchangevalue (lambda (self req)
                       (let ((sympart (req-val req "value")))
                         (setf (sym-seg (u^ qx-document)) sympart)))
      :onkeypress (lambda (self req)
                    (let* ((key (req-val req "keyId"))
                           (jsv (req-val req "value"))
                           (v (cvtjs jsv)))
                      (setf (^value) (cond
                                      ((= 1 (length key))
                                       (conc$ v key))
                                      ((string-equal key "Backspace")
                                       (subseq v 0 (max 0 (1- (length v)))))
                                      (t v)))
                      (qxfmt "console.log('ackkeypress');" )))
      :kids (c? (let ((sympart (sym-seg (u^ qx-document))))
                  (if (plusp (length sympart))
                      (if (find sympart .cache :key 'label :test 'string-equal)
                          .cache
                        (cons (make-kid 'qx-list-item
                                :label sympart) .cache))
                    .cache))))
    
    
    (make-kid 'qx-button
      :label "Search"
      :enabled (c? (trc nil "enabled rule sees" (value (psib)))
                 (> (length (value (psib))) 1))
      :onexecute (lambda (self req)
                   (declare (ignorable req self))
                   (trc nil "sympart" (value (psib)))
                   (b-if sympart (value (psib))
                     (progn
                       (print `(:sympart-onexec ,sympart))
                       (setf (sym-seg (u^ qx-document)) sympart))
                     (qxfmt "alert('Disable me!!!')" ))))
    ))


