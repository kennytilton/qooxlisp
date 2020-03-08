(in-package :qooxlisp)

;;; The top-level apropos structure, which defines
;;; slots for key data items and also constitutes
;;; the root of the GUI framework

(defmd apropos-classic (apropos-variant)
  :kids (c? (the-kids
             (vbox (:spacing 6) 
               (:add '(:left 0 :top 0 :width "100%" :height "100%")
                 :padding 6)
               (search-panel self)
               (hbox (:spacing 6)()
                 (vbox (:spacing 6)(:add '(:flex 1))
                   ;;(checkbox :exported-only "Show Exported Symbols Only")
                   (pkg-filter self))
                 (type-filter self))
               (vbox (:spacing 6)(:add '(:flex 1))
                 (lbl (c? (let ((sym-seg (sym-seg (u^ apropos-variant))))
                            (if (plusp (length sym-seg))
                                (format nil "Symbols containing ~s" sym-seg)
                              "Symbols Found:"))))
                 (symbols-found self))))))

;;; The top row of the dialogue, where one specifies the search substring.
;;; The combo-box used to hold prior matches is overkill but let's us
;;; show off a new widget and is indeed how the original dialog works.

(defun search-panel (self)
  (hbox (:align-y 'middle :spacing 12)
    (:allow-grow-y :js-false
      :padding 4)
    (lbl "String:")
    (combobox :symbol-string
      (:add '(:flex 1)
        :allow-grow-x t
        :onkeypress (lambda (self req)
                      (let* ((key (req-val req "keyId"))
                             (priorv (req-val req "priorv")))
                        (when (string-equal key "Enter")
                          ;; the session property "sym-seg" is the dataflow origin, along
                          ;; with filters on which matches to show.
                          (setf (sym-seg (u^ apropos-variant)) priorv))))
        :onchangevalue (lambda (self req)
                         ;;; emulating the Allegro IDE, just save the changed value, do not kick
                         ;;; off an immediate search.
                         (setf (^value) (req-val req "value"))))
      ;; rule below runs whenever the user triggers a search for a segment
      ;; if they did not search on "" and they searched on something new,
      ;; that is added to the existing list of prior searches and
      ;; thru hidden plumbing gets added to the client-side menu of the combo box
      (let ((sympart (sym-seg (u^ apropos-variant))))
        (if (plusp (length sympart))
            (if (find sympart .cache :key 'label :test 'string-equal)
                .cache
              (cons (make-kid 'qx-list-item
                      :label sympart) .cache))
          .cache)))
    (button "Search" ()
      :onexec (b-when sympart (value (psib))
                (setf (sym-seg (u^ apropos-variant)) sympart)))))

#+xxxx (inspect (find-package :socket))

;;; The next two functions build the second row of the GUI
;;; which contains various widgets to filter the search results

(defun pkg-filter (self)
  ;; this guy could be a lot simpler (for user and developer)
  ;; as a check-group-box "Search Specific Package", but the
  ;; original works this way, so...
  (groupbox (:spacing 2)(:legend "Package(s) to Search")
    (hbox (:spacing 20)()
      (checkbox :all-packages "All"
        :value (c-in t))
      (selectbox :selected-pkg (:add '(:flex 1)
                                 :enabled (c? (not (value (fm-other :all-packages))))
                                 :onchangeselection (lambda (self req)
                                                      (let* ((nv (req-val req "value")))
                                                        (b-when item (oid$-to-object nv :ochgsel nil)
                                                          (setf (^value) (model item))))))
        (loop for pkg in (b-if syms nil #+xxxx (syms-unfiltered (u^ apropos-variant))
                           (loop with pkgs
                               for symi in syms
                               do (pushnew (symbol-info-pkg symi) pkgs)
                               finally (return pkgs))
                           (list-all-packages))
            collecting (listitem (package-name pkg)))))))

(defun type-filter (self)
  (groupbox (:spacing 10)(:legend "Show")
    (radiobuttongroup :type-filter (:value (c-in "all"))
      (qx-grid :spacing-x 12 :spacing-y 6)
      (radiobutton "all" "All"
        :add '(:row 0 :column 0))
      (radiobutton "var" "Variables"
        :add '(:row 0 :column 1))
      (radiobutton "fn" "Functions"
        :add '(:row 1 :column 0))
      (radiobutton "class" "Classes"
        :add '(:row 1 :column 1)))
    (checkbox :exported-only "Exported Symbols Only")))

;;; The search results table itself
;;; Missing is a table column model to control initial
;;; column widths and other things like flagging cells
;;; as boolean to get a neat check-mark rendering

(defun symbols-found (self)
  (make-kid 'qxl-table-remote
    :md-name :sym-info-table
    :add '(:flex 1)
    :allow-grow-x t
    :allow-grow-y t
    ;; next three are for data model delegate
    :cb-row-count (lambda (self req)
                    (declare (ignore req))
                    ;(trcx :load-row-count-says (length (sym-info (u^ apropos-variant))))
                    (length (sym-info (u^ apropos-variant))))
    :cb-load-row-data 'sym-get
    :cb-sort-row-data 'sym-sort
    :block-size 100
    ;; columns go to table, table model, table column model, resize behavior....
    :columns (flet ((mtc (n i &rest iargs)
                      (apply 'make-table-column :name n :id i iargs)))
               (list 
                (mtc "Symbol Name" 'name :width 192)
                (mtc "Package" 'pkg)
                (mtc "Function" 'fntype)
                (mtc "Setf" 'setf? :width 48)
                (mtc "Var" 'var? :width 48)
                (mtc "Class" 'class? :width 48)
                (mtc "Exp" 'exported? :width 48)
                ))))