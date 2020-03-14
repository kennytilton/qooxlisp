﻿(in-package :qooxlisp)

;;; The top-level apropos structure, an abstract class which defines
;;; slots for key data items and also constitutes the root of the GUI framework

(defmd apropos-variant (kb-manager qxl-column)
  :onkeydown 'apropos-onkeydown
  (sym-seg (c-in nil))
  (apropos-pkg-syms (c? (b-when seg (^sym-seg)
                         (trcx :calcing-symunfiltered! seg)
                         (symbol-info-raw seg :pkg (unless (fmv :all-packages)
                                                     (value (fm-other :selected-pkg)))))))
  (syms-filtered (c? (symbol-info-filtered (^apropos-pkg-syms)
                       (value (fm-other :type-filter))
                       (value (fm-other :exported-only)))))
  (sym-sort-spec (c-in nil))
  (sym-info (c? (b-if sort (^sym-sort-spec)
                  (destructuring-bind (sort-key order) sort
                    (sort (copy-list (^syms-filtered))
                      (if (equal order "asc")
                          'string-lessp 'string-greaterp)
                      :key (if (equal sort-key "pkg")
                               (lambda (si) (package-name (symbol-info-pkg si)))
                             (qxl-sym (conc$ 'symbol-info- sort-key)))))
                  (^syms-filtered)))))

(defun apropos-onkeydown (self req)
  (declare (ignorable self))
  (let* ((key (req-val req "keyId"))
         (mods (parse-integer (req-val req "mods"))))
    ;; ugh. Re-inventing qx propagation, returning nil if keydown not for me
    (prog1 t  ;; optimistic. above exceptions will have to be hardcoded to handle others
       (trcx onkeydown!!!!!!!!!! self key mods)
       (terpri)
      (b-if control (kb-control-match self key mods)
        (progn
          (b-if action (onexecute control)
            (funcall action control req)
            (warn "keychord ~a match yes, onexec binding no: ~a" (list key mods) control)))
        (trcx no-kb-control!!!!!! self key mods)))))

(defobserver sym-info ()
  (with-integrity (:client `(:post-make-qx ,self))
    (let ((tbl (fm-other :sym-info-table)))
      (b-when oid (oid (table-model tbl))
        (qxfmt "clDict[~a].reloadData();" oid)))))

(defstruct symbol-info name pkg fntype setf? var? class? exported?)

;;; Next two functions support rules for gathering matching symbols
;;; and filtering them as user changes constraints on what to show

(defun symbol-info-raw (s &key pkg)
  (when (plusp (length s)) ;; todoo (when s...
    (flet (#+xxx(eor (x)
                  (if x "x" ""))
           (eor (x)
                                  (if x t :js-false))
           (exportedp (sym)
             (eq (nth-value 1 (find-symbol (symbol-name sym)(symbol-package sym))) :external)))
      (loop for sym in (apropos-list s pkg)
          collecting ; TODOO memoize next in a 'build-symbol-info fn
            (make-symbol-info
             :name (symbol-name sym)
             :pkg (symbol-package sym)
             :fntype (cond
                      ((macro-function sym) "macro")
                      ((and (fboundp sym)
                         (typep (symbol-function sym) 'standard-generic-function)) "generic")
                      ((fboundp sym) "function")
                      (t ""))
             :var? (if (boundp sym)
                       (if (constantp sym)
                           "con" "var")
                     "")
             :setf? (eor (fboundp `(setf ,sym)))
             :class? (eor (find-class sym nil))
             :exported? (progn (trcx :expo? (exportedp sym) sym)
                          (exportedp sym)))))))

(defun symbol-info-filtered (syms type exported-only-p)
  (trcx :symbol-info-filtered-sees type exported-only-p)
  (loop for sym in syms
      when (and
            (or (not exported-only-p)
              (symbol-info-exported? sym))
            (case$ type
              ("all" t)
              ("fn" (not (equal "" (symbol-info-fntype sym))))
              ("var" (plusp (length (symbol-info-var? sym))))
              ("class" (or (eq t (symbol-info-class? sym))
                         (equal "x" (symbol-info-class? sym))))
              (otherwise (error "Type selector ~a invalid" type))))
      collecting sym))

(defun sym-get (self req)
  (let* ((start (req-val req "start"))
         (row-count (req-val req "count")))   
    (setf start (parse-integer start))
    (setf row-count (parse-integer row-count))
    (trcx :qx-getdata start row-count)
    (loop for sym in (sym-info (u^ apropos-variant))
        for n upfrom 0
        when (< (1- start) n (+ start row-count))
        collect (list
                 (cons :name (symbol-info-name sym))
                 (cons :pkg (conc$ (b-if nns (remove "" (package-nicknames (symbol-info-pkg sym))
                                        :test 'string-equal)
                              (car nns)
                              (package-name (symbol-info-pkg sym)))
                              (if (symbol-info-exported? sym) ":" "::")))
                 (cons :fntype (symbol-info-fntype sym))
                 (cons :var? (symbol-info-var? sym))
                 (cons :setf? (symbol-info-setf? sym))
                 (cons :class? (symbol-info-class? sym))
                 (cons :exported? (if (symbol-info-exported? sym)
                                      t :js-false))))))

(defun sym-sort (self req)
  (prog1 nil
    (setf (sym-sort-spec (upper (table self) apropos-variant))
      (list (req-val req "key")(req-val req "order")))))

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

(defun symbols-found (self &optional classicp)
  (vbox (:spacing 6)(:add '(:flex 1))
    (lbl (c? (let ((sym-seg (sym-seg (u^ apropos-variant))))
               (if classicp
                   (if (plusp (length sym-seg))
                       (format nil "Symbols containing ~s:" sym-seg)
                     "Symbols Found:")
                 (if (plusp (length sym-seg))
                     (format nil "Found ~d~a symbols containing ~s:" 
                       (length (sym-info (u^ apropos-variant)))
                       (if (fmv :exported-only) " exported" "")
                       sym-seg)
                   "")))))
    (symbols-found-table self)))

;;; The search results table itself
;;; Missing is a table column model to control initial
;;; column widths and other things like flagging cells
;;; as boolean to get a neat check-mark rendering

(defun symbols-found-table (self)
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
                (mtc "Setf" 'setf? :width 48 :renderer 'boolean)
                (mtc "Var" 'var? :width 48 :renderer '(html "center" "" "" ""))
                (mtc "Class" 'class? :width 48 :renderer 'boolean)
                (mtc "Exp" 'exported? :width 48 :renderer 'boolean)))))

