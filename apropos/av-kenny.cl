(in-package :qooxlisp)

(defparameter *uguide* (format nil "Below: a UI into the Lisp function <code>apropos-list</code> which searches ~
the running application for any Lisp source name containing a given substring. Yes, names live on in ~
Lisp even after native compilation. ~
Try \"qx\" or \"qxl\" to see elements used in <b>qooxlisp</b>. ~
Try \"apropos\" to see elements used in this specific example. Packages, by the way, are Lisp groups of names."))

(defmd apropos-ala-kenny (apropos-variant)
  :syms-unfiltered (c? (b-when seg (^sym-seg)
                         (symbol-info-raw seg
                           :pkg (value (fm-other :selected-pkg))
                           :eor (lambda (x)
                                  (if x t :js-false)))))
  :selected-pkg-p (c? (not (null (value (fm-other :selected-pkg)))))
  :kids (c? (the-kids
             (vbox (:spacing 6)
               (:add '(:left 0 :top 0 :width "100%" :height "100%")
                 :padding 6)
               (lbl *uguide* :rich t :width 600)
               (search-panel-kt self)
               (hbox (:spacing 6)()
                 (pkg-filter-kt self)
                 (vbox (:spacing 6 :align-x "center")()
                   (type-filter self)
                   (checkbox :exported-only "Exported Only"
                     :width 192
                     :background-color 'yellow)))
               (symbols-found-kenny self)))))

(defun symbols-found-kenny (self)
  (make-kid 'qxl-table-remote
    :md-name :sym-info-table
    :add '(:flex 1)
    :allow-grow-x t
    :allow-grow-y t
    ;; next three are for data model delegate
    :cb-row-count (lambda (self req)
                    (declare (ignore req))
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
                (mtc "Var" 'var? :width 48)
                (mtc "Class" 'class? :width 48 :renderer 'boolean)
                (mtc "Exp" 'exported? :width 48 :renderer 'boolean)
                ))))

(defun search-panel-kt (self)
  (hbox (:align-y 'middle :spacing 12)
    (:allow-grow-y :js-false
      :padding 4)
    (lbl "Search for:")
    (textfield :symbol-string ;; WARNING -- new and untested
      :add '(:flex 1)
      :allow-grow-x t
      :onchangevalue (lambda (self req)
                       (let ((sympart (req-val req "value")))
                         (setf (sym-seg (u^ apropos-variant)) sympart))))
    (button "Search" (:enabled t #+not (c? (> (length (value (psib))) 1)))
      :onexec (b-when sympart (value (psib))
                (print `(:sympart-onexec-rethought ,sympart))
                (setf (sym-seg (u^ apropos-variant)) sympart)))))

(defun pkg-filter-kt (self)
  (vbox ()(:add '(:flex 1))
    (lbl "...within package(s):")
    (qxlist :selected-pkg
      (:add '(:flex 1)
        :max-height 96 
        :spacing -6
        :selection-mode 'additive
        :onchangeselection (lambda (self req)
                             (print :pkg-filter-kt-changesel-fires)
                             (let* ((nv (req-val req "value"))
                                    (nvs (split-sequence #\! nv)))
                               (trcx :pkg-filter-kt-changesel-sees :nv nv :nvs nvs)
                               (setf (^value) (delete nil
                                                (loop for pkg$ in nvs
                                                    for pkg = (unless (string-equal "" pkg$)
                                                                (find-package pkg$))
                                                      
                                                    do (trcx :pkgfound pkg$ pkg)
                                                    when (find-package pkg$)
                                                    collect pkg))))))
      (loop for pkg in (b-if syms (syms-unfiltered (u^ apropos-variant))
                         (loop with pkgs
                             for symi in syms
                             do (pushnew (symbol-info-pkg symi) pkgs)
                             finally (return pkgs))
                         (subseq (list-all-packages) 0 #+testing 4))
          collecting
            (make-kid 'qx-list-item
              :model (package-name pkg)
              :label (package-name pkg))))))




