(in-package :qooxlisp)

(defmd av-classic-plus (apropos-variant)
  :kids (c? (the-kids
             (vbox (:spacing 6) 
               (:add '(:left 0 :top 0 :width "100%" :height "100%")
                 :padding 6)
               (search-panel-plus self)
               (hbox (:spacing 6)()
                 (pkg-filter-plus self)
                 (type-filter self))
               (symbols-found self)))))

(defun search-panel-plus (self)
  (hbox (:align-y 'middle :spacing 12)
    (:allow-grow-y :js-false
      :tool-tip-text "Enter symbol name or name fragment for case-insensitive match:\."
      :padding 4)
    (lbl "Symbol name/segment:")
    (combobox :symbol-string
      (:add '(:flex 1)
        :allow-grow-x t
         :onchangevalue (lambda (self req)
                         (setf (sym-seg (u^ apropos-variant)) (req-val req "value"))))

      ;; TODO: why the internals .cache hacking?

      (let ((sympart (sym-seg (u^ apropos-variant))))
        (if (plusp (length sympart))
            (if (find sympart .cache :key 'label :test 'string-equal)
                .cache
              (cons (make-kid 'qx-list-item
                      :label sympart) .cache))
          .cache)))
    (rtf "Just Hit Enter")))

(defun pkg-filter-plus (self)
  ;; this guy could be a lot simpler (for user and developer)
  ;; as a check-group-box "Search Specific Package", but the
  ;; original works this way, so...
  (groupbox (:spacing 2)(:legend "Package(s) to Search"
                          :add '(:flex 1))
    (hbox (:spacing 20)()
      (checkbox :all-packages "All"
        :value (c-in nil))
      (make-kid 'pkg-selector
        :md-name :selected-pkg
        :add '(:flex 1)
        :visibility (c? (vis/not (not (value (fm^ :all-packages)))))))))


(defmd pkg-selector (qx-select-box)
  (pkgs-to-match (c? (mapcar 'symbol-info-pkg 
                       (apropos-pkg-syms (u^ apropos-variant)))))
  :onchangeselection (lambda (self req)
                       (let* ((nv (req-val req "value")))
                         (b-when item (oid$-to-object nv :ochgsel nil)
                           (setf (^value) (model item)))))
  :kids (c? (the-kids
             (loop for pkg in (list-all-packages)
                 collecting (make-kid 'qx-list-item
                              :label (package-name pkg)
                              :model (package-name pkg)
                              :visibility (let ((pkg pkg))
                                            (c? (vis/collapsed
                                                 (b-if pkgs (pkgs-to-match (u^ pkg-selector))
                                                   (find pkg pkgs)
                                                   t)))))))))
