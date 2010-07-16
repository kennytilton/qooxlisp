(in-package :qooxlisp)

(defmd apropos-session-makeover (apropos-session)
  :syms-filtered (c? (symbol-info-filtered (^syms-unfiltered)
                       (value (fm-other :type-filter))
                       (value (fm-other :exported-only))
                       (value (fm-other :selected-pkg-p))
                       (value (fm-other :selected-pkg))))
  :kids (c? (the-kids
             (vbox (:spacing 6) 
               (:add '(:left 0 :top 0 :width "100%" :height "100%")
                 :padding 6)
               (search-panel self)
               (hbox (:spacing 6)()
                 (pkg-filter-mo self)
                 (vbox (:spacing 6 :align-x "center")()
                   (type-filter-mo self)
                   (checkbox :exported-only "Exported Only")))
               (symbols-found self)))))


(defun pkg-filter-mo (self)
  (checkgroupbox (:spacing 2)(:md-name :selected-pkg-p
                               :add '(:flex 1)
                               :allow-grow-y :js-false
                               :legend "Search One Package"
                               :value (c-in nil)) ;; becomes state of check-box!
    (selectbox :selected-pkg (:add '(:flex 1)
                               :enabled (c? (value (fm-other :selected-pkg-p)))
                               :onchangeselection (lambda (self req)
                                                        (let ((nv (req-val req "value")))
                                                          (setf (^value) (find-package nv)))))
      (b-if syms (syms-unfiltered (u^ qxl-session))
        (loop with pkgs
            for symi in syms
            do (pushnew (symbol-info-pkg symi) pkgs)
            finally (return (loop for pkg in pkgs
                                collecting
                                  (make-kid 'qx-list-item
                                    :model (package-name pkg)
                                    :label (package-name pkg)))))
        (loop for pkg in (subseq (list-all-packages) 0 #+testing  5)
            collecting
              (make-kid 'qx-list-item
                :model (package-name pkg)
                :label (package-name pkg)))))))

(defun type-filter-mo (self)
  (groupbox ()(:legend "Show")
    (radiobuttongroup :type-filter (:value (c-in "all"))
      (qx-grid :spacing-x 12 :spacing-y 6)
      (radiobutton "all" "All"
        :add '(:row 0 :column 0))
      (radiobutton "var" "Variables"
        :add '(:row 0 :column 1))
      (radiobutton "fn" "Functions"
        :add '(:row 1 :column 0))
      (radiobutton "class" "Classes"
        :add '(:row 1 :column 1)))))
