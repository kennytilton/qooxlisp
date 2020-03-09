(in-package :qooxlisp)

;;; The top-level apropos structure, which defines
;;; slots for key data items and also constitutes
;;; the root of the GUI framework

(defmd av-classic (apropos-variant)
  :kids (c? (the-kids
             (vbox (:spacing 6) 
               (:add '(:left 0 :top 0 :width "100%" :height "100%")
                 :padding 6)
               (search-panel self)
               (hbox (:spacing 6)()
                 (vbox (:spacing 6)(:add '(:flex 1))
                   (pkg-filter self))
                 (type-filter self))
               (symbols-found self t)))))

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
  (groupbox (:spacing 2)(:legend "Package(s) to Search"
                          :add '(:flex 1))
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
