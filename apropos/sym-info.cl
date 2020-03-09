(in-package :qooxlisp)

(in-package :qooxlisp)
(defstruct symbol-info name pkg fntype setf? var? class? exported?)

;;; Next two functions support rules for gathering matching symbols
;;; and filtering them as user changes constraints on what to show

(defun symbol-info-raw (s &key pkg (eor (lambda (x)
                                          (if x "x" ""))))
  (when (plusp (length s))
    (flet ((exportedp (sym)
             (eql (nth-value 1 (find-symbol (symbol-name sym)(symbol-package sym))) :external)))
      (loop for sym in (apropos-list s pkg)
          collecting (make-symbol-info
                      :name (symbol-name sym)
                      :pkg (symbol-package sym)
                      :fntype (cond
                               ((macro-function sym) "macro")
                               ((fboundp sym) "function")
                               (t ""))
                      :var? (when (boundp sym)
                                (if (constantp sym)
                                    "con" "var") "")
                      :setf? (funcall eor (fboundp `(setf ,sym)))
                      :class? (funcall eor (find-class sym nil))
                      :exported? (funcall eor (exportedp sym)))))))

(defun symbol-info-filtered (syms type exported-only-p)
  (trcx :symbol-info-filtered-sees type exported-only-p)
  (loop for sym in syms
      when (and
            (or (not exported-only-p) (or (eq t (symbol-info-exported? sym))
                                        (equal "x" (symbol-info-exported? sym))))
            (case$ type
              ("all" t)
              ("fn" (not (equal "" (symbol-info-fntype sym))))
              ("var" (plusp (length (symbol-info-var? sym))))
              ("class" (or (eq t (symbol-info-class? sym))
                         (equal "x" (symbol-info-class? sym))))
              (otherwise (error "Type selector ~a invalid" type) )))
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
                 (cons :pkg (b-if nns (remove "" (package-nicknames (symbol-info-pkg sym))
                                        :test 'string-equal)
                              (car nns)
                              (package-name (symbol-info-pkg sym))))
                 (cons :fntype (symbol-info-fntype sym))
                 (cons :var? (symbol-info-var? sym))
                 (cons :setf? (symbol-info-setf? sym))
                 (cons :class? (symbol-info-class? sym))
                 (cons :exported? (symbol-info-exported? sym))))))

(defun sym-sort (self req)
  (prog1 nil
    (setf (sym-sort-spec (upper (table self) apropos-variant))
      (list (req-val req "key")(req-val req "order")))))

