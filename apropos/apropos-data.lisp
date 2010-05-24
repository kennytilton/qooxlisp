(in-package :apropos-qx)

(defstruct symbol-info name pkg fntype setf? var? class? exported?)

(defun symbol-info-raw (s)
  (when (plusp (length s))
    (flet ((eor (x)
             (if x "x" ""))
           (exportedp (sym)
             (eql (nth-value 1 (find-symbol (symbol-name sym)(symbol-package sym))) :external)))
      (loop for sym in (apropos-list s)
          collecting (make-symbol-info
                      :name (symbol-name sym)
                      :pkg (or (symbol-package sym)
                             (break "no sympkg for ~a" sym))
                      :fntype (cond
                               ((macro-function sym) "macro")
                               ((fboundp sym) "function")
                               (t ""))
                      :var? (if (boundp sym)
                                (if (constantp sym)
                                    "con" "var") "")
                      :setf? (eor (fboundp `(setf ,sym)))
                      :class? (eor (find-class sym nil))
                      :exported? (eor (exportedp sym)))))))

(defun symbol-info-filtered (syms type exported-only-p all-pkgs-p selected-pkg)
  (loop for sym in syms
      when (and
            (or (not exported-only-p) (equal "x" (symbol-info-exported? sym)))
            (or all-pkgs-p (eq selected-pkg (symbol-info-pkg sym)))
            (ecase type
              (:all t)
              (:fn (not (equal "" (symbol-info-fntype sym))))
              (:var (equal "x" (symbol-info-var? sym)))
              (:class (equal "x" (symbol-info-class? sym)))))
      collecting sym))

(defun qx-getdata (req ent)
  (prog1 nil
    (with-json-response (req ent)
        (let* ((start (req-val req "start"))
               (row-count (req-val req "count")))   
          
          (setf start (parse-integer start))
          (setf row-count (parse-integer row-count))
          (trcx :qx-getdata start row-count)
          (whtml
           (:princ 
            (json:encode-json-to-string
             (loop for sym in (sym-info *qxdoc*)
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
                          (cons :exported? (symbol-info-exported? sym)))))))))))

(package-nicknames (find-package :keyword))

(defun qx-getdatacount (req ent)
  (prog1 nil
    (with-json-response (req ent)
      (whtml
         (:princ
          (json:encode-json-to-string (length (sym-info *qxdoc*))))))))

(defun qx-sortdata (req ent)
  (with-qx-js-response (req ent)
    (let ((sort-key (req-val req "key"))
          (order (req-val req "order"))
          (*qxdoc* (qxl-request-session req)))
      (setf (sym-info *qxdoc*)
        (sort (sym-info *qxdoc*) 
          (if (find$ sort-key '("name" "pkg"))
              (if (equal order "asc")
                  'string-lessp 'string-greaterp)
            (if (equal order "asc")
                'string-lessp 'string-greaterp)
            #+notyet
            (if (equal order "asc")
                'boolean< 'boolean>))
          :key (if (equal sort-key "pkg")
                   (lambda (si) (package-name (symbol-info-pkg si)))
                 (intern (conc$ "symbol-info-" sort-key) :apropos-qx)))))
    (qxfmt "console.log('ack sort');")))

(defun boolean< (a b)
  (and b (not a)))

(defun boolean> (a b)
  (and a (not b)))

