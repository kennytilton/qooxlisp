(in-package :apropos-qx)

(defstruct symbol-info name pkg fntype setf? var? class? exported?)

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
                          (cons :pkg (symbol-info-pkg sym))
                          (cons :fntype (symbol-info-fntype sym))
                          (cons :var? (symbol-info-var? sym))
                          (cons :setf? (symbol-info-setf? sym))
                          (cons :class? (symbol-info-class? sym))
                          (cons :exported? (symbol-info-exported? sym)))))))))))

(defun symbol-info-load (s)
  (when (plusp (length s))
    (flet ((eor (x)
             (if x "x" " ")))
      (loop for sym in (apropos-list s)
          collecting (make-symbol-info
                      :name (symbol-name sym)
                      :pkg (package-name (symbol-package sym))
                      :fntype (cond
                               ((macro-function sym) "macro")
                               ((fboundp sym) "function")
                               (t ""))
                      :var? (eor (boundp sym))
                      :setf? (eor (fboundp `(setf ,sym)))
                      :class? (eor (find-class sym nil))
                      :exported? (eor (eql (nth-value 1 (find-symbol (symbol-name sym)(symbol-package sym))) :external)))))))

(defun qx-getdatacount (req ent)
  (prog1 nil
    (with-json-response (req ent)
      (whtml
       (:princ
        (json:encode-json-to-string (length (sym-info *qxdoc*))))))))

(defun qx-sortdata (req ent)
  (with-qx-js-response (req ent)
    (let ((sort-key (req-val req "key"))
          (order (req-val req "order")))
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
          :key (intern (conc$ "symbol-info-" sort-key) :apropos-qx))))))

(defun boolean< (a b)
  (and b (not a)))

(defun boolean> (a b)
  (and a (not b)))

