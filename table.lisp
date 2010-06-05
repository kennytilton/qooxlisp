(in-package :qxl)

(defmd qx-table-model-abstract (qx-object)
  ;; columns are defined by table and supplied from its columns observer
  (table nil :cell nil))

(defmethod session ((self qx-table-model-abstract))
  (session (table self)))

(defmd qx-table-model-remote (qx-table-model-abstract)
  (qx-class "ide.TableModelQXL" :cell nil)
  :constructor-args (c? (list (oid (table self))
                          (block-size (table self)))))

(defmd qx-table (qx-widget)
  (qx-class "qx.ui.table.Table" :allocation :class :cell nil)
  table-model
  columns
  block-size
  ;; callbacks from table data model:
  cb-row-count cb-load-row-data cb-sort-row-data)

(defun loadrowcount (self req)
  (prog1 nil
    (whtml
     (:princ
      (json:encode-json-to-string
       (funcall (cb-row-count self) self req))))))

(defun loadrowdata (self req)
  (prog1 nil
    (whtml
     (:princ
      (json:encode-json-to-string
       (funcall (cb-load-row-data self) self req))))))

(defun sortrowdata (self) ;; js response so a little different
  (cb-sort-row-data self))

(defmd qxl-table-remote (qx-table)
  :table-model (c? (make-instance 'qx-table-model-remote
                     :oid (get-next-oid (session self))
                     :table self)))

(defobserver table-model ()
  (when new-value
    (setf (table new-value) self)
    (with-integrity (:client `(:post-make-qx ,self))
      ;; we could and perhaps shouls get fancy with queue
      ;; so stColumns gets called before the model gets
      ;; gets inserted in the table, but that does have to
      ;; be the order (and we cannot change columns after the
      ;; fact (qooxdoo limitation) so observer would be pointless
      (let ((model-oid (oid new-value))
            (cols (loop for col in (columns self)
                      for name = (tcol-name col)
                      for id = (tcol-id col)
                      collecting name into names
                      collecting id into ids
                      finally (return (list (json$ names)(json$ ids))))))
        ;; first tell table (data) model...
        (apply 'qxfmt "clDict[~a].setColumns(~a,~a);" model-oid cols)
        ;; ...then tell table column model
        #+chill
        (loop for col in new-value
            for n upfrom 0
            do (b-when w (tcol-width col)
                 (qxfmt "clDict[~a].getTableColumnModel().setColumnWidth(~a,~a)" (oid self) n w)))
        (qxfmt "clDict[~a].setTableModel(clDict[~a]);" (oid self) model-oid)))))

(defstruct (table-column (:conc-name tcol-))
  name id width renderer visible editable)
