(in-package :qxl)

(defmd qx-table-model-abstract (qx-object)
  column-name-ids)

(defobserver column-name-ids ((self qx-table-model-abstract))
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (apply 'qxfmt "clDict[~a].setColumns(~a,~a);" (oid self)
        (loop for (name id) in new-value
            collecting name into names
            collecting id into ids
            finally (return (list (json$ names)(json$ ids))))))))

(defmd qx-table-model-remote (qx-table-model-abstract)
  (qx-class "ide.TableModelQXL" :cell nil))

(defmd qx-table (qx-widget)
  (qx-class "qx.ui.table.Table" :allocation :class :cell nil)
  table-model)

(defobserver table-model ()
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setTableModel(clDict[~a]);" (oid self)(oid (table-model self))))))
