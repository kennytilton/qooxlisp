(in-package :qxl)

(defmd qx-table-model-abstract (qx-object)
  column-name-ids)

(defobserver column-name-ids ((self qx-table-model-abstract))
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (let ((cols (loop for (name id) in new-value
                      collecting name into names
                      collecting id into ids
                      finally (return (list (json$ names)(json$ ids))))))
      (apply 'qxfmt "console.log('call setColumns: ~s');
clDict[~a].setColumns(~a,~a);" (cadr cols) (oid self) cols)))))

(defmd qx-table-model-remote (qx-table-model-abstract)
  (qx-class "ide.TableModelQXL" :cell nil))

(defmd qx-table (qx-widget)
  (qx-class "qx.ui.table.Table" :allocation :class :cell nil)
  table-model)

(defmethod make-qx-instance :after ((self qx-table))
  (with-integrity (:client `(:post-make-qx ,self))
    ;(qxfmt "clDict[~a].setColumnVisibilityButtonVisible(false);" (oid self))
    (qxfmt "var cvbtn= clDict[~a].getChildControl('column-button');" (oid self))
    (qxfmt "console.log('col vis btn '+ cvbtn);")
    (qxfmt "cvbtn.addListener('execute', function(e) {
    console.log('cvbtn fires'+cvbtn);
});" )
    (qxfmt "
clDict[~a].addListener('columnVisibilityMenuCreateStart', function(e) {
    console.log('columnVisibilityMenuCreateStart: '+e.getData());
});" (oid self))
    (qxfmt "
clDict[~a].addListener('columnVisibilityMenuCreateEnd', function(e) {
    console.log('columnVisibilityMenuCreateEnd: '+e.getData());
});" (oid self))))

(defobserver table-model ()
  (when new-value
    (with-integrity (:client `(:post-make-qx ,self))
      (qxfmt "clDict[~a].setTableModel(clDict[~a]);" (oid self)(oid (table-model self))))))
