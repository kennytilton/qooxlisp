qx.Class.define("ide.TableColumnCenteredHeader",
{
  extend : qx.ui.table.headerrenderer.Default,

  construct : function()
  {
    this.base(arguments);
  },


  members :
  {
    // overridden
    updateHeaderCell : function(cellInfo, cellWidget)
    {
      this.base(arguments, cellInfo, cellWidget);
      var label = cellWidget.getChildControl('label');
      label.setTextAlign('center');
      label.setAllowGrowX(true);
    }
  }
});