/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(ide/*)

************************************************************************ */

var clDict = [];
var qxDoc;

/**
 * This is the main application class of your custom application "ide"
 */
qx.Class.define("ide.Application",
{
  extend : qx.application.Standalone,



  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
    /**
     * This method contains the initial application code and gets called 
     * during startup of the application
     * 
     * @lint ignoreDeprecated(alert)
     */
    main : function()
    {
      // Call super class
      this.base(arguments);

      // Enable logging in debug variant
      if (qx.core.Variant.isSet("qx.debug", "on"))
      {
        // support native logging capabilities, e.g. Firebug for Firefox
        qx.log.appender.Native;
        // support additional cross-browser console. Press F7 to toggle visibility
        qx.log.appender.Console;
      }

      /*
      -------------------------------------------------------------------------
        Below is your actual application code...
      -------------------------------------------------------------------------
      */
      console.log("ide.js> start");
      clDict[0] = qxDoc = this.getRoot();
      console.log("app.js> doc "+clDict[0]);
      (new qx.io.remote.Request("/begin","GET", "text/javascript")).send();
    }
  }
});

function forceClassLoad () {
    var a = [];
    a.push(new ide.TableModelQXL);
    a.push(new qx.ui.layout.HBox);
    a.push(new qx.ui.layout.VBox);
    a.push(new qx.ui.form.ComboBox);
    a.push(new qx.ui.form.ListItem);
    a.push(new qx.ui.form.Button);
    a.push(new qx.ui.basic.Label);
    a.push(new qx.ui.table.model.Simple);
    a.push(new qx.ui.table.model.Remote);
    a.push(new qx.ui.table.Table);
    a.push(new qx.ui.table.columnmodel.Basic);
    a.push(new qx.ui.table.ITableModel);
    return a;
}
