/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(ide/*)

************************************************************************ */

// qooxlisp glue data:

var clDict = [];
var sessId;

// back to the boilerplate:

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

      /* qooxlisp:
        To get the qooxdoo generator to include a class we want
        to instantiate only through eval'ed JS, we need the static
        project source to mention the class somewhere/anywhere:
      */
      qx.ui.form.SelectBox;
      qx.ui.decoration.Single;
      qx.ui.decoration.Double;
      qx.ui.decoration.Beveled;
      qx.ui.decoration.Uniform;
      ide.TableModelQXL;
      qx.ui.layout.HBox;
      qx.ui.layout.VBox;
      qx.ui.layout.Grid;
      qx.ui.form.ComboBox;
      qx.ui.form.ListItem;
      qx.ui.form.Button;
      qx.ui.table.columnmenu.Button;
      qx.ui.basic.Label;
      qx.ui.table.model.Simple;
      qx.ui.table.model.Remote;
      qx.ui.table.Table;
      qx.ui.table.columnmodel.Basic;
      qx.ui.table.ITableModel;
      qx.ui.form.RadioButtonGroup;
      qx.ui.form.RadioButton;
      qx.ui.form.CheckBox;
      qx.ui.groupbox.GroupBox;
      qx.ui.groupbox.CheckGroupBox;
      qx.theme.Classic;
      qx.theme.Modern;
      qx.ui.table.cellrenderer.Boolean;
      qx.ui.form.List;
      qx.ui.form.ListItem;
      // var x = new ide.TableModelQXL(7,100);
      // in effect, start the repl...
      (new qx.io.remote.Request("/begin","GET", "text/javascript")).send();
    }
  }
});

