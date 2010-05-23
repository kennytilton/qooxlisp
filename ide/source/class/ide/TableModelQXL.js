
qx.Class.define("ide.TableModelQXL", {
    extend: qx.ui.table.model.Remote,
    construct: function(countURL, rowURL, sortURL, blocksize){
        this.base(arguments);
        //this.countU = countURL;
        //this.rowU = rowURL;
        //this.sortU = sortURL;

        if (blocksize===undefined)
            blocksize=100;
        this.setBlockSize(blocksize);

        if (this.sortU) {
		this.addListener('metaDataChanged', function(e){
				var sortx = this.getSortColumnIndex();
				var sortid = this.getColumnId(sortx);
				var sortdir = this.isSortAscending() ? "asc" : "dsc";
				if (sortx > -1 && sortid) {
					var req = new qx.io.remote.Request(this.sortU, "GET", "text/javascript");
					req.setParameter('key', sortid);
					req.setParameter('order', sortdir);
					req.addListener("completed", function(e){
						this.reloadData();
					},this);
					req.send();
				}
		});
	   }
		
    },
    members: {
        countU: "/getdatacount",
	   dbg: false,
        rowU: "/getdata",
		sortU: "/sortdata",
		req : null,
		dataReq : null,
		rowCount : null,
	   reloadData : function (fn) {
    		//console.log("reloadData entry "+this.rowU);
		this.rowCount = null;
		this.base(arguments);
	   },
        _loadRowCount: function(){
           this.req = new qx.io.remote.Request(this.countU, "GET", "application/json");
		this.req.setTimeout(2000);
		this.req.addListener("completed", function(response){					
			var result = response.getContent();
			//console.log("loaded row count: "+ result +": "+this.countU);
			if (result === null) {
				this._onRowCountLoaded(0);
				this.rowCount = 0;
				return 0;
			} else {
				this._onRowCountLoaded(result);
				this.rowCount = result;
				return result;
                }
		}, this);
		this.req.send();
        },
        _loadRowData: function(firstRow, lastRow){
            var ru = (typeof this.rowU=='function')? (this.rowU)():this.rowU;
            var req = new qx.io.remote.Request(ru, "GET", "application/json");
			this.req = req;
			req.setParameter('start',firstRow);
			req.setParameter('count',parseInt(lastRow)-parseInt(firstRow)+1);
			
            req.addListener("completed", function(response){
                var result = response.getContent();
                this._onRowDataLoaded(result);
            }, this);
            req.send();
        }
    }
});
