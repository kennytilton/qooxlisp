
qx.Class.define("ide.TableModelQXL", {
    extend: qx.ui.table.model.Remote,
    construct: function(oid,blocksize){
        this.base(arguments);
        // todo: add some assertions in re params
        //this.oid = oid;
        this.setBlockSize(blocksize);

        this.addListener('metaDataChanged', function(e){
			var sortx = this.getSortColumnIndex();
			var sortid = this.getColumnId(sortx);
			var sortdir = this.isSortAscending() ? "asc" : "dsc";
			//console.log('metadata '+sortx+' '+sortid+' '+sortdir);
			if (sortx > -1 && sortid) {
				var req = new qx.io.remote.Request("/callback", "GET", "text/javascript");
				this.buildReq(req,'sortrowdata');
    			req.setParameter('key', sortid);
				req.setParameter('order', sortdir);
				req.addListener("completed", function(e){
					this.reloadData();
				},this);
				req.send();
			}
		});	
    },
    members: {
        _loadRowCount: function(){
    			var req = new qx.io.remote.Request("/cbjson", "GET", "application/json");
                        this.buildReq(req,'loadrowcount');
    			//req.setTimeout(2000);
    			req.addListener("completed", function(response){					
    				var result = response.getContent();
    				//console.log("loaded row count: "+ result +": "+this.oid);
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
    			
    			req.send();
        },
        _loadRowData: function(firstRow, lastRow){
        	var req = new qx.io.remote.Request("/cbjson", "GET", "application/json");
        	this.buildReq(req,'loadrowdata');
    		req.setParameter('start',firstRow);
        	req.setParameter('count',parseInt(lastRow)-parseInt(firstRow)+1);
			
            req.addListener("completed", function(response){
                var result = response.getContent();
                this._onRowDataLoaded(result);
            }, this);
            req.send();
        }, buildReq: function(req, opcode){
            //console.log('sending request '+opcode+' sess: '+sessId+' oid '+ this.oid);
            req.setParameter('sessId', sessId);
	    req.setParameter('oid', this.oid);
	    req.setParameter('opcode', opcode);
        }
    }
});
