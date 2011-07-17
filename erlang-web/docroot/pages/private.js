goApp.addStrings({
	gridRecordText: 'Viewing {0} - {1} of {2}'
});

/**
 * @property {Integer} Page size for grids
 */
goApp.pageSize	= (function() {
	var aMatches	= document.cookie.match(/pageSize=(\d+)/);
	return aMatches ? aMatches[1] : 20;
})();

/**
 * @property {String} JQ selector for editor
 */
goApp.editorSelector	= '';

/**
 * ID of user account for context
 */
goApp.contextId	= null;

/**
 * Info on addintional scripts required for page
 */
goApp.scripts	= {};
	
/**
 * Get user ID for context
 * @returm {Integer} User ID of null
 */
goApp.getContextId	= function()
{
	var aMatches	= location.pathname.match(/(?:[^/]+)\/(\d+)/);
	return goApp.contextId = (aMatches ? aMatches[1] * 1 : null);
};
	
/**
 * Process fault of editor operation
 * @param {XMLHTTPRequest} Transport object
 * @param {String} sTextStatus Type label
 * @param {Object} Exception
 */
goApp.processEditorFault	= function(oXHR, sTextStatus, oException)
{
	$(goApp.editorSelector).dialog('open');
	goApp.processFailedRequest(oXHR, sTextStatus, oException);
};

/**
 * Put selected data into jqGrid
 * @param {String} Grid element id
 * @param {Object} oResult Command result
 */
goApp.fillGrid	= function(sGridId, oResult)
{
	function mapRecords(aRecords)
	{
		var
			oData	= {},
			nIndex	= aRecords.length;
		
		while (--nIndex >= 0)
			oData[aRecords[nIndex].id] = aRecords[nIndex];
			
		return oData;
	}
	
	if (!oResult.page)
	{
		oResult.page	= {
			index: 0
		};
		
		if (!oResult.hasOwnProperty('totalSize'))
			oResult.totalSize	= oResult.data.length;
	}
	
	$('#' + sGridId)[0].addJSONData({
		records: oResult.totalSize,
		total: Math.ceil(oResult.totalSize / this.pageSize),
		page: oResult.page.index + 1,
		rows: oResult.data,
		userdata: mapRecords(oResult.data)
	});		
};

/**
 * Adds record to stored data
 * @param {String} sId Record ID
 * @param {Object} oData New record data from server
 * @param {Object} oGrid Grid object
 */
goApp.addRecord	= function(sId, oData, oGrid)
{
	(oGrid || $('#grid')).jqGrid('getGridParam', 'userData')[sId]	= oData;
};

/**
 * Updates record into stored data
 * @param {String} sId Record ID
 * @param {Object} oData Updated data from server
 * @param {Object} oGrid Grid object
 */
goApp.storeProperties	= function(sId, oData, oGrid)
{
	var oRecord	= (oGrid || $('#grid')).jqGrid('getGridParam', 'userData')[sId];
	
	for (var sProp in oRecord) if (oData.hasOwnProperty(sProp))
		oRecord[sProp]	= oData[sProp];
};

/**
 * Remembers page size into property and cookie
 * @param {Integer} nSize Number of records per page
 */
goApp.storePageSize	= function(nSize)
{
	this.setCookie('pageSize', this.pageSize = nSize);
};

/**
 * Sample is used to fast search
 */
goApp.searchSample	= '';

/**
 * Creates filter to fast search
 * @param {String[]} aFields List of fields to search
 * @return {Object} Filter for 'select' request
 */
goApp.getSearchFilter	= function(aFields)
{
	var aRules	= [];
	
	$.each(aFields, function(nIndex, sField) {
		aRules.push({
			field: sField,
			match: 'contains',
			value: goApp.searchSample
		});
	});
	
	return {
		match: 'or',
		rules: aRules
	};
};

/**
 * Checks for ERR_AUTHORIZATION_REQUIRED and redirect user to home page
 * @param {Object} oCommand Command
 * @return {Boolean} true if authorization is required
 */
goApp.needAuthorization	= 
goApp.handleLostAccount	= function(oCommand)
{
	if (oCommand.errors[0].code == this.errors.ERR_AUTHORIZATION_REQUIRED)
	{
		new MessageBox('warning', goApp.getErrorMessage(oCommand.errors[0]))
			.open().one('dialogclose', function() {
				location	= goApp.loginPath;
			});
			
		return true;
	}
	
	return false;
};

/**
 * Checks result for error. If it's found, shows error message and set focus to
 * passed field in form
 * @param {Object} oCommand Command
 * @param {String} sId ID of field is to be focused
 * @return {Boolean} true always
 */
goApp.handleOtherError	= function(oCommand, sId)
{
	new MessageBox('warning', goApp.getErrorMessage(oCommand.errors[0]))
		.open()
		.one('dialogclose', function()
		{
			$('#' + (sId || '')).focus().select();
		});
	
	return true;
};

/**
 * Loads application-specific HTML and JavaScript
 * @param {String} sPage Page name
 */
goApp.load	= function(sPage)
{
	if (sPage)
	{
		var sBasicName	= '/' + this.folder + sPage + '/' + sPage;
		
		if (this.onlyScriptPageMask && this.onlyScriptPageMask.test(sPage)) 
			this.loadScripts(sPage, sBasicName);
		else // layout then script
			$('#app-content').load(sBasicName + '.html', function()
				{
					goApp.loadScripts(sPage, sBasicName);
				});
	}
};

/**
 * Loads scripts required for page
 * @param {String} sPage Page name
 * @param {String} sBasicName Basic files name without extension
 */
goApp.loadScripts	= function(sPage, sBasicName)
{
	var
		oParams	= this.scripts[sPage] || (this.scripts[sPage] = {}),
		aScripts	= oParams.libs || [];
		
	function handleLoading()
	{
		!--oParams.libCount
			&& oParams.callback && goApp[oParams.callback]();
	}
	
	function load(sFile)
	{
		if (typeof sFile == 'string')
		{
			$.getScript(sFile, handleLoading);
		}
		else
		{
			var sNext	= sFile.next;
			
			$.getScript(sFile.path, function()
			{
				oParams.libCount++;
				handleLoading();
				load(sNext);
			});
		}
	}
	
	aScripts.push(sBasicName + '.js');
	oParams.libCount	= aScripts.length;
	
	$.each(aScripts, function(nIndex, sFile) {
		load(sFile);
	});
};

/**
 * Creates grid
 * @param {String} sId ID of HTML element
 * @param {Object} Grid parameters
 */
goApp.createGrid	= function(sId, oSettings)
{
	var oParams	= {
		autowidth: true,
		gridview: true,
		rowNum: this.pageSize,
		rowList: [10, 20, 30, 50],
		pager: '#pager',
		pagerpos: 'left',
		recordtext: this.strings.gridRecordText,
		viewrecords: true,
		height: '100%',
		multiselectWidth: 24,
		multiboxonly: true
	};
	
	$('#' + sId).jqGrid(this.completeObject(oParams, oSettings));
};

/**
 * Creates search pane
 * @param {String} sSample Search sample
 */
goApp.createSearchPane	= function()
{
	this.searchPane	= new SearchPane('search-pane', function(sSample)
	{
		goApp.searchSample	= sSample;
		$('#grid').jqGrid('setGridParam', {page: 0});
		$('#grid').trigger('reloadGrid');
	});	
};

/**
 * Replaces commas with comma-and-space sequences
 * @param {String} sValue Text to process
 * @return {String} Corrected text
 */
goApp.correctListCommas	= function(sValue)
{
	return sValue.replace(/\s*,\s*/g, ', ');
};

/**
 * Gets selected item for single-selection mode
 * @return {Object} Selected record
 */
goApp.getSelectedRecord	= function()
{
	var
		oGrid	= $('#grid'),
		oData	= oGrid.jqGrid('getGridParam', 'userData'),
		nID		= oGrid.jqGrid('getGridParam', 'selrow');
	
	return oData[nID];
};

/**
 * Processes response to select request
 * @param {Object} oResponse Response object
 */
goApp.processSelectResponse	= function(oResponse)
{
	if (!oResponse) return;
	
	try
	{
		var oResult	= oResponse.commands[0];
		
		goApp.checkCommandType(oResult, 'select');
			
		if (oResult.errors)
		{
			if (goApp.needAuthorization(oResult))
				return;
			
			new MessageBox('warning', goApp.getErrorMessage(oResult.errors[0]))
				.open();
			return;
		}
		
		goApp.handleSelectResponse && goApp.handleSelectResponse(oResult);
		
		goApp.fillGrid('grid', oResult);
	}
	catch (oException)
	{
		new MessageBox('error', oException.message).open();
	}	
};
