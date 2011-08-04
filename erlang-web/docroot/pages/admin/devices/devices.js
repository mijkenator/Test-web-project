goApp.setPageTitle('GTK::Pbx devices');

/**
 * App-specific strings
 */
goApp.addStrings({
	confirmDelete: 'Delete selected device?',
	create: 'Create',
	createAccount: 'Create new device',
	editAccount: 'Device properties',
	modifiedLost: 'Several devices that you modify were removed by '
		+ 'third party application.',
	update: 'Save'
});

goApp.addErrorMessage(goApp.errors.ERR_DUPLICATION,
{
	email: 'Passed e-mail is already used.',
	login: 'Passed username is already used.'
});
goApp.addErrorMessage(goApp.errors.ERR_RECORD_LOST,
	'Device that you edit was removed by third party application.');

/**
 * ID of current account
 */
goApp.currentDeviceId	= null;

/**
 * Search pane
 */
goApp.createSearchPane();

/**
 * Formats created or last login date and time
 * @param {String} Date and time in ISO representation
 */
goApp.formatTime	= function(sValue)
{
	return sValue == '' || sValue.charAt(0) == '0'
		? ''
		: goApp.getDatetimeHTML(
			goApp.trimSeconds(goApp.convertUTCToLocal(sValue)));
};

goApp.get_account_name_by_id = function(sValue)
{
	return goApp.AccountMData[sValue] || ''
};


goApp.get_accounts = function()
{
	var
		oRequest	= new Request('/json/accounts'),
		oCommand	= {
			type: 'select',
			needTotalSize: true,
			needFields: !goApp.validator,
			fields: ['id', 'name']
		};
	
	oRequest.add(oCommand);
	oRequest.send(function(O){
		var oResult= O.commands[0];
		var options = $('#account_id').attr('options');
		for(var i=0;i < oResult.data.length;i++){
			goApp.AccountMData[oResult.data[i].id] = oResult.data[i].name;
			options[options.length] = new Option(oResult.data[i].name, oResult.data[i].id, true, true);
		}
	}, function(O){});
};

goApp.AccountMData = [];
goApp.get_accounts();


goApp.get_org_name_by_id = function(sValue)
{
	return goApp.OrgData[sValue]
};
goApp.get_type_name_by_id = function(sValue)
{
	var ret = '7';
	switch(sValue){
		case '1':  ret = "Cisco";   break;
		case '2':  ret = "Linksys"; break;
		case '3':  ret = "Polycom"; break;
		case '4':  ret = "Kekufon"; break;
		default: ret = "Default"
	}
	return ret
};

goApp.get_organizations = function()
{
	var
		oRequest	= new Request('/json/organization'),
		oCommand	= {
			type: 'select',
			needTotalSize: true,
			needFields: !goApp.validator,
			fields: ['id', 'name']
		};
	
	oRequest.add(oCommand);
	oRequest.send(function(O){
		var oResult= O.commands[0];
		var options = $('#organization_id').attr('options');
		for(var i=0;i < oResult.data.length;i++){
			goApp.OrgData[oResult.data[i].id] = oResult.data[i].name;
			options[options.length] = new Option(oResult.data[i].name, oResult.data[i].id, true, true);
		}
	}, function(O){});
};

goApp.OrgData = [];
goApp.get_organizations();


/**
 * Grid to show accounts
 */
goApp.createGrid('grid', {
	sortname: 'id',
	sortorder: 'desc',
	multiselect: true,
	colNames: [
		'ID',
		'Name',
		'Organization Name',
		'Account Name',
		'Type',
		'Mac',
		'Info',
		'Created',
		'Active'
	],
	colModel: [
		{
			name: 'id',
			index: 'id',
			width: 50,
			align: 'center'
		},
		{
			name: 'name',
			index: 'name',
			width: 190,
			classes: 'bold'
		},
		{
			name: 'organization_id',
			index: 'organization_id',
			width: 190,
			classes: 'bold',
			formatter: goApp.get_org_name_by_id
		},
		{
			name: 'account_id',
			index: 'account_id',
			width: 190,
			classes: 'bold',
			formatter: goApp.get_account_name_by_id
		},
		{
			name: 'type',
			index: 'type',
			width: 190,
			classes: 'bold',
			formatter: goApp.get_type_name_by_id
		},
		{
			name: 'mac',
			index: 'mac',
			width: 190,
			classes: 'bold'
		},
		{
			name: 'info',
			index: 'info',
			width: 190,
			classes: 'bold'
		},
		{
			name: 'created',
			index: 'created',
			width: 160,
			align: 'center',
			title: false,
			classes: 'datetime',
			formatter: goApp.formatTime
		},
		{
			name: 'active',
			index: 'active',
			width: 70,
			formatter: function(bValue)
			{
				return bValue=="Y" ? 'Yes' : 'No';
			},
			align: 'center'
		}
	],
	datatype: function(oParams)
	{
		goApp.storePageSize(oParams.rows); // store used page size
		
		var
			oRequest	= new Request('/json/device'),
			oCommand	= {
				type: 'select',
				needTotalSize: true,
				needFields: !goApp.validator,
				fields: ['id', 'name', 'account_id', 'organization_id', 'info', 'password', 'created', 'active', 'type', 'mac'],
				page: {
					index: oParams.page - 1,
					size: oParams.rows * 1
				},
				orderBy: [{
					field: oParams.sidx,
					order: oParams.sord
				}]
			};
			
		if (goApp.searchSample != '')
			oCommand.filter	= goApp.getSearchFilter(['name']);
		
		oRequest.add(oCommand);
		oRequest.send(goApp.processSelectResponse, goApp.processFailedRequest);
	},
	jsonReader: {
		repeatitems: false
	},
	gridComplete: function()
	{
		$('#page-action-buttons').removeClass('ui-helper-hidden');
		$('#button-create').button('option', 'disabled', false);
		goApp.processSelection();
	},
	onSelectAll: function()
	{
		goApp.processSelection();
	},
	onSelectRow: function()
	{
		goApp.processSelection();
	},
	ondblClickRow: function(sId)
	{
		$('#grid').jqGrid('resetSelection').jqGrid('setSelection', sId, true);
		goApp.showEditDialog();
	}
});

// correct layout
goApp.removeTextNodes('page-action-buttons');

/**
 * Button 'Create'
 */
$('#button-create').button({
	disabled: true,
	icons: {
		primary: 'ui-icon-plus'
	}
}).click(function() {
	goApp.showCreateDialog();
});

/**
 * Button 'Edit'
 */
$('#button-edit').button({
	disabled: true,
	icons: {
		primary: 'ui-icon-pencil'
	}
}).click(function() {
	goApp.showEditDialog();
});

/**
 * Button 'Activate'
 */
$('#button-activate').button({
	disabled: true,
	icons: {
		primary: 'ui-icon-play'
	}
}).click(function() {
	goApp.changeActiveStatus("Y");
});

/**
 * Button 'Inactivate'
 */
$('#button-inactivate').button({
	disabled: true,
	icons: {
		primary: 'ui-icon-pause'
	}
}).click(function() {
	goApp.changeActiveStatus("N");
});

/**
 * Button 'Delete'
 */
$('#button-delete').button({
	disabled: true,
	icons: {
		primary: 'ui-icon-trash'
	}
}).click(function() {
	goApp.confirmDelete();
});

/**
 * Button 'Details'
 */
$('#button-details').button({
	disabled: true,
	icons: {
		primary: 'ui-icon-person',
		secondary: 'ui-icon-triangle-1-s'
	}
});

/**
 * Controller for "Details" menu button
 */
goApp.detailsMenu	= new ButtonMenu('button-details', 'button-details-menu');
$('#menu-item-lines').click(function() {
	$.window({
		title: "Lines for device ID:" + goApp.currentDeviceId,
		url: goApp.currentHostPort + "/admin/pbxlines/"+ goApp.currentDeviceId,
		width: 800,
		height: 450,
		scrollable: false
	     });
});




/**
 * Dialog to create/edit account
 */
$(goApp.editorSelector = '#account').dialog({
	autoOpen: false,
	modal: true,
	draggable: true,
	resizable: false,
	width: 430
});

$('#account').submit(function()
{
	return goApp.performEditorOperation();
});

new FormButtons({
	targetId: 'edit-buttons',
	buttons: [
		{
			label: '',
			id: 'button-submit',
			onclick: function()
			{
				$('#account').submit();
				return false;
			}
		},
		{
			label: 'Cancel',
			onclick: function()
			{
				$(goApp.editorSelector).dialog('close');
				return false;
			}
		}		
	]
});

/**
 * Performs additionally processing of 'select' command
 * @param {Object} Command result
 */
goApp.handleSelectResponse	= function(oResult)
{
	if (oResult.fields && !this.validator)
		this.validator	= this.getValidator(oResult.fields, {
			name: {
				max_length: 'Name length must be no more than %val% characters.',
				min_length: 'Name length must be at least %val% characters.',
				required:   'Name is expected.' 
			}
		});
};

/**
 * Process selection in grid
 */
goApp.processSelection	= function()
{
	var
		oGrid	= $('#grid'),
		oData	= oGrid.jqGrid('getGridParam', 'userData'),
		aIDs	= oGrid.jqGrid('getGridParam', 'selarrrow');
		
	function hasActiveValue(bValue)
	{
		var nIndex	= aIDs.length;
		
		while (--nIndex >= 0)
			if (oData[aIDs[nIndex]].active == bValue)
				return true;
			
		return false;
	}
	
	$('#button-edit').button('option', 'disabled', aIDs.length != 1);
	$('#button-activate').button('option', 'disabled', !hasActiveValue("N"));
	$('#button-inactivate').button('option', 'disabled', !hasActiveValue("Y"));
	$('#button-details').button('option', 'disabled', aIDs.length != 1);
	$('#button-delete').button('option', 'disabled', !aIDs.length);
	goApp.detailsMenu.hide();
	
	goApp.currentDeviceId	= aIDs.length == 1 ? aIDs[0] : null;
};

/**
 * Editor mode
 */
goApp.isUpdate	= null;

/**
 * Executes editor operation and closes dialog
 * @return {Boolean} false always
 */
goApp.performEditorOperation	= function()
{
	if (goApp[goApp.isUpdate ? 'updateOrganization' : 'createOrganization']())
		$(goApp.editorSelector).dialog('close');
		
	return false;
}

/**
 * Shows dialog to create new user
 */
goApp.showCreateDialog	= function()
{
	goApp.isUpdate	= false;
	
	// setup dialog
	var oForm	= $(goApp.editorSelector);
	oForm.dialog('option', 'title', goApp.strings.createAccount);
	$('#button-submit').button('option', 'label', goApp.strings.create);

	// clear fields
	$('#name,#account_id,#organization_id,#email,#info,#password,#type,#mac').val('');
 	
	// open dialog
	oForm.dialog('open');
	$('#name').focus();
};

/**
 * Shows dialog to update user properties
 */
goApp.showEditDialog	= function()
{
	goApp.isUpdate	= true;
	
	// setup dialog
	var
		oForm	= $(goApp.editorSelector),
		oGrid	= $('#grid'),
		oData	= oGrid.jqGrid('getGridParam', 'userData')
			[oGrid.jqGrid('getGridParam', 'selarrrow')[0]];
	
	oForm.dialog('option', 'title', goApp.strings.editAccount);
	$('#button-submit').button('option', 'label', goApp.strings.update);
	
	// fill form
	this.editedId	= oData.id;
	
	$('#name').val(oData.name);
	$('#organization_id').val(oData.organization_id);
	$('#account_id').val(oData.account_id);
	$('#mac').val(oData.mac);
	$('#type').val(oData.type);
	$('#info').val(oData.info);
	$('#password').val(oData.password);
	
	// open dialog
	oForm.dialog('open');
	$('#name').focus();
};

/**
 * Collects and checks data to create or update contact
 * @return {Object} Data or null if some values are invalid
 */
goApp.collectFormData	= function()
{
	var oData	= new FormData(this.validator);

	oData.put('name', true);
	oData.put('organization_id', true);
	oData.put('account_id', true);
	oData.put('info', true);
	oData.put('password', true);
	oData.put('type', true);
	oData.put('mac', true);
	
	return oData.get();
};

/**
 * Checks data and sends command to create user account
 */
goApp.createOrganization = function()
{
	var oData	= this.getFormData();
	if (!oData)
		return false;
	
	goApp.openWait();
	
	// send request
	var oRequest	= new Request('/json/device');
	oRequest.add({
		type: 'create',
		data: oData
	});
	oRequest.send(this.processCreateResponse, this.processEditorFault);
	
	return true;
};

/**
 * Processes response to command 'create'
 * @param {Object} oResponse Response object
 */
goApp.processCreateResponse	= function(oResponse)
{
	goApp.closeWait();
	
	try
	{
		var oResult	= oResponse.commands[0];
		
		goApp.checkCommandType(oResult, 'create');
			
		if (oResult.errors)
		{
			if (goApp.needAuthorization(oResult))
				return;
				
			$(goApp.editorSelector).dialog('open');
			
			var
				oError	= oResult.errors[0],
				oAlert	= new MessageBox('warning', goApp.getErrorMessage(oError))
					.open();
					
			if (oError.field)
				oAlert.one('dialogclose', function()
				{
					$('#' + oError.field).focus().select();
				});
			return;
		}
		
		// add record
		var oGrid	= $('#grid');
		oGrid.jqGrid('addRowData', oResult.data.id, oResult.data, 'last');
		goApp.addRecord(oResult.data.id, oResult.data, oGrid);
	}
	catch (oException)
	{
		new MessageBox('error', oException.message).open();
	}
};

/**
 * Checks data and sends command to update user account
 */
goApp.updateOrganization = function()
{
	var oData	= this.getFormData();
	if (!oData)
		return false;
	
	goApp.openWait();
	
	// send request
	var oRequest	= new Request('/json/device');
	oRequest.add({
		type: 'update',
		id: goApp.editedId * 1,
		data: oData
	});
	oRequest.send(this.processUpdateResponse, this.processEditorFault);
	
	return true;
};

/**
 * Processes response to command 'update'
 * @param {Object} oResponse Response object
 */
goApp.processUpdateResponse	= function(oResponse)
{
	goApp.closeWait();
	
	try
	{
		var oResult	= oResponse.commands[0];
		
		goApp.checkCommandType(oResult, 'update');
			
		if (oResult.errors)
		{
			if (goApp.needAuthorization(oResult))
				return;
			
			var oError	= oResult.errors[0];
			
			if (oError.code == goApp.errors.ERR_RECORD_LOST)
				$('#grid').trigger('reloadGrid');
			else
				$(goApp.editorSelector).dialog('open');

			var oAlert	= new MessageBox('warning', goApp.getErrorMessage(oError))
				.open();
				
			if (oError.field)
				oAlert.one('dialogclose', function()
				{
					$('#' + oError.field).focus().select();
				});
				
			return;
		}
		
		// update record
		$('#grid').jqGrid('setRowData', oResult.id, oResult.data);
		goApp.storeProperties(oResult.id, oResult.data);
	}
	catch (oException)
	{
		new MessageBox('error', oException.message).open();
	}
};

/**
 * Sends request to change 'active' status
 * @param {Boolean} New value of 'active' property
 */
goApp.changeActiveStatus	= function(bValue)
{
	var
		oGrid	= $('#grid'),
		oData	= oGrid.jqGrid('getGridParam', 'userData'),
		aIDs	= oGrid.jqGrid('getGridParam', 'selarrrow'),
		oRequest	= new Request('/json/device');
		
	$.each(aIDs, function(nIndex, sId) {
		if (oData[sId].active != bValue)
			oRequest.add({
				type: 'update',
				id: sId,
				data: { active: bValue}
			});
	});
	
	if (oRequest.hasCommands())
	{
		goApp.openWait();
		oRequest.send(this.processActivationResponse, this.processOperationFault);
	}
};

/**
 * Processes response to command 'update' to (in)activatу account(s)
 * @param {Object} oResponse Response object
 */
goApp.processActivationResponse	= function(oResponse)
{
	goApp.closeWait();
	
	var
		oGrid	= $('#grid'),
		nLost	= 0,
		aErrors	= [];
	
	$.each(oResponse.commands, function(nIndex, oResult)
	{
		try
		{
			if (oResult.errors)
			{
				if (goApp.needAuthorization(oResult))
					return;
				
				if (oResult.errors[0].code == goApp.errors.ERR_RECORD_LOST)
					nLost++;
				else // other errors
					aErrors.push(goApp.getErrorMessage(oResult.errors[0]));
			}
			else if (oResult.type != 'update')
			{
				aErrors.push(goApp.texts.errors.unexpectedCommand);
			}
			else // update record
			{
				oGrid.jqGrid('setRowData', oResult.id, oResult.data);
				goApp.storeProperties(oResult.id, oResult.data, oGrid);
			}
		}
		catch (oException)
		{
			aErrors.push(oException.message);
		}
	});
	
	if (nLost)
	{
		$('#grid').trigger('reloadGrid');
		aErrors.unshift(goApp.strings.modifiedLost);
	}
	
	if (aErrors.length)
		new MessageBox('warning', aErrors.join('<br /><br />')).open();
		
	goApp.processSelection();
};

/**
 * Shows message box to confirm delete accounts
 */
goApp.confirmDelete	= function()
{
	new MessageBox('question', this.strings.confirmDelete, {
		Yes: function()
		{
			$(this).dialog('close');
			goApp.deleteOrganization();
		},
		No: function()
		{
			$(this).dialog('close');
		}
	}).open();
};

/**
 * Requests server to delete selected accounts
 */
goApp.deleteOrganization	= function()
{
	var
		aIDs	= $('#grid').jqGrid('getGridParam', 'selarrrow'),
		oRequest	= new Request('/json/device');
		
	$.each(aIDs, function(nIndex, sId) {
		oRequest.add({
			type: 'delete',
			id: sId * 1
		});
	});
	
	if (oRequest.hasCommands())
	{
		goApp.openWait();
		oRequest.send(this.processDeleteResponse, this.processOperationFault);
	}	
};

/**
 * Process response to 'delete' commands
 * @param {Object} oResponse Response object
 */
goApp.processDeleteResponse	= function(oResponse)
{
	goApp.closeWait();
	
	var
		aErrors	= [],
		nDeleted	= 0;
	
	$.each(oResponse.commands, function(nIndex, oResult)
	{
		try
		{
			if (oResult.type != 'delete')
			{
				aErrors.push(goApp.texts.errors.unexpectedCommand);
			}
			else if (oResult.errors)
			{
				if (goApp.needAuthorization(oResult))
					return;
				
				aErrors.push(goApp.getErrorMessage(oResult.errors[0]));
			}
			else
			{
				nDeleted++;
			}
		}
		catch (e)
		{
			aErrors.push(e.toString());
		}
	});
	
	if (nDeleted)
		$('#grid').trigger('reloadGrid');
	
	if (aErrors.length)
		new MessageBox('warning', aErrors.join('<br /><br />')).open();
};