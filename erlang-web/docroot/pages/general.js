var goApp	= {
	folder: 'pages/',
	errors: {
		ERR_DB_FAULT: 100,
		ERR_NO_COMMAND_TYPE: 101,
		ERR_COMMAND_UNKNOWN: 102,
		ERR_AUTHORIZATION_FAULT: 103,
		ERR_USER_INACTIVE: 104,
		ERR_INVALID_VALUE: 105,
		ERR_AUTHORIZATION_REQUIRED: 106,
		ERR_INVALID_COMMAND: 107,
		ERR_DUPLICATION: 108,
		ERR_RECORD_LOST: 109,
		ERR_FORBIDDEN: 110,
		ERR_EMAIL_NOT_SENT: 111
	},
	
	inProduction: /\./.test(location.hostname),
	
	/**
	 * Time from last action to logout
	 */
	autoLogoutTime: (/\./.test(location.hostname) ? 15 : 15) * 60 * 1000,
	
	responseErrors: {
		error: 'Transport error.',
		notmodified: 'Data are not modified.',
		parsererror: 'Response parsing error.',
		timeout: 'Request was aborder by timeout.'
	},
	/**
	 * Common set of strings
	 */
	texts: {
		errors: {
			other: 'Error %code%.',
			unexpectedCommand: 'Unexpected command type.'
		},
		validator: {
			format: 'Value doesn\'t fit to format.',
			invalid_value: 'Value is invalid.',
			is_boolean: 'Boolean is expected.',
			is_float: 'Real number is expected.',
			is_integer: 'Integer number is expected.',
			is_string: 'String value is expected.',
			required: 'Value is expected.',
			date: 'Value "%sample%" is not valid date.'
		}
	},
	/**
	 * App-specific set of strings; to be redefined for each page
	 */
	strings: {
		wait: 'Please wait...'
	},
	
	/**
	 * Adds strings to string map
	 * @param {Object} oStrings
	 */
	addStrings: function(oStrings)
	{
		this.completeObject(this.strings, oStrings);
	},
	
	/**
	 * Current page name
	 */
	currentPage: '',
	
	/**
	 * Copies properties from source to target object
	 * @param {Object} oTarget Object is to be completed
	 * @param {Object} oSource Source object
	 * @return {Object} Completed object
	 */
	completeObject: function(oTarget, oSource)
	{
		for (var sKey in oSource) if (oSource.hasOwnProperty(sKey))
			oTarget[sKey]	= oSource[sKey];
			
		return oTarget;
	},
	
	/**
	 * Process failed request
	 * @param {XMLHTTPRequest} Transport object
	 * @param {String} sTextStatus Type label
	 * @param {Object} Exception
	 * @return Dialog
	 */
	processFailedRequest: function(oXHR, sTextStatus, oException)
	{
		return new MessageBox('error', goApp.responseErrors[sTextStatus]
				|| (oException ? oException.toString() : oXHR.status))
			.open();
	},

	/**
	 * Process fault of active operation request
	 * @param {XMLHTTPRequest} Transport object
	 * @param {String} sTextStatus Type label
	 * @param {Object} Exception
	 */
	processOperationFault: function(oXHR, sTextStatus, oException)
	{
		goApp.closeWait();
		goApp.processFailedRequest(oXHR, sTextStatus, oException);
	},
	
	/**
	 * Process fault of background operation request
	 * @param {XMLHTTPRequest} Transport object
	 * @param {String} sTextStatus Type label
	 * @param {Object} Exception
	 */
	processBackgroundFault: function(oXHR, sTextStatus, oException)
	{
		goApp.closeWait();
		goApp.showInfobox('error', goApp.responseErrors[sTextStatus]
				|| (oException ? oException.toString() : oXHR.status));
	},
	
	/**
	 * Shows error message after delay
	 * @param {String} sText Message text
	 */
	showErrorDelayed: function(sText)
	{
		setTimeout(function() {
			new MessageBox('error', '?+!' + sText).open();
		}, 300);
	},
	
	/**
	 * Opens wait dialog
	 * @param {String} sText Message
	 * @param {Integer} nWidth Width of dialog
	 */
	openWait: function(sText, nWidth)
	{
		$('#wait')
			.html(sText || this.strings.wait)
			.dialog('option', 'width', nWidth || 190)
			.dialog('open');
	},
	
	/**
	 * Closes wait dialog
	 */
	closeWait: function()
	{
		$('#wait').dialog('close');
	},
	
	/**
	 * Adds custom error message into string table
	 * @param {Integer} nCode Error code
	 * @param {String} sText Error message
	 */
	addErrorMessage: function(nCode, sText)
	{
		this.texts.errors[nCode]	= sText;
	},
	
	/**
	 * Get error message text
	 * @param {Object} oError Error information
	 * @return {String} Error message
	 */
	getErrorMessage: function(oError)
	{
		var
			sDefault	= this.texts.errors.other.replace('%code%', oError.code),
			vItem	= this.texts.errors[oError.code] || sDefault;
			
		return typeof vItem == 'string'
			? vItem : (vItem[oError.field] || sDefault);
	},
	
	/**
	 * Creates and localizes Validator object
	 * @param {Object[]} Set of field definitions
	 * @param {Object{}} Map of localized messages for fields and constraints
	 */
	getValidator: function(aFields, oMessages)
	{
		var oValidator	= new Validator(aFields, oMessages || {});
		oValidator.localize(this.texts.validator);
		return oValidator;
	},
	
	/**
	 * Checks whether command is expected.
	 * Raises exception if command is unexpected.
	 * @param {Object} oCommand Command
	 * @param {String} sType Expected command type
	 */
	checkCommandType: function(oCommand, sType)
	{
		if (oCommand.type != sType)
			throw new Error(this.texts.errors.unexpectedCommand);
	},
	
	/**
	 * Configures and shows infobox
	 * @param {String} sType Dialog type (error|information|question|warning)
	 * @param {String} sMessage Message text
	 */
	showInfobox: function(sType, sMessage)
	{
		$('#infobox-icon').attr('class', 'message-box-icon ' + sType);
		$('#infobox-message').empty();
		$('#infobox-message').html(sMessage);
		$('#infobox-message *:first-child').css('margin-top', '0px');
		$('#infobox-message *:last-child').css('margin-bottom', '0px');
		$('#infobox').show();
	},
	
	/**
	 * Hides infobox
	 */
	hideInfobox: function()
	{
		$('#infobox').hide();
	},
	
	/**
	 * Removes all text-type child nodes of element
	 * @param {String} sId Element ID
	 */
	removeTextNodes: function(sId)
	{
		$('#' + sId).contents().filter(function() {
			return this.nodeType == 3;
		}).remove();
	},
	
	/**
	 * Collects entity-specific data from main page form.
	 * Raises exception if some value is invalid
	 * @return {Object} Data for create/update operation
	 */
	collectFormData: function()
	{
		alert('Must be overriden');
	},
	
	/**
	 * Gets and validates data from main page form
	 * @return {Object} Validated data of null if some value is wrong
	 */
	getFormData: function()
	{
		try
		{
			return this.collectFormData();
		}
		catch (oException)
		{
			var oError	= this.validator.error;
			if (oError)
			{
				new MessageBox('warning', oError.description)
					.open()
					.one('dialogclose', function() {
						$('#' + oError.field).focus().select();
					});
			}
			else
			{
				new MessageBox('warning', oException.message)
					.open();
			}
			
			return null;
		}
	},
	
	/**
	 * Gets cookie value
	 * @param {String} sName Cookie name
	 * @return {String} Cookie value or undefined
	 */
	getCookie: function(sName)
	{
		var
			sItem,
			aParts	= document.cookie.split('; '),
			nIndex	= aParts.length;
			
		while (--nIndex >= 0)
		{
			sItem	= aParts[nIndex];
			
			if (sItem.indexOf(sName + '=') >= 0)
				return sItem.replace(/^[^=]+=/, '');
		}
		
		return undefined;
	},
	
	/**
	 * Sets cookie
	 * @param {String} sName Cookie name
	 * @param {String} sValue Cookie value
	 */
	setCookie: function(sName, sValue)
	{
		function getDomain()
		{
			var aParts	= location.hostname.split('.');
			
			return aParts;
			return aParts.length > 1
				? ('; domain=.'
				   + aParts[aParts.length - 2] + '.' + aParts[aParts.length - 1])
				: '';
		}
		
		var oDate	= new Date();
		oDate.setFullYear(oDate.getFullYear() + 1);
//alert('SC: ' + sName + '=' + sValue + '; path=/; expires=' + oDate.toGMTString() + getDomain());
		document.cookie	= sName + '=' + sValue
			+ '; path=/; expires=' + oDate.toGMTString() + getDomain();
	},
	
	/**
	 * Removes cookie
	 * @param {String} sName Cookie name
	 */	
	removeCookie: function(sName)
	{
		var oDate	= new Date();
		oDate.setFullYear(oDate.getFullYear() - 50);
		document.cookie	= sName + '=; path=/; expires=' + oDate.toGMTString();
	},
	
	/**
	 * Makes request with logout command
	 * @param {String} sURL Controller address
	 * @param {Function} fHandler Function to process logout
	 */
	logout: function(sURL, fHandler)
	{
		// send request
		var oRequest	= new Request(sURL);
		oRequest.add({
			type: 'logout'
		});
		this.openWait();
		oRequest.send(
			/**
			 * Processes response to command 'logout'
			 * @param {Object} oResponse Response object
			 */		
			function(oResponse)
			{
				goApp.closeWait();
				
				try
				{
					var oResult	= oResponse.commands[0];
					
					goApp.checkCommandType(oResult, 'logout');
						
					if (oResult.errors)
					{
						new MessageBox('warning', goApp.getErrorMessage(oResult.errors[0]))
							.open();
						return;
					}
					
					fHandler();
				}
				catch (oException)
				{
					new MessageBox('error', oException.message).open();
				}
			},
			this.processOperationFault
		);
	},
	
	/**
	 * Makes request to logout user
	 */
	logoutUser: function()
	{
		goApp.logout('/json/account', goApp.handleUserLogout);
	},
	
	/**
	 * Makes request to logout user
	 */
	logoutAdmin: function()
	{
		goApp.logout('/json/admin', goApp.handleAdminLogout);
	},
	
	/**
	 * Gets account properties
	 * @param {String} sType Account type: user|admin
	 * @return {Object} Admin properties
	 */
	getAccount: function(sType)
	{
		return (this.account || (this.account = {}))[sType];
	},
	
	/**
	 * Removes local account info
	 * @param {String} sType Account type: user|admin
	 */
	clearAccount: function(sType)
	{
		this.account[sType]	= null;
	},
	
	/**
	 * Adds handlers to button links
	 * @param {String} sId Element ID
	 * @return {JQObject} JQ Object
	 */
	setupButtonLink: function(sId)
	{
		return $('#' + sId)
			.addClass('button-link')
			.mouseenter(function() {
				$(this).addClass('button-link-hover');
			}).mouseleave(function() {
				$(this).removeClass('button-link-hover');
			});
	},
	
	/**
	 * Converts date and time string into date object
	 * @param {String} sDatetime UTC date and time in ISO format
	 * @return {Date} Object
	 */
	getDateFromUTC: function(sDatetime)
	{
		var
			aParts	= $.trim(sDatetime).split(/[-: ]+/),
			oDate	= new Date();
			
		oDate.setUTCFullYear(aParts[0], aParts[1] - 1, aParts[2]);
		oDate.setUTCHours(aParts[3], aParts[4], aParts[5]);
		
		return oDate;
	},
	
	/**
	 * Converts date and time string into date object
	 * @param {String} sDatetime Local date and time in ISO format
	 * @return {Date} Object
	 */
	getDateFromLocal: function(sDatetime)
	{
		var
			aParts	= $.trim(sDatetime).split(/[-: ]+/),
			oDate	= new Date();
			
		oDate.setFullYear(aParts[0], aParts[1] - 1, aParts[2]);
		oDate.setHours(aParts[3], aParts[4], aParts[5]);
		
		return oDate;
	},
	
	/**
	 * Completes numbers less than 10 with leading zero
	 * @param {String} sValue Value to complete
	 * @return {String} Completed value
	 */
	addZero: function(sValue)
	{
		return (sValue < 10 ? '0' : '') + sValue;
	},
	
	/**
	 * Returns ISO representation nate and time
	 * @param {Date} oDate Date to convert
	 * @return {String} YYYY-MM-DD hh:mm:ss
	 */
	convertDateToISO: function(oDate)
	{
		return [oDate.getFullYear(),
				'-',
				this.addZero(oDate.getMonth() + 1),
				'-',
				this.addZero(oDate.getDate()),
				' ',
				this.addZero(oDate.getHours()),
				':',
				this.addZero(oDate.getMinutes()),
				':',
				this.addZero(oDate.getSeconds())
			   ].join('');
	},
	
	/**
	 * Converts UTF date time into local datetime
	 * @param {String} sDatetime UTC date and time in ISO format
	 * @return {String} Local date and time in ISO format
	 */
	convertUTCToLocal: function(sDatetime)
	{
		return this.convertDateToISO(this.getDateFromUTC(sDatetime));
	},
	
	/**
	 * Removes seconds from date time string
	 * param {String} sValue Date and time in ISO format
	 */
	trimSeconds: function(sValue)
	{
		return sValue.replace(/:\d{2}$/, '');
	},
	
	/**
	 * Converts UTF date time into local datetime
	 * @param {String} sDatetime UTC date and time in ISO format
	 * @return {String} Local date and time in ISO format
	 */
	convertLocalToUTC: function(sDatetime)
	{
		var oDate	= goApp.getDateFromLocal(sDatetime);
		
		return [oDate.getUTCFullYear(),
				'-',
				this.addZero(oDate.getUTCMonth() + 1),
				'-',
				this.addZero(oDate.getUTCDate()),
				' ',
				this.addZero(oDate.getUTCHours()),
				':',
				this.addZero(oDate.getUTCMinutes()),
				':',
				this.addZero(oDate.getUTCSeconds())
			   ].join('');
	},
	
	/**
	 * Determines whether day is possible
	 * @param {String} sDate Date as YYYY-MM-DD
	 * @return {Boolean} true if day is valid
	 */
	isDayPossible: function(sDate)
	{
		var aMatches	= sDate.match(/(\d+)-0?(\d+)-0?(\d+)/);
		
		if (aMatches)
		{
			var oDate	= new Date(aMatches[1], aMatches[2] - 1, aMatches[3]);
			
			return oDate.getFullYear() == aMatches[1]
				&& oDate.getMonth() + 1 == aMatches[2]
				&& oDate.getDate() == aMatches[3];
		}
		
		return false;
	},
	
	/**
	 * Converts duration into hour-min-sec value
	 * @param {Integer} nSec Duration, seconds
	 * @return {Object} Object containg 'hours', 'mins', 'secs'
	 */
	getDuration: function(nSec)
	{
		var	oResult	= {};
	
		function extract(nSize)
		{
			var nCount  = Math.floor(nSec/nSize);

			if (nCount)
				nSec    -= nCount * nSize;
				
			return nCount;
		}
	
		oResult.hours	= extract(3600);
		oResult.mins	= extract(60);
		oResult.secs	= nSec;
	
		return oResult;
	},
	
	/**
	 * Converts duration into hour-min-sec value
	 * @param {Integer} nSec Duration, seconds
	 * @return {String} Formatted duration
	 */
	formatDuration: function(nSecs)
	{
		try
		{
			var
				oDuration	= goApp.getDuration(nSecs),
				aParts	= [];
	
			if (oDuration.secs)
				aParts.unshift(oDuration.secs + ' ' + goApp.strings.secs);
				
			if (oDuration.mins)
				aParts.unshift(oDuration.mins + ' ' + goApp.strings.mins);
			
			if (oDuration.hours)
				aParts.unshift(oDuration.hours + ' ' + goApp.strings.hours);
				
			return aParts.join(' ');
		}
		catch (oError)
		{
			return '?';
		}
	},

	/**
	 * Get formatted duration computed from two dates
	 * @param {String} sValue Is not used
	 * @param {Object} oCol Column config
	 * @param {Object} oData Record
	 * @return {String} Formatted duration
	 */
	formatRangeDuration: function(sValue, oCol, oData)
	{
		var
			oParams	= oCol.colModel.formatoptions,
			nMsecs	= goApp.getDateFromUTC(oData[oParams.endField])
					- goApp.getDateFromUTC(oData[oParams.beginField]);
			
		return goApp.formatDuration(Math.round(nMsecs / 1000));
	},

	/**
	 * Splits path of URL into parts
	 * @param {Boolean} bRemoveDir Flag to temove the first part if it's app directory
	 * @return {String[]} Parts of path
	 */
	getPathParts: function(bRemoveDir)
	{
		var aParts	= location.pathname.replace(/^\/|\/$/g, '').split('/');
		
		if (bRemoveDir && aParts.length && aParts[0] == this.dir)
			aParts.shift();
			
		return aParts;
	},

	/**
	 * Gets current page name
	 * @return {String} page name
	 */
	getPageName: function()
	{
		var aParts	= goApp.getPathParts(true);
		return aParts.length ? aParts[0] : this.defaultPage;
	},

	/**
	 * Creates main page menu
	 * @param {sPage} Page name
	 */
	createMenu: function(sPage)
	{
		var aBuff	= [];
		
		$.each(this.menuItems, function()
		{
			aBuff.push(goApp.getMenuItemHTML(this, sPage));
		});
		
		$('#page-menu').append(aBuff.join(''));
	},
	
	/**
	 * Gets HTML for menu item
	 * @param {Object} oItem Menu item description
	 * @param {String} sPage Page name
	 * @return {String} HTML
	 */
	getMenuItemHTML: function(oItem, sPage)
	{
		if (oItem.local && this.inProduction)
			return '';
			
		if (oItem.items)
			return goApp.getMenuSectionHTML(oItem, sPage);
	
		var bNoContext	= !/\d+$/.test(location.pathname);
		
		return oItem.page == sPage && bNoContext
			? '<div class="menu-item current-menu-item">' + oItem.label + '</div>'
			: '<div class="menu-item"><a href="'
				+ (oItem.url || (goApp.path + oItem.page))
				+ '">'
				+ oItem.label + '</a></div>';
	},
	
	/**
	 * Gets HTML for menu section
	 * @param {Object} oItem Menu item description
	 * @param {String} sPage Page name
	 * @return {String} HTML
	 */
	getMenuSectionHTML: function(oItem, sPage)
	{
		var
			nIndex	= 0,
			nCount	= oItem.items.length,
			aBuff	= [
				'<div class="menu-section">',
				'<div class="menu-section-title">',
				oItem.label,
				'</div><div class="menu-section-items">'
				];
			
			while (nIndex < nCount)
				aBuff.push(this.getMenuItemHTML(oItem.items[nIndex++], sPage));
		
		aBuff.push('</div></div>');
		
		return aBuff.join('');
	},
	
	/**
	 * Adds to footer link to private area
	 * @param {String} sType Account type
	 */
	addPrivateAreaLink: function(sType)
	{
		if (this.getAccount(sType))
			$('#page-footer').append([
				'<a id="footer-',
				sType,
				'-area-link" href="/',
				sType,
				'">',
				goApp.strings[sType + 'Area'],
				'</a>'
			].join(''));
	},
	
	/**
	 * Configures pair of text control (sId) and its hint control (sId-hint)
	 * @param {String} sId ID of text input
	 */
	setupFieldHint: function(sId)
	{
		sId	= '#' + sId;
		var sHintId	= sId + '-hint';
		
		/**
		 * Process blur on control
		 */
		$(sId).blur(function()
		{
			var
				oInput	= $(this),
				sValue	= $.trim(oInput.val());
				
			if (sValue == '')
			{
				oInput.hide();
				oInput.val(sValue);
				$(sHintId).show();
			}
		});
	
		/**
		 * Hides hint control and shows real control
		 */
		$(sHintId)
			.attr('readonly', true)
			.focus(function()
			{
				$(this).hide();
				$(sId).show().focus();
			});		
	},
	
	/**
	 * Shows control and hides related hint control
	 * @param {String} sId Control ID
	 */
	showInsteadHint: function(sId)
	{
		$('#' + sId + '-hint').hide();
		$('#' + sId).show();
	},
	
	/**
	 * Sets cookie registering last access time
	 */
	registerUserAcivity: function()
	{

		this.setCookie('lastAccess', (new Date()).getTime());
		//goApp.setCookie('lastAccess', (new Date()).getTime());
//alert('SET LA1' + goApp.getCookie('lastAccess'));
	},
	
	/**
	 * Checks last access time and logout user if app was incative for a defined time
	 */
	proccessUserActivity: function()
	{
		var	nPrevTime	= goApp.getCookie('lastAccess');
		
//alert('pUA2: '+ nPrevTime);
		if (!/^\d+$/.test(nPrevTime)
			|| new Date().getTime() - nPrevTime * 1 < this.autoLogoutTime)
			return;
		
		/**
		 * Sends logout request
		 * @param {String} sURL Controller address
		 * @param {Function} Scuccessful response handler
		 */
		function sendLogout(sURL, fHandler)
		{
			goApp.autoLogoutExecuting	= true;
			
			var oRequest	= new Request(sURL);
			oRequest.add({
				type: 'logout'
			});
			oRequest.send(fHandler, $.noop);

			goApp.autoLogoutExecuting	= false;
		}
		
//alert('pUA1' + this.getAccount('admin') + ' - ' + this.getAccount('user'));
		if (this.getAccount('admin'))
			sendLogout('/json/admin', this.handleAdminAutoLogout);
		else if (this.getAccount('user'))
			sendLogout('/json/account', this.handleUserAutoLogout);
		else
			this.handleAutoLogout();
	},
	
	/**
	 * Process admin autologout
	 */
	handleAdminAutoLogout: function()
	{
		$('#footer-admin-area-link').remove();
		goApp.clearAccount('admin');
		goApp.proccessUserActivity();
	},
	
	/**
	 * Process user autologout
	 */
	handleUserAutoLogout: function()
	{
		$('#footer-user-area-link').remove();
		goApp.clearAccount('user');
		goApp.proccessUserActivity();
	}
};

goApp.registerUserAcivity();

/**
 * Common error messages
 */
goApp.addErrorMessage(goApp.errors.ERR_AUTHORIZATION_REQUIRED,
	'Authorization is required.<br />'
		+ 'You will be redirected to home page.');
goApp.addErrorMessage(goApp.errors.ERR_FORBIDDEN,
	'Requested operation is forbidden.');
goApp.addErrorMessage(goApp.errors.ERR_INVALID_VALUE,
	'Passed value is invalid.');

/**
 * Configure application
 */
$(function()
{
	// setup requests defaults
	$.ajaxSetup({
		type: 'POST',
		error: goApp.processFailedRequest
	});

	/**
	 * Dialog to block GUI for operations
	 */
	$('#wait').dialog({
		autoOpen: false,
		modal: true,
		draggable: false,
		resizable: false,
		close: function(oEvent, oUI)
		{
			$('.ui-dialog-titlebar', this.parentNode).show(); // shows title bar
		},
		open: function(oEvent, oUI)
		{
			$('.ui-dialog-titlebar', this.parentNode).hide(); // hides title bar
		}
	});
	
	/**
	 * Starts logout timer
	 */
	var nTime	= goApp.getCookie('lastAccess');
	
//alert('GS nTime:' + nTime);

	if (/^\d+$/.test(nTime) && nTime * 1)
	{
//alert('GS Set autologout');
		goApp.autoLogoutTimer	= setInterval(function() {
			goApp.proccessUserActivity();
		}, goApp.autoLogoutTime);
	}
	
	$(document.body).ajaxSend(function(oEvent, oXHR, oSettings) {
		goApp.autoLogoutExecuting || goApp.registerUserAcivity();
	}).click(function() {
		goApp.autoLogoutExecuting || goApp.registerUserAcivity();
	});
});
