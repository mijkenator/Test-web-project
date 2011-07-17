/**
 * Initializes message box dialog with defaults
 */
$(function()
{
	$('#alert').dialog({
		autoOpen: false,
		modal: true,
		draggable: true,
		resizable: false
	});
});

/**
 * Configures and shows message box
 * @param {String} sType Dialog type (error|information|question|warning)
 * @param {String} sMessage Message text
 * @param {Object} oButtons Set of buttons; optional
 */
function MessageBox(sType, sMessage, oButtons)
{
	$('#alert').dialog('option', 'title',
		sType.replace(/^./, function(sText) { return sText.toUpperCase() }))
		.dialog('option', {
			buttons: oButtons || this.defaultButtons,
			width: 400
		});
	$('#alert-icon').attr('class', 'message-box-icon ' + sType);
	
	$('#alert-message').html(sMessage || '&nbsp;');
	$('#alert-message *:first-child').css('margin-top', '0px');
	$('#alert-message *:last-child').css('margin-bottom', '0px');
	
	// Handle Enter key if there is only one button
	oButtons	= oButtons || this.defaultButtons;
	var nCount	= 0;
	for (var sKey in oButtons) if (oButtons.hasOwnProperty(sKey))
	{
		if (nCount > 1)
			return;
		else
			nCount++;
	}
	
	var	oEnterHandler	= function(oEvent)
	{
		if (oEvent.keyCode == 13)
		{
			$('#alert').dialog('close');
			return false;
		}
		
		return true;
	};
	
	$(document.body).one('keydown', oEnterHandler);
	
	$('#alert').bind('dialogclose', function()
	{
		$(document.body).unbind('keydown', oEnterHandler);
	});

};

var oProto	= MessageBox.prototype;

/**
 * Set of default buttons
 * @private
 */
oProto.defaultButtons	= {
	Ok: function()
	{
		$(this).dialog('close');
	}
};

/**
 * Confirures dialog if it's necessary and opens it
 * @param {Object} oParams Any options acceptable by JQuery dialog
 * @return {Object} Reference to Jquery dialog
 */
oProto.open	= function(oParams)
{
	var oBox	= $('#alert');
	
	if (oParams)
		for (var sProp in oParams) if (oParams.hasOwnProperty(sProp))
			oBox.dialog('option', sProp, oParams[sProp]);
			
	return oBox.dialog('open');
};