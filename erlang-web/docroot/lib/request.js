/**
 * @constructor Creates new request object
 */
function Request(sURL)
{
	/**
	 * @property {String} URL to send request
	 */
	this.URL	= sURL;
	
	/**
	 * @property {Object[]} List of acoomands are to be sent
	 */
	this.commands	= [];
}

var oProto	= Request.prototype;

/**
 * Registers command is to be sent
 * @param {oCommand} oCommand Command description
 * @return {Request} Request object
 */
oProto.add	= function(oCommand)
{
	if (!oCommand.hasOwnProperty('type') || !oCommand.type )
		throw new Error('Type is not defined in command ' + $.param(oCommand));
		
	this.commands.push(oCommand);
	
	return this;
};

/**
 * Determines whether request contains commands
 * @return {Boolean} true if there are added commands
 */
oProto.hasCommands	= function()
{
	return !!this.commands.length;
};

/**
 * Assign handler and sends request
 * @param {Function} fSuccessHandler Function to handle successfull response
 * @param {Function} fErrorHandler Function to handle failed response; optional
 */
oProto.send	= function(fSuccessHandler, fErrorHandler)
{
	//$.post(this.URL, this.get(), fHandler, 'json');
	var oRequest	= {
		data: {request: JSON.stringify({commands: this.commands})},
		dataType: 'json',
		success: fSuccessHandler,
		type: 'POST',
		url: this.URL
	};
	
	if (fErrorHandler)
		oRequest.error	= fErrorHandler;
		
	$.ajax(oRequest);
};