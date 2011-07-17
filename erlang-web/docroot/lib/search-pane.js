/**
 * @class Search pane to use with grid
 */

/**
 * @constructor Creates new search pane instance
 * @param {String} sBoxId ID of container for search pane
 * @param {Function} fOnChange Handler is to be called when sample is changed
 */
function SearchPane(sBoxId, fOnChange)
{
	var
		oSelf	= this,
		sButtonId	= sBoxId + '-button',
		sInputId	= sBoxId + '-input';

	/**
	 * Timeout to call change handler
	 */
	this.timeout	= null;
	
	/**
	 * Last sample hassed to handler
	 */
	this.sample	= '';
	
	// create layout
	$('#' + sBoxId)
		.addClass('search-pane')
		.html([
			'<span class="search-pane-label">',
			this.strings.search,
			': </span>',
			'<input id="',
			sInputId,
			'" type="text" size="32" />',
			'<img id="',
			sButtonId,
			'" src="/images/delete.gif" style="visibility:hidden" title="',
			this.strings.clear,
			'" />'
		].join(''));
	
	// setup input
	$('#' + sInputId).keyup(function()
	{
		var sValue	= $.trim($(this).val());
		
		if (sValue == oSelf.sample)
			return;
		
		clearTimeout(oSelf.timeout);
		oSelf.sample	= sValue;
		oSelf.timeout	= setTimeout(function()
		{
			fOnChange(sValue);
		}, 1000);
		
		$('#' + sButtonId).css('visibility', sValue == '' ? 'hidden' : 'visible');
	});
	
	// setup clear button
	$('#' + sButtonId).click(function()
	{
		$(this).css('visibility', 'hidden');
		clearTimeout(oSelf.timeout);
		oSelf.sample	= '';
		$('#' + sInputId).val('').focus();
		fOnChange('');
	});
}

var oProto	= SearchPane.prototype;

/**
 * Set of strings
 */
oProto.strings	= {
	clear: 'Clear filter',
	search: 'Search'
};
