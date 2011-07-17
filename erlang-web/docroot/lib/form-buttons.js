/**
 * Class Creates ordered set of buttons are to be used in forms
 */

/**
 * @object {TFormButtonParams} Parameters for FormButtons object
 * @property {String} targetId	ID of target container
 * @property {TButtonParams[]} buttons Set of buttons is to be shown in form
 */

/**
 * @object {TButtonParams} Button properties
 * @param {String} id Button id; optional
 * @param {String} label Button text
 * @param {Function} onclick Click handler
 */

/**
 * @constructor Adds JQ-styled buttons in container
 * @param {TFormButtonParams} Button set parameters
 */
function FormButtons(oParams)
{
	this.instanceId	= 'fb_' + this.counter.number++;
	
	$('#' + oParams.targetId).html(this.getButtonsHTML(oParams.buttons)
		+ this.getSubmitHTML());
	
	this.setupButtons(oParams.buttons);
}

var oProto	= FormButtons.prototype;

/**
 * Instance counter
 */
oProto.counter	= {number: 0};

/**
 * Gets ID for button
 * @param {Integer} nIndex Button index
 * @return {String} ID
 */
oProto.getButtonId	= function(nIndex)
{
	return this.instanceId + '_' + nIndex;
};

/**
 * Gets HTML for set of buttons
 * @param {TButtonParams[]} aButtons Button configs
 * @return {String} HTML
 */
oProto.getButtonsHTML	= function(aButtons)
{
	var
		oButton,
		nIndex	= 0,
		nCount	= aButtons.length,
		aBuff	= [];
		
	while (nIndex < nCount)
	{
		oButton	= aButtons[nIndex];
		
		aBuff.push('<button id="');
		aBuff.push(oButton.id || this.getButtonId(nIndex));
		aBuff.push('">');
		aBuff.push(oButton.label);
		aBuff.push('</button>');
		
		nIndex++;
	}
	
	return aBuff.join('');
};

/**
 * Gets HTML for hidden submit input
 * @return {String} HTML
 */
oProto.getSubmitHTML	= function()
{
	return $.support.submitBubbles // false in IE
		? ''
		: '<input type="submit" class="form-button-submit" tabindex="-1" />';
};

/**
 * Configure buttons
 * @param {TButtonParams[]} aButtons Button configs
 */
oProto.setupButtons	= function(aButtons)
{
	var
		oItem, oButton,
		nIndex	= 0,
		nCount	= aButtons.length;
		
	while (nIndex < nCount)
	{
		oItem	= aButtons[nIndex];
		oButton	= $('#' + (oItem.id || this.getButtonId(nIndex))).button();
		oItem.onclick && oButton.click(oItem.onclick);
			
		nIndex++;
	}
};