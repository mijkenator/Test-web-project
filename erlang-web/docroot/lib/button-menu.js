/**
 * Configures button showing menu.
 * IMPORTANT: parent box MUST have position 'relative'
 * @param {String} sButtonId ID of button element
 * @param {String} sMenuId ID of menu element
 */
function ButtonMenu(sButtonId, sMenuId)
{
	try
	{
		this.buttonId	= sButtonId;
		this.menuId	= sMenuId;

		function openMenu(oEvent)
		{
			var
				oButton	= $('#' + sButtonId),
				oMenu	= $('#' + sMenuId),
				oOffset	= oButton.offset(),
				nWidth	= Math.max(oButton.outerWidth(), oButton.width()),
				bIsIE	= '\v' == 'v';
				
			oMenu
				.css({
					top: oButton[0].offsetTop + (oButton.outerHeight() || oButton.height()),
					left: oButton[0].offsetLeft
				})
				.show();
	
			// correct vert position if there is no space to menu below the button
			if (oMenu.offset().top + oMenu[0].offsetHeight
				+ (bIsIE ? document.documentElement.scrollTop : 0) // menu bottom line
				> 
				Math.max(document.documentElement.scrollTop, document.body.scrollTop)
				+ document.documentElement.clientHeight)
			{
				oMenu.css({
					top: -oMenu[0].offsetHeight
				});		
			}

			if (Math.max(oMenu.outerWidth(), oMenu.width()) < nWidth)
				oMenu.css({width: nWidth});
			
			oEvent.stopPropagation();
			oEvent.cancelBubble	= true;

			$(document).one('click', closeMenu);
		}
		
		function closeMenu()
		{
			$('#' + sMenuId).hide();
			$('#' + sButtonId).one('click', openMenu);
		}
		
		// setup elements
		this.getButton().one('click', openMenu);
		this.getMenu().addClass('button-menu');
		$('#' + sMenuId + '>*')
			.mouseenter(this.handleMouseEnter)
			.mouseleave(this.handleMouseLeave);
	}
	catch (e)
	{
		console.log ? console.log(e.message) : alert(e.message);
	}
};

var oProto	= ButtonMenu.prototype;

/**
 * Gets button element
 * @return {HTMLElement} Button
 */
oProto.getButton	= function()
{
	return $('#' + this.buttonId);
};

/**
 * Gets menu element
 * @return {HTMLElement} Button
 */
oProto.getMenu	= function()
{
	return $('#' + this.menuId);
};

/**
 * Adds hover class to element
 */
oProto.handleMouseEnter	= function()
{
	$(this).addClass('ui-state-hover');
};

/**
 * Removes hover class from element
 */
oProto.handleMouseLeave	= function()
{
	$(this).removeClass('ui-state-hover');
};

/**
 * Hides menu
 */
oProto.hide	= function()
{
	$(document).trigger('click');
	this.getMenu().hide();
};