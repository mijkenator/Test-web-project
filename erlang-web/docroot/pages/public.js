goApp.idToFocus	= '';
goApp.path	= '';

goApp.addStrings({
	adminArea: 'Admin Area',
	userArea: 'My Unison'
});

/**
 * Configure application
 */
$(function()
{
	goApp.addPrivateAreaLink('admin');
	goApp.addPrivateAreaLink('user');
});

/**
 * Finalizes automatic logout
 */
goApp.handleAutoLogout	= $.noop;