/**
 * Working directory
 */
goApp.dir	= 'admin';

/**
 * Page directory
 */
goApp.folder	= 'pages/' + goApp.dir + '/';

/**
 * URL path
 */
goApp.path	= '/' + goApp.dir + '/';

/**
 * Path to default page
 */
goApp.defaultPage	= 'system';

/**
 * Path to login page
 */
//goApp.loginPath	= '/' + goApp.dir;
goApp.loginPath = '/';


goApp.addStrings({
	noContextAccount: 'Account is absent',
	userArea: 'GTKiller member zone'
});

/**
 * Main menu items
 */
goApp.menuItems	= [
	{
		label: 'Data',
		items: [
			{
				label: 'System',
				page: 'system'
			},
			{
				label: 'Resellers',
				page: 'resellers'
			},
			{
				label: 'Organizations',
				page: 'organizations'
			},
			{
				label: 'Admins',
				page: 'orgadmins'
			},
			{
				label: 'Accounts',
				page: 'accounts'
			},
			{
				label: 'Phone numbers',
				page: 'extensions'
			},
			{
				label: 'Devices',
				page: 'devices'
			},
		]
	},
	{
		label: 'Statistics',
		items: [
			{
				label: 'Registered devices',
				page: 'registered-devices'
			}
		]
	},
	{
		label: 'Tools',
		items: [
			{
				label: 'Test',
				page: 'admintest'
			}
		]
	}
];

goApp.texts.errors.contextLost	= 'Selected user account is lost.<br />';

/*
goApp.scripts['wall-messages']	= {
	libs: ['/lib/message-thread.js'],
	callback: 'start'
};

goApp.scripts.contacts	= {
	libs: ['/lib/contact-viewer.js'],
	callback: 'start'
};

*/

/**
 * Contextual (ny user) menu items
 */
goApp.contextItems	= [
	{
		label: 'Contacts',
		page: 'contacts'
	},
	{
		label: 'Call history',
		page: 'call-history'
	},
	{
		label: 'Videomails',
		page: 'videomails'
	}
];

/**
 * Sets page title
 * @param {String} sTitle Page title
 * @param {String} sContextTitle Page title for user context
 */
goApp.setPageTitle	= function(sTitle, sContextTitle)
{
	if (this.contextAccount)
	{
		var sText	= sContextTitle.replace('%user%', this.contextAccount.name);
		
		document.title	= sText + ' - ' + document.title;
		$('#page-title-text').html(sText);
	}
	else
	{
		document.title	= sTitle + ' - ' + document.title;
		$('#page-title-text').html(sTitle);
	}
};

/**
 * Performs app-specific actions on admin logout
 */
goApp.handleAdminLogout	= function()
{
	setTimeout('location="' + goApp.loginPath + '"', 0);
};

/**
 * Finalizes automatic logout
 */
goApp.handleAutoLogout	= goApp.handleAdminLogout;

/**
 * Process selection of context account info
 * @param {Object} oResponse Response object
 */
goApp.processContextSelect	= function(oResponse)
{
	goApp.closeWait();
	
	try
	{
		var oResult	= oResponse.commands[0];
		
		goApp.checkCommandType(oResult, 'select');
			
		if (oResult.errors) // handle errors
		{
			goApp.showInfobox('error',
				goApp.getErrorMessage(oResult.errors[0]));
		}
		else // handle account existance
		if (!(oResult.data && oResult.data.length))
		{
			goApp.setPageTitle(goApp.strings.noContextAccount);
			goApp.showInfobox('warning', goApp.texts.errors.contextLost);
		}
		else // load application
		{
			goApp.contextAccount	= oResult.data[0];
			goApp.load(goApp.currentPage);
			
			// show user context menu
			goApp.createAccountContextMenu();
		}
	}
	catch (oException)
	{
		goApp.showInfobox('error', oException.message);
	}
};

/**
 * Adds menus to manage context account
 */
goApp.createAccountContextMenu	= function()
{
	var	aBuff	= [
		'<div class="menu-section">',
		'<div class="menu-section-title">',
		this.contextAccount.name,
		'</div><div class="menu-section-items">'
		];

	$.each(this.contextItems, function(nIndex)
	{
		if (this.page == goApp.currentPage)
			aBuff.push('<div class="menu-item current-menu-item">',
				this.label, '</div>');
		else
			aBuff.push(
				'<div class="menu-item"><a href="',
				goApp.path,
				this.page,
				'/',
				goApp.contextId,
				'">',
				this.label,
				'</a></div>');
		
		return true;
	});
	
	aBuff.push('</div></div>');
	
	$('#page-menu').append(aBuff.join(''));
};

/**
 * Init application
 */
$(function()
{
	/**
	 * Displays general statistics
	 * @param {String} sKey Statistic name
	 */
	function showPageStatistic(sKey)
	{
		$('#stat-' + sKey.replace(/_/g, '-')).text((goApp.stats || {})[sKey]);
	}

	showPageStatistic('connected_now');
	showPageStatistic('registered_today');
	
	goApp.setupButtonLink('page-logout-link').click(function()
	{
		goApp.logoutAdmin();
	});
	
	goApp.addPrivateAreaLink('user');
	
	// load app-specific content
	var
		oUser	= goApp.getAccount('admin'),
		sPage	= goApp.getPageName();
	
	if (oUser)
	{
		// show account info
		$('#page-user-name').text(oUser.login);
	
		goApp.createMenu(sPage);
	}
	else // user not authorized
	{
		location.replace(goApp.loginPath);
		return;
	}
	
	// request context account from server
	/*if (goApp.getContextId() && /^(call-history|contacts|videomails)$/.test(sPage))
	{
		goApp.currentPage	= sPage;
		goApp.openWait();
		
		var oRequest	= new Request('/json/account');
		oRequest.add({
			type: 'select',
			fields: ['id', 'name'],
			filter: {
				field: 'id',
				match: 'equal',
				value: goApp.contextId
			}
		});
		oRequest.send(goApp.processContextSelect, goApp.processBackgroundFault);
		
		return;
	}
	*/
	goApp.load(sPage);
});

/**
 * Wraps ISO time into span
 * @param {String} sValue Date and time as "YYYY-MM-DD hh:mm:ss"
 * @return {String} Date and time as "YYYY-MM-DD <span>hh:mm:ss</span>"
 */
goApp.getDatetimeHTML	= function(sValue)
{
	return sValue.replace(/^(\S+\s)(\S+)$/, '$1<span>$2</span>')
};