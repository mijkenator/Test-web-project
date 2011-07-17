goApp.adminStartPage	= '/admin/system';

if (goApp.getAccount('admin'))
	location	= goApp.adminStartPage;

goApp.addErrorMessage(goApp.errors.ERR_AUTHORIZATION_FAULT,
	'Username or password is invalid.<br />Correct it and try again.'); /// 
goApp.addErrorMessage(goApp.errors.ERR_RECORD_LOST,
	'Аccount with passed username is not found.'); ///

goApp.idToFocus	= 'admin-login';
	
/**
 * Configure page
*/
$(function() {
	/**
	 * Setup login form
	 */
	$('#login-form').submit(function()
	{
		return goApp.login();
	});
	
	var
		sLogin	= goApp.getCookie('admin'),
		oLogin	= $('#admin-login');
	
	if (sLogin)
	{
		oLogin.val(sLogin);
		$('#admin-password').focus();
		$('#remember-admin-login').attr('checked', true);
	}
	else
	{
		oLogin.focus();
	}
	
	new FormButtons({
		targetId: 'login-form-buttons',
		buttons: [
			{
				label: 'Log in',
				onclick: function()
				{
					$('#login-form').submit();
					return false;
				}
			}
		]
	});
});

/**
 * Checks login form and try to log user on
 */
goApp.login	= function()
{
	var
		oError,
		sLogin	= $.trim($('#admin-login').val()),
		sPassword	= $.trim($('#admin-password').val());
	
	if (sLogin == '')
		oError	= {
			message: 'Username is expected',
			field: 'admin-login'
		};
	else if (sPassword == '')
		oError	= {
			message: 'Password is expected',
			field: 'admin-password'
		};
	
	if (oError)
	{
		new MessageBox('warning', oError.message)
			.open()
			.one('dialogclose', function() {
				$('#' + oError.field).focus().select();
			});
		return false;
	}

	// process remember login
	if ($('#remember-admin-login').attr('checked'))
		goApp.setCookie('admin', sLogin);
	else
		goApp.removeCookie('admin');
	
	goApp.openWait();
	
	// send request
	var oRequest	= new Request('/json/admin');
	oRequest.add({
		type: 'login',
		login: sLogin,
		password: sPassword
	});
	oRequest.send(goApp.processAdminLoginResponse, goApp.processOperationFault);
	
	return false;
};

/**
 * Processes response to authorization request
 * @param {Object} oResponse Response object
 */
goApp.processAdminLoginResponse	= function(oResponse)
{
	goApp.closeWait();
	
	try
	{
		var oResult	= oResponse.commands[0];
		
		goApp.checkCommandType(oResult, 'login');
			
		if (oResult.errors)
			new MessageBox('warning', goApp.getErrorMessage(oResult.errors[0]))
				.open()
				.one('dialogclose', function()
				{
					$('#admin-login').focus().select();
				});
		else
			location	= oResult.requestedURL || goApp.adminStartPage;
	}
	catch (oException)
	{
		new MessageBox('error', oException.message).open();
	}
};