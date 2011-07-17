/**
 * @class Form data collector.
 * Use 'put' method to check and collect value.
 * Use 'get' method to get collected data.
 */

/**
 * @constructor Creates new validator
 * @param {Validator} Data validator object
 */
function FormData(oValidator)
{
	this.validator	= oValidator;
	this.data	= {};
}

var oProto	= FormData.prototype;

/**
 * Gets and checks field value from the form
 * @param {String} sField Field name
 * @param {Boolean} bRequired Flag marking field as required
 * @public
 */
oProto.put	= function(sField, bRequired)
{
	this.data[sField]	=
		this.validator.check(sField, $('#' + sField).val(), !!bRequired)
};

/**
 * Adds value without validation
 * @param {String} sField Field name
 * @param {Mixed} vValue Field value
 * @public
 */
oProto.add	= function(sField, vValue)
{
	this.data[sField]	= vValue;
};

/**
 * Gets collected and checked data
 * @return {Object} Data
 * @public
 */
oProto.get	= function()
{
	return this.data;
};

/**
 * Checks whether data set has at least one nonempty value
 * @param {String} sMessage Error message
 */
oProto.checkEmptySet	= function(sMessage)
{
	for (var sField in this.data) if (this.data.hasOwnProperty(sField))
		if (typeof this.data[sField] != 'boolean'
			&& this.data[sField] != ''
			&& this.data[sField] != null)
			return;
		
	this.validator.throwEmptyError(sMessage);
};