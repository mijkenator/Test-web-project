/**
 * @class Data validator.
 * Use 'check' method to check and get filtered value
 */

/**
 * @constructor Creates new validator
 * @param {Object[]} Set of field definitions
 * @param {Object{}} Map of localized messages for fields and constraints
 */
function Validator(aFields, oMessages)
{
	/**
	 * @property {Object} Current error
	 * @public
	 */
	this.error	= {
		field: '',
		description: ''
	};
	
	/**
	 * @property {Object} Field is currently checked
	 * @private
	 */
	this.currentField	= null;
	
	/**
	 * @property {Object} Map fields by name
	 * @private
	 */
	this.fieldMap	= {};
	
	/**
	 * @property {Object} Map of localized error messages
	 * @private
	 */
	this.messages	= {};
	
	var
		oField,
		nIndex	= aFields.length;
		
	while (--nIndex >= 0)
	{
		oField	= aFields[nIndex];
		
		this.fieldMap[oField.name]	= {
			name: oField.name,
			type: oField.type,
			filter: this.getFilter(oField.type),
			constraints: oField.constraints || [],
			messages: oMessages[oField.name] || {}
		};
	}
}

var oProto	= Validator.prototype;

/**
 * Strings are to be used for standard cases
 */
oProto.strings	= {
	format: 'Value doesn\'t fit to format.',
	invalid_value: 'Value is invalid.',
	is_boolean: 'Boolean is expected.',
	is_float: 'Real number is expected.',
	is_integer: 'Integer number is expected.',
	is_string: 'String value is expected.',
	required: 'Value is expected.',
	date: 'Value "%sample%" is not valid date.'
};

/**
 * Mask to check data format
 */
oProto.dateMask	= /^[1,2]\d{3}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[1,2][0-9]|3[0,1]) (?:[0,1][0-9]|2[0-3]):(?:[0-5][0-9]):(?:[0-5][0-9])$/;

/**
 * Mask to trim terminal spaces
 */
oProto.trimMask	= /^\s+|\s+$/g;

/**
 * Mask to trim leading spaces
 */
oProto.zeroMask	= /^0+(?!$)/;

/**
 * Puts localized error messages for errors
 * @param {Object} oMessages Map of default messages for constraint methods
 */
oProto.localize	= function(oMessages)
{
	this.messages	= oMessages;
};

/**
 * Gets type-specific filter
 * @return {Function} Filtering function
 */
oProto.getFilter	= function(sType)
{
	switch (sType)
	{
		case 'datetime':
		case 'string':
			return this.filterString;
		
		case 'integer':
			return this.filterInteger;
		
		case 'float':
			return this.filterFloat;
		
		case 'flag':
		case 'boolean':
			return this.filterBoolean;
		
		default:
			throw new Error('Unexpected type "' + sType + '"');
	}
};

/**
 * Corrects passed string value
 * @param {String} sValue Raw value
 * @return {String} Corrected value
 */
oProto.filterString	= function(sValue)
{
	return ('' + sValue).replace(/^\s+|\s+$/g, '');
};

/**
 * Converts passed string into int value
 * @param {String} sValue Raw value
 * @return {Number} Integer value
 */
oProto.filterInteger	= function(sValue)
{
	return (sValue = ('' + sValue).replace(this.trimMask, '')) == ''
		? sValue
		: (/^-?\d+$/.test(sValue)
			? parseInt(sValue.replace(this.zeroMask, ''))
			: NaN);
};
/**
 * Converts passed string into float value
 * @param {String} sValue Raw value
 * @return {Number} Float value
 */
oProto.filterFloat	= function(sValue)
{
	return (sValue = ('' + sValue).replace(this.trimMask, '')) == ''
		? sValue
		: (/^-?\d+([\.,]\d*?)?$/.test('' + sValue)
			? parseFloat(sValue.replace(this.zeroMask, '').replace(/,/g, '.'))
			: NaN);
};

/**
 * Filters boolean value
 * @param {Boolean} bValue Raw value
 * @return {Boolean} Boolean value
 */
oProto.filterBoolean	= function(bValue)
{
	return bValue;
};

/**
 * Stores error info into validator and throws exception
 * @param {String} sMethod Method name
 * @param {Object} oParams Set of placeholders to put params in message; optional
 */
oProto.throwError	= function(sMethod, oParams)
{
	var sMsg	= this.currentField.messages[sMethod]
		|| this.messages[sMethod]
		|| this.strings[sMethod]
		|| this.messages.invalid_value
		|| this.strings.invalid_value;
	
	if (oParams)
		for (var sName in oParams) if (oParams.hasOwnProperty(sName))
			sMsg	= sMsg.replace('%' + sName + '%', oParams[sName]);
	
	this.error	= {
		field: this.currentField.name,
		description: sMsg
	};
	
	throw new Error('Validation error (field "' + this.currentField.name + '")');
};

/**
 * Stores info about empty data set and throws exception
 * @param {String} sMessage Warning message
 * @public
 */
oProto.throwEmptyError	= function(sMessage)
{
	this.error	= {
		description: sMessage
	};
	
	throw new Error(sMessage);
};

/**
 * Aplies filter and constraints. Throws exception if constraint was failed.
 * Get error info in Validator.error.
 * @param {String} sField Field name
 * @param {Mixed} vValue Value is to be filtered and checked
 * @param {Boolean} bRequired Flag to check value existance; optional
 * @return {Mixed} Filtered and checked value
 */
oProto.check	= function(sField, vValue, bRequired)
{
	this.error	= null;
	
	var oField	= this.currentField = this.fieldMap[sField];
	
	if (!oField)
		throw new Error('Field "' + sField + '" is not defined in select response');
	
	// apply filter
	vValue	= oField.filter.call(this, vValue);
	
	// check for non-empty
	if (bRequired
		&& (vValue === '' || vValue === null || vValue === undefined))
		this.throwError('required');
		
	// check type
	this['is_' + oField.type](vValue);

	
	// check with constraints
	for (var oConstraint, nIndex = 0, nCount = oField.constraints.length;
		nIndex < nCount; )
	{
		oConstraint	= oField.constraints[nIndex++];
		this[oConstraint.method](vValue, oConstraint.data);
	}
	
	return vValue;
};

/**
 * Checks string against datetime regexp
 * @param {String} sValue Value is to be checked
 */
oProto.format	= function(sValue)
{
	this.dateMask.test(sValue) || this.throwError('format');
};

/**
 * Determines whether value has boolean type.
 * @param {Variant} vValue Values is to be checked
 */
oProto.is_boolean	= function(vValue)
{
	if (vValue !== true && vValue !== false)
		this.throwError('is_boolean');
};

/**
 * Determines whether value has integer type.
 * @param {Variant} vValue Values is to be checked
 */
oProto.is_integer	= function(vValue)
{
	if (isNaN(vValue))
		this.throwError('is_integer');
};

/**
 * Determines whether value has float type.
 * @param {Variant} vValue Values is to be checked
 */
oProto.is_float	= function(vValue)
{
	if (isNaN(vValue))
		this.throwError('is_float');
};

/**
 * Determines whether value has string type.
 * @param {Variant} vValue Values is to be checked
 */
oProto.is_datetime	=
oProto.is_string	= function(vValue)
{
	if (typeof vValue != 'string')
		this.throwError('is_string');
};

/**
 * Checks value against exact length
 * @param {String} sValue Value is to be checked
 * @param {Integer} nSize Required value
 */
oProto.length	= function(sValue, nSize)
{
	if (sValue.length != nSize)
		this.throwError('length', {val: nSize});
};

/**
 * Checks value against max value
 * @param {Number} nValue Value is to be checked
 * @param {Integer} nMin Minimal possible value
 */
oProto.max	= function(nValue, nLimit)
{
	if (nValue > nLimit)
		this.throwError('max', {val: nLimit});
};

/**
 * Checks string length against max values
 * @param {String} sValue Value is to be checked
 * @param {Integer} nLimit Maximal possible value
 */
oProto.max_length	= function(sValue, nLimit)
{
	if (sValue.length > nLimit)
		this.throwError('max_length', {val: nLimit});
};

/**
 * Checks value against min value
 * @param {Number} nValue Value is to be checked
 * @param {Integer} nMin Minimal possible value
 */
oProto.min	= function(nValue, nLimit)
{
	if (nValue < nLimit)
		this.throwError('min', {val: nLimit});
};

/**
 * Checks string length against min values
 * @param {String} sValue Value is to be checked
 * @param {Integer} nLimit Minimal possible value
 */
oProto.min_length	= function(sValue, nLimit)
{
	if (sValue.length < nLimit)
		this.throwError('min_length', {val: nLimit});
};


/**
 * Checks value against min length
 * @param {String} sValue Value is to be checked
 * @param {Object} oParams Regexp data
 */
oProto.regexp	= function(sValue, oParams)
{
	(new RegExp(oParams)).test(sValue)
		|| this.throwError('regexp', {sample: sValue});
};

/**
 * Checks date in YYYY-MM-DD format
 * @param {String} sValue Value is to be checked
 * @param {Object} oParams Additional params
 */
oProto.date	= function(sValue, oParams)
{
	var aMatches	= sValue.match(/^(\d+)-0?(\d+)-0?(\d+)$/);
	
	if (aMatches)
	{
		var oDate	= new Date(aMatches[1], aMatches[2] - 1, aMatches[3]);
		
		if (oDate.getFullYear() == aMatches[1]
			&& oDate.getMonth() + 1 == aMatches[2]
			&& oDate.getDate() == aMatches[3])
			return;
	}
	
	this.throwError('date', {sample: sValue});	
};