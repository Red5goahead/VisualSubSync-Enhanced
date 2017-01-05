// Italian Subs Addicted typos v2.5
// by kickaha
// based on Nathbot's
// Translated by rock3r

LoadScript("common/itasa.js");

VSSPlugin = {

	// ----- Plugin constant -----
	Name : 'ITASA - Common errors',
	Description : 'Checks the presence of common errors (developed by kickaha for italiansubs.net)',
	Color : 0x25737D,
	Message : 'Error in sub:',
	ParamDisableCheckAccentedLetter : { Value : 1, Unit : '[0=False 1=True]' },

	// ----- HasError method called for each subtitle during the error checking -----
	// If there is an error on CurrentSub return a string containing the error description.
	// Otherwise return an empty string.
	// Don't forget that PreviousSub and NextSub can be null
	HasError : function(CurrentSub, PreviousSub, NextSub)
	{
		if (CurrentSub.Text == "") return 'No empty lines allowed. Delete manually';
		var acc = (this.ParamDisableCheckAccentedLetter.Value == 0) ? true : false;
		var res = CheckItasaRules(CurrentSub, PreviousSub, NextSub, false, acc);
		if (res == null)
			return '';
		return res.msg;
	},

	FixError : function(CurrentSub, PreviousSub, NextSub)
	{
		var acc = (this.ParamDisableCheckAccentedLetter.Value == 0) ? true : false;
		var res = CheckItasaRules(CurrentSub, PreviousSub, NextSub, true, acc);
		if (res == null)
			return;
		if (res.fix != null)
			CurrentSub.Text = res.fix;
	}

}
