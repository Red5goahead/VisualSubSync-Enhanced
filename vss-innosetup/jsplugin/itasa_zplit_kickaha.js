// Redux 1.0 by rock3r
// -----------------------------------------------
// Based upon Italian Subs Addicted split sub v2.5
// by kickaha
// -----------------------------------------------
// Version 1.0 (Jul 12nd, 2012)
// 		* Code translation
//		* Minor refactorings

LoadScript("common/itasa.js");

VSSPlugin = {

	// VSS Constants
	Name : 'ITASA - Row splitter',
	Description : 'Splits or joins rows with the provided parameters (developed by kickaha and rock3r for italiansubs.net)',
	Color : 0x00C0C0,
	Message : 'Row splitter :',

	// Parameters
	// ParamMaxRighe   : { Value :  2, Unit : 'Characters' },	// Max lines - fixed at 2, leaving it parametric is a mess - do we really need it?
	ParamMaxPerRiga : { Value : 45, Unit : 'Characters' },	// Maximum number of characters per row
	ParamMinPerRiga : { Value : 40, Unit : 'Characters' },	// Doesn't split a row with less than this number of characters
	ParamMaxSbilancia : { Value : 15, Unit : 'Characters' },	// Maximum row length unbalancement allowed when splitting

	// Check rows
	HasError : function(CurrentSub, PreviousSub, NextSub) {
		var current_sub = CurrentSub.Text;
		
		var credits_regex = /(?:Traduzione:|Revisione:|Resynch:|Traduzione e synch:|Synch\s?fix:)\s.+((?:Resynch:|Synch\s?fix:)?\s?.+)?/i;
		
		// Check that this isn't a credits row (ItaSA-style)
		if (credits_regex.test(current_sub)) {
			// We won't resplit credits.
			return '';
		}
		
		var fixed_sub = SplitRows(current_sub, this.ParamMaxPerRiga, this.ParamMaxSbilancia, this.ParamMinPerRiga);
		if (fixed_sub == null) return '';
		if (current_sub == fixed_sub.sub && fixed_sub.res == '') return '';
		return (current_sub == fixed_sub.sub ? 'no edits' : 'reformatted subtitle') +
		    (fixed_sub.res == '' ? '' : ' - ' + fixed_sub.res);
	},

	// Resplit rows
	FixError : function(CurrentSub, PreviousSub, NextSub) {
		var current_sub = CurrentSub.Text;
		
		var credits_regex = /(?:Traduzione:|Revisione:|Resynch:|Traduzione e synch:|Synch\s?fix:)\s.+(\r\n(?:Resynch:|Synch\s?fix:)?\s?.+)?/i;
		
		// Check that this isn't a credits row (ItaSA-style)
		if (credits_regex.test(current_sub)) {
			// We won't resplit credits.
			return;
		}
		
		var fixed_sub = SplitRows(current_sub, this.ParamMaxPerRiga, this.ParamMaxSbilancia, this.ParamMinPerRiga);
		if (fixed_sub == null) return;
		CurrentSub.Text = fixed_sub.sub;
		return;
	}

}
