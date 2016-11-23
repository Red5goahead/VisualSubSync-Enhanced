// Trop de lignes
// Dapitch666 - Lords of kobol

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Un-closed tags',
  Description : 'Detects and fixes un-closed tags.',
  Color : 0xff99cc,
  Message : 'Un-closed tag :',

  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----

HasError : function(CurrentSub, PreviousSub, NextSub) {
  	var SubText = CurrentSub.Text;
  	var regOuvert = /<(.)>/
  	regOuvert.exec(SubText);
  	var Tag = regOuvert.exec(SubText);
  	var regFerme = new RegExp('<\\/' + RegExp.$1 + '>');
  	if (SubText.match(regOuvert) && !SubText.match(regFerme))
  	{
  		return ('<' + RegExp.$1 + '>');
    } else {
    	return '';
    }
  },

FixError : function(CurrentSub, PreviousSub, NextSub) {
	var SubText = CurrentSub.Text;
	var regOuvert = /<(.)>/
  	regOuvert.exec(SubText);
  	CurrentSub.Text += '</' + RegExp.$1 + '>';
	}
}
