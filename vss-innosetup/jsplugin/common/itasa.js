// ---------------------------------------------------------------------------
// ItaSA tools redux 1.0
// by rock3r
// 
// Based upon ITASA tools v2.5 (compatible with VSS v0.9.18.1052)
// by kickaha
// ---------------------------------------------------------------------------
//
// Version 1.1 (Dec 18th, 2012)
//      * Improved song detection regex for row splitter to
//		  avoid getting false positives with #'s inside subtitles
//
// Version 1.0 (Jul 12nd, 2012)
// 		* Code refactoring and translation
// 		* Added row split exceptions for credits (ItaSA-style)



// ---------------------------------------------------------------------------
// Typos (based on Nathbot's)
// ---------------------------------------------------------------------------

// We use a table of rules to define the typography rules
// Each rule is defined by these field :
//   - re : a regular expression
//   - msg : a message to display when the text match
//   - replaceby (optional) : a replace expression use to fix the error
//   - prev (optional) : a regular expression for the previous sub (applied only if exists)
//   - next (optional) : a regular expression for the next sub (applied only if exists)
//   - position (optional) : "F"=first sub, "M"=middle sub, "L"=last sub, "B"=both first and last sub

var ItasaTyposRules1 = new Array(
	// ++++ IMPORTANT ++++ //
	// Please note that all the following corrections assume you're writing in Italian
	
	// dollar
	{ re : / \x24/, msg : "Invalid character: -$-", replaceby : " dollari"},
	{ re : /([^ ])\x24/, msg : "Invalid character: -$-", replaceby : "$1 dollari"}
);

var ItasaTyposRules2a = new Array(
	{ re : /À/, msg : "Accented letter", replaceby: "A'"},
	{ re : /È/, msg : "Accented letter", replaceby: "E'"},
	{ re : /É/, msg : "Accented letter", replaceby: "E'"},
	{ re : /Ì/, msg : "Accented letter", replaceby: "I'"},
	{ re : /Ò/, msg : "Accented letter", replaceby: "O'"},
	{ re : /Ù/, msg : "Accented letter", replaceby: "U'"},
	{ re : /à/, msg : "Accented letter", replaceby: "a'"},
	{ re : /è/, msg : "Accented letter", replaceby: "e'"},
	{ re : /é/, msg : "Accented letter", replaceby: "e'"},
	{ re : /ì/, msg : "Accented letter", replaceby: "i'"},
	{ re : /ò/, msg : "Accented letter", replaceby: "o'"},
	{ re : /ù/, msg : "Accented letter", replaceby: "u'"},
	{ re : /[^A-Za-z0-9Ø\.,;:\-_()!"%&£=\?'\+#@\*\\\/ \n\r<>\[\]\{\}]/, msg : "Invalid character", replaceby : "^"}
);

var ItasaTyposRules2b = new Array(
	{ re : /[^A-Za-z0-9àáèéìíòóùúÀÁÈÉÌÍÒÓÙÚØ\.,;:\-_()!"%&£=\?'\+#@\*\\\/ \n\r<>\[\]\{\}]/, msg : "Invalid character", replaceby : "^"}
);

var ItasaTyposRules3 = new Array(
	// ++++ IMPORTANT ++++ //
	// Please note that all the following corrections assume you're writing in Italian
	
	// italian typo: "ehi"
	{ re : /(\b)HE[IY](\b)/, msg : "It's -EHI-", replaceby: "$1EHI$2"},
	{ re : /(\b)He[iy](\b)/, msg : "It's -Ehi-", replaceby: "$1Ehi$2"},
	{ re : /(\b)he[iy](\b)/, msg : "It's -ehi-", replaceby: "$1ehi$2"},

	// italian typo: "un altro"
	{ re : /([Uu][Nn])'([Aa][Ll][Tt][Rr][Oo])(\b)/, msg : "-Un altro- does not have the apostrophe", replaceby: "$1 $2$3"},

	// italian typo: "qual e'"
	{ re : /(\b)([Qq][Uu][Aa][Ll])'([EeÈè])/, msg : "-Qual e'- does not have the apostrophe", replaceby: "$1$2 $3"},

	// "okay"
	{ re : /(\b)([Oo][Kk])[Aa][Yy](\b)/, msg : "It's -Ok-", replaceby: "$1$2$3"},

	// spaces and tags
	{ re : /(<\s*)(\x5c)(\s*[a-zA-Z]+\s*>)/, msg : "The tag contains a backslash instead of a slash", replaceby: "$1\x2f$3"},
	{ re : /(<)( )+(\x2f? *[a-zA-Z]+ *>)/, msg : "No spaces within a tag", replaceby: "$1$3"},
	{ re : /(<\x2f?)( )+([a-zA-Z]+ *>)/, msg : "No spaces within a tag", replaceby: "$1$3"},
	{ re : /(<\x2f?[a-zA-Z]+)( )+(>)/, msg : "No spaces within a tag", replaceby: "$1$3"},
	 //Uppercased tags
    { re: /(<\x2f?[A-Z]+>)/, msg: "Uppercase letter not allowed into tag", replaceby: "@LOWER@$1@" },

	// No heading .,;:'
	{ re : /^((?:<\x2f?[a-zA-Z]+>)*)[\.,;:']/, msg : "No heading punctuation", replaceby: "$1"},

	// No heading (
	{ re : /\(([\s]|<\x2f?[a-zA-Z]+>)*$/, msg : "No heading open parenthesis", replaceby: "$1"},

	// ../....+ --> ...
	{ re : /([^\.])\.\.([^\.])/, msg : "Suspension mark has three dots", replaceby: "$1...$2"},
	{ re : /([^\.])\.\.$/, msg : "Suspension mark has three dots", replaceby: "$1..."},
	{ re : /([^\.])[\.]{4,}/, msg : "Suspension mark has only three dots", replaceby: "$1..."},

	// Caps after the dot
	{ re : /((?:[^\.][\.])|[\?!])((?:[\s-\(\"]|<\x2f?[a-zA-Z]+>)*)([a-zè])/, msg : "Capital letter after a dot!", replaceby: "$1$2@UPPER@$3@"},
	{ prev : /(([^\.][\.])|[\?!])([\s-\(\"]|<\x2f?[a-zA-Z]+>)*$/, re : /^((?:[\s-\(\"]|<\x2f?[a-zA-Z]+>)*)([a-zè])/, msg : "Capital letter after a dot in preceding line!", replaceby: "$1@UPPER@$2@"},
	{ position : "F", re : /^((?:[\s-\(\"]|<\x2f?[a-zA-Z]+>)*)([a-zè])/, msg : "Capital letter in the first line!", replaceby: "$1@UPPER@$2@"},
	{ position : "B", re : /^((?:[\s-\(\"]|<\x2f?[a-zA-Z]+>)*)([a-zè])/, msg : "Capital letter in the first line!", replaceby: "$1@UPPER@$2@"},

	// Multiple caps
	{ re : /(\b[A-Z])([A-Z]+)([a-z])/, msg : "Repeated capital letter!", replaceby: "$1@LOWER@$2@$3"},

	// Capital letter in between words
	{ re : /(\b[a-z]+)([A-Z])/, msg : "Capital letter within a word!", replaceby: "$1@LOWER@$2@"},

	// italian typo: "Signor" per esteso
	{ re : /(\b)([Ss]ig)\.(re)?(\b)/, msg : "-Signore- is spelled in its entirety", replaceby: "$1$2nor$3"},
	{ re : /(\b)(Mr)\./, msg : "It's -signor-", replaceby: "$1Signor"},
	{ re : /(\b)(mr)\./, msg : "It's -signor-", replaceby: "$1signor"},
	{ re : /(\b)(Mr)(\b)/, msg : "It's -signor-", replaceby: "$1Signor$3"},
	{ re : /(\b)(mr)(\b)/, msg : "It's -signor-", replaceby: "$1signor$3"},
	{ re : /(\b)(Mrs)\./, msg : "It's -signora-", replaceby: "$1Signora"},
	{ re : /(\b)(mrs)\./, msg : "It's -signora-", replaceby: "$1signora"},
	{ re : /(\b)(Mrs)(\b)/, msg : "It's -signora-", replaceby: "$1Signora$3"},
	{ re : /(\b)(mrs)(\b)/, msg : "It's -signora-", replaceby: "$1signora$3"},
	{ re : /(\b)(Miss)(\b)/, msg : "It's -signorina-", replaceby: "$1Signorina$3"},
	{ re : /(\b)(miss)(\b)/, msg : "It's -signorina-", replaceby: "$1signorina$3"},

	// Heading/trailing spaces
	{ re : /^((?:<\x2f?[a-zA-Z]+>)*)( )+/, msg : "No heading whitespaces", replaceby: "$1"},
	{ re : /([\r\n])((?:<\x2f?[a-zA-Z]+>)*)( )+/, msg : "No whitespaces after a newline", replaceby: "$1$2"},
	{ re : /( )+((?:<\x2f?[a-zA-Z]+>)*)$/, msg : "No trailing whitespaces", replaceby: "$2"},
	{ re : /( )+((?:<\x2f?[a-zA-Z]+>)*)([\r\n])/, msg : "No whitespaces before a newline", replaceby: "$2$3"},
	{ re : /([\r\n])+((?:<\x2f?[a-zA-Z]+>)*)$/, msg: "No trailing empty line", replaceby: "$2"},

	// Multiple spaces
	{ re : /( ){2,}/, msg : "No multiple whitespaces", replaceby: " "},
	{ re : /( )+((?:<\x2f?[a-zA-Z]+>)*)( )+/, msg : "No multiple whitespaces", replaceby: "$2 "},

	// Space after a dash
	{ re : /^((?:<\x2f?[a-zA-Z]+>)*)-((?:<\x2f?[a-zA-Z]+>)*)([^ ])/, msg : "There must be a space after the dash", replaceby: "$1- $2$3"},
	{ re : /([\r\n])((?:<\x2f?[a-zA-Z]+>)*)-((?:<\x2f?[a-zA-Z]+>)*)([^ ])/, msg : "There must be a space after the dash", replaceby: "$1$2- $3$4"},

	// Add a space before (
	{ re : /([^\s>])((?:<\x2f?[a-zA-Z]+>)*)(\()/, msg : "There must be a space before an open parenthesis", replaceby: "$1$2 $3"},

	// Add a space after ,.;:?!)...
	{ re : /([\?!])((?:<\x2f?[a-zA-Z]+>)*)([^<\s\"\)\?!])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /([;])((?:<\x2f?[a-zA-Z]+>)*)([^<\s])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /([\)])((?:<\x2f?[a-zA-Z]+>)*)([^<\s\"\.\?!;,:])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /([,:])((?:<\x2f?[a-zA-Z]+>)*)([^<\s0-9])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /([\.]{3})((?:<\x2f?[a-zA-Z]+>)*)([^<\s\"\?!\)])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
// Obsolete check for spaces after dots, follows a check that allows for acronyms.
//	{ re : /([^\.][\.])((?:<\x2f?[a-zA-Z]+>)*)([^<\s\.0-9\"])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /(\.)((?:<\x2f?[a-zA-Z]+>)*)([a-z_(\[])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /(\.)((?:<\x2f?[a-zA-Z]+>)*)([A-Z][^\.]|[A-Z]$)/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /([^0-9][\.])((?:<\x2f?[a-zA-Z]+>)*)([0-9])/, msg : "There must be a space after punctuation", replaceby: "$1$2 $3"},
	{ re : /([^A-Z]|\w[A-Z])(\.)((?:<\x2f?[a-zA-Z]+>)*)([^A-Za-z0-9_\?!\.\"<\s(\[])/, msg : "There must be a space after punctuation", replaceby: "$1$2$3 $4"},

	// Removes spaces before .,;:?!)...
	{ re : /([\s]+)((?:<\x2f?[a-zA-Z]+>)*)([,;:\?!\)\.])/, msg : "There must not be a space before punctuation", replaceby: "$2$3"},

	// Removes spaces before ( and '
	{ re : /(\()((?:<\x2f?[a-zA-Z]+>)*)([\s]+)/, msg : "There must not be a space after open parenthesis", replaceby: "$1$2"},
	{ re : /([cCdDlLmMnNtTrRvVzZ])((?:<\x2f?[a-zA-Z]+>)*)(')((?:<\x2f?[a-zA-Z]+>)*)([\s]+)/, msg : "There must not be a space after apostrophe", replaceby: "$1$2$3$4"},

	// asdasd
	{ re : /(\b)Kickaha(\b)/, msg : "Kickaha has an heading capital letter", replaceby: "$1kickaha$2"}
);

// CheckItasaRules verifies each subtitle during the error checking.
// If there is an error on CurrentSub, returns a string containing the error description (msg) and the correction (fix).
// Otherwise returns null.

function ApplyExtension(OriginalText, FuncName, ExtFunc)
{
	var FinalText = OriginalText;
	var FullMatch = new RegExp("^([\\s\\S]*)(@" + FuncName + "@)([^@]*)(@)([\\s\\S]*)$");
	var MatchExec;
	while (true)
	{
		MatchExec = FullMatch.exec(FinalText);
		if (MatchExec == null) break;
		FinalText = MatchExec[1] + ExtFunc(MatchExec[3]) + MatchExec[5];
	}
	return FinalText;
}

function TyposFixer(OriginalText, RegExpression, ReplaceRule)
{
	var FinalText = OriginalText.replace(RegExpression, ReplaceRule);
	var tanto_per = new String();

	FinalText = ApplyExtension(FinalText, "UPPER", function(x){return x.toUpperCase()});
	FinalText = ApplyExtension(FinalText, "LOWER", function(x){return x.toLowerCase()});

	return FinalText;
}

//function CheckItasaTypos(SubText, SubPrev, SubNext, fix_only)
function CheckItasaTypos(SubText, SubPrev, SubNext, fix_only, accent)
{
	//var credits1 = /^(<\x2f?[a-zA-Z]+>)*(::Italian [^\n]+ Addicted::)(<\x2f?[a-zA-Z]+>)*(\r\n)(<\x2f?[a-zA-Z]+>)*(\[www\.italiansubs\.net\])(<\x2f?[a-zA-Z]+>)*$/;
	var credits1 = /^(<\x2f?[a-zA-Z]+>)*(::[^\n]+::)(<\x2f?[a-zA-Z]+>)*(\r\n)(<\x2f?[a-zA-Z]+>)*(\[www\.italiansubs\.net\])(<\x2f?[a-zA-Z]+>)*$/;
	var credits2 = /^(<\x2f?[a-zA-Z]+>)*(\[www\.italiansubs\.net\])(<\x2f?[a-zA-Z]+>)*$/;
	var credits3 = /(?:Traduzione:|Revisione:|Resynch:|Traduzione e synch:|Synch\s?fix:)\s.+((?:Resynch:|Synch\s?fix:)?\s?.+)?/i;
	
	if (credits1.test(SubText)) return null;
	if (credits2.test(SubText)) return null;
	if (credits3.test(SubText)) return null;
	
	for (i = 0; i < ItasaTyposRules1.length; i++)
	{
		if	((fix_only == false || ItasaTyposRules1[i].replaceby != null) &&
			ItasaTyposRules1[i].re.test(SubText) &&
			(ItasaTyposRules1[i].prev == null || (SubPrev != null && ItasaTyposRules1[i].prev.test(SubPrev))) &&
			(ItasaTyposRules1[i].next == null || (SubNext != null && ItasaTyposRules1[i].next.test(SubNext))) &&
			(ItasaTyposRules1[i].position == null ||
				(ItasaTyposRules1[i].position == "F" && SubPrev == null && SubNext != null) ||
				(ItasaTyposRules1[i].position == "M" && SubPrev != null && SubNext != null) ||
				(ItasaTyposRules1[i].position == "L" && SubPrev != null && SubNext == null) ||
				(ItasaTyposRules1[i].position == "B" && SubPrev == null && SubNext == null))
			)
		{
			return { msg : ItasaTyposRules1[i].msg, fix : (ItasaTyposRules1[i].replaceby != null) ? TyposFixer(SubText, ItasaTyposRules1[i].re, ItasaTyposRules1[i].replaceby) : null };
		}
	}	

    if (accent)
    {
		for (i = 0; i < ItasaTyposRules2a.length; i++)
		{
			if	((fix_only == false || ItasaTyposRules2a[i].replaceby != null) &&
				ItasaTyposRules2a[i].re.test(SubText) &&
				(ItasaTyposRules2a[i].prev == null || (SubPrev != null && ItasaTyposRules2a[i].prev.test(SubPrev))) &&
				(ItasaTyposRules2a[i].next == null || (SubNext != null && ItasaTyposRules2a[i].next.test(SubNext))) &&
				(ItasaTyposRules2a[i].position == null ||
					(ItasaTyposRules2a[i].position == "F" && SubPrev == null && SubNext != null) ||
					(ItasaTyposRules2a[i].position == "M" && SubPrev != null && SubNext != null) ||
					(ItasaTyposRules2a[i].position == "L" && SubPrev != null && SubNext == null) ||
					(ItasaTyposRules2a[i].position == "B" && SubPrev == null && SubNext == null))
				)
			{
				return { msg : ItasaTyposRules2a[i].msg, fix : (ItasaTyposRules2a[i].replaceby != null) ? TyposFixer(SubText, ItasaTyposRules2a[i].re, ItasaTyposRules2a[i].replaceby) : null };
			}
		}
	}
	else
	{
		if	((fix_only == false || ItasaTyposRules2b[0].replaceby != null) &&
			ItasaTyposRules2b[0].re.test(SubText) &&
			(ItasaTyposRules2b[0].prev == null || (SubPrev != null && ItasaTyposRules2b[0].prev.test(SubPrev))) &&
			(ItasaTyposRules2b[0].next == null || (SubNext != null && ItasaTyposRules2b[0].next.test(SubNext))) &&
			(ItasaTyposRules2b[0].position == null ||
				(ItasaTyposRules2b[0].position == "F" && SubPrev == null && SubNext != null) ||
				(ItasaTyposRules2b[0].position == "M" && SubPrev != null && SubNext != null) ||
				(ItasaTyposRules2b[0].position == "L" && SubPrev != null && SubNext == null) ||
				(ItasaTyposRules2b[0].position == "B" && SubPrev == null && SubNext == null))
			)
		{
			return { msg : ItasaTyposRules2b[0].msg, fix : (ItasaTyposRules2b[0].replaceby != null) ? TyposFixer(SubText, ItasaTyposRules2b[0].re, ItasaTyposRules2b[0].replaceby) : null };
		}
	}
	
	for (i = 0; i < ItasaTyposRules3.length; i++)
	{
		if	((fix_only == false || ItasaTyposRules3[i].replaceby != null) &&
			ItasaTyposRules3[i].re.test(SubText) &&
			(ItasaTyposRules3[i].prev == null || (SubPrev != null && ItasaTyposRules3[i].prev.test(SubPrev))) &&
			(ItasaTyposRules3[i].next == null || (SubNext != null && ItasaTyposRules3[i].next.test(SubNext))) &&
			(ItasaTyposRules3[i].position == null ||
				(ItasaTyposRules3[i].position == "F" && SubPrev == null && SubNext != null) ||
				(ItasaTyposRules3[i].position == "M" && SubPrev != null && SubNext != null) ||
				(ItasaTyposRules3[i].position == "L" && SubPrev != null && SubNext == null) ||
				(ItasaTyposRules3[i].position == "B" && SubPrev == null && SubNext == null))
			)
		{
			return { msg : ItasaTyposRules3[i].msg, fix : (ItasaTyposRules3[i].replaceby != null) ? TyposFixer(SubText, ItasaTyposRules3[i].re, ItasaTyposRules3[i].replaceby) : null };
		}
	}
	
	return null;
}

//function CheckItasaRules(CurrentSub, PreviousSub, NextSub, fix_only)
function CheckItasaRules(CurrentSub, PreviousSub, NextSub, fix_only, accent)
{
	var SubText = CurrentSub.Text;
	var SubPrev = (PreviousSub != null ? PreviousSub.Text : null);
	var SubNext = (NextSub != null ? NextSub.Text : null);
	var Corrections = new Array();
	var idx = 0;
	var scan = 0;
	var res;
	var Message = new String("");
	var max_tries = 100;

	do
	{
		Corrections[idx] = SubText;
		++idx;
		//res = CheckItasaTypos(SubText, SubPrev, SubNext, fix_only);
		res = CheckItasaTypos(SubText, SubPrev, SubNext, fix_only, accent);
		if (res)
		{
			if (res.fix != null)
			{
				for (scan = 0; scan < Corrections.length; ++scan)
				{
					if (res.fix == Corrections[scan])
					{
						SubText = CurrentSub.Text;
						//res = CheckItasaTypos(SubText, SubPrev, SubNext, fix_only);
						res = CheckItasaTypos(SubText, SubPrev, SubNext, fix_only, accent);
						return { msg : "Loop, correcting only the first one: " + res.msg, fix : res.fix };
					}
				}
				SubText = res.fix;
			}
			else
			{
				SubText = null;
			}
			Message = (Message == "") ? (res.msg) : (Message + " | " + res.msg);
		}
		--max_tries;
	}
	while (res != null && SubText != null && max_tries > 0);

	if (max_tries == 0)
	{
		SubText = CurrentSub.Text;
		//res = CheckItasaTypos(SubText, SubPrev, SubNext, fix_only);
		res = CheckItasaTypos(SubText, SubPrev, SubNext, fix_only, accent);
		return { msg : "Error, correcting only the first one: " + res.msg, fix : res.fix };
	}
	if (Message == "") return null;
	return { msg: Message, fix: SubText };
}

// ---------------------------------------------------------------------------
// Row splitting
// ---------------------------------------------------------------------------

// Internal constants
var dialog = /^ *-/;
var song = /^(?:<i>)?#.*(?:#(?:<\/i>)?$|#?(?:<\/i>)?$[\r\n]+^(?:<i>)?#?.*(#(?:<\/i>)?)$)/m;
var empty_row = /^ *$/;

// Change from row array to sub
function FormatSubtitle(rows)
{
	var sub = '';
    var i = 0;

	for (i = 0; i < rows.length; ++i)
	{
		if (!empty_row.test(rows[i]))
		{
		    if (sub != '') sub += '\r\n';
			sub += rows[i];
		}
	}
	return sub;
}

// Trims heading and trailing whitespaces, cleans up consecutive spaces
function CleanSpaces(row)
{
    var words_list = row.split(' ');
    var clean_row = '';
    var i = 0;
    
    for (i = 0; i < words_list.length; ++i)
    {
		if (!empty_row.test(words_list[i]))
		{
			if (clean_row != '')
			    clean_row += ' ';
			clean_row += words_list[i];
		}            
    }
    
    return clean_row;
}

// Remove Tags
function CleanTags(row)
{
    var row_to_clean = '';
    var clean_row = row;
    var stop_loop = 100;

    do        
    {
        row_to_clean = clean_row;
        clean_row = row_to_clean.replace(/(< *\x2f? *[a-zA-Z]+ *>)|(\{ *\\ *a[n]?[\d]{1,2} *\})/, "");
        --stop_loop;
    }
    while (row_to_clean != clean_row && stop_loop > 0)
    
    if (stop_loop <= 0) throw "Error in CleanTags, infinite loop";
    
    return clean_row;
}

// Joins the rows of the same dialog
function JoinDialogues(rows_list, is_song)
{
	var joined_rows = new Array;
    var i = 0;
	var u = 0;
	var new_dialog = true;

	for (i = 0; i < rows_list.length; ++i)
	{
		row = CleanSpaces(rows_list[i]);
		if (!empty_row.test(row))
		{
			if (new_dialog)
			{
				joined_rows[u] = row;
				new_dialog = false;
			}
			else
			{
				if (dialog.test(CleanTags(row)) || is_song)
				{
					++u;
					joined_rows[u] = row;
				}
				else
				{
					joined_rows[u] += ' ';
					joined_rows[u] += row;
				}
			}
		}
	}
	
	if (!is_song)
	{
		// Removes the dash if we have a single row
		if (joined_rows.length == 1 && CleanSpaces(CleanTags(joined_rows[0]))[0] == '-')
		{
			joined_rows[0] = joined_rows[0].replace(/^(< *\x2f? *[a-zA-Z]+ *> *)?(- *)/, "$1");
		}
	
		// Adds the dash on the first line if we have more than one rows
		if (joined_rows.length > 1 && CleanSpaces(CleanTags(joined_rows[0]))[0] != '-')
		{
			joined_rows[0] = joined_rows[0].replace(/^(< *\x2f? *[a-zA-Z]+ *>)?/, "$1- ");
		}
	}
	
	return joined_rows;
}

// Splits rows at the requested character
function SplitAtChar(row, character)
{
	var words_list = row.split(' ');
	var first_part = '';
	var second_part = '';
	var iin = 0;
	var ifi = words_list.length - 1;
	
	while (iin <= ifi)
	{
	    if (CleanTags(first_part).length > CleanTags(second_part).length)
	    {
			do
			{
				if (second_part != '') second_part = ' ' + second_part;
				second_part = words_list[ifi] + second_part;
				--ifi;
			}
			while (iin <= ifi && !character.test(CleanTags(words_list[ifi])));
	    }
	    else
	    {
			do
			{
				if (first_part != '') first_part += ' ';
				first_part += words_list[iin];
				++iin;
			}
			while (iin <= ifi && !character.test(CleanTags(words_list[iin - 1])));
	    }
	}
	
	return { first_half : first_part, second_half : second_part };
}

// Checks if the split adheres to the parameters
function IsGoodSplit(x, ParamMaxPerRow, ParamMaxUnbalance)
{
	var res;
	
	res = (CleanTags(x.first_half).length <= ParamMaxPerRow.Value);
	res = res && (CleanTags(x.second_half).length <= ParamMaxPerRow.Value);
	res = res && (Math.abs(CleanTags(x.first_half).length - CleanTags(x.second_half).length) <= ParamMaxUnbalance.Value);
		
	return res;
}

// Splits rows. Returns the character to split the row at.
function Split(row, ParamMaxPerRow, ParamMaxUnbalance)
{
	var test_char;

	// Try to split at "...",  ".",  "?" or "!"
	test_char = SplitAtChar(row, /[\.\?!]$/);
	if (IsGoodSplit(test_char, ParamMaxPerRow, ParamMaxUnbalance))
		return test_char;
	
	// Try to split at ";" o ":"
	test_char = SplitAtChar(row, /[;:]$/);
	if (IsGoodSplit(test_char, ParamMaxPerRow, ParamMaxUnbalance))
		return test_char;
		
	// Try to split at ","
	test_char = SplitAtChar(row, /,$/);
	if (IsGoodSplit(test_char, ParamMaxPerRow, ParamMaxUnbalance))
		return test_char;

	// Split at the centermost space
	test_char = SplitAtChar(row, /.*/);
	return test_char;
}

// Gets rows correctly splitted
function SplitRows(original_sub, ParamMaxPerRow, ParamMaxUnbalance, ParamMinPerRow)
{
	var credits1 = /^(<\x2f?[a-zA-Z]+>)*(::[^\n]+::)(<\x2f?[a-zA-Z]+>)*(\r\n)(<\x2f?[a-zA-Z]+>)*(\[www\.italiansubs\.net\])(<\x2f?[a-zA-Z]+>)*$/;
	if (credits1.test(original_sub)) return null;

	var reason = '';
	var max_rows = 2;
	var i = 0;

	// Splits sub in array of rows
	var original_rows = original_sub.split('\r\n');

	// Joins rows from the same dialog
	var joined_rows = JoinDialogues(original_rows, song.test(original_sub));
	
	// Resplits
	if (joined_rows.length > max_rows)
	{
		reason = 'too many dialogues';
    }
    else
    {
        if (joined_rows.length == max_rows &&
            (CleanTags(joined_rows[0]).length > ParamMaxPerRow.Value ||
            CleanTags(joined_rows[1]).length > ParamMaxPerRow.Value))
        {
    		reason = 'too long rows';
        }
        else
        {
            if (joined_rows.length == 1 && !song.test(original_sub))
            {
                if (CleanTags(joined_rows[0]).length > ParamMinPerRow.Value)
                {
                    var split_row = Split(joined_rows[0], ParamMaxPerRow, ParamMaxUnbalance);
                    joined_rows[0] = split_row.first_half;
                    joined_rows[1] = split_row.second_half;
                    if (CleanTags(joined_rows[0]).length > ParamMaxPerRow.Value ||
                        CleanTags(joined_rows[1]).length > ParamMaxPerRow.Value)
                    {
    		            reason = 'too long row';
                    }
                }
            }
        }
	}

	// Gets the recalculated subtitle
	return { sub : FormatSubtitle(joined_rows), res : reason };
}

