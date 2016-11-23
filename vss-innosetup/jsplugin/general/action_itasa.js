// Redux 1.0 by rock3r
// ---------------------------------------------------------------------------
// Based upon ITASA tools v0.9.21.4 (compatible with VSS v0.9.21.IMod.0.4.4)
// (if the plugin "itasa_zplit_kickaha.js" is installed, this uses its params)
// by kickaha
// ---------------------------------------------------------------------------
// Version 1.0 (Jul 18nd, 2012)
//		* Code translation 
// 		* Adapted for the new version of the common\itasa.js file


// ---------------------------------------------------------------------------
// Split line
// ---------------------------------------------------------------------------

JSAction_split_line =
{
	onExecute : function()
	{
		// Defaults
		var ParamMaxPerRigaValue = 45;
		var ParamMinPerRigaValue = 40;
		var ParamMaxSbilanciaValue = 15;

		// Try to get parameters from plugin
		try { ParamMaxPerRigaValue = parseInt(VSSCore.GetPluginParamValue('ITASA - Row splitter', 'ParamMaxPerRiga')); } catch (err) {}
		try { ParamMinPerRigaValue = parseInt(VSSCore.GetPluginParamValue('ITASA - Row splitter', 'ParamMinPerRiga')); } catch (err) {}
		try { ParamMaxSbilanciaValue = parseInt(VSSCore.GetPluginParamValue('ITASA - Row splitter', 'ParamMaxSbilancia')); } catch (err) {}

		var ParamMaxPerRiga = { Value : ParamMaxPerRigaValue };
		var ParamMinPerRiga = { Value : ParamMinPerRigaValue };
		var ParamMaxSbilancia = { Value : ParamMaxSbilanciaValue };

		var sub = VSSCore.GetFirstSelected();
		var sub_current;
		var sub_fixed;

		while (sub != null)
		{
			sub_current = sub.Text;
			
			var credits_regex = /(?:Traduzione:|Revisione:|Resynch:|Traduzione e synch:|Synch\s?fix:)\s.+((?:Resynch:|Synch\s?fix:)?\s?.+)?/i;
		
			// Check that this isn't a credits row (ItaSA-style)
			if (credits_regex.test(sub_current)) {
				sub = VSSCore.GetNextSelected(sub);
				continue;
			}
			
			sub_fixed = SplitRows(sub_current, ParamMaxPerRiga, ParamMaxSbilancia, ParamMinPerRiga);
			sub.Text = sub_fixed.sub;
			sub = VSSCore.GetNextSelected(sub);
		}
	}
};

VSSCore.RegisterJavascriptAction('JSAction_split_line', 'Split line', 'Ctrl+E');



// ---------------------------------------------------------------------------
// Check typos
// ---------------------------------------------------------------------------

JSAction_typos =
{
	onExecute : function()
	{
		var sub = VSSCore.GetFirstSelected();
		var CurrentSub;
		var PreviousSub;
		var NextSub;
		var res;

		while (sub != null)
		{
			CurrentSub = sub;
			
			if (CurrentSub.Index > 1)
				PreviousSub = VSSCore.GetSubAt(CurrentSub.Index - 1);
			else
				PreviousSub = null;

			if (CurrentSub.Index < VSSCore.GetSubCount())
				NextSub = VSSCore.GetSubAt(CurrentSub.Index + 1);
			else
				NextSub = null;

			res = CheckItasaRules(CurrentSub, PreviousSub, NextSub, true);
			if (res != null)
				if (res.fix != null)
					sub.Text = res.fix;
				
			sub = VSSCore.GetNextSelected(sub);
		}
	}
};

VSSCore.RegisterJavascriptAction('JSAction_typos', 'Fix typos', 'Ctrl+H');



// ---------------------------------------------------------------------------
// Add sharp (#) to both ends of each line
// ---------------------------------------------------------------------------

JSAction_addsharp =
{
	onExecute : function()
	{
		//var add_sharp = /^((?:<\x2f?[a-zA-Z]+>|\s)*)(#?[\s]*)([^\s<#]|[^\s<#][^#]*[^\s>#])([\s]*#?)((?:<\x2f?[a-zA-Z]+>|\s)*)$/;
		//var add_replace = "$1# $3 #$5";
		var add_sharp = /^((?:<\x2f?[a-zA-Z]+>|\s)*)([^\s<#]|[^\s<#][^#]*[^\s>#])((?:<\x2f?[a-zA-Z]+>|\s)*)$/;
		var add_replace = "$1# $2 #$3";
		var remove_sharp = /^((?:<\x2f?[a-zA-Z]+>|\s)*)(#[\s]*)([^\s<#]|[^\s<#][^#]*[^\s>#])([\s]*#)((?:<\x2f?[a-zA-Z]+>|\s)*)$/;
		var remove_replace = "$1$3$5";
		var rows_list;
		var idx;

		var sub = VSSCore.GetFirstSelected();

		while (sub != null)
		{
			rows_list = sub.Text.split('\r\n');

			for (idx = 0; idx < rows_list.length; idx++)
			{
				if (rows_list[idx].match(add_sharp))
					rows_list[idx] = rows_list[idx].replace(add_sharp, add_replace);
				else if (rows_list[idx].match(remove_sharp))
					rows_list[idx] = rows_list[idx].replace(remove_sharp, remove_replace);
			}

			sub.Text = FormatSubtitle(rows_list);

			sub = VSSCore.GetNextSelected(sub);
		}
	}
};

VSSCore.RegisterJavascriptAction('JSAction_addsharp', 'Add sharp', 'Ctrl+à');

