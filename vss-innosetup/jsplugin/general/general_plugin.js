// status bar display
// thyresias <at> gmail.com (www.calorifix.net)
// 20-Jan-2007  first version

LoadScript("../common/tools.js");
LoadScript("../common/itasa.js");

// ---------------------------------------------------------------------------
// ASCII bar stuff
// ---------------------------------------------------------------------------

var templateBarDS = null;
var templateBarRS = null;

function initBar(min, max) {
    var bar = "·····················································";
    var length = max - min + 1;
    for (; bar.length < length ;)
      bar = bar + bar;
    return bar.substr(0, length);
}

function getBar(value, min, max, templateBar) {
    // ~{|}[\]^_`:<=>-+!#%&‘’¡¤§«¬­¯°±´º»
    var iVal = Math.round(value);
    // below min: *  ·························
    if (iVal < min)
      return "«" + templateBar;
    // above max: ·························  *
    else if (iVal > max)
      return templateBar + "»";
    // in the range
    else {
      iVal = iVal - min;
      return templateBar.substr(0, iVal-1) + "¦" + templateBar.substr(iVal+1);
    }
}

// ---------------------------------------------------------------------------

function statusBarText(Sub) {

    // current length, duration
    var len = Sub.StrippedText.length;
    var durMs = Sub.Stop - Sub.Start;
    var durS = decimal1Round(durMs / 1000);

    // display speed
    var dsMin = 4;
    var dsMax = 22;
    var dsX = len * 1000 / durMs;       // exact
    var ds = decimal1Round(dsX);

    // reading speed
    var rsMin = 5;
    var rsMax = 35;
    if (durMs < 500) {
      durMs = 500;
    }    
    var rsX = len * 1000 / (durMs - 500);
    var rs = decimal1Round(rsX);

    // rating
    var rating = getReadingSpeedRating(rs);

    // initialize template bars if needed
    if (templateBarDS === null) templateBarDS = initBar(dsMin, dsMax);
    if (templateBarRS === null) templateBarRS = initBar(rsMin, rsMax);

    // get display bars
    var barDS = getBar(dsX, dsMin, dsMax, templateBarDS);
    var barRS = getBar(rsX, rsMin, rsMax, templateBarRS);

    // compute Lavie duration
    var rsIdeal = 20;
    var durLavie = 0.5 + len / rsIdeal;
    durLavie = decimal1Round(durLavie);

    return "DS: " + ds + " " + barDS +
      "  |  RS: " + rs + " " + barRS +
      "  |  Duration: " + durS + " (ideal: " + durLavie + ")  |  " + rating;

}

// ---------------------------------------------------------------------------

VSSPlugin = {

  // Called on subtitle modifications (time or text)
  OnSubtitleModification : function(CurrentSub, PreviousSub, NextSub) {
    SetStatusBarText(statusBarText(CurrentSub));
  },

  // Called when the selected subtitle change
  OnSelectedSubtitle : function(CurrentSub, PreviousSub, NextSub) {
    SetStatusBarText(statusBarText(CurrentSub));
  },
  
  // Called when the selection is doubleclicked at start on the WAV display
  OnRangeStartDblClick : function(CurrentSub, PreviousSub, NextSub) {

    // No overlap onto previous subtitle
    if (PreviousSub) {
        var afterPreviousSub = PreviousSub.Stop + VSSCore.MinimumBlank;

        if (CurrentSub.Start < afterPreviousSub) {
            CurrentSub.Start = afterPreviousSub;
        }
    }

    // TODO: unoverlap onto any previous or next scene change,
    // if scene changes are visible
  },
  
  // Called when the selection is doubleclicked at stop on the WAV display
  OnRangeStopDblClick : function(CurrentSub, PreviousSub, NextSub) {

  // Lavie duration
  var duration = CurrentSub.StrippedText.length * 50 + 500;
	if (duration < VSSCore.MinimumDuration)
		duration = VSSCore.MinimumDuration;
    var stop = CurrentSub.Start + duration;

    // No overlap onto next subtitle
    if (NextSub) {
        var beforeNextSub = NextSub.Start - VSSCore.MinimumBlank;

        if (stop > beforeNextSub) {
            stop = beforeNextSub;
        }
    }

    // TODO: unoverlap onto any previous or next scene change,
    // if scene changes are visible

    if (stop > CurrentSub.Start) {
        CurrentSub.Stop = stop;
    }
  },
  
  // COLUMNS -----------------------------------------------------------------

  // VSS core columns index
  // VSSCore.INDEX_COL_IDX : Index of the subtitle index column
  // VSSCore.START_COL_IDX : Index of the start time column
  // VSSCore.STOP_COL_IDX  : Index of the stop time colum
  // VSSCore.STYLE_COL_IDX : Index of the style column (SSA/ASS only)
  // VSSCore.TEXT_COL_IDX  : Index of the text column
  //
  // VSSCore.LAST_CORE_COL_IDX : Index of the last column of VSS core
  //
  
  // Declare extra column index here
  RS_COL_IDX : VSSCore.LAST_CORE_COL_IDX + 1, // Reading speed
  CPS_COL_IDX : VSSCore.LAST_CORE_COL_IDX + 2, // Characters per second
  DURATION_COL_IDX : VSSCore.LAST_CORE_COL_IDX + 3, // Duration
  
  // Get the number of extra-columns (called only at VSS startup)
  GetExtraColumnsCount : function() {
    //return 1;
    //return 2;
	return 3;
  },
  
  // Get the title of each extra-column (called only at VSS startup)
  GetColumnTitle : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX:  return 'RS';
      case this.CPS_COL_IDX: return 'CPS';
	  case this.DURATION_COL_IDX: return 'Duration';
      default: return '';
    }
  },
  
  // Get the size of each extra-column (called only at VSS startup)
  GetColumnSize : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX:  return 30;
      case this.CPS_COL_IDX: return 30;
	  case this.DURATION_COL_IDX: return 52;
      default: return '';
    }
  },

  // Check if a column background can be colorized (called only at VSS startup)
  IsColumnBGColorized : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX:  return true;
      case this.CPS_COL_IDX: return true;
	  case this.DURATION_COL_IDX: return true;
      default: return false;
    }
  },
  
  // Check if a column has custom text (called only at VSS startup)
  HasColumnCustomText : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX:  return true;
      case this.CPS_COL_IDX: return true;
	  case this.DURATION_COL_IDX: return true;
      default: return false;
    }
  },  
  
  // Get the column background color (called on each cell repaint)
  GetColumnBGColor : function(Index, CurrentSub, PreviousSub, NextSub) {
    switch(Index) {
      case this.RS_COL_IDX:  return getReadingSpeedAsColor(CurrentSub);
      case this.CPS_COL_IDX: return getCPSAsColor(CurrentSub);
      case this.DURATION_COL_IDX:
        if (this.cache <   500) return 0xff9999;  // TF color
        if (this.cache <   750) return 0xffcc99;  // FA color
        if (this.cache <  1000) return 0xffff99;  // ABF color
        if (this.cache <  2000) return 0xccff99;  // G color
        if (this.cache <= 3000) return 0x99ff99;  // P color
        if (this.cache <= 4000) return 0x99ffcc;  // G color
        if (this.cache <= 5000) return 0x99ffff;  // ABS color
        if (this.cache <= 6000) return 0x99ccff;  // SA color
        return 0x9999ff;                          // TS color
      default: return 0xffffff;
    }
  },
  
  // Get the text of the extra-column (called on each cell repaint)
  GetColumnText : function(Index, CurrentSub, PreviousSub, NextSub) {
    switch(Index) {
      case this.RS_COL_IDX:  return getReadingSpeedAsText(CurrentSub);
      case this.CPS_COL_IDX: return getCPSAsText(CurrentSub);	  
      case this.DURATION_COL_IDX:
        this.cache = CurrentSub.Stop - CurrentSub.Start;
        return (this.cache / 1000).toFixed(3);
      default: return '';
    }
  }
  
};

// ---------------------------------------------------------------------------
// Load javascript actions
// ---------------------------------------------------------------------------

LoadScript('action_*.js');

// ---------------------------------------------------------------------------
