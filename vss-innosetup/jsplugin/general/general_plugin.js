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
  RS_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 1, // Reading speed
  RATING_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 2, // Reading speed rating
  CPS_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 3, // Characters per second
  DURATION_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 4, // Duration
  LINE_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 5, // Maximum line length
  PIXELS_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 6, // Maximum pixel width
  BLANK_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 7, // Blank
  BALANCE_COL_IDX: VSSCore.LAST_CORE_COL_IDX + 8, // Line balance

  // Get the number of extra-columns (called only at VSS startup)
  GetExtraColumnsCount: function () {
    return 8;
  },

  // Get the title of each extra-column (called only at VSS startup)
  GetColumnTitle: function (Index) {
    switch (Index) {
      case this.RS_COL_IDX:
        return 'RS';
      case this.RATING_COL_IDX:
        return 'Rating';
      case this.CPS_COL_IDX:
        return 'CPS';
      case this.DURATION_COL_IDX:
        return 'Duration';
      case this.LINE_COL_IDX:
        return 'Line';
      case this.PIXELS_COL_IDX:
        return 'Pixels';
      case this.BLANK_COL_IDX:
        return 'Blank';
      case this.BALANCE_COL_IDX:
        return 'Balance';
      default:
        return '';
    }
  },

  // Get the size of each extra-column (called only at VSS startup)
  GetColumnSize: function (Index) {
    switch (Index) {
      case this.RS_COL_IDX:
        return 37;
      case this.RATING_COL_IDX:
        return 45;
      case this.CPS_COL_IDX:
        return 37;
      case this.DURATION_COL_IDX:
        return 52;
      case this.LINE_COL_IDX:
        return 32;
      case this.PIXELS_COL_IDX:
        return 40;
      case this.BLANK_COL_IDX:
        return 40;
      case this.BALANCE_COL_IDX:
        return 52;
      default:
        return '';
    }
  },

  // Check if a column background can be colorized (called only at VSS startup)
  IsColumnBGColorized: function (Index) {
    switch (Index) {
      case this.RS_COL_IDX:
        return true;
      case this.RATING_COL_IDX:
        return true;
      case this.CPS_COL_IDX:
        return true;
      case this.DURATION_COL_IDX:
        return true;
      case this.LINE_COL_IDX:
        return true;
      case this.PIXELS_COL_IDX:
        return true;
      case this.BLANK_COL_IDX:
        return true;
      case this.BALANCE_COL_IDX:
        return true;
      default:
        return false;
    }
  },

  // Check if a column has custom text (called only at VSS startup)
  HasColumnCustomText: function (Index) {
    switch (Index) {
      case this.RS_COL_IDX:
        return true;
      case this.RATING_COL_IDX:
        return true;
      case this.CPS_COL_IDX:
        return true;
      case this.DURATION_COL_IDX:
        return true;
      case this.LINE_COL_IDX:
        return true;
      case this.PIXELS_COL_IDX:
        return true;
      case this.BLANK_COL_IDX:
        return true;
      case this.BALANCE_COL_IDX:
        return true;
      default:
        return false;
    }
  },

  // Get the column background color (called on each cell repaint)
  GetColumnBGColor: function (Index, CurrentSub, PreviousSub, NextSub) {
    switch (Index) {
      case this.RS_COL_IDX:
      case this.RATING_COL_IDX:
        var index = Common.getReadingSpeedIndex(this.cache);
        return Common.READING_SPEED_DEF[index].color;

      case this.CPS_COL_IDX:
        if (this.cache < 4) return 0x9999ff; // TS color
        if (this.cache < 7) return 0x99ccff; // SA color
        if (this.cache < 10) return 0x99ffff; // ABS color
        if (this.cache < 13) return 0x99ffcc; // G color
        if (this.cache <= 16) return 0x99ff99; // P color
        if (this.cache <= 19) return 0xccff99; // G color
        if (this.cache <= 22) return 0xffff99; // ABF color
        if (this.cache <= 25) return 0xffcc99; // FA color
        return 0xff9999; // TF color

      case this.DURATION_COL_IDX:
        if (this.cache < 500) return 0xff9999; // TF color
        if (this.cache < 750) return 0xffcc99; // FA color
        if (this.cache < 1000) return 0xffff99; // ABF color
        if (this.cache < 2000) return 0xccff99; // G color
        if (this.cache <= 3000) return 0x99ff99; // P color
        if (this.cache <= 4000) return 0x99ffcc; // G color
        if (this.cache <= 5000) return 0x99ffff; // ABS color
        if (this.cache <= 6000) return 0x99ccff; // SA color
        return 0x9999ff; // TS color

      case this.LINE_COL_IDX:
        if (this.cache <= 2) return 0x9999ff; // TS color
        if (this.cache <= 5) return 0x99ccff; // SA color
        if (this.cache <= 10) return 0x99ffff; // ABS color
        if (this.cache <= 20) return 0x99ffcc; // G color
        if (this.cache <= 30) return 0x99ff99; // P color
        if (this.cache <= 35) return 0xccff99; // G color
        if (this.cache <= 37) return 0xffff99; // ABF color
        if (this.cache <= 40) return 0xffcc99; // FA color
        return 0xff9999; // TF color

      case this.PIXELS_COL_IDX:
        if (this.cache <= 25) return 0x9999ff; // TS color
        if (this.cache <= 61) return 0x99ccff; // SA color
        if (this.cache <= 123) return 0x99ffff; // ABS color
        if (this.cache <= 245) return 0x99ffcc; // G color
        if (this.cache <= 368) return 0x99ff99; // P color
        if (this.cache <= 429) return 0xccff99; // G color
        if (this.cache <= 454) return 0xffff99; // ABF color
        if (this.cache <= 500) return 0xffcc99; // FA color
        return 0xff9999; // TF color

      case this.BLANK_COL_IDX:
        if (undefined != this.blankToSceneChangeStart) {
          return 0x9999ff; // TS color
        } else if (undefined !== this.blankToSceneChangeStop) {
          return Math.abs(this.blankToSceneChangeStop - SceneChange.StartOffset) <= 1
            ? 0xccff99 // G color
            : this.blankToSceneChangeStop < SceneChange.StartOffset
            ? 0xffcc99 // FA color
            : this.blankToSceneChangeStop < 250
            ? 0xffff99 // ABF color
            : 0xffffff; // White
        } else if (undefined !== this.blank) {
          return Math.abs(this.blank - VSSCore.MinimumBlank) <= 1
            ? 0x99ff99 // P color
            : this.blank < VSSCore.MinimumBlank
            ? 0xff9999 // TF color
            : this.blank < 250
            ? 0xffff99 // ABF color
            : 0xffffff; // White
        }

        return 0xffffff; // White

      case this.BALANCE_COL_IDX:
        if (this.cache >= 0.75) return 0x99ff99; // P color
        if (this.cache >= 0.5) return 0xccff99; // G color
        if (this.cache >= 0.25) return 0xffff99; // ABF color
        if (this.cache >= 0.125) return 0xffcc99; // FA color
        return 0xff9999; // TF color

      default:
        return 0xffffff; // White
    }
  },

  // Get the text of the extra-column (called on each cell repaint)
  GetColumnText: function (Index, CurrentSub, PreviousSub, NextSub) {
    switch (Index) {
      case this.RS_COL_IDX:
        this.cache = Common.getReadingSpeed(CurrentSub);
        var rounded = Common.decimal1Round(this.cache);
        return (rounded < 10 ? '  ' : '') + (rounded == this.cache ? rounded : rounded.toFixed(1));

      case this.RATING_COL_IDX:
        this.cache = Common.getReadingSpeed(CurrentSub);
        return Common.getReadingSpeedRating(this.cache);

      case this.CPS_COL_IDX:
        this.cache = Common.getCharactersPerSecond(CurrentSub);
        var rounded = Common.decimal1Round(this.cache);
        return (rounded < 10 ? '  ' : '') + (rounded == this.cache ? rounded : rounded.toFixed(1));

      case this.DURATION_COL_IDX:
        this.cache = CurrentSub.Stop - CurrentSub.Start;
        return (this.cache / 1000).toFixed(3);

      case this.LINE_COL_IDX:
        var lines = Common.getLines(CurrentSub.StrippedText);
        var numLines = lines.length;
        this.cache = 0;

        for (var i = 0; i < numLines; ++i) {
          var len = lines[i].length;

          if (len > this.cache) {
            this.cache = len;
          }
        }

        return this.cache;

      case this.PIXELS_COL_IDX:
        var lines = Common.getLines(CurrentSub.StrippedText);
        var numLines = lines.length;
        this.cache = 0;

        for (var i = 0; i < numLines; ++i) {
          var len = Common.getPixelWidth(lines[i], this.pixelFont);

          if (len > this.cache) {
            this.cache = len;
          }
        }

        return this.cache;

      case this.BLANK_COL_IDX:
        this.blankToSceneChangeStart = undefined;
        this.blankToSceneChangeStop = undefined;
        this.blank = NextSub ? NextSub.Start - CurrentSub.Stop : undefined;

        if (SceneChange.Visible && this.blank >= VSSCore.MinimumBlank) {
          // Around start
          var previousSceneChange = SceneChange.GetPrevious(CurrentSub.Start);

          if (previousSceneChange >= 0) {
            var blankToSceneChangeStart = CurrentSub.Start - previousSceneChange;

            if (blankToSceneChangeStart < SceneChange.StopOffset) {
              return (this.blankToSceneChangeStart = blankToSceneChangeStart) /*  + " scStart" */;
            }
          }

          var nextSceneChange = SceneChange.GetNext(CurrentSub.Start + 1);

          if (nextSceneChange >= 0 && nextSceneChange - CurrentSub.Start <= SceneChange.FilterOffset) {
            return (this.blankToSceneChangeStart = CurrentSub.Start - nextSceneChange) /*  + " filterStart" */;
          }

          // Around stop
          var previousSceneChange = SceneChange.GetPrevious(CurrentSub.Stop - 1);

          if (previousSceneChange >= 0 && CurrentSub.Stop - previousSceneChange <= SceneChange.FilterOffset) {
            return (this.blankToSceneChangeStop = previousSceneChange - CurrentSub.Stop) /*  + " filterStop" */;
          }

          var nextSceneChange = SceneChange.GetNext(CurrentSub.Stop);
        } else {
          var nextSceneChange = -1;
        }

        if (nextSceneChange < 0) {
          if (this.blank) {
            return this.blank;
          }
        } else {
          if (this.blank && NextSub.Start < nextSceneChange) {
            return this.blank;
          } else {
            return (this.blankToSceneChangeStop = nextSceneChange - CurrentSub.Stop) /*  + " scStop" */;
          }
        }

        return '';

      case this.BALANCE_COL_IDX:
        var lines = Common.getLines(CurrentSub.StrippedText);
        var numLines = lines.length;
        var minLineLen = Infinity;
        var maxLineLen = 0;

        for (var i = 0; i < numLines; ++i) {
          var lineLen = lines[i].length;

          if (lineLen > maxLineLen) {
            maxLineLen = lineLen;
          }
          if (lineLen < minLineLen) {
            minLineLen = lineLen;
          }
        }

        this.cache = minLineLen / maxLineLen || 1;
        var pct = Math.round(this.cache * 100);
        var numBlanks = 2 - Math.floor(Math.log(pct) / Math.log(10));
        var blanks = '';
        for (var i = 0; i < numBlanks; ++i) {
          blanks += '  ';
        }
        return blanks + pct + '%';

      default:
        return '';
    }
  },
};

// ---------------------------------------------------------------------------
// Load javascript actions
// ---------------------------------------------------------------------------

LoadScript('action_*.js');

// ---------------------------------------------------------------------------
