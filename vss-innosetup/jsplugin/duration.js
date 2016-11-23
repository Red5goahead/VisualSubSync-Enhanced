// Duration plugin for VisualSubSync
// Spirit <hiddenspirit (at) gmail.com>
// Nathbot
// ItaSA customization: zefram cochrane [www.italiansubs.net]
// Translated by rock3r

// LoadScript("common/custom.js"); // Inlined

VSSPlugin = {
  // Plugin constants
  Name : "Duration",
  Description : "Detects if the duration is below the minimum " +
    "or above the maximum thresholds. Fixes the duration without creating overlaps.",
  Color : 0x3787ff,
  Message : "Duration",

  // Plugin parameters available from VSS GUI (name must start with "Param")
  ParamStrictMinDuration : { Value : 1000, Unit : "ms", Description :
    "Minimum mandatory subtitle duration (default: 1000 ms).\n" +
    "Needed for error checking. Subtitle correction is made " +
    "is done using the minimum duration set in the common preferences." },
  ParamStrictMaxDuration : { Value : 5000, Unit : "ms", Description :
    "Maximum mandatory subtitle duration (default:  5000 ms)." },
  ParamMaxStartMove : { Value : 0, Unit : "ms", Description :
    "Maximum shifting for subtitles' starting points (default:  0 ms)." },

  TooShortMessage : "< {minSeconds} s",
  TooLongMessage : "> {maxSeconds} s",

  // HasError method called for each subtitle during the error checking
  // If there is an error on CurrentSub
  // return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null.
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    var duration = CurrentSub.Stop - CurrentSub.Start;

    if (duration < this.ParamStrictMinDuration.Value) {
        return Custom.formatMessage(this.TooShortMessage,
            {minSeconds: this.ParamStrictMinDuration.Value / 1000.});
    } else if (duration > this.ParamStrictMaxDuration.Value) {
        return Custom.formatMessage(this.TooLongMessage,
            {maxSeconds: this.ParamStrictMaxDuration.Value / 1000.});
    } else {
        return "";
    }
  },

  FixError : function(CurrentSub, PreviousSub, NextSub) {
    var duration = CurrentSub.Stop - CurrentSub.Start;

    if (duration < this.ParamStrictMinDuration.Value) {
        this.FixShortDuration(CurrentSub, PreviousSub, NextSub);
    } else if (duration > this.ParamStrictMaxDuration.Value) {
        CurrentSub.Stop = CurrentSub.Start + this.ParamStrictMaxDuration.Value;
    }
  },

  FixShortDuration : function(CurrentSub, PreviousSub, NextSub) {
    var newStop = Custom.getNonOverlappedStop(
        CurrentSub.Start + VSSCore.MinimumDuration,
        NextSub, SceneChange.GetNext(CurrentSub.Stop));

    if (newStop > CurrentSub.Stop) {
        CurrentSub.Stop = newStop;
    }

    var maxStart = CurrentSub.Stop - VSSCore.MinimumDuration;

    if (CurrentSub.Start <= maxStart || this.ParamMaxStartMove.Value == 0) {
        return;
    }

    var newStart = Custom.getNonOverlappedStart(
        Math.max(maxStart, CurrentSub.Start - this.ParamMaxStartMove.Value),
        PreviousSub, SceneChange.GetPrevious(CurrentSub.Start));

    if (newStart < CurrentSub.Start) {
        CurrentSub.Start = newStart;
    }
  }
};

// Inlined custom.js
var Custom = {

// Get non overlapped start.
getNonOverlappedStart : function(start, previousSub, previousScene) {
    // No overlap on previous subtitle (it can be null or undefined).
    if (previousSub) {
        var afterPreviousSub = previousSub.Stop + VSSCore.MinimumBlank;

        if (start < afterPreviousSub) {
            start = afterPreviousSub;
        }
    }

    // No ovelap on previous scene change (it can be -1 or undefined).
    if (previousScene >= 0) {
        var previousSceneStop = previousScene + SceneChange.StopOffset;

        if (start < previousSceneStop) {
            start = previousSceneStop;
        }
    }

    return start;
},

// Get non overlapped stop.
getNonOverlappedStop : function(stop, nextSub, nextScene) {
    // No overlap on next subtitle (it can be null or undefined).
    if (nextSub) {
        var beforeNextSub = nextSub.Start - VSSCore.MinimumBlank;

        if (stop > beforeNextSub) {
            stop = beforeNextSub;
        }
    }

    // No overlap on next scene change (it can be -1 or undefined).
    if (nextScene >= 0) {
        var nextSceneStart = nextScene - SceneChange.StartOffset;

        if (stop > nextSceneStart) {
            stop = nextSceneStart;
        }
    }

    return stop;
},

// Perform a formatting operation on a message.
formatMessage : function(message, values) {
    return message.replace(/{(\w+)}/g, function(s, p) { return values[p]; });
},

// Custom
};
