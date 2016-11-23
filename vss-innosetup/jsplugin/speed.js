// Speed plugin for VisualSubSync
// Spirit <hiddenspirit (at) gmail.com>
// Nathbot <nathbot (at) gmail.com>
// Personalizzazione ItaSA: zefram cochrane [www.italiansubs.net]
// Rs fixes by Gpl
// Translated by rock3r

// LoadScript("common/custom.js"); // Inlined

VSSPlugin = {
  // Plugin constants
  Name : "Speed",
  Description : "Detects when the reading speed (RS) and/or the " +
    "characters per second (CPS) are lower or higher than the specified thresholds. " +
    "Fixes subtitles that are too fast by converging to the ideal speed.",
  Color : 0xffc437,
  Message : "Speed",

  // Plugin parameters available from VSS GUI (name must start with "Param")
  ParamMode : { Value : 3, Unit : "(1/2/3)", Description :
    "Speed mode.\n" +
    "1 = RS mode\n" +
    "2 = CPS mode\n" +
    "3 = RS and CPS mode (default)" },
  ParamMinRsLevel : { Value : 1, Unit : "(1/2/3/4)", Description :
    "Minimum RS detection threshold.\n" +
    "1 = TOO SLOW (default)\n" +
    "2 = Slow, acceptable\n" +
    "3 = A bit slow\n" +
    "4 = Good" },
  ParamMaxRsLevel : { Value : 1, Unit : "(1/2/3/4)", Description :
    "Maximum RS detection threshold.\n" +
    "1 = TOO FAST (default)\n" +
    "2 = Fast, acceptable\n" +
    "3 = A bit fast\n" +
    "4 = Good" },
  ParamMinCps : { Value : 4, Unit : "Char/s", Description :
    "Minimum CPS (default: 4)." },
  ParamMaxCps : { Value : 30, Unit : "Char/s", Description :
    "Maximum CPS (default: 30)." },
  ParamOptimize : { Value : 1, Unit : "(0/1/2/3)", Description :
    "Optimizable subtitles detection mode.\n" +
    "0 = Off\n" +
    "1 = Detect subtitles that can be longer (default)\n" +
    "2 = Detect subtitles that can be shorter\n" +
    "3 = Detect subtitles that can be both longer or shorter" },
  ParamMaxDuration : { Value : 5000, Unit : "ms", Description :
    "Maximum correction length for fast subtitles (default: 5000 ms)" },
  ParamMaxStartMove : { Value : 0, Unit : "ms", Description :
    "Maximum shifting for fast subtitles' starting points (default: 0 ms)" },
  ParamBreakSceneChanges : { Value : 1, Unit : "(0/1)", Description :
    "Bypass scene change during correction if necessary.\n" +
    "0 = Off\n" +
    "1 = On (default)" },
  ParamIgnoreLinesOf : { Value : 3, Unit : "Characters", Description :
    "Ignore lines that have up to N characters (default: 3)" },
  ParamRsTarget : { Value : 23, Unit : "Char/s", Description :
    "Ideal read speed (default: 23)" },

  // Non-configurable parameters
  RsTarget : 23, //default 20, default itasa 23.
  strictMaxRsTarget : 40, //default 30, default itasa 40.
  strictMaxCpsTarget : 30, //default 25, default itasa 30.

  // Messages
  TooHighCpsMessage : "CPS > {maxCps}",
  TooLowCpsMessage : "CPS < {minCps}",
  //OptimizableMessage : "Optimizable: {valueSeconds} s",
	OptimizableMessage : "Optimizable length",
  JoinMessage : " & ",

  // HasError method called for each subtitle during the error checking
  // If there is an error on CurrentSub
  // return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null.
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    this.RsTarget = this.ParamRsTarget.Value;
    var len = CurrentSub.StrippedText.length;

    if (len <= this.ParamIgnoreLinesOf.Value) {
        return "";
    }

    var duration = CurrentSub.Stop - CurrentSub.Start;
    var resultList = new Array();
    var targetDuration = 0;

    // RS
    if (this.ParamMode.Value != 2) {
        var rs = Custom.getRsFromLengthDuration(len, duration);

        if (rs >= Custom.getRsFromHighLevel(this.ParamMaxRsLevel.Value) ||
            rs < Custom.getRsFromLowLevel(this.ParamMinRsLevel.Value))
        {
            resultList.push(Custom.getReadingSpeedRating(rs));
        } else if (this.ParamOptimize.Value) {
            targetDuration = Math.ceil(
                Custom.getDurationFromLengthRs(len, this.RsTarget));
        }
    }

    // CPS
    if (this.ParamMode.Value != 1) {
        var cps = Custom.getCpsFromLengthDuration(len, duration);

        if (cps > this.ParamMaxCps.Value) {
            resultList.push(Custom.formatMessage(this.TooHighCpsMessage,
                {maxCps: this.ParamMaxCps.Value}));
        } else if (cps < this.ParamMinCps.Value) {
            resultList.push(Custom.formatMessage(this.TooLowCpsMessage,
                {minCps: this.ParamMinCps.Value}));
        } else if (this.ParamOptimize.Value) {
            var targetDurationCps = Math.round(
                Custom.getDurationFromLengthCps(len, VSSCore.CpsTarget));

            if (targetDurationCps > targetDuration) {
                targetDuration = targetDurationCps;
            }
        }
    }

    if (resultList.length) {
        return resultList.join(this.JoinMessage);
    }

    // Check for optimizable subtitles.
    if (targetDuration) {
        var newStop = this.GetOptimalStop(targetDuration, CurrentSub, NextSub);
        var diff = newStop - CurrentSub.Stop;

        if (diff > 0 && this.ParamOptimize.Value != 2) {
            return Custom.formatMessage(this.OptimizableMessage,
                {valueSeconds: "+" + diff / 1000.});
        } else if (diff < 0 && this.ParamOptimize.Value != 1 &&
            newStop - CurrentSub.Start == targetDuration)
        {
            return Custom.formatMessage(this.OptimizableMessage,
                {valueSeconds: diff / 1000.});
        }
    }

    return "";
  },

  FixError : function(CurrentSub, PreviousSub, NextSub) {
    this.RsTarget = this.ParamRsTarget.Value;
    var len = CurrentSub.StrippedText.length;
    var duration = CurrentSub.Stop - CurrentSub.Start;
    var maxDuration = Infinity;
    var minDuration = 0;
    var targetDuration = 0;

    // RS
    if (this.ParamMode.Value != 2) {
        var rs = Custom.getRsFromLengthDuration(len, duration);
        var minRs = Custom.getRsFromLowLevel(this.ParamMinRsLevel.Value);

        if (rs < minRs) {
            maxDuration = Math.floor(
                Custom.getDurationFromLengthRs(len, minRs));
        } else {
            targetDuration = Math.ceil(
                Custom.getDurationFromLengthRs(len, this.RsTarget));
            var maxRs = Custom.getRsFromHighLevel(this.ParamMaxRsLevel.Value);

            if (rs >= maxRs) {
                minDuration = Math.ceil(
                    Custom.getDurationFromLengthRs(len, maxRs - .05));
            }
        }
    }

    // CPS
    if (this.ParamMode.Value != 1) {
        var cps = Custom.getCpsFromLengthDuration(len, duration);

        if (cps < this.ParamMinCps.Value) {
            var maxDurationCps = Math.floor(
                Custom.getDurationFromLengthCps(len, this.ParamMinCps.Value));

            if (maxDurationCps < maxDuration) {
                maxDuration = maxDurationCps;
            }
        } else {
            var targetDurationCps = Math.round(
                Custom.getDurationFromLengthCps(len, VSSCore.CpsTarget));

            if (targetDurationCps > targetDuration) {
                targetDuration = targetDurationCps;
            }

            if (cps > this.ParamMaxCps.Value) {
                var minDurationCps = Math.ceil(
                    Custom.getDurationFromLengthCps(
                    len, this.ParamMaxCps.Value));

                if (minDurationCps > minDuration) {
                    minDuration = minDurationCps;
                }
            }
        }
    }

    // Too low speed.
    if (isFinite(maxDuration)) {
        CurrentSub.Stop = CurrentSub.Start + maxDuration;
        return;
    }

    var newStop = this.GetOptimalStop(targetDuration, CurrentSub, NextSub);

    if (newStop > CurrentSub.Stop || this.ParamOptimize.Value > 1) {
        CurrentSub.Stop = newStop;
    }

    // If needed, try to move the start timing.
    if (this.ParamMaxStartMove.Value) {
        // Maximum allowed start timing to be below the maximum speed.
        var maxStart = CurrentSub.Stop - minDuration;

        // Exit if the speed is below the maximum.
        if (CurrentSub.Start <= maxStart) {
            return;
        }

        var newStart = Custom.getNonOverlappedStart(
            Math.max(maxStart, CurrentSub.Start - this.ParamMaxStartMove.Value),
            PreviousSub, SceneChange.GetPrevious(CurrentSub.Start));

        if (newStart < CurrentSub.Start) {
            CurrentSub.Start = newStart;
        }
    }

    // If needed, try to break scene changes.
    if (this.ParamBreakSceneChanges.Value) {
        // Minimum allowed stop timing to be below the maximum speed.
        //var minStop = CurrentSub.Start + minDuration;

        // Check against the strict minimum stop timing.
        this.CheckMode();
        var strictMinDuration = StrictMinDuration[this.ParamMode.Value](len);
        var strictMinStop = CurrentSub.Start + strictMinDuration;

        // Exit if the speed is below the strict maximum.
        if (CurrentSub.Stop >= strictMinStop) {
            return;
        }

        // The speed is still above the strict maximum,
        // so we try to break the scene changes.
        //ScriptLog("Try to break the scene changes.");

        newStop = this.GetNonOverlappedSceneChangeStop(
            Custom.getNonOverlappedStop(CurrentSub.Start +
            Math.min(targetDuration, this.ParamMaxDuration.Value), NextSub));

        // Apply the new stop timing only if it fixes the speed.
        if (newStop >= strictMinStop) {
            CurrentSub.Stop = newStop;
        }
    }
  },

  GetOptimalStop : function(targetDuration, CurrentSub, NextSub) {
    return Custom.getNonOverlappedStop(CurrentSub.Start +
        Math.min(targetDuration, this.ParamMaxDuration.Value),
        NextSub, SceneChange.GetNext(CurrentSub.Stop));
  },

  GetNonOverlappedSceneChangeStop : function(stop) {
    // Get the scene change next to the stop timing.
    var nextScene = SceneChange.GetNext(stop);

    if (nextScene >= 0) {
        var nextSceneStart = nextScene - SceneChange.StartOffset;

        if (nextSceneStart < stop) {
            stop = nextSceneStart;
        }
    }

    // Get the scene change previous to the stop timing.
    var previousScene = SceneChange.GetPrevious(stop);

    if (previousScene >= 0) {
        var previousSceneStop = previousScene + SceneChange.StopOffset;

        if (previousSceneStop >= stop) {
            stop = previousScene - SceneChange.StartOffset;
        }
    }

    return stop;
  },

  CheckMode : function() {
    // We can't afford an error when indexing the function arrays.
    if (!(this.ParamMode.Value >= 1 && this.ParamMode.Value <= 3)) {
        this.ParamMode.Value = 1;
    }
  },
};

var StrictMinDuration = new Array(
    undefined,

    // RS mode.
    function(len) {
        return Math.ceil(Custom.getDurationFromLengthRs(len,
                Math.max(
                    Custom.getRsFromHighLevel(VSSPlugin.ParamMaxRsLevel.Value),
                    VSSPlugin.strictMaxRsTarget
                ) - .05
            ));
    },

    // CPS mode.
    function(len) {
        return Math.ceil(Custom.getDurationFromLengthCps(len,
                Math.max(VSSPlugin.ParamMaxCps.Value,
                VSSPlugin.strictMaxCpsTarget)
            ));
    },

    // RS and CPS mode.
    function(len) {
        return Math.max(this[1](len), this[2](len));
    }
);

// Inlined custom.js
var Custom = {

// Get higher reading speed from level. 1: 35, 2: 31, 3: 27, 4: 23.
getRsFromHighLevel : function(level) {
    // return 39 - 4 * level;
    return [35, 31, 27, 23, 20.05][level - 1] || 35;
},

// Get lower reading speed from level. 1: 5, 2: 10, 3: 13, 4: 15.
getRsFromLowLevel : function(level) {
    return [5, 10, 13, 15, 20][level - 1] || 5;
},

// Get reading speed from text length and duration.
getRsFromLengthDuration : function(len, duration) {
    if (duration < 500) {
        return Infinity;
    }

    return len * 1000 / (duration - 500);
},

// Get duration from text length and reading speed.
getDurationFromLengthRs : function(len, rs) {
    return len * 1000 / rs + 500;
},

// Get characters per second from text length and duration.
getCpsFromLengthDuration : function(len, duration) {
    return len * 1000 / duration;
},

// Get duration from text length and characters per second.
getDurationFromLengthCps : function(len, cps) {
    return len * 1000 / cps;
},

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

// Reading speed definitions.
READING_SPEED_DEF : [
    // Pure green hue = 120 degrees (red = 0 degree, blue = 240 degrees)
    // Base color: 0x99ff99
    {value: 5,        color: 0x9999ff /* 240 */, text: "TOO SLOW!"},
    {value: 10,       color: 0x99ccff /* 210 */, text: "Slow, acceptable."},
    {value: 13,       color: 0x99ffff /* 180 */, text: "A bit slow."},
    {value: 15,       color: 0x99ffcc /* 150 */, text: "Good."},
    {value: 23,       color: 0x99ff99 /* 120 */, text: "Perfect."},
    {value: 27,       color: 0xccff99 /*  90 */, text: "Good."},
    {value: 31,       color: 0xffff99 /*  60 */, text: "A bit fast."},
    {value: 35,       color: 0xffcc99 /*  30 */, text: "Fast, acceptable."},
    {value: Infinity, color: 0xff9999 /*   0 */, text: "TOO FAST!"}
],

// Get reading speed rating.
getReadingSpeedRating : function(rs) {
    var index = Custom.getReadingSpeedIndex(rs);
    return Custom.READING_SPEED_DEF[index].text;
},

// Get reading speed index.
getReadingSpeedIndex : function(rs) {
    for (var i = 0, len = Custom.READING_SPEED_DEF.length - 1; i < len; ++i) {
        if (rs < Custom.READING_SPEED_DEF[i].value) {
            return i;
        }
    }

    return len;
},

// Custom
};
