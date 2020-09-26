// Flashing plugin for VisualSubSync
// nixxo github.com/nixxo

VSSPlugin = {
    // Plugin constants
    Name: "Flashing",
    Description: "Detects if the sub start or stop is near a scenechange" +
        "and prevent the sub from flashing or bleeding into another scene.",
    Color: 0x000ff0,
    Message: "Flash",

    // Plugin parameters available from VSS GUI (name must start with "Param")
    ParamStrictMargin: {
        Value: 100,
        Unit: "ms",
        Description: "Margin of time to detect a correction (default: 100 ms)."
    },

    HasError: function(CurrentSub, PreviousSub, NextSub) {
        var msg = "";
        var mb = VSSCore.MinimumBlank;
        var mrg = this.ParamStrictMargin.Value - mb;
        var stt = CurrentSub.Start;
        var sc1 = SceneChange.GetNext(stt);
        var sc2 = SceneChange.GetPrevious(stt);

        if ((sc1 > 0) && ((sc1 - stt) < mrg) && (sc1 != stt)) {
            msg += "fix start >>>";
        } else if ((sc1 > 0) && ((stt - sc2) < mrg) && (sc2 != stt)) {
            msg += "fix start <<<";
        }

        var stt = CurrentSub.Stop;
        var sc1 = SceneChange.GetNext(stt);
        var sc2 = SceneChange.GetPrevious(stt);
        if ((sc1 > 0) && ((sc1 - stt) < mrg) && (sc1 - mb != stt) && (sc1 != stt)) {
            if (msg != "") msg += " + ";
            msg += "fix stop >>>";
        } else if ((sc2 > 0) && ((stt - sc2) < mrg) && (sc2 - mb != stt)) {
            if (msg != "") msg += " + ";
            msg += "fix stop <<<";
        }

        return msg;
    },

    FixError: function(CurrentSub, PreviousSub, NextSub) {
        var mb = VSSCore.MinimumBlank;
        var mrg = this.ParamStrictMargin.Value - mb;
        var stt = CurrentSub.Start;
        var sc1 = SceneChange.GetNext(stt);
        var sc2 = SceneChange.GetPrevious(stt);
        if (((sc1 - stt) < mrg) && (sc1 != stt)) {
            CurrentSub.Start = sc1;
        } else if (((stt - sc2) < mrg) && (sc2 != stt)) {
            CurrentSub.Start = sc2;
        }

        var stt = CurrentSub.Stop;
        var sc1 = SceneChange.GetNext(stt);
        var sc2 = SceneChange.GetPrevious(stt);
        if (((sc1 - stt) < mrg) && (sc1 != stt)) {
            CurrentSub.Stop = sc1 - mb;
        } else if ((stt - sc2) < mrg) {
            CurrentSub.Stop = sc2 - mb;
        }
    }
};