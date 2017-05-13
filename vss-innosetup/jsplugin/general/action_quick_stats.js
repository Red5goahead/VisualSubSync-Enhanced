// ---------------------------------------------------------------------------

JSAction_QuickStats = {
  onExecute : function() {
    var subCount = VSSCore.GetSubCount();
    ScriptLog('--- Quick stats ----------');
    ScriptLog('Subtitle count : ' + subCount);
    
    if(subCount > 0) {
    
      // Table to store results
      var rsArray = new Array(RSDef.length);
      for (var i = 0; i < rsArray.length; i++) {
        rsArray[i] = 0;
      }
      
      // Iterate over all subtitles to collect stats
      var sub = VSSCore.GetFirst();
      while (sub != null) {
        var rs = getReadingSpeed(sub);
        var rsIdx = getReadingSpeedIndex(rs);
        rsArray[rsIdx]++;
        sub = VSSCore.GetNext(sub);
      }
      
      // Display results
      for (var i = 0; i < rsArray.length; i++) {
        var rsCount = rsArray[i];
        var rsCountPercent = (rsCount * 100) / subCount;
        ScriptLog(RSDef[i].text + ' = ' + decimal1Round(rsCountPercent)
        	+ '% (' + rsCount + ')');
      }
      
    }
    
    ScriptLog('---');
  }
};

VSSCore.RegisterJavascriptAction('JSAction_QuickStats', 'Quick stats', 'Ctrl+M');

// ---------------------------------------------------------------------------
