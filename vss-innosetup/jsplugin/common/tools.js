// ---------------------------------------------------------------------------

// Round a number to 1 decimal
// eg: 1.23 => 1.2
function decimal1Round(avalue) {
  return Math.round(avalue * 10) / 10;
}

// ---------------------------------------------------------------------------
 
var ReadingSpeedDef = [
  { value: 5,        color: 0x9999FF, text: 'TOO SLOW!'},
  { value: 10,       color: 0x99CCFF, text: 'Slow, acceptable.'},
  { value: 13,       color: 0x99FFFF, text: 'A bit slow.'},
  { value: 15,       color: 0x99FFCC, text: 'Good.'},
  { value: 23,       color: 0x99FF99, text: 'Perfect.'},
  { value: 27,       color: 0xCCFF99, text: 'Good.'},
  { value: 31,       color: 0xFFFF99, text: 'A bit fast.'},
  { value: 35,       color: 0xFFCC99, text: 'Fast, acceptable.'},
  { value: Infinity, color: 0xFF9999, text: 'TOO FAST!'}
];

// Pure green hue = 120° (red = 0°, blue = 240°)
// Base color : 0x99FF99
// 240° "TOO SLOW!"
// 210° "Slow, acceptable.";
// 180° "A bit slow.";
// 150° "Good."
// 120° "Perfect.";
//  90° "Good.";
//  60° "A bit fast.";
//  30° "Fast, acceptable.";
//   0° "TOO FAST!";

// ---------------------------------------------------------------------------

function getReadingSpeed(Sub) {
  var len = Sub.StrippedText.length;
  var durMs = Sub.Stop - Sub.Start;
  if (durMs < 500) {
    durMs = 500;
  }
  var rs = (len * 1000) / (durMs - 500);
  return rs;
}

// ---------------------------------------------------------------------------

function getReadingSpeedRating(rs) {
    // rating
    var rating;
    if      (rs < 5)  return "TOO SLOW!";
    else if (rs < 10) return "Slow, acceptable.";
    else if (rs < 13) return "A bit slow.";
    else if (rs < 15) return "Good.";
    else if (rs < 23) return "Perfect.";
    else if (rs < 27) return "Good.";
    else if (rs < 31) return "A bit fast.";
    else if (rs < 35) return "Fast, acceptable.";
    else              return "TOO FAST!";
}
// ---------------------------------------------------------------------------

function getReadingSpeedAsText(Sub) {
  var rs = getReadingSpeed(Sub);  
  var rsRounded = decimal1Round(rs);
  return '' + rsRounded;
}

// ---------------------------------------------------------------------------

function getReadingSpeedAsColor(Sub) {
  var rs = getReadingSpeed(Sub);
  
  // Pure green hue = 120° (red = 0°, blue = 240°)
  // Base color : 0x99FF99

  if (rs < 5)       return 0x9999FF;  // 240° "TOO SLOW!"
  else if (rs < 10) return 0x99CCFF;  // 210° "Slow, acceptable.";
  else if (rs < 13) return 0x99FFFF;  // 180° "A bit slow.";
  else if (rs < 15) return 0x99FFCC;  // 150° "Good."
  else if (rs < 23) return 0x99FF99;  // 120° "Perfect.";
  else if (rs < 27) return 0xCCFF99;  //  90° "Good.";
  else if (rs < 31) return 0xFFFF99;  //  60° "A bit fast.";
  else if (rs < 35) return 0xFFCC99;  //  30° "Fast, acceptable.";
  else              return 0xFF9999;  //   0° "TOO FAST!";
}

// ---------------------------------------------------------------------------

function getReadingSpeedIndex(rs) {
  for (var i = 0; i < ReadingSpeedDef.length; i++) {
    if (rs < ReadingSpeedDef[i].value) {
      return i;
    }
  }
  return (ReadingSpeedDef.length - 1);
}

// ---------------------------------------------------------------------------