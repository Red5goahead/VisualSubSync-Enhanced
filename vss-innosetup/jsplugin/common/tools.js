// ---------------------------------------------------------------------------

// Round a number to 1 decimal
// eg: 1.23 => 1.2
function decimal1Round(avalue) {
  return Math.round(avalue * 10) / 10;
}

// ---------------------------------------------------------------------------
 
var RSDef = [
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
 
var CPSDef = [
  { value: 5,        color: 0x9999FF, text: 'TOO SLOW!'},
  { value: 10,       color: 0x99CCFF, text: 'Slow, acceptable.'},
  { value: 15,       color: 0x99FFFF, text: 'A bit slow.'},
  { value: 18,       color: 0x99FFCC, text: 'Good.'},
  { value: 20,       color: 0x99FF99, text: 'Perfect.'},
  { value: 25,       color: 0xCCFF99, text: 'Good.'},
  { value: 27,       color: 0xFFFF99, text: 'A bit fast.'},
  { value: 30,       color: 0xFFCC99, text: 'Fast, acceptable.'},
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
  for (var i = 0; i < RSDef.length; i++) {
    if (rs < RSDef[i].value) {
      return RSDef[i].text;
    }
  }
  return (RSDef.text - 1);
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
  for (var i = 0; i < RSDef.length; i++) {
    if (rs < RSDef[i].value) {
      return RSDef[i].color;
    }
  }
  return (RSDef.color - 1);
}

// ---------------------------------------------------------------------------

function getReadingSpeedIndex(rs) {
  for (var i = 0; i < RSDef.length; i++) {
    if (rs < RSDef[i].value) {
      return i;
    }
  }
  return (RSDef.length - 1);
}

// ---------------------------------------------------------------------------

function getCPS(Sub) {
  var len = Sub.StrippedText.length;
  var durMs = Sub.Stop - Sub.Start;
  var cps = (len * 1000) / (durMs);
  return cps;
}

// ---------------------------------------------------------------------------

function getCPSRating(cps) {
  for (var i = 0; i < CPSDef.length; i++) {
    if (cps < CPSDef[i].value) {
      return CPSDef[i].text;
    }
  }
  return (CPSDef.text - 1);
}
// ---------------------------------------------------------------------------

function getCPSAsText(Sub) {
  var cps = getCPS(Sub);  
  var cpsRounded = decimal1Round(cps);
  return '' + cpsRounded;
}

// ---------------------------------------------------------------------------

function getCPSAsColor(Sub) {
  var cps = getCPS(Sub);
  for (var i = 0; i < CPSDef.length; i++) {
    if (cps < CPSDef[i].value) {
      return CPSDef[i].color;
    }
  }
  return (CPSDef.color - 1);
}

// ---------------------------------------------------------------------------

function getCPSIndex(cps) {
  for (var i = 0; i < CPSDef.length; i++) {
    if (cps < CPSDef[i].value) {
      return i;
    }
  }
  return (CPSDef.length - 1);
}

// ---------------------------------------------------------------------------