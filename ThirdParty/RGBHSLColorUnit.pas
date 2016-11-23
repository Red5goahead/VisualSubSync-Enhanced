{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclGraphUtils.pas.                                                          }
{                                                                                                  }
{ The Initial Developers of the Original Code are Pelle F. S. Liljendal and Marcel van Brakel.     }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Jack N.A. Bakker
{   Mike Lischke                                                                                   }
{   Robert Marquardt (marquardt)                                                                   }
{   Alexander Radchenko                                                                            }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}

unit RGBHSLColorUnit;

interface

uses Math, Graphics;

procedure RGBToHLS(const R, G, B: Single; out H, L, S: Single);
procedure RGBToHSL(const R, G, B: Byte; out H, S, L: Single);
procedure HLSToRGB(const H, L, S: Single; out R, G, B: Single);
procedure HSLToRGB(const H, S, L: Single; out R, G, B : Byte);

function ChangeColorHue(Color: TColor; NewHue : Single) : TColor;
function ChangeColorHueDeg(Color: TColor; NewHueDeg : Single) : TColor;
function ChangeColorSaturation(Color: TColor; NewSaturation : Single) : TColor;
function ChangeColorLuminance(Color: TColor; NewLuminance : Single) : TColor;

function ChangeColorHSL(Color: TColor; HueOffset, SaturationOffset, LuminanceOffset : Single) : TColor;

implementation

procedure RGBToHLS(const R, G, B: Single; out H, L, S: Single);
var
  D, Cmax, Cmin: Single;
begin
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
    if G = Cmax then
      H := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;

procedure HLSToRGB(const H, L, S: Single; out R, G, B: Single);
var
  M1, M2: Single;

  function HueToColorValue(Hue: Single): Single;
  begin
    Hue := Hue - Floor(Hue);

    if 6 * Hue < 1 then
      Result := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      Result := M2
    else
    if 3 * Hue < 2 then
      Result := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      Result := M1;
  end;
    
begin
  if S = 0 then
  begin
    R := L;
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H + 1 / 3);
    G := HueToColorValue(H);
    B := HueToColorValue(H - 1 / 3)
  end;
end;

procedure RGBToHSL(const R, G, B: Byte; out H, S, L: Single);
begin
  RGBToHLS(R / 255.0, G / 255.0, B / 255.0, H, L, S);
end;

procedure HSLToRGB(const H, S, L: Single; out R, G, B : Byte);
var
  Re, Gr, Bl: Single;
begin
  HLSToRGB(H, L, S, Re, Gr, Bl);
  R := Round(Re * 255);
  G := Round(Gr * 255);
  B := Round(Bl * 255);
end;

function ChangeColorHue(Color: TColor; NewHue : Single) : TColor;
var
  R, G, B, A : Byte;
  H, S, L : Single;
begin
  R := (Color and $000000FF);
  G := (Color and $0000FF00) shr 8;
  B := (Color and $00FF0000) shr 16;
  A := (Color and $FF000000) shr 24;
  RGBToHSL(R, G, B, H, S, L);
  HSLToRGB(NewHue, S, L, R, G, B);
  Result := (A shl 24) or (B shl 16) or (G shl 8) or R;
end;

function ChangeColorHueDeg(Color: TColor; NewHueDeg : Single) : TColor;
var NewHue : Single;
begin
  NewHue := NewHueDeg / 360.0;
  Result := ChangeColorHue(Color, NewHue);
end;

function ChangeColorSaturation(Color: TColor; NewSaturation : Single) : TColor;
var
  R, G, B, A : Byte;
  H, S, L : Single;
begin
  R := (Color and $000000FF);
  G := (Color and $0000FF00) shr 8;
  B := (Color and $00FF0000) shr 16;
  A := (Color and $FF000000) shr 24;
  RGBToHSL(R, G, B, H, S, L);
  HSLToRGB(H, NewSaturation, L, R, G, B);
  Result := (A shl 24) or (B shl 16) or (G shl 8) or R;
end;

function ChangeColorLuminance(Color: TColor; NewLuminance : Single) : TColor;
var
  R, G, B, A : Byte;
  H, S, L : Single;
begin
  R := (Color and $000000FF);
  G := (Color and $0000FF00) shr 8;
  B := (Color and $00FF0000) shr 16;
  A := (Color and $FF000000) shr 24;
  RGBToHSL(R, G, B, H, S, L);
  HSLToRGB(H, S, NewLuminance, R, G, B);
  Result := (A shl 24) or (B shl 16) or (G shl 8) or R;
end;

function Constrain(Value : Single; MinValue, MaxValue : Single) : Single;
begin
  if Value < MinValue then
    Result := MinValue
  else if Value > MaxValue then
    Result := MaxValue
  else
    Result := Value;
end;

function ChangeColorHSL(Color: TColor; HueOffset, SaturationOffset, LuminanceOffset : Single) : TColor;
var
  R, G, B, A : Byte;
  H, S, L : Single;
  NewHue, NewSaturation, NewLuminance : Single;
begin
  R := (Color and $000000FF);
  G := (Color and $0000FF00) shr 8;
  B := (Color and $00FF0000) shr 16;
  A := (Color and $FF000000) shr 24;
  RGBToHSL(R, G, B, H, S, L);

  NewHue := H + HueOffset;
  NewSaturation := Constrain(S + SaturationOffset, 0, 1);
  NewLuminance := Constrain(L + LuminanceOffset, 0, 1);

  HSLToRGB(NewHue, NewSaturation, NewLuminance, R, G, B);
  Result := (A shl 24) or (B shl 16) or (G shl 8) or R;
end;

end.
