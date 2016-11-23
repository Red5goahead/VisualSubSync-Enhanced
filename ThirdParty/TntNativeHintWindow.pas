unit TntNativeHintWindow;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Contributor(s):
 *   Xu, Qian <http://stanleyxu2005.blogspot.com/>
 *
 * ***** END LICENSE BLOCK ***** *)

 {
    Known Limition(s):

     1. Once THintWindow is replaced with THintWindowFix, the procedure
        TApplication.HideHint() must be patched as well. However, after
        patching the original method pointer will be discarded, which
        means, that it is impossible to revert to THintWindow again.

     2. TApplication.ActivateHint() doesn't work with THintWindowFix
        properly. Please use ApplicationActivateHint() instead.
        To note, that I have tried to patch TApplication.ActivateHint(),
        but not tooltip can be shown any more. I will be appreciated,
        if you can help me to fix this issue ^^)
 }

interface

uses
  Classes, Controls, Windows, Messages, TntControls;

type
  TTntHintWindowFix = class(TTntCustomHintWindow)
  private
    OldAppHintHandler: TNotifyEvent;
    function NativeActivateHintEx(PopupPoint: TPoint; const HintTextA: AnsiString): THandle;
  protected
    procedure AppHintHandler(Sender: TObject);
    function CalcHintRectA(MaxWidth: Integer; const AHint: AnsiString; AData: Pointer): TRect;
    function CalcHintRectW(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect;
  // BLOCK-BEGIN: Disable the following procedures.
  private
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure NCPaint(DC: HDC); override;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
  // BLOCK-END
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReleaseHandle;
    procedure ActivateHintData(Rect: TRect; const AHint: AnsiString; AData: Pointer); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

// A replacement of TntApplication.ActivateHint() when HintWindowClass=TTntHintWindowFix.
procedure TntApplicationActivateHint(CursorPos: TPoint);
// Applies the patch of HintWindowClass.
procedure PatchHintWindowClass;


var
  AutoPatchOnStartup: Boolean = True;


implementation

uses
  ActnList, CommCtrl, Forms, Menus, SysUtils, TntForms, TntMenus, TntSysUtils,
  FastcodePatch{http://fastcode.sourceforge.net/};




{ Utils }

function MeasureSingleLineStringU(const S: WideString): TSize;
var
  DC: HDC;
  hOldFont: HFONT;
begin
  FillChar(Result, SizeOf(Result), #0);
  DC := GetDC(0);
  try
    hOldFont := SelectObject(DC, Screen.HintFont.Handle);
    if Win32PlatformIsUnicode then
         GetTextExtentPoint32W(DC, PWideChar(S), Length(S), Result)
    else GetTextExtentPoint32A(DC, PAnsiChar(AnsiString(S)), Length(AnsiString(S)), Result);
    SelectObject(DC, hOldFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

function MeasureStringU(S: WideString; LineMargin: Integer = 0): TSize;
const
  cLengthOfLineBreak = Length(sLineBreak);
var
  P: Integer;
  Dim: TSize;
begin
  Result.cx := 0;
  Result.cy := 0;

  P := Pos(sLineBreak, S);
  while P > 0 do
  begin
    Dim := MeasureSingleLineStringU(Copy(S, 1, P - 1));
    if Result.cx < Dim.cx then
      Result.cx := Dim.cx;
    Inc(Result.cy, Dim.cy + LineMargin);
    Delete(S, 1, P + cLengthOfLineBreak - 1);
    P := Pos(sLineBreak, S);
  end;
  Dim := MeasureSingleLineStringU(S);
  if Result.cx < Dim.cx then
    Result.cx := Dim.cx;
  Inc(Result.cy, Dim.cy);
end;

function DataPointsToHintInfoForTnt(AData: Pointer): Boolean;
// Copied from TntControls v2.3.0 (free version)
begin
  try
    Result := (AData <> nil)
          and (PHintInfo(AData).HintData = AData) {points to self}
          and (PHintInfo(AData).HintWindowClass.InheritsFrom(TTntCustomHintWindow));
  except
    Result := False;
  end;
end;

function ExtractTntHintCaption(AData: Pointer): WideString;
// Copied from TntControls v2.3.0 (free version)
var
  Control: TControl;
  WideHint: WideString;
  AnsiHintWithShortCut: AnsiString;
  ShortCut: TShortCut;
begin
  Result := PHintInfo(AData).HintStr;
  if Result <> '' then begin
    Control := PHintInfo(AData).HintControl;
    WideHint := WideGetShortHint(WideGetHint(Control));
    if (AnsiString(WideHint) = PHintInfo(AData).HintStr) then
      Result := WideHint
    else if Application.HintShortCuts and (Control <> nil)
    and (Control.Action is TCustomAction{TNT-ALLOW TCustomAction}) then begin
      ShortCut := TCustomAction{TNT-ALLOW TCustomAction}(Control.Action).ShortCut;
      if (ShortCut <> scNone) then
      begin
        AnsiHintWithShortCut := Format{TNT-ALLOW Format}('%s (%s)', [WideHint, ShortCutToText{TNT-ALLOW ShortCutToText}(ShortCut)]);
        if AnsiHintWithShortCut = PHintInfo(AData).HintStr then
          Result := WideFormat('%s (%s)', [WideHint, WideShortCutToText(ShortCut)]);
      end;
    end;
  end;
end;



{ TTntHintWindowFix }

var
  HintWindowInstances: TList;

function TTntHintWindowFix.NativeActivateHintEx(PopupPoint: TPoint; const HintTextA: AnsiString): THandle;
var
  MousePos, HintWinDim: TPoint;
  Mon: TMonitor;
  MonRect, TmpRect: TRect;
  TIA: TOOLINFOA;
  TIW: TOOLINFOW;
  hControl: THandle;
  HintTextW: WideString;
begin
  Result := CreateWindow(TOOLTIPS_CLASS, nil, WS_POPUP or TTS_NOPREFIX or TTS_ALWAYSTIP,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),
    0, 0, HInstance, nil);
  if Result <> 0 then
  begin
    SetWindowPos(Result, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    // Retrieves handle before PopupPoint is changed.
    hControl := WindowFromPoint(PopupPoint);

    // Calculates the rect of the hint window.
    Mon := Screen.MonitorFromPoint(PopupPoint);
    if Mon <> nil then
         MonRect := Mon.BoundsRect
    else MonRect := Screen.DesktopRect;
    if Win32PlatformIsUnicode then
    begin
      HintTextW := UTF8Decode(HintTextA);
      TmpRect := CalcHintRectW(MonRect.Right - MonRect.Left, HintTextW, nil);
    end
    else
      TmpRect := CalcHintRectA(MonRect.Right - MonRect.Left, HintTextA, nil);
    HintWinDim.X := TmpRect.Right - TmpRect.Left;
    HintWinDim.Y := TmpRect.Bottom - TmpRect.Top;
    FillChar(TmpRect, SizeOf(TmpRect), #0);
    SendMessage(Result, {TTM_ADJUSTRECT}WM_USER+31, 1, WPARAM(@TmpRect));
    Inc(HintWinDim.X, (TmpRect.Right - TmpRect.Left) * 2);
    Inc(HintWinDim.Y, (TmpRect.Bottom - TmpRect.Top) * 2);
    // Corrects the position of the hint window, when it is partial out of the screen.
    if PopupPoint.Y + HintWinDim.Y >= MonRect.Bottom then
    begin
      GetCursorPos(MousePos);
      PopupPoint.Y := MousePos.Y - HintWinDim.Y;
    end;
    if PopupPoint.X + HintWinDim.X >= MonRect.Right then
      PopupPoint.X := MonRect.Right - HintWinDim.X;
    if PopupPoint.X < MonRect.Left then
      PopupPoint.X := MonRect.Left;
    if PopupPoint.Y < MonRect.Top then
      PopupPoint.Y := MonRect.Top;

    // Enables multi-line hint (delimeter #13#10)
    SendMessage(Result, TTM_SETMAXTIPWIDTH, 0, MonRect.Right - MonRect.Left);
    // Specifies the text font style
    //SendMessage(Result, WM_SETFONT, Screen.HintFont.Handle, Integer(LongBool(False)))

    if Win32PlatformIsUnicode then
    begin
      FillChar(TIW, SizeOf(TIW), #0);
      TIW.cbSize   := SizeOf(TIW);
      TIW.hwnd     := hControl;
      TIW.lpszText := PWideChar(HintTextW);
      TIW.uFlags   := TTF_TRANSPARENT or TTF_SUBCLASS or TTF_TRACK or TTF_ABSOLUTE;
      SendMessage(Result, TTM_ADDTOOLW, 0, Integer(@TIW));
      SendMessage(Result, TTM_TRACKPOSITION, 0, MAKELPARAM(PopupPoint.X, PopupPoint.Y + 1));
      SendMessage(Result, TTM_TRACKACTIVATE, Integer(LongBool(True)), Integer(@TIW));
    end else
    begin
      FillChar(TIA, SizeOf(TIA), #0);    
      TIA.cbSize   := SizeOf(TIA);
      TIA.hwnd     := hControl;
      TIA.lpszText := PAnsiChar(HintTextA);
      TIA.uFlags   := TTF_TRANSPARENT or TTF_SUBCLASS or TTF_TRACK or TTF_ABSOLUTE;
      SendMessage(Result, TTM_ADDTOOLA, 0, Integer(@TIA));
      SendMessage(Result, TTM_TRACKPOSITION, 0, MAKELPARAM(PopupPoint.X, PopupPoint.Y + 1));
      SendMessage(Result, TTM_TRACKACTIVATE, Integer(LongBool(True)), Integer(@TIA));
    end;
  end;
end;

constructor TTntHintWindowFix.Create(AOwner: TComponent);
begin
  inherited;
  OldAppHintHandler  := Application.OnHint;
  Application.OnHint := AppHintHandler;
  // http://msdn2.microsoft.com/en-us/library/aa511495.aspx
  Application.HintPause      := GetDoubleClickTime();
  Application.HintShortPause := GetDoubleClickTime() div 5;
  Application.HintHidePause  := GetDoubleClickTime() * 10;

  // Adds to global instance register
  HintWindowInstances.Add(Self);
end;

destructor TTntHintWindowFix.Destroy;
begin
  // Removes from global instance register
  HintWindowInstances.Remove(Self);

  Application.OnHint := OldAppHintHandler;
  inherited;
end;

procedure TTntHintWindowFix.CreateParams(var Params: TCreateParams);
begin
  //Do nothing
end;

procedure TTntHintWindowFix.WMNCPaint(var Message: TMessage);
begin
  //Do nothing
end;

procedure TTntHintWindowFix.Paint;
begin
  //Do nothing
end;

procedure TTntHintWindowFix.CMTextChanged(var Message: TMessage);
begin
  //Do nothing
end;

procedure TTntHintWindowFix.NCPaint(DC: HDC);
begin
  //Do nothing
end;

procedure TTntHintWindowFix.WMPrint(var Message: TMessage);
begin
  //Do nothing
end;

procedure TTntHintWindowFix.AppHintHandler(Sender: TObject);
begin
  if Trim(GetShortHint(Application.Hint)) = '' then
  begin
    ReleaseHandle;
  end;
end;

type
  THintWindowHack = class(THintWindow) // CAUTION: Please check the declaration
  private                              //          of Controls.THintWindow
    FActivating: Boolean;
    FLastActive: Cardinal;
  end;

procedure TTntHintWindowFix.ReleaseHandle;
begin
  with THintWindowHack(Self) do
    if IsWindow(FLastActive) then
      DestroyWindow(FLastActive);
end;

procedure TTntHintWindowFix.ActivateHintData(Rect: TRect; const AHint: AnsiString; AData: Pointer);
begin
  if Win32PlatformIsUnicode and DataPointsToHintInfoForTnt(AData) then
       ActivateHint(Rect, UTF8Encode(ExtractTntHintCaption(AData)))
  else inherited;
end;

procedure TTntHintWindowFix.ActivateHint(Rect: TRect; const AHint: string);
begin
  with THintWindowHack(Self) do
  begin
    if IsWindow(FLastActive) then // Copied from ReleaseHandle()
    begin
      if AHint = Caption then
        Exit // To avoid the hint to be be drawn multiple times
      else
        DestroyWindow(FLastActive); // Copied from ReleaseHandle()
    end;

    FActivating := True;
    try
      Caption := AHint;
      FLastActive := NativeActivateHintEx(Rect.TopLeft, AHint);
    finally
      FActivating := False;
    end;
  end;
end;

function TTntHintWindowFix.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  Result := CalcHintRectA(MaxWidth, AHint, AData);
end;

function TTntHintWindowFix.CalcHintRectA(MaxWidth: Integer; const AHint: AnsiString; AData: Pointer): TRect;
begin
  //NOTE: Canvas is invalid in TTntHintWindowFix.
  with MeasureStringU(AHint) do
    Result := Rect(0, 0, cx, cy);
end;

function TTntHintWindowFix.CalcHintRectW(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect;
begin
  //NOTE: Canvas is invalid in TTntHintWindowFix.
  with MeasureStringU(AHint) do
    Result := Rect(0, 0, cx, cy);
end;

// A replacement of TntApplication.ActivateHint() when HintWindowClass=TTntHintWindowFix.
procedure TntApplicationActivateHint(CursorPos: TPoint);
begin
  if Application.ShowHint then
  begin
    if HintWindowInstances.Count = 0 then
    begin
      //NOTE: Forces Application to create an instance of its HintWindowClass.
      Application.ShowHint := False;
      Application.ShowHint := True;
    end;
    if HintWindowInstances.Count > 0 then
      TTntHintWindowFix(HintWindowInstances.Items[0]).ActivateHint(
        Rect(CursorPos.X, CursorPos.Y, 0, 0), UTF8Encode(TntApplication.Hint));
  end;
end;



{ RTL patch }

procedure TApplicationHideHintStub;
asm
  call TApplication.HideHint;
end;

type
  TApplicationPatch = class(TApplication)
  public
    procedure HideHint;
    //NOTE: Failed to patch procedure ActivateHint(CursorPos: TPoint);
  end;

procedure TApplicationPatch.HideHint;
begin
  if HintWindowInstances.Count > 0 then
    TTntHintWindowFix(HintWindowInstances.Items[0]).ReleaseHandle;
end;



// Applies the patch of HintWindowClass.
procedure PatchHintWindowClass;
begin
  HintWindowClass := TTntHintWindowFix;
  // This cannot be undone
  FastcodeAddressPatch(
    FastcodeGetAddress(@TApplicationHideHintStub),
    @TApplicationPatch.HideHint);
end;



initialization
  HintWindowInstances := TList.Create;

  if AutoPatchOnStartup then
    PatchHintWindowClass;

finalization
  HintWindowInstances.Free;

end.
