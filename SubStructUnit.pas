// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003 Christophe Paris
// -----------------------------------------------------------------------------
//  This Program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.
//
//  This Program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with GNU Make; see the file COPYING.  If not, write to
//  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//  http://www.gnu.org/copyleft/gpl.html
// -----------------------------------------------------------------------------

unit SubStructUnit;

interface

uses WAVDisplayerUnit, VirtualTrees, GlobalUnit, Classes, Graphics;

type
  TDelayShiftType = (dstBothTime, dstStartTimeOnly, dstStopTimeOnly);
  TDelayApplyToType = (dattAll, dattSelected, dattFromCursor);

  TSubtitleRange = class(TRange)
    Text : WideString;

    Effect : WideString;
    Layer : WideString;
    Marked : WideString;
    Actor : WideString;
    RightMarg : WideString;
    LeftMarg : WideString;
    VertMarg : WideString;
    Style: WideString;

    Node : PVirtualNode; // TSubtitleRange and tree node are linked both way

    procedure Assign(const Range : TRange); override;
    procedure SaveToStream(Stream : TStream); override;
    procedure LoadFromStream(Stream : TStream); override;
  end;

  TSubtitleRangeFactory = class(TRangeFactory)
    function CreateRange : TRange; override;
  end;

  TSubtitleRangeWrapper = class(TContext)
    Index : Integer;
    SubRange : TSubtitleRange;
    function GetFieldValue(Name : string; var Found : Boolean) : string; override;
  end;

  TTreeData = record
    Range: TSubtitleRange;
  end;
  PTreeData = ^TTreeData;

  TVSSCoreEngineIntf = interface(IInterface)
    procedure RegisterJavascriptAction(AName, ACaption, ADefaultShortcut : WideString);

    function GetSubCount : Integer;
    function GetFirst : TSubtitleRange;
    function GetNext(SubtitleRange : TSubtitleRange) : TSubtitleRange;
    function GetPrevious(SubtitleRange : TSubtitleRange) : TSubtitleRange;

    function GetSelectedCount : Integer;
    function GetFirstSelected : TSubtitleRange;
    function GetNextSelected(SubtitleRange : TSubtitleRange) : TSubtitleRange;

    function GetPluginParamValue(JsSection, JsParameter : WideString) : Integer;

    procedure JsDeleteSubtitle(Index : Integer);

    procedure DisableJavascriptItemMenu(AName : WideString);
    procedure EnableJavascriptItemMenu(AName : WideString);
    procedure InsertBreakBeforeJavascriptMenuItem(ACaption : WideString);
    procedure InsertBreakAfterJavascriptMenuItem(ACaption : WideString);
    procedure SetPluginParamValue(JsSection, JsParameter : WideString; value: Integer);
    function GetCursorPosition: Integer;
    procedure SetSubColor(StartIndex,LastIndex : Integer; Color : TColor);
    procedure ResetSubColor;
    procedure SetSubIcon(Index : Integer; BitMapIndex : Integer);

    function GetAt(Index : Integer) : TSubtitleRange;

    function GetTextSelectionStart : Integer;
    function GetTextSelectionLength : Integer;
    function GetAudioCursorPosition : Integer;

  end;

  function Sub2SrtString(SubRange : TSubtitleRange) : WideString;  

implementation

uses MiscToolsUnit, SysUtils, TntSysUtils;


// -----------------------------------------------------------------------------

function Sub2SrtString(SubRange : TSubtitleRange) : WideString;
begin
  Result := IntToStr(SubRange.Node.Index + 1) + CRLF +
    TimeMsToString(SubRange.StartTime,',') + ' --> ' + TimeMsToString(SubRange.StopTime,',') + CRLF +
    Subrange.Text;
end;

//==============================================================================

procedure TSubtitleRange.Assign(const Range : TRange);
var SubRange : TSubtitleRange;
begin
  inherited Assign(Range);
  SubRange := TSubtitleRange(Range);
  Self.Text := SubRange.Text;
  Self.Effect := SubRange.Effect;
  Self.Layer := SubRange.Layer;
  Self.Marked := SubRange.Marked;
  Self.Actor := SubRange.Actor;
  Self.RightMarg := SubRange.RightMarg;
  Self.LeftMarg := SubRange.LeftMarg;
  Self.VertMarg := SubRange.VertMarg;
  Self.Style := SubRange.Style;
end;

procedure TSubtitleRange.SaveToStream(Stream : TStream);
begin
  inherited SaveToStream(Stream);

  SaveToStreamWS(Stream, Self.Text);
  SaveToStreamWS(Stream, Self.Effect);
  SaveToStreamWS(Stream, Self.Layer);
  SaveToStreamWS(Stream, Self.Marked);
  SaveToStreamWS(Stream, Self.Actor);
  SaveToStreamWS(Stream, Self.RightMarg);
  SaveToStreamWS(Stream, Self.LeftMarg);
  SaveToStreamWS(Stream, Self.VertMarg);
  SaveToStreamWS(Stream, Self.Style);
end;

procedure TSubtitleRange.LoadFromStream(Stream : TStream);
begin
  inherited LoadFromStream(Stream);

  LoadFromStreamWS(Stream, Self.Text);
  LoadFromStreamWS(Stream, Self.Effect);
  LoadFromStreamWS(Stream, Self.Layer);
  LoadFromStreamWS(Stream, Self.Marked);
  LoadFromStreamWS(Stream, Self.Actor);
  LoadFromStreamWS(Stream, Self.RightMarg);
  LoadFromStreamWS(Stream, Self.LeftMarg);
  LoadFromStreamWS(Stream, Self.VertMarg);
  LoadFromStreamWS(Stream, Self.Style);
end;


function TSubtitleRangeFactory.CreateRange : TRange;
begin
  Result := TSubtitleRange.Create;
  Result.StartTime := 0;
  Result.StopTime := 0;
  SetLength(Result.SubTime, 0);
  TSubtitleRange(Result).Text := '';
  TSubtitleRange(Result).Effect := '';
  TSubtitleRange(Result).Layer := '0';
  TSubtitleRange(Result).Marked := 'Marked=0';
  TSubtitleRange(Result).Actor := '';
  TSubtitleRange(Result).RightMarg := '0000';
  TSubtitleRange(Result).LeftMarg := '0000';
  TSubtitleRange(Result).VertMarg := '0000';
  TSubtitleRange(Result).Style := 'Default';
  TSubtitleRange(Result).Node := nil;
end;

//------------------------------------------------------------------------------

function TSubtitleRangeWrapper.GetFieldValue(Name : string; var Found : Boolean) : string;
var i : Integer;
begin
  Found := True;
  if (Name = 'index') then
    Result := IntToStr(Index)
  else if (Name = 'time-index') then
    Result := Format('%x-%x',[SubRange.StartTime,SubRange.StopTime])
  else if (Name = 'start') then
    Result := TimeMsToString(SubRange.StartTime)
  else if (Name = 'stop') then
    Result := TimeMsToString(SubRange.StopTime)
  else if (Name = 'html-text') then
    Result := StringConvertCRLFToBR(SubRange.Text)
  else if (Name = 'raw-text') then
    Result := SubRange.Text
  else if (Name = 'stripped-text') then
    Result := StripTags(SubRange.Text)
  else if (Name = 'html-text8') then
    Result := UTF8Encode(StringConvertCRLFToBR(SubRange.Text))
  else if (Name = 'raw-text8') then
    Result := UTF8Encode(SubRange.Text)
  else if (Name = 'wav-size') then
  begin
    i := Round(((SubRange.StopTime - SubRange.StartTime) / 1000) * g_GlobalContext.WavAverageBytePerSecond);
    if (i < 1024) then
      Result := IntToStr(i) + ' B'
    else if (i < 1024*1024) then
      Result := IntToStr(i div 1024) + ' KB'
    else
      Result := IntToStr(i div 1024 div 1024) + ' MB';
  end
  else if (Name = 'index-padded') then
    Result := Format('%6.6d',[Index])
  else
    Found := False
end;

//==============================================================================
end.
//==============================================================================
