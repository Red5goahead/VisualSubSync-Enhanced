// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2006 Christophe Paris
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
unit SSAParserUnit;

interface

uses Classes, TntClasses;

type
  TAbstractLineParser = class
    procedure ParseLine(line: WideString); virtual; abstract;
  end;

  TMemorizerParser = class(TAbstractLineParser)
  private
    memory : WideString;
  public
    procedure ParseLine(line: WideString); override;
  end;

  TFormatedParser = class(TAbstractLineParser)
  private
    keywordLst : TTntStringList;
    parsedLines : TList;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseFormatLine(line: WideString; start : WideString) : Integer;
    function ParseFormatedLine(line: WideString; start : WideString) : Integer;
    function GetCount : Integer;
    function GetValueAsInteger(lineIndex : integer; valueKey : WideString) : Integer;
    function GetValueAsString(lineIndex : integer; valueKey : WideString) : WideString;
    function GetValueAsBoolean(lineIndex : integer; valueKey : WideString) : Boolean;
    function GetValueAsDouble(lineIndex : integer; valueKey : WideString) : Double;
    function KeyExists(key : WideString) : Boolean;
  end;

  TScriptInfoParser = class(TAbstractLineParser)
  private
    KeyValueLst : TTntStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function IsSection(line : WideString) : Boolean;
    procedure ParseLine(line: WideString); override;
    function GetCount : Integer;
    function KeyExists(key : WideString) : Boolean;
    function GetValueAsString(Index : integer) : WideString; overload;
    function GetValueAsString(Key : WideString) : WideString; overload;
    function GetAsString : WideString;
  end;

  TStyleParser = class(TFormatedParser)
  private
    FIsASS : Boolean;
  public
    function IsSection(line : WideString) : Boolean;
    procedure ParseLine(line: WideString); override;

    property IsASS : Boolean read FIsASS;
  end;

  TDialogueParser = class(TFormatedParser)
  public
    function IsSection(line : WideString) : Boolean;
    procedure ParseLine(line: WideString); override;
  end;

  TSSAParser = class
  private
    ScriptInfoParser : TScriptInfoParser;
    StyleParser : TStyleParser;
    DialogueParser : TDialogueParser;
    FooterParser : TMemorizerParser;
    IsUTF8 : Boolean;
    IsASS : Boolean;

    function GetParserForSection(line : WideString) : TAbstractLineParser;
  public
    constructor Create;
    destructor Destroy; override;

    function Load(Stream : TStream) : Integer; overload;
    function Load(Source : TTntStrings) : Integer; overload;
    function Load(Filename : WideString) : Integer; overload; 

    function GetStylesCount : Integer;
    function GetDialoguesCount : Integer;
    function GetStyleValueAsString(lineIndex : integer; valueKey : WideString) : WideString;
    function GetStyleValueAsInteger(lineIndex : integer; valueKey : WideString) : Integer;
    function GetStyleValueAsBoolean(lineIndex : integer; valueKey : WideString) : Boolean;
    function GetStyleValueAsDouble(lineIndex : integer; valueKey : WideString) : Double;
    function GetDialogueValueAsString(lineIndex : integer; valueKey : WideString) : WideString;
    function GetDialogueValueAsInteger(lineIndex : integer; valueKey : WideString) : Integer;
    function GetHeaderLines : WideString;
    function GetFooterLines : WideString;
    function GetIsUTF8 : Boolean;
    function GetIsASS : Boolean;
    function StyleKeyExists(key : WideString) : Boolean;
    function DialogueKeyExists(key : WideString) : Boolean;
  end;

implementation

uses SysUtils, StrUtils, MiscToolsUnit, Windows, TntWideStrings;

// =============================================================================

procedure TMemorizerParser.ParseLine(line: WideString);
begin
  if Length(memory) > 0 then
    memory := memory + #13#10 + line
  else
    memory := line;
end;

// =============================================================================

constructor TFormatedParser.Create;
begin
  keywordLst := TTntStringList.Create;
  keywordLst.CaseSensitive := False;
  parsedLines := TList.Create;
end;

destructor TFormatedParser.Destroy;
var tmpSL : TTntStringList;
    i : Integer;
begin
  keywordLst.Free;
  for i:=0 to parsedLines.Count-1 do
  begin
    tmpSL := TTntStringList(parsedLines.Items[i]);
    tmpSL.Clear;
    tmpSL.Free;
  end;
  parsedLines.Clear;
  parsedLines.Free;
end;

function TFormatedParser.GetCount : Integer;
begin
  Result := parsedLines.Count;
end;

function TFormatedParser.ParseFormatLine(line: WideString; start : WideString) : Integer;
var p : integer;
begin
  Delete(line, 1, Length(start));
  while True do
  begin
    p := Pos(',', line);
    if p = 0 then
    begin
      keywordLst.Add(Trim(line));
      Break;
    end;
    keywordLst.Add(Trim(Copy(line, 1, p - 1)));
    Delete(line,1,p);
  end;
  Result := keywordLst.Count;
end;

function TFormatedParser.ParseFormatedLine(line: WideString; start : WideString) : Integer;
var parsedLine : TTntStringList;
    p : integer;
    tmpLine : WideString;
begin
  parsedLine := TTntStringList.Create;
  Delete(line, 1, Length(start));
  while True do
  begin
    if (parsedLine.Count = keywordLst.Count - 1) then
    begin
      // Last element
      parsedLine.Add(Trim(line));
      Break;
    end;

    p := Pos(',', line);
    if p = 0 then
    begin
      parsedLine.Add(Trim(line));
      Break;
    end;

    tmpLine := Copy(line, 1, p - 1);
    tmpLine := Trim(tmpLine);
    parsedLine.Add(tmpLine);
    Delete(line, 1, p);
  end;
  parsedLines.Add(parsedLine);
  if (parsedLine.Count < keywordLst.Count) then
    Result := -1
  else
    Result := parsedLine.Count;
end;

function TFormatedParser.GetValueAsBoolean(lineIndex : integer; valueKey : WideString) : Boolean;
var intResult : Integer;
begin
  intResult := GetValueAsInteger(lineIndex, valueKey);
  if intResult = 0 then
    Result := False
  else
    Result := True;
end;

function TFormatedParser.GetValueAsInteger(lineIndex : integer; valueKey : WideString) : Integer;
var valueAsString : WideString;
begin
  valueAsString := GetValueAsString(lineIndex, valueKey);
  Result := StrToIntDef(valueAsString,0);
end;

function TFormatedParser.GetValueAsDouble(lineIndex : integer; valueKey : WideString) : Double;
var valueAsString : WideString;
    ssaFloatFormatSettings : TFormatSettings;
begin
  GetLocaleFormatSettings(0,ssaFloatFormatSettings);
  ssaFloatFormatSettings.DecimalSeparator := '.';
  valueAsString := GetValueAsString(lineIndex, valueKey);
  Result := StrToFloatDef(valueAsString, 0.0, ssaFloatFormatSettings);
end;

function TFormatedParser.GetValueAsString(lineIndex : integer; valueKey : WideString) : WideString;
var keyIndex : Integer;
    lineValues : TTntStringList;
begin
  Result := '';
  keyIndex := keywordLst.IndexOf(valueKey);
  if (keyIndex <> -1) then
  begin
    if (lineIndex < parsedLines.Count) then
    begin
      lineValues := TTntStringList(parsedLines[lineIndex]);
      if (keyIndex < lineValues.Count) then
      begin
        Result := lineValues[keyIndex];
      end;
    end;
  end;
end;

function TFormatedParser.KeyExists(key : WideString) : Boolean;
begin
  Result := keywordLst.IndexOf(key) <> -1;
end;

// =============================================================================

function StartsText(StartWidth, Text : WideString) : Boolean;
var ls : WideString;
begin
  ls := LeftStr(Text, Length(StartWidth));
  Result := (WideCompareText(ls, StartWidth) = 0);
end;

function EndsText(EndWidth, Text : WideString) : Boolean;
var rs : WideString;
begin
  rs := RightStr(Text,Length(EndWidth));
  Result := (WideCompareText(rs, EndWidth) = 0);
end;

function TStyleParser.IsSection(line : WideString) : Boolean;
begin
  Result := StartsText('[v4', Trim(line)) and EndsText('Styles]', Trim(line));
  if Result then
  begin
    FIsASS := StartsText('[v4+', Trim(line));
  end;
end;

procedure TStyleParser.ParseLine(line: WideString);
begin
  if StartsText('Format:', line) then
  begin
    ParseFormatLine(line, 'Format:');
  end
  else if StartsText('Style:', line) then
  begin
    ParseFormatedLine(line, 'Style:');
  end;
end;

// =============================================================================

function TDialogueParser.IsSection(line : WideString) : Boolean;
begin
  Result := StartsText('[Events]', Trim(line));
end;

procedure TDialogueParser.ParseLine(line: WideString);
begin
  if StartsText('Format:', line) then
  begin
    ParseFormatLine(line, 'Format:');
  end
  else if StartsText('Dialogue:', line) then
  begin
    ParseFormatedLine(line, 'Dialogue:');
  end;
end;

// =============================================================================

constructor TScriptInfoParser.Create;
begin
  KeyValueLst := TTntStringList.Create;
  KeyValueLst.CaseSensitive := False;
  KeyValueLst.NameValueSeparator := ':';
end;

destructor TScriptInfoParser.Destroy;
begin
  KeyValueLst.Free;
end;

function TScriptInfoParser.IsSection(line : WideString) : Boolean;
begin
  Result := StartsText('[Script Info]', Trim(line));
end;

procedure TScriptInfoParser.ParseLine(line: WideString);
var PosSemiColon : Integer;
begin
  if not StartsText(';', line) then
  begin
    PosSemiColon := Pos(':', line);
    if PosSemiColon > 0 then
    begin
      KeyValueLst.Add(line);
    end;
  end;
end;

function TScriptInfoParser.GetCount : Integer;
begin
  Result := KeyValueLst.Count;
end;

function TScriptInfoParser.KeyExists(Key : WideString) : Boolean;
begin
  Result := Length(KeyValueLst.Values[Key]) > 0;
end;

function TScriptInfoParser.GetValueAsString(Index : integer) : WideString;
begin
  Result := KeyValueLst.ValueFromIndex[Index];
end;

function TScriptInfoParser.GetValueAsString(Key : WideString) : WideString;
begin
  Result := KeyValueLst.Values[Key];
end;

function TScriptInfoParser.GetAsString : WideString;
begin
  Result := KeyValueLst.Text;
end;

// =============================================================================

constructor TSSAParser.Create;
begin
  ScriptInfoParser := TScriptInfoParser.Create;
  StyleParser := TStyleParser.Create;
  DialogueParser := TDialogueParser.Create;
  FooterParser := TMemorizerParser.Create;
end;

destructor TSSAParser.Destroy;
begin
  ScriptInfoParser.Free;
  StyleParser.Free;
  DialogueParser.Free;
  FooterParser.Free;
  inherited Destroy;
end;

function TSSAParser.GetParserForSection(line : WideString) : TAbstractLineParser;
begin
  if ScriptInfoParser.IsSection(line) then
    Result := ScriptInfoParser
  else if StyleParser.IsSection(line) then
    Result := StyleParser
  else if DialogueParser.IsSection(line) then
    Result := DialogueParser
  else
    Result := FooterParser;
end;

function TSSAParser.Load(Source: TTntStrings) : Integer;
var line : WideString;
    sectionParser : TAbstractLineParser;
    ScriptType : WideString;
    i : Integer;
begin
  IsUTF8 := (Source.LastFileCharSet <> csAnsi);

  // Read file
  sectionParser := FooterParser;

  for i := 0 to Source.Count-1 do
  begin
    line := Source.Strings[i];
    if StartsText('[', line) and EndsText(']', Trim(line))  then
    begin
      // New section
      sectionParser := GetParserForSection(line);
    end;
    sectionParser.ParseLine(line);
  end;

  ScriptType := Trim(ScriptInfoParser.GetValueAsString('ScriptType'));
  IsASS := StartsText('v4.00+', ScriptType) or StyleParser.IsASS;

  Result := GetDialoguesCount; 
end;

function TSSAParser.Load(Filename: WideString) : Integer;
var Source : TTntStringList;
begin
  Source := MyTTntStringList.Create;
  Source.LoadFromFile(Filename);
  Result := Load(Source);
  Source.Free;
end;

function TSSAParser.Load(Stream : TStream) : Integer;
var Source : TTntStringList;
begin
  Source := MyTTntStringList.Create;
  Source.LoadFromStream(Stream);
  Result := Load(Source);
  Source.Free;
end;

function TSSAParser.GetStylesCount : Integer;
begin
  Result := StyleParser.GetCount;
end;

function TSSAParser.GetDialoguesCount : Integer;
begin
  Result := DialogueParser.GetCount;
end;

function TSSAParser.GetStyleValueAsString(lineIndex : integer; valueKey : WideString) : WideString;
begin
  Result := StyleParser.GetValueAsString(lineIndex, valueKey);
end;

function TSSAParser.GetStyleValueAsDouble(lineIndex : integer; valueKey : WideString) : Double;
begin
  Result := StyleParser.GetValueAsDouble(lineIndex, valueKey);
end;

function TSSAParser.GetStyleValueAsInteger(lineIndex : integer; valueKey : WideString) : Integer;
begin
  Result := StyleParser.GetValueAsInteger(lineIndex, valueKey);
end;

function TSSAParser.GetStyleValueAsBoolean(lineIndex : integer; valueKey : WideString) : Boolean;
begin
  Result := StyleParser.GetValueAsBoolean(lineIndex, valueKey);
end;

function TSSAParser.GetDialogueValueAsString(lineIndex : integer; valueKey : WideString) : WideString;
begin
  Result := DialogueParser.GetValueAsString(lineIndex, valueKey);
end;

function TSSAParser.GetDialogueValueAsInteger(lineIndex : integer; valueKey : WideString) : Integer;
begin
  Result := DialogueParser.GetValueAsInteger(lineIndex, valueKey);
end;

function TSSAParser.GetHeaderLines : WideString;
begin
  Result := ScriptInfoParser.GetAsString;
end;

function TSSAParser.GetFooterLines : WideString;
begin
  Result := FooterParser.memory;
end;

function TSSAParser.GetIsUTF8 : Boolean;
begin
  Result := IsUTF8;
end;

function TSSAParser.GetIsASS : Boolean;
begin
  Result := IsASS;
end;

function TSSAParser.StyleKeyExists(key : WideString) : Boolean;
begin
  Result := StyleParser.KeyExists(key);
end;

function TSSAParser.DialogueKeyExists(key : WideString) : Boolean;
begin
  Result := DialogueParser.KeyExists(key);
end;

end.
// =============================================================================
