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

unit MiscToolsUnit;

interface

uses Classes, Graphics, Types, Richedit, TntComCtrls, TntClasses, StdCtrls, TntStdCtrls,IdIcmpClient;

type
  TFileVersion = class
  private
    FFileParsed : Boolean;
    FFilename : string;
    FShortVersionString : string;
    FVersionString : string;
    function ParseFile : Boolean;
    function GetVersionString : string;
    function GetShortVersionString : string;
  public
    constructor Create(Filename : string);
  published
    property VersionString : string read GetVersionString;
    property ShortVersionString : string read GetShortVersionString;
  end;

  procedure Constrain(var Value : Integer; MinValue, MaxValue : Integer);
  function TimeMsToString(const TimeMS: Integer; const DecimalSeparator : string = '.') : string;
  function TimeMsToSSAString(const TimeMS: Integer) : string;
  function TimeMsToCUE(const TimeMS: Integer) : string;

  function TimeStringToMS_SSA(Time : string) : Integer;
  function TimeStringToMs(const Time : string) : Integer;

  function IsTimeStampsLine(const Line : string; var Start, Stop : Integer) : Boolean;
  function StringConvertShrinkSpace(const s : WideString) : WideString;
  function StringConvertPipeToSpace(const s : WideString) : WideString;
  function StringConvertPipeToCRLF(const s : WideString) : WideString;
  function StringConvertPipeToBR(const s : WideString) : WideString;
  function StringConvertCRLFToPipe(const s : WideString) : WideString;
  function StringConvertCRLFToBR(const s : WideString) : WideString;
  function GetFileVersionString(Filename : string) : string;

  function PosStream(const SubString : string; Stream : TStream; StartPos : Integer = -1) : Integer;
  function PosStreamEx(const SubString : string; Stream : TStream; StartPos, StopPos : Integer) : Integer;

  function URLDecode(ASrc: string): string;

  function IsRegistryClassesRootKeyWritable : Boolean;
  procedure ShellRegisterExtension(Extension, AppName, DocType, Executable : string; IconIndex : Integer = -1);
  function ShellIsExtensionRegistered(Extension, AppName, DocType, Executable : string) : Boolean;
  procedure ShellUnRegisterExtension(Extension, AppName, DocType, Executable : string);

  function IsRegistryCurrentUserRootKeyWritable : Boolean;
  procedure ShellRegisterCurrentUserExtension(Extension, AppName, DocType, Executable : string; IconIndex : Integer = -1);
  function ShellIsCurrentUserExtensionRegistered(Extension, AppName, DocType, Executable : string) : Boolean;
  procedure ShellUnRegisterCurrentUserExtension(Extension, AppName, DocType, Executable : string);

  function ReadLineStream(Stream : TStream; var s : string) : Boolean;
  function RPos(const Substr, S: WideString): Integer;

  function Font2String(Font : TFont) : string;
  procedure String2Font(s : string; Font : TFont);
  procedure String2FontNoSetColor(s : string; Font : TFont);
  procedure String2FontProperties(s : string; out Name : TFontName; out Size : Integer; out Style : TFontStyles; out Charset : TFontCharset; out Color : TColor);

  procedure TagSplit(const Text : WideString; var WordArray : TWideStringDynArray);
  function StripTags(const Text : WideString) : WideString;
  function StripText(const Text : WideString) : WideString;
  function StripTagsKeepNL(const Text : WideString) : WideString;

  function WideIsAbsolutePath(const Path : WideString) : Boolean;
  function WideMakeRelativePath(const BaseName, DestName : WideString) : WideString;
  function WideResolveRelativePath(const BaseName, DestName : WideString) : WideString;

  function ABGRColor2AssColorString(Input: Cardinal): WideString;
  function AssColorString2ABGR(Color: string): Cardinal;

  function GetUserApplicationDataFolder : WideString;
  function GetUserDocumentsFolder : WideString;
  function GetTemporaryFolder : WideString;

  function ExplodeW(const cSeparator: WideString; const vString: WideString; var WordArray : TWideStringDynArray): Integer;
  function Explode(const cSeparator: String; const vString: String; var WordArray : TStringDynArray): Integer;
  function Implode(const cSeparator: String; const cArray: TStringDynArray): String;

  function WideStringFind(const Offset : Integer; const S, Pattern: WideString;
    IgnoreCase: Boolean = False; WholeWord: Boolean = False): Integer;
  function WideStringCount(const S, Pattern: WideString;
    IgnoreCase: Boolean = False; WholeWord: Boolean = False): Integer;

  function ConvertSSAToSRT(Src : WideString) : WideString;
  function ConvertSRTToSSA(Src : WideString) : WideString;
  function ConvertNull(Src : WideString) : WideString;

  function SwapColor(Color : Integer) : Integer;

  function BinarySearch(IntArray : TIntegerDynArray; Value : Integer;
    Backward : Boolean = False) : Integer;

  procedure SaveToStreamWS(Stream : TStream; Value : WideString);
  procedure LoadFromStreamWS(Stream : TStream; out Value : WideString);
  procedure SaveToStreamInt(Stream : TStream; Value : Integer);
  procedure LoadFromStreamInt(Stream : TStream; out Value : Integer);

  // Some faster richedit function
  procedure SetRESelection(RichEdit : TTntRichEdit; Start, Len : Integer);
  procedure SetRESelectionColor(ARichEdit : TTntRichEdit; AColor : TColor);
  procedure SetReUnderline(ARichEdit: TTntRichEdit; Underline : Boolean);

  function WC2MB(S : WideString) : string;
  function MB2WC(S : string) : WideString;

  procedure SetCheckedState(const checkBox : TCheckBox; const check : Boolean); overload;
  procedure SetCheckedState(const checkBox : TTntCheckBox; const check : Boolean); overload;

  function LeftPad(S: string; Ch: Char; Len: Integer): string;
  function RightPad(S: string; Ch: Char; Len: Integer): string;

  procedure SetOpenDialogPath(OpenDlg: TObject);

  function GetLayoutShortName: String;

type
  MyTTntStringList = class(TTntStringList)
  public
    procedure LoadFromStream_BOM(Stream: TStream; WithBOM: Boolean); override;
  end;

implementation

uses SysUtils, Windows, Registry, ShlObj, StrUtils, TntSysUtils, TntWindows, VFW, TntSystem, Main, TntDialogs, GlobalUnit;

// -----------------------------------------------------------------------------

procedure Constrain(var Value : Integer; MinValue, MaxValue : Integer);
begin
  if (MinValue <= MaxValue) then
  begin
    if (Value < MinValue) then
      Value := MinValue
    else if (Value > MaxValue) then
      Value := MaxValue;
  end;
end;

// -----------------------------------------------------------------------------

function TimeMsToString(const TimeMS: Integer; const DecimalSeparator : string) : string;
var
  hh, min, sec, ms: Integer;
begin
  ms := TimeMs div 1000;
  hh := ms div 3600;
  min := ms mod 3600 div 60;
  sec := ms mod 3600 mod 60;
  ms := TimeMS - (hh * 3600 * 1000) - (min * 60 * 1000) - (sec * 1000);
  Result := Format('%2.2d:%2.2d:%2.2d%s%3.3d', [hh, min, sec, DecimalSeparator, ms])
end;

// -----------------------------------------------------------------------------

function TimeMsToSSAString(const TimeMS: Integer) : string;
var
  hh, min, sec, ms, cs: Integer;
begin
  ms := TimeMs div 1000;
  hh := ms div 3600;
  min := ms mod 3600 div 60;
  sec := ms mod 3600 mod 60;
  ms := TimeMS - (hh * 3600 * 1000) - (min * 60 * 1000) - (sec * 1000);
  cs := ms div 10;
  Result := Format('%d:%2.2d:%2.2d%s%2.2d', [hh, min, sec, '.', cs])
end;

//------------------------------------------------------------------------------

function TimeStringToMs(const Time : string) : Integer;
  var h, m, s, ms: Integer;
begin
  Result := -1;
  // 00:50:06,969 or
  // 00:50:06.969
  // 123456789012
  if Length(Time) < 8 then
    Exit;
  if (Time[3] <> ':') or (Time[6] <> ':') then
    Exit;
  h := StrToIntDef(Copy(Time, 1,2),-1);
  if (h = -1) then
    Exit;
  m := StrToIntDef(Copy(Time, 4,2),-1);
  if (m = -1) then
    Exit;
  s := StrToIntDef(Copy(Time, 7,2),-1);
  if (s = -1) then
    Exit;
  ms := StrToIntDef(Copy(Time, 10,3),-1);
  if (ms = -1) then
    Exit;
  Result := h * 3600000 + m * 60000 + s * 1000 + ms;
end;

//------------------------------------------------------------------------------

function TimeStringToMS_SSA(Time : string) : Integer;
  var h, m, s, ms: Integer;
begin
  Result := -1;
  // 00:50:06.96
  // 123456789012
  if Length(Time) < 8 then
    Exit;
  if Time[2] = ':' then
    Time := '0' + Time;
  if (Time[3] <> ':') or (Time[6] <> ':') then
    Exit;
  h := StrToIntDef(Copy(Time, 1,2),-1);
  if (h = -1) then
    Exit;
  m := StrToIntDef(Copy(Time, 4,2),-1);
  if (m = -1) then
    Exit;
  s := StrToIntDef(Copy(Time, 7,2),-1);
  if (s = -1) then
    Exit;
  ms := StrToIntDef(Copy(Time, 10,2),-1);
  if (ms = -1) then
    Exit;
  Result := h * 3600000 + m * 60000 + s * 1000 + 10 * ms;
end;

// -----------------------------------------------------------------------------

function TimeMsToCUE(const TimeMS: Integer) : string;
var
  hh, min, sec, ms, ff: Integer;
begin
  ms := TimeMs div 1000;
  hh := ms div 3600;
  min := ms mod 3600 div 60;
  sec := ms mod 3600 mod 60;
  ms := TimeMS - (hh * 3600 * 1000) - (min * 60 * 1000) - (sec * 1000);
  ff := Round(ms / (1000 / 75));
  Result := Format('%2.2d:%2.2d:%2.2d', [min, sec, ff])
end;

//------------------------------------------------------------------------------

function IsTimeStampsLine(const Line : string; var Start, Stop : Integer) : Boolean;
var i : integer;
    SS : string;
begin
  Result := False;
  i := Pos('-->',Line);
  if i > 0 then
  begin
    SS := Trim(Copy(Line,1,i-1));
    Start := TimeStringToMs(SS);
    SS := Trim(Copy(Line,i+3,Length(Line)));
    Stop := TimeStringToMs(SS);
    Result := (Start <> -1) and (Stop <> -1);
  end
end;

//------------------------------------------------------------------------------

function StringConvertShrinkSpace(const s : WideString) : WideString;
var i : integer;
  LastSpace :  Boolean;
  AddSpace :  Boolean;
begin
  Result := '';
  LastSpace := True;
  AddSpace := False;
  for i:=1 to Length(s) do
  begin
    if (s[i] in [WideChar(' '), WideChar(#13), WideChar(#10), WideChar('|')]) then
    begin
      if not LastSpace then
        AddSpace := True;
      LastSpace := True;
    end
    else
    begin
      if AddSpace then
        Result := Result + ' ';
      AddSpace := False;
      Result := Result + s[i];
      LastSpace := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

function StringConvertPipeToSpace(const s : WideString) : WideString;
var i : integer;
begin
  Result := '';
  for i:=1 to Length(s) do
  begin
    if (s[i] = '|') then
      Result := Result + ' '
    else
      Result := Result + s[i]
  end;
end;

//------------------------------------------------------------------------------

function StringConvertPipeToCRLF(const s : WideString) : WideString;
var i : integer;
begin
  Result := '';
  for i:=1 to Length(s) do
  begin
    if (s[i] = '|') then
      Result := Result + #13#10
    else
      Result := Result + s[i]
  end;
end;

//------------------------------------------------------------------------------

function StringConvertCRLFToPipe(const s : WideString) : WideString;
var i : integer;
begin
  Result := '';
  i := 1;
  while (i <= Length(s)) do
  begin
    if (s[i] = #13) then
    begin
      Result := Result + '|';
      Inc(i);
    end
    else
      Result := Result + s[i];
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

function StringConvertPipeToBR(const s : WideString) : WideString;
var i : integer;
begin
  Result := '';
  for i:=1 to Length(s) do
  begin
    if (s[i] = '|') then
      Result := Result + '<br>'
    else
      Result := Result + s[i]
  end;
end;

//------------------------------------------------------------------------------

function StringConvertCRLFToBR(const s : WideString) : WideString;
var i : integer;
begin
  Result := '';
  i := 1;
  while (i <= Length(s)) do
  begin
    if (s[i] = #13) then
    begin
      Result := Result + '<br>';
      Inc(i);
    end
    else
      Result := Result + s[i];
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

function GetFileVersionComponents(Filename : string; var c1, c2, c3, c4 : Integer) : Boolean;
var VersionInfoSize : Cardinal;
    dwHandle : Cardinal;
    VersionInfo : Pointer;
    VersionValue : PVSFixedFileInfo;
    VersionValueSize : Cardinal;
begin
  Result := False;
  c1 := 0; c2 := 0; c3 := 0; c4 := 0;
  VersionInfoSize := GetFileVersionInfoSize(PAnsiChar(Filename), dwHandle);
  if (VersionInfoSize > 0) then
  begin
    GetMem(VersionInfo, VersionInfoSize);
    if GetFileVersionInfo(PAnsiChar(Filename), dwHandle, VersionInfoSize, VersionInfo) then
    begin
      if VerQueryValue(VersionInfo, '\', Pointer(VersionValue), VersionValueSize) then
      begin
        Result := True;      
        c1 := VersionValue.dwFileVersionMS shr 16;
        c2 := VersionValue.dwFileVersionMS and $FFFF;
        c3 := VersionValue.dwFileVersionLS shr 16;
        c4 := VersionValue.dwFileVersionLS and $FFFF;
      end;
    end;
    FreeMem(VersionInfo, VersionInfoSize);
  end;
end;

//------------------------------------------------------------------------------

function GetFileVersionString(Filename : string) : string;
var c1, c2, c3, c4 : Integer;
begin
  if GetFileVersionComponents(Filename, c1, c2, c3, c4) then
    Result := Format('%d.%d.%d.%d', [c1, c2, c3, c4])
  else
    Result := '';
end;

//==============================================================================
// Class that keep the file version
//==============================================================================

constructor TFileVersion.Create(Filename : string);
begin
  FFileParsed := False;
  FFilename := Filename;
end;

//------------------------------------------------------------------------------

function TFileVersion.ParseFile : Boolean;
var c1, c2, c3, c4 : Integer;
begin
  Result := GetFileVersionComponents(FFilename, c1, c2, c3, c4);
  FFileParsed := True;
  if c4>9 then
    FVersionString := Format('%d.%d.%d.%d', [c1, c2, c3, c4])
  else FVersionString := Format('%d.%d.%d.0%d', [c1, c2, c3, c4]);
  if c3 <> 0 then
    FShortVersionString := Format('%d.%d.%d', [c1, c2, c3])
  else
    FShortVersionString := Format('%d.%d', [c1, c2]);
end;

//------------------------------------------------------------------------------

function TFileVersion.GetVersionString : string;
begin
  if not FFileParsed then
    ParseFile;
  Result := FVersionString;
end;

//------------------------------------------------------------------------------

function TFileVersion.GetShortVersionString : string;
begin
  if not FFileParsed then
    ParseFile;
  Result := FShortVersionString;
end;

//==============================================================================
// Equivalent of Pos but on TStream
//==============================================================================

function PosStream(const SubString : string; Stream : TStream; StartPos : Integer = -1) : Integer;
var i : Integer;
    c : Char;
    OriginalPos : Int64;
begin
  Result := -1;
  OriginalPos := Stream.Position;
  if StartPos <> -1 then
    Stream.Seek(StartPos, soFromBeginning);
  i := 1;
  while (Stream.Read(c,1) = 1) do
  begin
    if SubString[i] = c then
    begin
      if (i = Length(SubString)) then
      begin
        Result := Stream.Position - i;
        Break;
      end;
      Inc(i);
    end
    else
      i := 1
  end;
  // Restore position
  Stream.Position := OriginalPos;
end;

// -----------------------------------------------------------------------------

function PosStreamEx(const SubString : string; Stream : TStream; StartPos, StopPos : Integer) : Integer;
var i : Integer;
    c : Char;
    OriginalPos : Int64;
    WalkLen : Integer;
begin
  Result := -1;
  OriginalPos := Stream.Position;
  Stream.Seek(StartPos, soFromBeginning);
  WalkLen := StopPos - StartPos;
  i := 1;
  while (Stream.Read(c,1) = 1) and (WalkLen > 0) do
  begin
    if SubString[i] = c then
    begin
      if (i = Length(SubString)) then
      begin
        Result := Stream.Position - i;
        Break;
      end;
      Inc(i);
    end
    else
      i := 1;
    Dec(WalkLen);
  end;
  // Restore position
  Stream.Position := OriginalPos;
end;

//==============================================================================

// From Indy components a bit modified
function URLDecode(ASrc: string): string;
var
  i: integer;
  ESC: string[2];
  CharCode: integer;
begin
  Result := '';
  ASrc := StringReplace(ASrc, '+', ' ', [rfReplaceAll]);
  i := 1;
  while i <= Length(ASrc) do
  begin
    if ASrc[i] <> '%' then
    begin
      Result := Result + ASrc[i]
    end
    else
    begin
      Inc(i); // skip the % char
      ESC := Copy(ASrc, i, 2); // Copy the escape code
      Inc(i, 1); // Then skip it.
      CharCode := StrToIntDef('$' + ESC, -1);
      if (CharCode > 0) and (CharCode < 256) then begin
        Result := Result + Char(CharCode);
      end;
    end;
    Inc(i);
  end;
end;

// =============================================================================
// Shell extension registration
// =============================================================================

function IsRegistryClassesRootKeyWritable : Boolean;
var hResultKey : HKEY;
    OpenResult :Integer;
begin
  OpenResult := RegOpenKeyEx(HKEY_CLASSES_ROOT, nil, 0, KEY_WRITE, hResultKey);
  if (OpenResult = ERROR_SUCCESS) then
  begin
    Result := True;
    RegCloseKey(hResultKey);
  end
  else
    Result := False;
end;

procedure ShellRegisterExtension(Extension, AppName, DocType, Executable : string; IconIndex : Integer);
var
  Reg: TRegistry;
  s : string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    // Add extension
    Reg.OpenKey('.' + Extension, True);
    try
      s := Reg.ReadString('');
      if (s <> '') and (s <> AppName + '.' + DocType) then
      begin
        // Save key
        Reg.WriteString(AppName + '.sav', s);
      end;
      Reg.WriteString('', AppName + '.' + DocType);
    finally
      Reg.CloseKey;
    end;
    Reg.CreateKey(AppName + '.' + DocType);
    // Add command
    Reg.OpenKey(AppName + '.' + DocType + '\shell\open\command', True);
    try
      Reg.WriteString('', Executable + ' "%1"');
    finally
      Reg.CloseKey;
    end;
    if (IconIndex <> -1) then
    begin
      // Add icon
      Reg.OpenKey(AppName + '.' + DocType + '\DefaultIcon\', True);
      try
        Reg.WriteString('', Executable + ',' + IntToStr(IconIndex));
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

//------------------------------------------------------------------------------

function ShellIsExtensionRegistered(Extension, AppName, DocType, Executable : string) : Boolean;
var
  Reg: TRegistry;
  s : string;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKey('.' + Extension, False) = True then
    begin
      s := Reg.ReadString('');
      Reg.CloseKey;
      if (AppName + '.' + DocType) = s then
      begin
        if Reg.OpenKey(AppName + '.' + DocType + '\shell\open\command', False) = True then
        begin
          s := Reg.ReadString('');
          Reg.CloseKey;
          if (Executable + ' "%1"') = s then
            Result := True;
        end;
      end;
    end
  finally
    Reg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure ShellUnRegisterExtension(Extension, AppName, DocType, Executable : string);
var
  Reg: TRegistry;
  s : string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKey('.' + Extension, False) = True then
    begin
      s := Reg.ReadString(AppName + '.sav');
      if (s <> '') then
      begin
        // restore
        Reg.WriteString('', s);
        // delete '.sav'
        Reg.DeleteValue(AppName + '.sav');
        Reg.CloseKey;
      end
      else
      begin
        // nothing to restore delete whole key
        Reg.CloseKey;
        Reg.DeleteKey('.' + Extension);
      end;
    end;
    // Delete our command key
    Reg.DeleteKey(AppName + '.' + DocType);
  finally
    Reg.Free;
  end;
end;

// =============================================================================
// Shell extension registration HKCU
// =============================================================================

function IsRegistryCurrentUserRootKeyWritable : Boolean;
var hResultKey : HKEY;
    OpenResult :Integer;
begin
  OpenResult := RegOpenKeyEx(HKEY_CURRENT_USER, nil, 0, KEY_WRITE, hResultKey);
  if (OpenResult = ERROR_SUCCESS) then
  begin
    Result := True;
    RegCloseKey(hResultKey);
  end
  else
    Result := False;
end;

//------------------------------------------------------------------------------

procedure ShellRegisterCurrentUserExtension(Extension, AppName, DocType, Executable : string; IconIndex : Integer);
const
  HKCUClassesRoot = '\Software\Classes\';
var
  Reg: TRegistry;
  s : string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    // Add extension
    Reg.OpenKey(HKCUClassesRoot + '.' + Extension, True);
    try
      s := Reg.ReadString('');
      if (s <> '') and (s <> AppName + '.' + DocType) then
      begin
        // Save key
        Reg.WriteString(AppName + '.sav', s);
      end;
      Reg.WriteString('', AppName + '.' + DocType);
    finally
      Reg.CloseKey;
    end;
    Reg.CreateKey(HKCUClassesRoot + AppName + '.' + DocType);
    // Add command
    Reg.OpenKey(HKCUClassesRoot + AppName + '.' + DocType + '\shell\open\command', True);
    try
      Reg.WriteString('', Executable + ' "%1"');
    finally
      Reg.CloseKey;
    end;
    if (IconIndex <> -1) then
    begin
      // Add icon
      Reg.OpenKey(HKCUClassesRoot + AppName + '.' + DocType + '\DefaultIcon\', True);
      try
        Reg.WriteString('', Executable + ',' + IntToStr(IconIndex));
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

//------------------------------------------------------------------------------

function ShellIsCurrentUserExtensionRegistered(Extension, AppName, DocType, Executable : string) : Boolean;
const
  HKCUClassesRoot = '\Software\Classes\';
var
  Reg: TRegistry;
  s : string;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(HKCUClassesRoot + '.' + Extension, False) = True then
    begin
      s := Reg.ReadString('');
      Reg.CloseKey;
      if (AppName + '.' + DocType) = s then
      begin
        if Reg.OpenKey(HKCUClassesRoot + AppName + '.' + DocType + '\shell\open\command', False) = True then
        begin
          s := Reg.ReadString('');
          Reg.CloseKey;
          if (Executable + ' "%1"') = s then
            Result := True;
        end;
      end;
    end
  finally
    Reg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure ShellUnRegisterCurrentUserExtension(Extension, AppName, DocType, Executable : string);
const
  HKCUClassesRoot = '\Software\Classes\';
var
  Reg: TRegistry;
  s : string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(HKCUClassesRoot + '.' + Extension, False) = True then
    begin
      s := Reg.ReadString(AppName + '.sav');
      if (s <> '') then
      begin
        // restore
        Reg.WriteString('', s);
        // delete '.sav'
        Reg.DeleteValue(AppName + '.sav');
        Reg.CloseKey;
      end
      else
      begin
        // nothing to restore delete whole key
        Reg.CloseKey;
        Reg.DeleteKey(HKCUClassesRoot + '.' + Extension);
      end;
    end;
    // Delete our command key
    Reg.DeleteKey(HKCUClassesRoot + AppName + '.' + DocType);
  finally
    Reg.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

// -----------------------------------------------------------------------------

function ReadLineStream(Stream : TStream; var s : string) : Boolean;
var c : Char;
begin
  s := '';
  while Stream.Read(c,1) = 1 do
  begin
    if (c = #10) then
      Break
    else if (c = #13) then
      Continue;
    s := s + c;
  end;
  Result := (Stream.Position = Stream.Size) and (s = '');
end;

// -----------------------------------------------------------------------------

function RPos(const Substr, S: WideString): Integer;
var i,j : integer;
begin
  i := Length(S);
  j := Length(Substr);
  while (i > 0) do
  begin
    if S[i] = Substr[j] then
    begin
      Dec(j);
      if j = 0 then
      begin
        Result := i;
        Exit;
      end;
    end
    else
      j := Length(Substr);
    Dec(i);
  end;
  Result := 0;
end;

// -----------------------------------------------------------------------------

function Font2String(Font : TFont) : string;
begin
  Result := Font.Name + ',' +
    IntToStr(Font.Size) + ',' +
    IntToStr(Byte(Font.Style)) + ',' +
    IntToStr(Byte(Font.Charset)) + ',' +
    ColorToString(Font.Color);
end;

// -----------------------------------------------------------------------------

procedure String2Font(s : string; Font : TFont);
var ea : TStringDynArray;
begin
  Explode(',', s, ea);
  if (Length(ea) = 5) and Assigned(Font) then
  begin
    Font.Name := Trim(ea[0]);
    Font.Size := StrToIntDef(Trim(ea[1]), 12);
    Font.Style := TFontStyles(Byte(StrToIntDef(Trim(ea[2]), 0)));
    Font.Charset := StrToIntDef(Trim(ea[3]), 0);
    Font.Color := StringToColor(Trim(ea[4]));
  end;
end;

// -----------------------------------------------------------------------------

procedure String2FontNoSetColor(s : string; Font : TFont);
var ea : TStringDynArray;
begin
  Explode(',', s, ea);
  if (Length(ea) = 5) and Assigned(Font) then
  begin
    Font.Name := Trim(ea[0]);
    Font.Size := StrToIntDef(Trim(ea[1]), 12);
    Font.Style := TFontStyles(Byte(StrToIntDef(Trim(ea[2]), 0)));
    Font.Charset := StrToIntDef(Trim(ea[3]), 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure String2FontProperties(s : string; out Name : TFontName; out Size : Integer; out Style : TFontStyles; out Charset : TFontCharset; out Color : TColor);
var ea : TStringDynArray;
begin
  Explode(',', s, ea);
  if (Length(ea) = 5) then
  begin
    Name := Trim(ea[0]);
    Size := StrToIntDef(Trim(ea[1]), 12);
    Style := TFontStyles(Byte(StrToIntDef(Trim(ea[2]), 0)));
    Charset := StrToIntDef(Trim(ea[3]), 0);
    Color := StringToColor(Trim(ea[4]));
  end;
end;


// =============================================================================
// Implode/Explode from http://www.swissdelphicenter.ch/en/showcode.php?id=1326
// =============================================================================

function Implode(const cSeparator: String; const cArray: TStringDynArray): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(cArray) -1 do begin
    Result := Result + cSeparator + cArray[i];
  end;
  System.Delete(Result, 1, Length(cSeparator));
end;

// -----------------------------------------------------------------------------

function ExplodeW(const cSeparator: WideString; const vString: WideString; var WordArray : TWideStringDynArray): Integer;
var
  i: Integer;
  S: WideString;
begin
  S := vString;
  SetLength(WordArray, 0);
  i := 0;
  while Pos(cSeparator, S) > 0 do begin
    SetLength(WordArray, Length(WordArray) + 1);
    WordArray[i] := Copy(S, 1, Pos(cSeparator, S) -1);
    Inc(i);
    S := Copy(S, Pos(cSeparator, S) + Length(cSeparator), Length(S));
  end;
  SetLength(WordArray, Length(WordArray) +1);
  WordArray[i] := Copy(S, 1, Length(S));

  if (Length(S) = 0) then
    Result := 0
  else
    Result := Length(WordArray);
end;


function Explode(const cSeparator: String; const vString: String; var WordArray : TStringDynArray): Integer;
var
  i: Integer;
  S: String;
begin
  S := vString;
  SetLength(WordArray, 0);
  i := 0;
  while Pos(cSeparator, S) > 0 do begin
    SetLength(WordArray, Length(WordArray) +1);
    WordArray[i] := Copy(S, 1, Pos(cSeparator, S) -1);
    Inc(i);
    S := Copy(S, Pos(cSeparator, S) + Length(cSeparator), Length(S));
  end;
  SetLength(WordArray, Length(WordArray) +1);
  WordArray[i] := Copy(S, 1, Length(S));

  if (Length(S) = 0) then
    Result := 0
  else
    Result := Length(WordArray);
end;

// -----------------------------------------------------------------------------

procedure TagSplit(const Text : WideString; var WordArray : TWideStringDynArray);
var
  i, i1 : integer;
  s : WideString;
  ssaType : Boolean;
begin
  SetLength(WordArray, 0);
  s := Text;
  ssaType := False;
  
  while (Length(s) > 0) do
  begin

    i1 := 0;
    for i:=1 to Length(s) do
    begin
      if (s[i] = '{') then
      begin
        if (i+2 < Length(s)) and
           (s[i+1] = '\') then
        begin
          i1 := i;
          ssaType := True;
          Break;
        end;
      end
      else
      if (s[i] = '<') then
      begin
        i1 := i;
        ssaType := False;        
        Break;
      end;
    end;

    i := i1;
    
    if (i > 0) then
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := Copy(s, 1, i - 1);
      Delete(s, 1, i - 1);

      i1 := 0;
      if (ssaType = True) then
      begin
        for i:=1 to Length(s) do
        begin
          if (s[i] = '}') then
          begin
            i1 := i;
            Break;
          end
        end;
      end
      else
      begin
        for i:=1 to Length(s) do
        begin
          if (s[i] = '>') then
          begin
            i1 := i;
            Break;
          end
        end;
      end;

      i := i1;

      if(i > 0) then
      begin
        SetLength(WordArray, Length(WordArray)+1);
        WordArray[Length(WordArray)-1] := Copy(s, 1, i);
        Delete(s, 1, i);
      end
      else
      begin
        WordArray[Length(WordArray)-1] := WordArray[Length(WordArray)-1] + s;
        s := '';
      end;
    end
    else
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := s;
      s := '';
    end;
  end;
end;

// -----------------------------------------------------------------------------

function StripTags(const Text : WideString) : WideString;
var i : Integer;
    WordArray : TWideStringDynArray;
begin
  TagSplit(Text, WordArray);
  Result := '';
  for i:=0 to Length(WordArray)-1 do
  begin
    if (i mod 2) = 0 then
      Result := Result + WordArray[i];
  end;
end;

function StripText(const Text : WideString) : WideString;
var i : Integer;
    WordArray : TWideStringDynArray;
begin
  TagSplit(Text, WordArray);
  Result := '';
  for i:=0 to Length(WordArray)-1 do
  begin
    if (i mod 2) = 1 then
      Result := Result + WordArray[i];
  end;
end;

// A strip tag that keep new lines inside tag
function StripTagsKeepNL(const Text: WideString) : WideString;
var i, NLCount, j : Integer;
    WordTagArray : TWideStringDynArray;
begin
  TagSplit(Text, WordTagArray);
  Result := '';  
  for i:=0 to Length(WordTagArray)-1 do
  begin
    if (i mod 2) = 0 then
    begin
      Result := Result + WordTagArray[i];
    end
    else
    begin
      // Check if there is new lines in tag
      NLCount := WideStringCount(WordTagArray[i], CRLF);
      for j := 0 to NLCount-1 do
      begin
        Result := Result + CRLF;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function WideMakeRelativePath(const BaseName, DestName : WideString) : WideString;
begin
  Result := WideExtractRelativePath(BaseName, DestName);
end;

// -----------------------------------------------------------------------------

function WideIsAbsolutePath(const Path : WideString) : Boolean;
begin
  Result := (Pos(':', Path) = 2) or (Pos('\\', Path) = 1);
end;

// -----------------------------------------------------------------------------

function WideResolvePath(Filename : WideString) : WideString;
var i, j, p : integer;
    PathElem : WideString;
begin
  i := 1;
  p := 1;
  Filename := Tnt_WideStringReplace(Filename, '/', '\', [rfReplaceAll]);
  while (p <> 0) do
  begin
    p := PosEx('\', Filename, i);
    if (p > 0) then
      PathElem := Copy(Filename, i, p-i)
    else
      PathElem := Copy(Filename, i, Length(Filename));

    if (PathElem = '..') then
    begin
      j := Length(Result);
      while (j > 0) and (Result[j] <> '\') do
        Dec(j);
      Delete(Result, j, Length(Result));
    end
    else if (PathElem <> '.') and (PathElem <> '') then
      Result := Result + '\' + PathElem;
    i := p+1;
  end;
  // Remove slash at start
  Delete(Result, 1, 1);
  // Check for network path
  if (Pos('\\', Filename) = 1) then
    Result := '\\' + Result; 
end;

// -----------------------------------------------------------------------------

function WideResolveRelativePath(const BaseName, DestName : WideString) : WideString;
begin
  if WideIsAbsolutePath(DestName) then
    Result := DestName
  else if (Length(Trim(DestName)) > 0) then
  begin
    Result := WideExtractFilePath(BaseName);
    Result := Result + DestName;
    Result := WideResolvePath(Result);
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function ABGRColor2AssColorString(Input: Cardinal): WideString;
var
  R,G,B,A : Byte;
begin
  R := (Input and $000000FF);
  G := (Input and $0000FF00) shr 8;
  B := (Input and $00FF0000) shr 16;
  A := (Input and $FF000000) shr 24;
  Result := Format('&H%.2x%.2x%.2x%.2x', [A, B, G, R]);
end;

// -----------------------------------------------------------------------------

function AssColorString2ABGR(Color: string): Cardinal;
var ColorInt : Cardinal;
    R, G, B, A : Cardinal;
    CleanColor : string;
    Len : Cardinal;
begin
  ColorInt := 0;
  if (Length(Color) > 0) then
  begin
    // &HAABBGGRR& or &HAABBGGRR& or &HBBGGRR&
    if Pos('&H',Color) = 1 then
    begin
      // ASS hexadecimal color
      // &HAABBGGRR& or &HAABBGGRR& or &HBBGGRR&
      CleanColor := StringReplace(Color, '&', '', [rfReplaceAll]);
      CleanColor := StringReplace(CleanColor, 'H', '', [rfReplaceAll]);
      Len := Length(CleanColor);
      R := StrToIntDef('$' + Copy(CleanColor, Len-1, 2), 0);
      G := StrToIntDef('$' + Copy(CleanColor, Len-3, 2), 0);
      B := StrToIntDef('$' + Copy(CleanColor, Len-5, 2), 0);
      if (Len > 6) then
        A := StrToIntDef('$' + Copy(CleanColor, Len-7, 2), 0)
      else
        A := 0;
      ColorInt := (A shl 24) or (B shl 16) or (G shl 8) or R;
    end
    else
    begin
      // SSA/ASS decimal color
      ColorInt := StrToIntDef(Color, 0);
    end;
  end;
  Result := ColorInt;
end;

// -----------------------------------------------------------------------------

function GetUserApplicationDataFolder : WideString;
var FolderName : WideString;
begin
  SetLength(FolderName, MAX_PATH);
  if SHGetSpecialFolderPathW(0, @FolderName[1], CSIDL_APPDATA, False) then
    Result := PWideChar(@FolderName[1])
  else
    Result := '';
end;
// -----------------------------------------------------------------------------

function GetUserDocumentsFolder : WideString;
var FolderName : WideString;
begin
  SetLength(FolderName, MAX_PATH);
  if SHGetSpecialFolderPathW(0, @FolderName[1], CSIDL_PERSONAL, False) then
    Result := PWideChar(@FolderName[1])
  else
    Result := '';
end;
// -----------------------------------------------------------------------------

function GetTemporaryFolder : WideString;
var TmpFolderLen : Cardinal;
begin
  SetLength(Result, MAX_PATH);
  TmpFolderLen := Tnt_GetTempPathW(MAX_PATH, @Result[1]);
  if (TmpFolderLen > 0) then
    SetLength(Result, TmpFolderLen)
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

// Based on Tnt_WideStringReplace
function WideStringFind(const Offset : Integer; const S, Pattern: WideString;
  IgnoreCase: Boolean = False; WholeWord: Boolean = False): Integer;

  function IsWordSeparator(WC: WideChar): Boolean;
  begin
    Result := (WC = WideChar(#0))
           or IsWideCharSpace(WC)
           or IsWideCharPunct(WC);
  end;

var
  SearchStr, Patt: WideString;
  Off: Integer;
  PrevChar, NextChar: WideChar;
begin
  Result := 0;
  if (Length(S) <= 0) then Exit;
  if (Length(Pattern) <= 0) then
  begin
    Result := 1;
    Exit;
  end;
  if IgnoreCase then
  begin
    SearchStr := Tnt_WideUpperCase(S);
    Patt := Tnt_WideUpperCase(Pattern);
  end else
  begin
    SearchStr := S;
    Patt := Pattern;
  end;
  Off := Offset;
  while (Off <= Length(SearchStr)) do
  begin
    Off := PosEx(Patt, SearchStr, Off);
    if Off = 0 then
    begin
      Break;
    end; // done

    if (WholeWord) then
    begin
      if (Off = 1) then
        PrevChar := WideChar(#0)
      else
        PrevChar := SearchStr[Off - 1];

      if Off + Length(Pattern) <= Length(SearchStr) then
        NextChar := SearchStr[Off + Length(Pattern)]
      else
        NextChar := WideChar(#0);

      if (not IsWordSeparator(PrevChar))
      or (not IsWordSeparator(NextChar)) then
      begin
        Off := Off + Length(Pattern);
        Continue;
      end;
    end;

    Result := Off;
    Break;
  end;
end;

// Based on Tnt_WideStringReplace
function WideStringCount(const S, Pattern: WideString;
  IgnoreCase: Boolean = False; WholeWord: Boolean = False): Integer;

  function IsWordSeparator(WC: WideChar): Boolean;
  begin
    Result := (WC = WideChar(#0))
           or IsWideCharSpace(WC)
           or IsWideCharPunct(WC);
  end;

var
  SearchStr, Patt: WideString;
  Off: Integer;
  PrevChar, NextChar: WideChar;
begin
  Result := 0;
  if (Length(S) <= 0) then Exit;
  if (Length(Pattern) <= 0) then
  begin
    Result := 1;
    Exit;
  end;
  if IgnoreCase then
  begin
    SearchStr := Tnt_WideUpperCase(S);
    Patt := Tnt_WideUpperCase(Pattern);
  end else
  begin
    SearchStr := S;
    Patt := Pattern;
  end;
  Off := 1;
  while (Off <= Length(SearchStr)) do
  begin
    Off := PosEx(Patt, SearchStr, Off);
    if Off = 0 then
    begin
      Break;
    end; // done

    if (WholeWord) then
    begin
      if (Off = 1) then
        PrevChar := WideChar(#0)
      else
        PrevChar := SearchStr[Off - 1];

      if Off + Length(Pattern) <= Length(SearchStr) then
        NextChar := SearchStr[Off + Length(Pattern)]
      else
        NextChar := WideChar(#0);

      if (not IsWordSeparator(PrevChar))
      or (not IsWordSeparator(NextChar)) then
      begin
        Off := Off + Length(Pattern);
        Continue;
      end;
    end;

    Inc(Result);
    Inc(Off);
  end;
end;

// -----------------------------------------------------------------------------

// TODO : better convertion
// 1) convert SRT/SSA/XXX into an internal format
// 2) convert internal format into SRT/SSA/XXX
//
// font size: <font size="10"></font> <=> {\fs10}
// font color: <font color="#FF0000"></font> <=> {\1c&H000000&}

function ConvertSSAToSRT(Src : WideString) : WideString;
begin
  // Italic
  Result := Tnt_WideStringReplace(Src, '{\i1}', '<i>', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '{\i0}', '</i>', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '{\i}', '</i>', [rfReplaceAll]);

  // Bold
  Result := Tnt_WideStringReplace(Result, '{\b1}', '<b>', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '{\b0}', '</b>', [rfReplaceAll]);

  // Underline
  Result := Tnt_WideStringReplace(Result, '{\u1}', '<u>', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '{\u0}', '</u>', [rfReplaceAll]);
end;

function ConvertSRTToSSA(Src : WideString) : WideString;
begin
  // Italic
  Result := Tnt_WideStringReplace(Src, '<i>', '{\i1}', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '</i>', '{\i0}', [rfReplaceAll]);

  // Bold
  Result := Tnt_WideStringReplace(Result, '<b>', '{\b1}', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '</b>', '{\b0}', [rfReplaceAll]);

  // Underline
  Result := Tnt_WideStringReplace(Result, '<u>', '{\u1}', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '</u>', '{\u0}', [rfReplaceAll]);
end;

function ConvertNull(Src : WideString) : WideString;
begin
  Result := Src;
end;

// -----------------------------------------------------------------------------

function SwapColor(Color : Integer) : Integer;
begin
  Result := Color and $0000FF00;
  Result := Result or ((Color and $000000FF) shl 16);
  Result := Result or ((Color and $00FF0000) shr 16);
end;

// -----------------------------------------------------------------------------

function BinarySearch(IntArray : TIntegerDynArray; Value : Integer;
  Backward : Boolean = False) : Integer;
var Min, Mid, Max : Integer;
begin
  Min := Low(IntArray);
  Max := High(IntArray);
  Mid := (Max + Min) div 2;

  while (Min <= Max) do
  begin
    if (IntArray[Mid] < Value) then
      Min := Mid + 1
    else if (IntArray[Mid] > Value) then
      Max := Mid - 1
    else begin
      Result := Mid;
      Exit;
    end;
    Mid := (Max + Min) div 2;
  end;
  if Backward then
    Result := Max
  else
    Result := Min;
end;

// -----------------------------------------------------------------------------


procedure SaveToStreamWS(Stream : TStream; Value : WideString);
var SizeOfString : Integer;
begin
  SizeOfString := Length(Value);
  // Write the length of the WideString
  Stream.WriteBuffer(SizeOfString, SizeOf(SizeOfString));
  // Write the data
  Stream.WriteBuffer(Value[1], SizeOfString * SizeOf(WideChar));
end;

procedure LoadFromStreamWS(Stream : TStream; out Value : WideString);
var SizeOfString : Integer;
begin
  Stream.ReadBuffer(SizeOfString, SizeOf(SizeOfString));
  SetLength(Value, SizeOfString);
  Stream.ReadBuffer(Value[1], SizeOfString * SizeOf(WideChar));
end;

procedure SaveToStreamInt(Stream : TStream; Value : Integer);
begin
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

procedure LoadFromStreamInt(Stream : TStream; out Value : Integer);
begin
  Stream.ReadBuffer(Value, SizeOf(Value));
end;

// -----------------------------------------------------------------------------

procedure SetRESelection(RichEdit : TTntRichEdit; Start, Len : Integer);
var cr : CHARRANGE;
begin
  cr.cpMin := RichEdit.RawWin32CharPos(Start);
  cr.cpMax := cr.cpMin + Len;
  RichEdit.Perform(EM_EXSETSEL, 0, Longint(@(cr)));
end;

procedure SetRESelectionColor(ARichEdit : TTntRichEdit; AColor : TColor);
var Format: CHARFORMAT2;
begin
  FillChar(Format, SizeOf(Format), 0);
  Format.cbSize := SizeOf(Format);
  Format.dwMask := CFM_COLOR;
  Format.crTextColor := ColorToRGB(AColor);
  ARichEdit.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Longint(@Format));
end;

// Underline styles
const
  CFU_UNDERLINETHICK = 9;
  CFU_UNDERLINEWAVE = 8;
  CFU_UNDERLINEDASHDOTDOT = 7;
  CFU_UNDERLINEDASHDOT = 6;
  CFU_UNDERLINEDASH = 5;
  CFU_UNDERLINEDOTTED = 4;
  CFU_UNDERLINE = 1;
  CFU_UNDERLINENONE = 0;
  
procedure SetRECharFormat(ARichEdit: TTntRichEdit; AUnderlineType: Byte; AColor: Word);
var
  // The CHARFORMAT2 structure contains information about
  // character formatting in a rich edit control.
  Format: CHARFORMAT2;
begin
  FillChar(Format, SizeOf(Format), 0);
  with Format do
  begin
    cbSize := SizeOf(Format);
    dwMask := CFM_UNDERLINETYPE;
    bUnderlineType := AUnderlineType or AColor;
    ARichEdit.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Longint(@Format));
  end;
end;

procedure SetReUnderline(ARichEdit: TTntRichEdit; Underline : Boolean);
begin
  if (Underline) then
    SetRECharFormat(ARichEdit, CFU_UNDERLINEWAVE, $50)
  else
    SetRECharFormat(ARichEdit, CFU_UNDERLINENONE, 0)
end;

// -------------------------------------------------------------------------------------------------

function WC2MB(S : WideString) : string;
var realLen : Cardinal;
begin
  SetLength(Result, Length(S) * 4);
  realLen := WideCharToMultiByte(GetACP, 0, @S[1], Length(S), @Result[1],
        Length(S) * 4, nil, nil);
  SetLength(Result, realLen);
end;

function MB2WC(S : string) : WideString;
var realLen : Cardinal;
begin
  SetLength(Result, Length(S) * 4);
  realLen := MultiByteToWideChar(GetACP, 0, @S[1], Length(S), @Result[1],
        Length(S) * 4);
  SetLength(Result, realLen);
end;

procedure MyTTntStringList.LoadFromStream_BOM(Stream: TStream; WithBOM: Boolean);
var
  DataLeft: Integer;
  StreamCharSet: TTntStreamCharSet;
  SW: WideString;
  SA: AnsiString;
  TSA: AnsiString;

begin
  BeginUpdate;
  try
    if WithBOM then
      StreamCharSet := AutoDetectCharacterSet(Stream)
    else
      StreamCharSet := csUnicode;
    DataLeft := Stream.Size - Stream.Position;
    if (StreamCharSet in [csUnicode, csUnicodeSwapped]) then
    begin
      // BOM indicates Unicode text stream
      if DataLeft < SizeOf(WideChar) then
        SW := ''
      else begin
        SetLength(SW, DataLeft div SizeOf(WideChar));
        Stream.Read(PWideChar(SW)^, DataLeft);
        if StreamCharSet = csUnicodeSwapped then
          StrSwapByteOrder(PWideChar(SW));
      end;
      SetTextStr(SW);
    end
    else if StreamCharSet = csUtf8 then
    begin
      // BOM indicates UTF-8 text stream
      SetLength(SA, DataLeft div SizeOf(AnsiChar));
      Stream.Read(PAnsiChar(SA)^, DataLeft);
      SetTextStr(UTF8ToWideString(SA));
    end
    else
    begin
      // without byte order mark it is assumed that we are loading ANSI text
      SetLength(SA, DataLeft div SizeOf(AnsiChar));
      Stream.Read(PAnsiChar(SA)^, DataLeft);
      //check an utf8 file without bom
      TSA := UTF8ToWideString(SA);
      if Length(TSA)<>0 then
        SetTextStr(TSA)
      else
        SetTextStr(MB2WC(SA));
    end;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetCheckedState(const checkBox : TCheckBox; const check : Boolean);
var
  onClickHandler : TNotifyEvent;
begin
  with checkBox do
  begin
    onClickHandler := OnClick;
    OnClick := nil;
    Checked := check;
    OnClick := onClickHandler;
  end;
end;

procedure SetCheckedState(const checkBox : TTntCheckBox; const check : Boolean);
var
  onClickHandler : TNotifyEvent;
begin
  with checkBox do
  begin
    onClickHandler := OnClick;
    OnClick := nil;
    Checked := check;
    OnClick := onClickHandler;
  end;
end;

function LeftPad(S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result  := S;
  RestLen := Len - Length(s);
  if RestLen < 1 then Exit;
  Result := S + StringOfChar(Ch, RestLen);
end;

function RightPad(S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result  := S;
  RestLen := Len - Length(s);
  if RestLen < 1 then Exit;
  Result := StringOfChar(Ch, RestLen) + S;
end;

// -----------------------------------------------------------------------------

procedure SetOpenDialogPath(OpenDlg: TObject);
begin
  TTntOpenDialog(OpenDlg).InitialDir := '';
  if WideDirectoryExists(MainForm.ConfigObject.DefaultPath) then
  begin
    if (MainForm.ConfigObject.ForceDefaultPath) or
       (not DirectoryExists(WideGetCurrentDir)) or
       (WideGetCurrentDir+'\' = ExtractFilePath(ParamStr(0))) then
    begin
      TTntOpenDialog(OpenDlg).InitialDir := MainForm.ConfigObject.DefaultPath;
    end;
    if MainForm.ConfigObject.ForceDefaultPath then
      TTntOpenDialog(OpenDlg).FileName := '';
  end;
end;

// -----------------------------------------------------------------------------

function GetLayoutShortName: String;
var
  LayoutName: array [0 .. KL_NAMELENGTH + 1] of Char;
  LangName: array [0 .. 1024] of Char;
begin
  Result := '??';
  if GetKeyboardLayoutName(@LayoutName) then
  begin
    if GetLocaleInfo(StrToInt('$' + StrPas(LayoutName)),
      LOCALE_SABBREVLANGNAME,
      @LangName, SizeOf(LangName) - 1) <> 0
    then Result := StrPas(LangName);
  end;
  Result := AnsiUpperCase(Copy(Result, 1, 2));
end;


end.
// -----------------------------------------------------------------------------
