// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003-2008 Christophe Paris
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

unit JavaScriptPluginUnit;

interface

uses Classes, js15decl, jsintf, SubStructUnit, TntClasses, TntSysUtils, Graphics, Types, Contnrs, TntActnList;

type
{$TYPEINFO ON}
  TSubtitleRangeJSWrapper = class; // Forward declaration

  TSubtitleRangeJSWrapperChangeStartEvent = procedure (Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : Integer) of object;
  TSubtitleRangeJSWrapperChangeStopEvent = procedure (Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : Integer) of object;
  TSubtitleRangeJSWrapperChangeTextEvent = procedure (Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : WideString) of object;


  TSubtitleRangeJSWrapper = class(TObject)
  private
    FStrippedText : WideString;
    FStrippedTextProcessed : Boolean;
    FOnChangeStart : TSubtitleRangeJSWrapperChangeStartEvent;
    FOnChangeStop : TSubtitleRangeJSWrapperChangeStopEvent;
    FOnChangeText : TSubtitleRangeJSWrapperChangeTextEvent;
    FSubtitleRange : TSubtitleRange;
    function GetIndex : Integer;
    function GetStart : Integer;
    function GetStop : Integer;
    function GetText : WideString;
    function GetStrippedText : WideString;
    procedure SetStart(Value : Integer);
    procedure SetStop(Value : Integer);
    procedure SetText(Value : WideString);
    procedure SetSubtitle(Value : TSubtitleRange);
  public
    constructor Create;
    property OnChangeStart : TSubtitleRangeJSWrapperChangeStartEvent read FOnChangeStart write FOnChangeStart;
    property OnChangeStop : TSubtitleRangeJSWrapperChangeStopEvent read FOnChangeStop write FOnChangeStop;
    property OnChangeText : TSubtitleRangeJSWrapperChangeTextEvent read FOnChangeText write FOnChangeText;
  published
    property Index : Integer read GetIndex;
    property Start : Integer read GetStart write SetStart;
    property Stop : Integer read GetStop write SetStop;
    property Text : WideString read GetText write SetText;
    property StrippedText : WideString read GetStrippedText write SetText;
  end;

  TSceneChangeWrapper = class(TObject)
  private
    FSceneChangeList : TIntegerDynArray;
    FStartOffset, FStopOffset, FFilterOffset : Integer;
    FVisible : Boolean;

    function GetNextIndex(TimeMs : Integer; Backward : Boolean = False) : Integer;
  public
    procedure RegisterSceneChange(JSObject : TJSObject);
    procedure SetSceneChangeList(SceneChangeList : TIntegerDynArray);
    procedure SetOffsets(StartOffset, StopOffset, FilterOffset : Integer);
    procedure SetVisible(Value : Boolean);
    function Delete(StartTimeMs, StopTimeMs : Integer) : TIntegerDynArray;
    procedure Insert(Src : TIntegerDynArray);
    function GetSCArray : TIntegerDynArray;
  published
    function Contains(Start, Stop : Integer) : Boolean;
    function GetCount : Integer;
    function GetAt(Index : Integer) : Integer;
    function GetNext(TimeMs : Integer) : Integer;
    function GetPrevious(TimeMs : Integer) : Integer;

    property StartOffset : Integer read FStartOffset;
    property StopOffset : Integer read FStopOffset;
    property FilterOffset : Integer read FFilterOffset;
    property Visible : Boolean read FVisible;
  end;

  TVSSCoreWrapper = class(TObject)
  private
    FINDEX_COL_IDX : Integer;
    FSTART_COL_IDX : Integer;
    FSTOP_COL_IDX : Integer;
    FSTYLE_COL_IDX : Integer;
    FTEXT_COL_IDX : Integer;
    FVO_COL_IDX : Integer;
    FLAST_CORE_COL_IDX : Integer;

    FMinimumBlank : Integer;
    FCpsTarget : Integer;
    FMinimumDuration : Integer;
    FMaximumDuration : Integer;

    FVideoWidth : Integer;
    FVideoHeight : Integer;

    FSubRangeWrapperPool : array[0..9] of TSubtitleRangeJSWrapper;
    FSubRangeWrapperPoolIndex : Integer;

    FOnSubtitleChangeStart : TSubtitleRangeJSWrapperChangeStartEvent;
    FOnSubtitleChangeStop : TSubtitleRangeJSWrapperChangeStopEvent;
    FOnSubtitleChangeText : TSubtitleRangeJSWrapperChangeTextEvent;

    FVSSCoreEngine : TVSSCoreEngineIntf;

    function MakeWrappedSub(SubtitleRange : TSubtitleRange) : TSubtitleRangeJSWrapper;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Set_INDEX_COL_IDX(Value : Integer);
    procedure Set_START_COL_IDX(Value : Integer);
    procedure Set_STOP_COL_IDX(Value : Integer);
    procedure Set_STYLE_COL_IDX(Value : Integer);
    procedure Set_TEXT_COL_IDX(Value : Integer);
    procedure Set_VO_COL_IDX(Value : Integer);
    procedure Set_LAST_CORE_COL_IDX(Value : Integer);

    procedure SetCpsTarget(Value : Integer);
    procedure SetMinimumDuration(Value : Integer);
    procedure SetMaximumDuration(Value : Integer);
    procedure SetMinimumBlank(Value : Integer);

    procedure SetVideoWidth(Value : Integer);
    procedure SetVideoHeight(Value : Integer);

    procedure RegisterJS(JSParent : TJSObject);

    procedure SetOnSubtitleChangeStartEvent(Value : TSubtitleRangeJSWrapperChangeStartEvent);
    procedure SetOnSubtitleChangeStopEvent(Value : TSubtitleRangeJSWrapperChangeStopEvent);
    procedure SetOnSubtitleChangeTextEvent(Value : TSubtitleRangeJSWrapperChangeTextEvent);

    procedure SetVSSCoreEngine(VSSCoreEngine : TVSSCoreEngineIntf);

    property OnSubtitleChangeStart : TSubtitleRangeJSWrapperChangeStartEvent read FOnSubtitleChangeStart write SetOnSubtitleChangeStartEvent;
    property OnSubtitleChangeStop : TSubtitleRangeJSWrapperChangeStopEvent read FOnSubtitleChangeStop write SetOnSubtitleChangeStopEvent;
    property OnSubtitleChangeText : TSubtitleRangeJSWrapperChangeTextEvent read FOnSubtitleChangeText write SetOnSubtitleChangeTextEvent;

  published
    procedure RegisterJavascriptAction(AName : WideString;
      ACaption : WideString; ADefaultShortcut : WideString);

    function GetSubCount : Integer;
    function GetSubAt(Index : Integer) : TSubtitleRangeJSWrapper;

    function GetFirst : TSubtitleRangeJSWrapper;
    function GetNext(Sub : TSubtitleRangeJSWrapper) : TSubtitleRangeJSWrapper;
    function GetPrevious(Sub : TSubtitleRangeJSWrapper) : TSubtitleRangeJSWrapper;

    function GetSelectedCount : Integer;
    function GetFirstSelected : TSubtitleRangeJSWrapper;
    function GetNextSelected(Sub : TSubtitleRangeJSWrapper) : TSubtitleRangeJSWrapper;

    function DeleteSubtitle(Index : Integer) : Boolean;

    function GetPluginParamValue(JsSection, JsParameter : WideString) : Integer;

    procedure DisableJavascriptItemMenu(ACaption : WideString);
    procedure EnableJavascriptItemMenu(ACaption : WideString);
    procedure InsertBreakBeforeJavascriptMenuItem(ACaption : WideString);
    procedure InsertBreakAfterJavascriptMenuItem(ACaption : WideString);
    procedure SetPluginParamValue(JsSection, JsParameter : WideString; Value : Integer);
    function GetCursorPosition : Integer;
    procedure SetSubColor(StartIndex,LastIndex : Integer; Color : WideString);
    procedure ResetSubColor;
    procedure SetSubIcon(Index : Integer; UpdateIconIndex : Integer);

    function MeasureStringWidth(FontName : WideString; FontSize : Integer;
      Bold : Boolean; Text : WideString) : Integer;

    function GetTextSelectionStart : Integer;
    function GetTextSelectionLength : Integer;
    function GetAudioCursorPosition : Integer;

    property INDEX_COL_IDX : Integer read FINDEX_COL_IDX;
    property START_COL_IDX : Integer read FSTART_COL_IDX;
    property STOP_COL_IDX : Integer read FSTOP_COL_IDX;
    property STYLE_COL_IDX : Integer read FSTYLE_COL_IDX;
    property TEXT_COL_IDX : Integer read FTEXT_COL_IDX;
    property VO_COL_IDX : Integer read FVO_COL_IDX;
    property LAST_CORE_COL_IDX : Integer read FLAST_CORE_COL_IDX;

    property CpsTarget : Integer read FCpsTarget;
    property MinimumDuration : Integer read FMinimumDuration;
    property MaximumDuration : Integer read FMaximumDuration;
    property MinimumBlank : Integer read FMinimumBlank;

    property VideoWidth : Integer read FVideoWidth;
    property VideoHeight : Integer read FVideoHeight;
  end;

{$TYPEINFO OFF}

  TJSPluginParamType = (jsptUnknown, jsptBoolean, jsptInteger, jsptDouble, jsptWideString);
  PJSPluginParam = ^TJSPluginParam;
  TJSPluginParam = record
    Name : WideString;
    ParamType : TJSPluginParamType;
    // Maybe use variant for the value
    BooleanValue : Boolean;
    IntegerValue : Integer;
    DoubleValue : Double;
    WideStringValue : WideString;
    UnitStr : WideString;
    Description : WideString;
  end;

  TJSPluginNotifyEvent = procedure (const Msg : WideString) of object;

  TBaseJavascriptPlugin = class
  protected
    FEngine : TJSEngine;
    FLastErrorMsg : WideString;
    FFatalError : Boolean;
    FVSSPluginObject : TJSObject;
    FFilename : WideString;
    FOnJSPluginError : TJSPluginNotifyEvent;
    FLoadingStack : TStack; // Needed for nested include stuff 
    FPreInitDone : Boolean;
    FFilenames : TTntStringList; // To keep unicode filenames

    procedure OnJsError(eng : TJSEngine; cx: PJSContext; Msg: PChar;
      report: PJSErrorReport);

    // function called during the script loading just after compilation and before execution
    procedure PreInitScript; virtual;
    function InternalLoadScript(Filename : WideString) : Boolean;
    function InternalLoadScripts(Filename : WideString) : Boolean;
    function GetCurrentLoadingFile : WideString;     
  public
    constructor Create;
    destructor Destroy; override;
    function LoadScript(Filename : WideString) : Boolean;
    procedure CallJSFunction(JSFunctionName : WideString);
    procedure Eval(Code : WideString);

    property Filename : WideString read FFilename;
    property LastErrorMsg : WideString read FLastErrorMsg;
    property FatalError : Boolean read FFatalError;    
    property OnJSPluginError : TJSPluginNotifyEvent read FOnJSPluginError write FOnJSPluginError;
  end;

  // Error plugin
  TJavaScriptPlugin = class(TBaseJavascriptPlugin)
  private
    FHasErrorFunc, FFixErrorFunc : TJSFunction;
    FVSSPluginObject : TJSObject;
    FCurrentSub, FPreviousSub, FNextSub : TSubtitleRangeJSWrapper;
    FCurrentSubJS, FPreviousSubJS, FNextSubJS : TJSObject;
    FParamCount : Integer;
    FOnSubtitleChangeStart : TSubtitleRangeJSWrapperChangeStartEvent;
    FOnSubtitleChangeStop : TSubtitleRangeJSWrapperChangeStopEvent;
    FOnSubtitleChangeText : TSubtitleRangeJSWrapperChangeTextEvent;
    FParamArray : array[0..2] of TJSBase; // used when calling a JS function

    // ----- Plugin constant -----
    FName : WideString;
    FDescription : WideString;
    FColor : Integer;
    FMessage : WideString;

    procedure FillParamArray(CurrentSub, PreviousSub, NextSub : TSubtitleRange);
    procedure SetOnSubtitleChangeStartEvent(Value : TSubtitleRangeJSWrapperChangeStartEvent);
    procedure SetOnSubtitleChangeStopEvent(Value : TSubtitleRangeJSWrapperChangeStopEvent);
    procedure SetOnSubtitleChangeTextEvent(Value : TSubtitleRangeJSWrapperChangeTextEvent);

  public
    constructor Create;
    destructor Destroy; override;
    function LoadScript(Filename : WideString) : Boolean;

    procedure FillParamList(ParamList : TList);
    procedure SetParamValue(Name, Value : WideString);
    function HasError(CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;
    procedure FixError(CurrentSub, PreviousSub, NextSub : TSubtitleRange);
    function CanFixError : Boolean;

    property Filename;
    property OnJSPluginError;
    property LastErrorMsg;
    property FatalError;

    property Name : WideString read FName;
    property Description : WideString read FDescription;
    property Color : Integer read FColor;
    property Msg : WideString read FMessage;
    property OnSubtitleChangeStart : TSubtitleRangeJSWrapperChangeStartEvent read FOnSubtitleChangeStart write SetOnSubtitleChangeStartEvent;
    property OnSubtitleChangeStop : TSubtitleRangeJSWrapperChangeStopEvent read FOnSubtitleChangeStop write SetOnSubtitleChangeStopEvent;
    property OnSubtitleChangeText : TSubtitleRangeJSWrapperChangeTextEvent read FOnSubtitleChangeText write SetOnSubtitleChangeTextEvent;
  end;

  // General plugin
  TSimpleJavascriptWrapper = class(TBaseJavascriptPlugin)
  private
    FNotifySubtitleModificationFunc : TJSFunction;
    FNotifySelectionModificationFunc : TJSFunction;
    FNotifyRangeStartDblClickFunc : TJSFunction;
    FNotifyRangeStopDblClickFunc : TJSFunction;
    FCurrentSub, FPreviousSub, FNextSub : TSubtitleRangeJSWrapper;
    FCurrentSubJS, FPreviousSubJS, FNextSubJS : TJSObject;
    FVSSPluginObject : TJSObject;
    FParamArray : array[0..2] of TJSBase; // used when calling a JS function
    FOnJSPluginSetStatusBarText : TJSPluginNotifyEvent;

    FOnSubtitleChangeStart : TSubtitleRangeJSWrapperChangeStartEvent;
    FOnSubtitleChangeStop : TSubtitleRangeJSWrapperChangeStopEvent;
    FOnSubtitleChangeText : TSubtitleRangeJSWrapperChangeTextEvent;
    
    // Columns
    FGetColumnBGColor : TJSFunction;
    FGetColumnText : TJSFunction;

    FCoreColumnsCount : Integer;
    FExtraColumnsCount : Integer;
    FColumnsTitle : TWideStringDynArray;
    FColumnsSize : TIntegerDynArray;
    FColumnsBGColorized : TBooleanDynArray;
    FColumnsCustomText : TBooleanDynArray;
    FIndexParam : TJSInteger;
    FIndexParamArray : array[0..0]of TJSBase;
    FICPNParamArray : array[0..3]of TJSBase; // Index, CurrentSub, PreviousSub, NextSub

    procedure FillParamArray(CurrentSub, PreviousSub, NextSub : TSubtitleRange);
    procedure FillICPNParamArray(Index : Integer; CurrentSub, PreviousSub,
      NextSub : TSubtitleRange);

    procedure SetOnSubtitleChangeStartEvent(Value : TSubtitleRangeJSWrapperChangeStartEvent);
    procedure SetOnSubtitleChangeStopEvent(Value : TSubtitleRangeJSWrapperChangeStopEvent);
    procedure SetOnSubtitleChangeTextEvent(Value : TSubtitleRangeJSWrapperChangeTextEvent);

  protected
    // nothing for now
    
  public
    constructor Create;
    destructor Destroy; override;

    function LoadScript(Filename : WideString) : Boolean;
    function NotifySubtitleModification(CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;
    function NotifySelectionModification(CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;
    function NotifyRangeStartDblClick(CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;
    function NotifyRangeStopDblClick(CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;

    function GetColumnTitle(Index : Integer) : WideString;
    function GetColumnSize(Index : Integer) : Integer;
    function IsColumnBGColorized(Index : Integer) : Boolean;
    function HasColumnCustomText(Index : Integer) : Boolean;
    function GetColumnBgColor(Index : Integer; CurrentSub, PreviousSub, NextSub : TSubtitleRange) : TColor;
    function GetColumnText(Index : Integer; CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;

    property CoreColumnsCount : Integer read FCoreColumnsCount write FCoreColumnsCount;
    property ExtraColumnsCount : Integer read FExtraColumnsCount;
    property OnJSPluginSetStatusBarText : TJSPluginNotifyEvent read FOnJSPluginSetStatusBarText write FOnJSPluginSetStatusBarText;
    property OnSubtitleChangeStart : TSubtitleRangeJSWrapperChangeStartEvent read FOnSubtitleChangeStart write SetOnSubtitleChangeStartEvent;
    property OnSubtitleChangeStop : TSubtitleRangeJSWrapperChangeStopEvent read FOnSubtitleChangeStop write SetOnSubtitleChangeStopEvent;
    property OnSubtitleChangeText : TSubtitleRangeJSWrapperChangeTextEvent read FOnSubtitleChangeText write SetOnSubtitleChangeTextEvent;
  end;

  TAbstractJavaScriptPluginEnumerator = class
  protected
    FOnJSPluginError : TJSPluginNotifyEvent;
  public
    procedure Reset; virtual; abstract;
    function GetNext(var JSP : TJavaScriptPlugin) : Boolean; virtual; abstract;

    property OnJSPluginError : TJSPluginNotifyEvent read FOnJSPluginError write FOnJSPluginError;
  end;

  TJavaScriptPluginEnumerator = class(TAbstractJavaScriptPluginEnumerator)
  private
    FPluginPath : WideString;
    FFindFileAttrs : Integer;
    FSearchRec : TSearchRecW;
  public
    constructor Create(PluginPath : WideString);
    destructor Destroy; override;
    procedure Reset; override;
    function GetNext(var JSP : TJavaScriptPlugin) : Boolean; override;
    function GetPluginByName(Name : WideString) : TJavaScriptPlugin;
    function GetPluginByFilename(Filename : WideString) : TJavaScriptPlugin;
  end;

  TJavascriptAction = class(TTntAction)
  private
    FJSObjectName : WideString;
  public
    property JSObjectName : WideString read FJSObjectName write FJSObjectName;
  end;

  function GetParamValueAsWideString(pParam : PJSPluginParam) : WideString;
  procedure SetParamValueAsWideString(pParam : PJSPluginParam; Value : WideString);
  function JSColorToTColor(RGBColor : Integer) : TColor;

var
  g_SceneChangeWrapper : TSceneChangeWrapper;
  g_VSSCoreWrapper : TVSSCoreWrapper;

implementation

uses SysUtils, Windows, WAVDisplayerUnit, MiscToolsUnit, GlobalUnit;

function IsValidSubRange(Sub : TSubtitleRange) : Boolean;
begin
  Result := g_GlobalContext.SubList.IndexOf(Sub) <> -1;
end;

//------------------------------------------------------------------------------

function GetParamValueAsWideString(pParam : PJSPluginParam) : WideString;
begin
  case pParam.ParamType of
    jsptBoolean: Result := BoolToStr(pParam.BooleanValue);
    jsptInteger: Result := IntToStr(pParam.IntegerValue);
    jsptDouble: Result := FloatToStr(pParam.DoubleValue);
    jsptWideString: Result := pParam.WideStringValue;
    else Result := '';
  end;
end;

//------------------------------------------------------------------------------

procedure SetParamValueAsWideString(pParam : PJSPluginParam; Value : WideString);
begin
  case pParam.ParamType of
    jsptBoolean: pParam.BooleanValue := StrToBool(Value);
    jsptInteger: pParam.IntegerValue := StrToInt(Value);
    jsptDouble: pParam.DoubleValue := StrToFloat(Value);
    jsptWideString: pParam.WideStringValue := Value;
  end;
end;

//------------------------------------------------------------------------------

function JSColorToTColor(RGBColor : Integer) : TColor;
begin
  Result := RGBColor and $0000FF00;
  Result := Result or ((RGBColor and $000000FF) shl 16);
  Result := Result or ((RGBColor and $00FF0000) shr 16);
end;

//------------------------------------------------------------------------------

function _JS_ScriptLog(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  jseng: TJSEngine;
  jsplugin : TBaseJavascriptPlugin;
  pstr : PJSString;
begin
  if (argc = 1) then
  begin
    jseng := TJSEngine(PEngineData(JS_GetContextPrivate(cx))^);
    jsplugin := TBaseJavascriptPlugin(jseng.UserData);
    if Assigned(jsplugin.OnJSPluginError) then
    begin
      pstr := JS_ValueToString(cx, argv^);
      jsplugin.OnJSPluginError(JSStringToString(pstr));
    end;
  end;
  Result := JS_TRUE;
end;

//------------------------------------------------------------------------------

function _JS_SetStatusBarText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  jseng: TJSEngine;
  jsplugin : TSimpleJavascriptWrapper;
  Func : TJSPluginNotifyEvent;
  pstr : PJSString;
begin
  if (argc = 1) then
  begin
    jseng := TJSEngine(PEngineData(JS_GetContextPrivate(cx))^);
    jsplugin := TSimpleJavascriptWrapper(jseng.UserData);
    Func := jsplugin.OnJSPluginSetStatusBarText;
    if Assigned(Func) then
    begin
      pstr := JS_ValueToString(cx, argv^);
      jsplugin.OnJSPluginSetStatusBarText(JSStringToString(pstr));
    end;
  end;
  Result := JS_TRUE;
end;

function _JS_LoadScript(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  jseng: TJSEngine;
  jsplugin : TBaseJavascriptPlugin;
  pstr : PJSString;  
begin
  if (argc = 1) then
  begin
    jseng := TJSEngine(PEngineData(JS_GetContextPrivate(cx))^);
    jsplugin := TBaseJavascriptPlugin(jseng.UserData);
    pstr := JS_ValueToString(cx, argv^);
    jsplugin.InternalLoadScripts(JSStringToString(pstr));
  end;
  Result := JS_TRUE;
end;

// =============================================================================

constructor TSubtitleRangeJSWrapper.Create;
begin
  FStrippedText := '';
  FStrippedTextProcessed := False;
  FSubtitleRange := nil;
  FOnChangeStart := nil;
  FOnChangeStop := nil;
  FOnChangeText := nil;
end;

function TSubtitleRangeJSWrapper.GetIndex : Integer;
begin
  if Assigned(FSubtitleRange.Node) then
    Result := FSubtitleRange.Node.Index + 1
  else
    Result := -1
end;

function TSubtitleRangeJSWrapper.GetStart : Integer;
begin
  Result := FSubtitleRange.StartTime;
end;

function TSubtitleRangeJSWrapper.GetStop : Integer;
begin
  Result := FSubtitleRange.StopTime;
end;

function TSubtitleRangeJSWrapper.GetText : WideString;
begin
  Result := FSubtitleRange.Text;
end;

function TSubtitleRangeJSWrapper.GetStrippedText : WideString;
begin
  if (FStrippedTextProcessed = False) then
  begin
    FStrippedText := StripTags(FSubtitleRange.Text);
    FStrippedTextProcessed := True;
  end;
  Result := FStrippedText;
end;

procedure TSubtitleRangeJSWrapper.SetStart(Value : Integer);
begin
  if (Value <> FSubtitleRange.StartTime) then
  begin
    if Assigned(FOnChangeStart) then
      FOnChangeStart(Self, FSubtitleRange, Value);
    FSubtitleRange.StartTime := Value;
  end;
end;

procedure TSubtitleRangeJSWrapper.SetStop(Value : Integer);
begin
  if (Value <> FSubtitleRange.StopTime) then
  begin
    if Assigned(FOnChangeStop) then
      FOnChangeStop(Self, FSubtitleRange, Value);
    FSubtitleRange.StopTime := Value;
  end;
end;

procedure TSubtitleRangeJSWrapper.SetText(Value : WideString);
begin
  if (Value <> FSubtitleRange.Text) then
  begin
    if Assigned(FOnChangeText) then
      FOnChangeText(Self, FSubtitleRange, Value);
    FSubtitleRange.Text := Value;
    FStrippedTextProcessed := False;
  end;
end;

procedure TSubtitleRangeJSWrapper.SetSubtitle(Value : TSubtitleRange);
begin
  FSubtitleRange := Value;
  FStrippedTextProcessed := False;
  FStrippedText := '';
end;

// =============================================================================

constructor TBaseJavascriptPlugin.Create;
begin
  inherited Create;
  FLoadingStack := TStack.Create;
  FFilenames := TTntStringList.Create;
  FPreInitDone := False;
  FEngine := TJSEngine.Create(256*1024);
  FEngine.OnJSError := OnJsError;
  FEngine.UserData := Self;
  FEngine.Global.AddMethod('ScriptLog', _JS_ScriptLog, 1);
  FEngine.Global.AddMethod('LoadScript', _JS_LoadScript, 1);
end;

//------------------------------------------------------------------------------

destructor TBaseJavascriptPlugin.Destroy;
begin
  FEngine.Free;
  FFilenames.Free;
  FLoadingStack.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TBaseJavascriptPlugin.OnJsError(eng : TJSEngine; cx: PJSContext;
  Msg: PChar; report: PJSErrorReport);
var
	ErrorTypeStr: WideString;
  CurrentFile : WideString;
  FileNameIndex : Integer;
begin
  if JSReportIsException(report) then
  begin
		ErrorTypeStr := 'Exception';
    FFatalError := True;
  end
	else if JSReportIsWarning(report) then
  begin
		ErrorTypeStr := 'Warning';
    FFatalError := False;
  end
	else
  begin
		ErrorTypeStr := 'Error';
    FFatalError := True;
  end;

  FileNameIndex := StrToIntDef(report^.filename, -1);
  if (FileNameIndex >= 0) and (FileNameIndex < FFilenames.Count) then
  begin
    CurrentFile := FFilenames[FileNameIndex];
  end
  else if (FLoadingStack.Count > 0) then
  begin
    CurrentFile := PWideString(FLoadingStack.Peek)^;
  end
  else
  begin
    CurrentFile := FFilename;
  end;

	FLastErrorMsg := Format('%s in %s at line %d', [Msg, WideExtractFileName(CurrentFile), report^.lineno]);
  if (report^.uclinebuf <> '') then
  begin
    FLastErrorMsg := FLastErrorMsg + CRLF + '-> ' + report^.uclinebuf;
  end;
  if Assigned(FOnJSPluginError) then
    FOnJSPluginError(FLastErrorMsg);
end;

//------------------------------------------------------------------------------

procedure TBaseJavascriptPlugin.PreInitScript;
begin
  // VSSCore
  g_VSSCoreWrapper.RegisterJS(FEngine.Global);
end;

//------------------------------------------------------------------------------

function TBaseJavascriptPlugin.GetCurrentLoadingFile : WideString;
begin
  if (FLoadingStack.Count > 0) then
  begin
    Result := PWideString(FLoadingStack.Peek)^;
  end
  else
  begin
    Result := FFilename;
  end;
end;

//------------------------------------------------------------------------------
function TBaseJavascriptPlugin.InternalLoadScripts(Filename : WideString) : Boolean;
var CurrentFilePath : WideString;
    FSearchPath : WideString;
    FFindFileAttrs : Integer;
    FSearchRec : TSearchRecW;
begin
  if (Pos('*', Filename) > 0) then
  begin
    // Check if Filename is an absolute path
    if not WideIsAbsolutePath(Filename) then
    begin
      // Resolve path to be absolute
      CurrentFilePath := WideExtractFilePath(GetCurrentLoadingFile);
      Filename := WideResolveRelativePath(CurrentFilePath, Filename);
    end;
    FSearchPath := WideIncludeTrailingBackslash(WideExtractFilePath(Filename));
    FFindFileAttrs := faAnyFile;
    FSearchRec.FindHandle := INVALID_HANDLE_VALUE;

    if (WideFindFirst(Filename, FFindFileAttrs, FSearchRec) <> 0) then
      WideFindClose(FSearchRec)
    else
    begin
      while (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) do
      begin
        InternalLoadScript(FSearchPath + FSearchRec.Name);
        if (WideFindNext(FSearchRec) <> 0) then
          WideFindClose(FSearchRec);
      end;
    end;

    Result := True;    
  end
  else
  begin
    Result := InternalLoadScript(Filename);
  end;
end;

function TBaseJavascriptPlugin.InternalLoadScript(Filename : WideString) : Boolean;
var Script : TJSScript;
    PFilename : PWideString;
    CurrentFilePath : WideString;
begin
  Result := False;

  // Check if Filename is an absolute path
  if not WideIsAbsolutePath(Filename) then
  begin
    // Resolve path to be absolute
    CurrentFilePath := WideExtractFilePath(GetCurrentLoadingFile);
    Filename := WideResolveRelativePath(CurrentFilePath, Filename);
  end;

  if not WideFileExists(Filename) then
  begin
    if Assigned(FOnJSPluginError) then
    begin
      FOnJSPluginError('Can''t find ' + Filename);
    end;
    Exit;
  end;

  // Save the file name in a stack because spydermonkey doesn't support unicode filename
  New(PFilename);
  PFilename ^:= Filename;
  FLoadingStack.Push(PFilename);
  FFilenames.Add(Filename);

  Script := TJSScript.Create;
  Script.LoadRaw(Filename);
  // try to compile the script, we pass the index of the filename to avoid unicode problems
  Script.Compile(FEngine, IntToStr(FFilenames.Count-1));
  if Script.Compiled then
  begin
    if not FPreInitDone then
    begin
      PreInitScript;
      FPreInitDone := True;
    end;
    Result := Script.Execute(FEngine);
  end
  else
  begin
    // There is probably a pending exception, try to report it now
    if JS_IsExceptionPending(FEngine.Context) = JS_TRUE then
    begin
      // We need an updated DLL to do that
      JS_ReportPendingException(FEngine.Context);
    end
  end;

  Script.Free;
  FLoadingStack.Pop;
  Dispose(PFilename);
end;

//------------------------------------------------------------------------------

function TBaseJavascriptPlugin.LoadScript(Filename : WideString) : Boolean;
begin
  FFilename := Filename;
  Result := InternalLoadScript(Filename);
end;

//------------------------------------------------------------------------------

procedure TBaseJavascriptPlugin.CallJSFunction(JSFunctionName : WideString);
var JSFunction : TJSFunction;
    Dummy : Integer;
begin
  JSFunction := FEngine.Global.GetFunction(JSFunctionName);
  if Assigned(JSFunction) then
  begin
    JSFunction.Call([], Dummy);
    FreeAndNil(JSFunction);
  end;
end;

//------------------------------------------------------------------------------

procedure TBaseJavascriptPlugin.Eval(Code : WideString);
begin
  FEngine.Global.Evaluate(Code);
end;

// =============================================================================

constructor TJavaScriptPlugin.Create;
begin
  inherited Create;

  FCurrentSub := TSubtitleRangeJSWrapper.Create;
  FPreviousSub := TSubtitleRangeJSWrapper.Create;
  FNextSub := TSubtitleRangeJSWrapper.Create;
  FCurrentSubJS := nil; FPreviousSubJS := nil; FNextSubJS := nil;
  FHasErrorFunc := nil; FFixErrorFunc := nil;
  FVSSPluginObject := nil;
end;

//------------------------------------------------------------------------------

destructor TJavaScriptPlugin.Destroy;
begin
  if Assigned(FCurrentSubJS) then
    FreeAndNil(FCurrentSubJS);
  if Assigned(FPreviousSubJS) then
    FreeAndNil(FPreviousSubJS);
  if Assigned(FNextSubJS) then
    FreeAndNil(FNextSubJS);

  if Assigned(FHasErrorFunc) then
    FreeAndNil(FHasErrorFunc);
  if Assigned(FFixErrorFunc) then
    FreeAndNil(FFixErrorFunc);
  if Assigned(FVSSPluginObject) then
    FreeAndNil(FVSSPluginObject);
  
  FNextSub.Free;
  FPreviousSub.Free;
  FCurrentSub.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPlugin.SetOnSubtitleChangeStartEvent(Value : TSubtitleRangeJSWrapperChangeStartEvent);
begin
  if (@Value <> @FOnSubtitleChangeStart) then
  begin
    FOnSubtitleChangeStart := Value;
    FCurrentSub.FOnChangeStart := Value;
    FPreviousSub.FOnChangeStart := Value;
    FNextSub.FOnChangeStart := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPlugin.SetOnSubtitleChangeStopEvent(Value : TSubtitleRangeJSWrapperChangeStopEvent);
begin
  if (@Value <> @FOnSubtitleChangeStop) then
  begin
    FOnSubtitleChangeStop := Value;
    FCurrentSub.FOnChangeStop := Value;
    FPreviousSub.FOnChangeStop := Value;
    FNextSub.FOnChangeStop := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPlugin.SetOnSubtitleChangeTextEvent(Value : TSubtitleRangeJSWrapperChangeTextEvent);
begin
  if (@Value <> @FOnSubtitleChangeText) then
  begin
    FOnSubtitleChangeText := Value;
    FCurrentSub.FOnChangeText := Value;
    FPreviousSub.FOnChangeText := Value;
    FNextSub.FOnChangeText := Value;
  end;
end;

//------------------------------------------------------------------------------

function TJavaScriptPlugin.LoadScript(Filename : WideString) : Boolean;
begin
  Result := inherited LoadScript(Filename);
  if not Result then
    Exit;

  if FEngine.Global.GetProperty('VSSPlugin', FVSSPluginObject) then
  begin
    FCurrentSubJS := FVSSPluginObject.AddNativeObject(FCurrentSub, '__FCurrentSub');
    FPreviousSubJS := FVSSPluginObject.AddNativeObject(FPreviousSub, '__FPreviousSub');
    FNextSubJS := FVSSPluginObject.AddNativeObject(FNextSub, '__FNextSub');
    FHasErrorFunc := FVSSPluginObject.GetFunction('HasError');
    if FVSSPluginObject.IsFunction('FixError') then
      FFixErrorFunc := FVSSPluginObject.GetFunction('FixError');

    // ----- Plugin constant -----
    FVSSPluginObject.GetProperty('Name', FName);
    FVSSPluginObject.GetProperty('Description', FDescription);
    FVSSPluginObject.GetProperty('Color', FColor);
    FVSSPluginObject.GetProperty('Message', FMessage);

    // Scene change
    g_SceneChangeWrapper.RegisterSceneChange(FEngine.Global);

    // We need at least one function
    Result := (FHasErrorFunc <> nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPlugin.FillParamList(ParamList : TList);
var ListEnum : TStringArray;
    i : Integer;
    pPluginParam : PJSPluginParam;
    ParamObject : TJSObject;
    Desc : WideString;
begin
  ParamList.Clear;
  FParamCount := 0;
  ListEnum := FVSSPluginObject.Enumerate;
  for i := 0 to Length(ListEnum)-1 do
  begin
    if Pos('Param', ListEnum[i]) = 1 then
    begin
      FVSSPluginObject.GetProperty(ListEnum[i], ParamObject);
      if not Assigned(ParamObject) then
        Continue;
      pPluginParam := New(PJSPluginParam);
      ZeroMemory(pPluginParam, SizeOf(TJSPluginParam));
      pPluginParam.Name := ListEnum[i];
      ParamObject.GetProperty('Unit', pPluginParam.UnitStr);
      ParamObject.GetProperty('Description', Desc);
      if (Desc <> 'undefined') then
      begin
        pPluginParam.Description := Desc;
      end;
      case ParamObject.TypeOf('Value') of
        JSTYPE_NUMBER:
          if (ParamObject.IsInteger('Value')) then
          begin
            pPluginParam.ParamType := jsptInteger;
            ParamObject.GetProperty('Value', pPluginParam.IntegerValue);
          end else begin
            pPluginParam.ParamType := jsptDouble;
            ParamObject.GetProperty('Value', pPluginParam.DoubleValue);
          end;
        JSTYPE_STRING:
          begin
            pPluginParam.ParamType := jsptWideString;
            ParamObject.GetProperty('Value', pPluginParam.WideStringValue);
          end;
        JSTYPE_BOOLEAN:
          begin
            pPluginParam.ParamType := jsptBoolean;
            ParamObject.GetProperty('Value', pPluginParam.BooleanValue);
          end;
        else
            pPluginParam.ParamType := jsptUnknown;
      end;
      if (pPluginParam.ParamType <> jsptUnknown) then
      begin
        ParamList.Add(pPluginParam);
        Inc(FParamCount);
      end
      else
        Dispose(pPluginParam);
      //FreeAndNil(ParamObject);
    end;
  end;
  SetLength(ListEnum, 0);
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPlugin.SetParamValue(Name, Value : WideString);
var ParamObject : TJSObject;
    val: TJSBase;
begin
  if FVSSPluginObject.HasProperty(Name) and
    FVSSPluginObject.GetProperty(Name, ParamObject) and
    Assigned(ParamObject) then
  begin
    if ParamObject.HasProperty('Value') then
    begin
      case ParamObject.TypeOf('Value') of
        JSTYPE_NUMBER:
          if (ParamObject.IsInteger('Value')) then
            val := ParamObject.Declare(StrToInt(Value))
          else
            val := ParamObject.Declare(StrToFloat(Value));
        JSTYPE_STRING:
          val := ParamObject.Declare(Value);
        JSTYPE_BOOLEAN:
          val := ParamObject.Declare(StrToBool(Value));
        else
          val := nil;
      end;
      if Assigned(val) then
      begin
        ParamObject.SetProperty('Value', val);
      end;
    end;
    //FreeAndNil(ParamObject);
  end;
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPlugin.FillParamArray(CurrentSub, PreviousSub, NextSub : TSubtitleRange);
var i : Integer;
begin
  for i:=0 to Length(FParamArray)-1 do
    FParamArray[i] := nil;
  
  if Assigned(CurrentSub) then
  begin
    FCurrentSub.SetSubtitle(CurrentSub);
    FParamArray[0] := FCurrentSubJS;
  end;
  if Assigned(PreviousSub) then
  begin
    FPreviousSub.SetSubtitle(PreviousSub);
    FParamArray[1] := FPreviousSubJS;
  end;
  if Assigned(NextSub) then
  begin
    FNextSub.SetSubtitle(NextSub);
    FParamArray[2] := FNextSubJS;
  end;
end;

//------------------------------------------------------------------------------

function TJavaScriptPlugin.HasError(CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;
begin
  FillParamArray(CurrentSub, PreviousSub, NextSub);
  FHasErrorFunc.Call(FParamArray, Result);
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPlugin.FixError(CurrentSub, PreviousSub, NextSub : TSubtitleRange);
var Dummy : Boolean;
begin
  if Assigned(FFixErrorFunc) then
  begin
    FillParamArray(CurrentSub, PreviousSub, NextSub);
    FFixErrorFunc.Call(FParamArray, Dummy);
  end;
end;

//------------------------------------------------------------------------------

function TJavaScriptPlugin.CanFixError : Boolean;
begin
  Result := Assigned(FFixErrorFunc);
end;

// =============================================================================

constructor TJavaScriptPluginEnumerator.Create(PluginPath : WideString);
begin
  inherited Create;
  FPluginPath := WideIncludeTrailingBackslash(PluginPath);
  FFindFileAttrs := faAnyFile;
  FSearchRec.FindHandle := INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------

destructor TJavaScriptPluginEnumerator.Destroy;
begin
  if (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) then
    WideFindClose(FSearchRec);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TJavaScriptPluginEnumerator.Reset;
begin
  if (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) then
    WideFindClose(FSearchRec);
  if (WideFindFirst(FPluginPath + '*.js', FFindFileAttrs, FSearchRec) <> 0) then
      WideFindClose(FSearchRec);
end;

//------------------------------------------------------------------------------

function TJavaScriptPluginEnumerator.GetNext(var JSP : TJavaScriptPlugin) : Boolean;
begin
  Result := False;
  if (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) then
  begin
    JSP := TJavaScriptPlugin.Create;
    JSP.OnJSPluginError := FOnJSPluginError;
    Result := JSP.LoadScript(FPluginPath + FSearchRec.Name);
    if (WideFindNext(FSearchRec) <> 0) then
      WideFindClose(FSearchRec);
    if not Result then
    begin
      FreeAndNil(JSP);
      Result := GetNext(JSP);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TJavaScriptPluginEnumerator.GetPluginByName(Name : WideString) : TJavaScriptPlugin;
var JSPCurrent : TJavaScriptPlugin;
begin
  Result := nil;
  Reset;
  while (GetNext(JSPCurrent)) do
  begin
    if (JSPCurrent.Name = Name) then
    begin
      Result := JSPCurrent;
      Break;
    end;
    JSPCurrent.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJavaScriptPluginEnumerator.GetPluginByFilename(Filename : WideString) : TJavaScriptPlugin;
begin
  Result := nil;
  Reset;
  while (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) do
  begin
    if ((FPluginPath + FSearchRec.Name) = Filename) then
    begin
      Result := TJavaScriptPlugin.Create;
      Result.OnJSPluginError := FOnJSPluginError;
      if Result.LoadScript(FPluginPath + FSearchRec.Name) = False then
      begin
        Result.Free;
        Result := nil;
      end;
      Break;
    end;
    if (WideFindNext(FSearchRec) <> 0) then
      WideFindClose(FSearchRec);
  end;
end;

// =============================================================================

constructor TSimpleJavascriptWrapper.Create;
begin
  inherited Create;

  FEngine.Global.AddMethod('SetStatusBarText', _JS_SetStatusBarText, 1);

  FCurrentSub := TSubtitleRangeJSWrapper.Create;
  FPreviousSub := TSubtitleRangeJSWrapper.Create;
  FNextSub := TSubtitleRangeJSWrapper.Create;
  FCurrentSubJS := nil; FPreviousSubJS := nil; FNextSubJS := nil;
  FVSSPluginObject := nil;
  FNotifySubtitleModificationFunc := nil;
  FNotifySelectionModificationFunc := nil;
  FNotifyRangeStartDblClickFunc := nil;
  FNotifyRangeStopDblClickFunc := nil;

  FCoreColumnsCount := 0;
  FExtraColumnsCount := 0;
  FGetColumnBGColor := nil;
  FGetColumnText := nil;
  FIndexParam := nil;
end;

//------------------------------------------------------------------------------

destructor TSimpleJavascriptWrapper.Destroy;
begin
  if Assigned(FCurrentSubJS) then
    FreeAndNil(FCurrentSubJS);
  if Assigned(FPreviousSubJS) then
    FreeAndNil(FPreviousSubJS);
  if Assigned(FNextSubJS) then
    FreeAndNil(FNextSubJS);

  if Assigned(FNotifySubtitleModificationFunc) then
    FreeAndNil(FNotifySubtitleModificationFunc);
  if Assigned(FNotifySelectionModificationFunc) then
    FreeAndNil(FNotifySelectionModificationFunc);
  if Assigned(FNotifyRangeStartDblClickFunc) then
    FreeAndNil(FNotifyRangeStartDblClickFunc);
  if Assigned(FNotifyRangeStopDblClickFunc) then
    FreeAndNil(FNotifyRangeStopDblClickFunc);
    
  if Assigned(FGetColumnBGColor) then
    FreeAndNil(FGetColumnBGColor);
  if Assigned(FGetColumnText) then
    FreeAndNil(FGetColumnText);
  if Assigned(FIndexParam) then
    FreeAndNil(FIndexParam);

  if Assigned(FVSSPluginObject) then
    FreeAndNil(FVSSPluginObject);

  FNextSub.Free;
  FPreviousSub.Free;
  FCurrentSub.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleJavascriptWrapper.SetOnSubtitleChangeStartEvent(Value : TSubtitleRangeJSWrapperChangeStartEvent);
begin
  if (@Value <> @FOnSubtitleChangeStart) then
  begin
    FOnSubtitleChangeStart := Value;
    FCurrentSub.FOnChangeStart := Value;
    FPreviousSub.FOnChangeStart := Value;
    FNextSub.FOnChangeStart := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleJavascriptWrapper.SetOnSubtitleChangeStopEvent(Value : TSubtitleRangeJSWrapperChangeStopEvent);
begin
  if (@Value <> @FOnSubtitleChangeStop) then
  begin
    FOnSubtitleChangeStop := Value;
    FCurrentSub.FOnChangeStop := Value;
    FPreviousSub.FOnChangeStop := Value;
    FNextSub.FOnChangeStop := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleJavascriptWrapper.SetOnSubtitleChangeTextEvent(Value : TSubtitleRangeJSWrapperChangeTextEvent);
begin
  if (@Value <> @FOnSubtitleChangeText) then
  begin
    FOnSubtitleChangeText := Value;
    FCurrentSub.FOnChangeText := Value;
    FPreviousSub.FOnChangeText := Value;
    FNextSub.FOnChangeText := Value;
  end;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.NotifySubtitleModification(CurrentSub,
  PreviousSub, NextSub : TSubtitleRange) : WideString;
begin
  if Assigned(FNotifySubtitleModificationFunc) then
  begin
    FillParamArray(CurrentSub, PreviousSub, NextSub);
    FNotifySubtitleModificationFunc.Call(FParamArray, Result);
  end;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.NotifySelectionModification(CurrentSub,
  PreviousSub, NextSub : TSubtitleRange) : WideString;
begin
  if Assigned(FNotifySelectionModificationFunc) then
  begin
    FillParamArray(CurrentSub, PreviousSub, NextSub);
    FNotifySelectionModificationFunc.Call(FParamArray, Result);
  end;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.NotifyRangeStartDblClick(CurrentSub,
  PreviousSub, NextSub : TSubtitleRange) : WideString;
begin
  if Assigned(FNotifyRangeStartDblClickFunc) then
  begin
    FillParamArray(CurrentSub, PreviousSub, NextSub);
    FNotifyRangeStartDblClickFunc.Call(FParamArray, Result);
  end;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.NotifyRangeStopDblClick(CurrentSub,
  PreviousSub, NextSub : TSubtitleRange) : WideString;
begin
  if Assigned(FNotifyRangeStopDblClickFunc) then
  begin
    FillParamArray(CurrentSub, PreviousSub, NextSub);
    FNotifyRangeStopDblClickFunc.Call(FParamArray, Result);
  end;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.LoadScript(Filename : WideString) : Boolean;
var JSFunction : TJSFunction;
    i : Integer;
    ParamResultWS : WideString;
    ParamResultInt : Integer;
    ParamResultBool : Boolean;
begin
  Result := inherited LoadScript(Filename);
  if not Result then
    Exit;

  if FEngine.Global.GetProperty('VSSPlugin', FVSSPluginObject) then
  begin
    FCurrentSubJS := FVSSPluginObject.AddNativeObject(FCurrentSub, '__FCurrentSub');
    FPreviousSubJS := FVSSPluginObject.AddNativeObject(FPreviousSub, '__FPreviousSub');
    FNextSubJS := FVSSPluginObject.AddNativeObject(FNextSub, '__FNextSub');

    FNotifySubtitleModificationFunc := FVSSPluginObject.GetFunction('OnSubtitleModification');
    FNotifySelectionModificationFunc := FVSSPluginObject.GetFunction('OnSelectedSubtitle');
    FNotifyRangeStartDblClickFunc := FVSSPluginObject.GetFunction('OnRangeStartDblClick');
    FNotifyRangeStopDblClickFunc := FVSSPluginObject.GetFunction('OnRangeStopDblClick');

    JSFunction := FVSSPluginObject.GetFunction('GetExtraColumnsCount');
    if Assigned(JSFunction) then
    begin
      JSFunction.Call(FExtraColumnsCount);
      FreeAndNil(JSFunction);
    end;

    if (FExtraColumnsCount > 0) then
    begin
      FIndexParam := TJSInteger.Create(0, FEngine, '');
      FIndexParamArray[0] := FIndexParam;

      JSFunction := FVSSPluginObject.GetFunction('GetColumnTitle');
      if Assigned(JSFunction) then
      begin
        SetLength(FColumnsTitle, FCoreColumnsCount + FExtraColumnsCount);
        for i := 0 to (FCoreColumnsCount + FExtraColumnsCount - 1) do
        begin
          FIndexParam.Value := i;
          JSFunction.Call(FIndexParamArray, ParamResultWS);
          FColumnsTitle[i] := ParamResultWS;
        end;
        FreeAndNil(JSFunction);
      end;

      JSFunction := FVSSPluginObject.GetFunction('GetColumnSize');
      if Assigned(JSFunction) then
      begin
        SetLength(FColumnsSize, FCoreColumnsCount + FExtraColumnsCount);
        for i := 0 to (FCoreColumnsCount + FExtraColumnsCount - 1) do
        begin
          FIndexParam.Value := i;
          JSFunction.Call(FIndexParamArray, ParamResultInt);
          FColumnsSize[i] := ParamResultInt;
        end;
        FreeAndNil(JSFunction);
      end;

      JSFunction := FVSSPluginObject.GetFunction('IsColumnBGColorized');
      if Assigned(JSFunction) then
      begin
        SetLength(FColumnsBGColorized, FCoreColumnsCount + FExtraColumnsCount);
        for i := 0 to (FCoreColumnsCount + FExtraColumnsCount - 1) do
        begin
          FIndexParam.Value := i;
          JSFunction.Call(FIndexParamArray, ParamResultBool);
          FColumnsBGColorized[i] := ParamResultBool;
        end;
        FreeAndNil(JSFunction);
      end;

      JSFunction := FVSSPluginObject.GetFunction('HasColumnCustomText');
      if Assigned(JSFunction) then
      begin
        SetLength(FColumnsCustomText, FCoreColumnsCount + FExtraColumnsCount);
        for i := 0 to (FCoreColumnsCount + FExtraColumnsCount - 1) do
        begin
          FIndexParam.Value := i;
          JSFunction.Call(FIndexParamArray, ParamResultBool);
          FColumnsCustomText[i] := ParamResultBool;
        end;
        FreeAndNil(JSFunction);
      end;
    end;

    FGetColumnBGColor := FVSSPluginObject.GetFunction('GetColumnBGColor');
    FGetColumnText := FVSSPluginObject.GetFunction('GetColumnText');

    // Scene change
    g_SceneChangeWrapper.RegisterSceneChange(FEngine.Global);

    // We need at least one function
    Result := (FNotifySubtitleModificationFunc <> nil) or
      (FNotifySelectionModificationFunc <> nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleJavascriptWrapper.FillParamArray(CurrentSub, PreviousSub,
  NextSub : TSubtitleRange);
var i : Integer;
begin
  for i:=0 to Length(FParamArray)-1 do
    FParamArray[i] := nil;

  if Assigned(CurrentSub) then
  begin
    FCurrentSub.SetSubtitle(CurrentSub);
    FParamArray[0] := FCurrentSubJS;
  end;
  if Assigned(PreviousSub) then
  begin
    FPreviousSub.SetSubtitle(PreviousSub);
    FParamArray[1] := FPreviousSubJS;
  end;
  if Assigned(NextSub) then
  begin
    FNextSub.SetSubtitle(NextSub);
    FParamArray[2] := FNextSubJS;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleJavascriptWrapper.FillICPNParamArray(Index : Integer; CurrentSub, PreviousSub,
  NextSub : TSubtitleRange);
var i : Integer;
begin
  for i:=0 to Length(FICPNParamArray)-1 do
    FICPNParamArray[i] := nil;

  FIndexParam.Value := Index;
  FICPNParamArray[0] := FIndexParam;
  
  if Assigned(CurrentSub) then
  begin
    FCurrentSub.SetSubtitle(CurrentSub);
    FICPNParamArray[1] := FCurrentSubJS;
  end;
  if Assigned(PreviousSub) then
  begin
    FPreviousSub.SetSubtitle(PreviousSub);
    FICPNParamArray[2] := FPreviousSubJS;
  end;
  if Assigned(NextSub) then
  begin
    FNextSub.SetSubtitle(NextSub);
    FICPNParamArray[3] := FNextSubJS;
  end;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.GetColumnTitle(Index : Integer) : WideString;
begin
  if (Index >= Low(FColumnsTitle)) and (Index <= High(FColumnsTitle)) then
    Result := FColumnsTitle[Index]
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.GetColumnSize(Index : Integer) : Integer;
begin
  if (Index >= Low(FColumnsSize)) and (Index <= High(FColumnsSize)) then
    Result := FColumnsSize[Index]
  else
    Result := 50;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.IsColumnBGColorized(Index : Integer) : Boolean;
begin
  if (Index >= Low(FColumnsBGColorized)) and (Index <= High(FColumnsBGColorized)) then
    Result := FColumnsBGColorized[Index]
  else
    Result := False;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.HasColumnCustomText(Index : Integer) : Boolean;
begin
  if (Index >= Low(FColumnsCustomText)) and (Index <= High(FColumnsCustomText)) then
    Result := FColumnsCustomText[Index]
  else
    Result := False;
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.GetColumnBgColor(Index : Integer;
  CurrentSub, PreviousSub, NextSub : TSubtitleRange) : TColor;
var ParamResult : Integer;
begin
  if Assigned(FGetColumnBGColor) then
  begin
    FillICPNParamArray(Index, CurrentSub, PreviousSub, NextSub);
    FGetColumnBGColor.Call(FICPNParamArray, ParamResult);
  end;
  Result := JSColorToTColor(ParamResult);
end;

//------------------------------------------------------------------------------

function TSimpleJavascriptWrapper.GetColumnText(Index : Integer;
  CurrentSub, PreviousSub, NextSub : TSubtitleRange) : WideString;
var ParamResult : WideString;
begin
  if Assigned(FGetColumnText) then
  begin
    FillICPNParamArray(Index, CurrentSub, PreviousSub, NextSub);
    FGetColumnText.Call(FICPNParamArray, ParamResult);
  end;
  Result := ParamResult;
end;

//==============================================================================

// SceneChange.GetCount() : Get the total number of scene change
function TSceneChangeWrapper.GetCount : Integer;
begin
  Result := Length(FSceneChangeList);
end;

// SceneChange.GetAt(Index) : Get the time of the scene change in ms at the specified index. index is between 0 and GetCount()-1
function TSceneChangeWrapper.GetAt(Index : Integer) : Integer;
begin
  if (Index >= Low(FSceneChangeList)) and (Index <= High(FSceneChangeList)) then
    Result := FSceneChangeList[Index]
  else
    Result := -1;
end;

function TSceneChangeWrapper.GetNextIndex(TimeMs : Integer;
  Backward : Boolean = False) : Integer;
begin
  Result := BinarySearch(FSceneChangeList, TimeMs, Backward);
end;

// SceneChange.GetNext(TimeMs) : Get the time in ms of the next scene change >= TimeMs
function TSceneChangeWrapper.GetNext(TimeMs : Integer) : Integer;
begin
  Result := GetAt(GetNextIndex(TimeMs));
end;

// SceneChange.GetPrevious(TimeMs) : Get the time in ms of the next scene change <= TimeMs
function TSceneChangeWrapper.GetPrevious(TimeMs : Integer) : Integer;
begin
  Result := GetAt(GetNextIndex(TimeMs, True));
end;

// SceneChange.Contains(Start,Stop) : Check if there is a scene change between [Start,Stop]
function TSceneChangeWrapper.Contains(Start, Stop : Integer) : Boolean;
var I : Integer;
begin
  I := GetNextIndex(Start);
  Result := (I <= High(FSceneChangeList)) and (FSceneChangeList[I] <= Stop);
end;

procedure TSceneChangeWrapper.RegisterSceneChange(JSObject : TJSObject);
var SceneChangeJSObj : TJSObject;
begin
  SceneChangeJSObj := JSObject.AddNativeObject(Self, 'SceneChange');
  SceneChangeJSObj.SetMethodInfo('Contains', 2, rtBoolean);
  SceneChangeJSObj.SetMethodInfo('GetCount', 0, rtInteger);
  SceneChangeJSObj.SetMethodInfo('GetAt', 1, rtInteger);
  SceneChangeJSObj.SetMethodInfo('GetNext', 1, rtInteger);
  SceneChangeJSObj.SetMethodInfo('GetPrevious', 1, rtInteger);
end;

procedure TSceneChangeWrapper.SetSceneChangeList(SceneChangeList : TIntegerDynArray);
begin
  SetLength(FSceneChangeList, System.Length(SceneChangeList));
  if System.Length(SceneChangeList) > 0 then
  begin
    CopyMemory(@FSceneChangeList[0], @SceneChangeList[0],
      Length(SceneChangeList) * SizeOf(Integer));
  end;
end;

procedure TSceneChangeWrapper.SetOffsets(StartOffset, StopOffset, FilterOffset : Integer);
begin
  FStartOffset := StartOffset;
  FStopOffset := StopOffset;
  FFilterOffset := FilterOffset;
end;

procedure TSceneChangeWrapper.SetVisible(Value : Boolean);
begin
  FVisible := Value;
end;

function TSceneChangeWrapper.Delete(StartTimeMs, StopTimeMs : Integer) : TIntegerDynArray;
var StartIdx, StopIdx, Count, I, J : Integer;
begin
  StartIdx := GetNextIndex(StartTimeMs);
  StopIdx := GetNextIndex(StopTimeMs, True);
  if (StopIdx = -1) then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  Count := StopIdx - StartIdx + 1;

  SetLength(Result, Count);
  J := 0;
  for I := StartIdx to StopIdx do
  begin
    Result[J] := FSceneChangeList[I];
    Inc(J);
  end;

  for I := (StopIdx + 1) to High(FSceneChangeList) do
  begin
    FSceneChangeList[I - Count] := FSceneChangeList[I];
  end;
  SetLength(FSceneChangeList, Length(FSceneChangeList) - Count);
end;

procedure TSceneChangeWrapper.Insert(Src : TIntegerDynArray);
var NewSCArray : TIntegerDynArray;
    I, J, K : Integer;
begin
  I := 0; J := 0; K := 0;
  SetLength(NewSCArray, Length(FSceneChangeList) + Length(Src));
  while (I < Length(FSceneChangeList)) and (J < Length(Src))  do
  begin
    while (I < Length(FSceneChangeList)) and (FSceneChangeList[I] < Src[J]) do
    begin
      NewSCArray[K] := FSceneChangeList[I];
      Inc(K);
      Inc(I);
    end;
    while (J < Length(Src)) and (FSceneChangeList[I] > Src[J]) do
    begin
      NewSCArray[K] := Src[J];
      Inc(K);
      Inc(J);
    end;
    while (J < Length(Src)) and (I < Length(FSceneChangeList)) and (FSceneChangeList[I] = Src[J]) do
    begin
      NewSCArray[K] := FSceneChangeList[I];
      Inc(K);
      Inc(J);
      Inc(I);
    end;
  end;
  while (I < Length(FSceneChangeList)) do
  begin
    NewSCArray[K] := FSceneChangeList[I];
    Inc(K);
    Inc(I);
  end;
  while (J < Length(Src)) do
  begin
    NewSCArray[K] := Src[J];
    Inc(K);
    Inc(J);
  end;
  // Re-adjust space
  SetLength(NewSCArray, K);
  SetSceneChangeList(NewSCArray);
end;

function TSceneChangeWrapper.GetSCArray : TIntegerDynArray;
begin
  Result := FSceneChangeList;
end;

//==============================================================================

constructor TVSSCoreWrapper.Create;
var i : Integer;
begin
  inherited Create;

  for i := Low(FSubRangeWrapperPool) to High(FSubRangeWrapperPool) do
  begin
    FSubRangeWrapperPool[i] := TSubtitleRangeJSWrapper.Create;
  end;
  FSubRangeWrapperPoolIndex := 0;
end;

destructor TVSSCoreWrapper.Destroy;
var i : Integer;
begin
  for i := Low(FSubRangeWrapperPool) to High(FSubRangeWrapperPool) do
  begin
    FreeAndNil(FSubRangeWrapperPool[i]);
  end;
  FVSSCoreEngine := nil;
end;

function TVSSCoreWrapper.MakeWrappedSub(SubtitleRange : TSubtitleRange) : TSubtitleRangeJSWrapper;
begin
  Result := FSubRangeWrapperPool[FSubRangeWrapperPoolIndex];
  FSubRangeWrapperPoolIndex := (FSubRangeWrapperPoolIndex + 1) mod Length(FSubRangeWrapperPool);
  Result.SetSubtitle(SubtitleRange);
end;

procedure TVSSCoreWrapper.Set_INDEX_COL_IDX(Value : Integer);
begin
  FINDEX_COL_IDX := Value;
end;

procedure TVSSCoreWrapper.Set_START_COL_IDX(Value : Integer);
begin
  FSTART_COL_IDX := Value;
end;

procedure TVSSCoreWrapper.Set_STOP_COL_IDX(Value : Integer);
begin
  FSTOP_COL_IDX := Value;
end;

procedure TVSSCoreWrapper.Set_STYLE_COL_IDX(Value : Integer);
begin
  FSTYLE_COL_IDX := Value;
end;

procedure TVSSCoreWrapper.Set_TEXT_COL_IDX(Value : Integer);
begin
  FTEXT_COL_IDX := Value;
end;

procedure TVSSCoreWrapper.Set_VO_COL_IDX(Value : Integer);
begin
  FVO_COL_IDX := Value;
end;

procedure TVSSCoreWrapper.Set_LAST_CORE_COL_IDX(Value : Integer);
begin
  FLAST_CORE_COL_IDX := Value;
end;

procedure TVSSCoreWrapper.SetCpsTarget(Value : Integer);
begin
  FCpsTarget := Value;
end;

procedure TVSSCoreWrapper.SetMinimumDuration(Value : Integer);
begin
  FMinimumDuration := Value;
end;

procedure TVSSCoreWrapper.SetMaximumDuration(Value : Integer);
begin
  FMaximumDuration := Value;
end;

procedure TVSSCoreWrapper.SetMinimumBlank(Value : Integer);
begin
  FMinimumBlank := Value;
end;

procedure TVSSCoreWrapper.SetVideoWidth(Value : Integer);
begin
  FVideoWidth := Value;
end;

procedure TVSSCoreWrapper.SetVideoHeight(Value : Integer);
begin
  FVideoHeight := Value;
end;

procedure TVSSCoreWrapper.RegisterJS(JSParent : TJSObject);
var VSSCoreJSObj : TJSObject;
    i : Integer;
begin
  VSSCoreJSObj := JSParent.AddNativeObject(Self, 'VSSCore');
  VSSCoreJSObj.SetMethodInfo('RegisterJavascriptAction', 3, rtNone);

  VSSCoreJSObj.SetMethodInfo('GetSubCount', 0, rtInteger);
  VSSCoreJSObj.SetMethodInfo('GetSelectedCount', 0, rtInteger);
  VSSCoreJSObj.SetMethodInfo('GetSubAt', 1, rtObject);

  VSSCoreJSObj.SetMethodInfo('GetFirst', 0, rtObject);
  VSSCoreJSObj.SetMethodInfo('GetNext', 1, rtObject);
  VSSCoreJSObj.SetMethodInfo('GetPrevious', 1, rtObject);

  VSSCoreJSObj.SetMethodInfo('GetFirstSelected', 0, rtObject);
  VSSCoreJSObj.SetMethodInfo('GetNextSelected', 1, rtObject);
  
  VSSCoreJSObj.SetMethodInfo('DeleteSubtitle', 1, rtBoolean);

  VSSCoreJSObj.SetMethodInfo('GetPluginParamValue', 2, rtInteger);

  VSSCoreJSObj.SetMethodInfo('DisableJavascriptItemMenu', 1, rtNone);
  VSSCoreJSObj.SetMethodInfo('EnableJavascriptItemMenu', 1, rtNone);
  VSSCoreJSObj.SetMethodInfo('InsertBreakBeforeJavascriptMenuItem', 1, rtNone);
  VSSCoreJSObj.SetMethodInfo('InsertBreakAfterJavascriptMenuItem', 1, rtNone);
  VSSCoreJSObj.SetMethodInfo('GetCursorPosition', 0, rtInteger);
  VSSCoreJSObj.SetMethodInfo('SetPluginParamValue', 3, rtNone);
  VSSCoreJSObj.SetMethodInfo('SetSubColor', 3, rtNone);
  VSSCoreJSObj.SetMethodInfo('ResetSubColor', 0, rtNone);
  VSSCoreJSObj.SetMethodInfo('SetSubIcon', 2, rtNone);

  VSSCoreJSObj.SetMethodInfo('MeasureStringWidth', 4, rtInteger);
  VSSCoreJSObj.SetMethodInfo('GetTextSelectionStart', 0, rtInteger);
  VSSCoreJSObj.SetMethodInfo('GetTextSelectionLength', 0, rtInteger);
  VSSCoreJSObj.SetMethodInfo('GetAudioCursorPosition', 0, rtInteger);

  for i := Low(FSubRangeWrapperPool) to High(FSubRangeWrapperPool) do
  begin
    JSParent.AddNativeObject(FSubRangeWrapperPool[i],
      // If we don't put a name here it will crash !!!
      Format('__FSubRangeWrapperPool[%d]', [i]));
  end;
end;

procedure TVSSCoreWrapper.SetOnSubtitleChangeStartEvent(Value : TSubtitleRangeJSWrapperChangeStartEvent);
var i : Integer;
begin
  if (@Value <> @FOnSubtitleChangeStart) then
  begin
    FOnSubtitleChangeStart := Value;
    for i := Low(FSubRangeWrapperPool) to High(FSubRangeWrapperPool) do
    begin
      FSubRangeWrapperPool[i].FOnChangeStart := Value;
    end;
  end;
end;

procedure TVSSCoreWrapper.SetOnSubtitleChangeStopEvent(Value : TSubtitleRangeJSWrapperChangeStopEvent);
var i : Integer;
begin
  if (@Value <> @FOnSubtitleChangeStop) then
  begin
    FOnSubtitleChangeStop := Value;
    for i := Low(FSubRangeWrapperPool) to High(FSubRangeWrapperPool) do
    begin
      FSubRangeWrapperPool[i].FOnChangeStop := Value;
    end;
  end;
end;

procedure TVSSCoreWrapper.SetOnSubtitleChangeTextEvent(Value : TSubtitleRangeJSWrapperChangeTextEvent);
var i : Integer;
begin
  if (@Value <> @FOnSubtitleChangeText) then
  begin
    FOnSubtitleChangeText := Value;
    for i := Low(FSubRangeWrapperPool) to High(FSubRangeWrapperPool) do
    begin
      FSubRangeWrapperPool[i].FOnChangeText := Value;
    end;
  end;
end;

procedure TVSSCoreWrapper.SetVSSCoreEngine(VSSCoreEngine : TVSSCoreEngineIntf);
begin
  FVSSCoreEngine := VSSCoreEngine;
end;

procedure TVSSCoreWrapper.RegisterJavascriptAction(AName : WideString;
  ACaption : WideString; ADefaultShortcut : WideString);
begin
  FVSSCoreEngine.RegisterJavascriptAction(AName, ACaption, ADefaultShortcut);
end;

procedure TVSSCoreWrapper.DisableJavascriptItemMenu(ACaption : WideString);
begin
 FVSSCoreEngine.DisableJavascriptItemMenu(ACaption);
end;

procedure TVSSCoreWrapper.EnableJavascriptItemMenu(ACaption : WideString);
begin
 FVSSCoreEngine.EnableJavascriptItemMenu(ACaption);
end;

procedure TVSSCoreWrapper.InsertBreakBeforeJavascriptMenuItem(ACaption : WideString);
begin
 FVSSCoreEngine.InsertBreakBeforeJavascriptMenuItem(ACaption);
end;

procedure TVSSCoreWrapper.InsertBreakAfterJavascriptMenuItem(ACaption : WideString);
begin
 FVSSCoreEngine.InsertBreakAfterJavascriptMenuItem(ACaption);
end;

function TVSSCoreWrapper.GetSubCount : Integer;
begin
  Result := FVSSCoreEngine.GetSubCount;
end;

function TVSSCoreWrapper.GetPluginParamValue(JsSection, JsParameter : WideString) : Integer;
begin
 Result :=FVSSCoreEngine.GetPluginParamValue(JsSection,JsParameter);
end;

procedure TVSSCoreWrapper.SetPluginParamValue(JsSection, JsParameter : WideString; Value : Integer);
begin
 FVSSCoreEngine.SetPluginParamValue(JsSection,JsParameter, Value)
end;

function TVSSCoreWrapper.GetCursorPosition : Integer;
begin
 Result := FVSSCoreEngine.GetCursorPosition;
end;

procedure TVSSCoreWrapper.SetSubColor(StartIndex,LastIndex : Integer; Color : WideString);
begin
 if StartIndex <= LastIndex then
  FVSSCoreEngine.SetSubColor(StartIndex,LastIndex,StrToInt(Color));
end;

procedure TVSSCoreWrapper.SetSubIcon(Index : Integer; UpdateIconIndex : Integer);
begin
  FVSSCoreEngine.SetSubIcon(Index,UpdateIconIndex);
end;

procedure TVSSCoreWrapper.ResetSubColor;
begin
 FVSSCoreEngine.ResetSubColor;
end;

function TVSSCoreWrapper.GetSubAt(Index : Integer) : TSubtitleRangeJSWrapper;
begin
  if (Index < 0) or (Index >= g_GlobalContext.SubList.Count) then
  begin
    Result := nil;
    Exit;
  end;
  Result := MakeWrappedSub(TSubtitleRange(g_GlobalContext.SubList[Index]));
end;

function TVSSCoreWrapper.GetFirst : TSubtitleRangeJSWrapper;
var SubtitleRange : TSubtitleRange;
begin
  Result := nil;
  SubtitleRange := FVSSCoreEngine.GetFirst;
  if Assigned(SubtitleRange) then
  begin
    Result := MakeWrappedSub(SubtitleRange);
  end;
end;

function TVSSCoreWrapper.GetNext(Sub : TSubtitleRangeJSWrapper) : TSubtitleRangeJSWrapper;
var SubtitleRange : TSubtitleRange;
begin
  Result := nil;
  if Assigned(Sub) and (Sub is TSubtitleRangeJSWrapper) then
  begin
    SubtitleRange := FVSSCoreEngine.GetNext(Sub.FSubtitleRange);
    if Assigned(SubtitleRange) then
    begin
      Result := MakeWrappedSub(SubtitleRange);
    end;
  end;
end;

function TVSSCoreWrapper.GetPrevious(Sub : TSubtitleRangeJSWrapper) : TSubtitleRangeJSWrapper;
var SubtitleRange : TSubtitleRange;
begin
  Result := nil;
  if Assigned(Sub) and (Sub is TSubtitleRangeJSWrapper) then
  begin
    SubtitleRange := FVSSCoreEngine.GetPrevious(Sub.FSubtitleRange);
    if Assigned(SubtitleRange) then
    begin
      Result := MakeWrappedSub(SubtitleRange);
    end;
  end;
end;

function TVSSCoreWrapper.GetSelectedCount : Integer;
begin
  Result := FVSSCoreEngine.GetSelectedCount;
end;

function TVSSCoreWrapper.GetFirstSelected : TSubtitleRangeJSWrapper;
var SubtitleRange : TSubtitleRange;
begin
  Result := nil;
  SubtitleRange := FVSSCoreEngine.GetFirstSelected;
  if Assigned(SubtitleRange) then
  begin
    Result := MakeWrappedSub(SubtitleRange);
  end;
end;

function TVSSCoreWrapper.GetNextSelected(Sub : TSubtitleRangeJSWrapper) : TSubtitleRangeJSWrapper;
var SubtitleRange : TSubtitleRange;
begin
  Result := nil;
  if Assigned(Sub) and (Sub is TSubtitleRangeJSWrapper) then
  begin
    SubtitleRange := FVSSCoreEngine.GetNextSelected(Sub.FSubtitleRange);
    if Assigned(SubtitleRange) then
    begin
      Result := MakeWrappedSub(SubtitleRange);
    end;
  end;
end;

function TVSSCoreWrapper.DeleteSubtitle(Index : Integer) : Boolean;
begin
  if (Index < 0) or (Index >= g_GlobalContext.SubList.Count) then
  begin
    Result:= False;
    Exit;
  end;
  FVSSCoreEngine.DeleteSubtitle(Index-1);
  Result:=True;
end;

function TVSSCoreWrapper.MeasureStringWidth(FontName : WideString;
  FontSize : Integer; Bold : Boolean; Text : WideString) : Integer;
var
  DC : HDC;
  xysize : Size;
  FontLOG : LOGFONT;
  Font, OldFont : HFONT;
begin
  DC := CreateDC('DISPLAY', nil, nil, nil);
  ZeroMemory(@FontLOG, SizeOf(FontLOG));
  StrCopy(FontLOG.lfFaceName, PAnsiChar(String(FontName)));
  FontLOG.lfHeight := -MulDiv(FontSize, GetDeviceCaps(DC, LOGPIXELSY), 72);
  if (Bold) then
    FontLOG.lfWeight := FW_BOLD;
  Font := CreateFontIndirect(FontLOG);
  OldFont := SelectObject(DC, Font);
  GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), xysize);
  SelectObject(DC, OldFont);
  DeleteDC(DC);
  DeleteObject(Font);
  Result := xysize.cx;
end;

//==============================================================================

function TVSSCoreWrapper.GetTextSelectionStart : Integer;
begin
  Result := FVSSCoreEngine.GetTextSelectionStart;
end;

function TVSSCoreWrapper.GetTextSelectionLength : Integer;
begin
  Result := FVSSCoreEngine.GetTextSelectionLength;
end;

function TVSSCoreWrapper.GetAudioCursorPosition : Integer;
begin
  Result := FVSSCoreEngine.GetAudioCursorPosition;
end;

initialization
  g_SceneChangeWrapper := TSceneChangeWrapper.Create;
  g_VSSCoreWrapper := TVSSCoreWrapper.Create;

finalization
  FreeAndNil(g_VSSCoreWrapper);
  FreeAndNil(g_SceneChangeWrapper);

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
