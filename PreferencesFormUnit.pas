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

unit PreferencesFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, CheckLst,
  TntCheckLst, IniFiles, ExtCtrls, TntExtCtrls, TntActnList, Menus,
  WAVDisplayerUnit, VirtualTrees, JavaScriptPluginUnit,
  Buttons, TntButtons;

const
  WM_ENDEDITING = WM_USER + 736;

type
  PParamData = ^TParamData;
  TParamData = record
    Param : PJSPluginParam;
    ValueAsString : WideString;
    Changed: Boolean;
  end;

  TJSPluginInfo = class
    Enabled : Boolean;
    Name : WideString;
    Description : WideString;
    Msg: WideString;
    Color : TColor;
    ParamList : TList;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Value : TJSPluginInfo);
    procedure ClearParam;
    function GetParamByName(Name : WideString) : PParamData;
  end;

type
  TDefaultActionShortcut = record
    ActionName : string;
    ShortCut : string;
  end;

type
  TConfigObject = class
  private
    procedure ClearJSPluginInfoList;
  public
    // Web server
    ServerPort : Integer;
    EnableCompression : Boolean;
    // Misc
    SwapSubtitlesList : Boolean;
    DisableSubtitleEdition : Boolean;
    EnableToggleCreation : Boolean;
    EnableMouseAntiOverlapping : Boolean;
    EnableMouseSnapping : Boolean;
    SpaceKeyModifyTiming : Boolean;
    SpaceKeyCPSTarget : Integer;
    SpaceKeyMinimalDuration : Integer;
    SpaceKeyBlankBetweenSubtitles : Integer;
    MaximumSubtitleDuration : Integer;
    LoadMostRecentProjectOnStartup : Boolean;
    DictionaryOnLineEnabled : Boolean;
    DictionaryOnLineExcludeTextPipe : Boolean;
    QuickSmartSilentZonePanels : Boolean;
    UseInternalFilters : Boolean;
    DoNotUseInternalFiltersOnCreateProject : Boolean;
    UseDefaultCodecOnCreateProject : Boolean;
    UseAlternativeCodecOnCreateProject : Boolean;
    ForceStopOnPausing : Boolean;
    DoubledSubResolution : Boolean;
    ForceNoDesktopComposition : Boolean;
    PreferLegacyVideoRenderer : Boolean;
    PreferVmr7VideoRenderer : Boolean;
    PreferVmr9VideoRenderer : Boolean;
    UseReclockAudioRenderer : Boolean;
    SpaceKeyMoveAfterSmartButtonIsUsed : Integer;
    AutoPlaySmartButtonIsUsed : Boolean;
    AutoScrollWavDisplay : Boolean;
    AutoScrollSubtitles : Boolean;
    SilentZoneThreshold : Integer;
    SilentZoneDuration: Integer;
    SilentZoneMinimunBetweenZones: Integer;
    UseDefaultInternalCodec : boolean;
    UseAlternativeInternalCodec : boolean;
    LavVHwNone : Boolean;
    LavVQuickSync : Boolean;
    LavVCuda : Boolean;
    LavVDxva2 : Boolean;
    EnableLavAudioMixing : boolean;
    LavPreferredLanguages : String;
    ForceRegisteredCodecs : boolean;
    CheckUpdates : Boolean;
    OnLineSearchDict1 : Boolean;
    WordReferenceLanguage : String;
    OnLineSearchDict2 : Boolean;
    OnLineSearchDict3 : Boolean;
    OnLineSearchDict4 : Boolean;
    OnLineSearchDict5 : Boolean;
    LinkDictionaries : WideString;
    FlippedPicture : Boolean;
    FlippedSubtitles : Boolean;
    AccentsAssistant : Boolean;
    UTF8AsDefault : Boolean;
    KeepDurationIntact : Boolean;

    CustomText : WideString;

    WavColors : TWavColors;
    WavColorsTemp : TWavColors;

    // desynch tools
    DesynchToolsAutoReset : boolean;
    DesynchUseColorsForSubtitles : boolean;
    DesynchUseIconsForSubtitles : boolean;
    DesynchAssumeFirstSubtitleSynched : boolean;
    DesynchLog : boolean;

    // Hotkeys
    ListHotkeys : TList;
    ListDefaultHotkeys : TList;
    // Mouse
    MouseWheelTimeScrollModifier : TMouseWheelModifier;
    MouseWheelVZoomModifier : TMouseWheelModifier;
    MouseWheelHZoomModifier : TMouseWheelModifier;
    MouseEnableSSATimingMode : Boolean;
    // Backup
    EnableBackup : Boolean;
    AutoBackupEvery : Integer;
    AutoSaveWhenPlaying : Boolean;
    // Plugins
    ListJSPlugin : TList;
    // Fonts
    SubListFont : string;
    SubTextFont : string;
    SubVideoFont : string;
    TransparencySubVideoFont : Integer;
    OutlineSubVideoFont : Integer;
    OutlineSubVideoColorFont : TColor;
    TransparencyOutlineSubVideoFont : Integer;
    ShadowSubVideoFont : Integer;
    ShadowSubVideoColorFont : TColor;
    TransparencyShadowSubVideoFont : Integer;
    // WAV Display
    ShowSceneChange : Boolean;
    SceneChangeStartOffset : Integer;
    SceneChangeStopOffset : Integer;
    SceneChangeFilterOffset : Integer;
    ShowTextInWAVDisplay : Boolean;
    ShowSpecialTagSubs : Boolean;
    SwapSubVO : Boolean;
    DisableVOShowTextInWAVDisplay : Boolean;
    EnableShowVideoForNewProject : Boolean;
    EnableDetachedVideoForNewProject : Boolean;
    //Itasa
    Username : String;
    Passwd : String;
    Msg : TStringList;
    ZipPath: String;
    ZipPathOnlyForCP:Boolean;
    //Default Path
    DefaultPath: WideString;
    ForceDefaultPath:Boolean;
    // Mod string
    ModString : WideString;
    // enhanced Flag
    IsEnhanced : Boolean;
    // universal app enviroment
    IsUniversalAppEnviroment : Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure SetDefault;
    procedure SaveIni(IniFile : TIniFile);
    procedure LoadIni(IniFile : TIniFile; IsPresets : Boolean);
    procedure SetDefaultHotKeys(ActionList : TTntActionList);
    procedure UpdatePluginList;
    function GetJSPluginInfoByName(Name : WideString) : TJSPluginInfo;
    procedure ApplyParam(JSPlugin : TJavaScriptPlugin);
  end;

  THotkeyListItemData = class
    Action : TTntAction;
    NormalShortCut : TShortCut;
    TimingShortCut : TShortCut;
    procedure Assign(Source : THotkeyListItemData);
  end;

  TPreferencesForm = class(TForm)
    PageControlPreferences: TTntPageControl;
    tsGeneral: TTntTabSheet;
    tsErrorChecking: TTntTabSheet;
    GroupBoxWebServer: TTntGroupBox;
    EditServerPort: TTntEdit;
    TntLabel1: TTntLabel;
    ListErrorChecking: TTntCheckListBox;
    UpDownServerPort: TTntUpDown;
    GroupBoxMisc: TGroupBox;
    chkAssociateExtVSSPRJ: TCheckBox;
    chkSwapSubList: TCheckBox;
    tsHotKeys: TTntTabSheet;
    ListHotkeys: TTntListView;
    tsMouse: TTntTabSheet;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboWheelTimeScrollModifier: TComboBox;
    ComboWheelVZoomModifier: TComboBox;
    ComboWheelHZoomModifier: TComboBox;
    GroupBox3: TGroupBox;
    chkEnableSSATimingMode: TCheckBox;
    GroupBoxBackup: TGroupBox;
    chkCreateBackup: TCheckBox;
    bttOpenBackupDir: TButton;
    EditBackupTime: TEdit;
    Label6: TLabel;
    UpDownBackupTime: TUpDown;
    Panel1: TPanel;
    Panel2: TPanel;
    TntLabel3: TTntLabel;
    lbErrorDescription: TTntLabel;
    TntLabel4: TTntLabel;
    ShapeErrorColor: TShape;
    Bevel1: TBevel;
    ListPluginParam: TVirtualStringTree;
    tsFonts: TTntTabSheet;
    FontDialog1: TFontDialog;
    TntGroupBox2: TTntGroupBox;
    EditSubListFont: TTntEdit;
    bttSubListFont: TTntButton;
    TntGroupBox3: TTntGroupBox;
    EditSubTextFont: TTntEdit;
    bttSubTextFont: TTntButton;
    chkAutoSaveWhenPlaying: TCheckBox;
    chkEnableMouseAntiOverlapping: TCheckBox;
    chkEnableMouseSnapping: TCheckBox;
    tsTimingMode: TTntTabSheet;
    TntGroupBox4: TTntGroupBox;
    chkEnableSubCreationWithSpaceKey: TCheckBox;
    chkSpaceKeyModifyTiming: TCheckBox;
    chkDisableSubEditionInTimingMode: TCheckBox;
    bttOpenBackupTempDir: TButton;
    tsWAVDisplay: TTntTabSheet;
    TntGroupBox5: TTntGroupBox;
    chkSceneChange: TCheckBox;
    EditSCStartOffset: TEdit;
    UpDownSCStart: TTntUpDown;
    EditSCStopOffset: TEdit;
    UpDownSCStop: TTntUpDown;
    TntGroupBox6: TTntGroupBox;
    chkShowTextInWAVDisplay: TCheckBox;
    TntLabel9: TTntLabel;
    TntLabel10: TTntLabel;
    TntLabel11: TTntLabel;
    TntLabel12: TTntLabel;
    EditSCFilterOffset: TEdit;
    UpDownSCFilter: TTntUpDown;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    tsSubtitle: TTntTabSheet;
    TntGroupBox1: TTntGroupBox;
    TntLabel8: TTntLabel;
    EditBlankBetweenSub: TTntEdit;
    UpDownBlankBetweenSub: TTntUpDown;
    TntLabel13: TTntLabel;
    TntLabel2: TTntLabel;
    EditCPSTarget: TTntEdit;
    UpDownCPSTarget: TTntUpDown;
    TntLabel7: TTntLabel;
    EditMinimalDuration: TTntEdit;
    UpDownMinimalDuration: TTntUpDown;
    pmErrorChecking: TPopupMenu;
    pmiSelectAll: TMenuItem;
    pmiUnselectAll: TMenuItem;
    PanelHotKey: TPanel;
    TntLabel5: TTntLabel;
    ComboHotkeyMode: TTntComboBox;
    HotKey1: THotKey;
    TntLabel6: TTntLabel;
    Bevel9: TBevel;
    TntPanel1: TTntPanel;
    bttOk: TTntButton;
    bttCancel: TTntButton;
    EditMaximalDuration: TTntEdit;
    UpDownMaximumDuration: TTntUpDown;
    TntLabel14: TTntLabel;
    chkAssociateExtSRT: TCheckBox;
    chkAssociateExtSSA: TCheckBox;
    chkAssociateExtASS: TCheckBox;
    chkLoadMostRecentProjectOnStartup: TCheckBox;
    ChkQuickSmartSilentZonePanels: TCheckBox;
    chkDisableVOShowTextInWAVDisplay: TCheckBox;
    TntLabelMoveAfterSmartButtonIsUsed: TTntLabel;
    EditMoveAfterSmartButtonIsUsed: TTntEdit;
    UpDownMoveAfterSmartButtonIsUsed: TTntUpDown;
    ChkMoveAfterSmartButtonIsUsed: TCheckBox;
    tsDesynchTools: TTntTabSheet;
    TntGroupBox7: TTntGroupBox;
    ChkDesynchToolsAutoReset: TCheckBox;
    ChkDesynchUseColorsForSubtitles: TCheckBox;
    ChkDesynchUseIconsForSubtitles: TCheckBox;
    ChkDesynchAssumeFirstSubtitleSynched: TCheckBox;
    ChkDesynchLog: TCheckBox;
    chkEnableCompression: TCheckBox;
    tsItasa: TTntTabSheet;
    TntGroupBox8: TTntGroupBox;
    TntLabel15: TTntLabel;
    TntLabel16: TTntLabel;
    TntEditUsername: TTntEdit;
    TntEditPasswd: TTntEdit;
    TntLabel17: TTntLabel;
    TntSpeedButtonCheckLogin: TTntSpeedButton;
    ImgLoginKO: TTntImage;
    ImgLoginOK: TTntImage;
    chkShowSpecialTagSubs: TCheckBox;
    ChkCustomText: TTntCheckBox;
    EditCustomText: TTntEdit;
    Bevel10: TBevel;
    TntGroupBox9: TTntGroupBox;
    TntLabel19: TTntLabel;
    ColorDialog1: TColorDialog;
    ShapeWC: TTntShape;
    TntLabel20: TTntLabel;
    ShapeWBC: TTntShape;
    TntLabel21: TTntLabel;
    ShapeCC: TTntShape;
    TntLabel22: TTntLabel;
    ShapeRC1: TTntShape;
    TntLabel23: TTntLabel;
    ShapeRC2: TTntShape;
    TntLabel24: TTntLabel;
    ShapeRCNE: TTntShape;
    ButtonDefaultWavColours: TTntButton;
    chkSwapSubVO: TCheckBox;
    TsPlayback: TTntTabSheet;
    TntGroupBoxPlaybackOptions: TTntGroupBox;
    ChkUseInternalFilters: TCheckBox;
    ChkDefaultInternalCodec: TRadioButton;
    ChkAlternativeInternalCodec: TRadioButton;
    GroupBoxLavVideoHwModes: TGroupBox;
    ChkLavVQuickSync: TRadioButton;
    ChkLavVCuda: TRadioButton;
    ChkLavVDxva2: TRadioButton;
    ChkLavVHwNone: TRadioButton;
    ChkDoubledSubResolution: TCheckBox;
    ChkDisableDesktopComposition: TCheckBox;
    ChkForceStopOnPausing: TCheckBox;
    ChkUseReclockAudioRenderer: TCheckBox;
    labelMessagePlaybackAvailable: TLabel;
    GroupBoxCreateProject: TGroupBox;
    ChkDoNotUseInternalFiltersOnCreateProject: TCheckBox;
    ChkUseDefaultCodecOnCreateProject: TCheckBox;
    ChkUseAlternativeCodecOnCreateProject: TCheckBox;
    ChkEnableLavAudioMixing: TCheckBox;
    ChkDefaultPath: TTntCheckBox;
    LblDefaultPath: TTntLabel;
    ChkForceDefaultPath: TTntCheckBox;
    ChkForceRegisteredCodecs: TCheckBox;
    EditLavPreferredLanguages: TLabeledEdit;
    chkEnableShowVideoForNewProject: TCheckBox;
    chkEnableDetachedVideoForNewProject: TCheckBox;
    TntGroupBox10: TTntGroupBox;
    EditSubVideoFont: TTntEdit;
    bttSubVideoFont: TTntButton;
    TntLabel25: TTntLabel;
    EditOutlineSubVideoFont: TEdit;
    UpDownSubVideoOutline: TTntUpDown;
    TntLabel26: TTntLabel;
    EditShadowSubVideoFont: TEdit;
    UpDownSubVideoShadow: TTntUpDown;
    ShapeSubVideoOutline: TTntShape;
    ShapeSubVideoShadow: TTntShape;
    ButtonDefaultSubtitlesVideo: TTntButton;
    TntLabel27: TTntLabel;
    TrackBarTransparencySubVideo: TTntTrackBar;
    TrackBarTransparencyOutline: TTntTrackBar;
    TrackBarTransparencyShadow: TTntTrackBar;
    tsDictionary: TTntTabSheet;
    TntGroupBoxOnlineDictSearch: TTntGroupBox;
    chkOnLineSearchDict1: TCheckBox;
    chkOnLineSearchDict2: TCheckBox;
    chkOnLineSearchDict3: TCheckBox;
    chkOnLineSearchDict4: TCheckBox;
    chkDictionaryOnLineEnabled: TCheckBox;
    chkDictionaryOnLineExcludeTextPipe: TCheckBox;
    chkOnLineSearchDict5: TCheckBox;
    chkFlippedPicture: TCheckBox;
    chkFlippedSubtitles: TCheckBox;
    lbledtLinkDictionaries: TLabeledEdit;
    GroupBoxRenderer: TGroupBox;
    ChkPreferVmr9VideoRenderer: TRadioButton;
    ChkPreferVmr7VideoRenderer: TRadioButton;
    ChkPreferLegacyVideoRenderer: TRadioButton;
    cbbWordReferenceLanguage: TComboBox;
    chkUTF8AsDefault: TCheckBox;
    TimerCheckCodec: TTimer;
    bttSetHotkey: TButton;
    bttClearHotkey: TButton;
    bttResetAllHotkeys: TButton;
    ChkUpdates: TCheckBox;
    chkAccentsAssistant: TCheckBox;
    ItaliansubsOptionsGroupBox: TGroupBox;
    TntLabel18: TTntLabel;
    TntMemoMsg: TTntMemo;
    ButtonBold: TTntButton;
    ButtonItalic: TTntButton;
    ButtonUnderline: TTntButton;
    TntMemo1: TTntMemo;
    ChkZipPath: TCheckBox;
    LabelZipPath: TTntLabel;
    ChkZipOnlyCP: TCheckBox;
    procedure bttOkClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure ListErrorCheckingClick(Sender: TObject);
    procedure chkAssociateExtVSSPRJClick(Sender: TObject);
    procedure ListHotkeysSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ComboHotkeyModeSelect(Sender: TObject);
    procedure ListHotkeysDeletion(Sender: TObject; Item: TListItem);
    procedure bttOpenBackupDirClick(Sender: TObject);
    procedure ListPluginParamGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ListPluginParamEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ListPluginParamCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure ListErrorCheckingClickCheck(Sender: TObject);
    procedure bttSubListFontClick(Sender: TObject);
    procedure bttSubTextFontClick(Sender: TObject);
    procedure bttOpenBackupTempDirClick(Sender: TObject);
    procedure ListPluginParamGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure pmiSelectAllClick(Sender: TObject);
    procedure pmiUnselectAllClick(Sender: TObject);
    procedure chkAssociateExtSRTClick(Sender: TObject);
    procedure chkAssociateExtSSAClick(Sender: TObject);
    procedure chkAssociateExtASSClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ChkQuickSmartSilentZonePanelsClick(Sender: TObject);
    procedure chkDictionaryOnLineEnabledClick(Sender: TObject);
    procedure ChkUseInternalFiltersClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ChkDefaultInternalCodecClick(Sender: TObject);
    procedure ChkAlternativeInternalCodecClick(Sender: TObject);
    procedure TntSpeedButtonCheckLoginClick(Sender: TObject);
    procedure AddTag(var Msg: string; tag:string; ids, len:Integer);
    procedure ButtonBoldClick(Sender: TObject);
    procedure ButtonItalicClick(Sender: TObject);
    procedure ButtonUnderlineClick(Sender: TObject);
    procedure ChkCustomTextClick(Sender: TObject);
    procedure ShapeWCMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonDefaultWavColoursClick(Sender: TObject);
    procedure LabelZipPathClick(Sender: TObject);
    procedure LabelZipPathMouseEnter(Sender: TObject);
    procedure LabelZipPathMouseLeave(Sender: TObject);
    procedure ChkZipPathClick(Sender: TObject);
    procedure ChkDoNotUseInternalFiltersOnCreateProjectClick(
      Sender: TObject);
    procedure ChkUseDefaultCodecOnCreateProjectClick(Sender: TObject);
    procedure ChkUseAlternativeCodecOnCreateProjectClick(Sender: TObject);
    procedure bttResetAllHotkeysClick(Sender: TObject);
    procedure bttSetHotkeyClick(Sender: TObject);
    procedure bttClearHotkeyClick(Sender: TObject);
    procedure ChkDefaultPathClick(Sender: TObject);
    procedure LblDefaultPathClick(Sender: TObject);
    procedure bttSubVideoFontClick(Sender: TObject);
    procedure ShapeSubVideoOutlineMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeSubVideoShadowMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ButtonDefaultSubtitlesVideoClick(Sender: TObject);
    procedure chkOnLineSearchDict1Click(Sender: TObject);
    procedure TimerCheckCodecTimer(Sender: TObject);
  private
    { Private declarations }
    ListDefaultHotkeys : TList;

    LavVideoQuickSyncModeAvaiable : Boolean;
    LavVideoCudaAvaiable : Boolean;
    LavVideoDxva2Avaiable : Boolean;

    function GetCurrentModeShortCut(HLID : THotkeyListItemData) : TShortCut;
    function GetCurrentModeShortCutFromList : TShortCut;
    procedure SetCurrentModeShortCut(HLID : THotkeyListItemData; ShortCut : TShortCut);
    procedure SetCurrentModeShortCutFromList(ShortCut : TShortCut);
    procedure WMEndEditing(var Message: TMessage); message WM_ENDEDITING;
    procedure ClearErrorList;
    procedure UpdateAssociationCheckBoxes;
    function CheckRegistryAccess : Boolean;
    procedure UpdateLavVideoCodecHwModeSupported;
  public
    { Public declarations }
    procedure LoadConfig(Config : TConfigObject);
    procedure SaveConfig(Config : TConfigObject);
    procedure TempSaveWavColors(Config : TConfigObject);
  end;

  TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEditControl: TWinControl; // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditIntegerKeyPress(Sender: TObject; var Key: Char);
    procedure EditFloatKeyPress(Sender: TObject; var Key: Char);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  function PreferencesFormInstance : TPreferencesForm;

implementation

uses MiscToolsUnit, GlobalUnit, ActnList, TntWindows, TntSysUtils,
  TntForms, Mask, TntClasses, LogWindowFormUnit, FileCtrl,
  main, Renderer, HTTPSend, DsiWin32, Md5;

{$R *.dfm}

const
  DefaultTimingShortcuts : array[0..5] of TDefaultActionShortcut = (
    (ActionName: 'ActionStop'; ShortCut: 'Esc'),
    (ActionName: 'ActionPlay'; ShortCut: 'F1'),
    (ActionName: 'ActionShowHideVideo'; ShortCut: 'F4'),
    (ActionName: 'ActionSave'; ShortCut: 'Ctrl+S'),
    (ActionName: 'ActionUndo'; ShortCut: 'Ctrl+Z'),
    (ActionName: 'ActionRedo'; ShortCut: 'Ctrl+Y')
  );

const
  VSSPRJ_ICON_INDEX = 1;
  SRT_ICON_INDEX = 2;
  SSA_ICON_INDEX = 2;
  ASS_ICON_INDEX = 2;

const
  ZIP_PATH = 'Select path...';
  DEF_PATH = 'Select path...';

var
  PreferencesForm: TPreferencesForm = nil;

function ASCIIEncrypt(Data, Cipher : String) : String;
var
  I, Z : integer;
  C : char;
  Code : byte;
begin
  Result:= '';
  if length(Data) > 0 then begin
    Z:= length(Cipher);
    for I:= 1 to length(Data) Do begin
      Code:= ord(Cipher[(I - 1) mod Z + 1]);
      if Data[I] >= #128 then
        C:= Chr(ord(Data[I]) xor (Code and $7F))
      else if Data[I] >= #64 then
        C:= Chr(ord(Data[I]) xor (Code and $3F))
      else if Data[I] >= #32 then
        C:= Chr(ord(Data[I]) xor (Code and $1F))
      else
        C:= Data[I];
      Result:= Result + C;
      end;
    end;
  //ShowMessage('>'+Data+'<'+#10#13+#10#13+'>'+Result+'<');
end;

{$IFNDEF enhanced}
function LocalLavCodecInstalled() : Boolean;
var
  resultMd5CalculationSplitter, resultMd5CalculationAudio, resultMd5CalculationVideo : String;
begin
 resultMd5CalculationSplitter := '';
 if FileExists(ExtractFilePath(ParamStr(0))+'CodecLav\LAVSplitter.ax') then
   resultMd5CalculationSplitter := uppercase(FileMD5Digest(ExtractFilePath(ParamStr(0))+'CodecLav\LAVSplitter.ax'));

 resultMd5CalculationAudio := '';
 if FileExists(ExtractFilePath(ParamStr(0))+'CodecLav\LAVAudio.ax') then
  resultMd5CalculationAudio := uppercase(FileMD5Digest(ExtractFilePath(ParamStr(0))+'CodecLav\LAVAudio.ax'));

 resultMd5CalculationVideo := '';
 if FileExists(ExtractFilePath(ParamStr(0))+'CodecLav\LAVVideo.ax') then
   resultMd5CalculationVideo := uppercase(FileMD5Digest(ExtractFilePath(ParamStr(0))+'CodecLav\LAVVideo.ax'));

 if (resultMd5CalculationSplitter <> 'E6C19B478CB5BD10ADDDD398786AF50C') OR
    (resultMd5CalculationAudio <> '9150D2133CE2A4127A2ECC4A2B3D2409') OR
    (resultMd5CalculationVideo <> '3AB5B08E59EDEC18719FA48276146A0B') then
      Result := False
 else
      Result := True;
end;
{$ELSE}
function LocalLavCodecInstalled() : Boolean;
begin
  Result := True;
end;
{$ENDIF}

// =============================================================================

function PreferencesFormInstance : TPreferencesForm;
begin
  if (PreferencesForm = nil) then
    Application.CreateForm(TPreferencesForm, PreferencesForm);
  Result := PreferencesForm;
end;

// =============================================================================

procedure THotkeyListItemData.Assign(Source : THotkeyListItemData);
begin
  Action := Source.Action;
  NormalShortCut := Source.NormalShortCut;
  TimingShortCut := Source.TimingShortCut;
end;

// =============================================================================

constructor TJSPluginInfo.Create;
begin
  ParamList := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TJSPluginInfo.Destroy;
begin
  ClearParam;
  ParamList.Free;
end;

//------------------------------------------------------------------------------

procedure TJSPluginInfo.ClearParam;
var i : Integer;
begin
  for i:=0 to ParamList.Count-1 do
    Dispose(PJSPluginParam(ParamList[i]));
  ParamList.Clear;
end;

//------------------------------------------------------------------------------

procedure TJSPluginInfo.Assign(Value : TJSPluginInfo);
var i : Integer;
    pParamSrc, pParamDst : PJSPluginParam;
begin
  Self.Enabled := Value.Enabled;
  Self.Name := Value.Name;
  Self.Description := Value.Description;
  Self.Msg := Value.Msg;
  Self.Color := Value.Color;
  ClearParam;
  for i:=0 to Value.ParamList.Count-1 do
  begin
    pParamSrc := Value.ParamList[i];
    pParamDst := New(PJSPluginParam);
    pParamDst^ := pParamSrc^;
    Self.ParamList.Add(pParamDst);
  end;
end;

//------------------------------------------------------------------------------

function TJSPluginInfo.GetParamByName(Name : WideString) : PParamData;
var i : Integer;
begin
  Result := nil;
  for i:=0 to ParamList.Count-1 do
  begin
    if PJSPluginParam(ParamList[i]).Name = Name then
    begin
      Result := ParamList[i];
      Exit;
    end;
  end;
end;

// =============================================================================

constructor TConfigObject.Create;
begin
  ListHotkeys := TList.Create;
  ListDefaultHotkeys := TList.Create;
  ListJSPlugin := TList.Create;
  SetDefault;
end;

//------------------------------------------------------------------------------

destructor TConfigObject.Destroy;
var i : integer;
begin
  for i:= 0 to ListHotkeys.Count-1 do
    THotkeyListItemData(ListHotkeys[i]).Free;
  ListHotkeys.Free;

  for i:= 0 to ListDefaultHotkeys.Count-1 do
    THotkeyListItemData(ListDefaultHotkeys[i]).Free;
  ListDefaultHotkeys.Free;

  ClearJSPluginInfoList;
  ListJSPlugin.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TConfigObject.GetJSPluginInfoByName(Name : WideString) : TJSPluginInfo;
var i : Integer;
begin
  Result := nil;
  for i:=0 to ListJSPlugin.Count-1 do
  begin
    if TJSPluginInfo(ListJSPlugin[i]).Name = Name then
    begin
      Result := ListJSPlugin[i];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.ApplyParam(JSPlugin : TJavaScriptPlugin);
var JSPluginInfo : TJSPluginInfo;
    pParam : PJSPluginParam;
    i : Integer;
begin
  JSPluginInfo := GetJSPluginInfoByName(JSPlugin.Name);
  if Assigned(JSPluginInfo) then
  begin
    for i:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pParam := JSPluginInfo.ParamList[i];
      JSPlugin.SetParamValue(pParam.Name, GetParamValueAsWideString(pParam));
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SetDefault;
begin
  // Misc
  SwapSubtitlesList := False;
  DisableSubtitleEdition := True;
  EnableToggleCreation := True;
  EnableMouseAntiOverlapping := False;
  EnableMouseSnapping := True;
  SpaceKeyModifyTiming := True;
  SpaceKeyCPSTarget := 18;
  SpaceKeyMinimalDuration := 1000;
  SpaceKeyBlankBetweenSubtitles := 120;
  MaximumSubtitleDuration := 6000;
  LoadMostRecentProjectOnStartup := False;
  DictionaryOnLineEnabled := True;
  DictionaryOnLineExcludeTextPipe := False;
  QuickSmartSilentZonePanels := True;
  UseInternalFilters := True;
  DoNotUseInternalFiltersOnCreateProject := false;
  UseDefaultCodecOnCreateProject := false;
  UseAlternativeCodecOnCreateProject := true;
  ForceStopOnPausing := false;
  DoubledSubResolution := true;
  ForceNoDesktopComposition := false;
  PreferVmr7VideoRenderer := True;
  PreferVmr9VideoRenderer := False;
  PreferLegacyVideoRenderer := False;
  UseReclockAudioRenderer := False;
  SpaceKeyMoveAfterSmartButtonIsUsed := 750;
  AutoPlaySmartButtonIsUsed := True;
  DesynchToolsAutoReset := True;
  DesynchUseColorsForSubtitles := True;
  DesynchUseIconsForSubtitles := True;
  DesynchAssumeFirstSubtitleSynched := True;
  DesynchLog := False;
  AutoScrollWavDisplay := True;
  AutoScrollSubtitles := False;
  SilentZoneThreshold := 100;
  SilentZoneDuration := 500;
  SilentZoneMinimunBetweenZones := 30;
  UseDefaultInternalCodec := False;
  UseAlternativeInternalCodec := True;
  LavVHwNone := true;
  LavVQuickSync := false;
  LavVCuda := false;
  LavVDxva2 := false;
  EnableLavAudioMixing := false;
  LavPreferredLanguages := 'eng,ita,fra,deu';
  ForceRegisteredCodecs := false;
  CheckUpdates := false;
  AccentsAssistant := false;
  if GetLayoutShortName = 'IT' then AccentsAssistant := True;
  OnLineSearchDict1 := true;
  WordReferenceLanguage := 'ENIT (English-Italian)';
  OnLineSearchDict2 := false;
  OnLineSearchDict3 := false;
  OnLineSearchDict4 := false;
  OnLineSearchDict5 := false;
  LinkDictionaries := 'http://extensions.openoffice.org/';
  CustomText := '';
  FlippedPicture := false;
  FlippedSubtitles := false;
  UTF8AsDefault := false;
  KeepDurationIntact := False;
  if AccentsAssistant = True then UTF8AsDefault := True;

  //Default Wav Colors
  WavColors.WAV_COLOR := $00A7F24A;
  WavColors.WAV_BACK_COLOR := clBlack;
  WavColors.RANGE_COLOR_1 := $003333FF;
  WavColors.RANGE_COLOR_2 := $00FF8000;
  WavColors.RANGE_COLOR_NOT_EDITABLE := $00C0C0C0;
  WavColors.CURSOR_COLOR := clYellow;

  // Web server
  ServerPort := 80;
  EnableCompression := False; // Some IE version doesn't support deflate but say they does :p
  // Error
  UpdatePluginList;
  // Mouse
  MouseWheelTimeScrollModifier := mwmCtrl;
  MouseWheelVZoomModifier := mwmShift;
  MouseWheelHZoomModifier := mwmNone;
  MouseEnableSSATimingMode := False;
  // Backup
  EnableBackup := True;
  AutoBackupEvery := 0;
  AutoSaveWhenPlaying := False;
  // Fonts
  SubListFont := 'Arial,8,0,0,clWindowText';
  SubTextFont := 'Arial,10,1,0,clWindowText';
  SubVideoFont := 'Arial,18,1,0,clWhite';
  TransparencySubVideoFont := 0;
  OutlineSubVideoFont := 2;
  OutlineSubVideoColorFont := clBlack;
  TransparencyOutlineSubVideoFont := 0;
  ShadowSubVideoFont := 3;
  ShadowSubVideoColorFont := clBlack;
  TransparencyShadowSubVideoFont := 4;

  // WAV Display
  ShowSceneChange := False;
  SceneChangeStartOffset := 130;
  SceneChangeStopOffset := 130;
  ShowTextInWAVDisplay := True;
  ShowSpecialTagSubs := True;
  SwapSubVO := False;
  DisableVOShowTextInWAVDisplay := False;
  SceneChangeFilterOffset := 250;
  EnableShowVideoForNewProject := True;
  EnableDetachedVideoForNewProject := False;
  // Itasa
  Username := 'Your Username';
  Passwd := 'Your Password';
  if not Assigned(Msg) then
    Msg := TStringList.Create
  else
    Msg.Clear;
  Msg.Add('In allegato il file :');
  Msg.Add('');
  Msg.Add('[b]#sub1#[/b]');
  Msg.Add('');
  Msg.Add('Versione file video:');
  Msg.Add('');
  Msg.Add('[b]#video#[/b]');
  Msg.Add('');
  Msg.Add(':ciao:');
  Msg.Add('');
  Msg.Add('**spedito da VisualSubSync for Italiansubs');

  ZipPath:=ZIP_PATH;
  ZipPathOnlyForCP:=False;

  //Default Path
  DefaultPath:=DEF_PATH;
  ForceDefaultPath:=False;

  ModString := 'for Italiansubs';

  {$IFNDEF enhanced}
  IsEnhanced := False;
  IsUniversalAppEnviroment := False;
  {$ELSE}
  IsEnhanced := True;
  IsUniversalAppEnviroment := False;
  if Pos('windowsapp', lowercase(g_ApplicationPath)) > 0 then
    IsUniversalAppEnviroment := True;
  {$ENDIF}

end;

//------------------------------------------------------------------------------

procedure TConfigObject.UpdatePluginList;
var JSPEnum : TJavaScriptPluginEnumerator;
    JSPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
begin
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  while JSPEnum.GetNext(JSPlugin) do
  begin
    JSPluginInfo := TJSPluginInfo.Create;
    JSPluginInfo.Enabled := True;
    JSPluginInfo.Name := JSPlugin.Name;
    JSPluginInfo.Description := JSPlugin.Description;
    JSPluginInfo.Color := JSColorToTColor(JSPlugin.Color);
    JSPluginInfo.Msg := JSPlugin.Msg;
    JSPlugin.FillParamList(JSPluginInfo.ParamList);
    ListJSPlugin.Add(JSPluginInfo);
    FreeAndNil(JSPlugin);
  end;
  JSPEnum.Free;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.ClearJSPluginInfoList;
var i : Integer;
begin
  for i:=0 to ListJSPlugin.Count-1 do
    TJSPluginInfo(ListJSPlugin[i]).Free;
  ListJSPlugin.Clear;
end;

//------------------------------------------------------------------------------

function GetDefaultShortcut(ActionName : string; ArraySC : array of TDefaultActionShortcut) : TShortCut;
var i : integer;
begin
  Result := 0;
  for i:=0 to Length(ArraySC)-1 do
  begin
    if (ActionName = ArraySC[i].ActionName) then
    begin
      Result := TextToShortCut(ArraySC[i].ShortCut);
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SetDefaultHotKeys(ActionList : TTntActionList);
var i : integer;
    Action : TTntAction;
    HLID : THotkeyListItemData;
    SC : TShortCut;
begin
  for i:=0 to ActionList.ActionCount-1 do
  begin
    Action := TTntAction(ActionList.Actions[i]);
    if (Action.Tag = 1) then
    begin
      SC := GetDefaultShortcut(Action.Name, DefaultTimingShortcuts);

      HLID := THotkeyListItemData.Create;
      HLID.Action := Action;
      HLID.NormalShortCut := Action.ShortCut;
      HLID.TimingShortCut := SC;
      ListHotkeys.Add(HLID);

      HLID := THotkeyListItemData.Create;
      HLID.Action := Action;
      HLID.NormalShortCut := Action.ShortCut;
      HLID.TimingShortCut := SC;
      ListDefaultHotkeys.Add(HLID);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SaveIni(IniFile : TIniFile);
var i, j : integer;
    HLID : THotkeyListItemData;
    JSPluginInfo : TJSPluginInfo;
    pPluginParam : PJSPluginParam;
begin
  // Misc
  IniFile.WriteBool('Misc','SwapSubtitlesList',SwapSubtitlesList);
  IniFile.WriteBool('Misc','DisableSubtitleEdition',DisableSubtitleEdition);
  IniFile.WriteBool('Misc','EnableToggleCreation',EnableToggleCreation);
  IniFile.WriteBool('Misc','SpaceKeyModifyTiming',SpaceKeyModifyTiming);
  IniFile.WriteInteger('Misc','SpaceKeyCPSTarget',SpaceKeyCPSTarget);
  IniFile.WriteInteger('Misc','SpaceKeyMinimalDuration',SpaceKeyMinimalDuration);
  IniFile.WriteBool('Misc','EnableMouseAntiOverlapping',EnableMouseAntiOverlapping);
  IniFile.WriteBool('Misc','EnableMouseSnapping',EnableMouseSnapping);
  IniFile.WriteInteger('Misc','SpaceKeyBlankBetweenSubtitles',SpaceKeyBlankBetweenSubtitles);
  IniFile.WriteInteger('Misc','MaximumSubtitleDuration',MaximumSubtitleDuration);
  IniFile.WriteBool('Misc','LoadMostRecentProjectOnStartup',LoadMostRecentProjectOnStartup);
  IniFile.WriteBool('Misc','DictionaryOnLineEnabled',DictionaryOnLineEnabled);
  IniFile.WriteBool('Misc','DictionaryOnLineExcludeTextPipe',DictionaryOnLineExcludeTextPipe);
  IniFile.WriteBool('Misc','QuickSmartSilentZonePanels',QuickSmartSilentZonePanels);
  IniFile.WriteBool('Misc','UseInternalFilters',UseInternalFilters);
  IniFile.WriteBool('Misc','DoNotUseInternalFiltersOnCreateProject',DoNotUseInternalFiltersOnCreateProject);
  IniFile.WriteBool('Misc','UseDefaultCodecOnCreateProject',UseDefaultCodecOnCreateProject);
  IniFile.WriteBool('Misc','UseAlternativeCodecOnCreateProject',UseAlternativeCodecOnCreateProject);
  IniFile.WriteBool('Misc','ForceStopOnPausing',ForceStopOnPausing);
  IniFile.WriteBool('Misc','DoubledSubResolution',DoubledSubResolution);
  IniFile.WriteBool('Misc','ForceNoDesktopComposition',ForceNoDesktopComposition);
  IniFile.WriteBool('Misc','PreferLegacyVideoRenderer',PreferLegacyVideoRenderer);
  IniFile.WriteBool('Misc','PreferVrm7VideoRenderer',PreferVmr7VideoRenderer);
  IniFile.WriteBool('Misc','PreferVrm9VideoRenderer',PreferVmr9VideoRenderer);
  IniFile.WriteBool('Misc','UseReclockAudioRenderer',UseReclockAudioRenderer);
  IniFile.WriteInteger('Misc','SpaceKeyMoveAfterSmartButtonIsUsed',SpaceKeyMoveAfterSmartButtonIsUsed);
  IniFile.WriteBool('Misc','UseDefaultInternalCodec',UseDefaultInternalCodec);
  IniFile.WriteBool('Misc','UseAlternativeInternalCodec',UseAlternativeInternalCodec);
  IniFile.WriteBool('Misc','LavVHwNone',LavVHwNone);
  IniFile.WriteBool('Misc','LavVQuickSync',LavVQuickSync);
  IniFile.WriteBool('Misc','LavVCuda',LavVCuda);
  IniFile.WriteBool('Misc','LavVDxva2',LavVDxva2);
  IniFile.WriteBool('Misc','EnableLavAudioMixing',EnableLavAudioMixing);
  IniFile.WriteString('Misc','LavPreferredLanguages',LavPreferredLanguages);
  IniFile.WriteBool('Misc','ForceRegisteredCodecs',ForceRegisteredCodecs);
  IniFile.WriteBool('Misc','CheckUpdates',CheckUpdates);
  IniFile.WriteBool('Misc','OnLineSearchDict1',OnLineSearchDict1);
  IniFile.WriteString('Misc','WordReferenceLanguage',WordReferenceLanguage);
  IniFile.WriteBool('Misc','OnLineSearchDict2',OnLineSearchDict2);
  IniFile.WriteBool('Misc','OnLineSearchDict3',OnLineSearchDict3);
  IniFile.WriteBool('Misc','OnLineSearchDict4',OnLineSearchDict4);
  IniFile.WriteBool('Misc','OnLineSearchDict5',OnLineSearchDict5);
  IniFile.WriteString('Misc','LinkDictionaries',LinkDictionaries);
  IniFile.WriteString('Misc','CustomText',CustomText);
  IniFile.WriteBool('Misc','FlippedPicture',FlippedPicture);
  IniFile.WriteBool('Misc','FlippedSubtitles',FlippedSubtitles);
  IniFile.WriteBool('Misc','UTF8AsDefault',UTF8AsDefault);
  IniFile.WriteBool('Misc','KeepDurationIntact',KeepDurationIntact);
  IniFile.WriteBool('Misc','AccentsAssistant',AccentsAssistant);

  //Custom Wav Colors
  IniFile.WriteInteger('Misc','WavColorsWC',  WavColors.WAV_COLOR);
  IniFile.WriteInteger('Misc','WavColorsWBC', WavColors.WAV_BACK_COLOR);
  IniFile.WriteInteger('Misc','WavColorsRC1', WavColors.RANGE_COLOR_1);
  IniFile.WriteInteger('Misc','WavColorsRC2', WavColors.RANGE_COLOR_2);
  IniFile.WriteInteger('Misc','WavColorsRCNE',WavColors.RANGE_COLOR_NOT_EDITABLE);
  IniFile.WriteInteger('Misc','WavColorsCC',  WavColors.CURSOR_COLOR);

  // Silent Zone parameters
  IniFile.WriteInteger('Misc','SilentZoneThreshold',SilentZoneThreshold);
  IniFile.WriteInteger('Misc','SilentZoneDuration',SilentZoneDuration);
  IniFile.WriteInteger('Misc','SilentZoneMinimunBetweenZones',SilentZoneMinimunBetweenZones);

  IniFile.WriteBool('Misc','AutoPlaySmartButtonIsUsed',AutoPlaySmartButtonIsUsed);
  IniFile.WriteBool('Misc','AutoScrollWavDisplay',AutoScrollWavDisplay);
  IniFile.WriteBool('Misc','AutoScrollSubtitles',AutoScrollSubtitles);

  // Web server
  IniFile.WriteInteger('WebServer','Port',ServerPort);
  IniFile.WriteBool('WebServer','EnableCompression',EnableCompression);

  // Error plugin
  for i:=0 to ListJSPlugin.Count-1 do
  begin
    JSPluginInfo := ListJSPlugin[i];
    IniFile.WriteBool(JSPluginInfo.Name, 'Enabled', JSPluginInfo.Enabled);
    for j:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pPluginParam := JSPluginInfo.ParamList[j];
      case pPluginParam.ParamType of
        jsptBoolean: IniFile.WriteBool(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.BooleanValue);
        jsptInteger: IniFile.WriteInteger(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.IntegerValue);
        jsptDouble: IniFile.WriteFloat(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.DoubleValue);
        jsptWideString: IniFile.WriteString(JSPluginInfo.Name, pPluginParam.Name, UTF8Encode(pPluginParam.WideStringValue));
      end;
    end;
  end;

  // Hotkeys
  IniFile.WriteBool('Hotkeys', 'UseIntegerShortcut', True);
  for i:=0 to ListHotkeys.Count-1 do
  begin
    HLID := ListHotkeys[i];
    IniFile.WriteInteger(
      'Hotkeys',
      HLID.Action.Name + '[Normal]',
      HLID.NormalShortCut);
    IniFile.WriteInteger(
      'Hotkeys',
      HLID.Action.Name + '[Timing]',
      HLID.TimingShortCut);
  end;

  // Mouse
  IniFile.WriteInteger('Mouse','WheelTimeScrollModifier',Ord(MouseWheelTimeScrollModifier));
  IniFile.WriteInteger('Mouse','WheelVZoomModifier',Ord(MouseWheelVZoomModifier));
  IniFile.WriteInteger('Mouse','WheelHZoomModifier',Ord(MouseWheelHZoomModifier));
  IniFile.WriteBool('Mouse','EnableSSATimingMode',MouseEnableSSATimingMode);

  // Backup
  IniFile.WriteBool('Backup','EnableBackup',EnableBackup);
  IniFile.WriteInteger('Backup','AutoBackupEvery',AutoBackupEvery);
  IniFile.WriteBool('Backup','AutoSaveWhenPlaying',AutoSaveWhenPlaying);

  // Fonts
  IniFile.WriteString('Fonts', 'SubList', SubListFont);
  IniFile.WriteString('Fonts', 'SubText', SubTextFont);
  IniFile.WriteString('Fonts', 'SubVideo', SubVideoFont);
  IniFile.WriteInteger('Fonts', 'TransparencyColorSubVideo', TransparencySubVideoFont);
  IniFile.WriteInteger('Fonts', 'OutlineSubVideo', OutlineSubVideoFont);
  IniFile.WriteInteger('Fonts', 'OutlineColorSubVideo', OutlineSubVideoColorFont);
  IniFile.WriteInteger('Fonts', 'TransparencyOutlineColorSubVideo', TransparencyOutlineSubVideoFont);
  IniFile.WriteInteger('Fonts', 'ShadowSubVideo', ShadowSubVideoFont);
  IniFile.WriteInteger('Fonts', 'ShadowColorSubVideo', ShadowSubVideoColorFont);
  IniFile.WriteInteger('Fonts', 'TransparencyShadowColorSubVideo', TransparencyShadowSubVideoFont);

  // WAV Display
  IniFile.WriteBool('WAVDisplay', 'ShowSceneChange', ShowSceneChange);
  IniFile.WriteInteger('WAVDisplay', 'SceneChangeStartOffset', SceneChangeStartOffset);
  IniFile.WriteInteger('WAVDisplay', 'SceneChangeStopOffset', SceneChangeStopOffset);
  IniFile.WriteInteger('WAVDisplay', 'SceneChangeFilterOffset', SceneChangeFilterOffset);
  IniFile.WriteBool('WAVDisplay', 'ShowTextInWAVDisplay', ShowTextInWAVDisplay);
  IniFile.WriteBool('WAVDisplay', 'ShowSpecialTagSubs', ShowSpecialTagSubs);
  IniFile.WriteBool('WAVDisplay', 'SwapSubVO', SwapSubVO);
  IniFile.WriteBool('WAVDisplay', 'DisableVOShowTextInWAVDisplay', DisableVOShowTextInWAVDisplay);
  IniFile.WriteBool('WAVDisplay', 'EnableShowVideoForNewProject', EnableShowVideoForNewProject);
  IniFile.WriteBool('WAVDisplay', 'EnableDetachedVideoForNewProject', EnableDetachedVideoForNewProject);

  // Desynch tools
  IniFile.WriteBool('DesynchTools','DesynchToolsAutoReset',DesynchToolsAutoReset);
  IniFile.WriteBool('DesynchTools','DesynchUseColorsForSubtitles',DesynchUseColorsForSubtitles);
  IniFile.WriteBool('DesynchTools','DesynchUseIconsForSubtitles',DesynchUseIconsForSubtitles);
  IniFile.WriteBool('DesynchTools','DesynchAssumeFirstSubtitleSynched',DesynchAssumeFirstSubtitleSynched);
  IniFile.WriteBool('DesynchTools','DesynchLog',DesynchLog);

  // Itasa
  IniFile.WriteString('ItasaUser','Username',Username);

  IniFile.WriteString('ItasaUser','Passwd','@@@'+ASCIIEncrypt(Passwd,DSiGetDiskSerial('C'))+'@@@');
  IniFile.WriteInteger('ItasaUser','MsgLines', Msg.Count);
  for i := 0 to Msg.Count-1 do
    IniFile.WriteString('ItasaUser','MsgLine_'+IntToStr(i), Msg.Strings[i]);
  IniFile.WriteString('ItasaUser','ZipPath',ZipPath);
  IniFile.WriteBool('ItasaUser','ZipPathOnlyForCP',ZipPathOnlyForCP);

  //Default Path
  IniFile.WriteString('Misc','DefaultPath',DefaultPath);
  IniFile.WriteBool('Misc','ForceDefaultPath',ForceDefaultPath);

  IniFile.WriteString('Misc','LastUsedPath',WideGetCurrentDir);

  // Mod String
  IniFile.WriteString('Misc','ModString',ModString);

end;

//------------------------------------------------------------------------------

procedure TConfigObject.LoadIni(IniFile : TIniFile; IsPresets : Boolean);
var i, j, IntValue : integer;
    HLID : THotkeyListItemData;
    JSPluginInfo : TJSPluginInfo;
    pPluginParam : PJSPluginParam;
    KeyName : string;
begin
  // Misc
  SwapSubtitlesList := IniFile.ReadBool('Misc','SwapSubtitlesList',SwapSubtitlesList);
  DisableSubtitleEdition := IniFile.ReadBool('Misc','DisableSubtitleEdition',DisableSubtitleEdition);
  EnableToggleCreation := IniFile.ReadBool('Misc','EnableToggleCreation',EnableToggleCreation);
  SpaceKeyModifyTiming := IniFile.ReadBool('Misc','SpaceKeyModifyTiming',SpaceKeyModifyTiming);
  SpaceKeyCPSTarget := IniFile.ReadInteger('Misc','SpaceKeyCPSTarget',SpaceKeyCPSTarget);
  SpaceKeyMinimalDuration := IniFile.ReadInteger('Misc','SpaceKeyMinimalDuration',SpaceKeyMinimalDuration);
  EnableMouseAntiOverlapping := IniFile.ReadBool('Misc','EnableMouseAntiOverlapping',EnableMouseAntiOverlapping);
  EnableMouseSnapping := IniFile.ReadBool('Misc','EnableMouseSnapping',EnableMouseSnapping);
  SpaceKeyBlankBetweenSubtitles := IniFile.ReadInteger('Misc','SpaceKeyBlankBetweenSubtitles',SpaceKeyBlankBetweenSubtitles);
  MaximumSubtitleDuration := IniFile.ReadInteger('Misc','MaximumSubtitleDuration',MaximumSubtitleDuration);
  LoadMostRecentProjectOnStartup := IniFile.ReadBool('Misc','LoadMostRecentProjectOnStartup',LoadMostRecentProjectOnStartup);
  DictionaryOnLineEnabled := IniFile.ReadBool('Misc','DictionaryOnLineEnabled',DictionaryOnLineEnabled);
  DictionaryOnLineExcludeTextPipe := IniFile.ReadBool('Misc','DictionaryOnLineExcludeTextPipe',DictionaryOnLineExcludeTextPipe);
  QuickSmartSilentZonePanels := IniFile.ReadBool('Misc','QuickSmartSilentZonePanels',QuickSmartSilentZonePanels);
  UseInternalFilters := IniFile.ReadBool('Misc','UseInternalFilters',UseInternalFilters);
  DoNotUseInternalFiltersOnCreateProject := IniFile.ReadBool('Misc','DoNotUseInternalFiltersOnCreateProject',DoNotUseInternalFiltersOnCreateProject);
  UseDefaultCodecOnCreateProject := IniFile.ReadBool('Misc','UseDefaultCodecOnCreateProject',UseDefaultCodecOnCreateProject);
  UseAlternativeCodecOnCreateProject := IniFile.ReadBool('Misc','UseAlternativeCodecOnCreateProject',UseAlternativeCodecOnCreateProject);
  ForceStopOnPausing := IniFile.ReadBool('Misc','ForceStopOnPausing',ForceStopOnPausing);
  DoubledSubResolution := IniFile.ReadBool('Misc','DoubledSubResolution',DoubledSubResolution);
  ForceNoDesktopComposition := IniFile.ReadBool('Misc','ForceNoDesktopComposition',ForceNoDesktopComposition);
  PreferLegacyVideoRenderer := IniFile.ReadBool('Misc','PreferLegacyVideoRenderer',PreferLegacyVideoRenderer);
  PreferVmr7VideoRenderer := IniFile.ReadBool('Misc','PreferVrm7VideoRenderer',PreferVmr7VideoRenderer);
  PreferVmr9VideoRenderer := IniFile.ReadBool('Misc','PreferVrm9VideoRenderer',PreferVmr9VideoRenderer);
  UseReclockAudioRenderer := IniFile.ReadBool('Misc','UseReclockAudioRenderer',UseReclockAudioRenderer);
  SpaceKeyMoveAfterSmartButtonIsUsed := IniFile.ReadInteger('Misc','SpaceKeyMoveAfterSmartButtonIsUsed',SpaceKeyMoveAfterSmartButtonIsUsed);
  UseDefaultInternalCodec := IniFile.ReadBool('Misc','UseDefaultInternalCodec',UseDefaultInternalCodec);
  UseAlternativeInternalCodec := IniFile.ReadBool('Misc','UseAlternativeInternalCodec',UseAlternativeInternalCodec);
  LavVHwNone := IniFile.ReadBool('Misc','LavVHwNone',LavVHwNone);
  LavVQuickSync := IniFile.ReadBool('Misc','LavVQuickSync',LavVQuickSync);
  LavVCuda := IniFile.ReadBool('Misc','LavVCuda',LavVCuda);
  LavVDxva2 := IniFile.ReadBool('Misc','LavVDxva2',LavVDxva2);
  EnableLavAudioMixing := IniFile.ReadBool('Misc','EnableLavAudioMixing',EnableLavAudioMixing);
  LavPreferredLanguages := IniFile.ReadString('Misc','LavPreferredLanguages',LavPreferredLanguages);
  ForceRegisteredCodecs := IniFile.ReadBool('Misc','ForceRegisteredCodecs',ForceRegisteredCodecs);
  if LocalLavCodecInstalled = False then ForceRegisteredCodecs := True;
  CheckUpdates := IniFile.ReadBool('Misc','CheckUpdates',CheckUpdates);
  OnLineSearchDict1 := IniFile.ReadBool('Misc','OnLineSearchDict1',OnLineSearchDict1);
  WordReferenceLanguage := IniFile.ReadString('Misc','WordReferenceLanguage',WordReferenceLanguage);
  if not(lowercase(MainForm.ConfigObject.ModString) = 'for italiansubs') then
   OnLineSearchDict1 := false;
  OnLineSearchDict2 := IniFile.ReadBool('Misc','OnLineSearchDict2',OnLineSearchDict2);
  OnLineSearchDict3 := IniFile.ReadBool('Misc','OnLineSearchDict3',OnLineSearchDict3);
  OnLineSearchDict4 := IniFile.ReadBool('Misc','OnLineSearchDict4',OnLineSearchDict4);
  OnLineSearchDict5 := IniFile.ReadBool('Misc','OnLineSearchDict5',OnLineSearchDict5);
  LinkDictionaries := IniFile.ReadString('Misc','LinkDictionaries',LinkDictionaries);
  CustomText := IniFile.ReadString('Misc','CustomText',CustomText);
  FlippedPicture := IniFile.ReadBool('Misc','FlippedPicture',FlippedPicture);
  FlippedSubtitles := IniFile.ReadBool('Misc','FlippedSubtitles',FlippedSubtitles);
  UTF8AsDefault := IniFile.ReadBool('Misc','UTF8AsDefault',UTF8AsDefault);
  KeepDurationIntact := IniFile.ReadBool('Misc','KeepDurationIntact',KeepDurationIntact);
  MainForm.MenuItemKeepDurationIntact.Checked := KeepDurationIntact;
  AccentsAssistant := IniFile.ReadBool('Misc','AccentsAssistant',AccentsAssistant);

  //Custom Wav Colors
  WavColors.WAV_COLOR               := IniFile.ReadInteger('Misc','WavColorsWC',  WavColors.WAV_COLOR);
  WavColors.WAV_BACK_COLOR          := IniFile.ReadInteger('Misc','WavColorsWBC', WavColors.WAV_BACK_COLOR);
  WavColors.RANGE_COLOR_1           := IniFile.ReadInteger('Misc','WavColorsRC1', WavColors.RANGE_COLOR_1);
  WavColors.RANGE_COLOR_2           := IniFile.ReadInteger('Misc','WavColorsRC2', WavColors.RANGE_COLOR_2);
  WavColors.RANGE_COLOR_NOT_EDITABLE:= IniFile.ReadInteger('Misc','WavColorsRCNE',WavColors.RANGE_COLOR_NOT_EDITABLE);
  WavColors.CURSOR_COLOR            := IniFile.ReadInteger('Misc','WavColorsCC',  WavColors.CURSOR_COLOR);

  SilentZoneThreshold := IniFile.ReadInteger('Misc','SilentZoneThreshold',SilentZoneThreshold);
  SilentZoneDuration := IniFile.ReadInteger('Misc','SilentZoneDuration',SilentZoneDuration);
  SilentZoneMinimunBetweenZones := IniFile.ReadInteger('Misc','SilentZoneMinimunBetweenZones',SilentZoneMinimunBetweenZones);
  MainForm.udThreshold.Position := SilentZoneThreshold;
  MainForm.udDuration.Position := SilentZoneDuration;
  MainForm.udMinimunBetweenZones.Position := SilentZoneMinimunBetweenZones;

  AutoPlaySmartButtonIsUsed := IniFile.ReadBool('Misc','AutoPlaySmartButtonIsUsed',AutoPlaySmartButtonIsUsed);
  AutoScrollWavDisplay := IniFile.ReadBool('Misc','AutoScrollWavDisplay',AutoScrollWavDisplay);
  AutoScrollSubtitles := IniFile.ReadBool('Misc','AutoScrollSubtitles',AutoScrollSubtitles);

  // Web server
  if (not IsPresets) then
  begin
    ServerPort := IniFile.ReadInteger('WebServer','Port',ServerPort);
    EnableCompression := IniFile.ReadBool('WebServer','EnableCompression',EnableCompression);
  end;

  // Error plugin
  for i:=0 to ListJSPlugin.Count-1 do
  begin
    JSPluginInfo := ListJSPlugin[i];
    JSPluginInfo.Enabled := IniFile.ReadBool(JSPluginInfo.Name, 'Enabled', JSPluginInfo.Enabled);
    for j:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pPluginParam := JSPluginInfo.ParamList[j];
      case pPluginParam.ParamType of
        jsptBoolean: pPluginParam.BooleanValue := IniFile.ReadBool(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.BooleanValue);
        jsptInteger: pPluginParam.IntegerValue := IniFile.ReadInteger(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.IntegerValue);
        jsptDouble: pPluginParam.DoubleValue := IniFile.ReadFloat(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.DoubleValue);
        jsptWideString: pPluginParam.WideStringValue := UTF8Decode(IniFile.ReadString(JSPluginInfo.Name, pPluginParam.Name, UTF8Encode(pPluginParam.WideStringValue)));
      end;
    end;
  end;

  // Hotkeys
  if IniFile.ReadBool('Hotkeys', 'UseIntegerShortcut', False) = True then
  begin
    for i:=0 to ListHotkeys.Count-1 do
    begin
      HLID := ListHotkeys[i];
      
      KeyName := HLID.Action.Name + '[Normal]';
      if IniFile.ValueExists('Hotkeys', KeyName) then
      begin
        HLID.NormalShortCut := IniFile.ReadInteger('Hotkeys', KeyName, HLID.NormalShortCut);
      end;

      KeyName := HLID.Action.Name + '[Timing]';
      if IniFile.ValueExists('Hotkeys', KeyName) then
      begin
        HLID.TimingShortCut := IniFile.ReadInteger('Hotkeys', KeyName, HLID.TimingShortCut);
      end;
    end;
  end
  else
  begin
    for i:=0 to ListHotkeys.Count-1 do
    begin
      HLID := ListHotkeys[i];

      KeyName := HLID.Action.Name + '[Normal]';
      // Check if value really exists
      if IniFile.ValueExists('Hotkeys', KeyName) then
      begin
        HLID.NormalShortCut := TextToShortCut(
          IniFile.ReadString('Hotkeys', KeyName, ShortCutToText(HLID.NormalShortCut)));
      end;

      KeyName := HLID.Action.Name + '[Timing]';
      if IniFile.ValueExists('Hotkeys', KeyName) then
      begin
        HLID.TimingShortCut := TextToShortCut(
          IniFile.ReadString('Hotkeys', KeyName, ShortCutToText(HLID.TimingShortCut)));
      end;
    end;
  end;

  // Mouse
  MouseWheelTimeScrollModifier := TMouseWheelModifier(IniFile.ReadInteger('Mouse',
    'WheelTimeScrollModifier',Ord(MouseWheelTimeScrollModifier)));
  MouseWheelVZoomModifier := TMouseWheelModifier(IniFile.ReadInteger('Mouse',
    'WheelVZoomModifier',Ord(MouseWheelVZoomModifier)));
  MouseWheelHZoomModifier := TMouseWheelModifier(IniFile.ReadInteger('Mouse',
    'WheelHZoomModifier',Ord(MouseWheelHZoomModifier)));
  MouseEnableSSATimingMode := IniFile.ReadBool('Mouse',
    'EnableSSATimingMode',MouseEnableSSATimingMode);

  if (not IsPresets) then
  begin
    // Backup
    EnableBackup := IniFile.ReadBool('Backup','EnableBackup',EnableBackup);
    AutoBackupEvery := IniFile.ReadInteger('Backup','AutoBackupEvery',AutoBackupEvery);
    AutoSaveWhenPlaying := IniFile.ReadBool('Backup','AutoSaveWhenPlaying',AutoSaveWhenPlaying);

    // Fonts
    SubListFont := IniFile.ReadString('Fonts', 'SubList', SubListFont);
    SubTextFont := IniFile.ReadString('Fonts', 'SubText', SubTextFont);
    SubVideoFont := IniFile.ReadString('Fonts', 'SubVideo', SubVideoFont);
    TransparencySubVideoFont := IniFile.ReadInteger('Fonts', 'TransparencyColorSubVideo',TransparencySubVideoFont);
    OutlineSubVideoFont := IniFile.ReadInteger('Fonts', 'OutlineSubVideo', OutlineSubVideoFont);
    OutlineSubVideoColorFont := IniFile.ReadInteger('Fonts', 'OutlineColorSubVideo',OutlineSubVideoColorFont);
    TransparencyOutlineSubVideoFont := IniFile.ReadInteger('Fonts', 'TransparencyOutlineColorSubVideo',TransparencyOutlineSubVideoFont);
    ShadowSubVideoFont := IniFile.ReadInteger('Fonts', 'ShadowSubVideo', ShadowSubVideoFont);
    ShadowSubVideoColorFont := IniFile.ReadInteger('Fonts', 'ShadowColorSubVideo',ShadowSubVideoColorFont);
    TransparencyShadowSubVideoFont := IniFile.ReadInteger('Fonts', 'TransparencyShadowColorSubVideo',TransparencyShadowSubVideoFont);

  end;

  // WAV Display
  ShowSceneChange := IniFile.ReadBool('WAVDisplay', 'ShowSceneChange', ShowSceneChange);
  SceneChangeStartOffset := IniFile.ReadInteger('WAVDisplay','SceneChangeStartOffset',SceneChangeStartOffset);
  SceneChangeStopOffset := IniFile.ReadInteger('WAVDisplay','SceneChangeStopOffset',SceneChangeStopOffset);
  SceneChangeFilterOffset := IniFile.ReadInteger('WAVDisplay','SceneChangeFilterOffset',SceneChangeFilterOffset);
  ShowTextInWAVDisplay := IniFile.ReadBool('WAVDisplay', 'ShowTextInWAVDisplay', ShowTextInWAVDisplay);
  ShowSpecialTagSubs := IniFile.ReadBool('WAVDisplay', 'ShowSpecialTagSubs', ShowSpecialTagSubs);
  SwapSubVO := IniFile.ReadBool('WAVDisplay', 'SwapSubVO', SwapSubVO);
  DisableVOShowTextInWAVDisplay := IniFile.ReadBool('WAVDisplay', 'DisableVOShowTextInWAVDisplay', DisableVOShowTextInWAVDisplay);
  EnableShowVideoForNewProject := IniFile.ReadBool('WAVDisplay', 'EnableShowVideoForNewProject', EnableShowVideoForNewProject);
  EnableDetachedVideoForNewProject := IniFile.ReadBool('WAVDisplay', 'EnableDetachedVideoForNewProject', EnableDetachedVideoForNewProject);

  // Desynch tools
  DesynchToolsAutoReset := IniFile.ReadBool('DesynchTools', 'DesynchToolsAutoReset', DesynchToolsAutoReset);
  DesynchUseColorsForSubtitles := IniFile.ReadBool('DesynchTools', 'DesynchUseColorsForSubtitles', DesynchUseColorsForSubtitles);
  DesynchUseIconsForSubtitles := IniFile.ReadBool('DesynchTools', 'DesynchUseIconsForSubtitles', DesynchUseIconsForSubtitles);
  DesynchAssumeFirstSubtitleSynched := IniFile.ReadBool('DesynchTools', 'DesynchAssumeFirstSubtitleSynched', DesynchAssumeFirstSubtitleSynched);
  DesynchLog := IniFile.ReadBool('DesynchTools', 'DesynchLog', DesynchLog);

  // Itasa
  Username := IniFile.ReadString('ItasaUser','Username',Username);
  Passwd := IniFile.ReadString('ItasaUser','Passwd',Passwd);
  if (Copy(Passwd,1,3)='@@@') then
    Passwd := Copy(Passwd,4,Length(Passwd)-6);
  Passwd := ASCIIEncrypt(Passwd,DSiGetDiskSerial('C'));
  //Passwd := ASCIIEncrypt(IniFile.ReadString('ItasaUser','Passwd',Passwd),DSiGetDiskSerial('C'));

  Msg.Clear;
  j := IniFile.ReadInteger('ItasaUser','MsgLines', 1);
  for i := 0 to j-1 do
    Msg.Add(IniFile.ReadString('ItasaUser','MsgLine_'+IntToStr(i), '#sub1# :ciao:'));
  ZipPath:=IniFile.ReadString('ItasaUser','ZipPath',ZipPath);
  ZipPathOnlyForCP:=IniFile.ReadBool('ItasaUser','ZipPathOnlyForCP',ZipPathOnlyForCP);
  //Default Path
  DefaultPath:=IniFile.ReadString('Misc','DefaultPath',DefaultPath);
  ForceDefaultPath:=IniFile.ReadBool('Misc','ForceDefaultPath',ForceDefaultPath);

  WideSetCurrentDir(IniFile.ReadString('Misc','LastUsedPath',WideGetCurrentDir));

  // Mod String
  ModString := IniFile.ReadString('Misc','ModString',ModString);
  if ModString = 'ItaSA' then ModString := 'for Italiansubs';
  if ModString > '' then ApplicationName := 'VisualSubSync ' + ModString;
  {$IFDEF enhanced}
  ApplicationName := 'VisualSubSync Enhanced';
  {$ENDIF}
  {$ifdef DEBUG}ApplicationName := ApplicationName + ' [DEBUG VERSION]';{$endif}
end;

// =============================================================================

procedure TPreferencesForm.UpdateAssociationCheckBoxes;
var IsExtReged : Boolean;
begin
  IsExtReged := ShellIsCurrentUserExtensionRegistered('vssprj', ApplicationName, 'Document', Application.ExeName);
  SetCheckedState(chkAssociateExtVSSPRJ, IsExtReged);

  IsExtReged := ShellIsCurrentUserExtensionRegistered('srt', ApplicationName, 'Document.srt', Application.ExeName);
  SetCheckedState(chkAssociateExtSRT, IsExtReged);

  IsExtReged := ShellIsCurrentUserExtensionRegistered('ssa', ApplicationName, 'Document.ssa', Application.ExeName);
  SetCheckedState(chkAssociateExtSSA, IsExtReged);

  IsExtReged := ShellIsCurrentUserExtensionRegistered('ass', ApplicationName, 'Document.ass', Application.ExeName);
  SetCheckedState(chkAssociateExtASS, IsExtReged);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ClearErrorList;
var i : integer;
begin
  ListPluginParam.Clear;
  for i:=0 to ListErrorChecking.Items.Count-1 do
    TJSPluginInfo(ListErrorChecking.Items.Objects[i]).Free;
  ListErrorChecking.Clear;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.LoadConfig(Config : TConfigObject);
var i : integer;
    HLID : THotkeyListItemData;
    ListItem : TTntListItem;
    JSPluginInfoSrc, JSPluginInfoDst  : TJSPluginInfo;
begin
  // Misc
  chkSwapSubList.Checked := Config.SwapSubtitlesList;
  chkDisableSubEditionInTimingMode.Checked := Config.DisableSubtitleEdition;
  chkEnableSubCreationWithSpaceKey.Checked := Config.EnableToggleCreation;
  chkEnableMouseAntiOverlapping.Checked := Config.EnableMouseAntiOverlapping;
  chkEnableMouseSnapping.Checked := Config.EnableMouseSnapping;
  chkSpaceKeyModifyTiming.Checked := Config.SpaceKeyModifyTiming;
  UpDownCPSTarget.Position := Config.SpaceKeyCPSTarget;
  UpDownMinimalDuration.Position := Config.SpaceKeyMinimalDuration;
  UpDownBlankBetweenSub.Position := Config.SpaceKeyBlankBetweenSubtitles;
  UpDownMaximumDuration.Position := Config.MaximumSubtitleDuration;
  chkLoadMostRecentProjectOnStartup.Checked := Config.LoadMostRecentProjectOnStartup;
  chkDictionaryOnLineEnabled.Checked := Config.DictionaryOnLineEnabled;
  chkDictionaryOnLineExcludeTextPipe.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict1.Enabled := chkDictionaryOnLineEnabled.Checked;
  //if not(lowercase(MainForm.ConfigObject.ModString) = 'for italiansubs') then
  //  ChkOnLineSearchDict1.Enabled := false;
  ChkOnLineSearchDict2.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict3.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict4.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict5.Enabled := chkDictionaryOnLineEnabled.Checked;
  chkDictionaryOnLineExcludeTextPipe.Checked := Config.DictionaryOnLineExcludeTextPipe;
  chkQuickSmartSilentZonePanels.Checked := Config.QuickSmartSilentZonePanels;
  if not ChkQuickSmartSilentZonePanels.Checked then
  begin
   TntLabelMoveAfterSmartButtonIsUsed.Font.Color := clGray;
   EditMoveAfterSmartButtonIsUsed.Enabled := False;
   ChkMoveAfterSmartButtonIsUsed.Enabled := False;
  end;
  ChkUseInternalFilters.Checked := Config.UseInternalFilters;
  ChkDoNotUseInternalFiltersOnCreateProject.Checked := Config.DoNotUseInternalFiltersOnCreateProject;
  ChkUseDefaultCodecOnCreateProject.Checked := Config.UseDefaultCodecOnCreateProject;
  {$IFDEF enhanced}
  ChkUseDefaultCodecOnCreateProject.Checked := False;
  Config.UseDefaultCodecOnCreateProject := False;
  {$ENDIF}
  ChkUseAlternativeCodecOnCreateProject.Checked := Config.UseAlternativeCodecOnCreateProject;
  {$IFDEF enhanced}
  ChkUseAlternativeCodecOnCreateProject.Checked := True;
  Config.UseAlternativeCodecOnCreateProject := True;
  {$ENDIF}
  ChkForceStopOnPausing.Checked := Config.ForceStopOnPausing;
  ChkDoubledSubResolution.Checked := Config.DoubledSubResolution;
  ChkDisableDesktopComposition.Checked := Config.ForceNoDesktopComposition;
  {$IFDEF enhanced}
  ChkDisableDesktopComposition.Checked := False;
  ChkDisableDesktopComposition.Enabled := False;
  Config.ForceNoDesktopComposition := False;
  {$ELSE}
  if not ( (DSiGetWindowsVersion = wvWin7 ) OR (DSiGetWindowsVersion = wvWinVista ) ) then
  begin
    ChkDisableDesktopComposition.Checked := False;
    ChkDisableDesktopComposition.Enabled := False;
    Config.ForceNoDesktopComposition := False;
  end;
  {$ENDIF}
  ChkPreferVmr7VideoRenderer.Checked := Config.PreferVmr7VideoRenderer;
  ChkPreferVmr9VideoRenderer.Checked := Config.PreferVmr9VideoRenderer;
  ChkPreferLegacyVideoRenderer.Checked := Config.PreferLegacyVideoRenderer Or
   (not ChkPreferVmr7VideoRenderer.Checked And not ChkPreferVmr9VideoRenderer.Checked);
  ChkUseReclockAudioRenderer.Checked := Config.UseReclockAudioRenderer;
  UpDownMoveAfterSmartButtonIsUsed.Position := Config.SpaceKeyMoveAfterSmartButtonIsUsed;
  ChkMoveAfterSmartButtonIsUsed.Checked := Config.AutoPlaySmartButtonIsUsed;
  ChkDefaultInternalCodec.Checked := Config.UseDefaultInternalCodec;
  {$IFDEF enhanced}
  ChkDefaultInternalCodec.Checked := False;
  Config.UseDefaultInternalCodec := False;
  {$ENDIF}
  ChkAlternativeInternalCodec.Checked := Config.UseAlternativeInternalCodec;
  ChkLavVHwNone.Checked := Config.LavVHwNone;
  ChkLavVQuickSync.Checked := Config.LavVQuickSync;
  ChkLavVCuda.Checked := Config.LavVCuda;
  ChkLavVDxva2.Checked := Config.LavVDxva2;
  ChkEnableLavAudioMixing.Checked := Config.EnableLavAudioMixing;
  EditLavPreferredLanguages.Text := Config.LavPreferredLanguages;
  ChkForceRegisteredCodecs.Checked := Config.ForceRegisteredCodecs;
  if not LocalLavCodecInstalled then
   begin
    ChkForceRegisteredCodecs.Checked := True;
    ChkForceRegisteredCodecs.Enabled := False;
   end;
  ChkUpdates.Checked := Config.CheckUpdates;
  {$IFDEF enhanced}
  ChkUpdates.Checked := False;
  ChkUpdates.Enabled := False;
  ChkUpdates.Visible := False;
  Config.CheckUpdates := False;
  {$ENDIF}
  ChkOnLineSearchDict1.Checked := Config.OnLineSearchDict1;
  cbbWordReferenceLanguage.ItemIndex := cbbWordReferenceLanguage.Items.IndexOf(Config.WordReferenceLanguage);
  cbbWordReferenceLanguage.Enabled := ChkOnLineSearchDict1.Checked And ChkOnLineSearchDict1.Enabled;
  ChkOnLineSearchDict2.Checked := Config.OnLineSearchDict2;
  ChkOnLineSearchDict3.Checked := Config.OnLineSearchDict3;
  ChkOnLineSearchDict4.Checked := Config.OnLineSearchDict4;
  ChkOnLineSearchDict5.Checked := Config.OnLineSearchDict5;
  lbledtLinkDictionaries.Text := Config.LinkDictionaries;
  EditCustomText.Text := Config.CustomText;
  if Config.CustomText <> '' then
    ChkCustomText.Checked := True
  else
    ChkCustomText.Checked := False;
  ChkFlippedPicture.Checked := Config.FlippedPicture;
  ChkFlippedSubtitles.Checked := Config.FlippedSubtitles;
  ChkUTF8AsDefault.Checked := Config.UTF8AsDefault;
  chkAccentsAssistant.Checked := Config.AccentsAssistant;

  //Custom Wav Colors
  Config.WavColorsTemp  := Config.WavColors;
  ShapeWC.Brush.Color   := Config.WavColors.WAV_COLOR;
  ShapeWBC.Brush.Color  := Config.WavColors.WAV_BACK_COLOR;
  ShapeCC.Brush.Color   := Config.WavColors.CURSOR_COLOR;
  ShapeRC1.Brush.Color  := Config.WavColors.RANGE_COLOR_1;
  ShapeRC2.Brush.Color  := Config.WavColors.RANGE_COLOR_2;
  ShapeRCNE.Brush.Color := Config.WavColors.RANGE_COLOR_NOT_EDITABLE;

  ChkDesynchToolsAutoReset.Checked := Config.DesynchToolsAutoReset;
  ChkDesynchUseColorsForSubtitles.Checked := Config.DesynchUseColorsForSubtitles;
  ChkDesynchUseIconsForSubtitles.Checked := Config.DesynchUseIconsForSubtitles;
  ChkDesynchAssumeFirstSubtitleSynched.Checked := Config.DesynchAssumeFirstSubtitleSynched;
  ChkDesynchLog.Checked := Config.DesynchLog;

  // Web server
  UpDownServerPort.Position := Config.ServerPort;
  chkEnableCompression.Checked := Config.EnableCompression;

  // Error plugin
  ClearErrorList;
  ListErrorChecking.Sorted := True;
  for i:=0 to Config.ListJSPlugin.Count-1 do
  begin
    // We do a local copy
    JSPluginInfoSrc := Config.ListJSPlugin[i];
    JSPluginInfoDst := TJSPluginInfo.Create;
    JSPluginInfoDst.Assign(JSPluginInfoSrc);
    ListErrorChecking.AddItem(JSPluginInfoDst.Name, JSPluginInfoDst);
  end;
  ListErrorChecking.Sorted := False;
  // Do the checking now cause sorting would have messed up the order 
  for i:=0 to ListErrorChecking.Items.Count-1 do
  begin
    JSPluginInfoSrc := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    ListErrorChecking.Checked[i] := JSPluginInfoSrc.Enabled;
  end;
  // Select the first item
  if (ListErrorChecking.Items.Count > 0) then
  begin
    ListErrorChecking.ItemIndex := 0;
    ListErrorCheckingClick(nil);
  end;

  // Hotkeys
  ListHotkeys.Clear;
  for i:=0 to Config.ListHotkeys.Count-1 do
  begin
    HLID := THotkeyListItemData.Create;
    HLID.Assign(Config.ListHotkeys[i]);
    ListItem := ListHotkeys.Items.Add;
    ListItem.Data := HLID;
    ListItem.Caption := HLID.Action.Caption;
    ListItem.SubItems.Add(ShortCutToText(HLID.NormalShortCut));
    ListItem.SubItems.Add(ShortCutToText(HLID.TimingShortCut));
  end;

  // Default hotkeys never change once loaded
  // we just keep a pointer
  ListDefaultHotkeys := Config.ListDefaultHotkeys;

  // Mouse
  ComboWheelTimeScrollModifier.ItemIndex := Ord(Config.MouseWheelTimeScrollModifier);
  ComboWheelVZoomModifier.ItemIndex := Ord(Config.MouseWheelVZoomModifier);
  ComboWheelHZoomModifier.ItemIndex := Ord(Config.MouseWheelHZoomModifier);
  chkEnableSSATimingMode.Checked := Config.MouseEnableSSATimingMode;

  // Backup
  chkCreateBackup.Checked := Config.EnableBackup;
  UpDownBackupTime.Position := Config.AutoBackupEvery;
  chkAutoSaveWhenPlaying.Checked := Config.AutoSaveWhenPlaying;

  // Fonts
  EditSubListFont.Text := Config.SubListFont;
  String2Font(EditSubListFont.Text, EditSubListFont.Font);
  EditSubTextFont.Text := Config.SubTextFont;
  String2Font(EditSubTextFont.Text, EditSubTextFont.Font);
  EditSubVideoFont.Text := Config.SubVideoFont;
  String2FontNoSetColor(EditSubVideoFont.Text, EditSubVideoFont.Font);
  TrackBarTransparencySubVideo.Position := Config.TransparencySubVideoFont;
  UpDownSubVideoOutline.Position := Config.OutlineSubVideoFont;
  ShapeSubVideoOutline.Brush.Color := Config.OutlineSubVideoColorFont;
  TrackBarTransparencyOutline.Position := Config.TransparencyOutlineSubVideoFont;
  UpDownSubVideoShadow.Position := Config.ShadowSubVideoFont;
  ShapeSubVideoShadow.Brush.Color := Config.ShadowSubVideoColorFont;
  TrackBarTransparencyShadow.Position := Config.TransparencyShadowSubVideoFont;

  // WAV Display
  chkSceneChange.Checked := Config.ShowSceneChange;
  UpDownSCStart.Position := Config.SceneChangeStartOffset;
  UpDownSCStop.Position := Config.SceneChangeStopOffset;
  UpDownSCFilter.Position := Config.SceneChangeFilterOffset;
  chkShowTextInWAVDisplay.Checked := Config.ShowTextInWAVDisplay;
  chkShowSpecialTagSubs.Checked := Config.ShowSpecialTagSubs;
  chkSwapSubVO.Checked := Config.SwapSubVO;
  chkDisableVOShowTextInWAVDisplay.Checked := Config.DisableVOShowTextInWAVDisplay;
  chkEnableShowVideoForNewProject.Checked := Config.EnableShowVideoForNewProject;
  chkEnableDetachedVideoForNewProject.Checked := Config.EnableDetachedVideoForNewProject;

  // Itasa
  TntEditUsername.Text := Config.Username;
  TntEditPasswd.Text := Config.Passwd;
  if ItaliansubsAuthorization(TntEditUsername.Text,TntEditPasswd.Text) then
  begin
     ImgLoginOK.Visible := True;
     ItaliansubsOptionsGroupBox.Visible:=True;
  end;
  TntMemoMsg.Text := Config.Msg.Text;
  LabelZipPath.Caption := Config.ZipPath;
  if Config.ZipPath <> ZIP_PATH then
  begin
    ChkZipPath.Checked:=True;
    LabelZipPath.Enabled:=True;
    LabelZipPath.Caption:=Config.ZipPath;
    LabelZipPath.Hint:=Config.ZipPath;
    ChkZipOnlyCP.Enabled:=True;
    ChkZipOnlyCP.Checked:=Config.ZipPathOnlyForCP;
  end;

  //Default Path
  LblDefaultPath.Caption:=Config.DefaultPath;
  if Config.DefaultPath <> DEF_PATH then
  begin
    ChkDefaultPath.Checked:=True;
    LblDefaultPath.Enabled:=True;
    LblDefaultPath.Hint:=Config.DefaultPath;
    ChkForceDefaultPath.Enabled:=True;
    ChkForceDefaultPath.Checked:=Config.ForceDefaultPath;
  end;

end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SaveConfig(Config : TConfigObject);
var i : integer;
    HLID : THotkeyListItemData;
    JSPluginInfoSrc, JSPluginInfoDst : TJSPluginInfo;
begin
  // Misc
  Config.SwapSubtitlesList := chkSwapSubList.Checked;
  Config.DisableSubtitleEdition := chkDisableSubEditionInTimingMode.Checked;
  Config.EnableToggleCreation := chkEnableSubCreationWithSpaceKey.Checked;
  Config.EnableMouseAntiOverlapping := chkEnableMouseAntiOverlapping.Checked;
  Config.EnableMouseSnapping := chkEnableMouseSnapping.Checked;
  Config.SpaceKeyModifyTiming := chkSpaceKeyModifyTiming.Checked;
  Config.SpaceKeyCPSTarget := UpDownCPSTarget.Position;
  Config.SpaceKeyMinimalDuration := UpDownMinimalDuration.Position;
  Config.SpaceKeyBlankBetweenSubtitles := UpDownBlankBetweenSub.Position;
  Config.MaximumSubtitleDuration := UpDownMaximumDuration.Position;
  Config.LoadMostRecentProjectOnStartup := chkLoadMostRecentProjectOnStartup.Checked;
  Config.DictionaryOnLineEnabled := chkDictionaryOnLineEnabled.Checked;
  Config.DictionaryOnLineExcludeTextPipe := chkDictionaryOnLineExcludeTextPipe.Checked;
  Config.QuickSmartSilentZonePanels := chkQuickSmartSilentZonePanels.Checked;
  Config.UseInternalFilters := ChkUseInternalFilters.Checked;
  Config.DoNotUseInternalFiltersOnCreateProject := ChkDoNotUseInternalFiltersOnCreateProject.Checked;
  Config.UseDefaultCodecOnCreateProject := ChkUseDefaultCodecOnCreateProject.Checked;
  Config.UseAlternativeCodecOnCreateProject := ChkUseAlternativeCodecOnCreateProject.Checked;
  Config.ForceStopOnPausing := ChkForceStopOnPausing.Checked;
  Config.DoubledSubResolution := ChkDoubledSubResolution.Checked;
  Config.ForceNoDesktopComposition := ChkDisableDesktopComposition.Checked;
  Config.PreferLegacyVideoRenderer := ChkPreferLegacyVideoRenderer.Checked;
  Config.PreferVmr7VideoRenderer := ChkPreferVmr7VideoRenderer.Checked;
  Config.PreferVmr9VideoRenderer := ChkPreferVmr9VideoRenderer.Checked;
  Config.UseReclockAudioRenderer := ChkUseReclockAudioRenderer.Checked;
  Config.SpaceKeyMoveAfterSmartButtonIsUsed := UpDownMoveAfterSmartButtonIsUsed.Position;
  Config.AutoPlaySmartButtonIsUsed := ChkMoveAfterSmartButtonIsUsed.Checked;
  Config.DesynchToolsAutoReset := ChkDesynchToolsAutoReset.Checked;
  Config.DesynchUseColorsForSubtitles := ChkDesynchUseColorsForSubtitles.Checked;
  Config.DesynchUseIconsForSubtitles := ChkDesynchUseIconsForSubtitles.Checked;
  Config.DesynchAssumeFirstSubtitleSynched := ChkDesynchAssumeFirstSubtitleSynched.Checked;
  Config.DesynchLog := ChkDesynchLog.Checked;
  Config.UseDefaultInternalCodec := ChkDefaultInternalCodec.Checked;
  {$IFDEF enhanced}
  Config.UseDefaultInternalCodec := False;
  {$ENDIF}
  Config.UseAlternativeInternalCodec := ChkAlternativeInternalCodec.Checked;
  if ChkCustomText.Checked then
    Config.CustomText := EditCustomText.Text
  else
    Config.CustomText := '';

  Config.LavVHwNone := ChkLavVHwNone.Checked;
  Config.LavVQuickSync := ChkLavVQuickSync.Checked;
  Config.LavVCuda := ChkLavVCuda.Checked;
  Config.LavVDxva2 := ChkLavVDxva2.Checked;
  Config.EnableLavAudioMixing := ChkEnableLavAudioMixing.Checked;
  Config.LavPreferredLanguages := EditLavPreferredLanguages.Text;
  Config.ForceRegisteredCodecs := ChkForceRegisteredCodecs.Checked;
  Config.CheckUpdates := ChkUpdates.Checked;
  Config.OnLineSearchDict1 := ChkOnLineSearchDict1.Checked;
  Config.WordReferenceLanguage := cbbWordReferenceLanguage.Text;
  //if not(lowercase(MainForm.ConfigObject.ModString) = 'for Italiansubs') then
  //  Config.OnLineSearchDict1 := false;
  Config.OnLineSearchDict2 := ChkOnLineSearchDict2.Checked;
  Config.OnLineSearchDict3 := ChkOnLineSearchDict3.Checked;
  Config.OnLineSearchDict4 := ChkOnLineSearchDict4.Checked;
  Config.OnLineSearchDict5 := ChkOnLineSearchDict5.Checked;
  Config.LinkDictionaries := lbledtLinkDictionaries.Text;
  Config.FlippedPicture := ChkFlippedPicture.Checked;
  Config.FlippedSubtitles := ChkFlippedSubtitles.Checked;
  Config.UTF8AsDefault := ChkUTF8AsDefault.Checked;
  Config.AccentsAssistant := chkAccentsAssistant.Checked;


  // Web server
  Config.ServerPort := UpDownServerPort.Position;
  Config.EnableCompression := chkEnableCompression.Checked;

  // Error plugin
  for i:=0 to ListErrorChecking.Items.Count-1 do
  begin
    JSPluginInfoSrc := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    JSPluginInfoDst := Config.ListJSPlugin[i];
    JSPluginInfoDst.Assign(JSPluginInfoSrc);
  end;

  // Hotkeys
  for i:=0 to ListHotkeys.Items.Count-1 do
  begin
    HLID := ListHotkeys.Items.Item[i].Data;
    THotkeyListItemData(Config.ListHotkeys[i]).Assign(HLID);
  end;

  // Mouse
  Config.MouseWheelTimeScrollModifier := TMouseWheelModifier(ComboWheelTimeScrollModifier.ItemIndex);
  Config.MouseWheelVZoomModifier := TMouseWheelModifier(ComboWheelVZoomModifier.ItemIndex);
  Config.MouseWheelHZoomModifier := TMouseWheelModifier(ComboWheelHZoomModifier.ItemIndex);
  Config.MouseEnableSSATimingMode := chkEnableSSATimingMode.Checked;

  // Backup
  Config.EnableBackup := chkCreateBackup.Checked;
  Config.AutoBackupEvery := UpDownBackupTime.Position;
  Config.AutoSaveWhenPlaying := chkAutoSaveWhenPlaying.Checked;

  // Fonts
  Config.SubListFont := EditSubListFont.Text;
  Config.SubTextFont := EditSubTextFont.Text;
  Config.SubVideoFont := EditSubVideoFont.Text;
  Config.TransparencySubVideoFont := TrackBarTransparencySubVideo.Position;
  Config.OutlineSubVideoFont := UpDownSubVideoOutline.Position;
  Config.OutlineSubVideoColorFont := ShapeSubVideoOutline.Brush.Color;
  Config.TransparencyOutlineSubVideoFont := TrackBarTransparencyOutline.Position;
  Config.ShadowSubVideoFont := UpDownSubVideoShadow.Position;
  Config.ShadowSubVideoColorFont := ShapeSubVideoShadow.Brush.Color;
  Config.TransparencyShadowSubVideoFont := TrackBarTransparencyShadow.Position;

  // WAV Display
  Config.ShowSceneChange := chkSceneChange.Checked;
  Config.SceneChangeStartOffset := UpDownSCStart.Position;
  Config.SceneChangeStopOffset := UpDownSCStop.Position;
  Config.SceneChangeFilterOffset := UpDownSCFilter.Position;
  Config.ShowTextInWAVDisplay := chkShowTextInWAVDisplay.Checked;
  Config.ShowSpecialTagSubs := chkShowSpecialTagSubs.Checked;
  Config.SwapSubVO := chkSwapSubVO.Checked;
  Config.DisableVOShowTextInWAVDisplay := chkDisableVOShowTextInWAVDisplay.Checked;
  Config.EnableShowVideoForNewProject := chkEnableShowVideoForNewProject.Checked;
  Config.EnableDetachedVideoForNewProject := chkEnableDetachedVideoForNewProject.Checked;


  // Itasa
  Config.Username := TntEditUsername.Text;
  Config.Passwd := TntEditPasswd.Text;
  Config.Msg.Text := TntMemoMsg.Text;
  if ChkZipPath.Checked and
    (LabelZipPath.Hint <> ZIP_PATH) then
  begin
    Config.ZipPath := LabelZipPath.Hint;
    Config.ZipPathOnlyForCP := ChkZipOnlyCP.Checked;
  end
  else Config.ZipPath := ZIP_PATH;

  //Default Path
  if ChkDefaultPath.Checked and
    (LblDefaultPath.Hint <> DEF_PATH) then
  begin
    Config.DefaultPath := LblDefaultPath.Hint;
    Config.ForceDefaultPath := ChkForceDefaultPath.Checked;
  end
  else Config.DefaultPath := DEF_PATH;

end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.TempSaveWavColors(Config : TConfigObject);
begin
  Config.WavColors.WAV_COLOR                := ShapeWC.Brush.Color;
  Config.WavColors.WAV_BACK_COLOR           := ShapeWBC.Brush.Color;
  Config.WavColors.CURSOR_COLOR             := ShapeCC.Brush.Color;
  Config.WavColors.RANGE_COLOR_1            := ShapeRC1.Brush.Color;
  Config.WavColors.RANGE_COLOR_2            := ShapeRC2.Brush.Color;
  Config.WavColors.RANGE_COLOR_NOT_EDITABLE := ShapeRCNE.Brush.Color;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttOkClick(Sender: TObject);
begin
  ListPluginParam.EndEditNode;
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListErrorCheckingClick(Sender: TObject);
var i : Integer;
    JSPluginInfo : TJSPluginInfo;
    paramData : PParamData;
    pNode : PVirtualNode;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    ShapeErrorColor.Brush.Color := JSPluginInfo.Color;
    lbErrorDescription.Caption := JSPluginInfo.Description;
    ListPluginParam.Clear;
    for i:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pNode := ListPluginParam.AddChild(nil);
      paramData := ListPluginParam.GetNodeData(pNode);
      paramData.Param := JSPluginInfo.ParamList[i];
      paramData.ValueAsString := GetParamValueAsWideString(paramData.Param);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListErrorCheckingClickCheck(Sender: TObject);
var JSPluginInfo : TJSPluginInfo;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    JSPluginInfo.Enabled := ListErrorChecking.Checked[ListErrorChecking.ItemIndex];
  end;
end;

//------------------------------------------------------------------------------

function TPreferencesForm.GetCurrentModeShortCut(HLID : THotkeyListItemData) : TShortCut;
begin
  Result := 0;
  if Assigned(HLID) then
  begin
    if (ComboHotkeyMode.ItemIndex = 0) then
      Result := HLID.NormalShortCut
    else if (ComboHotkeyMode.ItemIndex = 1) then
      Result := HLID.TimingShortCut;
  end;
end;

//------------------------------------------------------------------------------

function TPreferencesForm.GetCurrentModeShortCutFromList : TShortCut;
var
  HLID : THotkeyListItemData;
begin
  if Assigned(ListHotkeys.ItemFocused) then
  begin
    HLID := ListHotkeys.ItemFocused.Data;
    Result := GetCurrentModeShortCut(HLID);
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SetCurrentModeShortCut(HLID : THotkeyListItemData; ShortCut : TShortCut);
begin
  if Assigned(HLID) then
  begin
    if (ComboHotkeyMode.ItemIndex = 0) then
      HLID.NormalShortCut := ShortCut
    else if (ComboHotkeyMode.ItemIndex = 1) then
      HLID.TimingShortCut := ShortCut;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SetCurrentModeShortCutFromList(ShortCut : TShortCut);
var
  HLID : THotkeyListItemData;
begin
  if Assigned(ListHotkeys.ItemFocused) then
  begin
    HLID := ListHotkeys.ItemFocused.Data;
    SetCurrentModeShortCut(HLID, ShortCut);
    ListHotkeys.ItemFocused.SubItems.Strings[ComboHotkeyMode.ItemIndex] := ShortCutToText(ShortCut);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListHotkeysSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  HotKey1.HotKey := GetCurrentModeShortCutFromList;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ComboHotkeyModeSelect(Sender: TObject);
begin
  HotKey1.HotKey := GetCurrentModeShortCutFromList;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListHotkeysDeletion(Sender: TObject;
  Item: TListItem);
begin
  THotkeyListItemData(Item.Data).Free;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttOpenBackupDirClick(Sender: TObject);
begin
  CheckBackupDirectory;
  Tnt_ShellExecuteW(Handle, 'explore', PWideChar(g_BackupDirectory), nil,
    nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  ParamData: PParamData;
begin
  if (TextType = ttNormal) then
  begin
    ParamData := Sender.GetNodeData(Node);
    case Column of
      0: CellText := ParamData.Param.Name;
      1: CellText := ParamData.ValueAsString;
      2: CellText := ParamData.Param.UnitStr;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  ParamData: PParamData;
begin
  with Sender do
  begin
    ParamData := GetNodeData(Node);
    Allowed := (Column = 1) and (ParamData.Param.ParamType <> jsptUnknown);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditLink.Create;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.WMEndEditing(var Message: TMessage);
begin
  ListPluginParam.EndEditNode;
end;

// =============================================================================
// Adapted from the VTV Advanced demo

destructor TPropertyEditLink.Destroy;
begin
  FEditControl.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.EditIntegerKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then // #8 is Backspace
    Key := #0; // Discard the key
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.EditFloatKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9', '.']) then // #8 is Backspace
    Key := #0; // Discard the key
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
  RootControl : TWinControl;
begin
  case Key of
    VK_RETURN:
      begin
        RootControl := FTree;
        while (RootControl.Parent <> nil) do
          RootControl := RootControl.Parent;
        PostMessage(RootControl.Handle, WM_ENDEDITING, 0, 0);
      end;
    VK_ESCAPE,
    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        if FEditControl is TComboBox then
          CanAdvance := CanAdvance and not TComboBox(FEditControl).DroppedDown;

        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FEditControl.Show;
  FEditControl.SetFocus;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.CancelEdit: Boolean;
begin
  Result := True;
  FEditControl.Hide;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.EndEdit: Boolean;
var
  ParamData: PParamData;
  Buffer: array[0..1024] of Char;
  S: WideString;
begin
  Result := True;

  ParamData := FTree.GetNodeData(FNode);
  if FEditControl is TComboBox then
    S := TComboBox(FEditControl).Text
  else
  begin
    GetWindowText(FEditControl.Handle, Buffer, 1024);
    S := Buffer;
  end;

  if S <> ParamData.ValueAsString then
  begin
    SetParamValueAsWideString(ParamData.Param, S);
    ParamData.ValueAsString := S;
    ParamData.Changed := True;
    FTree.InvalidateNode(FNode);
  end;
  FEditControl.Hide;
  FTree.SetFocus;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.GetBounds: TRect;
begin
  Result := FEditControl.BoundsRect;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  ParamData: PParamData;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // determine what edit type actually is needed
  FEditControl.Free;
  FEditControl := nil;
  ParamData := FTree.GetNodeData(Node);
  case ParamData.Param.ParamType of
    jsptWideString:
      begin
        FEditControl := TTntEdit.Create(nil);
        with FEditControl as TTntEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := ParamData.ValueAsString;
          OnKeyDown := EditKeyDown;
        end;
      end;
    jsptBoolean:
      begin
        FEditControl := TComboBox.Create(nil);
        with FEditControl as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := ParamData.ValueAsString;
          Items.Add(BoolToStr(True));
          Items.Add(BoolToStr(False));
          OnKeyDown := EditKeyDown;
        end;
      end;
    jsptInteger, jsptDouble:
      begin
        FEditControl := TEdit.Create(nil);
        with FEditControl as TEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := ParamData.ValueAsString;
          OnKeyDown := EditKeyDown;
          if ParamData.Param.ParamType = jsptInteger then
            OnKeyPress := EditIntegerKeyPress;
          if ParamData.Param.ParamType = jsptDouble then
            OnKeyPress := EditFloatKeyPress;
        end;
      end;
  else
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);
begin
  FEditControl.WindowProc(Message);
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEditControl.BoundsRect := R;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttSubListFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(EditSubListFont.Font);
  if FontDialog1.Execute then
  begin
    EditSubListFont.Font.Assign(FontDialog1.Font);
    EditSubListFont.Text := Font2String(EditSubListFont.Font);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttSubTextFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(EditSubTextFont.Font);
  if FontDialog1.Execute then
  begin
    EditSubTextFont.Font.Assign(FontDialog1.Font);
    EditSubTextFont.Text := Font2String(EditSubTextFont.Font);
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF enhanced}
procedure TPreferencesForm.bttOpenBackupTempDirClick(Sender: TObject);
var TmpFolder : WideString;
begin
  TmpFolder := GetTemporaryFolder + WideIncludeTrailingBackslash(RootAppData);
  if WideDirectoryExists(TmpFolder) then
  begin
    Tnt_ShellExecuteW(Handle, 'explore', PWideChar(TmpFolder), nil,
      nil, SW_SHOWNORMAL);
  end;
end;
{$ELSE}
procedure TPreferencesForm.bttOpenBackupTempDirClick(Sender: TObject);
var TmpFolder : WideString;
begin
  TmpFolder := GetTemporaryFolder + 'VisualSubSync';
  if WideDirectoryExists(TmpFolder) then
  begin
    Tnt_ShellExecuteW(Handle, 'explore', PWideChar(TmpFolder), nil,
      nil, SW_SHOWNORMAL);
  end;
end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
var
  ParamData: PParamData;
begin
  ParamData := Sender.GetNodeData(Node);
  case Column of
    0: HintText := ParamData.Param.Description;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.pmiSelectAllClick(Sender: TObject);
var i : Integer;
    JSPluginInfo : TJSPluginInfo;
begin
  for i := 0 to ListErrorChecking.Count-1 do
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    ListErrorChecking.Checked[i] := True;
    JSPluginInfo.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.pmiUnselectAllClick(Sender: TObject);
var i : Integer;
    JSPluginInfo : TJSPluginInfo;
begin
  for i := 0 to ListErrorChecking.Count-1 do
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    ListErrorChecking.Checked[i] := False;
    JSPluginInfo.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

function TPreferencesForm.CheckRegistryAccess : Boolean;
begin
  if not IsRegistryCurrentUserRootKeyWritable then
  begin
    MessageBoxW(Handle, PWideChar(WideString('Sorry, but Administrator access rights are needed to change file association. Nothing has been changed.')),
        PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    UpdateAssociationCheckBoxes;
    Result := False;
  end
  else
    Result := True;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.chkAssociateExtVSSPRJClick(Sender: TObject);
begin
  if not CheckRegistryAccess then
    Exit;

  if chkAssociateExtVSSPRJ.Checked then
  begin
    ShellRegisterCurrentUserExtension('vssprj', ApplicationName, 'Document', Application.ExeName, VSSPRJ_ICON_INDEX);
  end
  else
  begin
    ShellUnRegisterCurrentUserExtension('vssprj', ApplicationName, 'Document', Application.ExeName);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.chkAssociateExtSRTClick(Sender: TObject);
begin
  if not CheckRegistryAccess then
    Exit;

  if chkAssociateExtSRT.Checked then
  begin
    ShellRegisterCurrentUserExtension('srt', ApplicationName, 'Document.srt', Application.ExeName, SRT_ICON_INDEX);
  end
  else
  begin
    ShellUnRegisterCurrentUserExtension('srt', ApplicationName, 'Document.srt', Application.ExeName);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.chkAssociateExtSSAClick(Sender: TObject);
begin
  if not CheckRegistryAccess then
    Exit;

  if chkAssociateExtSSA.Checked then
  begin
    ShellRegisterCurrentUserExtension('ssa', ApplicationName, 'Document.ssa', Application.ExeName, SSA_ICON_INDEX);
  end
  else
  begin
    ShellUnRegisterCurrentUserExtension('ssa', ApplicationName, 'Document.ssa', Application.ExeName);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.chkAssociateExtASSClick(Sender: TObject);
begin
  if not CheckRegistryAccess then
    Exit;
    
  if chkAssociateExtASS.Checked then
  begin
    ShellRegisterCurrentUserExtension('ass', ApplicationName, 'Document.ass', Application.ExeName, ASS_ICON_INDEX);
  end
  else
  begin
    ShellUnRegisterCurrentUserExtension('ass', ApplicationName, 'Document.ass', Application.ExeName);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (not ListPluginParam.IsEditing) and (not HotKey1.Focused) then
  begin
    Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TPreferencesForm.ChkQuickSmartSilentZonePanelsClick(
  Sender: TObject);
begin
 TntLabelMoveAfterSmartButtonIsUsed.Font.Color := clWindowText;
 EditMoveAfterSmartButtonIsUsed.Enabled := True;
 ChkMoveAfterSmartButtonIsUsed.Enabled := True;
 if not ChkQuickSmartSilentZonePanels.Checked then
  begin
   TntLabelMoveAfterSmartButtonIsUsed.Font.Color := clGray;
   EditMoveAfterSmartButtonIsUsed.Enabled := False;
   ChkMoveAfterSmartButtonIsUsed.Enabled := False;
  end;
end;

procedure TPreferencesForm.chkDictionaryOnLineEnabledClick(
  Sender: TObject);
begin
  ChkDictionaryOnLineExcludeTextPipe.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict1.Enabled := chkDictionaryOnLineEnabled.Checked;
  cbbWordReferenceLanguage.Enabled := ChkOnLineSearchDict1.Enabled And ChkOnLineSearchDict1.Checked;
  ChkOnLineSearchDict2.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict3.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict4.Enabled := chkDictionaryOnLineEnabled.Checked;
  ChkOnLineSearchDict5.Enabled := chkDictionaryOnLineEnabled.Checked;
end;

procedure TPreferencesForm.ChkUseInternalFiltersClick(Sender: TObject);
begin
 ChkPreferLegacyVideoRenderer.Enabled := True;
 ChkPreferVmr7VideoRenderer.Enabled := True;
 ChkPreferVmr9VideoRenderer.Enabled := True;
 ChkDefaultInternalCodec.Enabled := True;
 ChkAlternativeInternalCodec.Enabled := True;
 ChkLavVHwNone.Enabled := true;
 GroupBoxLavVideoHwModes.Enabled := True;
 GroupBoxCreateProject.Enabled := True;
 ChkForceRegisteredCodecs.Enabled := LocalLavCodecInstalled;
 ChkDoNotUseInternalFiltersOnCreateProject.Enabled := True;
 ChkUseDefaultCodecOnCreateProject.Enabled := True;
 ChkUseAlternativeCodecOnCreateProject.Enabled := True;
 if ChkDoNotUseInternalFiltersOnCreateProject.Checked = true then
  begin
   ChkUseDefaultCodecOnCreateProject.Enabled := False;
   ChkUseAlternativeCodecOnCreateProject.Enabled := False;
  end;
 if LavVideoQuickSyncModeAvaiable = true then ChkLavVQuickSync.Enabled := true;
 if LavVideoCudaAvaiable = true then ChkLavVCuda.Enabled := true;
 if LavVideoDxva2Avaiable = true then ChkLavVDxva2.Enabled := true;
 ChkEnableLavAudioMixing.Enabled := True;
 EditLavPreferredLanguages.Enabled := True;
 if not ChkUseInternalFilters.Checked Or not ChkUseInternalFilters.Enabled then
  begin
   ChkPreferLegacyVideoRenderer.Enabled := False;
   ChkPreferVmr7VideoRenderer.Enabled := False;
   ChkPreferVmr9VideoRenderer.Enabled := False;
   ChkDefaultInternalCodec.Enabled := False;
   ChkAlternativeInternalCodec.Enabled := False;
   GroupBoxLavVideoHwModes.Enabled := False;
   GroupBoxCreateProject.Enabled := False;
   ChkDoNotUseInternalFiltersOnCreateProject.Enabled := False;
   ChkUseDefaultCodecOnCreateProject.Enabled := False;
   ChkUseAlternativeCodecOnCreateProject.Enabled := False;
   ChkLavVHwNone.Enabled := false;
   ChkLavVQuickSync.Enabled := False;
   ChkLavVCuda.Enabled := False;
   ChkLavVDxva2.Enabled := False;
   ChkEnableLavAudioMixing.Enabled := False;
   EditLavPreferredLanguages.Enabled := False;
   ChkForceRegisteredCodecs.Enabled := False;
  end;
 {$IFDEF enhanced}
 ChkDefaultInternalCodec.Enabled := False;
 ChkUseDefaultCodecOnCreateProject.Enabled := False;
 ChkUseAlternativeCodecOnCreateProject.Enabled := False;
 {$ENDIF}
end;

procedure TPreferencesForm.FormDestroy(Sender: TObject);
begin
 ClearErrorList;
end;

procedure TPreferencesForm.FormCreate(Sender: TObject);
begin
  PageControlPreferences.ActivePage := tsGeneral;
  ListPluginParam.NodeDataSize := SizeOf(TParamData);
  ListPluginParam.TreeOptions.MiscOptions := ListPluginParam.TreeOptions.MiscOptions
    + [toEditOnDblClick] - [toToggleOnDblClick];
  EditLavPreferredLanguages.Hint := 'Enter your preferred languages as their 3-letter language codes, comma separated. ' + #13+ #10+ '(example: eng,ger,fre) http://en.wikipedia.org/wiki/List_of_ISO_639-2_codes';
  tsItasa.TabVisible := (lowercase(MainForm.ConfigObject.ModString) = 'for italiansubs');
end;

procedure TPreferencesForm.FormActivate(Sender: TObject);
begin
 Self.ClientHeight := 540;
 Self.ClientWidth := 740;
 UpdateAssociationCheckBoxes;
end;

procedure TPreferencesForm.UpdateLavVideoCodecHwModeSupported;
 var Renderer : TDShowRenderer; ModesAvaiable:String;
begin

 LavVideoQuickSyncModeAvaiable:=false;
 LavVideoCudaAvaiable:=false;
 LavVideoDxva2Avaiable:=false;

 Screen.Cursor := crHourGlass;

 Renderer := TDShowRenderer.Create;

 ModesAvaiable := Renderer.GetLavHwModeSupported;

 Renderer.Close;

 Screen.Cursor := crDefault;

 if pos('Intel Quick Sync', ModesAvaiable) >= 1 then LavVideoQuickSyncModeAvaiable:=true;
 if pos('Cuda', ModesAvaiable) >= 1 then LavVideoCudaAvaiable:=true;
 if pos('Dxva2(copy-back)', ModesAvaiable) >= 1  then LavVideoDxva2Avaiable:=true;

 if ChkAlternativeInternalCodec.Checked = true then
  begin
   if LavVideoQuickSyncModeAvaiable = false then ChkLavVQuickSync.Enabled := false else ChkLavVQuickSync.Enabled := true;
   if LavVideoCudaAvaiable = false then ChkLavVCuda.Enabled := false else ChkLavVCuda.Enabled := true;
   if LavVideoDxva2Avaiable = false then ChkLavVDxva2.Enabled := false else ChkLavVDxva2.Enabled := true;
  end
 else
  begin
   ChkLavVQuickSync.Enabled := False;
   ChkLavVCuda.Enabled := False;
   ChkLavVDxva2.Enabled := False;
  end;

end;

procedure TPreferencesForm.ChkDefaultInternalCodecClick(Sender: TObject);
begin
 ChkLavVHwNone.Enabled := False;
 ChkLavVQuickSync.Enabled := False;
 ChkLavVCuda.Enabled := False;
 ChkLavVDxva2.Enabled := False;
 ChkEnableLavAudioMixing.Enabled := False;
 EditLavPreferredLanguages.Enabled := False;
 if not ChkDefaultInternalCodec.Checked AND ChkUseInternalFilters.Enabled then
  begin
   ChkLavVHwNone.Enabled := True;
   if LavVideoQuickSyncModeAvaiable = true then ChkLavVQuickSync.Enabled := true;
   if LavVideoCudaAvaiable = true then ChkLavVCuda.Enabled := true;
   if LavVideoDxva2Avaiable = true then ChkLavVDxva2.Enabled := true;
   ChkEnableLavAudioMixing.Enabled := True;
   EditLavPreferredLanguages.Enabled := True;
  end;
end;

procedure TPreferencesForm.ChkAlternativeInternalCodecClick(
  Sender: TObject);
begin
 ChkLavVHwNone.Enabled := False;
 ChkLavVQuickSync.Enabled := False;
 ChkLavVCuda.Enabled := False;
 ChkLavVDxva2.Enabled := False;
 ChkEnableLavAudioMixing.Enabled := False;
 EditLavPreferredLanguages.Enabled := False;
 if ChkAlternativeInternalCodec.Checked AND ChkUseInternalFilters.Enabled then
  begin
   UpdateLavVideoCodecHwModeSupported;
   ChkLavVHwNone.Enabled := True;
   if LavVideoQuickSyncModeAvaiable = true then ChkLavVQuickSync.Enabled := true;
   if LavVideoCudaAvaiable = true then ChkLavVCuda.Enabled := true;
   if LavVideoDxva2Avaiable = true then ChkLavVDxva2.Enabled := true;
   ChkEnableLavAudioMixing.Enabled := True;
   EditLavPreferredLanguages.Enabled := True;
  end;
end;

procedure TPreferencesForm.TntSpeedButtonCheckLoginClick(Sender: TObject);
begin
  g_ItaliansubsAuthorization := False;
  ImgLoginOK.Visible := False;
  ImgLoginKO.Visible := False;

  if ItaliansubsAuthorization(TntEditUsername.Text,TntEditPasswd.Text) then
  begin
     ImgLoginOK.Visible := True;
     ItaliansubsOptionsGroupBox.Visible:=True;
  end
  else
  begin
      ImgLoginKO.Visible := True;
      ItaliansubsOptionsGroupBox.Visible:=False;
  end;
end;

procedure TPreferencesForm.AddTag(var Msg: string; tag:string; ids, len:Integer);
var
  tmsg:string;
begin
  tmsg:= Copy( Msg, 1, ids) + '['+tag+']'
       + Copy( Msg, ids + 1, len) + '[/'+tag+']'
       + Copy( Msg, ids + len + 1, Length(Msg));
  Msg:=tmsg;
end;

procedure TPreferencesForm.ButtonBoldClick(Sender: TObject);
var tmsg : string;
begin
  tmsg:=TntMemoMsg.Text;
  AddTag(tmsg,'b',TntMemoMsg.SelStart,TntMemoMsg.SelLength);
  TntMemoMsg.Text:=tmsg;
end;

procedure TPreferencesForm.ButtonItalicClick(Sender: TObject);
var tmsg : string;
begin
  tmsg:=TntMemoMsg.Text;
  AddTag(tmsg,'i',TntMemoMsg.SelStart,TntMemoMsg.SelLength);
  TntMemoMsg.Text:=tmsg;
end;

procedure TPreferencesForm.ButtonUnderlineClick(Sender: TObject);
var tmsg : string;
begin
  tmsg:=TntMemoMsg.Text;
  AddTag(tmsg,'u',TntMemoMsg.SelStart,TntMemoMsg.SelLength);
  TntMemoMsg.Text:=tmsg;
end;

procedure TPreferencesForm.ChkCustomTextClick(Sender: TObject);
begin
  EditCustomText.Visible := ChkCustomText.Checked;
end;

procedure TPreferencesForm.ShapeWCMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Color := TTntShape(Sender).Brush.Color;
  if ColorDialog1.Execute then
    TTntShape(Sender).Brush.Color := ColorDialog1.Color;
  MainForm.WAVDisplayer1UpdateColors(nil);
end;

procedure TPreferencesForm.ButtonDefaultWavColoursClick(Sender: TObject);
begin
  ShapeWC.Brush.Color   := $00A7F24A;
  ShapeWBC.Brush.Color  := clBlack;
  ShapeCC.Brush.Color   := clYellow;
  ShapeRC1.Brush.Color  := $003333FF;
  ShapeRC2.Brush.Color  := $00FF8000;
  ShapeRCNE.Brush.Color := $00C0C0C0;
  MainForm.WAVDisplayer1UpdateColors(nil);
end;

procedure TPreferencesForm.LabelZipPathClick(Sender: TObject);
var seldir:String;
begin
  if SelectDirectory(ZIP_PATH,'',seldir) then
  begin
    LabelZipPath.Caption := seldir;
    LabelZipPath.Hint := seldir;
  end;
end;

procedure TPreferencesForm.LabelZipPathMouseEnter(Sender: TObject);
begin
  TTntLabel(Sender).Font.Style:=TTntLabel(Sender).Font.Style + [fsUnderline];
end;

procedure TPreferencesForm.LabelZipPathMouseLeave(Sender: TObject);
begin
  TTntLabel(Sender).Font.Style:=TTntLabel(Sender).Font.Style - [fsUnderline];
end;

procedure TPreferencesForm.ChkZipPathClick(Sender: TObject);
begin
  if ChkZipPath.Checked then
  begin
    LabelZipPath.Enabled:=True;
    LabelZipPath.Caption:=ZIP_PATH;
    ChkZipOnlyCP.Enabled:=True;
  end
  else
  begin
    LabelZipPath.Enabled:=False;
    ChkZipOnlyCP.Enabled:=False;
  end;
end;

procedure TPreferencesForm.ChkDoNotUseInternalFiltersOnCreateProjectClick(
  Sender: TObject);
begin
 ChkUseDefaultCodecOnCreateProject.Enabled := True;
 ChkUseAlternativeCodecOnCreateProject.Enabled := True;
 if ChkDoNotUseInternalFiltersOnCreateProject.Checked = true then
  begin
   ChkUseDefaultCodecOnCreateProject.Enabled := False;
   ChkUseAlternativeCodecOnCreateProject.Enabled := False;
  end;
 {$IFDEF enhanced}
 ChkUseDefaultCodecOnCreateProject.Enabled := False;
 ChkUseAlternativeCodecOnCreateProject.Enabled := False;
 {$ENDIF}
end;

procedure TPreferencesForm.ChkUseDefaultCodecOnCreateProjectClick(
  Sender: TObject);
begin
 if ChkUseDefaultCodecOnCreateProject.Checked = True then
  ChkUseAlternativeCodecOnCreateProject.Checked := False;
end;

procedure TPreferencesForm.ChkUseAlternativeCodecOnCreateProjectClick(
  Sender: TObject);
begin
 if ChkUseAlternativeCodecOnCreateProject.Checked = True then
  ChkUseDefaultCodecOnCreateProject.Checked := False;
end;

procedure TPreferencesForm.bttResetAllHotkeysClick(Sender: TObject);
var i,j : integer;
    HLID, HLID2 : THotkeyListItemData;
begin
  if not Assigned(ListDefaultHotkeys) then
    Exit;
  for i:=0 to ListHotkeys.Items.Count-1 do
  begin
    HLID := ListHotkeys.Items.Item[i].Data;
    // Search based on action, cause List is alpha. sorted
    for j:=0 to ListDefaultHotkeys.Count-1 do
    begin
      HLID2 := ListDefaultHotkeys[j];
      if (HLID.Action = HLID2.Action) then
      begin
        HLID.NormalShortCut := HLID2.NormalShortCut;
        ListHotkeys.Items.Item[i].SubItems[0] := ShortCutToText(HLID2.NormalShortCut);
        HLID.TimingShortCut := HLID2.TimingShortCut;
        ListHotkeys.Items.Item[i].SubItems[1] := ShortCutToText(HLID2.TimingShortCut);
        Break;
      end;
    end;
  end;
end;

procedure TPreferencesForm.bttSetHotkeyClick(Sender: TObject);
var i : integer;
    HLID : THotkeyListItemData;
    Shortcut : TShortCut;
begin
  // First check if the hotkey is not already used
  for i:=0 to ListHotkeys.Items.Count-1 do
  begin
    HLID := ListHotkeys.Items.Item[i].Data;
    Shortcut := GetCurrentModeShortCut(HLID);
    if (Shortcut <> 0) and (Shortcut = HotKey1.HotKey) then
    begin
      // Clear the hotkey
      SetCurrentModeShortCut(HLID, 0);
      ListHotkeys.Items.Item[i].SubItems.Strings[ComboHotkeyMode.ItemIndex] := ShortCutToText(0);
      Break;
    end;
  end;

  // Set the hotkey
  SetCurrentModeShortCutFromList(HotKey1.HotKey);
end;

procedure TPreferencesForm.bttClearHotkeyClick(Sender: TObject);
begin
  SetCurrentModeShortCutFromList(0);
end;

procedure TPreferencesForm.ChkDefaultPathClick(Sender: TObject);
begin
  if ChkDefaultPath.Checked then
  begin
    LblDefaultPath.Enabled := True;
    ChkForceDefaultPath.Enabled := True;
  end
  else
  begin
    LblDefaultPath.Enabled := False;
    ChkForceDefaultPath.Enabled := False;
  end;
end;

procedure TPreferencesForm.LblDefaultPathClick(Sender: TObject);
var seldir:String;
begin
  if SelectDirectory(DEF_PATH,'',seldir) then
  begin
    LblDefaultPath.Caption := seldir;
    LblDefaultPath.Hint := seldir;
  end;
end;

procedure TPreferencesForm.bttSubVideoFontClick(Sender: TObject);
var
  FontName : TFontName; FontSize : Integer; FontStyle : TFontStyles; FontCharset : TFontCharset; FontColor : TColor;
begin
  String2FontProperties(EditSubVideoFont.Text,FontName,FontSize,FontStyle,FontCharset,FontColor);
  FontDialog1.Font.Name := FontName;
  FontDialog1.Font.Size := FontSize;
  FontDialog1.Font.Style := FontStyle;
  FontDialog1.Font.Charset := FontCharset;
  FontDialog1.Font.Color := FontColor;
  if FontDialog1.Execute then
  begin
    EditSubVideoFont.Font.Assign(FontDialog1.Font);
    EditSubVideoFont.Font.Color := clWindowText;
    EditSubVideoFont.Text := Font2String(FontDialog1.Font);
  end;
end;

procedure TPreferencesForm.ShapeSubVideoOutlineMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Color := TTntShape(Sender).Brush.Color;
  if ColorDialog1.Execute then
    TTntShape(Sender).Brush.Color := ColorDialog1.Color;
end;

procedure TPreferencesForm.ShapeSubVideoShadowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Color := TTntShape(Sender).Brush.Color;
  if ColorDialog1.Execute then
    TTntShape(Sender).Brush.Color := ColorDialog1.Color;
end;

procedure TPreferencesForm.ButtonDefaultSubtitlesVideoClick(
  Sender: TObject);
begin
  EditSubVideoFont.Text := 'Arial,18,1,0,clWhite';
  TrackBarTransparencySubVideo.Position := 0;
  UpDownSubVideoOutline.Position := 2;
  ShapeSubVideoOutline.Brush.Color := clBlack;
  TrackBarTransparencyOutline.Position := 0;
  UpDownSubVideoShadow.Position := 3;
  ShapeSubVideoShadow.Brush.Color := clBlack;
  TrackBarTransparencyShadow.Position := 4;
end;

procedure TPreferencesForm.chkOnLineSearchDict1Click(Sender: TObject);
begin
  cbbWordReferenceLanguage.Enabled := chkOnLineSearchDict1.Checked;
end;

procedure TPreferencesForm.TimerCheckCodecTimer(Sender: TObject);
begin
 ChkDefaultInternalCodecClick(self);
 ChkAlternativeInternalCodecClick(self);
 TimerCheckCodec.enabled := False;
end;

end.
//------------------------------------------------------------------------------

