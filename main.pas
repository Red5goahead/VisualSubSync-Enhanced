// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003-2007 Christophe Paris
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
//
// SSA/ASS loading code contribution by Mirage (2005)
//
// -----------------------------------------------------------------------------

unit main;

interface

uses
  GlobalUnit, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, WAVDisplayerUnit, SubStructUnit, ComCtrls, Menus, Renderer,
  MiniScrollBarUnit, VirtualTrees, MiscToolsUnit, TntStdCtrls, TntMenus,
  Buttons, TntActnList, ImgList, ActnList, TntDialogs, TntComCtrls,
  PeakCreationProgressFormUnit, ProjectUnit, ServerUnit, TntExtCtrls, IniFiles,
  PreferencesFormUnit, MRUListUnit, StdActns, TntStdActns, TntButtons, TntForms,
  DetachedVideoFormUnit, XPMan, JavaScriptPluginUnit, Contnrs, DelayFormUnit,
  UndoableTaskUnit, UndoableSubTaskUnit, Types, LibHunspellUnit, TranslateFormUnit,
  OleCtrls, SHDocVw, VistaAltFixUnit, XMLDoc, xmldom, XMLIntf, msxmldom;

type
  // -----

  TPlayingModeType = (pmtAll, pmtSelection, pmtSelectionStart, pmtSelectionEnd);
  TTagType = (ttItalic, ttBold, ttUnderline, ttColor, ttSize, ttPosBottomLeft, ttPosBottomMiddle, ttPosBottomRight, ttPosTopLeft, ttPosTopMiddle, ttPosTopRight, ttPosMiddleRight, ttPosMiddleLeft, ttPosMiddleMiddle);

  TMainForm = class(TTntForm, TVSSCoreEngineIntf)
    TntMainMenu1: TTntMainMenu;
    MenuItemFile: TTntMenuItem;
    MenuItemNewProject: TTntMenuItem;
    MenuItemOpenProject: TTntMenuItem;
    N1: TTntMenuItem;
    MenuItemSave: TTntMenuItem;
    MenuItemSaveAs: TTntMenuItem;
    N2: TTntMenuItem;
    MenuItemExit: TTntMenuItem;
    PanelTop: TPanel;
    PanelVideo: TPanel;
    PanelWAVDisplay: TPanel;
    SplitterWAVDisplay_Video: TSplitter;
    MenuItemEdit: TTntMenuItem;
    MenuItemView: TTntMenuItem;
    TntActionList1: TTntActionList;
    ActionZoomIn: TTntAction;
    ImageList1: TImageList;
    ActionZoomOut: TTntAction;
    ActionZoomSelection: TTntAction;
    ActionZoomAll: TTntAction;
    ActionPlay: TTntAction;
    ActionStop: TTntAction;
    ActionLoop: TTntAction;
    MenuItemZoomIn: TTntMenuItem;
    MenuItemZoomOut: TTntMenuItem;
    MenuItemZoomSelection: TTntMenuItem;
    MenuItemZoomAll: TTntMenuItem;
    ActionExit: TTntAction;
    ActionNewProject: TTntAction;
    ActionShowHideVideo: TTntAction;
    N3: TTntMenuItem;
    MenuItemShowHideVideo: TTntMenuItem;
    TntOpenDialog1: TTntOpenDialog;
    ActionOpenProject: TTntAction;
    N4: TTntMenuItem;
    MenuItemProjectProperties: TTntMenuItem;
    ActionFind: TTntAction;
    MenuItemFind: TTntMenuItem;
    ActionSave: TTntAction;
    ActionSaveAs: TTntAction;
    TntSaveDialog1: TTntSaveDialog;
    MenuItemPlayback: TTntMenuItem;
    MenuItemPlay: TTntMenuItem;
    MenuItemStop: TTntMenuItem;
    MenuItemLoop: TTntMenuItem;
    SubListPopupMenu: TTntPopupMenu;
    ActionProjectProperties: TTntAction;
    pmiSubListDelete: TTntMenuItem;
    pmiSubListMerge: TTntMenuItem;
    MenuItemFindNext: TTntMenuItem;
    ActionFindNext: TTntAction;
    TimerStatusBarMsg: TTimer;
    MenuItemHelpRoot: TTntMenuItem;
    MenuItemAbout: TTntMenuItem;
    N5: TTntMenuItem;
    MenuItemPreferences: TTntMenuItem;
    Splitter2: TSplitter;
    N6: TTntMenuItem;
    SubMenuItemErrorChecking: TTntMenuItem;
    MenuItemCheckErrors: TTntMenuItem;
    MenuItemErrorCheckingPreferences: TTntMenuItem;
    MenuItemDelay: TTntMenuItem;
    ActionCheckErrors: TTntAction;
    ActionDelay: TTntAction;
    ActionShowErrorReport: TTntAction;
    MenuItemShowErrorReport: TTntMenuItem;
    ActionNextSub: TTntAction;
    ActionPreviousSub: TTntAction;
    NextSub1: TTntMenuItem;
    PreviousSub1: TTntMenuItem;
    pmiSubListDelay: TTntMenuItem;
    TimerAutoScrollSub: TTimer;
    Goto1: TTntMenuItem;
    ActionGoto: TTntAction;
    SubMenuItemWebServer: TTntMenuItem;
    MenuItemStartWebServer: TTntMenuItem;
    MenuItemStopWebServer: TTntMenuItem;
    N7: TTntMenuItem;
    MenuItemShowSuggestions: TTntMenuItem;
    MenuItemOpenRecentRoot: TTntMenuItem;
    ActionPreferences: TTntAction;
    MenuItemClose: TTntMenuItem;
    ActionClose: TTntAction;
    ActionErrorPreferences: TTntAction;
    ActionShowSuggestions: TTntAction;
    MenuItemHelp: TTntMenuItem;
    N8: TTntMenuItem;
    MemoSubPopupMenu: TTntPopupMenu;
    pmiMemoSubCopy: TTntMenuItem;
    pmiMemoSubCut: TTntMenuItem;
    pmiMemoSubPaste: TTntMenuItem;
    EditCut1: TTntEditCut;
    EditSelectAll1: TTntEditSelectAll;
    EditDelete1: TTntEditDelete;
    pmiMemoSubUndo: TTntMenuItem;
    N9: TTntMenuItem;
    pmiMemoSubDelete: TTntMenuItem;
    N10: TTntMenuItem;
    pmiMemoSubSelectAll: TTntMenuItem;
    MenuItemHelpIndex: TTntMenuItem;
    MenuItemHelpIndexWAVDisplayControl: TTntMenuItem;
    N11: TTntMenuItem;
    ActionInsertTextFile: TTntAction;
    Inserttextfile1: TTntMenuItem;
    ActionClearSelection: TTntAction;
    ActionLoopSelStart: TTntAction;
    ActionLoopSelEnd: TTntAction;
    ActionAddSubtitle: TTntAction;
    ActionSetSubtitleTime: TTntAction;
    ActionSetPlaybackRate80: TTntAction;
    ActionSetPlaybackRate90: TTntAction;
    ActionSetPlaybackRate100: TTntAction;
    N12: TTntMenuItem;
    Rate1: TTntMenuItem;
    Setplaybackrateat1001: TTntMenuItem;
    Setplaybackrateat901: TTntMenuItem;
    Setplaybackrateat801: TTntMenuItem;
    Loopselectionstart1: TTntMenuItem;
    Loopselectionend1: TTntMenuItem;
    ActionShiftStartPlus100: TTntAction;
    ActionShiftStartMinus100: TTntAction;
    ActionShiftStopPlus100: TTntAction;
    ActionShiftStopMinus100: TTntAction;
    ActionShiftStartPlus10: TTntAction;
    ActionShiftStartMinus10: TTntAction;
    ActionShiftStopPlus10: TTntAction;
    ActionShiftStopMinus10: TTntAction;
    MenuItemVerticalScaling: TTntMenuItem;
    ActionZoomVertical: TTntAction;
    MemoTextPipe: TTntRichEdit;
    SplitterMemoTextPipe: TSplitter;
    ActionShowHideTextPipe: TTntAction;
    MenuItemTextPipe: TTntMenuItem;
    MenuItemLoadTextFileInPipe: TTntMenuItem;
    MenuItemShowHideTextPipe: TTntMenuItem;
    ActionLoadTextPipe: TTntAction;
    MemoTextPipePopupMenu: TTntPopupMenu;
    pmiLoadTextPipe: TTntMenuItem;
    pmiAddAsSubtitle: TTntMenuItem;
    ActionClearTextPipe: TTntAction;
    MenuItemClearTextPipe: TTntMenuItem;
    ActionSaveTextPipeAs: TTntAction;
    MenuItemSaveTextPipe: TTntMenuItem;
    ActionAddSubFromPipe: TTntAction;
    N13: TTntMenuItem;
    pmiAutoColorizeText: TTntMenuItem;
    pmiAutoDeleteText: TTntMenuItem;
    pmiAutoDeleteAllTextBefore: TTntMenuItem;
    ActionReplaceFromPipe: TTntAction;
    pmiReplaceSubtitleFromPipe: TTntMenuItem;
    TimerAutoBackup: TTimer;
    ActionDetachVideo: TTntAction;
    MenuItemDetachVideoWindow: TTntMenuItem;
    N14: TTntMenuItem;
    ShowHidelogs1: TTntMenuItem;
    ActionShowHideLogs: TTntAction;
    pmiFixError: TTntMenuItem;
    ActionFixErrorMain: TTntAction;
    ActionStartSub: TTntAction;
    ActionStopSub: TTntAction;
    ActionStopAndStartSub: TTntAction;
    N15: TTntMenuItem;
    ShowHideTextPipe1: TTntMenuItem;
    ActionInsertKaraokeMarker: TTntAction;
    pmiCreateKaraoke: TTntMenuItem;
    pmiClearKaraoke: TTntMenuItem;
    ActionShowStartFrame: TTntAction;
    ActionShowStopFrame: TTntAction;
    ActionShowFrameAtCursor: TTntAction;
    N16: TTntMenuItem;
    ActionExportToSSA: TTntAction;
    MenuItemExportToWAV: TTntMenuItem;
    ActionExportToWAV: TTntAction;
    ActionPlaySelStart: TTntAction;
    ActionPlaySelEnd: TTntAction;
    Playselectionstart1: TTntMenuItem;
    Playselectionend1: TTntMenuItem;
    ActionPlaceKaraokeCursorsAtEnd: TTntAction;
    ActionPlayToEnd: TTntAction;
    MenuItemPlayToEnd: TTntMenuItem;
    ActionPlay1sBefore: TTntAction;
    ActionSelectNextSub: TTntAction;
    ActionSelectPreviousSub: TTntAction;
    SubListHeaderPopupMenu: TTntPopupMenu;
    ActionSetSubtitleStartTime: TTntAction;
    ActionSetSubtitleStopTime: TTntAction;
    ActionSetSubtitleStopTimeAndGoToNext: TTntAction;
    ActionStyles: TTntAction;
    Styles1: TTntMenuItem;
    ActionSetPlaybackRate70: TTntAction;
    ActionSetPlaybackRate60: TTntAction;
    Setplaybackrateat701: TTntMenuItem;
    Setplaybackrateat601: TTntMenuItem;
    ActionPause: TTntAction;
    MenuItemPause: TTntMenuItem;
    ActionToggleTimingMode: TTntAction;
    ActionSetEditorFocus: TTntAction;
    ActionSetEditorFocusAndSelect: TTntAction;
    MenuItemShowFrameAtCursor: TTntMenuItem;
    MenuItemShowStartFrame: TTntMenuItem;
    MenuItemShowStopFrame: TTntMenuItem;
    ActionNextError: TTntAction;
    StatusBarMainPanel: TPanel;
    ActionUndo: TTntAction;
    ActionRedo: TTntAction;
    Undo2: TTntMenuItem;
    Redo1: TTntMenuItem;
    N17: TTntMenuItem;
    ActionWAVDisplayScrollRight: TTntAction;
    ActionWAVDisplayScrollLeft: TTntAction;
    ActionShowHideSceneChange: TTntAction;
    N18: TTntMenuItem;
    ActionTextItalic: TTntAction;
    ActionTextBold: TTntAction;
    pmiMemoSubItalic: TTntMenuItem;
    ActionTextUnderline: TTntAction;
    ActionTextColor: TTntAction;
    ActionTextSize: TTntAction;
    pmiMemoSubOtherStyle: TTntMenuItem;
    pmiMemoSubExtInBold: TTntMenuItem;
    pmiMemoSubExtinUnderline: TTntMenuItem;
    pmiMemoSubExtColor: TTntMenuItem;
    pmiMemoSubExtSize: TTntMenuItem;
    N19: TTntMenuItem;
    ActionStripTags: TTntAction;
    pmiStriptags: TTntMenuItem;
    ActionMerge: TTntAction;
    ActionSplitAtCursor: TTntAction;
    extinitalic1: TTntMenuItem;
    Otherstyles1: TTntMenuItem;
    extinbold2: TTntMenuItem;
    extinunderline2: TTntMenuItem;
    extcolor2: TTntMenuItem;
    extsize2: TTntMenuItem;
    MenuItemOpenProjectFolder: TTntMenuItem;
    ActionOpenProjectFolder: TTntAction;
    ActionPlay1sBeforeToEnd: TTntAction;
    ActionDeleteSceneChange: TTntAction;
    MenuItemShowHideSceneChange: TTntMenuItem;
    ActionReload: TTntAction;
    Reload1: TTntMenuItem;
    ActionMergeWithPrevious: TTntAction;
    ActionMergeWithNext: TTntAction;
    ActionMergeDialog: TTntAction;
    ActionFixSelectedErrors: TTntAction;
    pmiSubListMergeDialog: TTntMenuItem;
    ActionMergeOnOneLine: TTntAction;
    MenuItemJSTools: TTntMenuItem;
    N20: TTntMenuItem;
    ActionLoadPresets: TTntAction;
    MenuItemLoadPresets: TTntMenuItem;
    ActionCopy: TTntAction;
    OpenDialogPresets: TTntOpenDialog;
    ActionShowSilentZones: TTntAction;
    Showsilentzones1: TTntMenuItem;
    N21: TTntMenuItem;
    ActionLiveSpellCheck: TTntAction;
    SubMenuItemSpellcheck: TTntMenuItem;
    MenuItemLiveSpellcheck: TTntMenuItem;
    N22: TTntMenuItem;
    N23: TTntMenuItem;
    MenuItemGetMoreDictionaries: TTntMenuItem;
    ActionSpellCheck: TTntAction;
    MenuItemSpellcheck: TTntMenuItem;
    StatusBarPanel1: TTntPanel;
    StatusBarPanel2: TTntPanel;
    ActionInsertSceneChange: TTntAction;
    ActionPaste: TTntAction;
    ActionPasteAtCursor: TTntAction;
    ActionToggleVO: TTntAction;
    N25: TTntMenuItem;
    MenuItemShowHideReferenceVO: TTntMenuItem;
    ActionShowHideSubs: TTntAction;
    MenuShowHideSubtitles: TTntMenuItem;
    ActionTranslationTemplate: TTntAction;
    MenuItemTranslationTemplate: TTntMenuItem;
    WAVDisplayPopupMenu: TTntPopupMenu;
    pmiWAVDispAddSubtitle: TTntMenuItem;
    pmiWAVDispDeleteRange: TTntMenuItem;
    pmiWAVDispSplitAtCursor: TTntMenuItem;
    pmiWAVDispSetSubtitleTime: TTntMenuItem;
    pmiWAVDispPasteAtCursor: TTntMenuItem;
    N24: TTntMenuItem;
    pmiInsertSC: TTntMenuItem;
    pmiDeleteSC: TTntMenuItem;
    pmiInsertKaraokeMarker: TTntMenuItem;
    ImageListWavDisplayer: TImageList;
    PageControlMain: TPageControl;
    TabSheetSub: TTabSheet;
    Splitter1: TSplitter;
    PanelMiddle: TPanel;
    vtvSubsList: TVirtualStringTree;
    PanelPlaybackControl: TPanel;
    bttPlay: TSpeedButton;
    bttStop: TSpeedButton;
    bttPause: TSpeedButton;
    bttLoop: TSpeedButton;
    bttZoomIn: TSpeedButton;
    bttZoomOut: TSpeedButton;
    bttShowHideVideo: TSpeedButton;
    bttCheckErrors: TSpeedButton;
    bttPreviousSub: TSpeedButton;
    bttNextSub: TSpeedButton;
    bttZoomSel: TSpeedButton;
    bttZoomAll: TSpeedButton;
    bttShowSuggestions: TSpeedButton;
    bttWorkingMode: TTntSpeedButton;
    bttZoomVertical: TSpeedButton;
    bttPlayToEnd: TSpeedButton;
    bttShowPreferences: TSpeedButton;
    lblVolume: TTntLabel;
    tbVolume: TTntTrackBar;
    Panel1: TPanel;
    lbAutoScroll: TTntLabel;
    chkAutoScrollSub: TCheckBox;
    chkAutoScrollWAVDisp: TTntCheckBox;
    plCursorPos: TPanel;
    PanelTimeSelView: TPanel;
    Label1: TLabel;
    LabelSelLenght: TLabel;
    LabelSelEnd: TLabel;
    LabelSelBegin: TLabel;
    Label5: TLabel;
    edSelLength: TEdit;
    edSelEnd: TEdit;
	  edSelBegin: TEdit;
    edViewBegin: TEdit;
    edViewEnd: TEdit;
    edViewLength: TEdit;
    bttStyles: TTntButton;
    cbStyles: TTntComboBox;
    PageQuickSmartSilentZonePanels: TPageControl;
    TabSheetQuickDelay: TTabSheet;
    LabelFastShiftDelayNegative: TLabel;
    bttFastLess200ms: TSpeedButton;
    bttFastLess100ms: TSpeedButton;
    bttFastLess50ms: TSpeedButton;
    bttFastLess10ms: TSpeedButton;
    bttFastSmartWavCursor: TSpeedButton;
    bttFastMore50ms: TSpeedButton;
    bttFastMore100ms: TSpeedButton;
    bttFastMore200ms: TSpeedButton;
    bttFastMore10ms: TSpeedButton;
    LabelFastShiftDelayPositive: TLabel;
    TabSheetSilentZone: TTabSheet;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    SpeedButtonUpQuickSilentZone: TSpeedButton;
    edThreshold: TTntEdit;
    udThreshold: TTntUpDown;
    edDuration: TTntEdit;
    udDuration: TTntUpDown;
    ComboBoxSilentZone: TComboBox;
    PanelBottom: TPanel;
    SplitterSubtitleVO: TTntSplitter;
    MemoSubtitleText: TTntRichEdit;
    MemoLinesCounter: TTntRichEdit;
    MemoSubtitleVO: TTntRichEdit;
    TabSheetUrbanDictionary: TTabSheet;
    WebBrowserUrbanDictionary: TWebBrowser;
    TabSheetWordNet: TTabSheet;
    WebBrowserWordNet: TWebBrowser;
    MenuItemQuickSmartDelay: TTntMenuItem;
    MenuItemSynchToCursor: TTntMenuItem;
    ActionDelaySynchToCursor: TTntAction;
    N26: TTntMenuItem;
    ActionDelayMoveNegative50: TTntAction;
    ActionDelayMoveNegative10: TTntAction;
    Movecurrentsubtitleby50Ms1: TTntMenuItem;
    Movecurrentsubtitleby10Ms1: TTntMenuItem;
    N27: TTntMenuItem;
    ActionDelayMoveNegative100: TTntAction;
    ActionDelayMoveNegative200: TTntAction;
    ActionDelayMovePositive10: TTntAction;
    ActionDelayMovePositive50: TTntAction;
    ActionDelayMovePositive100: TTntAction;
    ActionDelayMovePositive200: TTntAction;
    Movecurrentsubtitleby100Ms1: TTntMenuItem;
    Movecurrentsubtitleby200Ms1: TTntMenuItem;
    Movecurrentsubtitleby10Ms2: TTntMenuItem;
    Movecurrentsubtitleby50Ms2: TTntMenuItem;
    Movecurrentsubtitleby100Ms2: TTntMenuItem;
    Movecurrentsubtitleby200Ms2: TTntMenuItem;
    ActionScrollTextPipeUp: TTntAction;
    ActionScrollTextPipeDown: TTntAction;
    SpeedButtonGoCurrentSilentZone: TSpeedButton;
    ActionMoveForwardToClosestSilentZone: TTntAction;
    ActionMoveBackwardToClosestSilentZone: TTntAction;
    MenuItemsWaveCorrection: TTntMenuItem;
    MenuItemActionShiftForwardWaveForm: TTntMenuItem;
    ActionShiftForwardWaveForm: TTntAction;
    ActionShiftBackwardWaveForm: TTntAction;
    MenuItemActionShiftBackwardWaveForm: TTntMenuItem;
    ChangeFpsCinemaToPal: TTntMenuItem;
    MenuItemSmartDesynch: TTntMenuItem;
    ActionResynchToolsReset: TTntAction;
    ActionResynchToolsSetPos: TTntAction;
    ActionResynchToolsDelPos: TTntAction;
    MenuItemRTNew: TTntMenuItem;
    MenuItemRTDelete: TTntMenuItem;
    MenuItemRTReset: TTntMenuItem;
    MenuItemFpsAdaptation: TTntMenuItem;
    ChangeFpsNtscToPal: TTntMenuItem;
    ChangeFpsPalToNtsc: TTntMenuItem;
    ChangeFpsNtscToCinema: TTntMenuItem;
    ChangeFpsCinemaToNtsc: TTntMenuItem;
    ChangeFpsPalToCinema: TTntMenuItem;
    tntmntmResynchShowHide: TTntMenuItem;
    TntLabel3: TTntLabel;
    edMinimunBetweenZones: TTntEdit;
    udMinimunBetweenZones: TTntUpDown;
    ActionTextBottomLeft: TTntAction;
    ActionTextBottomMiddle: TTntAction;
    ActionTextBottomRight: TTntAction;
    ActionTextTopLeft: TTntAction;
    ActionTextTopMiddle: TTntAction;
    ActionTextTopRight: TTntAction;
    ActionTextMiddleLeft: TTntAction;
    ActionTextMiddleRight: TTntAction;
    ActionTextMiddleMiddle: TTntAction;
    N28: TTntMenuItem;
    BottomLeft1: TTntMenuItem;
    BottomMiddle1: TTntMenuItem;
    BottomRight1: TTntMenuItem;
    opLeft1: TTntMenuItem;
    opMiddle1: TTntMenuItem;
    opRight1: TTntMenuItem;
    MiddleLeft1: TTntMenuItem;
    MiddleMiddle1: TTntMenuItem;
    MiddleRight1: TTntMenuItem;
    TabSheetWordReference: TTabSheet;
    WebBrowserWordReference: TWebBrowser;
    bttShowProjectProperties: TSpeedButton;
    MemoSubtitleVOPopupMenu: TTntPopupMenu;
    pmiMemoSubtitleCopyVOToSubtitleText: TTntMenuItem;
    TabSheetOneLook: TTabSheet;
    WebBrowserOneLook: TWebBrowser;
    pmiWAVDispCreateFromVO: TTntMenuItem;
    VistaAltFix1: TVistaAltFix;
    ActionMouseAntiOverlapping: TTntAction;
    ActionRangeStopDblClick: TTntAction;
    ActionRangeStartDblClick: TTntAction;
    MenuItemSetVideoResolution: TTntMenuItem;
    MenuItemSetVideoResolutionAdd50Perc: TTntMenuItem;
    MenuItemSetVideoResolutionAdd25Perc: TTntMenuItem;
    MenuItemSetVideoResolutionNormal: TTntMenuItem;
    MenuItemSetVideoResolutionTo50Perc: TTntMenuItem;
    MenuItemSetVideoResolutionTo33Perc: TTntMenuItem;
    MenuItemSetVideoResolutionTo25Perc: TTntMenuItem;
    MenuItemSetVideoResolutionTo75Perc: TTntMenuItem;
    ActionDiffSubFiles: TTntAction;
    DiffFile: TTntMenuItem;
    XPManifest1: TXPManifest;
    ActionSendToItasa: TTntAction;
    bttSendToItasa: TSpeedButton;
    SourceforgeRssFeedXml: TXMLDocument;
    WebBrowserWordReferenceThesaurus: TWebBrowser;
    TabSheetTheFreeDictionary: TTabSheet;
    WebBrowserTheFreeDictionary: TWebBrowser;
    ChangeFpsTrueCinemaToPal: TTntMenuItem;
    ChangeFpsPalToTrueCinema: TTntMenuItem;
    MenuItemKeepDurationIntact: TTntMenuItem;
    N30: TTntMenuItem;
    extinlowercase: TTntMenuItem;
    extinuppercase: TTntMenuItem;
    extinuppercaseFirstLetter: TTntMenuItem;
    SourceforgeRssFeedCodeXml: TXMLDocument;
    TimerChkUpdates: TTimer;
    N29: TTntMenuItem;
    MenuItemCheckUpdates: TTntMenuItem;
    MenuItemRecentChanges: TTntMenuItem;
    pmiSelectAllTP: TTntMenuItem;
    pmiN31: TTntMenuItem;
    pmiN32: TTntMenuItem;
    pmiDeleteTP: TTntMenuItem;
    pmiMemoSubPasteTP: TTntMenuItem;
    pmiMemoSubCopyTP: TTntMenuItem;
    pmiMemoSubCutTP: TTntMenuItem;
    ChangeFpsCinemaToTrueCinema: TTntMenuItem;
    ChangeFpsTrueCinemaToCinema: TTntMenuItem;
    ChangeFpsTrueCinemaToNtsc: TTntMenuItem;
    ChangeFpsNtscToTrueCinema: TTntMenuItem;
    tntmntmZipSubtitle1: TTntMenuItem;
    ActionZipSubtitle: TTntAction;
    pmUndoTP: TTntMenuItem;
    N31: TTntMenuItem;
    MenuItemGooglePage: TTntMenuItem;
    chkFastSmartWavCursor: TTntCheckBox;
    ActionSaveSelectedAs: TTntAction;
    tntmntmN32: TTntMenuItem;
    pmiSaveSelectedAs: TTntMenuItem;
    TabSheetSmartDesynch: TTabSheet;
    TntSpeedButtonResynchToolsSetPos: TTntSpeedButton;
    TntSpeedButtonResynchToolsDelPos: TTntSpeedButton;
    TntSpeedButtonResynchToolsReset: TTntSpeedButton;
    TntSpeedButtonResynchShowHide: TTntSpeedButton;
    ActionResynchShowHide: TTntAction;
    N32: TTntMenuItem;
    SubMenuCustomDictionary: TTntMenuItem;
    MenuCustomDictionaryCopyToClipboard: TTntMenuItem;
    MenuCustomDictionaryPasteFromClipboard: TTntMenuItem;
    MenuCustomDictionaryAppendFromClipboard: TTntMenuItem;
    procedure FormCreate(Sender: TObject);

    procedure WAVDisplayer1CursorChange(Sender: TObject);
    procedure WAVDisplayer1PlayCursorChange(Sender: TObject);
    procedure WAVDisplayer1SelectionChange(Sender: TObject);
    procedure WAVDisplayer1ViewChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRange(Sender: TObject; SelectedRange : TRange; IsDynamic : Boolean);
    procedure WAVDisplayer1SelectedRangeChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRangeChanged(Sender: TObject;
      OldStart, OldStop : Integer; NeedSort : Boolean);
    procedure WAVDisplayer1AutoScrollChange(Sender: TObject);
    procedure WAVDisplayPopup_DeleteRange(Sender: TObject);
    procedure WAVDisplayer1StartPlaying(Sender: TObject);
    procedure WAVDisplayer1KaraokeChanged(Sender: TObject;
        Range : TRange; SubTimeIndex, OldTime : Integer);
    procedure WAVDisplayer1SelectedKaraokeRange(Sender: TObject;
        Range : TRange);
    procedure WAVDisplayer1CustomDrawRange(Sender: TObject; ACanvas: TCanvas; Range : TRange; Rect : TRect);
    procedure WAVDisplayer1RangeStartDblClick(Sender: TObject; Range : TRange);
    procedure WAVDisplayer1RangeStopDblClick(Sender: TObject; Range : TRange);
    procedure WAVDisplayer1UpdateColors(Sender: TObject);
    procedure vtvSubsListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure MemoSubtitleTextChange(Sender: TObject);
    procedure vtvSubsListDblClick(Sender: TObject);
    procedure ActionZoomInExecute(Sender: TObject);
    procedure ActionZoomOutExecute(Sender: TObject);
    procedure ActionZoomSelectionExecute(Sender: TObject);
    procedure ActionZoomAllExecute(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionLoopExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionNewProjectExecute(Sender: TObject);
    procedure ActionShowHideVideoExecute(Sender: TObject);
    procedure ActionOpenProjectExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure MemoSubtitleTextSelectionChange(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionProjectPropertiesExecute(Sender: TObject);
    procedure pmiSubListDeleteClick(Sender: TObject);
    procedure SubListPopupMenuPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure WAVDisplayPopupMenuPopup(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure TimerStatusBarMsgTimer(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure chkAutoScrollWAVDispClick(Sender: TObject);
    procedure ActionCheckErrorsExecute(Sender: TObject);
    procedure ActionDelayExecute(Sender: TObject);
    procedure ActionShowErrorReportExecute(Sender: TObject);
    procedure ActionNextSubExecute(Sender: TObject);
    procedure ActionPreviousSubExecute(Sender: TObject);
    procedure vtvSubsListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure chkAutoScrollSubClick(Sender: TObject);
    procedure TimerAutoScrollSubTimer(Sender: TObject);
    procedure ActionGotoExecute(Sender: TObject);
    procedure MenuItemStartWebServerClick(Sender: TObject);
    procedure MenuItemStopWebServerClick(Sender: TObject);
    procedure ActionPreferencesExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionErrorPreferencesExecute(Sender: TObject);
    procedure ActionShowSuggestionsExecute(Sender: TObject);
    procedure MenuItemHelpIndexClick(Sender: TObject);
    procedure MenuItemHelpIndexWAVDisplayControlClick(Sender: TObject);
    procedure ActionInsertTextFileExecute(Sender: TObject);
    procedure ActionClearSelectionExecute(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure bttWorkingModeClick(Sender: TObject);
    procedure ActionLoopSelStartExecute(Sender: TObject);
    procedure ActionLoopSelEndExecute(Sender: TObject);
    procedure ActionAddSubtitleExecute(Sender: TObject);
    procedure ActionSetSubtitleTimeExecute(Sender: TObject);
    procedure ActionSetPlaybackRate80Execute(Sender: TObject);
    procedure ActionSetPlaybackRate90Execute(Sender: TObject);
    procedure ActionSetPlaybackRate100Execute(Sender: TObject);
    procedure ActionShiftStartPlus100Execute(Sender: TObject);
    procedure ActionShiftStartMinus100Execute(Sender: TObject);
    procedure ActionShiftStopPlus100Execute(Sender: TObject);
    procedure ActionShiftStopMinus100Execute(Sender: TObject);
    procedure ActionShiftStartPlus10Execute(Sender: TObject);
    procedure ActionShiftStartMinus10Execute(Sender: TObject);
    procedure ActionShiftStopPlus10Execute(Sender: TObject);
    procedure ActionShiftStopMinus10Execute(Sender: TObject);
    procedure ActionZoomVerticalExecute(Sender: TObject);
    procedure ActionShowHideTextPipeExecute(Sender: TObject);
    procedure ActionLoadTextPipeExecute(Sender: TObject);
    procedure ActionClearTextPipeExecute(Sender: TObject);
    procedure ActionSaveTextPipeAsExecute(Sender: TObject);
    procedure ActionAddSubFromPipeExecute(Sender: TObject);
    procedure MemoTextPipePopupMenuPopup(Sender: TObject);
    procedure ActionReplaceFromPipeExecute(Sender: TObject);
    procedure TimerAutoBackupTimer(Sender: TObject);
    procedure ActionDetachVideoExecute(Sender: TObject);
    procedure ActionShowHideLogsExecute(Sender: TObject);
    procedure vtvSubsListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ActionFixErrorMainExecute(Sender: TObject);
    procedure TntFormActivate(Sender: TObject);
    procedure MemoSubtitleTextMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ActionStartSubExecute(Sender: TObject);
    procedure ActionStopSubExecute(Sender: TObject);
    procedure ActionStopAndStartSubExecute(Sender: TObject);
    procedure TntFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TntFormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ActionInsertKaraokeMarkerExecute(Sender: TObject);
    procedure pmiCreateKaraokeClick(Sender: TObject);
    procedure pmiClearKaraokeClick(Sender: TObject);
    procedure ActionShowStartFrameExecute(Sender: TObject);
    procedure ActionShowStopFrameExecute(Sender: TObject);
    procedure ActionShowFrameAtCursorExecute(Sender: TObject);
    procedure ActionExportToWAVExecute(Sender: TObject);
    procedure ActionPlaySelStartExecute(Sender: TObject);
    procedure ActionPlaySelEndExecute(Sender: TObject);
    procedure ActionPlaceKaraokeCursorsAtEndExecute(Sender: TObject);
    procedure ActionPlayToEndExecute(Sender: TObject);
    procedure ActionPlay1sBeforeExecute(Sender: TObject);
    procedure ActionSelectNextSubExecute(Sender: TObject);
    procedure ActionSelectPreviousSubExecute(Sender: TObject);
    procedure PanelVideoResize(Sender: TObject);
    procedure ActionSetSubtitleStartTimeExecute(Sender: TObject);
    procedure ActionSetSubtitleStopTimeExecute(Sender: TObject);
    procedure ActionSetSubtitleStopTimeAndGoToNextExecute(Sender: TObject);
    procedure cbStylesSelect(Sender: TObject);
    procedure ActionStylesExecute(Sender: TObject);
    procedure ActionSetPlaybackRate70Execute(Sender: TObject);
    procedure ActionSetPlaybackRate60Execute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionToggleTimingModeExecute(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure ActionSetEditorFocusExecute(Sender: TObject);
    procedure ActionSetEditorFocusAndSelectExecute(Sender: TObject);
    procedure ActionNextErrorExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure PanelVideoDblClick(Sender: TObject);
    procedure ActionWAVDisplayScrollRightExecute(Sender: TObject);
    procedure ActionWAVDisplayScrollLeftExecute(Sender: TObject);
    procedure ActionShowHideSceneChangeExecute(Sender: TObject);
    procedure vtvSubsListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure ActionTextItalicExecute(Sender: TObject);
    procedure ActionTextBoldExecute(Sender: TObject);
    procedure ActionTextUnderlineExecute(Sender: TObject);
    procedure ActionTextColorExecute(Sender: TObject);
    procedure ActionTextSizeExecute(Sender: TObject);
    procedure ActionStripTagsExecute(Sender: TObject);
    procedure ActionMergeExecute(Sender: TObject);
    procedure ActionSplitAtCursorExecute(Sender: TObject);
    procedure pmiToggleColumn(Sender: TObject);
    procedure vtvSubsListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure ActionOpenProjectFolderExecute(Sender: TObject);
    procedure ActionPlay1sBeforeToEndExecute(Sender: TObject);
    procedure ActionDeleteSceneChangeExecute(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure ActionMergeWithPreviousExecute(Sender: TObject);
    procedure ActionMergeWithNextExecute(Sender: TObject);
    procedure ActionMergeDialogExecute(Sender: TObject);
    procedure ActionFixSelectedErrorsExecute(Sender: TObject);
    procedure ActionMergeOnOneLineExecute(Sender: TObject);
    procedure ActionLoadPresetsExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCopyUpdate(Sender: TObject);
    procedure ActionShowSilentZonesExecute(Sender: TObject);
    procedure ActionLiveSpellCheckExecute(Sender: TObject);
    procedure MemoSubPopupMenuPopup(Sender: TObject);
    procedure MenuItemGetMoreDictionariesClick(Sender: TObject);
    procedure ActionSpellCheckExecute(Sender: TObject);
    procedure ActionSpellCheckUpdate(Sender: TObject);
    procedure ActionInsertSceneChangeExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
    procedure ActionPasteAtCursorExecute(Sender: TObject);
    procedure ActionToggleVOExecute(Sender: TObject);
    procedure ActionShowHideSubsExecute(Sender: TObject);
    procedure ActionTranslationTemplateExecute(Sender: TObject);
    //procedure bttFastSmartWavCursorClick(Sender: TObject);
    procedure SpeedButtonUpQuickSilentZoneClick(Sender: TObject);
    procedure ComboBoxSilentZoneSelect(Sender: TObject);
    procedure PageQuickSmartSilentZonePanelsChange(Sender: TObject);
    procedure MemoSubtitleVOSelectionChange(Sender: TObject);
    procedure PageControlMainChange(Sender: TObject);
    procedure MemoTextPipeSelectionChange(Sender: TObject);
    procedure MemoTextPipeEnter(Sender: TObject);
    procedure MemoSubtitleVOEnter(Sender: TObject);
    procedure vtvSubsListHeaderDragged(Sender: TVTHeader;
      Column: TColumnIndex; OldPosition: Integer);
    procedure ActionDelaySynchToCursorExecute(Sender: TObject);
    procedure ActionDelayMoveNegative50Execute(Sender: TObject);
    procedure ActionDelayMoveNegative10Execute(Sender: TObject);
    procedure ActionDelayMoveNegative200Execute(Sender: TObject);
    procedure ActionDelayMoveNegative100Execute(Sender: TObject);
    procedure ActionDelayMovePositive10Execute(Sender: TObject);
    procedure ActionDelayMovePositive50Execute(Sender: TObject);
    procedure ActionDelayMovePositive100Execute(Sender: TObject);
    procedure ActionDelayMovePositive200Execute(Sender: TObject);
    procedure MemoSubtitleVOMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MemoSubtitleVOMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MemoTextPipeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MemoTextPipeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActionScrollTextPipeDownExecute(Sender: TObject);
    procedure ActionScrollTextPipeUpExecute(Sender: TObject);
    procedure SpeedButtonGoCurrentSilentZoneClick(Sender: TObject);
    procedure ActionMoveForwardToClosestSilentZoneExecute(Sender: TObject);
    procedure ActionMoveBackwardToClosestSilentZoneExecute(
      Sender: TObject);
    procedure ActionShiftForwardWaveFormExecute(Sender: TObject);
    procedure ActionShiftBackwardWaveFormExecute(Sender: TObject);
    procedure ChangeFpsCinemaToPalClick(Sender: TObject);
    procedure ChangeFpsPalToCinemaClick(Sender: TObject);
    procedure ActionResynchToolsSetPosExecute(Sender: TObject);
    procedure ActionResynchToolsDelPosExecute(Sender: TObject);
    procedure ActionResynchToolsResetExecute(Sender: TObject);
    procedure ChangeFpsCinemaToNtscClick(Sender: TObject);
    procedure ChangeFpsNtscToCinemaClick(Sender: TObject);
    procedure ChangeFpsPalToNtscClick(Sender: TObject);
    procedure ChangeFpsNtscToPalClick(Sender: TObject);
    procedure ItasaResynchtoolsshowhideClick(Sender: TObject);
    procedure ActionTextBottomLeftExecute(Sender: TObject);
    procedure ActionTextBottomMiddleExecute(Sender: TObject);
    procedure ActionTextBottomRightExecute(Sender: TObject);
    procedure ActionTextTopLeftExecute(Sender: TObject);
    procedure ActionTextTopMiddleExecute(Sender: TObject);
    procedure ActionTextTopRightExecute(Sender: TObject);
    procedure ActionTextMiddleLeftExecute(Sender: TObject);
    procedure ActionTextMiddleRightExecute(Sender: TObject);
    procedure ActionTextMiddleMiddleExecute(Sender: TObject);
    procedure pmiMemoSubtitleCopyVOToSubtitleTextClick(Sender: TObject);
    procedure pmiWAVDispCreateFromVOClick(Sender: TObject);
    procedure ActionMouseAntiOverlappingExecute(Sender: TObject);
    procedure edThresholdChange(Sender: TObject);
    procedure edDurationChange(Sender: TObject);
    procedure edMinimunBetweenZonesChange(Sender: TObject);
    procedure ActionRangeStopDblClickExecute(Sender: TObject);
    procedure ActionRangeStartDblClickExecute(Sender: TObject);
    procedure MenuItemSetVideoResolutionAdd50PercClick(Sender: TObject);
    procedure MenuItemSetVideoResolutionAdd25PercClick(Sender: TObject);
    procedure MenuItemSetVideoResolutionNormalClick(Sender: TObject);
    procedure MenuItemSetVideoResolutionTo50PercClick(Sender: TObject);
    procedure MenuItemSetVideoResolutionTo75PercClick(Sender: TObject);
    procedure MenuItemSetVideoResolutionTo33PercClick(Sender: TObject);
    procedure MenuItemSetVideoResolutionTo25PercClick(Sender: TObject);
    procedure ActionDiffSubFilesExecute(Sender: TObject);
    procedure ActionSendToItasaExecute(Sender: TObject);
    procedure ChangeFpsTrueCinemaToPalClick(Sender: TObject);
    procedure ChangeFpsPalToTrueCinemaClick(Sender: TObject);
    procedure MenuItemKeepDurationIntactClick(Sender: TObject);
    procedure LabelSelBeginClick(Sender: TObject);
    procedure LabelSelEndClick(Sender: TObject);
    procedure LabelSelLenghtClick(Sender: TObject);
    procedure extinlowercaseClick(Sender: TObject);
    procedure extinuppercaseClick(Sender: TObject);
    procedure extinuppercaseFirstLetterClick(Sender: TObject);
    procedure TimerChkUpdatesTimer(Sender: TObject);
    procedure MenuItemUpdateClick(Sender: TObject);
    procedure MenuItemCheckUpdatesClick(Sender: TObject);
    procedure MenuItemRecentChangesClick(Sender: TObject);
    procedure ChangeFpsCinemaToTrueCinemaClick(Sender: TObject);
    procedure ChangeFpsTrueCinemaToCinemaClick(Sender: TObject);
    procedure ChangeFpsTrueCinemaToNtscClick(Sender: TObject);
    procedure ChangeFpsNtscToTrueCinemaClick(Sender: TObject);
    procedure ActionZipSubtitleExecute(Sender: TObject);
    procedure pmUndoTPClick(Sender: TObject);
    procedure MenuItemGooglePageClick(Sender: TObject);
    procedure ActionSaveSelectedAsExecute(Sender: TObject);
    procedure ActionResynchShowHideExecute(Sender: TObject);
    procedure MenuCustomDictionaryCopyToClipboardClick(Sender: TObject);
    procedure MenuCustomDictionaryPasteFromClipboardClick(Sender: TObject);
    procedure MenuCustomDictionaryAppendFromClipboardClick(
      Sender: TObject);
    procedure SubMenuCustomDictionaryClick(Sender: TObject);

  private

    { Private declarations }
    WAVDisplayer : TWAVDisplayer;
    SubRangeFactory : TSubtitleRangeFactory;
    PeakCreationProgressForm : TPeakCreationProgressForm;
    AudioOnlyRenderer : TDShowRenderer;
    SearchNodeIndex : Integer;
    SearchPos, SearchPosVo : Integer;
    SearchItem : ^TTntRichEdit;
    SearchItemPos : Integer;
    OldAutoScrollIdx : Integer;

    Server : THTTPServer;
    ServerRootDir : string;

    MRUList : TMRUList;

    Swapped : Boolean;
    PlayingMode : TPlayingModeType;
    ShowingVideo : Boolean;
    FormHasBeenActivated : Boolean;
    ShowingSubs : Boolean;

    StartSubtitleTime : Integer;
    ToggleStartSubtitleTime : Integer;
    SubtitleFileHeader : WideString;
    SubtitleFileFooter : WideString;
    VideoPreviewNeedSubtitleUpdate : Boolean;
    DisableVideoUpdatePreview : Boolean;

    StatusBarPrimaryText : WideString;
    StatusBarSecondaryText : WideString;

    GeneralJSPlugin : TSimpleJavascriptWrapper;

    UndoStack : TObjectStack;
    RedoStack : TObjectStack;
    // Used only during fix error
    UndoableMultiChangeTask : TUndoableMultiChangeTask;

    PrefFormInitialized : Boolean;

    StartupShowVideo, StartupDetachVideo : Boolean;

    StartupDictionary : WideString;

    HiddenByUserCoreColumnsList : TStrings;

    WavDisplayerCursorPos : Integer;

    FZoneList : TObjectList;

    MouseLeftButtonIsUpOnMemoSubtitleVO : boolean;
    SaveSelectionForMemoSubtitleVO : WideString;
    SaveCompleteSelectionForMemoSubtitleVO : WideString;

    MouseLeftButtonIsUpOnMemoTextPipe : boolean;
    SaveSelectionForMemoTextPipe : WideString;
    SaveCompleteSelectionForMemoTextPipe : WideString;

    LastCursorPositionOnPause : Integer;

    CurrentMemoSubtitleTextSelectionOn : Boolean;

    procedure InitVTV;
    procedure InitVTVExtraColumns;
    procedure EnableControl(Enable : Boolean);
    procedure EnableFpsMenu(Enable : Boolean);
    procedure EnableStyleControls(Enable : Boolean);
    procedure LoadSubtitles(Filename: WideString; var IsUTF8 : Boolean);
    procedure LoadVO(Filename: WideString);
    procedure SaveSubtitles(Filename: WideString; PreviousFilename : WideString;
      InUTF8 : Boolean; BackupOnly : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
    procedure SaveProject(Project : TVSSProject; SilentSave : Boolean);
    procedure UpdateLinesCounter;
    function CheckSubtitlesAreSaved : Boolean;

    procedure WAVDisplayer1OnPeakFileCreation(Sender: TObject;
      EventType : TPeakFileCreationEventType; Param : Integer);
    procedure CurrentProjectOnDirtyChange(Sender : TObject);
    procedure CurrentProjectOnDirtySet(Sender: TObject);

    procedure FullSortTreeAndSubList;

    procedure OnRecentMenuItemClick(Sender : TObject);
    procedure SetShortcut(TimingMode : Boolean);

    procedure OffsetCurrentSubtitleStartTime(Offset : Integer);
    procedure OffsetCurrentSubtitleStopTime(Offset : Integer);

    function ColorizeOrDeleteTextPipe : TUndoableTask;
    procedure LoadColumnsSettings(IniFile : TIniFile);
    procedure ApplyMiscSettings;
    procedure ApplyMouseSettings;
    procedure ApplyAutoBackupSettings;
    procedure UpdateVideoRendererWindow;
    procedure ApplyFontSettings;
    procedure AutoFitSubsListColumns;

    procedure OnSubtitleRangeJSWrapperChangeStart(Sender : TSubtitleRangeJSWrapper;
      SubtitleRange : TSubtitleRange; NewValue : Integer);
    procedure OnSubtitleRangeJSWrapperChangeStop(Sender : TSubtitleRangeJSWrapper;
      SubtitleRange : TSubtitleRange;  NewValue : Integer);
    procedure OnSubtitleRangeJSWrapperChangeText(Sender : TSubtitleRangeJSWrapper;
      SubtitleRange : TSubtitleRange; NewValue : WideString);

    procedure SaveSubtitlesAsSRT(Filename, PreviousExt: WideString; InUTF8 : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
    procedure SaveSubtitlesAsSSA(Filename, PreviousExt: WideString; InUTF8 : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
    procedure SaveSubtitlesAsASS(Filename, PreviousExt: WideString; InUTF8 : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
    procedure SaveSubtitlesAsCUE(Filename: WideString; ExcludeUnselected : Boolean);
    procedure SaveSubtitlesAsTXT(Filename, PreviousExt: WideString; InUTF8 : Boolean; ExcludeUnselected : Boolean);
    procedure SaveSubtitlesAsCSV(Filename, PreviousExt: WideString; InUTF8 : Boolean; ExcludeUnselected : Boolean);
    procedure SelectPreviousSub;
    procedure SelectNextSub;
    function LoadVOSubs(Filename: WideString) : Boolean;
    function LoadSRT(Filename: WideString; var IsUTF8 : Boolean) : Boolean;
    function LoadASS(Filename: WideString; var IsUTF8 : Boolean; IsSSA : Boolean) : Boolean;
    procedure AdvanceToNextSubtitleAfterFocus;
    procedure SetFocusedSubtitleStopTime(Advance: Boolean; KeepStopIfSuperiorToCPSTarget : Boolean);
    procedure UpdateStylesComboboxFromSelection;
    procedure UpdateStylesComboBox;
    procedure UpdateVolume;
    procedure UpdateSubtitleForPreview(ForceUpdate: Boolean);
    procedure SetSubtitleStartTime(SetStopTime : Boolean);
    procedure Merge(MergeType : TMergeType; MergeRange : TMergeRange);

    function DetectCharsetDataLoss : Boolean;

    procedure TagText(StartTag, StopTag : WideString); overload;
    procedure TagTextMemo(StartTag, StopTag : WideString);
    procedure TagTextLines(StartTag, StopTag : WideString);
    procedure TagText(TagType : TTagType); overload;

    // General JS plugins
    procedure CallJSOnSubtitleModification;
    procedure CallJSOnSelectedSubtitle;
    procedure GetCurrentPreviousNextSubtitles(CurrentNode : PVirtualNode; var CurrentSub,
      PreviousSub, NextSub : TSubtitleRange);
    procedure UpdateStatusBar;
    procedure OnJsSetStatusBarText(const Msg : WideString);

    procedure OnJavascriptAction(Sender : TObject);
    procedure UpdateAfterJSChange;

    // Undo/Redo stuff
    procedure PushUndoableTask(UndoableTask : TUndoableTask);
    procedure ClearStack(Stack : TObjectStack);

    procedure OnUndo(Sender: TTntRichEdit; UndoTask : TUndoableTask);

    procedure LoadPresetFile(Filename : WideString);
    procedure FillPresetsMenu;
    procedure OnLoadPresetMenuItemClick(Sender: TObject);

    procedure OnSpellcheckLanguageMenuItemClick(Sender: TObject);
    procedure OnSpellcheckSuggestionMenuItemClick(Sender: TObject);
    procedure OnLoadDictionary(Sender: TObject);
    procedure OnLoadDictionaryTerminate(Sender: TObject);
    procedure OnSpellcheckAddToDictionaryMenuItemClick(Sender: TObject);
    procedure OnSpellcheckIgnoreMenuItemClick(Sender: TObject);

    procedure CloseProject;

    function GetTextPipeAutoOption : Integer;
    procedure SetTextPipeAutoOption(Option : Integer);

    function SearchMemo(FindText : WideString) : Boolean;
    function SearchSubtitles(FindText : WideString) : Boolean;
    procedure ReplaceTextMemo;
    procedure ReplaceTextSubs;
    function ReplaceAllTextMemo(FindText : WideString) : Integer;
    function ReplaceAllTextSubs(FindText : WideString) : Integer;

    procedure UpdateDurationWithSubOnly;

    procedure ShowColumn(Column : TVirtualTreeColumn; Show : Boolean = True; FromUser : Boolean = False);

    procedure MoveToFromCurrentToEnd(DelayToApply : integer; CheckOverlap : boolean; UpdateIconIndex : Integer);

    procedure FillZone(ZoneList : TList);

    procedure ChangeFps(FPSSource : double; FPSDestination : double);

  public
    CurrentProject : TVSSProject;
    ConfigObject : TConfigObject;
    VideoRenderer : TDShowRenderer;
    FSpellChecker : THunspellChecker;

    { Public declarations }

    procedure ShowStatusBarMessage(const Text : WideString; const Duration : Integer = 4000);
    procedure SelectNode(Node : PVirtualNode);
    procedure SelectNodeFromTimestamps(Start, Stop : Integer);
    procedure SelectNodeAtIndex(Index : Integer);
    procedure StartStopServer(Stop : Boolean = False);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ShowPreferences(TabSheet : TTntTabSheet);
    procedure LoadVideoSceneChange;
    procedure ClearVideoSceneChange;
    procedure LoadProject(Filename : WideString);
    procedure LoadVideoInfo(VideoFilename : WideString);
    procedure SwapSubList(SwapSizeAlso : Boolean = True);
    procedure FinishLoadSettings;
    procedure LoadAfterParamsSettings;
    function CanFixError(PluginFilename : WideString) : Boolean;
    procedure FixErrorInList(ErrorList : TList);
    procedure RenameStyle(OldName, NewName : WideString);
    procedure CurrentProjectSetDirty;
    function GetVideoRendererFiltersList(list : TStrings) : Boolean;
    procedure SetStatusBarPrimaryText(const Text : WideString);
    procedure ProcessParams;
    procedure TurnOffVO;
    procedure TurnOnVO;
    procedure AdjustShowVO;
    function GetVOText(NodeData: PTreeData; Enhanced : Boolean) : WideString;
    procedure UpdateMemoVO(NodeData : PTreeData);

    function IsTimingMode : Boolean;
    function IsNormalMode : Boolean;

    procedure ApplyDelay(var Indexes : array of Integer;
      DelayInMs : Integer; DelayShiftType : TDelayShiftType);
    function AddSubtitle(StartTime, StopTime : Integer; Text : WideString; UpdateDisplay : Boolean) : PVirtualNode; overload;
    function AddSubtitle(SubRange : TSubtitleRange; UpdateDisplay : Boolean) : PVirtualNode; overload;
    function AddSubtitle(StartTime, StopTime : Integer; Text : WideString;
      UpdateDisplay : Boolean; AutoSelect : Boolean) : PVirtualNode; overload;
    procedure DeleteSubtitle(Index : Integer);
    procedure DeleteSubtitles(var Indexes : array of Integer);
    procedure CloneSubtitles(var Indexes : array of Integer; List : TList);
    procedure RestoreSubtitles(List : TList; Indexes : TIntegerDynArray = nil);
    function SetSubtitleTime(Index, NewStartTime, NewStopTime : Integer) : Integer;
    procedure SplitSubtitle(Index, SplitTime, BlankTime : Integer);
    function MergeSubtitles(var FIndexes : array of Integer; MergeType : TMergeType) : TSubtitleRange;
    procedure FocusNode(Node : PVirtualNode; WAVDisplaySelect : Boolean); overload;
    procedure FocusNodeAt(Index : Integer); overload;
    procedure ClearWAVSelection;
    procedure ProcessMultiChangeSub(ChangeList : TList; IsUndo : Boolean);
    procedure ResetFind;
    procedure ShowMatchingSubOnly(DisplayAll : Boolean);
    procedure ReplaceText;
    procedure ReplaceAllText;
    function LastFindSucceded : Boolean;
    procedure InitGeneralJSPlugin;

    procedure InsertSceneChange(SrcSCArray : TIntegerDynArray);
    function DeleteSceneChange(StartTimeMs, StopTimeMs : Integer) : TIntegerDynArray;

    procedure SetSelection(Start, Stop : Integer);
    procedure SmartAccents(RichEdit : TTntRichEdit);
    procedure SmartUppercase(RichEdit : TTntRichEdit; PreviousSubtitleText: String);
    procedure TagHighlight(RichEdit : TTntRichEdit; TagIndex : Integer);
    procedure LoadDict(Idx : Integer);
    function GetSpellChecker : THunspellChecker;

    // TVSSCoreEngineIntf
    procedure RegisterJavascriptAction(AName, ACaption, ADefaultShortcut : WideString);
    function GetSubCount : Integer;
    function GetFirst : TSubtitleRange;
    function GetNext(SubtitleRange : TSubtitleRange) : TSubtitleRange;
    function GetPrevious(SubtitleRange : TSubtitleRange) : TSubtitleRange;
    function GetSelectedCount : Integer;
    function GetFirstSelected : TSubtitleRange;
    function GetNextSelected(SubtitleRange : TSubtitleRange) : TSubtitleRange;
    function GetAt(Index : Integer) : TSubtitleRange;
    function GetTextSelectionStart : Integer;
    function GetTextSelectionLength : Integer;
    function GetAudioCursorPosition : Integer;
    function GetPluginParamValue(JsSection, JsParameter : WideString) : Integer;

    procedure DisableJavascriptItemMenu(ACaption : WideString);
    procedure EnableJavascriptItemMenu(ACaption : WideString);
    procedure InsertBreakBeforeJavascriptMenuItem(ACaption : WideString);
    procedure InsertBreakAfterJavascriptMenuItem(ACaption : WideString);
    procedure SetPluginParamValue(JsSection, JsParameter : WideString; Value: Integer);
    function GetCursorPosition : Integer;
    procedure SetSubColor(StartIndex,LastIndex : Integer; Color : TColor);
    procedure ResetSubColor;
    procedure SetSubIcon(Index : Integer; BitMapIndex : Integer);
    function GetSubIcon(Index : Integer) : Integer;
    procedure CheckResynchToolMenu;
    procedure CheckUpdates(Silent:Boolean);
    function GetUTF8Default : boolean;
  end;

const
  StartEndPlayingDuration: Integer = 500;
  EnableExperimentalKaraoke: Boolean = True;
  UTF8BOM : array[0..2] of BYTE = ($EF,$BB,$BF);
  INDEX_COL_INDEX = 0;
  START_COL_INDEX = 1;
  STOP_COL_INDEX = 2;
  STYLE_COL_INDEX = 3;
  TEXT_COL_INDEX = 4;
  VO_COL_INDEX = 5;
  LAST_CORE_COL_INDEX = VO_COL_INDEX;
  COLUMN_COUNT = LAST_CORE_COL_INDEX + 1;
  MemoVOMaxUnassigned : Integer = 100;
  MemoVOMaxLength : Integer = 40;
  MemoVOMaxRows : Integer = 4;

var
  MainForm: TMainForm;

implementation

uses ActiveX, Math, StrUtils, FindFormUnit, AboutFormUnit,
  ErrorReportFormUnit, SuggestionFormUnit, GotoFormUnit,
  VerticalScalingFormUnit, TntSysUtils, TntWindows,
  LogWindowFormUnit, CursorManager, FileCtrl, WAVFileUnit, PageProcessorUnit,
  tom_TLB, RichEdit, StyleFormUnit, SSAParserUnit, TntWideStrings, TntClasses,
  TntIniFiles, TntGraphics, TntSystem, TntRichEditCustomUndoUnit, RGBHSLColorUnit,
  SceneChangeUnit, SilentZoneFormUnit, RegExpr, SRTParserUnit, ShellAPI,
  VSSClipboardUnit, BgThreadTaskUnit, SpellCheckFormUnit, TntClipBrd, DeCAL,MediaInfoDll,
  ResynchToolFormUnit, DSiWin32, DiffSubsFormUnit, SendToItasaFormUnit, SendToItasaMiscUnit,
  SubtitleTimingFormUnit, KAZip, mshtml, ClipBrd, uLkJSON, WinINet;

{$R *.dfm}

//==============================================================================

// TODO : Configurable backup directory, backup n last version

// TODO : Test project properties dialog
// TODO : When opening a project fail, show the project properties page for modification
// TODO : Add some checking in project unit ok button

// TODO : Web : number of suggestion for current sub, and current suggestions
// TODO : Web : We need real gzip compression, deflate support is b0rked in IE

// TODO : Split at cursor for submemo
// TODO : Check HD space before extraction

// TODO : Rework project handling which is a bit messy ATM
// TODO : Separate subtitle file loading/saving, stats according to format into a new classes

//------------------------------------------------------------------------------

// TODO : auto clear karaoke timing that are out of subtitle bound ???
// TODO : update documentation (timing button)

//==============================================================================

procedure TMainForm.FormCreate(Sender: TObject);
var i : integer;
    CustomMemo : TTntRichEditCustomUndo;
    MenuItem : TTntMenuItem;
begin
  CustomMemo := TTntRichEditCustomUndo.Create(MemoSubtitleText.Owner);
  CustomMemo.Parent := MemoSubtitleText.Parent;
  CustomMemo.Font.Assign(MemoSubtitleText.Font);
  CustomMemo.OnChange := MemoSubtitleText.OnChange;
  CustomMemo.OnMouseDown := MemoSubtitleText.OnMouseDown;
  CustomMemo.OnSelectionChange := MemoSubtitleText.OnSelectionChange;
  CustomMemo.PopupMenu := MemoSubtitleText.PopupMenu;
  CustomMemo.ScrollBars := MemoSubtitleText.ScrollBars;
  CustomMemo.Width := MemoSubtitleText.Width;
  CustomMemo.Height := MemoSubtitleText.Height;
  CustomMemo.Top := MemoSubtitleText.Top;
  CustomMemo.Left := MemoSubtitleText.Left;
  CustomMemo.Align := MemoSubtitleText.Align;
  CustomMemo.HideSelection := MemoSubtitleText.HideSelection;
  CustomMemo.WordWrap := False;
  //CustomMemo.DisableWindowsUndo;
  MemoSubtitleText.Free;
  MemoSubtitleText := CustomMemo;
  CustomMemo.OnUndo := OnUndo;
  HiddenByUserCoreColumnsList := TStringList.Create;

  // Enable/disable experimental karaoke stuff
  pmiCreateKaraoke.Visible := EnableExperimentalKaraoke;
  pmiClearKaraoke.Visible := EnableExperimentalKaraoke;

  plCursorPos.DoubleBuffered := True;
  LogForm := TLogForm.Create(nil);

  UndoStack := TObjectStack.Create;
  RedoStack := TObjectStack.Create;
  ActionUndo.Enabled := False;
  ActionRedo.Enabled := False;
  MRUList := TMRUList.Create(MenuItemOpenRecentRoot);
  MRUList.OnRecentMenuItemClick := OnRecentMenuItemClick;
  ConfigObject := TConfigObject.Create;

  ServerRootDir := ExtractFilePath(Application.ExeName);
  ServerRootDir := IncludeTrailingPathDelimiter(ServerRootDir) + 'web\';

  vtvSubsList.Constraints.MinWidth := 600;
  PanelMiddle.Constraints.MinHeight := 125;
  PanelBottom.Constraints.MinHeight := 40;
  Splitter1.MinSize := 1;

  // Clear speed button caption text filled by action :p
  for i := 0 to Pred(PanelPlaybackControl.ControlCount) do
      if PanelPlaybackControl.Controls[i] is TSpeedButton then
        TSpeedButton(PanelPlaybackControl.Controls[i]).Caption := '';

  bttWorkingMode.Caption := 'Normal';
  ShowingSubs := True;
  MenuShowHideSubtitles.Checked := ShowingSubs;
  SubRangeFactory := TSubtitleRangeFactory.Create;

  WAVDisplayer := TWAVDisplayer.Create(nil);
  WAVDisplayer.Left := 0;
  WAVDisplayer.Top := 0;
  WAVDisplayer.Width := PanelWAVDisplay.Width;
  WAVDisplayer.Height := PanelWAVDisplay.Height - 12;
  WAVDisplayer.Align := alClient;
  WAVDisplayer.Parent := PanelWAVDisplay;

  WAVDisplayer.OnCursorChange := WAVDisplayer1CursorChange;
  WAVDisplayer.OnPlayCursorChange := WAVDisplayer1PlayCursorChange;
  WAVDisplayer.OnSelectionChange := WAVDisplayer1SelectionChange;
  WAVDisplayer.OnViewChange := WAVDisplayer1ViewChange;
  WAVDisplayer.OnSelectedRange := WAVDisplayer1SelectedRange;
  WAVDisplayer.OnPeakFileCreation := WAVDisplayer1OnPeakFileCreation;
  WAVDisplayer.OnSelectedRangeChange := WAVDisplayer1SelectedRangeChange;
  WAVDisplayer.OnSelectedRangeChanged := WAVDisplayer1SelectedRangeChanged;
  WAVDisplayer.OnAutoScrollChange := WAVDisplayer1AutoScrollChange;
  WAVDisplayer.OnStartPlaying := WAVDisplayer1StartPlaying;
  WAVDisplayer.OnKaraokeChanged := WAVDisplayer1KaraokeChanged;
  WAVDisplayer.OnSelectedKaraokeRange := WAVDisplayer1SelectedKaraokeRange;
  WAVDisplayer.OnCustomDrawRange := WAVDisplayer1CustomDrawRange;
  WAVDisplayer.OnRangeStartDblClick := WAVDisplayer1RangeStartDblClick;
  WAVDisplayer.OnRangeStopDblClick := WAVDisplayer1RangeStopDblClick;

  WAVDisplayer.UpdateColors;

  WAVDisplayer.Enabled := False;
  WAVDisplayer.PopupMenu := WAVDisplayPopupMenu;
  InitVTV;

  MemoSubtitleText.Font.Name := 'Arial';
  MemoSubtitleText.Font.Style := MemoSubtitleText.Font.Style + [fsBold];
  MemoSubtitleText.Font.Size := 10;
  MemoLinesCounter.Font.Assign(MemoSubtitleText.Font);
  MemoTextPipe.Font.Assign(MemoSubtitleText.Font);
  MemoSubtitleVO.Font.Name := 'Arial';
  MemoSubtitleVO.Font.Style := MemoSubtitleVO.Font.Style + [fsBold];
  MemoSubtitleVO.Font.Size := 10;

  EnableControl(False);
  EnableStyleControls(False);

  CurrentProject := TVSSProject.Create;
  CurrentProject.OnDirtyChange := CurrentProjectOnDirtyChange;
  CurrentProject.OnDirtySet := CurrentProjectOnDirtySet;
  AudioOnlyRenderer := TDShowRenderer.Create;
  AudioOnlyRenderer.SetAutoInsertCustomVSFilter(True);
  VideoRenderer := TDShowRenderer.Create;
  VideoRenderer.SetAutoInsertCustomVSFilter(True);
  ShowingVideo := False;
  VideoPreviewNeedSubtitleUpdate := False;
  DisableVideoUpdatePreview := False;

  g_GlobalContext.SubList := WAVDisplayer.RangeList;
  g_GlobalContext.CurrentProject := CurrentProject;
  StartSubtitleTime := -1;
  ToggleStartSubtitleTime := -1;
  //StartStopServer;
  Application.HintHidePause := 10000;
  MenuItemKeepDurationIntact.Checked := ConfigObject.KeepDurationIntact;

  StatusBarPrimaryText := '';
  StatusBarSecondaryText := '';

  // Init spellchecker menu
  FSpellChecker := THunspellChecker.Create;
  {$IFNDEF enhanced}
  FSpellChecker.LoadPersonalDict(g_DictPath + 'perso.dic');
  FSpellChecker.FindDict(g_DictPath);
  {$ELSE}
  FSpellChecker.LoadPersonalDict(WideIncludeTrailingBackslash(g_AppDataDictPath) + 'perso.dic');
  FSpellChecker.FindDict(g_ApplicationPath + 'Dict');
  {$ENDIF}

  for i := 0 to FSpellChecker.GetDictCount-1 do
  begin
    MenuItem := TTntMenuItem.Create(Self);
    MenuItem.GroupIndex := 1;
    MenuItem.RadioItem := True;
    MenuItem.AutoCheck := True;
    MenuItem.Caption := FSpellChecker.GetDict(i);
    MenuItem.Tag := i;
    MenuItem.OnClick := OnSpellcheckLanguageMenuItemClick;
    SubMenuItemSpellcheck.Insert(
      SubMenuItemSpellcheck.IndexOf(MenuItemLiveSpellcheck) + 2 + i, MenuItem);
  end;
  if (FSpellChecker.GetDictCount = 0) then
  begin
    MenuItem := TTntMenuItem.Create(Self);
    MenuItem.Caption := 'No dictionary';
    MenuItem.Enabled := False;
    SubMenuItemSpellcheck.Insert(
      SubMenuItemSpellcheck.IndexOf(MenuItemLiveSpellcheck) + 2, MenuItem);
  end;

  // Fill the presets menu
  FillPresetsMenu;

  // silent zone in MainForm
  FZoneList := TObjectList.Create(True);
  PageControlMain.ActivePage := TabSheetSub;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FSpellChecker) then
  begin
    {$IFNDEF enhanced}
    FSpellChecker.SavePersonalDict(g_DictPath + 'perso.dic');
    {$ELSE}
    FSpellChecker.SavePersonalDict(WideIncludeTrailingBackslash(g_AppDataDictPath) + 'perso.dic');
    {$ENDIF}
    FreeAndNil(FSpellChecker);
  end;
  StartStopServer(True);
  if Assigned(g_VSSCoreWrapper) then
  begin
    g_VSSCoreWrapper.SetVSSCoreEngine(nil);
  end;
  FreeAndNil(GeneralJSPlugin);
  FreeAndNil(AudioOnlyRenderer);
  FreeAndNil(VideoRenderer);
  FreeAndNil(WAVDisplayer);
  FreeAndNil(SubRangeFactory);
  FreeAndNil(MRUList);
  FreeAndNil(ConfigObject);
  FreeAndNil(CurrentProject);
  ClearStack(RedoStack);
  FreeAndNil(RedoStack);
  ClearStack(UndoStack);
  FreeAndNil(UndoStack);
  if Assigned(LogForm) then
    FreeAndNil(LogForm);
  FreeAndNil(HiddenByUserCoreColumnsList);
end;

//------------------------------------------------------------------------------

function TMainForm.GetTextPipeAutoOption : Integer;
begin
  if pmiAutoColorizeText.Checked then
    Result := 0
  else if pmiAutoDeleteText.Checked then
    Result := 1
  else if pmiAutoDeleteAllTextBefore.Checked then
    Result := 2
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetTextPipeAutoOption(Option : Integer);
begin
  pmiAutoColorizeText.Checked := (Option = 0);
  pmiAutoDeleteText.Checked := (Option = 1);
  pmiAutoDeleteAllTextBefore.Checked := (Option = 2);
end;

//------------------------------------------------------------------------------

procedure SaveFormPosition(IniFile : TIniFile; Form : TForm);
//var
//  WindowPlacement: TWindowPlacement;
begin
  {
  form placement using windows API
  todo: check why it creates problems

  WindowPlacement.length := Sizeof(WindowPlacement);
  GetWindowPlacement(Form.Handle, @WindowPlacement);
  IniFile.WriteInteger('Windows', Form.Name + '_Left', WindowPlacement.rcNormalPosition.Left);
  IniFile.WriteInteger('Windows', Form.Name + '_Top',  WindowPlacement.rcNormalPosition.Top);
  IniFile.WriteInteger('Windows', Form.Name + '_Width', WindowPlacement.rcNormalPosition.Right - WindowPlacement.rcNormalPosition.Left);
  IniFile.WriteInteger('Windows', Form.Name + '_Height', WindowPlacement.rcNormalPosition.Bottom - WindowPlacement.rcNormalPosition.Top);
  IniFile.WriteInteger('Windows', Form.Name + '_State', Ord(Form.WindowState));
  }
  //using direct form placement instead
  if Ord(Form.WindowState) <> 2 then
   begin
    IniFile.WriteInteger('Windows', Form.Name + '_Left',  Form.Left);
    IniFile.WriteInteger('Windows', Form.Name + '_Top',   Form.Top);
    IniFile.WriteInteger('Windows', Form.Name + '_Width', Form.Width);
    IniFile.WriteInteger('Windows', Form.Name + '_Height',Form.Height);
   end; 
  IniFile.WriteInteger('Windows', Form.Name + '_State', Ord(Form.WindowState));
end;

//------------------------------------------------------------------------------

procedure LoadFormPosition(IniFile : TIniFile; Form : TForm);
var i : integer;
    MonRect, FormRect, InterRect : TRect;
    FormIsVisible : Boolean;
    MainMonitor : TMonitor;
    L,T,W,H : Integer;
//    WindowPlacement: TWindowPlacement;
begin
  {
  form placement using windows API
  todo: check why it creates problems

  WindowPlacement.length := Sizeof(WindowPlacement);
  WindowPlacement.rcNormalPosition.Left := IniFile.ReadInteger('Windows', Form.Name + '_Left', Form.Left);
  WindowPlacement.rcNormalPosition.Top := IniFile.ReadInteger('Windows', Form.Name + '_Top', Form.Top);
  W := IniFile.ReadInteger('Windows', Form.Name + '_Width', Form.Width);
  WindowPlacement.rcNormalPosition.Right := WindowPlacement.rcNormalPosition.Left + W;
  H := IniFile.ReadInteger('Windows', Form.Name + '_Height', Form.Height);
  WindowPlacement.rcNormalPosition.Bottom := WindowPlacement.rcNormalPosition.Top + H;
  SetWindowPlacement(Form.Handle,@WindowPlacement);
  }
  //using direct form placement instead
  L := IniFile.ReadInteger('Windows', Form.Name + '_Left', Form.Left);
  T := IniFile.ReadInteger('Windows', Form.Name + '_Top', Form.Top);
  W := IniFile.ReadInteger('Windows', Form.Name + '_Width', Form.Width);
  H := IniFile.ReadInteger('Windows', Form.Name + '_Height', Form.Height);
  Form.SetBounds(L, T, W, H);
  Form.WindowState := TWindowState(IniFile.ReadInteger('Windows', Form.Name + '_State', Ord(Form.WindowState)));
  // Put the form on main screen if it's not on any screen
  FormIsVisible := False;
  MainMonitor := nil;
  for i:=0 to Screen.MonitorCount-1 do
  begin
    MonRect := Screen.Monitors[i].BoundsRect;
    InflateRect(MonRect,-10,-10);
    GetWindowRect(Form.Handle,FormRect);
    if IntersectRect(InterRect,MonRect,FormRect) = True then
    begin
      FormIsVisible := True;
      Break;
    end;
    if Screen.Monitors[i].Primary then
      MainMonitor := Screen.Monitors[i];
  end;
  if (not FormIsVisible) then
  begin
    if Assigned(MainMonitor) then
    begin
      Form.Left := MainMonitor.Left + 100;
      Form.Top := MainMonitor.Top + 100;
    end
    else
    begin
      Form.Left := 100;
      Form.Top := 100;
    end;
  end;
end;

//------------------------------------------------------------------------------

function GetIniFilename : WideString;
var ApplicationPath, ApplicationDataPath, IniFilename : WideString;
begin
  // By default place the ini file in the application directory
  Result := WideChangeFileExt(TntApplication.ExeName, '.ini');

  {$IFDEF enhanced}
  Result := StringReplace(Result, 'Uwp', '', []);
  {$ENDIF}

  // Check if application path and ini file is writable
  ApplicationPath := WideExtractFilePath(TntApplication.ExeName);
  ApplicationDataPath := GetUserApplicationDataFolder + '\' + RootAppData;

  if WideFileIsReadOnly(ApplicationPath) or not WideFileExists(Result) or (WideFileExists(Result) and WideFileIsReadOnly(Result)) then
  begin
    // The ini file is in read only mode, work in user application data directory
    IniFilename := ApplicationDataPath;
    IniFilename := WideIncludeTrailingPathDelimiter(IniFilename);
    IniFilename := IniFilename + 'VisualSubSync.ini';
    Result := IniFilename;
  end;

end;

//------------------------------------------------------------------------------

function GetVTVColumnByName(Vtv : TVirtualStringTree; Name : WideString) : TVirtualTreeColumn;
var i : Integer;
    Column : TVirtualTreeColumn;
begin
  for i := 0 to Vtv.Header.Columns.Count-1 do
  begin
    Column := Vtv.Header.Columns.Items[i];
    if (Column.Text = Name) then
    begin
      Result := Column;
      Exit;
    end;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSettings;
var IniFile : TIniFile;
    IniFilename : WideString;
    i : Integer;
    VisibleExtraColumnsList : TStrings;
    ColumnsPositionList : TStrings;
    ColumnsWidthList : TStrings;
    ColumnName : WideString;
    Column : TVirtualTreeColumn;
begin
  IniFilename := GetIniFilename;
  try
    IniFile := TIniFile.Create(IniFilename);
    MRUList.SaveIni(IniFile, 'MRUList');
    ConfigObject.SaveIni(IniFile);
    SaveFormPosition(IniFile, MainForm);
    SaveFormPosition(IniFile, ErrorReportForm);
    SaveFormPosition(IniFile, SuggestionForm);
    SaveFormPosition(IniFile, DetachedVideoForm);
    SaveFormPosition(IniFile, LogForm);
    SaveFormPosition(IniFile, FindForm);
    SaveFormPosition(IniFile, DiffSubsForm);
    if PrefFormInitialized then
      SaveFormPosition(IniFile, PreferencesFormInstance);
    if Assigned(SilentZoneForm) then
      SaveFormPosition(IniFile, SilentZoneForm);
    if Assigned(SpellCheckForm) then
      SaveFormPosition(IniFile, SpellCheckForm);

    IniFile.WriteInteger('Windows', 'MainForm_PanelTop_Height', PanelTop.Height);
    IniFile.WriteInteger('Windows', 'MainForm_PanelBottom_Height', PanelBottom.Height);
    IniFile.WriteBool('Windows', 'DetachedVideo', MenuItemDetachVideoWindow.Checked);
    IniFile.WriteBool('Windows', 'ShowVideo', ShowingVideo);

    IniFile.WriteInteger('General', 'Volume', tbVolume.Position);

    // Save column settings
    VisibleExtraColumnsList := TStringList.Create;
    for i := 0 to GeneralJSPlugin.ExtraColumnsCount-1 do
    begin
      ColumnName := GeneralJSPlugin.GetColumnTitle(LAST_CORE_COL_INDEX + 1 + i);
      Column := GetVTVColumnByName(vtvSubsList, ColumnName);
      if Assigned(Column) and (coVisible in Column.Options)  then
      begin
        VisibleExtraColumnsList.Add(ColumnName);
      end;
    end;
    IniFile.WriteString('General', 'VisibleExtraColumns', VisibleExtraColumnsList.CommaText);
    FreeAndNil(VisibleExtraColumnsList);

    ColumnsPositionList := TStringList.Create;
    ColumnsWidthList := TStringList.Create;
    for i := 0 to vtvSubsList.Header.Columns.Count-1 do
    begin
      Column := vtvSubsList.Header.Columns.Items[i];
      ColumnsPositionList.Add(IntToStr(Column.Position));
      ColumnsWidthList.Add(IntToStr(Column.Width));
    end;
    IniFile.WriteString('General', 'ColumnsPosition', ColumnsPositionList.CommaText);
    FreeAndNil(ColumnsPositionList);
    IniFile.WriteString('General', 'ColumnsWidth', ColumnsWidthList.CommaText);
    FreeAndNil(ColumnsWidthList);
    
    IniFile.WriteString('General', 'HiddenByUserCoreColumns', HiddenByUserCoreColumnsList.CommaText);

    // VO textbox
    IniFile.WriteInteger('Windows', 'MemoSubtitleVO_Width', MemoSubtitleVO.Width);

    // Text pipe
    IniFile.WriteBool('TextPipe', 'ShowTextPipe', MemoTextPipe.Visible);
    IniFile.WriteInteger('TextPipe', 'TextPipeAutoOption', GetTextPipeAutoOption);

    // Default dictionnry
    IniFile.WriteString('General', 'Dictionary', StartupDictionary);

    IniFile.Free;
  except
    on E: Exception do MessageBox(Handle, PChar(E.Message), nil, MB_OK or MB_ICONERROR);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadSettings;
var IniFile : TIniFile;
    IniFilename : WideString;
begin
  ConfigObject.SetDefaultHotKeys(TntActionList1);

  IniFilename := GetIniFilename;
  IniFile := TIniFile.Create(IniFilename);

  MRUList.LoadIni(IniFile, 'MRUList');

  ConfigObject.LoadIni(IniFile, False);
  SetShortcut(IsTimingMode);
  ApplyMouseSettings;
  ApplyFontSettings;
  ApplyMiscSettings;

  LoadFormPosition(IniFile,MainForm);
  LoadFormPosition(IniFile,ErrorReportForm);
  LoadFormPosition(IniFile,SuggestionForm);
  LoadFormPosition(IniFile,DetachedVideoForm);
  LoadFormPosition(IniFile,LogForm);
  LoadFormPosition(IniFile,FindForm);
  LoadFormPosition(IniFile,DiffSubsForm);

  Show;

  PanelTop.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelTop_Height',
    PanelTop.Height);
  PanelBottom.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelBottom_Height',
    PanelBottom.Height);
  StatusBarMainPanel.Top := Maxint;

  tbVolume.Position := IniFile.ReadInteger('General', 'Volume', 100);

  StartupDetachVideo := IniFile.ReadBool('Windows', 'DetachedVideo', False);
  StartupShowVideo := IniFile.ReadBool('Windows', 'ShowVideo', True);

  // Default dictionary
  StartupDictionary := IniFile.ReadString('General','Dictionary','');

  // VO textbox
  MemoSubtitleVO.Width := IniFile.ReadInteger('Windows', 'MemoSubtitleVO_Width', 400);

  // Text pipe
  if IniFile.ReadBool('TextPipe', 'ShowTextPipe', False) then
    ActionShowHideTextPipe.Execute;
  SetTextPipeAutoOption(IniFile.ReadInteger('TextPipe', 'TextPipeAutoOption', 0));

  // Autoscroll options
  chkAutoScrollWAVDisp.Checked := ConfigObject.AutoScrollWavDisplay;
  chkAutoScrollSub.Checked := ConfigObject.AutoScrollSubtitles;

  LoadColumnsSettings(IniFile);

  WAVDisplayer.UpdateColors;

  // disable bttSendToItasa ahd automatic updated when required
  {$IFNDEF enhanced}
  MenuItemCheckUpdates.Visible := ( lowercase(ConfigObject.ModString) = 'for italiansubs');
  MenuItemRecentChanges.Visible := ( lowercase(ConfigObject.ModString) = 'for italiansubs');
  N29.Visible := ( lowercase(ConfigObject.ModString) = 'for italiansubs');
  {$ELSE}
  MenuItemHelp.Visible := False;
  if IsUniversalAppEnviroment = True then MenuItemCheckUpdates.Visible := False;
  MenuItemRecentChanges.Visible := False;
  N29.Visible := False;
  N31.Visible := False;
  MenuItemGooglePage.Visible := False;
  MenuItemGetMoreDictionaries.Enabled := False;
  {$ENDIF}

  IniFile.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadColumnsSettings(IniFile : TIniFile);
var Column : TVirtualTreeColumn;
    i, NewPos, NewWidth : Integer;
    MenuItem : TTntMenuItem;
    ColumnsPositionList, ColumnsWidthList : TStrings;

    StartupHiddenByUserCoreColumns, StartupColumnsPosition, StartupColumnsWidth : string;
begin
  StartupHiddenByUserCoreColumns := IniFile.ReadString('General', 'HiddenByUserCoreColumns', '');
  StartupColumnsPosition := IniFile.ReadString('General', 'ColumnsPosition', '');
  StartupColumnsWidth := IniFile.ReadString('General', 'ColumnsWidth', '');

  ColumnsPositionList := TStringList.Create;
  ColumnsPositionList.CommaText := StartupColumnsPosition;
  ColumnsWidthList := TStringList.Create;
  ColumnsWidthList.CommaText := StartupColumnsWidth;
  HiddenByUserCoreColumnsList.CommaText := StartupHiddenByUserCoreColumns;
  for i := 0 to vtvSubsList.Header.Columns.Count-1 do
  begin
    Column := vtvSubsList.Header.Columns.Items[i];
    if (i < ColumnsPositionList.Count) then
    begin
      NewPos := StrToIntDef(ColumnsPositionList[i], MaxInt);
      Column.Position := NewPos;
    end;
    if (i < ColumnsWidthList.Count) then
    begin
      NewWidth := StrToIntDef(ColumnsWidthList[i], Column.Width);
      Column.Width := NewWidth;
    end;
    if (HiddenByUserCoreColumnsList.IndexOf(Column.Text) <> -1) then
    begin
      Column.Options := Column.Options - [coVisible];
    end;
    MenuItem := TTntMenuItem.Create(SubListHeaderPopupMenu);
    MenuItem.Caption := Column.Text;
    MenuItem.OnClick := pmiToggleColumn;
    MenuItem.Checked := (coVisible in Column.Options);
    MenuItem.Tag := Integer(Column);
    Column.Tag := Integer(MenuItem);
    SubListHeaderPopupMenu.Items.Add(MenuItem);
  end;
  ColumnsPositionList.Free;
  ColumnsWidthList.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyMiscSettings;
begin
  WAVDisplayer.SceneChangeEnabled := ConfigObject.ShowSceneChange;
  WAVDisplayer.SceneChangeStartOffset := ConfigObject.SceneChangeStartOffset;
  WAVDisplayer.SceneChangeStopOffset := ConfigObject.SceneChangeStopOffset;
  WAVDisplayer.SceneChangeFilterOffset := ConfigObject.SceneChangeFilterOffset;
  WAVDisplayer.MinimumBlank := ConfigObject.SpaceKeyBlankBetweenSubtitles;
  WAVDisplayer.UpdateView([uvfRange]);
  g_SceneChangeWrapper.SetOffsets(ConfigObject.SceneChangeStartOffset,
    ConfigObject.SceneChangeStopOffset,
    ConfigObject.SceneChangeFilterOffset);
  g_SceneChangeWrapper.SetVisible(ConfigObject.ShowSceneChange);

  g_VSSCoreWrapper.SetCpsTarget(ConfigObject.SpaceKeyCPSTarget);
  g_VSSCoreWrapper.SetMinimumDuration(ConfigObject.SpaceKeyMinimalDuration);
  g_VSSCoreWrapper.SetMinimumBlank(ConfigObject.SpaceKeyBlankBetweenSubtitles);
  g_VSSCoreWrapper.SetMaximumDuration(ConfigObject.MaximumSubtitleDuration);

  MenuItemShowHideSceneChange.Checked := ConfigObject.ShowSceneChange;

  // Quick & Smart & Silent Zone Panels
  PageQuickSmartSilentZonePanels.Visible := ConfigObject.QuickSmartSilentZonePanels;

  if CurrentProject.ShowVO then
   WAVDisplayer.ShowVO(not ConfigObject.DisableVOShowTextInWAVDisplay);

  // desktop composition
  if ConfigObject.ForceNoDesktopComposition AND DSiAeroIsEnabled AND
     ( (DSiGetWindowsVersion = wvWin7 ) OR (DSiGetWindowsVersion = wvWinVista ) ) then
   begin
    DSiAeroDisable();
   end;

  // check updates
  TimerChkUpdates.Enabled := false;
  if ConfigObject.CheckUpdates then
   TimerChkUpdates.Enabled := true;

end;

//------------------------------------------------------------------------------

procedure TMainForm.FinishLoadSettings;
begin
  if(ConfigObject.SwapSubtitlesList) then
    Self.SwapSubList(False);

  ApplyAutoBackupSettings;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadAfterParamsSettings;
var Idx : Integer;
begin
  if (not Assigned(CurrentProject)) or (CurrentProject.Dictionary = '') then
  begin
    Idx := FSpellChecker.GetDictIdx(StartupDictionary);
    if (Idx <> -1) then
    begin
      LoadDict(Idx);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.InitVTV;
var Column : TVirtualTreeColumn;
begin
  with vtvSubsList do
  begin
    Font.Name := 'Arial';
    NodeDataSize := SizeOf(TTreeData);
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReportMode];
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
      [toFullRowSelect, toMultiSelect, toDisableDrawSelection {,toExtendedFocus}];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines, toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines, toUseBlendedSelection
        {, toHotTrack, toAlwaysHideSelection}];

    Header.Options := Header.Options + [hoVisible, hoAutoResize, hoAutoSpring, hoDrag];
    Header.Style := hsFlatButtons;

    Column := Header.Columns.Add;
    Column.Text := '#';
    Column.Width := 50;
    Column.MinWidth := 25;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Start';
    Column.Width := 100;
    Column.MinWidth := 50;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Stop';
    Column.Width := 100;
    Column.MinWidth := 50;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Style';
    Column.Width := 100;
    Column.MinWidth := 60;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Text';
    Column.Options := Column.Options - [coAllowClick]  + [coAutoSpring];
    Column.Width := 500;
    Column.MinWidth := 100;

    Column := Header.Columns.Add;
    Column.Text := 'VO/Other';
    Column.Options := Column.Options - [coAllowClick] - [coVisible] + [coAutoSpring];
    Column.Width := 500;
    Column.MinWidth := 100;

    //FocusedColumn := 2;

    OnGetText := vtvSubsListGetText;
  end;
  vtvSubsList.Header.AutoSizeIndex := TEXT_COL_INDEX;
end;

//------------------------------------------------------------------------------

procedure TMainForm.InitVTVExtraColumns;
var Column : TVirtualTreeColumn;
    i : Integer;
    VisibleExtraColumnsList : TStrings;
    IniFile : TIniFile;
    IniFilename : WideString;
    StartupVisibleExtraColumns : string;
begin
  IniFilename := GetIniFilename;
  IniFile := TIniFile.Create(IniFilename);
  StartupVisibleExtraColumns := IniFile.ReadString('General', 'VisibleExtraColumns', '');
  IniFile.Free;

  VisibleExtraColumnsList := TStringList.Create;
  VisibleExtraColumnsList.CommaText := StartupVisibleExtraColumns;
  for i := 0 to GeneralJSPlugin.ExtraColumnsCount-1 do
  begin
    Column := vtvSubsList.Header.Columns.Add;
    Column.Text := GeneralJSPlugin.GetColumnTitle(LAST_CORE_COL_INDEX + 1 + i);
    Column.MinWidth := GeneralJSPlugin.GetColumnSize(LAST_CORE_COL_INDEX + 1 + i);
    Column.Options := Column.Options - [coAllowClick];
    if (VisibleExtraColumnsList.IndexOf(Column.Text) = -1) then
    begin
      Column.Options := Column.Options - [coVisible];
    end;
  end;
  VisibleExtraColumnsList.Free;
end;

//------------------------------------------------------------------------------


procedure TMainForm.InitGeneralJSPlugin;
var GeneralJSFilename : WideString;
begin
  // Init VSS Core Wrapper
  g_VSSCoreWrapper.Set_INDEX_COL_IDX(INDEX_COL_INDEX);
  g_VSSCoreWrapper.Set_START_COL_IDX(START_COL_INDEX);
  g_VSSCoreWrapper.Set_STOP_COL_IDX(STOP_COL_INDEX);
  g_VSSCoreWrapper.Set_STYLE_COL_IDX(STYLE_COL_INDEX);
  g_VSSCoreWrapper.Set_TEXT_COL_IDX(TEXT_COL_INDEX);
  g_VSSCoreWrapper.Set_VO_COL_IDX(VO_COL_INDEX);
  g_VSSCoreWrapper.Set_LAST_CORE_COL_IDX(LAST_CORE_COL_INDEX);

  g_VSSCoreWrapper.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
  g_VSSCoreWrapper.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
  g_VSSCoreWrapper.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

  g_VSSCoreWrapper.SetVSSCoreEngine(Self);

  GeneralJSPlugin := TSimpleJavascriptWrapper.Create;
  GeneralJSPlugin.CoreColumnsCount := COLUMN_COUNT;
  GeneralJSFilename := g_PluginPath + 'general\general_plugin.js';
  if WideFileExists(GeneralJSFilename) then
  begin
    GeneralJSPlugin.OnJSPluginError := LogForm.LogMsg;
    if GeneralJSPlugin.LoadScript(GeneralJSFilename) then
    begin
      GeneralJSPlugin.OnJSPluginSetStatusBarText := OnJsSetStatusBarText;
      InitVTVExtraColumns;
    end
    else
      LogForm.SilentLogMsg('Can''t load ' + GeneralJSFilename);
  end
  else
    LogForm.SilentLogMsg('Can''t find ' + GeneralJSFilename);
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnRecentMenuItemClick(Sender : TObject);
var MenuItem : TTntMenuItem;
begin
  MenuItem := Sender as TTntMenuItem;
  LoadProject(MenuItem.Caption);
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetStatusBarPrimaryText(const Text : WideString);
begin
  StatusBarPrimaryText := Text;
  UpdateStatusBar;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateStatusBar;
var NewText : WideString;
begin
  if Length(StatusBarPrimaryText) > 0 then
  begin
    if Length(StatusBarSecondaryText) > 0 then
    begin
      NewText := StatusBarSecondaryText + ' (' + StatusBarPrimaryText + ')';
    end
    else
    begin
      NewText := StatusBarPrimaryText;
    end;
  end
  else
  begin
    NewText := StatusBarSecondaryText;
  end;


  if (NewText <> StatusBarPanel2.Caption) then
  begin
    StatusBarPanel2.Caption := NewText;
    StatusBarPanel2.Repaint;
  end;

end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowStatusBarMessage(const Text : WideString;
  const Duration : Integer);
begin
  if (Text <> StatusBarSecondaryText) then
  begin
    StatusBarSecondaryText := Text;
    UpdateStatusBar;
  end;
  TimerStatusBarMsg.Interval := Duration;
  TimerStatusBarMsg.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TimerStatusBarMsgTimer(Sender: TObject);
begin
  StatusBarSecondaryText := '';
  TimerStatusBarMsg.Enabled := False;
  UpdateStatusBar;
end;

//------------------------------------------------------------------------------

procedure TMainForm.EnableStyleControls(Enable : Boolean);
begin
  ActionStyles.Enabled := Enable;
  cbStyles.Enabled := Enable;
  bttStyles.Enabled := Enable;
end;

//------------------------------------------------------------------------------

procedure TMainForm.EnableControl(Enable : Boolean);
begin

  //ActionPreferences.Enabled := not Enable;
  ActionPlay.Enabled := Enable and (VideoRenderer.IsOpen or AudioOnlyRenderer.IsOpen);
  ActionStop.Enabled := ActionPlay.Enabled;
  ActionLoop.Enabled := ActionPlay.Enabled;
  ActionLoopSelStart.Enabled := ActionPlay.Enabled;
  ActionLoopSelEnd.Enabled := ActionPlay.Enabled;
  ActionNextSub.Enabled := Enable;
  ActionPreviousSub.Enabled := Enable;
  ActionPlaySelStart.Enabled := ActionPlay.Enabled;
  ActionPlaySelEnd.Enabled := ActionPlay.Enabled;
  ActionPlayToEnd.Enabled := ActionPlay.Enabled;
  ActionPlay1sBefore.Enabled := ActionPlay.Enabled;
  ActionPause.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate60.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate70.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate80.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate90.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate100.Enabled := ActionPlay.Enabled;
  ActionShiftForwardWaveForm.Enabled := ActionPlay.Enabled;
  ActionShiftBackwardWaveForm.Enabled := ActionPlay.Enabled;

  ActionToggleTimingMode.Enabled := ActionPlay.Enabled;
  bttWorkingMode.Enabled := ActionToggleTimingMode.Enabled;

  ActionZoomIn.Enabled := Enable;
  ActionZoomOut.Enabled := Enable;
  ActionZoomSelection.Enabled := Enable;
  ActionZoomAll.Enabled := Enable;
  ActionZoomVertical.Enabled := Enable;

  ActionFind.Enabled := Enable;
  ActionFindNext.Enabled := Enable;
  ActionGoto.Enabled := Enable;
  ActionCheckErrors.Enabled := Enable;
  ActionDelay.Enabled := Enable;
  ActionDelaySynchToCursor.Enabled := Enable;
  ActionDelayMoveNegative200.Enabled := Enable;
  ActionDelayMoveNegative100.Enabled := Enable;
  ActionDelayMoveNegative50.Enabled := Enable;
  ActionDelayMoveNegative10.Enabled := Enable;
  ActionDelayMovePositive200.Enabled := Enable;
  ActionDelayMovePositive100.Enabled := Enable;
  ActionDelayMovePositive50.Enabled := Enable;
  ActionDelayMovePositive10.Enabled := Enable;
  ActionResynchToolsSetPos.Enabled := Enable;
  if not Enabled then ActionResynchToolsDelPos.Enabled := Enable;
  if not Enabled then ActionResynchToolsReset.Enabled := Enable;
  ActionInsertTextFile.Enabled := Enable;
  ActionDiffSubFiles.Enabled := Enable AND (CurrentProject.SubtitlesVO<>'');

  ActionShowHideSceneChange.Enabled := Enable;
  ActionShowHideVideo.Enabled := Enable AND (CurrentProject.VideoStreamCount > '');
  ActionDetachVideo.Enabled := Enable;
  ActionProjectProperties.Enabled := Enable;
  ActionSendToItasa.Enabled := Enable AND ItaliansubsAuthorization(MainForm.ConfigObject.Username,
    MainForm.ConfigObject.Passwd);
  ActionReload.Enabled := Enable;
  ActionSaveAs.Enabled := Enable;
  ActionZipSubtitle.Enabled := Enable;
  ActionExportToSSA.Enabled := Enable;
  ActionExportToWAV.Enabled := Enable;
  ActionShowStartFrame.Enabled := Enable and VideoRenderer.IsOpen;
  ActionShowStopFrame.Enabled := Enable and VideoRenderer.IsOpen;
  ActionShowFrameAtCursor.Enabled := Enable and VideoRenderer.IsOpen;

  vtvSubsList.Enabled := Enable;
  MemoLinesCounter.Enabled := Enable;
  MemoSubtitleText.Enabled := Enable and
    ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
  MemoSubtitleVO.Enabled := Enable and
    ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
  ActionToggleVO.Enabled := Enable;
  ActionShowHideSubs.Enabled := Enable;

  edSelBegin.Enabled := Enable;
  labelSelBegin.Enabled := Enable;
  edSelEnd.Enabled := Enable;
  labelSelEnd.Enabled := Enable;
  edSelLength.Enabled := Enable;
  labelSelLenght.Enabled := Enable;
  edViewBegin.Enabled := Enable;
  edViewEnd.Enabled := Enable;
  edViewLength.Enabled := Enable;

  bttFastLess200ms.Enabled := Enable;
  bttFastLess100ms.Enabled := Enable;
  bttFastLess50ms.Enabled := Enable;
  bttFastLess10ms.Enabled := Enable;

  bttFastMore200ms.Enabled := Enable;
  bttFastMore100ms.Enabled := Enable;
  bttFastMore50ms.Enabled := Enable;
  bttFastMore10ms.Enabled := Enable;

  bttFastSmartWavCursor.Enabled := Enable;
  chkFastSmartWavCursor.Enabled := Enable;

  PageQuickSmartSilentZonePanels.Enabled := Enable;
  PageQuickSmartSilentZonePanels.ActivePageIndex := 0;


  chkAutoScrollWAVDisp.Enabled := Enable;
  chkAutoScrollSub.Enabled := Enable;
  lbAutoScroll.Enabled := Enable;

  ActionClose.Enabled := Enable;
  if Assigned(CurrentProject) then
    ActionSave.Enabled := CurrentProject.IsDirty
  else
    ActionSave.Enabled := False;
  ActionOpenProjectFolder.Enabled := Enable;
  ActionTranslationTemplate.Enabled := Enable;

  lblVolume.Enabled := Enable;
  tbVolume.Enabled := Enable;

  PanelVideo.Enabled := Enable;
  
  EnableFpsMenu(Enable);

end;

//------------------------------------------------------------------------------

procedure TMainForm.EnableFpsMenu(Enable : Boolean);
begin
  MenuItemFpsAdaptation.Enabled:= Enable;
  if not Enable then Exit;

  ChangeFpsTrueCinemaToCinema.Enabled := False;
  ChangeFpsPalToCinema.Enabled := False;
  ChangeFpsNtscToCinema.Enabled := False;

  ChangeFpsCinemaToTrueCinema.Enabled := False;
  ChangeFpsPalToTrueCinema.Enabled := False;
  ChangeFpsNtscToTrueCinema.Enabled := False;

  ChangeFpsCinemaToPal.Enabled := False;
  ChangeFpsTrueCinemaToPal.Enabled := False;
  ChangeFpsNtscToPal.Enabled := False;

  ChangeFpsCinemaToNtsc.Enabled := False;
  ChangeFpsTrueCinemaToNtsc.Enabled := False;
  ChangeFpsPalToNtsc.Enabled := False;

  if Enable and (CurrentProject <> nil) then
    if Pos('23.976', CurrentProject.VideoFrameRate) = 1  then
      begin
       ChangeFpsPalToCinema.Enabled := True;
       ChangeFpsNtscToCinema.Enabled := True;
       ChangeFpsTrueCinemaToCinema.Enabled := True;
      end;

  if Enable and (CurrentProject <> nil) then
    if Pos('24', CurrentProject.VideoFrameRate) = 1 then
      begin
       ChangeFpsCinemaToTrueCinema.Enabled := True;
       ChangeFpsPalToTrueCinema.Enabled := True;
       ChangeFpsNtscToTrueCinema.Enabled := True;
      end;

  if Enable and (CurrentProject <> nil) then
    if Pos('25', CurrentProject.VideoFrameRate) = 1 then
       begin
        ChangeFpsCinemaToPal.Enabled := True;
        ChangeFpsTrueCinemaToPal.Enabled := True;
        ChangeFpsNtscToPal.Enabled := True;
       end;

  if Enable and (CurrentProject <> nil) then
    if Pos('29.970 fps', CurrentProject.VideoFrameRate) = 1 then
       begin
        ChangeFpsCinemaToNtsc.Enabled := True;
        ChangeFpsTrueCinemaToNtsc.Enabled := True;
        ChangeFpsPalToNtsc.Enabled := True;
       end;

end;

//------------------------------------------------------------------------------


procedure TMainForm.WAVDisplayer1CursorChange(Sender: TObject);
begin
  if not WAVDisplayer.IsPlaying then
    begin
     plCursorPos.Caption := TimeMsToString(WAVDisplayer.GetCursorPos);
     WavDisplayerCursorPos := WAVDisplayer.GetCursorPos;
    end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1PlayCursorChange(Sender: TObject);
begin
  plCursorPos.Caption := TimeMsToString(WAVDisplayer.GetPlayCursorPos);
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectionChange(Sender: TObject);
begin
  edSelBegin.Text := TimeMsToString(WAVDisplayer.Selection.StartTime);
  edSelEnd.Text := TimeMsToString(WAVDisplayer.Selection.StopTime);
  edSelLength.Text := TimeMsToString(WAVDisplayer.Selection.StopTime -
    WAVDisplayer.Selection.StartTime);
  if (not WAVDisplayer.SelectionIsEmpty) then
  begin
    case PlayingMode of
      pmtAll: ; // do nothing
      pmtSelection:
        WAVDisplayer.UpdatePlayRange(WAVDisplayer.Selection.StartTime,
          WAVDisplayer.Selection.StopTime);
      pmtSelectionStart:
        WAVDisplayer.UpdatePlayRange(
          WAVDisplayer.Selection.StartTime,
          Min(WAVDisplayer.Selection.StartTime + StartEndPlayingDuration,WAVDisplayer.Selection.StopTime));
      pmtSelectionEnd:
        WAVDisplayer.UpdatePlayRange(
          Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
          WAVDisplayer.Selection.StopTime);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1ViewChange(Sender: TObject);
begin
  edViewBegin.Text := TimeMsToString(WAVDisplayer.Position);
  edViewEnd.Text := TimeMsToString(WAVDisplayer.Position + WAVDisplayer.PageSize);
  edViewLength.Text := TimeMsToString(WAVDisplayer.PageSize);
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedRange(Sender: TObject; SelectedRange : TRange; IsDynamic : Boolean);
var Node : PVirtualNode;
begin

  if not ConfigObject.ShowSpecialTagSubs And
    (copy(TSubtitleRange(WAVDisplayer.SelectedRange).Text,1,1) = '{') And
    (pos('}',TSubtitleRange(WAVDisplayer.SelectedRange).Text) > -1) then Exit;

  if Assigned(WAVDisplayer.SelectedRange) then
   begin
    Node := TSubtitleRange(WAVDisplayer.SelectedRange).Node;
    vtvSubsList.ScrollIntoView(Node,True);
    if vtvSubsList.FocusedNode <> Node then
      vtvSubsList.FocusedNode := Node
    else
      // special case for Autoscroll subtitles, force focus update
      vtvSubsListFocusChanged(vtvSubsList, Node, 0);
    vtvSubsList.ClearSelection;
    vtvSubsList.Selected[Node] := True;
    if (not MemoSubtitleText.Focused) and (MemoSubtitleText.Enabled) then
     begin
      Try MemoSubtitleText.SetFocus; except end;
     end;
    if ShowingVideo and (not WAVDisplayer.IsPlaying) and (not IsDynamic) then
     begin
      ActionShowStartFrameExecute(nil);
    end;
   end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedRangeChange(Sender: TObject);
var SubRange : TSubtitleRange;
    NodeData : PTreeData;
begin

  // Update subtitle timestamps
  if Assigned(WAVDisplayer.SelectedRange) then
  begin

    SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
    vtvSubsList.RepaintNode(SubRange.Node);

    // Update VO
    if (WAVDisplayer.RangeListVO.Count > 0) then
    begin
      NodeData := vtvSubsList.GetNodeData(SubRange.Node);
      UpdateMemoVO(NodeData);
    end;

    DisableVideoUpdatePreview := True;
    CurrentProject.IsDirty := True;
    UpdateLinesCounter;
    DisableVideoUpdatePreview := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedRangeChanged(
  Sender: TObject; OldStart, OldStop : Integer; NeedSort : Boolean);
var SubRange : TSubtitleRange;
    UndoableSetTimeTask : TUndoableSetTimeTask;
begin
  if Assigned(WAVDisplayer.SelectedRange) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
    UndoableSetTimeTask := TUndoableSetTimeTask.Create;
    UndoableSetTimeTask.SetData(SubRange.Node.Index, OldStart, OldStop,
      SubRange.StartTime, SubRange.StopTime);
    UndoableSetTimeTask.DoTask;
    PushUndoableTask(UndoableSetTimeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1OnPeakFileCreation(Sender: TObject;
  EventType : TPeakFileCreationEventType; Param : Integer);
begin
  case EventType of
    pfcevtStart:
      begin
        PeakCreationProgressForm := TPeakCreationProgressForm.Create(nil);
        PeakCreationProgressForm.Show;
        Self.Enabled := False;
      end;
    pfcevtProgress:
      begin
        PeakCreationProgressForm.SetProgress(Param);
        Application.ProcessMessages;
      end;
    pfcevtStop:
      begin
        Self.Enabled := True;
        PeakCreationProgressForm.Free;
        PeakCreationProgressForm := nil;
        Application.ProcessMessages;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1AutoScrollChange(Sender: TObject);
begin
  SetCheckedState(chkAutoScrollWAVDisp, WAVDisplayer.AutoScrolling);
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1UpdateColors(Sender: TObject);
begin
  PreferencesFormInstance.TempSaveWavColors(ConfigObject);
  WAVDisplayer.UpdateColors;
end;

//------------------------------------------------------------------------------

procedure TMainForm.chkAutoScrollWAVDispClick(Sender: TObject);
begin
  WAVDisplayer.AutoScrolling := chkAutoScrollWAVDisp.Checked;
  ConfigObject.AutoScrollWavDisplay := chkAutoScrollWAVDisp.Checked;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CurrentProjectOnDirtyChange(Sender : TObject);
var s : WideString;
begin
  if CurrentProject.IsDirty then s := '*' else s := '';
  Self.Caption := ApplicationName + ' - ' +
    WideExtractFileName(CurrentProject.Filename) + s;
  ActionSave.Enabled := CurrentProject.IsDirty;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayPopup_DeleteRange(Sender: TObject);
var DeleteIdx : Integer;
    UndoableDeleteTask : TUndoableDeleteTask;
begin
  DeleteIdx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if (DeleteIdx <> -1) then
  begin
    UndoableDeleteTask := TUndoableDeleteTask.Create;
    UndoableDeleteTask.SetCapacity(1);
    UndoableDeleteTask.AddSubtitleIndex(DeleteIdx);
    UndoableDeleteTask.DoTask;
    PushUndoableTask(UndoableDeleteTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSplitAtCursorExecute(Sender: TObject);
var Idx : Integer;
    Range : TRange;
    UndoableSplitTask : TUndoableSplitTask;
begin
  Idx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if (Idx <> -1) then
  begin
    UndoableSplitTask := TUndoableSplitTask.Create;
    Range := WAVDisplayer.RangeList[Idx];
    UndoableSplitTask.SetData(Idx, Range.StartTime, Range.StopTime,
      WAVDisplayer.GetCursorPos, ConfigObject.SpaceKeyBlankBetweenSubtitles);
    UndoableSplitTask.DoTask;
    PushUndoableTask(UndoableSplitTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadSubtitles(Filename: WideString; var IsUTF8 : Boolean);
var Ext : WideString;
    StyleColumn : TVirtualTreeColumn;
    EnableStyle : Boolean;
begin
  EnableStyle := False;
  StyleColumn := vtvSubsList.Header.Columns.Items[STYLE_COL_INDEX];
  g_WebRWSynchro.BeginWrite;
  try
    vtvSubsList.BeginUpdate;
    Ext := WideLowerCase(WideExtractFileExt(Filename));
    if (Ext = '.srt') then
    begin
      LoadSRT(Filename, IsUTF8);
      ShowColumn(StyleColumn, False);
    end
    else if (Ext = '.ass') then
    begin
      EnableStyle := True;
      LoadASS(Filename, IsUTF8, False);
      ShowColumn(StyleColumn, True);
      StyleFormInstance.ConfigureMode(sfmASS);
    end
    else if (Ext = '.ssa') then
    begin
      EnableStyle := True;
      LoadASS(Filename, IsUTF8, True);
      ShowColumn(StyleColumn, True);
      StyleFormInstance.ConfigureMode(sfmSSA);
    end;
  finally
    g_WebRWSynchro.EndWrite;
    vtvSubsList.EndUpdate;
  end;
  WAVDisplayer.UpdateView([uvfRange]);

  AutoFitSubsListColumns;

  vtvSubsList.Repaint;

  if StyleFormInstance.GetCount = 0 then
  begin
    StyleFormInstance.AddDefaultStyle;
  end;
  UpdateStylesComboBox;

  EnableStyleControls(EnableStyle);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowColumn(Column : TVirtualTreeColumn; Show, FromUser : Boolean);
var i : Integer;
    MenuItem : TMenuItem;
    HiddenByUser : Boolean;
begin
  i := HiddenByUserCoreColumnsList.IndexOf(Column.Text);
  HiddenByUser := (i <> -1);

  if (FromUser = True) then
  begin
    // User force this column to always be displayed or hidden
    if (Show) then
    begin
      Column.Options := Column.Options + [coVisible];
      if (HiddenByUser) then
        HiddenByUserCoreColumnsList.Delete(i);
    end
    else
    begin
      Column.Options := Column.Options - [coVisible];
      if (not HiddenByUser) then
        HiddenByUserCoreColumnsList.Add(Column.Text);
    end;
  end
  else
  begin
    if (Show) and (not HiddenByUser) then
      Column.Options := Column.Options + [coVisible]
    else
      Column.Options := Column.Options - [coVisible];
  end;

  // Update the popup menu
  for i := 0 to SubListHeaderPopupMenu.Items.Count-1 do
  begin
    MenuItem := SubListHeaderPopupMenu.Items[i];
    Column := (TObject(MenuItem.Tag) as TVirtualTreeColumn);
    MenuItem.Checked := (coVisible in Column.Options);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadVO(Filename: WideString);
begin
  g_WebRWSynchro.BeginWrite;
  try
    vtvSubsList.BeginUpdate;
    LoadVOSubs(Filename);
    AdjustShowVO;
  finally
    g_WebRWSynchro.EndWrite;
    vtvSubsList.EndUpdate;
  end;
  vtvSubsList.Repaint;
  if Assigned(vtvSubsList.FocusedNode) then
    UpdateMemoVO(vtvSubsList.GetNodeData(vtvSubsList.FocusedNode));
end;

//------------------------------------------------------------------------------

function TMainForm.LoadVOSubs(Filename: WideString) : Boolean;
var
  i, Start, Stop : Integer;
  NewRange : TRange;
  SRTParser : TSRTParser;
  SRTSub : TSRTSubtitle;
  SSAParser : TSSAParser;
  Ext, SubText : WideString;
begin
  Result := False;
  WAVDisplayer.ClearRangeListVO;
  if not WideFileExists(Filename) then
  begin
    Exit;
  end;

  Ext := WideLowerCase(WideExtractFileExt(Filename));
  if (Ext = '.srt') then
  begin
    SRTParser := TSRTParser.Create;
    SRTParser.Load(Filename);
    for i := 0 to SRTParser.GetCount-1 do
    begin
      SRTSub := SRTParser.GetAt(i);
      NewRange := SubRangeFactory.CreateRangeSS(SRTSub.Start, SRTSub.Stop);
      TSubtitleRange(NewRange).Text := SRTSub.Text;
      WAVDisplayer.RangeListVO.AddAtEnd(NewRange);
    end;
    FreeAndNil(SRTParser);
  end
  else if (Ext = '.ass') or (Ext = '.ssa') then
  begin
    SSAParser := TSSAParser.Create;
    SSAParser.Load(Filename);
    for i := 0 to SSAParser.GetDialoguesCount-1 do
    begin
      Start := TimeStringToMS_SSA(SSAParser.GetDialogueValueAsString(i, 'Start'));
      Stop := TimeStringToMS_SSA(SSAParser.GetDialogueValueAsString(i, 'End'));
      if (Start <> -1) and (Stop <> -1) then
      begin
        NewRange := SubRangeFactory.CreateRangeSS(Start, Stop);
        SubText := SSAParser.GetDialogueValueAsString(i, 'Text');
        TSubtitleRange(NewRange).Text := Tnt_WideStringReplace(SubText, '\N', CRLF,[rfReplaceAll]);
        WAVDisplayer.RangeListVO.AddAtEnd(NewRange);
      end;
    end;
    FreeAndNil(SSAParser);
  end;

  WAVDisplayer.RangeListVO.FullSort;

  Result := True;
end;

//------------------------------------------------------------------------------

function TMainForm.LoadSRT(Filename: WideString; var IsUTF8 : Boolean) : Boolean;
var
  i : integer;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  SRTParser : TSRTParser;
  SRTSub : TSRTSubtitle;
begin
  Result := False;
  if not WideFileExists(Filename) then
  begin
    WAVDisplayer.ClearRangeList;
    vtvSubsList.Clear;
    Exit;
  end;

  SubtitleFileHeader := '';
  SubtitleFileFooter := '';
  WAVDisplayer.ClearRangeList;

  SRTParser := TSRTParser.Create;
  SRTParser.Load(Filename);
  IsUTF8 := SRTParser.IsUTF8;
  for i := 0 to SRTParser.GetCount-1 do
  begin
    SRTSub := SRTParser.GetAt(i);
    NewRange := SubRangeFactory.CreateRangeSS(SRTSub.Start, SRTSub.Stop);
    TSubtitleRange(NewRange).Text := SRTSub.Text;
    if (EnableExperimentalKaraoke = True) then
      NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);
    WAVDisplayer.RangeList.AddAtEnd(NewRange);
  end;
  WAVDisplayer.RangeList.FullSort;

  vtvSubsList.Clear;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    Node := vtvSubsList.AddChild(nil);
    NodeData := vtvSubsList.GetNodeData(Node);
    NodeData.Range := TSubtitleRange(WAVDisplayer.RangeList[i]);
    TSubtitleRange(WAVDisplayer.RangeList[i]).Node := Node;
  end;
  WAVDisplayer.UpdateView([uvfRange]);
  if SRTParser.AutoCorrectedFile then
    CurrentProject.IsDirty := True;
  FreeAndNil(SRTParser);
  Result := True;
end;

//------------------------------------------------------------------------------

function TMainForm.LoadASS(Filename: WideString; var IsUTF8 : Boolean; isSSA : Boolean) : Boolean;
var
  i, Start, Stop : Integer;
  SubText : WideString;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  ssaParser : TSSAParser;
begin
  Result := False;
  if not WideFileExists(Filename) then
  begin
    SubtitleFileHeader := '';
    SubtitleFileFooter := '';
    WAVDisplayer.ClearRangeList;
    vtvSubsList.Clear;
    StyleFormInstance.ClearAll;
    StyleFormInstance.AddDefaultStyle;
    Exit;
  end;

  WAVDisplayer.ClearRangeList;

  ssaParser := TSSAParser.Create;
  ssaParser.Load(Filename);
  IsUTF8 := ssaParser.GetIsUTF8;
  SubtitleFileHeader := ssaParser.GetHeaderLines;
  SubtitleFileFooter := ssaParser.GetFooterLines;

  StyleFormInstance.LoadStylesFromParser(ssaParser);

  for i := 0 to ssaParser.GetDialoguesCount-1 do
  begin
    Start := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'Start'));
    Stop := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'End'));
    if (Start <> -1) and (Stop <> -1) then
    begin
      NewRange := SubRangeFactory.CreateRangeSS(Start, Stop);
      SubText := ssaParser.GetDialogueValueAsString(i, 'Text');
      TSubtitleRange(NewRange).Text := Tnt_WideStringReplace(SubText, '\N', CRLF,[rfReplaceAll]);
      if ssaParser.GetIsASS then
        TSubtitleRange(NewRange).Layer := ssaParser.GetDialogueValueAsString(i, 'Layer')
      else
        TSubtitleRange(NewRange).Marked := ssaParser.GetDialogueValueAsString(i, 'Marked');
      TSubtitleRange(NewRange).Effect := ssaParser.GetDialogueValueAsString(i, 'Effect');
      TSubtitleRange(NewRange).RightMarg := ssaParser.GetDialogueValueAsString(i, 'MarginR');
      TSubtitleRange(NewRange).LeftMarg := ssaParser.GetDialogueValueAsString(i, 'MarginL');
      TSubtitleRange(NewRange).VertMarg := ssaParser.GetDialogueValueAsString(i, 'MarginV');
      TSubtitleRange(NewRange).Style := ssaParser.GetDialogueValueAsString(i, 'Style');
      TSubtitleRange(NewRange).Actor := ssaParser.GetDialogueValueAsString(i, 'Name');
      if (EnableExperimentalKaraoke = True) then
        NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);
      WAVDisplayer.RangeList.AddAtEnd(NewRange);
    end;
  end;
  ssaParser.Free;

  WAVDisplayer.RangeList.FullSort;

  vtvSubsList.Clear;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    Node := vtvSubsList.AddChild(nil);
    NodeData := vtvSubsList.GetNodeData(Node);
    NodeData.Range := TSubtitleRange(WAVDisplayer.RangeList[i]);
    TSubtitleRange(WAVDisplayer.RangeList[i]).Node := Node;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------

function TMainForm.GetVOText(NodeData: PTreeData; Enhanced : Boolean) : WideString;

  function SearchStartVO(timing : Integer) : Integer;
  	var i : Integer;
  begin
  	if WAVDisplayer.RangeListVO.Count > 0 then
    begin
  		for i := 0 to (WAVDisplayer.RangeListVO.Count - 1) do
      begin
  			if WAVDisplayer.RangeListVO[i].StopTime >= timing then
        begin
  				Result := i;
          exit;
        end;
      end;
  	end;
  	Result := WAVDisplayer.RangeListVO.Count;
  end;

  function SearchEndVO(timing : Integer) : Integer;
  	var i : Integer;
  begin
  	if WAVDisplayer.RangeListVO.Count > 0 then
    begin
  		for i := (WAVDisplayer.RangeListVO.Count - 1) downto 0 do
      begin
  			if WAVDisplayer.RangeListVO[i].StartTime <= timing then
        begin
  				Result := i;
          exit;
        end;
      end;
  	end;
  	Result := -1;
  end;

  var
    CurrSub : Integer;
    FirstVO : Integer;
    LastVO : Integer;
    PrevVO : Integer;
    NextVO : Integer;
    Text : WideString;
    PrevText : WideString;
    NextText : WideString;
    i : Integer;
begin
  Result := '';
  CurrSub := WAVDisplayer.RangeList.IndexOf(NodeData.Range);
  if (CurrSub >= 0) and (WAVDisplayer.RangeListVO.Count > 0) then
  begin
    FirstVO := SearchStartVO(NodeData.Range.StartTime);
    LastVO := SearchEndVO(NodeData.Range.StopTime);

    if CurrSub > 0 then
      PrevVO := SearchEndVO(WAVDisplayer.RangeList[CurrSub - 1].StopTime) + 1
    else
      PrevVO := 0;

    if CurrSub < (WAVDisplayer.RangeList.Count - 1) then
      NextVO := SearchStartVO(WAVDisplayer.RangeList[CurrSub + 1].StartTime) - 1
    else
      NextVO := WAVDisplayer.RangeListVO.Count - 1;

    Text := '';
    PrevText := '';
    NextText := '';

  	for i := FirstVO to LastVO do
    begin
  		if i > FirstVO then
        Text := Text + '|';
      Text := Text +
        StringConvertCRLFToPipe(
          StringConvertPipeToSpace(
            TSubtitleRange(WAVDisplayer.RangeListVO[i]).Text));
    end;

  	for i := (FirstVO - 1) downto PrevVO do
    begin
  		if i < (FirstVO - 1) then
        PrevText := ' ' + PrevText;
      PrevText := TSubtitleRange(WAVDisplayer.RangeListVO[i]).Text + PrevText;
      if Length(PrevText) > MemoVOMaxUnassigned then
        break;
    end;

  	for i := (LastVO + 1) to NextVO do
    begin
  		if i > (LastVO + 1) then
        NextText := NextText + ' ';
      NextText := NextText + TSubtitleRange(WAVDisplayer.RangeListVO[i]).Text;
      if Length(NextText) > MemoVOMaxUnassigned then
        break;
    end;

    if Enhanced then
    begin
      Text :=
        StringConvertShrinkSpace(PrevText)
        + '|'
        + Text
        + '|'
        + StringConvertShrinkSpace(NextText);
    end
    else
    begin
      if PrevText <> '' then
        Text := '[<<<] ' + Text;
      if NextText <> '' then
        Text := Text + ' [>>>]';
      Text := StringConvertShrinkSpace(Text);
    end;

    Result := Text;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateMemoVO(NodeData : PTreeData);
  var
    EnhText : WideString;
    PrevText : WideString;
    CurrText : WideString;
    NextText : WideString;
    TextFirst : Integer;
    TextLast : Integer;
    CountRows : Integer;
    StartCount : Integer;
    RowAvail : Integer;
    eventMask : Integer;
begin

  EnhText := GetVOText(NodeData, True);

  for TextFirst := 1 to Length(EnhText) do
    if EnhText[TextFirst] = '|' then
      Break;

  for TextLast := Length(EnhText) downto 1 do
    if EnhText[TextLast] = '|' then
      Break;

  if TextLast <= TextFirst then // Something wrong
  begin
    MemoSubtitleVO.Text := EnhText;
    exit;
  end;

  PrevText := Copy(EnhText, 1, TextFirst - 1);
  If PrevText <> '' then
    PrevText := PrevText + ' [<<<]';
  CurrText := Copy(EnhText, TextFirst + 1, TextLast - TextFirst - 1);
  NextText := Copy(EnhText, TextLast + 1, Length(EnhText) - TextLast);
  If NextText <> '' then
    NextText := '[>>>] ' + NextText;

  // A little shrinking

  if Length(PrevText) > MemoVOMaxLength then
    PrevText := '[...]' + Copy(PrevText, Length(PrevText) - MemoVOMaxLength + 6, MemoVOMaxLength - 5);

  if Length(NextText) > MemoVOMaxLength then
    NextText := Copy(NextText, 0, MemoVOMaxLength - 5) + '[...]';

  StartCount := 0;
  CountRows := 1;

  repeat
    StartCount := PosEx('|', CurrText, StartCount + 1);
    if StartCount > 0 then
      Inc(CountRows);
  until StartCount <= 0;

  RowAvail := MemoVOMaxRows;
  if PrevText <> '' then
    Dec(RowAvail);
  if NextText <> '' then
    Dec(RowAvail);

  if CountRows > RowAvail then
  begin
    CurrText := StringConvertShrinkSpace(CurrText);
    StartCount := 0;
    while RowAvail > 1 do
    begin
      StartCount := PosEx(' ', CurrText, StartCount + 1 + ((Length(CurrText) - StartCount - 1) div RowAvail));
      if StartCount <= 0 then
        break;
      CurrText := Copy(CurrText, 1, StartCount - 1) + '|' + Copy(CurrText, StartCount + 1, Length(CurrText) - StartCount);
      Dec(RowAvail);
    end;
  end;

  CurrText := StringConvertPipeToCRLF(CurrText);

  // Prepare text

  if (PrevText <> '') and ((CurrText <> '') or (NextText <> '')) then
    PrevText := PrevText + #13#10;

  TextFirst := Length(PrevText);

  if (CurrText <> '') and (NextText <> '') then
    CurrText := CurrText + #13#10;

  TextLast := TextFirst + Length(CurrText);

  EnhText := PrevText + CurrText + NextText;

  // Editbox

  MemoSubtitleVO.Text := EnhText;

  MemoSubtitleVO.Tag := 0;
  eventMask := MemoSubtitleVO.Perform(EM_SETEVENTMASK, 0, 0);
  MemoSubtitleVO.Lines.BeginUpdate;

  MemoSubtitleVO.SelectAll;
  SetRESelectionColor(MemoSubtitleVO, clWindowText);
  SetReUnderline(MemoSubtitleVO, False);

  if TextFirst > 0 then
  begin
    SetRESelection(MemoSubtitleVO, 0, TextFirst);
    SetRESelectionColor(MemoSubtitleVO, clGrayText);
  end;

  if TextLast > TextFirst then
  begin
    SetRESelection(MemoSubtitleVO, TextFirst, TextLast - TextFirst);
    SetRESelectionColor(MemoSubtitleVO, clWindowText);
  end;

  if Length(EnhText) > TextLast then
  begin
    SetRESelection(MemoSubtitleVO, TextLast, Length(EnhText) - TextLast);
    SetRESelectionColor(MemoSubtitleVO, clGrayText);
  end;

  SetRESelection(MemoSubtitleVO, 0, 0);

  MemoSubtitleVO.Lines.EndUpdate;
  MemoSubtitleVO.Perform(EM_SETEVENTMASK, 0, eventMask);
  MemoSubtitleVO.Tag := 1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  NodeData: PTreeData;
  CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  NodeData := Sender.GetNodeData(Node);

  case Column of
    -1: CellText := ''; // -1 if columns are hidden
    INDEX_COL_INDEX: CellText := IntToStr(Node.Index+1);
    START_COL_INDEX: CellText := TimeMsToString(NodeData.Range.StartTime);
    STOP_COL_INDEX: CellText := TimeMsToString(NodeData.Range.StopTime);
    STYLE_COL_INDEX: CellText := NodeData.Range.Style;
    TEXT_COL_INDEX: CellText := StringConvertCRLFToPipe(NodeData.Range.Text);
    VO_COL_INDEX: CellText := GetVOText(NodeData, False);
  end;

  if GeneralJSPlugin.HasColumnCustomText(Column) then
  begin
    GetCurrentPreviousNextSubtitles(Node, CurrentSub, PreviousSub, NextSub);
    CellText := GeneralJSPlugin.GetColumnText(Column, CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Msg : TCMMouseWheel;
  TargetControl : HWND;
begin
  if (WheelDelta = 0) then
  begin
    // When there is a lot of things to display the mouse driver seems to be going behind
    // and WheelDelta = 0, this is getting us in a stack overflow !
    Handled := True;
    Exit;
  end;
    
  // Recreate original CM_MOUSEWHEEL Message
  Msg.ShiftState := Shift;
  Msg.WheelDelta := WheelDelta;
  Msg.XPos := MousePos.X;
  Msg.YPos := MousePos.Y;

  // Find the control under the mouse pointer
  TargetControl := WindowFromPoint(MousePos);

  if (TargetControl = WAVDisplayer.Handle) then
  begin
    Handled := WAVDisplayer.Perform(CM_MOUSEWHEEL, TMessage(Msg).WParam,
      TMessage(Msg).LParam) <> 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagHighlight(RichEdit : TTntRichEdit; TagIndex : Integer);
var savSelStart, savSelLength : Integer;
    i, j, eventMask, len : Integer;
    WordArray : TWideStringDynArray;
    WordColor : TColor;
    TextToSpell, AWord : WideString;
    Offset : Integer;
    SpellOk : Boolean;
    WordInfo : TWordInfo;
begin
  RichEdit.Tag := 0;
  savSelStart := RichEdit.SelStart;
  savSelLength := RichEdit.SelLength;

  TagSplit(RichEdit.Text, WordArray);

  eventMask := RichEdit.Perform(EM_SETEVENTMASK, 0, 0);

  RichEdit.Lines.BeginUpdate;

  RichEdit.SelectAll;
  SetRESelectionColor(RichEdit, clWindowText);
  SetReUnderline(RichEdit, False);

  j := 0;
  for i:=0 to Length(WordArray)-1 do
  begin
    if (i = (TagIndex*2)+1) then
      WordColor := clRed
    else if (i mod 2) = 0 then
      WordColor := clWindowText
    else
      WordColor := clGrayText;

    len := Length(WordArray[i]);
    if (WordColor <> clWindowText) then
    begin
      // Richedit count line break as 1 character only
      SetRESelection(RichEdit, j, len - WideStringCount(WordArray[i], CRLF));
      SetRESelectionColor(RichEdit, WordColor);
    end
    else
    begin
      // Spellcheck
      if (MenuItemLiveSpellcheck.Checked and FSpellChecker.IsInitialized) then
      begin
        Offset := 1;
        TextToSpell := WordArray[i];
        while (Offset <= Length(TextToSpell)) do
        begin
          GetNextWordPos(Offset, TextToSpell, WordInfo);
          AWord := Copy(TextToSpell, WordInfo.Position, WordInfo.Length);

          SpellOk := ContainDigit(AWord) or IsUpperCase(AWord) or FSpellChecker.Spell(AWord);
          if (not SpellOk) then
          begin
            SetRESelection(RichEdit, j + WordInfo.Position - 1, WordInfo.Length);
            SetReUnderline(RichEdit, not SpellOk);
          end;

          Inc(Offset);
        end;
      end;
    end;

    Inc(j, len);
  end;
  SetRESelection(RichEdit, savSelStart, savSelLength);

  RichEdit.Lines.EndUpdate;

  RichEdit.Perform(EM_SETEVENTMASK, 0, eventMask);

  RichEdit.Tag := 1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoSubtitleTextChange(Sender: TObject);
var NodeData, NodeDataPrevious: PTreeData;
    NeedUpdate : Boolean;
begin
  if Assigned(vtvSubsList.FocusedNode) and (MemoSubtitleText.Tag = 1) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

   if (ConfigObject.AccentsAssistant And (CurrentProject.Dictionary = 'it_IT.dic') And MenuItemLiveSpellcheck.Checked And FSpellChecker.IsInitialized And not CurrentMemoSubtitleTextSelectionOn )  then
     begin
       if Assigned(vtvSubsList.GetPrevious(vtvSubsList.FocusedNode)) then
        begin
         NodeDataPrevious := vtvSubsList.GetNodeData(vtvSubsList.GetPrevious(vtvSubsList.FocusedNode));
         SmartUppercase(MemoSubtitleText, NodeDataPrevious.Range.Text);
        end
       else SmartUppercase(MemoSubtitleText, '');
     end;

    if (ConfigObject.AccentsAssistant And (CurrentProject.Dictionary = 'it_IT.dic') And MenuItemLiveSpellcheck.Checked And FSpellChecker.IsInitialized) then
      begin
        SmartAccents(MemoSubtitleText);
      end;

    g_WebRWSynchro.BeginWrite;
    try
      NodeData.Range.Text := MemoSubtitleText.Text;

      NeedUpdate := False;
      if (EnableExperimentalKaraoke = True) then
        NeedUpdate := NodeData.Range.UpdateSubTimeFromText(NodeData.Range.Text);

      vtvSubsList.RepaintNode(vtvSubsList.FocusedNode);
      CurrentProject.IsDirty := True;
    finally
      g_WebRWSynchro.EndWrite;
    end;

    if (NeedUpdate = True) then
    begin
      WAVDisplayer.UpdateView([uvfRange]);
    end;

    if (EnableExperimentalKaraoke = True) then
      TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);

  end;
  if CurrentMemoSubtitleTextSelectionOn then CurrentMemoSubtitleTextSelectionOn := False;
  UpdateLinesCounter;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoSubtitleTextSelectionChange(Sender: TObject);
//var TopTask : TUndoableTask;
begin

  if Length(MemoSubtitleText.SelText) > 0 then
  begin
    CurrentMemoSubtitleTextSelectionOn := True;
  end;

  UpdateLinesCounter;

  { TODO
  OutputDebugString(PChar(BoolToStr(TTntRichEditCustomUndo(MemoSubtitleText).HasTextChanged, True)));
  if (UndoStack.Count > 0) then
  begin
    TopTask := TUndoableTask(UndoStack.Peek);
    if (TopTask is TUndoableSubTextTask) then
    begin
      TUndoableTextTask(TUndoableSubTextTask(TopTask).GetSubTask).DisableMerge;
    end;
  end;}

end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateLinesCounter;
var s, StrippedText : WideString;
    i, CharCount, TotalCharCount, TotalCharCountWithCRLF, CPS : Integer;
    LineNo, LineStart, ColNo : Integer;
    NodeData: PTreeData;
    SubDuration : Integer;
    LineArray : TWideStringDynArray;
begin
  s := '';
  TotalCharCount := 0;
  TotalCharCountWithCRLF := 0;
  StrippedText := StripTagsKeepNL(MemoSubtitleText.Text);
  ExplodeW(CRLF, StrippedText, LineArray);

  for i := MemoSubtitleText.Perform(EM_GETFIRSTVISIBLELINE, 0, 0) to MemoSubtitleText.Lines.Count-1 do
  begin
    CharCount := Length(LineArray[i]);
    Inc(TotalCharCount, CharCount);
    s := s + IntToStr(CharCount) + CRLF;
  end;
  for i := MemoSubtitleText.Lines.Count to MemoSubtitleText.Perform(EM_GETLINECOUNT,0,0)-1 do
  begin
    s := s + '0' + CRLF;
  end;
  MemoLinesCounter.Text := Trim(s);

  // line return count for 2 chars
  if (MemoSubtitleText.Lines.Count > 0) then
    TotalCharCountWithCRLF := TotalCharCount + ((MemoSubtitleText.Lines.Count-1)*2);

  LineNo := MemoSubtitleText.Perform(EM_LINEFROMCHAR, -1, 0);
  LineStart := MemoSubtitleText.Perform(EM_LINEINDEX, LineNo, 0);
  ColNo := MemoSubtitleText.SelStart + MemoSubtitleText.SelLength - LineStart - LineNo + 1;

  s := 'Line: ' + IntToStr(LineNo+1) + ', Column: ' + IntToStr(ColNo);
  s := s + ' | Total: ' + IntToStr(TotalCharCount);
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    SubDuration := NodeData.Range.StopTime - NodeData.Range.StartTime;
    if (SubDuration > 0) then
      CPS := Round(TotalCharCountWithCRLF / (SubDuration / 1000))
    else
      CPS := -1;
    s := s + ', Char/s: ' + IntToStr(CPS);
  end;

  StatusBarPanel1.Caption := s;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitles(Filename: WideString; PreviousFilename : WideString;
  InUTF8 : Boolean; BackupOnly : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
var Ext, PreviousExt : WideString;
    BackupDstFilename : WideString;
    InternalTranslator : TTranslator;
begin
  if (not BackupOnly) and WideFileExists(Filename) and ConfigObject.EnableBackup then
  begin
    CheckBackupDirectory;
    BackupDstFilename := g_BackupDirectory + WideChangeFileExt(WideExtractFileName(Filename), '.bak');
    WideCopyFile(Filename, BackupDstFilename, False);
  end;

  if not Assigned(Translator) then
  begin
    InternalTranslator := GetTranslatorCopyOriginalCommon;
  end
  else
  begin
    InternalTranslator := Translator;
  end;

  Ext := WideLowerCase(WideExtractFileExt(Filename));
  if BackupOnly and (Ext = '.bak') then
    Ext := WideLowerCase(WideExtractFileExt(CurrentProject.SubtitlesFile));
  PreviousExt := WideLowerCase(WideExtractFileExt(PreviousFilename));
  if (Ext = '.srt') then
    SaveSubtitlesAsSRT(Filename, PreviousExt, InUTF8, InternalTranslator, ExcludeUnselected)
  else if (Ext = '.ass') then
    SaveSubtitlesAsASS(Filename, PreviousExt, InUTF8, InternalTranslator, ExcludeUnselected)
  else if (Ext = '.ssa') then
    SaveSubtitlesAsSSA(Filename, PreviousExt, InUTF8, InternalTranslator, ExcludeUnselected)
  else if (Ext = '.cue') then
    SaveSubtitlesAsCUE(Filename, ExcludeUnselected)
  else if (Ext = '.txt') then
    SaveSubtitlesAsTXT(Filename, PreviousExt, InUTF8, ExcludeUnselected)
  else if (Ext = '.csv') then
    SaveSubtitlesAsCSV(Filename, PreviousExt, InUTF8, ExcludeUnselected)
  else
  begin
    if Assigned(CurrentProject) and (Filename = CurrentProject.SubtitlesFile) then
      MessageBoxW(Handle, PWideChar(WideString('Cannot save the file "' + Filename +
        '". Please, specify a valid file extension in the "Project Properties" dialog ' +
        'under the "File" menu.')),
        PWideChar(WideString('Error')), MB_OK or MB_ICONERROR)
    else
      MessageBoxW(Handle, PWideChar(WideString('Cannot save the file "' + Filename +
        '". Please, specify a valid file extension.')),
        PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    Exit;
  end;

  if (not BackupOnly) then
  begin
    ApplyAutoBackupSettings; // Reset auto-backup timing
    CurrentProject.IsDirty := False
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListDblClick(Sender: TObject);
var NodeData: PTreeData;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    WAVDisplayer.ZoomAndSelectRange(NodeData.Range);
    if WAVDisplayer.IsPlaying then
      WAVDisplayer.AutoScrolling := False;
    if ShowingVideo and Assigned(Sender) and (not WAVDisplayer.IsPlaying) then
    begin
      ActionShowStartFrameExecute(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateVideoRendererWindow;
begin
  if (CurrentProject.VideoPanelWidth > 0) then
  begin
    PanelVideo.Width := CurrentProject.VideoPanelWidth;
  end
  else if (VideoRenderer.VideoWidth > 0) and (VideoRenderer.VideoHeight > 0) then
  begin
    PanelVideo.Width := (VideoRenderer.VideoWidth * PanelVideo.Height) div VideoRenderer.VideoHeight;
  end;

  if MenuItemDetachVideoWindow.Checked then
    VideoRenderer.SetDisplayWindow(DetachedVideoForm.Handle)
  else
    VideoRenderer.SetDisplayWindow(PanelVideo.Handle);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ClearVideoSceneChange;
var SCArray : TIntegerDynArray;
begin
  SetLength(SCArray, 0);
  WAVDisplayer.SetSceneChangeList(SCArray);
  WAVDisplayer.UpdateView([uvfRange]);
  g_SceneChangeWrapper.SetSceneChangeList(SCArray);
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadVideoSceneChange;
var SceneChangeFileName : WideString;
    SCArray :TIntegerDynArray;
begin
  // Check for a '.scenechange' file
  SceneChangeFileName := WideChangeFileExt(CurrentProject.VideoSource,
    '.scenechange');
  
  if WideFileExists(SceneChangeFileName) then
  begin
    LoadSceneChange(SceneChangeFileName, SCArray);
  end
  else
  begin
    // Extract scene change
    ShowStatusBarMessage('Extracting keyframes...');
    ExtractSceneChange(CurrentProject.VideoSource, SCArray);
    SaveSceneChange(SceneChangeFileName, SCArray);
  end;

  WAVDisplayer.SetSceneChangeList(SCArray);
  WAVDisplayer.UpdateView([uvfRange]);
  g_SceneChangeWrapper.SetSceneChangeList(SCArray);
end;

//------------------------------------------------------------------------------
procedure TMainForm.LoadVideoInfo(VideoFilename : WideString);
var
  MediaInfoHandle : Cardinal;
begin
  if WideFileExists(VideoFilename) then
  begin
    if (MediaInfoDLL_Load('MediaInfo.dll')=True) then
    begin
      MediaInfoHandle := MediaInfo_New();
      MediaInfo_Open(MediaInfoHandle, @VideoFilename[1]);
      CurrentProject.VideoStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'StreamCount', Info_Text, Info_Name);
      CurrentProject.VideoWidth := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Width', Info_Text, Info_Name);
      CurrentProject.VideoHeight := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Height', Info_Text, Info_Name);
      CurrentProject.VideoFrameRate := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'FrameRate/String', Info_Text, Info_Name);
      CurrentProject.VideoFormat := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Codec/String', Info_Text, Info_Name);
      CurrentProject.VideoCodecProfile := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Codec_Profile', Info_Text, Info_Name);
      CurrentProject.AudioStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'StreamCount', Info_Text, Info_Name);
      CurrentProject.AudioFormat := MediaInfo_Get(MediaInfoHandle, Stream_Audio, CurrentProject.SelectedAudioTrackOnExtract - 1, 'Format', Info_Text, Info_Name);
      CurrentProject.AudioBitRate := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'BitRate/String', Info_Text, Info_Name);
      CurrentProject.AudioSamplingRate := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'SamplingRate/String', Info_Text, Info_Name);
      CurrentProject.MatroskaContainer := uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name)) = 'MATROSKA';
      CurrentProject.AviContainer := uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name)) = 'AVI';
      CurrentProject.Mp4Container := (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name)) = 'MPEG-4') OR
         (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name)) = 'QUICKTIME');
      CurrentProject.FlvContainer := uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name)) = 'FLASH VIDEO';
      CurrentProject.MpegContainer := (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name)) = 'MPEG-PS') OR
         (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name)) = 'MPEG-TS');
      MediaInfo_Close(MediaInfoHandle);
    end;
  end
  else
  begin
    CurrentProject.VideoStreamCount := '';
    CurrentProject.VideoWidth := '';
    CurrentProject.VideoHeight := '';
    CurrentProject.VideoFrameRate := '';
    CurrentProject.VideoFormat := '';
    CurrentProject.VideoCodecProfile := '';
    CurrentProject.AudioStreamCount := '';
    CurrentProject.AudioFormat := '';
    CurrentProject.AudioBitRate := '';
    CurrentProject.AudioSamplingRate := '';
    CurrentProject.MatroskaContainer := False;
    CurrentProject.AviContainer := False;
    CurrentProject.Mp4Container := False;
    CurrentProject.FlvContainer := False;
    CurrentProject.MpegContainer := False;
  end;
end;


procedure TMainForm.LoadProject(Filename : WideString);
var
  CM : ICursorManager;
  LoadWAVOK : Boolean;
  Idx : Integer;
begin
  if (not WideFileExists(Filename)) then
  begin
    MessageBoxW(Handle, PWideChar(WideFormat('The project file %s doesn''t exist.', [Filename])),
      PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    Exit;
  end;

  if not CheckSubtitlesAreSaved then
  begin
    Exit;
  end;
  CloseProject;

  g_WebRWSynchro.BeginWrite;
  CM := TCursorManager.Create(crHourGlass);
  try
    CurrentProject.LoadFromINIFile(Filename);

    // ----- analize file
    LoadVideoInfo(CurrentProject.VideoSource);

    // ----- Load waveform
    ShowStatusBarMessage('Loading waveform...');
    LoadWAVOK := False;
    if CurrentProject.WAVMode = pwmPeakOnly then
    begin
      if WideFileExists(CurrentProject.PeakFile) then
        LoadWAVOK := WAVDisplayer.LoadWAV(WideChangeFileExt(CurrentProject.PeakFile,'.wav'))
      else
      begin
        if WideFileExists(CurrentProject.WAVFile) then
        begin
          CurrentProject.WAVMode := pwmExternal;
          LoadWAVOK := WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
          CurrentProject.IsDirty := True;
          // Show warning
          MessageBoxW(Handle, PWideChar(WideFormat(
          'Peak file %s hasn''t been found, project has been switched to "WAV file" mode', [CurrentProject.PeakFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end
        else
        begin
          MessageBoxW(Handle, PWideChar(WideFormat('Can''t open peak file : %s',
            [CurrentProject.PeakFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end;
      end;
    end
    else if CurrentProject.WAVMode = pwmExternal then
    begin
      if WideFileExists(CurrentProject.WAVFile) then
      begin
        LoadWAVOK := WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
      end
      else
      begin
        if WideFileExists(CurrentProject.PeakFile) then
        begin
          CurrentProject.WAVMode := pwmPeakOnly;
          LoadWAVOK := WAVDisplayer.LoadWAV(WideChangeFileExt(CurrentProject.PeakFile,'.wav'));
          CurrentProject.IsDirty := True;
          // Show warning
          MessageBoxW(Handle, PWideChar(WideFormat(
          'WAV file %s hasn''t been found, project has been switched to "Peak file only" mode', [CurrentProject.WAVFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end
        else if WideFileExists(WideChangeFileExt(CurrentProject.WAVFile,'.peak')) then
        begin
          CurrentProject.WAVMode := pwmPeakOnly;
          CurrentProject.PeakFile := WideChangeFileExt(CurrentProject.WAVFile,'.peak');
          LoadWAVOK := WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
          CurrentProject.IsDirty := True;
        end
        else
        begin
          MessageBoxW(Handle, PWideChar(WideFormat('Can''t open WAV file : %s',
            [CurrentProject.WAVFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end;
      end;
    end;
    // TODO : report invalid wav file error

    WAVDisplayer.Enabled := True;
    WAVDisplayer.VerticalScaling := CurrentProject.WAVDisplayerVerticalScaling;

    // ----- Load subtitles
    ShowStatusBarMessage('Loading subtitles...');
    LoadSubtitles(CurrentProject.SubtitlesFile,CurrentProject.IsUTF8);

    // ----- Load VO
    ShowStatusBarMessage('Loading reference VO/Other...');
    LoadVO(CurrentProject.SubtitlesVO);

    // ----- Load audio
    ShowStatusBarMessage('Loading audio file...');
    if not AudioOnlyRenderer.Open(CurrentProject.WAVFile, True) then
    begin
      if AudioOnlyRenderer.Open(CurrentProject.VideoSource, True) then
      begin
        AudioOnlyRenderer.KillVideo;
      end;
    end;
    if AudioOnlyRenderer.IsOpen then
      g_AudioGraphDebugInfo := AudioOnlyRenderer.GetFiltersAsString
    else
      g_AudioGraphDebugInfo := '';

    // ----- Load video
    ShowStatusBarMessage('Loading video file...');
    if VideoRenderer.Open(CurrentProject.VideoSource, False) then
    begin
      g_VideoGraphDebugInfo := VideoRenderer.GetFiltersAsString;
      if (CurrentProject.DetachedVideo <> MenuItemDetachVideoWindow.Checked) then
        ActionDetachVideoExecute(nil);
      if (CurrentProject.ShowVideo <> ShowingVideo) then
        ActionShowHideVideoExecute(nil);
      UpdateVideoRendererWindow;
      VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
      LoadVideoSceneChange;
      DetachedVideoForm.SetNormalPos(CurrentProject.VideoWindowNormalLeft,
        CurrentProject.VideoWindowNormalTop, CurrentProject.VideoWindowNormalWidth,
        CurrentProject.VideoWindowNormalHeight);
      g_VSSCoreWrapper.SetVideoWidth(VideoRenderer.VideoWidth);
      g_VSSCoreWrapper.SetVideoHeight(VideoRenderer.VideoHeight);
      if not ShowingSubs then
        ActionShowHideSubsExecute(nil);
    end
    else
    begin
      g_VSSCoreWrapper.SetVideoWidth(0);
      g_VSSCoreWrapper.SetVideoHeight(0);
    end;

    if ShowingVideo then
      begin
       WAVDisplayer.SetRenderer(VideoRenderer);
       MenuItemSetVideoResolution.Enabled := True;
      end
    else
      WAVDisplayer.SetRenderer(AudioOnlyRenderer);

    if ShowingVideo AND (CurrentProject.VideoStreamCount < '1') then
     ActionShowHideVideoExecute(self);

    if (not LoadWAVOK) then
    begin
      if VideoRenderer.IsOpen then
      begin
        // Extract duration form video
        WAVDisplayer.Length := VideoRenderer.GetDuration;
      end
      else
      begin
        if (WAVDisplayer.RangeList.Count > 0) then
        begin
          // Use last subtitle stop time + 1 minute
          WAVDisplayer.Length := WAVDisplayer.RangeList[
            WAVDisplayer.RangeList.Count - 1].StopTime + (1 * 60 * 1000);
        end
        else
        begin
          WAVDisplayer.Length := 5 * 60 * 1000; // 5 minutes by default it will be automatically adapted later
        end;
      end;
    end;

    // ----- Load script
    if WideFileExists(WideChangeFileExt(CurrentProject.Filename,'.vssscript')) then
    begin
      ShowStatusBarMessage('Loading script file...');
      MemoTextPipe.Lines.LoadFromFile(WideChangeFileExt(CurrentProject.Filename,'.vssscript'));
      // First go to bottom, so the line we want will be at the top
      MemoTextPipe.SelStart := Length(MemoTextPipe.Text);
      Perform(EM_SCROLLCARET, 0, 0);
      MemoTextPipe.SelStart := CurrentProject.TextPipePosition;
      Perform(EM_SCROLLCARET, 0, 0);
    end;

    // ----- Restore position ---
    if (CurrentProject.WAVDisplayerPositionStartMs <> -1) and
       (CurrentProject.WAVDisplayerPositionStopMs <> -1) then
    begin
      WAVDisplayer.ZoomRange(CurrentProject.WAVDisplayerPositionStartMs,
        CurrentProject.WAVDisplayerPositionStopMs);
    end
    else
    begin
      // Zoom in at beginning
      WAVDisplayer.ZoomRange(0,15000);
    end;

    if (CurrentProject.FocusedTimeMs <> -1) then
     begin
      Idx := WAVDisplayer.RangeList.FindInsertPos(CurrentProject.FocusedTimeMs, -1);
      SelectNodeAtIndex(Idx - 1);
     end
    else
     begin
      SelectNodeAtIndex(0);
     end;


    // ----- Load dictionary ---
    Idx := FSpellChecker.GetDictIdx(CurrentProject.Dictionary);
    if (Idx <> -1) then
    begin
      LoadDict(Idx);
    end;

    // ------ Load presets -----
    LoadPresetFile(g_PresetsPath + CurrentProject.Presets);

    UpdateVolume;
    EnableControl(True);

    CurrentProjectOnDirtyChange(nil);
  finally
    g_GlobalContext.WavAverageBytePerSecond := WAVDisplayer.GetWAVAverageBytePerSecond;
    g_WebRWSynchro.EndWrite;
    // Hide the video
    if (not VideoRenderer.IsOpen) and ShowingVideo then
        ActionShowHideVideo.Execute;
  end;

  TimerAutoScrollSub.Enabled := chkAutoScrollSub.Checked;
  WAVDisplayer.AutoScrolling := chkAutoScrollWAVDisp.Checked;

  WAVDisplayer.UpdateView([uvfPageSize]);
  MRUList.AddFile(CurrentProject.Filename);
  ShowStatusBarMessage('Project loaded.');

  PageControlMain.ActivePage := TabSheetSub;


end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveProject(Project : TVSSProject; SilentSave : Boolean);
var
  ProjectFileIni : TTntIniFile;
  NodeData : PTreeData;
  FocusedTimeMs : Integer;
begin
  if WideFileIsReadOnly(Project.Filename) then
  begin
    if not SilentSave then
    begin
      MessageBoxW(Handle, PWideChar(WideFormat('The project file %s can''t be saved because it''s in read only mode.', [Project.Filename])),
        PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    end;
    Exit;
  end;

  ProjectFileIni := TTntIniFile.Create(Project.Filename);
  ProjectFileIni.WriteString('VisualSubsync','VideoSource',
    WideMakeRelativePath(Project.Filename, Project.VideoSource));
  ProjectFileIni.WriteString('VisualSubsync','WAVFile',
    WideMakeRelativePath(Project.Filename, Project.WAVFile));
  ProjectFileIni.WriteString('VisualSubsync','PeakFile',
    WideMakeRelativePath(Project.Filename, Project.PeakFile));
  ProjectFileIni.WriteInteger('VisualSubsync','WAVMode', Ord(Project.WAVMode));
  ProjectFileIni.WriteString('VisualSubsync','SubtitlesFile',
    WideMakeRelativePath(Project.Filename, Project.SubtitlesFile));
  ProjectFileIni.WriteString('VisualSubsync','SubtitlesVO',
    WideMakeRelativePath(Project.Filename, Project.SubtitlesVO));

  if Length(Trim(MemoTextPipe.Text)) > 0 then
  begin
    MemoTextPipe.PlainText := False;
    MemoTextPipe.Lines.SaveToFile(WideChangeFileExt(Project.Filename,'.vssscript'));
    // Save cursor position
    ProjectFileIni.WriteInteger('VisualSubsync', 'TextPipePosition',
      MemoTextPipe.SelStart);
    ProjectFileIni.WriteString('VisualSubsync', 'TextPipeSource',
      WideMakeRelativePath(Project.Filename, Project.TextPipeSource));
  end;

  ProjectFileIni.WriteInteger('VisualSubsync','VideoPanelWidth', Project.VideoPanelWidth);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoPanelHeight', Project.VideoPanelHeight);
  ProjectFileIni.WriteBool('VisualSubsync','ShowVideo', Project.ShowVideo);
  ProjectFileIni.WriteBool('VisualSubsync','ShowVO', Project.ShowVO);
  ProjectFileIni.WriteBool('VisualSubsync','DetachedVideo', Project.DetachedVideo);

  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalLeft', DetachedVideoForm.NormalLeft);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalTop', DetachedVideoForm.NormalTop);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalWidth', DetachedVideoForm.NormalWidth);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalHeight', DetachedVideoForm.NormalHeight);

  ProjectFileIni.WriteInteger('VisualSubsync', 'WAVDisplayerPositionStartMs', WAVDisplayer.Position);
  ProjectFileIni.WriteInteger('VisualSubsync', 'WAVDisplayerPositionStopMs', WAVDisplayer.Position + WAVDisplayer.PageSize);
  ProjectFileIni.WriteInteger('VisualSubsync', 'WAVDisplayerVerticalScaling', WAVDisplayer.VerticalScaling);

  FocusedTimeMs := -1;
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    FocusedTimeMs := NodeData.Range.StartTime + ((NodeData.Range.StopTime - NodeData.Range.StartTime) div 2);
  end;
  ProjectFileIni.WriteInteger('VisualSubsync', 'FocusedTimeMs', FocusedTimeMs);

  ProjectFileIni.WriteString('VisualSubsync', 'Dictionary', FSpellChecker.GetCurrentDictName);

  ProjectFileIni.WriteString('VisualSubsync','Presets', Project.Presets);

  ProjectFileIni.Free;

end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomInExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomIn;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomOutExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomOut;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomSelectionExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomRange(WAVDisplayer.Selection);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomAllExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomAll;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlayExecute(Sender: TObject);
begin
  if IsTimingMode then
    PanelWAVDisplay.SetFocus;
  WAVDisplayer.AutoScrolling := True;
  UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
  if WAVDisplayer.SelectionIsEmpty then
  begin
    PlayingMode := pmtAll;
    if (LastCursorPositionOnPause>0) AND ConfigObject.ForceStopOnPausing then
     WAVDisplayer.PlayRange(LastCursorPositionOnPause,WAVDisplayer.Length)
    else
     WAVDisplayer.PlayRange(WAVDisplayer.GetCursorPos,WAVDisplayer.Length)
  end
  else
  begin
    PlayingMode := pmtSelection;
    if (LastCursorPositionOnPause>0) AND ConfigObject.ForceStopOnPausing then
      WAVDisplayer.PlayRange(LastCursorPositionOnPause,WAVDisplayer.Length)
    else
      WAVDisplayer.PlayRange(WAVDisplayer.Selection);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStopExecute(Sender: TObject);
begin
  WAVDisplayer.Stop;
  if ConfigObject.ForceStopOnPausing then
   LastCursorPositionOnPause := 0;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoopExecute(Sender: TObject);
begin
  if IsTimingMode then
    PanelWAVDisplay.SetFocus;
  WAVDisplayer.AutoScrolling := True;
  UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
  if WAVDisplayer.SelectionIsEmpty then
  begin
    PlayingMode := pmtAll;
    WAVDisplayer.PlayRange(WAVDisplayer.GetCursorPos,WAVDisplayer.Length,True)
  end
  else
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection,True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionExitExecute(Sender: TObject);
begin
  if CheckSubtitlesAreSaved then
  begin
    SaveSettings;
    Application.Terminate;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNewProjectExecute(Sender: TObject);
var
  ProjectFileIni : TTntIniFile;
begin
  // First close current project
  if not CheckSubtitlesAreSaved then
  begin
    Exit;
  end;
  CloseProject;

  // New project
  ProjectForm.ConfigureInNewProjectMode;
  ProjectForm.chkSaveAsUTF8.Checked := Self.GetUTF8Default;
  if (ProjectForm.ShowModal = mrOK) then
  begin
    // Create the project file
    ProjectFileIni := TTntIniFile.Create(ProjectForm.EditProjectFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','VideoSource',ProjectForm.EditVideoFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','WAVFile',ProjectForm.GetWAVFilename);
    ProjectFileIni.WriteString('VisualSubsync','PeakFile',ProjectForm.GetPeakFilename);
    ProjectFileIni.WriteInteger('VisualSubsync','WAVMode',Ord(ProjectForm.GetWAVMode));
    ProjectFileIni.WriteString('VisualSubsync','SubtitlesFile',ProjectForm.EditSubtitleFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','SubtitlesVO',ProjectForm.EditSubtitleVO.Text);
    ProjectFileIni.WriteBool('VisualSubsync','DetachedVideo',ConfigObject.EnableDetachedVideoForNewProject);
    ProjectFileIni.WriteBool('VisualSubsync','ShowVideo',ConfigObject.EnableShowVideoForNewProject);
    ProjectFileIni.WriteBool('VisualSubsync','ShowVO',WideFileExists(ProjectForm.EditSubtitleVO.Text));
    ProjectFileIni.WriteString('VisualSubsync','Dictionary',StartupDictionary);
    ProjectFileIni.WriteInteger('VisualSubsync','SelectedAudioTrackOnExtract',ProjectForm.SelectedAudioTrackOnExtract);

    ProjectFileIni.Free;

    // Load the project
    LoadProject(ProjectForm.EditProjectFilename.Text);
    CurrentProject.IsUTF8 := ProjectForm.chkSaveAsUTF8.Checked;
    // TODO : Maybe create an empty file to keep UTF8 info if we quit

    if ProjectForm.chkLaunchTraslateActionOnCreate.checked then
      ActionTranslationTemplateExecute(self);

  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideVideoExecute(Sender: TObject);
begin
  if (not VideoRenderer.IsOpen) and (not ShowingVideo) then
  begin
    ShowStatusBarMessage('Video file is not opened...');
    Exit;
  end;

  ShowingVideo := not ShowingVideo;

  if MenuItemDetachVideoWindow.Checked then
  begin
    if ShowingVideo then
      ShowWindow(DetachedVideoForm.Handle, SW_SHOWNOACTIVATE); // Don't give focus
    DetachedVideoForm.Visible := ShowingVideo;
  end
  else
  begin
    PanelVideo.Visible := ShowingVideo;
    SplitterWAVDisplay_Video.Visible := ShowingVideo;
  end;

  if ShowingVideo then
  begin
    UpdateSubtitleForPreview(False);
    WAVDisplayer.SetRenderer(VideoRenderer);
    MenuItemSetVideoResolution.Enabled := True;
  end
  else
  begin
    WAVDisplayer.SetRenderer(AudioOnlyRenderer);
    MenuItemSetVideoResolution.Enabled := False;
  end;
  WAVDisplayer.UpdateView([uvfPageSize]);

  // default : Resolution to 33% than original
  if PanelVideo.Width = 0 then
   begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 3;
     PanelVideo.Height := VideoRenderer.VideoHeight div 3;
     PanelTop.Height := VideoRenderer.VideoHeight div 3;
   end;

  if Assigned(CurrentProject) and (ShowingVideo <> CurrentProject.ShowVideo) then
  begin
    CurrentProject.ShowVideo := ShowingVideo;
    SaveProject(CurrentProject, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionOpenProjectExecute(Sender: TObject);
begin
  TntOpenDialog1.Filter := 'VSS project (*.vssprj)|*.VSSPRJ' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialog1);
  if TntOpenDialog1.Execute then
  begin
    Self.Repaint;
    LoadProject(TntOpenDialog1.FileName);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  // Save subtitles
  if Trim(CurrentProject.SubtitlesFile) <> '' then
  begin
    if (not CurrentProject.IsUTF8) and DetectCharsetDataLoss then
    begin
      MessageBoxW(Handle, PWideChar(WideString(
        'Subtitles contain some characters that may not be saved correctly,' +
        ' the file will be saved in Unicode UTF-8 starting from now.')),
        PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
      CurrentProject.IsUTF8 := True;
    end;
    SaveSubtitles(CurrentProject.SubtitlesFile, '', CurrentProject.IsUTF8, False, nil, False);
    if VideoRenderer.IsOpen then
      VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
  end;
  SaveProject(CurrentProject, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
var InputExt : WideString;
    NewExt : WideString;
    Translator : TTranslator;
begin
  TntSaveDialog1.Filter :=
    'SRT files (*.srt)|*.SRT' + '|' +
    'SRT files stripped of text (*.srt)|*.SRT' + '|' +
    'SSA files (*.ssa)|*.SSA' + '|' +
    'ASS files (*.ass)|*.ASS' + '|' +
    'CUE files (*.cue)|*.CUE' + '|' +
    'TXT files (*.txt)|*.TXT' + '|' +
    'CSV files (*.csv)|*.CSV' + '|' +
    'All files (*.*)|*.*';
  TntSaveDialog1.FileName := ChangeFileExt(CurrentProject.SubtitlesFile,'');
  // Preselect format
  InputExt := WideLowerCase(WideExtractFileExt(CurrentProject.SubtitlesFile));
  if (InputExt = '.srt') then
    TntSaveDialog1.FilterIndex := 1
  else if (InputExt = '.ssa') then
    TntSaveDialog1.FilterIndex := 3
  else if (InputExt = '.ass') then
    TntSaveDialog1.FilterIndex := 4
  else
    TntSaveDialog1.FilterIndex := 7;

  if TntSaveDialog1.Execute then
  begin
    // Force extension filter
    NewExt := '';
    case TntSaveDialog1.FilterIndex of
      1 : NewExt := '.srt';
      2 : NewExt := '.srt';
      3 : NewExt := '.ssa';
      4 : NewExt := '.ass';
      5 : NewExt := '.cue';
      6 : NewExt := '.txt';
      7 : NewExt := '.csv';
    end;
    if (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.srt') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.ssa') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.ass') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.cue') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.txt') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.csv') Then
       begin
        TntSaveDialog1.FileName := TntSaveDialog1.FileName + NewExt;
       end;
    if NewExt <> '' then
      TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, NewExt);
    Translator := nil;
    if (TntSaveDialog1.FilterIndex = 2) then
      Translator := TTranslatorEmpty.Create;
    SaveSubtitles(TntSaveDialog1.FileName, CurrentProject.SubtitlesFile,
      CurrentProject.IsUTF8, False, Translator, False);
    if (Assigned(Translator)) then
      FreeAndNil(Translator);
    if MessageDlg('Do you want assign new subtitle to current project and reload it?', mtConfirmation,[mbYes,mbNo],0) = mrYes then
      begin
        CurrentProject.SubtitlesFile := TntSaveDialog1.FileName;
        LoadSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
        ActionSaveExecute(Sender);
        ActionReloadExecute(Sender);
      end;

  end;
end;

//------------------------------------------------------------------------------

function HasExtensionChanged(OldFilename, NewFilename : WideString) : Boolean;
var OldExt, NewExt : WideString;
    OldWithDummyExt, NewWithDummyExt : WideString;
begin
  Result := False;
  OldExt := WideLowerCase(WideExtractFileExt(OldFilename));
  NewExt := WideLowerCase(WideExtractFileExt(NewFilename));
  if (OldExt = NewExt) then
    Exit;

  OldWithDummyExt := WideChangeFileExt(OldFilename, '.dummy');
  NewWithDummyExt := WideChangeFileExt(NewFilename, '.dummy');
  Result := (OldWithDummyExt = NewWithDummyExt);
end;

procedure TMainForm.ActionProjectPropertiesExecute(Sender: TObject);
var ProjectHasChanged, VideoHasChanged, SubtitleFileHasChanged : Boolean;
    PreviousSubtitlesFile : WideString;
begin
  // TODO : test this more

  ProjectForm.ConfigureInModifyProjectMode(CurrentProject);
  if (ProjectForm.ShowModal = mrOK) then
  begin
    g_WebRWSynchro.BeginWrite;

    try
      // Check the thing that have changed and update
      VideoHasChanged := False;
      ProjectHasChanged := False;
      SubtitleFileHasChanged := False;


      if (CurrentProject.SubtitlesFile <> ProjectForm.EditSubtitleFilename.Text) then
      begin
        CheckSubtitlesAreSaved;

        // Check if it's a file format change
        if HasExtensionChanged(CurrentProject.SubtitlesFile, ProjectForm.EditSubtitleFilename.Text) then
        begin
          PreviousSubtitlesFile := CurrentProject.SubtitlesFile;
          CurrentProject.SubtitlesFile := ProjectForm.EditSubtitleFilename.Text;

          if not WideFileExists(CurrentProject.SubtitlesFile) then
          begin
            if (not CurrentProject.IsUTF8) and DetectCharsetDataLoss then
            begin
              MessageBoxW(Handle, PWideChar(WideString(
                'Subtitles contain some characters that may not be saved correctly,' +
                ' the file will be saved in Unicode UTF-8 starting from now.')),
                PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
              CurrentProject.IsUTF8 := True;
            end;
            SaveSubtitles(CurrentProject.SubtitlesFile, PreviousSubtitlesFile,
              CurrentProject.IsUTF8, False, nil, False);
          end;
        end;

        CurrentProject.SubtitlesFile := ProjectForm.EditSubtitleFilename.Text;
        ErrorReportForm.Clear;
        LoadSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
        
        ProjectHasChanged := True;
        SubtitleFileHasChanged := True;
      end;

      if (CurrentProject.SubtitlesVO <> ProjectForm.EditSubtitleVO.Text) then
      begin
        CurrentProject.SubtitlesVO := ProjectForm.EditSubtitleVO.Text;
        LoadVO(CurrentProject.SubtitlesVO);
        ProjectHasChanged := True;
      end;

      if (CurrentProject.VideoSource <> ProjectForm.EditVideoFilename.Text) then
      begin
        CurrentProject.VideoSource := ProjectForm.EditVideoFilename.Text;
        LoadVideoInfo(CurrentProject.VideoSource);
        ProjectHasChanged := True;
        VideoHasChanged := True;
        if VideoRenderer.Open(CurrentProject.VideoSource, False) then
        begin
          g_VideoGraphDebugInfo := VideoRenderer.GetFiltersAsString;
          CurrentProject.VideoPanelWidth := 0;
          CurrentProject.VideoPanelHeight := 0;
          UpdateVideoRendererWindow;
          LoadVideoSceneChange;
          g_VSSCoreWrapper.SetVideoWidth(VideoRenderer.VideoWidth);
          g_VSSCoreWrapper.SetVideoHeight(VideoRenderer.VideoHeight);
        end
        else
        begin
          VideoRenderer.Close;
          g_VideoGraphDebugInfo := '';
          ClearVideoSceneChange;
          g_VSSCoreWrapper.SetVideoWidth(0);
          g_VSSCoreWrapper.SetVideoHeight(0);
        end;
        if not ShowingSubs then
          ActionShowHideSubsExecute(nil);
      end;

      if (CurrentProject.WAVMode <> ProjectForm.GetWAVMode) then
      begin
        CurrentProject.WAVMode := ProjectForm.GetWAVMode;
        if (CurrentProject.WAVMode = pwmExternal) then
        begin
          CurrentProject.WAVFile := ProjectForm.GetWAVFilename;
          WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
          AudioOnlyRenderer.Open(CurrentProject.WAVFile, True);
        end
        else if (CurrentProject.WAVMode = pwmPeakOnly) then
        begin
          CurrentProject.PeakFile := ProjectForm.GetPeakFilename;
          WAVDisplayer.LoadWAV(ChangeFileExt(CurrentProject.PeakFile, '.wav'));
          if AudioOnlyRenderer.Open(CurrentProject.VideoSource, True) then
            AudioOnlyRenderer.KillVideo;
        end
        else
        begin
          WAVDisplayer.ClearPeakData;
        end;
        if AudioOnlyRenderer.IsOpen then
          g_AudioGraphDebugInfo := AudioOnlyRenderer.GetFiltersAsString;
        ProjectHasChanged := True;
      end;

      if (CurrentProject.WAVFile <> ProjectForm.GetWAVFilename) then
      begin
        CurrentProject.WAVFile := ProjectForm.GetWAVFilename;
        WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
        if not AudioOnlyRenderer.Open(CurrentProject.WAVFile, True) then
        begin
          // Failed to load audio try to use video
          if AudioOnlyRenderer.Open(CurrentProject.VideoSource, True) then
            AudioOnlyRenderer.KillVideo
          else
            AudioOnlyRenderer.Close;
        end;
        ProjectHasChanged := True;
      end;

      if (CurrentProject.PeakFile <> ProjectForm.GetPeakFilename) then
      begin
        CurrentProject.PeakFile := ProjectForm.GetPeakFilename;
        WAVDisplayer.LoadWAV(ChangeFileExt(CurrentProject.PeakFile, '.wav'));
        ProjectHasChanged := True;
      end;

      if (CurrentProject.Filename <> ProjectForm.EditProjectFilename.Text) then
      begin
        CurrentProject.Filename := ProjectForm.EditProjectFilename.Text;
        ProjectHasChanged := True;
      end;

      if (CurrentProject.IsUTF8 <> ProjectForm.chkSaveAsUTF8.Checked) then
      begin
        CurrentProject.IsUTF8 := ProjectForm.chkSaveAsUTF8.Checked;
        ProjectHasChanged := True;
      end;

      if (ProjectHasChanged) then
      begin
        if AudioOnlyRenderer.IsOpen then
          g_AudioGraphDebugInfo := AudioOnlyRenderer.GetFiltersAsString
        else
          g_AudioGraphDebugInfo := '';

        SaveProject(CurrentProject, False);
      end;

      if (VideoHasChanged or SubtitleFileHasChanged) and (VideoRenderer.IsOpen) then
      begin
        VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
      end;

      // Hide the video
      if (not VideoRenderer.IsOpen) and ShowingVideo then
      begin
          ActionShowHideVideo.Execute;
      end;
      g_GlobalContext.WavAverageBytePerSecond := WAVDisplayer.GetWAVAverageBytePerSecond;
    finally
      g_WebRWSynchro.EndWrite;
    end;

    // Update controls
    EnableControl(True);

    WAVDisplayer.UpdateView([uvfPageSize]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ResetFind;
begin
  SearchNodeIndex := -1;
  SearchPos := 1;
  SearchItemPos := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowMatchingSubOnly(DisplayAll : Boolean);
var Node : PVirtualNode;
    NodeData : PTreeData;
    Match : Boolean;
    FindText : WideString;
    RegExpr : TRegExpr;
    MatchingCount : Integer;
begin
  FindText := FindForm.GetFindText;
  MatchingCount := 0;

  RegExpr := TRegExpr.Create;
  RegExpr.Expression := FindText;
  RegExpr.ModifierI := not FindForm.MatchCase;

  Node := vtvSubsList.GetFirst;
  while (Node <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Match := True;
    if (Length(FindText) > 0) and (not DisplayAll) then
    begin
      if FindForm.UseRegExp then
      begin
        Match := RegExpr.Exec(NodeData.Range.Text);
      end
      else
      begin
        Match := WideStringFind(1, NodeData.Range.Text, FindText,
          not FindForm.MatchCase, FindForm.WholeWord) > 0;
      end;
    end;
    vtvSubsList.IsVisible[Node] := Match;
    if Match then Inc(MatchingCount);
    Node := vtvSubsList.GetNext(Node);
  end;
  RegExpr.Free;
  vtvSubsList.Repaint;
  if (not DisplayAll) then
    ShowStatusBarMessage(IntToStr(MatchingCount) + ' subtitle(s) match.');
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFindExecute(Sender: TObject);
begin
  if not FindForm.Showing then
  begin
    // Setup mode: subs/memo
    if MemoTextPipe.Focused then
      SearchItem := @MemoTextPipe
    else
      SearchItem := nil;
    ResetFind;
    if MemoSubtitleText.Focused then FindForm.SetFindText(MemoSubtitleText.SelText);
    if MemoSubtitleVO.Focused then FindForm.SetFindText(MemoSubtitleVO.SelText);
    FindForm.Show;
    FindForm.SetMemoMode(SearchItem <> nil);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.SearchMemo(FindText : WideString) : Boolean;
var RegExp : TRegExpr;
    MatchLen : Integer;
begin
  Result := False;

  RegExp := TRegExpr.Create;
  RegExp.ModifierI := not FindForm.MatchCase;
  RegExp.Expression := FindText;

  if (SearchItemPos <= 0) and (not FindForm.FromCursor) then
    SearchItemPos := 1
  else
    SearchItemPos := SearchItem.SelStart + 1 + SearchItem.SelLength;

  if FindForm.UseRegExp then
  begin
    RegExp.InputString := SearchItem.Text;
    if RegExp.ExecPos(SearchItemPos) then
    begin
      SearchItemPos := RegExp.MatchPos[0];
      MatchLen := RegExp.MatchLen[0];
    end
    else
    begin
      SearchItemPos := -1;
      MatchLen := 0;
    end;
  end
  else
  begin
    SearchItemPos := WideStringFind(SearchItemPos, SearchItem.Text, FindText,
      not FindForm.MatchCase, FindForm.WholeWord);
    MatchLen := Length(FindText);
  end;

  if (SearchItemPos > 0) then
  begin
    SearchItem.SelStart := SearchItemPos - 1;
    SearchItem.SelLength := MatchLen;
    RegExp.Free;
    Result := True;
    Exit;
  end;

  RegExp.Free;
end;

//------------------------------------------------------------------------------

function TMainForm.SearchSubtitles(FindText : WideString) : Boolean;
var SearchNode : PVirtualNode;
    NodeData : PTreeData;
    RegExp : TRegExpr;
    MatchLen : Integer;
    VoText : String;
begin
  Result := False;

  RegExp := TRegExpr.Create;
  RegExp.ModifierI := not FindForm.MatchCase;
  RegExp.Expression := FindText;

  SearchNode := nil;
  if (SearchNodeIndex >= 0) and (SearchNodeIndex < WAVDisplayer.RangeList.Count) then
  begin
    SearchNode := TSubtitleRange(WAVDisplayer.RangeList[SearchNodeIndex]).Node;
  end;

  if FindForm.FromCursor then
  begin
    if SearchNode <> vtvSubsList.FocusedNode then
      SearchPos := 1;
    SearchNode := vtvSubsList.FocusedNode;
  end;
  if (SearchNode = nil) then
  begin
    SearchNode := vtvSubsList.GetFirst;
  end;

  while (SearchNode <> nil) do
  begin
    SearchNodeIndex := SearchNode.Index;
    NodeData := vtvSubsList.GetNodeData(SearchNode);
    MatchLen := Length(FindText);
    if FindForm.UseRegExp then
    begin
      RegExp.InputString := NodeData.Range.Text;
      if RegExp.ExecPos(SearchPos) then
      begin
        SearchPos := RegExp.MatchPos[0];
        MatchLen := RegExp.MatchLen[0];
      end else
        SearchPos := 0;
    end
    else
    begin
      SearchPos := WideStringFind(SearchPos, NodeData.Range.Text, FindText,
        not FindForm.MatchCase, FindForm.WholeWord);
      if (CurrentProject.SubtitlesVO<>'') And Not FindForm.chkDisableVOSearch.Checked then
       begin
        VoText := GetVOText(NodeData,False);
        SearchPosVo := WideStringFind(SearchPosVo, VoText, FindText, not FindForm.MatchCase, FindForm.WholeWord);
       end;
    end;

    if (SearchPos > 0) Or (SearchPosVo > 0) then
    begin
      vtvSubsList.ScrollIntoView(SearchNode,True);
      vtvSubsList.FocusedNode := SearchNode;
      vtvSubsList.ClearSelection;
      vtvSubsList.Selected[SearchNode] := True;
      if SearchPos > 0 Then
       begin
        MemoSubtitleText.SelStart := SearchPos - 1;
        MemoSubtitleText.SelLength := MatchLen;
       end;
      if SearchPosVo > 0 Then
       begin
        SearchPosVO := WideStringFind(SearchPos, MemoSubtitleVO.Text, FindText,
          not FindForm.MatchCase, FindForm.WholeWord);
        MemoSubtitleVO.SelStart := SearchPosVo - 1;
        MemoSubtitleVO.SelLength := MatchLen;
       end;
      if SearchPos > 0 Then
        Inc(SearchPos, MatchLen);
      if SearchPosVo > 0 Then
        Inc(SearchPosVo, MatchLen);
      RegExp.Free;
      Result := True;
      Exit;
    end;
    SearchNode := vtvSubsList.GetNext(SearchNode);
    SearchPos := 1;
  end;
  RegExp.Free;
  SearchNodeIndex := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFindNextExecute(Sender: TObject);
var FindText : WideString;
    IsFound : Boolean;
begin
  FindText := FindForm.GetFindText;
  if Length(FindText) <= 0 then
    Exit;

  if Assigned(SearchItem) then
  begin
    IsFound := SearchMemo(FindText);
  end
  else
  begin
    IsFound := SearchSubtitles(FindText);
  end;
  
  if (not IsFound) then
  begin
    ShowStatusBarMessage('No more items.');
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.ReplaceAllTextMemo(FindText : WideString) : Integer;
var RegExpr : TRegExpr;
    ReplaceFlags : TReplaceFlags;
    TotalReplaceCount : Integer;
begin
  TotalReplaceCount := 0;

  RegExpr := TRegExpr.Create;
  RegExpr.ModifierI := (not FindForm.MatchCase);
  RegExpr.ModifierG := True;
  RegExpr.Expression := FindText;
  
  if (SearchItemPos <= 0) and (not FindForm.FromCursor) then
    SearchItemPos := 1
  else
    SearchItemPos := SearchItem.SelStart + 1 + SearchItem.SelLength;

  g_WebRWSynchro.BeginWrite;
  try
    if FindForm.UseRegExp then
    begin
      SearchItem.Text :=
        Copy(SearchItem.Text, 1, SearchItemPos - 1)
        + RegExpr.Replace(
          Copy(SearchItem.Text,
            SearchItemPos,
            Length(SearchItem.Text) - SearchItemPos + 1),
          FindForm.GetReplaceText,
          True);
      TotalReplaceCount := TotalReplaceCount + RegExpr.ReplaceCount;
    end
    else
    begin
      ReplaceFlags := [rfReplaceAll];
      if (not FindForm.MatchCase) then
        Include(ReplaceFlags, rfIgnoreCase);
      TotalReplaceCount := TotalReplaceCount + WideStringCount(
        Copy(SearchItem.Text,
          SearchItemPos,
          Length(SearchItem.Text) - SearchItemPos + 1),
        FindForm.GetFindText,
        not FindForm.MatchCase,
        FindForm.WholeWord);
      SearchItem.Text :=
        Copy(SearchItem.Text, 1, SearchItemPos - 1)
        + Tnt_WideStringReplace(
          Copy(SearchItem.Text,
            SearchItemPos,
            Length(SearchItem.Text) - SearchItemPos + 1),
          FindForm.GetFindText,
          FindForm.GetReplaceText,
          ReplaceFlags,
          FindForm.WholeWord);
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  RegExpr.Free;

  if (TotalReplaceCount > 0) then
    CurrentProject.IsDirty := True;

  Result := TotalReplaceCount;
end;

//------------------------------------------------------------------------------

function TMainForm.ReplaceAllTextSubs(FindText : WideString) : Integer;
var Node : PVirtualNode;
    NodeData : PTreeData;
    RegExpr : TRegExpr;
    ReplaceFlags : TReplaceFlags;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    NewText : WideString;
    SubRange : TSubtitleRange;
    TotalReplaceCount : Integer;
begin
  TotalReplaceCount := 0;

  RegExpr := TRegExpr.Create;
  RegExpr.ModifierI := (not FindForm.MatchCase);
  RegExpr.ModifierG := True;
  RegExpr.Expression := FindText;

  if FindForm.FromCursor then
    Node := vtvSubsList.FocusedNode
  else
    Node := vtvSubsList.GetFirst;

  MultiChangeTask := TUndoableMultiChangeTask.Create;

  g_WebRWSynchro.BeginWrite;
  try
    while (Node <> nil) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      SubRange := NodeData.Range;
      if FindForm.UseRegExp then
      begin
        NewText := RegExpr.Replace(NodeData.Range.Text, FindForm.GetReplaceText, True);
        TotalReplaceCount := TotalReplaceCount + RegExpr.ReplaceCount;
      end
      else
      begin
        ReplaceFlags := [rfReplaceAll];
        if (not FindForm.MatchCase) then
          Include(ReplaceFlags, rfIgnoreCase);
        TotalReplaceCount := TotalReplaceCount + WideStringCount(
          NodeData.Range.Text, FindForm.GetFindText, not FindForm.MatchCase,
          FindForm.WholeWord);
        NewText := Tnt_WideStringReplace(NodeData.Range.Text, FindForm.GetFindText,
          FindForm.GetReplaceText, ReplaceFlags, FindForm.WholeWord);
      end;
      ChangeSubData := TChangeSubData.Create(Node.Index);
      ChangeSubData.OldText := SubRange.Text;
      ChangeSubData.NewText := NewText;
      MultiChangeTask.AddData(ChangeSubData);
      NodeData.Range.Text := NewText;
      Node := vtvSubsList.GetNext(Node);
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  RegExpr.Free;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
  Result := TotalReplaceCount;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ReplaceAllText;
var TotalReplaceCount : Integer;
    FindText : WideString;
begin
  FindText := FindForm.GetFindText;
  if Length(FindText) <= 0 then
    Exit;

  if Assigned(SearchItem) then
  begin
    TotalReplaceCount := ReplaceAllTextMemo(FindText);
  end
  else
  begin
    TotalReplaceCount := ReplaceAllTextSubs(FindText);
  end;

  ShowStatusBarMessage(IntToStr(TotalReplaceCount) + ' item(s) replaced.');
end;

//------------------------------------------------------------------------------

procedure TMainForm.ReplaceTextMemo;
var SelStart : Integer;
    NewText : WideString;
    RegExp : TRegExpr;
begin
  // Save selection start position
  SelStart := SearchItem.SelStart;

  if FindForm.UseRegExp then
  begin
    RegExp := TRegExpr.Create;
    RegExp.ModifierI := (not FindForm.MatchCase);
    RegExp.Expression := FindForm.GetFindText;
    NewText := RegExp.Replace(SearchItem.SelText, FindForm.GetReplaceText, True);
    RegExp.Free;
  end
  else
  begin
    NewText := FindForm.GetReplaceText;
  end;

  SearchItem.SelText := NewText;

  // Restaure selection
  SearchItem.SelStart := SelStart;
  SearchItem.SelLength := Length(NewText);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ReplaceTextSubs;
var SelStart : Integer;
    NewText : WideString;
    RegExp : TRegExpr;
begin
  // Save selection start position
  SelStart := MemoSubtitleText.SelStart;

  TTntRichEditCustomUndo(MemoSubtitleText).SaveSelectionInfo;

  if FindForm.UseRegExp then
  begin
    RegExp := TRegExpr.Create;
    RegExp.ModifierI := (not FindForm.MatchCase);
    RegExp.Expression := FindForm.GetFindText;
    NewText := RegExp.Replace(MemoSubtitleText.SelText, FindForm.GetReplaceText, True);
    RegExp.Free;
  end
  else
  begin
    NewText := FindForm.GetReplaceText;
  end;

  MemoSubtitleText.SelText := NewText;
  
  // Restaure selection
  MemoSubtitleText.SelStart := SelStart;
  MemoSubtitleText.SelLength := Length(NewText);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ReplaceText;
begin
  if not LastFindSucceded then
    Exit;

  if Assigned(SearchItem) then
  begin
    ReplaceTextMemo;
  end
  else
  begin
    ReplaceTextSubs;
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.LastFindSucceded : Boolean;
begin
  if Assigned(SearchItem) then
    Result := (SearchItemPos > 0)
  else
  Result := (SearchNodeIndex <> -1);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowErrorReportExecute(Sender: TObject);
begin
  ErrorReportForm.Visible := not ErrorReportForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionDelayExecute(Sender: TObject);
var DelayInMs : Integer;
    DelayShiftType : TDelayShiftType;
    Node : PVirtualNode; NodeData : PTreeData;
    UndoableDelayTask : TUndoableDelayTask;
begin
  DelayForm := TDelayForm.Create(nil);

  // Prefill the dialog
  if (vtvSubsList.SelectedCount > 1) then
  begin
    DelayForm.SetDelayApplyToType(dattSelected);
  end;
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    DelayForm.SetDelayInMS(WAVDisplayer.Selection.StopTime -
      WAVDisplayer.Selection.StartTime);
  end;

  if DelayForm.ShowModal = mrOk then
  begin
    DelayInMs := DelayForm.GetDelayInMs;

    Node := vtvSubsList.FocusedNode;
    NodeData := vtvSubsList.GetNodeData(Node);

    DelayShiftType := DelayForm.GetDelayShiftType;

    UndoableDelayTask := TUndoableDelayTask.Create;
    UndoableDelayTask.SetDelayInMs(DelayInMs);
    UndoableDelayTask.SetDelayShiftType(DelayShiftType);

    case DelayForm.GetDelayApplyToType of
      dattAll: // All subtitles
        begin
          UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
          Node := vtvSubsList.GetFirst;
          while Assigned(Node) do
          begin
            UndoableDelayTask.AddSubtitleIndex(Node.Index);
            Node := vtvSubsList.GetNext(Node);
          end;
        end;
      dattSelected: // Selected subs
        begin
          UndoableDelayTask.SetCapacity(vtvSubsList.SelectedCount);
          Node := vtvSubsList.GetFirstSelected;
          while Assigned(Node) do
          begin
            UndoableDelayTask.AddSubtitleIndex(Node.Index);
            Node := vtvSubsList.GetNextSelected(Node);
          end;
        end;
      dattFromCursor: // From focused subs to end
        begin
          Node := vtvSubsList.FocusedNode;
          if Assigned(Node) then
          begin
            UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount - Node.Index);
          end;
          while Assigned(Node) do
          begin
            UndoableDelayTask.AddSubtitleIndex(Node.Index);
            Node := vtvSubsList.GetNext(Node);
          end;
        end;
    end;

    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);

    Node := vtvSubsList.GetFirst;
    if Assigned(Node) then
     begin
       NodeData := vtvSubsList.GetNodeData(Node);
       if NodeData.Range.StartTime < 0 then
        begin
         ActionUndoExecute(self);
         MessageDlg('Subtitle with negative start time are not allowed (An undo Ctrl+Z has been applied)', mtWarning, [MbOk], 0);
         Exit;
        end;
     end;

  end;
  DelayForm.Free;
  DelayForm := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiSubListDeleteClick(Sender: TObject);
var Node : PVirtualNode;
    UndoableDeleteTask : TUndoableDeleteTask;
begin
  UndoableDeleteTask := TUndoableDeleteTask.Create;
  UndoableDeleteTask.SetCapacity(vtvSubsList.SelectedCount);
  Node := vtvSubsList.GetFirstSelected;
  while Assigned(Node) do
  begin
    UndoableDeleteTask.AddSubtitleIndex(Node.Index);
    Node := vtvSubsList.GetNextSelected(Node);
  end;
  UndoableDeleteTask.DoTask;
  PushUndoableTask(UndoableDeleteTask);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeExecute(Sender: TObject);
begin
  Merge(mtNormal, mrSelected);
end;

//------------------------------------------------------------------------------

procedure TMainForm.Merge(MergeType : TMergeType; MergeRange : TMergeRange);
var UndoableMergeTask : TUndoableMergeTask;
    Node, NodePrevious, NodeNext : PVirtualNode;
    SubRange : TSubtitleRange;
begin
  UndoableMergeTask := TUndoableMergeTask.Create;
  UndoableMergeTask.SetData(-1, MergeType);

  if (MergeRange = mrSelected) then
  begin
    UndoableMergeTask.SetCapacity(vtvSubsList.SelectedCount);
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      UndoableMergeTask.AddSubtitleIndex(Node.Index);
      Node := vtvSubsList.GetNextSelected(Node);
    end;
  end
  else if (MergeRange = mrWithPrevious) then
  begin
    Node := vtvSubsList.GetFirstSelected;
    NodePrevious := vtvSubsList.GetPrevious(Node);
    UndoableMergeTask.SetCapacity(2);
    UndoableMergeTask.AddSubtitleIndex(NodePrevious.Index);
    UndoableMergeTask.AddSubtitleIndex(Node.Index);
  end
  else if (MergeRange = mrWithNext) then
  begin
    Node := vtvSubsList.GetFirstSelected;
    NodeNext := vtvSubsList.GetNext(Node);
    UndoableMergeTask.SetCapacity(2);
    UndoableMergeTask.AddSubtitleIndex(Node.Index);
    UndoableMergeTask.AddSubtitleIndex(NodeNext.Index);
  end;

  if Assigned(WAVDisplayer.SelectedRange) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
    if vtvSubsList.Selected[SubRange.Node] then
    begin
      UndoableMergeTask.SetData(SubRange.Node.Index, MergeType);
    end;
  end;

  UndoableMergeTask.DoTask;
  PushUndoableTask(UndoableMergeTask);

  WAVDisplayer.UpdateView([uvfRange,uvfSelection]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ClearWAVSelection;
begin
  if Assigned(WAVDisplayer.SelectedRange) then
    WAVDisplayer.SelectedRange := nil;
  WAVDisplayer.ClearSelection;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SubListPopupMenuPopup(Sender: TObject);
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    pmiFixErrorEnabled : Boolean;
begin
  pmiSubListDelete.Enabled := (vtvSubsList.SelectedCount > 0);
  pmiSubListMerge.Enabled := (vtvSubsList.SelectedCount > 1);
  pmiSaveSelectedAs.Enabled := (vtvSubsList.SelectedCount > 1);

  pmiFixErrorEnabled := False;
  if (vtvSubsList.SelectedCount > 0) then
  begin
    JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
    JSPEnum.OnJSPluginError := LogForm.LogMsg;
    JSPEnum.Reset;
    while (not pmiFixErrorEnabled) and JSPEnum.GetNext(JPlugin) do
    begin
      JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
      pmiFixErrorEnabled := pmiFixErrorEnabled or ((Assigned(JSPluginInfo) and
        (JSPluginInfo.Enabled = True)) and JPlugin.CanFixError);
      FreeAndNil(JPlugin);
    end;
    JSPEnum.Free;
  end;
  pmiFixError.Enabled := pmiFixErrorEnabled;
end;

//------------------------------------------------------------------------------

function TMainForm.CheckSubtitlesAreSaved : Boolean;
var res : Integer;
begin
  Result := True;
  if CurrentProject.IsDirty then
  begin
    // Ask to save the subtitles
    res := MessageBoxW(Handle, PWideChar(WideFormat(
    'The subtitle file "%s" has changed, do you want to save it before closing ?',
      [CurrentProject.SubtitlesFile])),
      PWideChar(WideString('Warning')),
      MB_YESNOCANCEL or MB_ICONWARNING);
    if (res = IDYES) then
    begin
      SaveSubtitles(CurrentProject.SubtitlesFile, '', CurrentProject.IsUTF8, False, nil, False);
      SaveProject(CurrentProject, False);
      if CurrentProject.IsDirty then
        Result := False;
    end
    else if(res = IDCANCEL) then
      Result := False
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSubtitlesAreSaved;
  if CanClose then
    SaveSettings;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayPopupMenuPopup(Sender: TObject);
begin
  pmiWAVDispAddSubtitle.Enabled := not WAVDisplayer.SelectionIsEmpty;
  pmiWAVDispSetSubtitleTime.Enabled := (not WAVDisplayer.SelectionIsEmpty) and
    Assigned(vtvSubsList.FocusedNode);

  pmiWAVDispDeleteRange.Enabled := WAVDisplayer.RangeList.GetRangeIdxAt(
    WAVDisplayer.GetCursorPos) <> -1;
  pmiWAVDispSplitAtCursor.Enabled := pmiWAVDispDeleteRange.Enabled;
  pmiInsertKaraokeMarker.Enabled := pmiWAVDispDeleteRange.Enabled;
  pmiDeleteSC.Enabled := (not WAVDisplayer.SelectionIsEmpty);
  pmiInsertSC.Enabled := WAVDisplayer.SelectionIsEmpty;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
var
  AboutForm: TAboutForm;
begin
  AboutForm := TAboutForm.Create(nil);
  AboutForm.ShowModal;
  AboutForm.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNode(Node : PVirtualNode);
begin
  if Assigned(Node) then
  begin
    vtvSubsList.FocusedNode := Node;
    vtvSubsList.ClearSelection;
    vtvSubsList.Selected[Node] := True;
    vtvSubsList.ScrollIntoView(Node,True);
    vtvSubsListDblClick(nil);
    vtvSubsList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNextSub;
var CurrentNode, NextNode : PVirtualNode;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    WAVDisplayer.SelectNextKaraoke;
  end
  else
  begin
    if Assigned(vtvSubsList.FocusedNode) then
      CurrentNode := vtvSubsList.FocusedNode
    else
      CurrentNode := vtvSubsList.GetFirst;

    if Assigned(CurrentNode) then
    begin
      ActionStop.Execute;
      NextNode := vtvSubsList.GetNext(CurrentNode);
      if Assigned(NextNode) then
        SelectNode(NextNode)
      else
        SelectNode(CurrentNode);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNextSubExecute(Sender: TObject);
begin
  SelectNextSub;
  if (vtvSubsList.RootNodeCount > 0) then
  begin
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectPreviousSub;
var CurrentNode, PreviousNode : PVirtualNode;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    WAVDisplayer.SelectPreviousKaraoke;
  end
  else
  begin
    if Assigned(vtvSubsList.FocusedNode) then
      CurrentNode := vtvSubsList.FocusedNode
    else
      CurrentNode := vtvSubsList.GetFirst;

    if Assigned(CurrentNode) then
    begin
      ActionStop.Execute;
      PreviousNode := vtvSubsList.GetPrevious(CurrentNode);
      if Assigned(PreviousNode) then
        SelectNode(PreviousNode)
      else
        SelectNode(CurrentNode);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ActionPreviousSubExecute(Sender: TObject);
begin
  SelectPreviousSub;
  if (vtvSubsList.RootNodeCount > 0) then
  begin
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FullSortTreeAndSubList;
begin
  g_WebRWSynchro.BeginWrite;
  try
    WAVDisplayer.RangeList.FullSort;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsList.Sort(nil, 0, sdDescending, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PTreeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  Result := CompareRanges(Data1.Range,Data2.Range);
end;

//------------------------------------------------------------------------------

procedure TMainForm.chkAutoScrollSubClick(Sender: TObject);
begin
  TimerAutoScrollSub.Enabled := chkAutoScrollSub.Checked;
  OldAutoScrollIdx := -1;
  ConfigObject.AutoScrollSubtitles := chkAutoScrollSub.Checked;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TimerAutoScrollSubTimer(Sender: TObject);
var Idx : Integer;
    Node : PVirtualNode;
    SubRange : TSubtitleRange;
begin

  if WAVDisplayer.IsPlaying then
  begin
    Idx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetPlayCursorPos);
    if (Idx <> OldAutoScrollIdx) then
    begin
      if (Idx <> -1) then
      begin
        if not ConfigObject.ShowSpecialTagSubs And
         (copy(TSubtitleRange(WAVDisplayer.RangeList[Idx]).Text,1,1) = '{') And
         (pos('}',TSubtitleRange(WAVDisplayer.RangeList[Idx]).Text) > -1) then
         begin
          OldAutoScrollIdx := Idx;
          Exit;
         end;
        SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
        Node := SubRange.Node;
        vtvSubsList.ScrollIntoView(Node,True);
        // Needed if subtitle node is already the focused node
        if (MemoSubtitleText.Text = '') then
        begin
          vtvSubsList.FocusedNode := nil;
        end;
        vtvSubsList.FocusedNode := Node;
        vtvSubsList.ClearSelection;
        vtvSubsList.Selected[Node] := True;
      end
      else
      begin
        MemoSubtitleText.Tag := 0;
        TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
        MemoSubtitleText.Text := '';
        TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
        MemoSubtitleVO.Text := '';
      end;
      OldAutoScrollIdx := Idx;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionGotoExecute(Sender: TObject);
var Idx : integer;
    i : integer;
    GotoPositionMs : Integer;
    SubRange : TSubtitleRange;
begin
  GotoForm := TGotoForm.Create(nil);
  if GotoForm.ShowModal = mrOk then
  begin
    SubRange := nil;
    if GotoForm.rbGotoLine.Checked then
    begin
      Idx := StrToIntDef(GotoForm.EditLine.Text, 0) - 1;
      Constrain(Idx, 0, WAVDisplayer.RangeList.Count-1);
      SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
    end
    else if GotoForm.rbGotoTime.Checked then
    begin
      GotoPositionMs := TimeStringToMs(GotoForm.MaskEditTime.EditText);
      if (WAVDisplayer.RangeList.Count > 0) then
      begin
        for i:=0 to WAVDisplayer.RangeList.Count-1 do
        begin
          if ( ((WAVDisplayer.RangeList[i].StartTime <= GotoPositionMs) and
            (WAVDisplayer.RangeList[i].StopTime >= GotoPositionMs)) or
            (WAVDisplayer.RangeList[i].StartTime >= GotoPositionMs)) then
          begin
            SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
            Break;
          end;
        end;
        if not Assigned(SubRange) then
        begin
          SubRange := TSubtitleRange(WAVDisplayer.RangeList[WAVDisplayer.RangeList.Count-1]);
        end;
      end
      else
      begin
        // Display 20s centered on cursor
        WAVDisplayer.ZoomCenteredOn(GotoPositionMs,20000);
      end;
      WAVDisplayer.SetCursorPos(GotoPositionMs);
    end;
    if Assigned(SubRange) then
    begin
      SelectNode(SubRange.Node);
    end;
  end;
  GotoForm.Free;
  GotoForm := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNodeFromTimestamps(Start, Stop : Integer);
var i : integer;
    SubRange : TSubtitleRange;
begin
  SubRange := nil;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    if (WAVDisplayer.RangeList[i].StartTime = Start) and
      (WAVDisplayer.RangeList[i].StopTime = Stop) then
    begin
      SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
      Break;
    end;
  end;
  if Assigned(SubRange) then
  begin
    SelectNode(SubRange.Node);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNodeAtIndex(Index : Integer);
var SubRange : TSubtitleRange;
begin
  if (Index >= 0) and (Index < WAVDisplayer.RangeList.Count) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
    SelectNode(SubRange.Node);
  end
end;

//------------------------------------------------------------------------------

procedure TMainForm.StartStopServer(Stop : Boolean);
begin
  if Stop then
  begin
    if Assigned(Server) then
    begin
      Server.Free;
      Server := nil;
    end;
    MenuItemStopWebServer.Checked := True;
  end
  else
  begin
    if not Assigned(Server) then
    begin
      Server := THTTPServer.Create(ServerRootDir,ConfigObject.ServerPort);
      Server.EnableCompression := ConfigObject.EnableCompression;
      Server.Start;
    end;
    MenuItemStartWebServer.Checked := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemStartWebServerClick(Sender: TObject);
begin
  StartStopServer;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemStopWebServerClick(Sender: TObject);
begin
  StartStopServer(True);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowPreferences(TabSheet : TTntTabSheet);
var OldSwapState : Boolean;
    IniFile : TIniFile;
    OldSubListFont, OldSubTextFont : string;
    PrefFormResult: Integer;

begin
  OldSwapState := ConfigObject.SwapSubtitlesList;
  OldSubListFont := ConfigObject.SubListFont;
  OldSubTextFont := ConfigObject.SubTextFont;
  PreferencesFormInstance.LoadConfig(ConfigObject);
  if (TabSheet <> nil) then
    PreferencesFormInstance.PageControlPreferences.ActivePage := TabSheet;

  if (not PrefFormInitialized) then
  begin
    IniFile := TIniFile.Create(GetIniFilename);
    LoadFormPosition(IniFile, PreferencesFormInstance);
    IniFile.Free;
    PrefFormInitialized := True;
  end;

  PreferencesFormInstance.ChkUseInternalFilters.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.ChkEnableLavAudioMixing.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.ChkUseReclockAudioRenderer.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.LabelMessagePlaybackAvailable.Visible := ActionClose.Enabled;
  PreferencesFormInstance.EditSubVideoFont.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.TrackBarTransparencySubVideo.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.UpDownSubVideoOutline.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.EditOutlineSubVideoFont.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.ShapeSubVideoOutline.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.TrackBarTransparencyOutline.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.UpDownSubVideoShadow.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.EditShadowSubVideoFont.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.ShapeSubVideoShadow.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.TrackBarTransparencyShadow.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.ButtonDefaultSubtitlesVideo.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.bttSubVideoFont.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.ChkUseInternalFiltersClick(nil);
  PreferencesFormInstance.ChkDoNotUseInternalFiltersOnCreateProjectClick(nil);
  PreferencesFormInstance.ChkFlippedPicture.Enabled := not ActionClose.Enabled;
  PreferencesFormInstance.ChkFlippedSubtitles.Enabled := not ActionClose.Enabled;

  {$IFDEF enhanced}
  if IsUniversalAppEnviroment = True then
  begin
    PreferencesFormInstance.chkAssociateExtVSSPRJ.Enabled := False;
    PreferencesFormInstance.chkAssociateExtSRT.Enabled := False;
    PreferencesFormInstance.chkAssociateExtSSA.Enabled := False;
    PreferencesFormInstance.chkAssociateExtASS.Enabled := False;
  end;
  {$ENDIF}

  PrefFormResult := PreferencesFormInstance.ShowModal;
  if PrefFormResult = mrOk then
  begin
    PreferencesFormInstance.SaveConfig(ConfigObject);
    // force saving preference data
    IniFile := TIniFile.Create(GetIniFilename);
    ConfigObject.SaveIni(IniFile);
    IniFile.Free;
    // ****************************
    if Assigned(Server) then
      Server.EnableCompression := ConfigObject.EnableCompression;
    if(ConfigObject.SwapSubtitlesList <> OldSwapState) then
      Self.SwapSubList;
    MemoSubtitleText.Enabled := MemoLinesCounter.Enabled and
      ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
    MemoSubtitleVO.Enabled := MemoLinesCounter.Enabled and
      ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
    SetShortcut(IsTimingMode);
    ApplyMouseSettings;
    ApplyAutoBackupSettings;
    if (ConfigObject.SubListFont <> OldSubListFont) or (ConfigObject.SubTextFont <> OldSubTextFont) then
    begin
      ApplyFontSettings;
    end;
    ApplyMiscSettings;
  end
  else if PrefFormResult = mrCancel then
  begin
    //restore old colors when 'Cancel' is pressed
    ConfigObject.WavColors := ConfigObject.WavColorsTemp;
    WAVDisplayer.UpdateColors;
  end;
end;

//------------------------------------------------------------------------------

(*
procedure TMainForm.AutoFitSubsListColumns;
begin
  // Take into account all the lines to fit the index column
  vtvSubsList.Header.AutoFitColumns(False, smaNoColumn, INDEX_COL_INDEX, INDEX_COL_INDEX);
  // From start to style only use visible node
  vtvSubsList.Header.AutoFitColumns(False, smaAllColumns, START_COL_INDEX, STYLE_COL_INDEX);
  // Skip the text and VO column and autofit the rest of the columns only using visible node
  vtvSubsList.Header.AutoFitColumns(False, smaAllColumns, VO_COL_INDEX + 1);

  vtvSubsList.Invalidate;
end;
*)

procedure TMainForm.AutoFitSubsListColumns;
var Column : TVirtualTreeColumn;
    last_col : TColumnPosition;
    last_idx : Integer;
    i : Integer;
begin
  last_col := 0;
  last_idx := 0;
  for i := 0 to VO_COL_INDEX do
  begin
    Column := vtvSubsList.Header.Columns.Items[i];
    if (coVisible in Column.Options) and (last_col < Column.Position) then
    begin
      last_col := Column.Position;
      last_idx := i;
    end;
  end;
  vtvSubsList.Header.AutoSizeIndex := last_idx;
  vtvSubsList.Invalidate;
end;


//------------------------------------------------------------------------------

procedure TMainForm.ApplyFontSettings;
var DC : HDC;
    MLCanvas : TCanvas;
    TxtWidth : Integer;
    Node : PVirtualNode;
    TmpText : WideString;
    TmpSelStart, TmpSelLen, TmpTag : Integer;
    TmpTextVO : WideString;
begin
  // Update subtitle list font
  String2Font(ConfigObject.SubListFont, vtvSubsList.Font);
  AutoFitSubsListColumns;
  vtvSubsList.Canvas.Font.Assign(vtvSubsList.Font);
  // Update subtitle node height
  vtvSubsList.DefaultNodeHeight := Round(vtvSubsList.Canvas.TextHeight('Tj') * 1.1);
  vtvSubsList.BeginUpdate;
  Node := vtvSubsList.GetFirst;
  while Assigned(Node) do
  begin
    vtvSubsList.NodeHeight[Node] := vtvSubsList.DefaultNodeHeight;
    Node := vtvSubsList.GetNext(Node);
  end;
  vtvSubsList.EndUpdate;

  SuggestionForm.vtvSuggestionsLst.Font.Name := vtvSubsList.Font.Name;
  SuggestionForm.vtvSuggestionsLst.Canvas.Font.Name := vtvSubsList.Font.Name;
  SuggestionForm.MemoSuggPreview.Font.Name := vtvSubsList.Font.Name;
  SuggestionForm.UpdateFonts;


  // Workaround to apply font
  TmpTextVO := MemoSubtitleVO.Text;
  MemoSubtitleVO.Clear;
  TmpTag := MemoSubtitleText.Tag;
  MemoSubtitleText.Tag := 0;
  TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
  TmpText := MemoSubtitleText.Text;
  TmpSelStart := MemoSubtitleText.SelStart;
  TmpSelLen := MemoSubtitleText.SelLength;
  MemoSubtitleText.Clear;

  String2Font(ConfigObject.SubTextFont, MemoSubtitleText.Font);
  MemoLinesCounter.Font.Assign(MemoSubtitleText.Font);
  MemoSubtitleVO.Font.Assign(MemoSubtitleText.Font);

  MemoSubtitleText.Text := TmpText;
  MemoSubtitleText.SelStart := TmpSelStart;
  MemoSubtitleText.SelLength := TmpSelLen;
  TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
  MemoSubtitleText.Tag := TmpTag;
  MemoSubtitleVO.Text := TmpTextVO;

  
  // Update line counter size so we have at least 3 digit
  DC := GetDC(MemoLinesCounter.Handle);
  if(DC <> 0) then
  begin
    MLCanvas := TCanvas.Create;
    MLCanvas.Font.Assign(MemoLinesCounter.Font);
    MLCanvas.Handle := DC;
    TxtWidth := MLCanvas.TextWidth('000');
    MLCanvas.Free;
    ReleaseDC(0,DC);
    MemoLinesCounter.Width := TxtWidth + 2*4;
  end;

  // TODO ? : status bar
  // Adjust status bar panel size for people using bigger fonts
  //StatusBarPanel1.Assign(TntStatusBar1.Font);
  //StatusBarPanel1.Width :=  TntStatusBar1.Canvas.TextWidth(
  //  'Line: 99, Column: 999 | Total: 999, Char/s: 99');
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyAutoBackupSettings;
begin
  TimerAutoBackup.Enabled := False;
  TimerAutoBackup.Interval := ConfigObject.AutoBackupEvery * 60 * 1000;
  TimerAutoBackup.Enabled := (ConfigObject.AutoBackupEvery > 0);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPreferencesExecute(Sender: TObject);
begin
  ShowPreferences(nil);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionErrorPreferencesExecute(Sender: TObject);
begin
  ShowPreferences(PreferencesFormInstance.tsErrorChecking);
end;

//------------------------------------------------------------------------------

procedure TMainForm.CloseProject;
var EmptyArray : TIntegerDynArray;
begin
  TimerAutoScrollSub.Enabled := False;
  SetLength(EmptyArray, 0);

  g_WebRWSynchro.BeginWrite;
  try
    SuggestionForm.Clear;
    ErrorReportForm.Clear;
    vtvSubsList.Clear;

    WAVDisplayer.ClearRangeList;
    WAVDisplayer.ClearRangeListVO;
    WAVDisplayer.Enabled := False;
    WAVDisplayer.Close;
    WAVDisplayer.Invalidate;
    WAVDisplayer.VerticalScaling := 100;
    WAVDisplayer.SetSceneChangeList(EmptyArray);
    MenuItemSetVideoResolution.Enabled := False;

    CurrentProject.Filename := '';
    CurrentProject.VideoSource := '';
    CurrentProject.WAVFile := '';
    CurrentProject.PeakFile := '';
    CurrentProject.SubtitlesFile := '';
    CurrentProject.SubtitlesVO := '';
    CurrentProject.IsDirty := False;
  finally
    g_WebRWSynchro.EndWrite;
  end;
    
  Self.Caption := ApplicationName;
  MemoLinesCounter.Text := '';
  StatusBarPanel1.Caption := '';
  TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
  MemoSubtitleText.Text := '';
  TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
  MemoSubtitleVO.Text := '';

  EnableControl(False);
  cbStyles.Clear;
  EnableStyleControls(False);
  TurnOffVO;

  AudioOnlyRenderer.Close;
  VideoRenderer.Close;
  g_VideoGraphDebugInfo := '';
  g_WavExtractorGraphDebugInfo := '';
  g_AudioGraphDebugInfo := '';

  g_VSSCoreWrapper.SetVideoWidth(0);
  g_VSSCoreWrapper.SetVideoHeight(0);

  if Assigned(SilentZoneForm) then
    SilentZoneForm.Clear;

  ClearStack(RedoStack);
  ClearStack(UndoStack);
  ActionUndo.Enabled := False;
  ActionRedo.Enabled := False;
  ResynchToolForm.Clear;
  ComboBoxSilentZone.Items.Clear;

end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionCloseExecute(Sender: TObject);
begin
  if CheckSubtitlesAreSaved then
  begin
    CloseProject;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowSuggestionsExecute(Sender: TObject);
begin
  SuggestionForm.Visible := not SuggestionForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemHelpIndexClick(Sender: TObject);
var HelpFilename : WideString;
begin
  HelpFilename := WideExtractFilePath(TntApplication.ExeName);
  HelpFilename := WideIncludeTrailingBackslash(HelpFilename);
  HelpFilename := HelpFilename + 'help\index.html';
  Tnt_ShellExecuteW(0, 'open', PWideChar(HelpFilename), nil, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemHelpIndexWAVDisplayControlClick(
  Sender: TObject);
var HelpFilename : WideString;
begin
  HelpFilename := WideExtractFilePath(TntApplication.ExeName);
  HelpFilename := WideIncludeTrailingBackslash(HelpFilename);
  HelpFilename := HelpFilename + 'help\wavdisplaycontrol.html';
  Tnt_ShellExecuteW(0, 'open', PWideChar(HelpFilename), nil, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionInsertTextFileExecute(Sender: TObject);
var
  Line, Ext: WideString;
  LastRange, NewRange : TRange;
  StartTime : Integer;
  Node: PVirtualNode;
  NodeData: PTreeData;
  HaveNewSub : Boolean;
  Source : TTntStringList;
  I, LineIndex, SubCountBefore, fidx : Integer;
  UndoableMultiAddTask : TUndoableMultiAddTask;
  SRTParser : TSRTParser;
  SubList : TList;
  SRTSub : TSRTSubtitle; 
begin

  TntOpenDialog1.Filter := 'Text files|*.TXT' + '|' +
    'Subtitle files (*.srt)|*.SRT' + '|' +
    'All files (*.*)|*.*';
  TntOpenDialog1.FilterIndex := 2;
  TntOpenDialog1.Options := TntOpenDialog1.Options + [ofAllowMultiSelect];
  SetOpenDialogPath(TntOpenDialog1);
  if not TntOpenDialog1.Execute then
  begin
    TntOpenDialog1.Options := TntOpenDialog1.Options - [ofAllowMultiSelect];
    Exit;
  end;
  TntOpenDialog1.Options := TntOpenDialog1.Options - [ofAllowMultiSelect];

  Source := nil;
  
  g_WebRWSynchro.BeginWrite;
  try
    for fidx := 0 to TntOpenDialog1.Files.Count-1 do
    begin
      TntOpenDialog1.FileName := TntOpenDialog1.Files[fidx];

      // TODO : insert SSA/ASS file
      Ext := WideLowerCase(WideExtractFileExt(TntOpenDialog1.FileName));
      if (Ext = '.srt') then
      begin
        SubList := TList.Create;
        SRTParser := TSRTParser.Create;
        SRTParser.Load(TntOpenDialog1.FileName);
        for i := 0 to SRTParser.GetCount-1 do
        begin
          SRTSub := SRTParser.GetAt(i);
          NewRange := SubRangeFactory.CreateRangeSS(SRTSub.Start, SRTSub.Stop);
          TSubtitleRange(NewRange).Text := SRTSub.Text;
          NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);
          SubList.Add(NewRange);
        end;
        SRTParser.Free;

        if (SubList.Count > 0) then
        begin
          UndoableMultiAddTask := TUndoableMultiAddTask.Create;
          UndoableMultiAddTask.SetData(SubList);
          UndoableMultiAddTask.DoTask;
          PushUndoableTask(UndoableMultiAddTask);
          CurrentProject.IsDirty := True;
        end;
        FreeAndNil(SubList);
      end
      else
      begin
        HaveNewSub := False;
        Source := MyTTntStringList.Create;
        Source.LoadFromFile(TntOpenDialog1.FileName);

        // Get start time for the first new subtitle
        if (WAVDisplayer.RangeList.Count > 0) then
        begin
          LastRange := WAVDisplayer.RangeList[WAVDisplayer.RangeList.Count-1];
          StartTime := LastRange.StopTime + 2000;
        end
        else
          StartTime := 99 * 60 * 60 * 1000;

        SubCountBefore := WAVDisplayer.RangeList.Count;
        for LineIndex := 0 to Source.Count-1 do
        begin
          Line := Source[LineIndex];

          NewRange := SubRangeFactory.CreateRangeSS(StartTime, StartTime+1000);
          TSubtitleRange(NewRange).Text := Line;
          WAVDisplayer.RangeList.AddAtEnd(NewRange);
          Node := vtvSubsList.AddChild(nil);
          NodeData := vtvSubsList.GetNodeData(Node);
          NodeData.Range := TSubtitleRange(NewRange);
          TSubtitleRange(NewRange).Node := Node;

          Inc(StartTime, 2000);
          HaveNewSub := True;
        end;

        if HaveNewSub then
        begin
          CurrentProject.IsDirty := True;
          UndoableMultiAddTask := TUndoableMultiAddTask.Create;
          UndoableMultiAddTask.SetCapacity(Source.Count);
          for I := SubCountBefore to (SubCountBefore + Source.Count - 1) do
          begin
            UndoableMultiAddTask.AddSubtitleIndex(I);
          end;
          // this is a lazy task so don't "do" the task now
          PushUndoableTask(UndoableMultiAddTask);
        end;
        FreeAndNil(Source);
      end;

    end;

  finally
    g_WebRWSynchro.EndWrite;
  end;
  
  UpdateDurationWithSubOnly;
  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SwapSubList(SwapSizeAlso : Boolean);
var SavHeight, SavHeight2 : Integer;
begin
  if Swapped then
  begin
    SavHeight := vtvSubsList.Height;
    SavHeight2 := MemoLinesCounter.Height;

    vtvSubsList.Parent := PanelMiddle;
    MemoLinesCounter.Parent := PanelBottom;
    MemoSubtitleText.Parent := PanelBottom;
    MemoSubtitleVO.Parent := PanelBottom;
    SplitterSubtitleVO.Parent := PanelBottom;

    PanelBottom.Align := alBottom;
    Splitter1.Align := alBottom;
    PanelMiddle.Align := alClient;
    if(SwapSizeAlso) then
      PanelBottom.Height := SavHeight2;
    StatusBarMainPanel.Top := MaxInt; // make sure status bar stay at bottom
  end
  else
  begin
    SavHeight := vtvSubsList.Height;
    SavHeight2 := MemoLinesCounter.Height;

    vtvSubsList.Parent := PanelBottom;
    MemoLinesCounter.Parent := PanelMiddle;
    MemoSubtitleText.Parent := PanelMiddle;
    MemoSubtitleVO.Parent := PanelMiddle;
    SplitterSubtitleVO.Parent := PanelMiddle;

    PanelMiddle.Align := alTop;
    Splitter1.Align := alTop;
    PanelBottom.Align := alClient;

    if(SwapSizeAlso) then
      PanelMiddle.Height := PanelMiddle.Height - SavHeight + SavHeight2;
    Splitter1.Top := MaxInt;
  end;
  Swapped := not Swapped;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionClearSelectionExecute(Sender: TObject);
begin
  WAVDisplayer.ClearSelection;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if (Swapped) then
  begin
    Accept := (NewSize > PanelMiddle.Constraints.MinHeight) and
      ((PanelBottom.Height + PanelMiddle.Height - NewSize) > PanelBottom.Constraints.MinHeight);
  end
  else
  begin
    Accept := ((PanelMiddle.Height + PanelBottom.Height - NewSize) > PanelMiddle.Constraints.MinHeight) and
      (NewSize > PanelBottom.Constraints.MinHeight);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.bttWorkingModeClick(Sender: TObject);
begin
  ActionToggleTimingMode.Execute;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetShortcut(TimingMode : Boolean);
var i : integer;
    HLID : THotkeyListItemData;
begin
  for i:=0 to ConfigObject.ListHotkeys.Count-1 do
  begin
    HLID := ConfigObject.ListHotkeys[i];
    if TimingMode then
      HLID.Action.ShortCut := HLID.TimingShortCut
    else
      HLID.Action.ShortCut := HLID.NormalShortCut
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoopSelStartExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    if IsTimingMode then
      PanelWAVDisplay.SetFocus;
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionStart;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection.StartTime,
      Min(WAVDisplayer.Selection.StartTime + StartEndPlayingDuration, WAVDisplayer.Selection.StopTime),
      True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoopSelEndExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    if IsTimingMode then
      PanelWAVDisplay.SetFocus;
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionEnd;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
      WAVDisplayer.Selection.StopTime,
      True);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.AddSubtitle(StartTime, StopTime : Integer; Text : WideString; UpdateDisplay : Boolean) : PVirtualNode;
var NewRange, SiblingRange : TRange;
    NewNode, SiblingNode: PVirtualNode;
    NodeData: PTreeData;
    InsertPos : Integer;
begin
  NewRange := SubRangeFactory.CreateRangeSS(StartTime, StopTime);
  InsertPos := WAVDisplayer.RangeList.FindInsertPos(NewRange);
  if (InsertPos >= WAVDisplayer.RangeList.Count) then
  begin
    NewNode := vtvSubsList.AddChild(nil);
  end
  else
  begin
    SiblingRange := WAVDisplayer.RangeList[InsertPos];
    SiblingNode := TSubtitleRange(SiblingRange).Node;
    NewNode := vtvSubsList.InsertNode(SiblingNode, amInsertBefore);
  end;
  NodeData := vtvSubsList.GetNodeData(NewNode);
  NodeData.Range := TSubtitleRange(NewRange);
  TSubtitleRange(NewRange).Node := NewNode;
  if Length(Text) > 0 then
  begin
    TSubtitleRange(NewRange).Text := Text;
  end;
  // TODO improvement : use the previously calculated InsertPos
  WAVDisplayer.AddRange(NewRange, UpdateDisplay);

  // Update SearchNodeIndex (TODO : SearchPos)
  if (SearchNodeIndex >= NewNode.Index) then
    Inc(SearchNodeIndex);

  UpdateDurationWithSubOnly;

  Result := NewNode;
end;

//------------------------------------------------------------------------------

function TMainForm.AddSubtitle(SubRange : TSubtitleRange; UpdateDisplay : Boolean) : PVirtualNode;
var NewRange : TSubtitleRange;
    NewNode: PVirtualNode;
    NodeData: PTreeData;
begin
  NewNode := AddSubtitle(SubRange.StartTime, SubRange.StopTime, '', UpdateDisplay);
  NodeData := vtvSubsList.GetNodeData(NewNode);
  NewRange := TSubtitleRange(NodeData.Range);
  NewRange.Assign(SubRange);
  Result := NewNode;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FocusNodeAt(Index : Integer);
var SubRange : TSubtitleRange;
    Node : PVirtualNode;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  Node := SubRange.Node;
  FocusNode(Node, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.FocusNode(Node : PVirtualNode; WAVDisplaySelect : Boolean);
var NodeData : PTreeData;
begin
  if (not vtvSubsList.IsVisible[Node]) then
  begin
    vtvSubsList.ScrollIntoView(Node, True);
  end;
  vtvSubsList.FocusedNode := Node;
  vtvSubsList.ClearSelection;
  vtvSubsList.Selected[Node] := True;
  vtvSubsList.Repaint;

  if WAVDisplaySelect and (WAVDisplayer.SelMode = smCoolEdit) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    WAVDisplayer.SelectedRange := NodeData.Range;
  end;

  UpdateStylesComboboxFromSelection;
end;

//------------------------------------------------------------------------------

function TMainForm.AddSubtitle(StartTime, StopTime : Integer; Text : WideString;
  UpdateDisplay : Boolean; AutoSelect : Boolean) : PVirtualNode;
var NewNode : PVirtualNode;
begin
  g_WebRWSynchro.BeginWrite;
  try
    NewNode := AddSubtitle(StartTime, StopTime, Text, UpdateDisplay);
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  FocusNode(NewNode, AutoSelect);

  Result := NewNode;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionAddSubtitleExecute(Sender: TObject);
var UndoableAddTask : TUndoableAddTask;
begin
  UndoableAddTask := TUndoableAddTask.Create;
  UndoableAddTask.SetTime(WAVDisplayer.Selection.StartTime,
    WAVDisplayer.Selection.StopTime);
  UndoableAddTask.SetAutoSelect(True);
  UndoableAddTask.SetText(ConfigObject.CustomText);
  UndoableAddTask.DoTask;
  PushUndoableTask(UndoableAddTask);

  if IsNormalMode and MemoSubtitleText.Enabled then
    MemoSubtitleText.SetFocus;

  // Adjust column size when adding the first subtitle
  if vtvSubsList.RootNodeCount = 1 then
  begin
    AutoFitSubsListColumns;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleTimeExecute(Sender: TObject);
var NodeData : PTreeData;
    FocusedNode : PVirtualNode;
    SelDuration : Integer;
    UndoableSetTimeTask : TUndoableSetTimeTask;
begin
  if WAVDisplayer.SelectionIsEmpty then
    Exit;
  SelDuration := WAVDisplayer.Selection.StopTime - WAVDisplayer.Selection.StartTime;
  if (SelDuration < 100) then
    Exit;
  FocusedNode := vtvSubsList.FocusedNode;
  if Assigned(FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(FocusedNode);
    UndoableSetTimeTask := TUndoableSetTimeTask.Create;
    UndoableSetTimeTask.SetData(FocusedNode.Index,
      NodeData.Range.StartTime, NodeData.Range.StopTime,
      WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime);
    UndoableSetTimeTask.DoTask;
    PushUndoableTask(UndoableSetTimeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate70Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(70);
  AudioOnlyRenderer.SetRate(70);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate60Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(60);
  AudioOnlyRenderer.SetRate(60);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate80Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(80);
  AudioOnlyRenderer.SetRate(80);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate90Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(90);
  AudioOnlyRenderer.SetRate(90);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate100Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(100);
  AudioOnlyRenderer.SetRate(100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.OffsetCurrentSubtitleStartTime(Offset : Integer);
var NodeData : PTreeData;
    R : TRange;
    bStart, bStop, OldTime : Integer;
    Idx : Integer;
    UndoableDelayTask : TUndoableDelayTask;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    R := WAVDisplayer.KaraokeSelectedRange;
    Idx := (WAVDisplayer.KaraokeSelectedIndex - 1);
    if (Idx >= 0) then
    begin
      // Calcul bounds
      if (Idx = 0) then
        bStart := R.StartTime + 10
      else
        bStart := R.SubTime[Idx-1] + 10;

      if (Idx < Length(R.SubTime)-1) then
        bStop := R.SubTime[Idx+1] - 10
      else
        bStop := R.StopTime - 10;

      OldTime := R.SubTime[Idx];
      R.SubTime[Idx] := R.SubTime[Idx] + Offset;
      Constrain(R.SubTime[Idx], bStart, bStop);
      WAVDisplayer.Selection.StartTime := R.SubTime[Idx];
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(WAVDisplayer, R, Idx, OldTime);
      WAVDisplayer1SelectionChange(WAVDisplayer);
      Exit;
    end;
  end;

  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    if(NodeData.Range.StartTime + Offset < NodeData.Range.StopTime) then
    begin
      UndoableDelayTask := TUndoableDelayTask.Create;
      UndoableDelayTask.SetDelayInMs(Offset);
      UndoableDelayTask.SetDelayShiftType(dstStartTimeOnly);
      UndoableDelayTask.SetCapacity(1);
      UndoableDelayTask.AddSubtitleIndex(vtvSubsList.FocusedNode.Index);
      UndoableDelayTask.DoTask;
      PushUndoableTask(UndoableDelayTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OffsetCurrentSubtitleStopTime(Offset : Integer);
var NodeData : PTreeData;
    R : TRange;
    bStart, bStop, OldTime : Integer;
    Idx : Integer;
    UndoableDelayTask : TUndoableDelayTask;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    R := WAVDisplayer.KaraokeSelectedRange;
    Idx := WAVDisplayer.KaraokeSelectedIndex;
    if (Idx < Length(R.SubTime)) then
    begin
      // Calcul bounds
      if (Idx = 0) then
        bStart := R.StartTime + 10
      else
        bStart := R.SubTime[Idx-1] + 10;

      if (Idx < Length(R.SubTime)-1) then
        bStop := R.SubTime[Idx+1] - 10
      else
        bStop := R.StopTime - 10;

      OldTime := R.SubTime[Idx];
      R.SubTime[Idx] := R.SubTime[Idx] + Offset;
      Constrain(R.SubTime[Idx], bStart, bStop);
      WAVDisplayer.Selection.StopTime := R.SubTime[Idx];
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(WAVDisplayer, R, Idx, OldTime);
      WAVDisplayer1SelectionChange(WAVDisplayer);
      Exit;
    end;
  end;

  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    if(NodeData.Range.StopTime + Offset > NodeData.Range.StartTime) then
    begin
      UndoableDelayTask := TUndoableDelayTask.Create;
      UndoableDelayTask.SetDelayInMs(Offset);
      UndoableDelayTask.SetDelayShiftType(dstStopTimeOnly);
      UndoableDelayTask.SetCapacity(1);
      UndoableDelayTask.AddSubtitleIndex(vtvSubsList.FocusedNode.Index);
      UndoableDelayTask.DoTask;
      PushUndoableTask(UndoableDelayTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartPlus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartMinus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(-100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopPlus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopMinus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(-100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartPlus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartMinus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(-10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopPlus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopMinus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(-10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomVerticalExecute(Sender: TObject);
begin
  if (VerticalScalingForm = nil) then
    VerticalScalingForm := TVerticalScalingForm.Create(Self,WAVDisplayer);
  VerticalScalingForm.Visible := not VerticalScalingForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideTextPipeExecute(Sender: TObject);
begin
  SplitterMemoTextPipe.Visible := not SplitterMemoTextPipe.Visible;
  MemoTextPipe.Visible := not MemoTextPipe.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoadTextPipeExecute(Sender: TObject);
  const
    INDEX_STRIPPED : Integer = 4;
  var
    SRTParser : TSRTParser;
    SRTSub : TSRTSubtitle;
    i : Integer;
    StrippedLines : TTntStringList;
begin
  TntOpenDialog1.Filter :=
    'Text files (*.txt)|*.TXT' + '|' +
    'RTF files (*.rtf)|*.RTF' + '|' +
    'Subtitle files (*.srt)|*.SRT' + '|' +
    'Subtitle files stripped of timings (*.srt)|*.SRT' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialog1);
  if TntOpenDialog1.Execute then
  begin
    MemoTextPipe.Clear;
    if TntOpenDialog1.FilterIndex = INDEX_STRIPPED then
    begin
      SRTParser := TSRTParser.Create;
      SRTParser.Load(TntOpenDialog1.Filename);
      StrippedLines := TTntStringList.Create;
      for i := 0 to SRTParser.GetCount-1 do
      begin
        SRTSub := SRTParser.GetAt(i);
        StrippedLines.Add(SRTSub.Text);
      end;
      MemoTextPipe.Lines.AddStrings(StrippedLines);
      MemoTextPipe.SelStart := 0;
      MemoTextPipe.SelLength := 0;
      FreeAndNil(SRTParser);
      FreeAndNil(StrippedLines);
    end
    else
    begin
      MemoTextPipe.Lines.LoadFromFile(TntOpenDialog1.FileName);
    end;

    CurrentProject.TextPipeSource := TntOpenDialog1.FileName;
    SaveProject(CurrentProject, True);

    if (MemoTextPipe.Visible = False) then
      ActionShowHideTextPipe.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionClearTextPipeExecute(Sender: TObject);
begin
  MemoTextPipe.Clear;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveTextPipeAsExecute(Sender: TObject);
begin
  TntSaveDialog1.Filter := 'Text file (*.txt)|*.TXT|RTF file (*.rtf)|*.RTF';
  TntSaveDialog1.FileName := CurrentProject.TextPipeSource;
  if TntSaveDialog1.Execute then
  begin
    if TntSaveDialog1.FilterIndex = 1 then
      MemoTextPipe.PlainText := True
    else if TntSaveDialog1.FilterIndex = 2 then
      MemoTextPipe.PlainText := False;
    MemoTextPipe.Lines.SaveToFile(TntSaveDialog1.FileName);
    MemoTextPipe.PlainText := False;
    CurrentProject.TextPipeSource := TntSaveDialog1.FileName;
    SaveProject(CurrentProject, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionAddSubFromPipeExecute(Sender: TObject);
var UndoableAddTask : TUndoableAddTask;
    UndoablePipeTask : TUndoableTask;
    UndoableCompositeTask : TUndoableCompositeTask;
begin
  if (MemoTextPipe.SelLength > 0) and (not WAVDisplayer.SelectionIsEmpty) then
  begin
    UndoableCompositeTask := TUndoableCompositeTask.Create;

    // Add subtitle part
    UndoableAddTask := TUndoableAddTask.Create;
    UndoableAddTask.SetTime(WAVDisplayer.Selection.StartTime,
      WAVDisplayer.Selection.StopTime);
    UndoableAddTask.SetText(Trim(MemoTextPipe.SelText));
    UndoableAddTask.SetAutoSelect(True);

    // Pipe part
    UndoablePipeTask := ColorizeOrDeleteTextPipe;

    UndoableCompositeTask.AddTask(UndoableAddTask);
    UndoableCompositeTask.AddTask(UndoablePipeTask);

    UndoableCompositeTask.DoTask;
    PushUndoableTask(UndoableCompositeTask);
    if IsNormalMode and MemoSubtitleText.Enabled then
      MemoSubtitleText.SetFocus;
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.ColorizeOrDeleteTextPipe : TUndoableTask;
var ActionType : Integer;
    NewColor : TColor;
    UndoablePipeTask : TUndoablePipeTask;
begin
  NewColor := $003333FF;
  ActionType := 0;  
  UndoablePipeTask := TUndoablePipeTask.Create;
  if pmiAutoColorizeText.Checked then
  begin
    ActionType := 0;
    if (Assigned(vtvSubsList.FocusedNode)) and
       ((vtvSubsList.AbsoluteIndex(vtvSubsList.FocusedNode) mod 2) <> 0) then
      NewColor := $00FF8000;
  end
  else if pmiAutoDeleteText.Checked then
  begin
    ActionType := 1;
  end
  else if pmiAutoDeleteAllTextBefore.Checked then
  begin
    ActionType := 2;
  end;
  UndoablePipeTask.SetData(ActionType, MemoTextPipe, NewColor);
  Result := UndoablePipeTask;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoTextPipePopupMenuPopup(Sender: TObject);
begin
  pmiAddAsSubtitle.Enabled := (MemoTextPipe.SelLength > 0) and
    (not WAVDisplayer.SelectionIsEmpty);
  pmiReplaceSubtitleFromPipe.Enabled := (MemoTextPipe.SelLength > 0) and
    (vtvSubsList.FocusedNode <> nil);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionReplaceFromPipeExecute(Sender: TObject);
var NextNode : PVirtualNode;
begin
  if (MemoTextPipe.SelLength > 0) and (vtvSubsList.FocusedNode <> nil) then
  begin
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
    MemoSubtitleText.Text := Trim(MemoTextPipe.SelText);
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
    ColorizeOrDeleteTextPipe;
    // Go to next subtitle
    NextNode := vtvSubsList.GetNext(vtvSubsList.FocusedNode);
    if Assigned(NextNode) then
      SelectNode(NextNode);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyMouseSettings;
begin
  WAVDisplayer.WheelTimeScroll := ConfigObject.MouseWheelTimeScrollModifier;
  WAVDisplayer.WheelVZoom := ConfigObject.MouseWheelVZoomModifier;
  WAVDisplayer.WheelHZoom := ConfigObject.MouseWheelHZoomModifier;
  if IsTimingMode then
  begin
    // Timing mode
    if ConfigObject.MouseEnableSSATimingMode then
      WAVDisplayer.SelMode := smSSA
    else
      WAVDisplayer.SelMode := smCoolEdit;
  end;
  WAVDisplayer.SnappingEnabled := ConfigObject.EnableMouseSnapping;
  WAVDisplayer.EnableMouseAntiOverlapping := ConfigObject.EnableMouseAntiOverlapping;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TimerAutoBackupTimer(Sender: TObject);
var BackupDstFilename : WideString;
begin
  if ActionSave.Enabled then
  begin
    CheckBackupDirectory;
    BackupDstFilename := g_BackupDirectory +
      WideChangeFileExt(WideExtractFileName(CurrentProject.SubtitlesFile), '.bak');
    SaveSubtitles(BackupDstFilename, '', CurrentProject.IsUTF8, True, nil, False);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionDetachVideoExecute(Sender: TObject);
begin
  if MenuItemDetachVideoWindow.Checked then
  begin
    DetachedVideoForm.Visible := False;
    if VideoRenderer.IsOpen then
      VideoRenderer.SetDisplayWindow(PanelVideo.Handle);
    PanelVideo.Visible := ShowingVideo;
    SplitterWAVDisplay_Video.Visible := ShowingVideo;
  end
  else
  begin
    PanelVideo.Visible := False;
    SplitterWAVDisplay_Video.Visible := False;
    if VideoRenderer.IsOpen then
      VideoRenderer.SetDisplayWindow(DetachedVideoForm.Handle);
    if ShowingVideo then
      ShowWindow(DetachedVideoForm.Handle, SW_SHOWNOACTIVATE); // Don't give focus
    DetachedVideoForm.Caption := Caption;  
    DetachedVideoForm.Visible := ShowingVideo;
    // Workaround to missing resize event bug => force repaint
    WAVDisplayer.UpdateView([uvfPageSize]);
  end;
  MenuItemDetachVideoWindow.Checked := not MenuItemDetachVideoWindow.Checked;

  if Assigned(CurrentProject) and (MenuItemDetachVideoWindow.Checked <> CurrentProject.DetachedVideo) then
  begin
    CurrentProject.DetachedVideo := MenuItemDetachVideoWindow.Checked;
    SaveProject(CurrentProject, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionCheckErrorsExecute(Sender: TObject);
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    SubRangeCurrent, SubRangePrevious, SubRangeNext : TSubtitleRange;
    i : Integer;
    Start, ExecTime : Cardinal;
    ResultMsg, Msg : WideString;
    JSPluginInfo : TJSPluginInfo;
    CM : ICursorManager;
begin
  CM := TCursorManager.Create(crHourGlass);

  LogForm.Clear;
  LogForm.SilentLogMsg('Starting error checking :');

  ErrorReportForm.Clear;
  Start := GetTickCount;  
  ErrorReportForm.vtvErrorList.BeginUpdate;

  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  g_WebRWSynchro.BeginWrite; // Should not be needed cause we only read for error checking, but we never know
  try
    while JSPEnum.GetNext(JPlugin) do
    begin
      JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
      if Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = False) then
      begin
        FreeAndNil(JPlugin);
        Continue;
      end;

      ConfigObject.ApplyParam(JPlugin);

      for i := 0 to WAVDisplayer.RangeList.Count-1 do
      begin
        SubRangeCurrent := TSubtitleRange(WAVDisplayer.RangeList[i]);

        if (i > 0) then
          SubRangePrevious := TSubtitleRange(WAVDisplayer.RangeList[i-1])
        else
          SubRangePrevious := nil;

        if (i < WAVDisplayer.RangeList.Count-1) then
          SubRangeNext := TSubtitleRange(WAVDisplayer.RangeList[i+1])
        else
          SubRangeNext := nil;

        ResultMsg := JPlugin.HasError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        if (ResultMsg <> '') then
        begin
          ErrorReportForm.AddError(SubRangeCurrent, JSColorToTColor(JPlugin.Color),
            JPlugin.Msg, ResultMsg, JPlugin.Filename, JPlugin.Name);
        end;
        if JPlugin.FatalError then
          Break;
      end;
      FreeAndNil(JPlugin);
    end;
    JSPEnum.Free;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  ExecTime := GetTickCount - Start;

  ErrorReportForm.vtvErrorList.EndUpdate;
  if (ErrorReportForm.vtvErrorList.TotalCount > 0) then
  begin
    ErrorReportForm.Visible := True;
    Msg := Format('%d error(s) found', [ErrorReportForm.vtvErrorList.TotalCount]);
  end
  else
    Msg := 'No error found';
  Msg := Msg + Format(' (in %d ms).', [ExecTime]);
  ErrorReportForm.TntStatusBar1.Panels[0].Text := Msg;
  LogForm.SilentLogMsg(Msg);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideLogsExecute(Sender: TObject);
begin
  LogForm.Visible := not LogForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFixSelectedErrorsExecute(Sender: TObject);
begin
  ErrorReportForm.FixSelectedErrors;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFixErrorMainExecute(Sender: TObject);
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    i : integer;
    ResultMsg : WideString;
    SubRangeCurrent, SubRangePrevious, SubRangeNext : TSubtitleRange;
    NodeData : PTreeData;
    Node : PVirtualNode;
begin
  if not Assigned(vtvSubsList.FocusedNode) then
    Exit;

  UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;

  g_WebRWSynchro.BeginWrite;
  try
    while JSPEnum.GetNext(JPlugin) do
    begin
      JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
      if Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = False) then
      begin
        FreeAndNil(JPlugin);
        Continue;
      end;

      ConfigObject.ApplyParam(JPlugin);
      // Install handler to fill UndoableMultiChangeTask
      JPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
      JPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
      JPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

      Node := vtvSubsList.GetFirstSelected;
      while Assigned(Node) do
      begin
        NodeData := vtvSubsList.GetNodeData(Node);
        SubRangeCurrent := NodeData.Range;
        i := WAVDisplayer.RangeList.IndexOf(NodeData.Range);

        if (i > 0) then
          SubRangePrevious := TSubtitleRange(WAVDisplayer.RangeList[i-1])
        else
          SubRangePrevious := nil;

        if (i < WAVDisplayer.RangeList.Count-1) then
          SubRangeNext := TSubtitleRange(WAVDisplayer.RangeList[i+1])
        else
          SubRangeNext := nil;

        ResultMsg := JPlugin.HasError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        if (ResultMsg <> '') then
        begin
          JPlugin.FixError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        end;
        Node := vtvSubsList.GetNextSelected(Node);
      end;

      FreeAndNil(JPlugin);
    end;
    JSPEnum.Free;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);

  if (UndoableMultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(UndoableMultiChangeTask);
    // Do not free the task, it's on the stack now
    UndoableMultiChangeTask := nil;
    CurrentProject.IsDirty := True;
  end
  else
  begin
    FreeAndNil(UndoableMultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.CanFixError(PluginFilename : WideString) : Boolean;
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
begin
  Result := False;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  JPlugin := JSPEnum.GetPluginByFilename(PluginFilename);
  if Assigned(JPlugin) then
  begin
    JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
    Result := Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = True) and
      JPlugin.CanFixError;
    FreeAndNil(JPlugin);
  end;
  JSPEnum.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var NodeData: PTreeData;
begin
  if (Node <> nil) then
  begin
    NodeData := Sender.GetNodeData(Node);
    MemoSubtitleText.Tag := 0;
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
    MemoSubtitleText.Text := NodeData.Range.Text;
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
    MemoSubtitleText.Tag := 1;
    if (WAVDisplayer.KaraokeSelectedRange = NodeData.Range) then
      TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex)
    else
      TagHighlight(MemoSubtitleText, -1);
    UpdateMemoVO(NodeData);
    // Update styles combobox
    UpdateStylesComboboxFromSelection;
    CallJSOnSelectedSubtitle;
  end
  else
  begin
    MemoSubtitleText.Tag := 0;
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
    MemoSubtitleText.Text := '';
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
    MemoSubtitleVO.Text := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSubtitleRangeJSWrapperChangeStart(Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : Integer);
var ChangeSubData : TChangeSubData;
begin
  if Assigned(UndoableMultiChangeTask) then
  begin
    ChangeSubData := UndoableMultiChangeTask.GetData(SubtitleRange.Node.Index);
    if not Assigned(ChangeSubData) then
    begin
      ChangeSubData := TChangeSubData.Create(SubtitleRange.Node.Index);
      UndoableMultiChangeTask.AddData(ChangeSubData);
    end;
    ChangeSubData.OldStart := SubtitleRange.StartTime;
    ChangeSubData.NewStart := NewValue;
    // Subtitle match current selection, update selection accordingly
    if (SubtitleRange = WavDisplayer.SelectedRange) then
    begin
      WavDisplayer.Selection.StartTime := NewValue;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSubtitleRangeJSWrapperChangeStop(Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : Integer);
var ChangeSubData : TChangeSubData;
begin
  if Assigned(UndoableMultiChangeTask) then
  begin
    ChangeSubData := UndoableMultiChangeTask.GetData(SubtitleRange.Node.Index);
    if not Assigned(ChangeSubData) then
    begin
      ChangeSubData := TChangeSubData.Create(SubtitleRange.Node.Index);
      UndoableMultiChangeTask.AddData(ChangeSubData);
    end;
    ChangeSubData.OldStop := SubtitleRange.StopTime;
    ChangeSubData.NewStop := NewValue;
    // Subtitle match current selection, update selection accordingly
    if (SubtitleRange = WavDisplayer.SelectedRange) then
    begin
      WavDisplayer.Selection.StopTime := NewValue;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSubtitleRangeJSWrapperChangeText(Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : WideString);
var ChangeSubData : TChangeSubData;
begin
  if Assigned(UndoableMultiChangeTask) then
  begin
    ChangeSubData := UndoableMultiChangeTask.GetData(SubtitleRange.Node.Index);
    if not Assigned(ChangeSubData) then
    begin
      ChangeSubData := TChangeSubData.Create(SubtitleRange.Node.Index);
      UndoableMultiChangeTask.AddData(ChangeSubData);
    end;
    ChangeSubData.OldText := SubtitleRange.Text;
    ChangeSubData.NewText := NewValue;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FixErrorInList(ErrorList : TList);
var NodeData : PErrorTreeData;
    JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    JSPluginMap : DHashMap;
    Iter : DIterator;
    SubRangeCurrent, SubRangePrevious, SubRangeNext : TSubtitleRange;
    i, j : Integer;
    Start, ExecTime : Cardinal;
    FixedCount : Cardinal;
    Msg : WideString;
begin
  Start := GetTickCount;
  FixedCount := 0;
  
  UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  JSPluginMap := DHashMap.Create;

  g_WebRWSynchro.BeginWrite;
  try
    for j := 0 to ErrorList.Count-1 do
    begin
      NodeData := ErrorList[j];
      Iter := JSPluginMap.locate([NodeData.Filename]);
      if atEnd(Iter) then
      begin
        JPlugin := JSPEnum.GetPluginByFilename(NodeData.Filename);
        if Assigned(JPlugin) then
        begin
          JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
          if Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = True) then
          begin
            ConfigObject.ApplyParam(JPlugin);
            // Install handler to fill UndoableMultiChangeTask
            JPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
            JPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
            JPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;
          end
          else
          begin
            FreeAndNil(JPlugin);
          end;
        end;
        JSPluginMap.putPair([NodeData.Filename, JPlugin]);
      end
      else
      begin
        JPlugin := TJavaScriptPlugin(getObject(Iter));
      end;

      if Assigned(JPlugin) then
      begin
        SubRangeCurrent := NodeData.Range;
        i := WAVDisplayer.RangeList.IndexOf(NodeData.Range);

        if (i > 0) then
          SubRangePrevious := TSubtitleRange(WAVDisplayer.RangeList[i-1])
        else
          SubRangePrevious := nil;

        if (i < WAVDisplayer.RangeList.Count-1) then
          SubRangeNext := TSubtitleRange(WAVDisplayer.RangeList[i+1])
        else
          SubRangeNext := nil;

        JPlugin.FixError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        Inc(FixedCount);
      end;
    end;
  finally
    g_WebRWSynchro.EndWrite;
    JSPEnum.Free;
    FreeAndClear(JSPluginMap);
  end;

  ExecTime := GetTickCount - Start;

  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);

  if (UndoableMultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(UndoableMultiChangeTask);
    // Do not free the task, it's on the stack now
    UndoableMultiChangeTask := nil;
    CurrentProject.IsDirty := True;
  end
  else
  begin
    FreeAndNil(UndoableMultiChangeTask);
  end;

  Msg := Format('%d error(s) fixed (in %d ms).', [FixedCount, ExecTime]);
  ErrorReportForm.TntStatusBar1.Panels[0].Text := Msg;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormActivate(Sender: TObject);
begin
  FormHasBeenActivated := True;
end;

//------------------------------------------------------------------------------
procedure TMainForm.MemoSubtitleTextMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Pt : TPoint;
    CharIndex, LineIndex : Integer;
begin
  if FormHasBeenActivated then
  begin
    Pt := Point(X, Y);
    CharIndex := MemoSubtitleText.Perform(Messages.EM_CHARFROMPOS, 0, Integer(@Pt));
    LineIndex := MemoSubtitleText.Perform(EM_LINEFROMCHAR, CharIndex, 0);
    MemoSubtitleText.SelStart := CharIndex + LineIndex;
    FormHasBeenActivated := False;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStartSubExecute(Sender: TObject);
begin
  if WAVDisplayer.IsPlaying then
  begin
    StartSubtitleTime := WAVDisplayer.GetPlayCursorPos;
  end
  else
    StartSubtitleTime := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStopSubExecute(Sender: TObject);
begin
  if WAVDisplayer.IsPlaying and (StartSubtitleTime <> -1) and
    (WAVDisplayer.GetPlayCursorPos > (StartSubtitleTime + 400)) then
  begin
    AddSubtitle(StartSubtitleTime, WAVDisplayer.GetPlayCursorPos, '', True, False);
    StartSubtitleTime := -1;
  end
  else
    StartSubtitleTime := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStopAndStartSubExecute(Sender: TObject);
begin
  if WAVDisplayer.IsPlaying and (StartSubtitleTime <> -1) and
    (WAVDisplayer.GetPlayCursorPos > (StartSubtitleTime + 400)) then
  begin
    AddSubtitle(StartSubtitleTime, WAVDisplayer.GetPlayCursorPos, '', True, False);
    StartSubtitleTime := WAVDisplayer.GetPlayCursorPos + 1;
  end
  else
    StartSubtitleTime := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // subtitle dynamic creation (toggle mode) in timing mode only
  if IsTimingMode and (Key = VK_SPACE) and
    (ConfigObject.EnableToggleCreation = True) and
    (WAVDisplayer.IsPlaying = True) then
  begin
    if (ToggleStartSubtitleTime = -1) then
    begin
      ToggleStartSubtitleTime := WAVDisplayer.GetPlayCursorPos;
      if ConfigObject.SpaceKeyModifyTiming then
      begin
        SetSubtitleStartTime(True);
      end;
    end;
    Key := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // subtitle dynamic creation (toggle mode) in timing mode only
  if IsTimingMode and (Key = VK_SPACE) and
    (ConfigObject.EnableToggleCreation = True) and
    (WAVDisplayer.IsPlaying = True) then
  begin
    if (ToggleStartSubtitleTime <> -1) then
    begin
      if ConfigObject.SpaceKeyModifyTiming then
      begin
        SetFocusedSubtitleStopTime(True, True);
      end
      else
      begin
        if (WAVDisplayer.GetPlayCursorPos > (ToggleStartSubtitleTime + 400)) then
        begin
          AddSubtitle(ToggleStartSubtitleTime, WAVDisplayer.GetPlayCursorPos, '', True, False);
        end;
      end;
      ToggleStartSubtitleTime := -1;
    end;
    Key := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1StartPlaying(Sender: TObject);
begin
  if (ConfigObject.AutoSaveWhenPlaying = True) then
    ActionSave.Execute;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionInsertKaraokeMarkerExecute(Sender: TObject);
var Idx : Integer;
    SubRange : TSubtitleRange;
begin
  Idx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if (Idx <> -1) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
    SubRange.AddSubTime(WAVDisplayer.GetCursorPos);
    vtvSubsList.Repaint;
    CurrentProject.IsDirty := True;
  end;
end;

//------------------------------------------------------------------------------

// Split on white space and on '/' that is used to separate sylables
procedure WhiteSpaceSplit(const Text : WideString; var WordArray : TWideStringDynArray);
var i, j : Integer;
begin
  SetLength(WordArray, 0);
  j := 1;
  for i:=1 to Length(Text) do
  begin
    if (Text[i] = ' ') or (Text[i] = #10) or (Text[i] = '/') then
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := Tnt_WideStringReplace(
        Copy(Text, j, i-j+1),
        '/',
        '',
        [rfReplaceAll]);
      j := (i + 1);
    end;
  end;
  if (Length(Text) > 0) then
  begin
    SetLength(WordArray, Length(WordArray)+1);
    WordArray[Length(WordArray)-1] := Copy(Text, j, Length(Text)-j+1);
  end;
end;

// -----

function KaraLen(const Text : WideString) : Integer;
var i : Integer;
begin
  Result := 0;
  for i:=1 to Length(Text) do
    if (Text[i] <> #13) and (Text[i] <> #10) then
      Inc(Result);
end;

// -----

procedure CreateKaraoke(SubRange : TSubtitleRange);
var WordArray : TWideStringDynArray;
    i, j, total : Integer;
    s : WideString;
    tpl : Double; // time per letter
begin
  tpl := (SubRange.StopTime - SubRange.StartTime) /
    KaraLen(StringReplace(SubRange.Text, '/', '', [rfReplaceAll]));
  WhiteSpaceSplit(SubRange.Text, WordArray);

  SubRange.ClearSubTimes;

  // Recreate string with karaoke timing
  s :='';
  total := 0;
  if Length(WordArray) > 0 then
  begin
    for i:=0 to Length(WordArray)-1 do
    begin
      j := Round(tpl * KaraLen(WordArray[i]) / 10);
      total := total + j;
      if (i < Length(WordArray)-1) then
        SubRange.AddSubTime(SubRange.StartTime + (total*10));
      s := s + WideFormat('{\k%d}%s', [j, WordArray[i]]);
    end;
  end;

  // total should match duration (but we work in ms :] ?)

  SubRange.Text := s;
end;

// -----

procedure TMainForm.pmiCreateKaraokeClick(Sender: TObject);
var Node : PVirtualNode;
    NodeData: PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      SubtitleRange := NodeData.Range;
      ChangeSubData := TChangeSubData.Create(Node.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.OldSubTime := SubtitleRange.SubTime;
      CreateKaraoke(SubtitleRange);
      ChangeSubData.NewText := SubtitleRange.Text;
      ChangeSubData.NewSubTime := SubtitleRange.SubTime;
      MultiChangeTask.AddData(ChangeSubData);
      Node := vtvSubsList.GetNextSelected(Node);
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure ClearKaraoke(SubRange : TSubtitleRange);
var WordArray : TWideStringDynArray;
    TimeArray : TIntegerDynArray;
    CleanedText : WideString;
    i : Integer;
begin
  CleanedText := '';
  KaraSplit(SubRange.Text, WordArray, TimeArray);
  for i:=0 to Length(WordArray)-1 do
  begin
    CleanedText := CleanedText + WordArray[i];
  end;
  SubRange.Text := CleanedText;
  SubRange.ClearSubTimes;
end;

// -----

procedure TMainForm.pmiClearKaraokeClick(Sender: TObject);
var
  Node : PVirtualNode;
  NodeData: PTreeData;
  MultiChangeTask : TUndoableMultiChangeTask;
  ChangeSubData : TChangeSubData;
  SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      SubtitleRange := NodeData.Range;
      ChangeSubData := TChangeSubData.Create(Node.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.OldSubTime := SubtitleRange.SubTime;
      ClearKaraoke(NodeData.Range);
      ChangeSubData.NewText := SubtitleRange.Text;
      ChangeSubData.NewSubTime := SubtitleRange.SubTime;
      MultiChangeTask.AddData(ChangeSubData);
      Node := vtvSubsList.GetNextSelected(Node);
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

function CalculateNewTextFromSubTime(SubRange : TSubtitleRange) : WideString;
var WordArray : TWideStringDynArray;
    TimeArray : TIntegerDynArray;
    i, AccuTime : Integer;
begin
  Result := SubRange.Text;
  if Length(SubRange.SubTime) <= 0 then
    Exit;
  KaraSplit2(SubRange.Text, WordArray, TimeArray);
  if (Length(SubRange.SubTime) <> (Length(TimeArray) - 1)) then
    Exit;
  AccuTime := SubRange.StartTime;
  for i := Low(SubRange.SubTime) to High(SubRange.SubTime) do
  begin
    TimeArray[i] := Round((SubRange.SubTime[i] - AccuTime) / 10);
    AccuTime := AccuTime + (TimeArray[i] * 10);
  end;
  TimeArray[High(TimeArray)] := Round((SubRange.StopTime - AccuTime) / 10);

  Result := WordArray[0];
  for i := Low(TimeArray) to High(TimeArray) do
  begin
    Result := Result + IntToStr(TimeArray[i]) + WordArray[i + 1];
  end;
end;

procedure TMainForm.WAVDisplayer1KaraokeChanged(Sender: TObject; Range : TRange;
  SubTimeIndex, OldTime : Integer);
var SubRange : TSubtitleRange;
    NewText : WideString;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
begin
  g_WebRWSynchro.BeginWrite;
  try
    SubRange := Range as TSubtitleRange;
    NewText := CalculateNewTextFromSubTime(SubRange);
    if (NewText <> SubRange.Text) then
    begin
      if (SubTimeIndex <> -1) then
      begin
        MultiChangeTask := TUndoableMultiChangeTask.Create;
        ChangeSubData := TChangeSubData.Create(SubRange.Node.Index);
        ChangeSubData.OldSubTime := SubRange.SubTime;
        ChangeSubData.OldSubTime[SubTimeIndex] := OldTime;
        ChangeSubData.OldText := SubRange.Text;
        ChangeSubData.NewSubTime := SubRange.SubTime;
        ChangeSubData.NewText := NewText;
        MultiChangeTask.AddData(ChangeSubData);
        PushUndoableTask(MultiChangeTask);
      end;

      SubRange.Text := NewText;
      CurrentProject.IsDirty := True;
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowStartFrameExecute(Sender: TObject);
var NodeData: PTreeData;
    Fps : Double;
    FrameNumber : Double;
    FrameLen : Double;
    StartTimeFrame : Integer;
begin
  if Assigned(VideoRenderer) and Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

    StartTimeFrame := NodeData.Range.StartTime;

    // Get the video fps from VSFilter
    Fps := VideoRenderer.GetVSFilterFPS;
    if (FPS > 0) then
    begin
      // Calculate the frame duration in s
      FrameLen := (1000.0 / Fps);
      // Calculate the fame number
      FrameNumber := Ceil(StartTimeFrame / FrameLen);
      // DirectShow add a 1 frame delay ???
      FrameNumber := FrameNumber + 1;
      // Convert framenumber back in ms
      StartTimeFrame := Ceil(FrameNumber * FrameLen);
    end;

    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    // Finally show the image
    VideoRenderer.ShowImageAt(StartTimeFrame);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowStopFrameExecute(Sender: TObject);
var NodeData: PTreeData;
    Fps : Double;
    FrameNumber : Double;
    FrameLen : Double;
    StopTimeFrame : Integer;
begin
  if Assigned(VideoRenderer) and Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

    StopTimeFrame := NodeData.Range.StopTime;

    // Get the video fps from VSFilter
    Fps := VideoRenderer.GetVSFilterFPS;
    if (FPS > 0) then
    begin
      // Calculate the frame duration in s
      FrameLen := (1000.0 / Fps);
      // Calculate the fame number
      FrameNumber := Ceil(StopTimeFrame / FrameLen);
      // Don't add the one frame delay we want to see the last frame with subtitle
      // FrameNumber := FrameNumber + 1;
      // Convert framenumber back in ms
      StopTimeFrame := Ceil(FrameNumber * FrameLen);
    end;

    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    // Finally show the image
    VideoRenderer.ShowImageAt(StopTimeFrame);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowFrameAtCursorExecute(Sender: TObject);
begin
  if Assigned(VideoRenderer) then
  begin
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    VideoRenderer.ShowImageAt(WAVDisplayer.GetCursorPos);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.DetectCharsetDataLoss : Boolean;
var i : integer;
    Subrange : TSubtitleRange;
    ansiCharString : string;
    backWideCharString : WideString;
begin
  Result := False;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    ansiCharString := WC2MB(SubRange.Text);
    backWideCharString := MB2WC(ansiCharString);
    if (backWideCharString <> SubRange.Text) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsSRT(Filename, PreviousExt: WideString; InUTF8 : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    ConvertFunc : function(Src : WideString) : WideString;
    SubText : WideString;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;

    function IsSelected(StartTime, StopTime : Integer): Boolean;
    var Cursor : PVirtualNode; NodeData : PTreeData; Selected : Boolean;
    begin
      Selected := False;
      Cursor := vtvSubsList.GetFirstSelected;
      while (Cursor <> nil) do
        begin
          NodeData := vtvSubsList.GetNodeData(Cursor);
          if (NodeData.Range.StartTime = StartTime) And (NodeData.Range.StopTime = StopTime) then
            begin
              Selected := True;
              Break;
            end;
          Cursor := vtvSubsList.GetNextSelected(Cursor);
        end;

        Result := Selected;
    end;

begin
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  if (PreviousExt = '.ass') or (PreviousExt = '.ssa') then
    ConvertFunc := @ConvertSSAToSRT
  else
    ConvertFunc := @ConvertNull;

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);

    if ExcludeUnselected = True then
      if Not IsSelected(SubRange.StartTime, SubRange.StopTime) then Continue;

    WriteStringLnStream(IntToStr(i+1), FS);
    WriteStringLnStream(TimeMsToString(SubRange.StartTime,',') + ' --> ' +
      TimeMsToString(SubRange.StopTime,','), FS);
    SubText := Translator.Translate(Subrange.Text);
    SubText := ConvertFunc(SubText);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(SubText), FS)
    else
      WriteStringLnStream(WC2MB(SubText), FS);
    WriteStringLnStream('', FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsSSA(Filename, PreviousExt: WideString; InUTF8 : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    s, SubText : WideString;
    style : TSSAStyle;
    ConvertFunc : function(Src : WideString) : WideString;
    
    procedure WriteStringLnStream(str : string; Stream : TStream);
    begin
      str := str + #13#10;
      Stream.Write(str[1], Length(str));
    end;

begin
  // Write BOM if any
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  if (PreviousExt = '.srt') then
    ConvertFunc := @ConvertSRTToSSA
  else
    ConvertFunc := @ConvertNull;

  // Write Header
  WriteStringLnStream('[Script Info]', FS);
  WriteStringLnStream('; Written by VisualSubSync ' + g_ApplicationVersion.VersionString, FS);
  if (SubtitleFileHeader = '') then
  begin
    WriteStringLnStream('Title: <untitled>', FS);
    WriteStringLnStream('Original Script: <unknown>', FS);
    WriteStringLnStream('ScriptType: v4.00', FS);
    if VideoRenderer.IsOpen and (VideoRenderer.VideoWidth > 0) and (VideoRenderer.VideoHeight > 0) then
    begin
      WriteStringLnStream(Format('PlayResX: %d',[VideoRenderer.VideoWidth]), FS);
      WriteStringLnStream(Format('PlayResY: %d',[VideoRenderer.VideoHeight]), FS);
    end;
    WriteStringLnStream('PlayDepth: 0', FS);
    WriteStringLnStream('Timer: 100.0', FS);
  end
  else
  begin
    // Make sure to write "ScriptType: v4.00"
    s := ReplaceRegExpr('ScriptType: v4\.00\+?', SubtitleFileHeader,
      'ScriptType: v4.00');
    s := Trim(s);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(s), FS)
    else
      WriteStringLnStream(WC2MB(s), FS);
  end;

  // Write Styles
  WriteStringLnStream('', FS);
  WriteStringLnStream('[v4 Styles]', FS);
  WriteStringLnStream('Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding', FS);
  for i:=0 to StyleFormInstance.GetCount-1 do
  begin
    style := StyleFormInstance.GetStyleAt(i);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(style.getAsSSA), FS)
    else
      WriteStringLnStream(WC2MB(style.getAsSSA), FS);
  end;

  WriteStringLnStream('', FS);
  WriteStringLnStream('[Events]', FS);
  WriteStringLnStream('Format: Marked, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text', FS);

  //Dialogue: Marked=0,0:00:05.00,0:00:07.00,*Default,,0000,0000,0000,,toto tata

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    s := Format('Dialogue: %s,%s,%s,%s,%s,%s,%s,%s,%s,',
      [SubRange.Marked, TimeMsToSSAString(SubRange.StartTime),
       TimeMsToSSAString(SubRange.StopTime), SubRange.Style, SubRange.Actor,
       Subrange.LeftMarg, Subrange.RightMarg, SubRange.VertMarg, Subrange.Effect]);

    SubText := Translator.Translate(Subrange.Text);
    SubText := ConvertFunc(SubText);

    s := s + Tnt_WideStringReplace(SubText, CRLF, '\N', [rfReplaceAll]);

    if InUTF8 then
      WriteStringLnStream(UTF8Encode(s), FS)
    else
      WriteStringLnStream(WC2MB(s), FS);
  end;

  if (SubtitleFileFooter <> '') then
  begin
    WriteStringLnStream('', FS);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(Trim(SubtitleFileFooter)), FS)
    else
      WriteStringLnStream(WC2MB(Trim(SubtitleFileFooter)), FS);
  end;
  
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsASS(Filename, PreviousExt: WideString; InUTF8 : Boolean; Translator : TTranslator; ExcludeUnselected : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    s, SubText : WideString;
    style : TSSAStyle;
    ConvertFunc : function(Src : WideString) : WideString;

    procedure WriteStringLnStream(str : string; Stream : TStream);
    begin
      str := str + #13#10;
      Stream.Write(str[1], Length(str));
    end;
begin
  // Write BOM if any
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  if (PreviousExt = '.srt') then
    ConvertFunc := @ConvertSRTToSSA
  else
    ConvertFunc := @ConvertNull;

  // Write Header
  WriteStringLnStream('[Script Info]', FS);
  WriteStringLnStream('; Written by VisualSubSync ' + g_ApplicationVersion.VersionString, FS);
  if (SubtitleFileHeader = '') then
  begin
    WriteStringLnStream('Title: <untitled>', FS);
    WriteStringLnStream('Original Script: <unknown>', FS);
    WriteStringLnStream('ScriptType: v4.00+', FS);
    if VideoRenderer.IsOpen and (VideoRenderer.VideoWidth > 0) and (VideoRenderer.VideoHeight > 0) then
    begin
      WriteStringLnStream(Format('PlayResX: %d',[VideoRenderer.VideoWidth]), FS);
      WriteStringLnStream(Format('PlayResY: %d',[VideoRenderer.VideoHeight]), FS);
    end;
    WriteStringLnStream('PlayDepth: 0', FS);
    WriteStringLnStream('Timer: 100.0', FS);
    WriteStringLnStream('WrapStyle: 0', FS);
  end
  else
  begin
    // Make sure to write "ScriptType: v4.00+"
    s := ReplaceRegExpr('ScriptType: v4\.00\+?', SubtitleFileHeader,
      'ScriptType: v4.00+');
    s := Trim(s);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(s), FS)
    else
      WriteStringLnStream(WC2MB(s), FS);
  end;

  // Write Styles
  WriteStringLnStream('', FS);
  WriteStringLnStream('[v4+ Styles]', FS);
  WriteStringLnStream('Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic,  Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding', FS);
  for i:=0 to StyleFormInstance.GetCount-1 do
  begin
    style := StyleFormInstance.GetStyleAt(i);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(style.getAsASS), FS)
    else
      WriteStringLnStream(WC2MB(style.getAsASS), FS);
  end;

  WriteStringLnStream('', FS);
  WriteStringLnStream('[Events]', FS);
  WriteStringLnStream('Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text', FS);

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    s := Format('Dialogue: %s,%s,%s,%s,%s,%s,%s,%s,%s,',
      [SubRange.Layer, TimeMsToSSAString(SubRange.StartTime),
       TimeMsToSSAString(SubRange.StopTime), SubRange.Style,SubRange.Actor,
       Subrange.LeftMarg, Subrange.RightMarg, SubRange.VertMarg, Subrange.Effect]);

    SubText := Translator.Translate(Subrange.Text);
    SubText := ConvertFunc(SubText);

    s := s + Tnt_WideStringReplace(SubText, CRLF, '\N', [rfReplaceAll]);
    
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(s), FS)
    else
      WriteStringLnStream(WC2MB(s), FS);
  end;
  
  if (SubtitleFileFooter <> '') then
  begin
    WriteStringLnStream('', FS);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(Trim(SubtitleFileFooter)), FS)
    else
      WriteStringLnStream(WC2MB(Trim(SubtitleFileFooter)), FS);
  end;

  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedKaraokeRange(Sender: TObject;
  Range : TRange);
var Node : PVirtualNode;
begin
  if Assigned(Range) then
  begin
    Node := TSubtitleRange(Range).Node;
    vtvSubsList.ScrollIntoView(Node, True);
    vtvSubsList.FocusedNode := Node;
    vtvSubsList.ClearSelection;
    vtvSubsList.Selected[Node] := True;
  end;
  TagHighlight(MemoSubtitleText,WAVDisplayer.KaraokeSelectedIndex);
end;

//------------------------------------------------------------------------------

type
  TAccessCanvas = class(TCanvas);

procedure WideCanvasDrawText(Canvas: TCanvas; Rect: TRect;
  const Text: WideString; AllowLineBreak : Boolean = False; AlignBottom : Boolean = True);
var
  Options: Cardinal;
  OriginalRectBottom, OriginalRectRight, BottomDiff : Integer;
begin
  with TAccessCanvas(Canvas) do begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := DT_END_ELLIPSIS or DT_NOPREFIX or DT_EDITCONTROL;
    if AllowLineBreak then
      Options := Options or DT_WORDBREAK;
    if AlignBottom then
    begin
      OriginalRectBottom := Rect.Bottom;
      OriginalRectRight := Rect.Right;
      Windows.DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Options or DT_CALCRECT);
      BottomDiff := OriginalRectBottom - Rect.Bottom;
      if (BottomDiff > 0) then
        OffsetRect(Rect, 0, BottomDiff)
      else
        Rect.Bottom := OriginalRectBottom;
      Rect.Right := OriginalRectRight;
    end;
    Windows.DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Options);
    Changed;
  end;
end;

procedure TMainForm.WAVDisplayer1CustomDrawRange(Sender: TObject;
  ACanvas: TCanvas; Range : TRange; Rect : TRect);
//var RectShadow : TRect;
const MINIMAL_SPACE : Integer = 25;
      TEXT_MARGINS : Integer = 5;
var WAVZoneHeight : Integer;
    AlignBottom : Boolean;
begin
  if (not ConfigObject.ShowTextInWAVDisplay) then
    Exit;
  if not ConfigObject.ShowSpecialTagSubs AND
     (copy(TSubtitleRange(Range).Text,1,1) = '{') And (pos('}',TSubtitleRange(Range).Text) > -1) then
   Exit;
  if Length(Range.SubTime) > 0 then
  begin
    // TODO : karaoke support
  end
  else
  begin
    InflateRect(Rect, -TEXT_MARGINS, -TEXT_MARGINS);
    if (Rect.Right - Rect.Left) > MINIMAL_SPACE then
    begin
      ACanvas.Font.Assign(MemoSubtitleText.Font);

      WAVZoneHeight := WAVDisplayer.Height - WAVDisplayer.RulerHeight;
      AlignBottom := (Rect.Top > WAVZoneHeight div 2);

      {RectShadow.Top := Rect.Top + 1;
      RectShadow.Bottom := Rect.Bottom + 1;
      RectShadow.Left := Rect.Left + 1;
      RectShadow.Right := Rect.Right + 1;
      ACanvas.Font.Color := ChangeColorLuminance(ACanvas.Pen.Color, 0.25);
      WideCanvasDrawText(ACanvas, RectShadow, TSubtitleRange(Range).Text, False, AlignBottom);}

      ACanvas.Font.Color := ACanvas.Pen.Color;
      WideCanvasDrawText(ACanvas, Rect, TSubtitleRange(Range).Text, False, AlignBottom);

    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionExportToWAVExecute(Sender: TObject);
var
  SelectedDir: string;
  WAVFile : TWAVFile;
  Node : PVirtualNode;
  NodeData: PTreeData;
  Filename : string;
  FS : TFileStream;
  PP : TPageProcessor;
  DummyEnvVars : TStringList;
  CM : ICursorManager;
  //SelectedSubList : TRangeList;
begin
  if Trim(CurrentProject.WAVFile) = '' then
  begin
    MessageBoxW(Handle,
      PWideChar(WideString('You need to create a project with a WAV file')),
      PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
  end
  else
  begin
    if SelectDirectory('Select the folder where to place wav files', '', SelectedDir) = True then
    begin
      SelectedDir := IncludeTrailingPathDelimiter(SelectedDir);
      WAVFile := TWAVFile.Create;
      if WAVFile.Open(CurrentProject.WAVFile) then
      begin
        CM := TCursorManager.Create(crHourGlass);
        ShowStatusBarMessage('Exporting to wav files, please wait...');
        {
        if (vtvSubsList.SelectedCount > 1) then
        begin
          SelectedSubList := TRangeList.Create;
          Node := vtvSubsList.GetFirstSelected
        end else begin
          SelectedSubList := nil;}
          Node := vtvSubsList.GetFirst;{
        end;
        }
        
        while (Node <> nil) do
        begin
          NodeData := vtvSubsList.GetNodeData(Node);
          Filename := Format('%s%6.6d.wav', [SelectedDir, Node.Index+1]);
          FS := TFileStream.Create(Filename, fmCreate);
          WAVFile.ExtractToStream(NodeData.Range.StartTime,
            NodeData.Range.StopTime, FS);
          FS.Free;
          {if (vtvSubsList.SelectedCount > 1) then
          begin
            SelectedSubList.Add(NodeData.Range);
            Node := vtvSubsList.GetNextSelected(Node);
          end else begin}
            Node := vtvSubsList.GetNext(Node);
          {end;}
        end;

        {if (vtvSubsList.SelectedCount > 1) then
        begin
          g_GlobalContext.SubList := SelectedSubList;
        end;}

        PP := TPageProcessor.Create;
        DummyEnvVars := TStringList.Create;
        FS := TFileStream.Create(SelectedDir + 'index_wav.html', fmCreate);
        PP.ProcessPage(ServerRootDir + 'index_wav_export.shtml', FS, DummyEnvVars);
        PP.Free;
        FS.Free;
        DummyEnvVars.Free;

        {if (vtvSubsList.SelectedCount > 1) then
        begin
          // Restore complete sub list
          g_GlobalContext.SubList := WAVDisplayer.RangeList;
          SelectedSubList.Free;
        end;}
      end;
      WAVFile.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlaySelStartExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionStart;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection.StartTime,
      Min(WAVDisplayer.Selection.StartTime + StartEndPlayingDuration, WAVDisplayer.Selection.StopTime));
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlaySelEndExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionEnd;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
      WAVDisplayer.Selection.StopTime);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ActionPlaceKaraokeCursorsAtEndExecute(Sender: TObject);
var NodeData: PTreeData;
    SubRange : TSubtitleRange;
    i, CompressedStart : Integer;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
const CompressedDuration : Integer = 100;
begin
  // Compress karaoke cursors at end of subtitle
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    SubRange := NodeData.Range;
    CompressedStart := SubRange.StopTime - (Length(SubRange.SubTime) * CompressedDuration);
    if (CompressedStart > SubRange.StartTime) then
    begin
      MultiChangeTask := TUndoableMultiChangeTask.Create;
      ChangeSubData := TChangeSubData.Create(vtvSubsList.FocusedNode.Index);
      MultiChangeTask.AddData(ChangeSubData);
      ChangeSubData.OldText := SubRange.Text;
      ChangeSubData.OldSubTime := SubRange.SubTime;
      try
        g_WebRWSynchro.BeginWrite;
        for i := Low(SubRange.SubTime) to High(SubRange.SubTime) do
        begin
          SubRange.SubTime[i] := CompressedStart + (CompressedDuration * i);
        end;
        SubRange.Text := CalculateNewTextFromSubTime(SubRange);
      finally
        g_WebRWSynchro.EndWrite;
      end;
      if (ChangeSubData.OldText <> SubRange.Text) then
      begin
        ChangeSubData.NewText := SubRange.Text;
        ChangeSubData.NewSubTime := SubRange.SubTime;
        PushUndoableTask(MultiChangeTask);
        CurrentProject.IsDirty := True;
        WAVDisplayer.UpdateView([uvfRange]);
        vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
        vtvSubsList.Repaint;
      end
      else
      begin
        FreeAndNil(MultiChangeTask);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlayToEndExecute(Sender: TObject);
begin
  if IsTimingMode then
    PanelWAVDisplay.SetFocus;
  WAVDisplayer.AutoScrolling := True;
  if WAVDisplayer.SelectionIsEmpty then
  begin
    PlayingMode := pmtAll;
    WAVDisplayer.PlayRange(WAVDisplayer.GetCursorPos,WAVDisplayer.Length)
  end
  else
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection.StartTime,WAVDisplayer.Length);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlay1sBeforeExecute(Sender: TObject);
var NodeData: PTreeData;
begin
  if (not WAVDisplayer.SelectionIsEmpty) then
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime - 1000, 0),
      WAVDisplayer.Selection.StopTime);
  end
  else if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(NodeData.Range.StartTime - 1000, 0),
      NodeData.Range.StopTime)
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlay1sBeforeToEndExecute(Sender: TObject);
var NodeData: PTreeData;
begin
  if (not WAVDisplayer.SelectionIsEmpty) then
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime - 1000, 0),
      WAVDisplayer.Length);
  end
  else if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(NodeData.Range.StartTime - 1000, 0),
      WAVDisplayer.Length)
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSelectNextSubExecute(Sender: TObject);
begin
  SelectNextSub;
  if MemoSubtitleText.Focused then
    MemoSubtitleText.SelectAll;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSelectPreviousSubExecute(Sender: TObject);
begin
  SelectPreviousSub;
  if MemoSubtitleText.Focused then
    MemoSubtitleText.SelectAll;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsCUE(Filename: WideString; ExcludeUnselected : Boolean);
var i : integer;
    SubRange : TSubtitleRange;
    FS : TTntFileStream;
    SubText : WideString;

    procedure WriteStringLnStream(str : string; Stream : TStream);
    begin
      str := str + #13#10;
      Stream.Write(str[1], Length(str));
    end;
begin
  FS := TTntFileStream.Create(Filename, fmCreate);

  WriteStringLnStream('FILE "YourFileHere.wav" WAVE', FS);
  WriteStringLnStream('  PERFORMER "Performer Here"',FS);
  WriteStringLnStream('  TRACK 01 AUDIO', FS);
  WriteStringLnStream('    INDEX 01 00:00:00', FS);

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    SubText := Subrange.Text;
    // Remove line breaks
    SubText := Tnt_WideStringReplace(SubText, ' ' + CRLF + ' ', ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, ' ' + CRLF, ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, CRLF + ' ', ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, CRLF, ' ', [rfReplaceAll]);

    WriteStringLnStream(Format('  TRACK %2.2d AUDIO', [i*2+2]), FS);
    WriteStringLnStream(Format('    INDEX 01 %s', [TimeMsToCUE(SubRange.StartTime)]), FS);
    WriteStringLnStream(Format('    TITLE "%s"', [SubText]), FS);
    
    WriteStringLnStream(Format('  TRACK %2.2d AUDIO', [i*2+3]), FS);
    WriteStringLnStream(Format('    INDEX 01 %s', [TimeMsToCUE(SubRange.StopTime)]), FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PanelVideoResize(Sender: TObject);
begin
  if Assigned(CurrentProject) and PanelVideo.Enabled and
    (CurrentProject.VideoPanelWidth <> PanelVideo.Width)
    then
  begin
    CurrentProject.VideoPanelWidth := PanelVideo.Width;
    CurrentProject.VideoPanelHeight := PanelVideo.Height;
    SaveProject(CurrentProject, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PanelVideoDblClick(Sender: TObject);
begin
  ActionDetachVideo.Execute;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetSubtitleStartTime(SetStopTime : Boolean);
var NodeData : PTreeData;
    NewStartTime : Integer;
    DurationTarget : Integer;
    PreviousNode : PVirtualNode;
    PreviousNodeData : PTreeData;
    AutofixOverlapping : Boolean;
    MinDurationBetweenSub : Integer;
begin
  if not Assigned(vtvSubsList.FocusedNode) then
    Exit;

  try
    g_WebRWSynchro.BeginWrite;
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    if WAVDisplayer.IsPlaying then
      NewStartTime := WAVDisplayer.GetPlayCursorPos
    else
      NewStartTime := WAVDisplayer.GetCursorPos;

    // Autofix overlapping and ensure blank between subtitles
    PreviousNode := vtvSubsList.GetPrevious(vtvSubsList.FocusedNode);
    AutofixOverlapping := (ConfigObject.SpaceKeyBlankBetweenSubtitles > 0);
    if AutofixOverlapping and Assigned(PreviousNode) then
    begin
      PreviousNodeData := vtvSubsList.GetNodeData(PreviousNode);
      if (PreviousNodeData.Range.StopTime >= NewStartTime) or
         ((NewStartTime - PreviousNodeData.Range.StopTime) < ConfigObject.SpaceKeyBlankBetweenSubtitles) then
      begin
        if (PreviousNodeData.Range.StopTime >= NewStartTime) then
        begin
          MinDurationBetweenSub := ConfigObject.SpaceKeyBlankBetweenSubtitles;
          PreviousNodeData.Range.StopTime := NewStartTime - (MinDurationBetweenSub div 2);
          NewStartTime := PreviousNodeData.Range.StopTime + MinDurationBetweenSub;
        end
        else
        begin
          MinDurationBetweenSub := ConfigObject.SpaceKeyBlankBetweenSubtitles -
            (NewStartTime - PreviousNodeData.Range.StopTime);
          PreviousNodeData.Range.StopTime := PreviousNodeData.Range.StopTime - (MinDurationBetweenSub div 2);
          NewStartTime := PreviousNodeData.Range.StopTime + ConfigObject.SpaceKeyBlankBetweenSubtitles;
        end;
      end;
    end;

    NodeData.Range.StartTime := NewStartTime;
    // Fix stop time to be superior to start time
    if (NewStartTime > NodeData.Range.StopTime) then
    begin
      NodeData.Range.StopTime := NewStartTime + 100;
    end;

    if (SetStopTime) then
    begin
      // Set stop time based on CPS target
      DurationTarget := Round((Length(StripTags(NodeData.Range.Text)) / ConfigObject.SpaceKeyCPSTarget) * 1000);
      NodeData.Range.StopTime := NewStartTime + Max(DurationTarget,
        ConfigObject.SpaceKeyMinimalDuration);
    end;
    FullSortTreeAndSubList; // lazy me :) but well it's fast enough
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleStartTimeExecute(Sender: TObject);
begin
  SetSubtitleStartTime(False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetFocusedSubtitleStopTime(Advance: Boolean; KeepStopIfSuperiorToCPSTarget : Boolean);
var NodeData : PTreeData;
    NewStopTime, NewStopCPS, NewDuration : Integer;
    UpdateStop : Boolean;
begin
  if not Assigned(vtvSubsList.FocusedNode) then
  begin
    Exit;
  end;

  NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

  if WAVDisplayer.IsPlaying then
  begin
    NewStopTime := WAVDisplayer.GetPlayCursorPos;
    // When the system is slow the play cursor can hang for a while and NewStopTime can be
    // strictly equal to the previous StartTime.
    // The method would normally abort in this case (NewDuration <= 0) but we don't want that in timing mode.
    if (NewStopTime = NodeData.Range.StartTime) then
    begin
      Inc(NewStopTime);
    end;
  end
  else
    NewStopTime := WAVDisplayer.GetCursorPos;

  NewDuration := NewStopTime - NodeData.Range.StartTime;

  if (NewDuration <= 0) then
  begin
    Exit;
  end;

  UpdateStop := True;
  if KeepStopIfSuperiorToCPSTarget then
  begin
    NewStopCPS := Round(Length(StripTags(NodeData.Range.Text)) / NewDuration * 1000.0);
    if (NewStopCPS > ConfigObject.SpaceKeyCPSTarget) or
      (NewDuration <= ConfigObject.SpaceKeyMinimalDuration) then
    begin
      UpdateStop := False;
    end;
  end;

  if UpdateStop then
  begin
    try
      g_WebRWSynchro.BeginWrite;
      NodeData.Range.StopTime := NewStopTime;
      CurrentProject.IsDirty := True;
    finally
      g_WebRWSynchro.EndWrite;
    end;
  end;

  if Advance then
  begin
    AdvanceToNextSubtitleAfterFocus;
  end;
  
  UpdateDurationWithSubOnly;
  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleStopTimeExecute(Sender: TObject);
begin
  SetFocusedSubtitleStopTime(False, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.AdvanceToNextSubtitleAfterFocus;
var
  NextNodeToFocus : PVirtualNode;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NextNodeToFocus := vtvSubsList.GetNext(vtvSubsList.FocusedNode);
    if Assigned(NextNodeToFocus) then
    begin
      vtvSubsList.FocusedNode := NextNodeToFocus;
      vtvSubsList.ClearSelection;
      vtvSubsList.Selected[NextNodeToFocus] := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleStopTimeAndGoToNextExecute(Sender: TObject);
begin
  SetFocusedSubtitleStopTime(True, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.cbStylesSelect(Sender: TObject);
var
  Node : PVirtualNode;
  NodeData : PTreeData;
  NewStyle : WideString;
  SubChanged : Boolean;
begin
  if cbStyles.ItemIndex = -1 then
    Exit;
  SubChanged := False;
  try
    g_WebRWSynchro.BeginWrite;
    NewStyle := cbStyles.Items.Strings[cbStyles.ItemIndex];
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      if (NodeData.Range.Style <> NewStyle) then
      begin
        NodeData.Range.Style := NewStyle;
        SubChanged := True;
      end;
      Node := vtvSubsList.GetNextSelected(node);
    end;
    if SubChanged then
    begin
      CurrentProject.IsDirty := True;
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  if SubChanged then
  begin
    vtvSubsList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateStylesComboBox;
var i : Integer;
    style : TSSAStyle;
begin
  cbStyles.Clear;
  for i := 0 to StyleFormInstance.GetCount-1 do
  begin
    style := StyleFormInstance.GetStyleAt(i);
    cbStyles.AddItem(style.name, nil);
  end;
  UpdateStylesComboboxFromSelection;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStylesExecute(Sender: TObject);
begin
  // Try to pre sselect current subtitle style
  StyleFormInstance.PreSelect(cbStyles.Text);
  StyleFormInstance.ShowModal;
  if StyleFormInstance.HaveStylesChanged then
  begin
    UpdateStylesComboBox;
    CurrentProjectSetDirty;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateStylesComboboxFromSelection;
var NodeCursor : PVirtualNode;
    NodeCursorData, PreviousNodeData : PTreeData;
    SeveralStylesSelected : Boolean;
begin
  SeveralStylesSelected := False;
  PreviousNodeData := nil;
  NodeCursor := vtvSubsList.GetFirstSelected;
  while Assigned(NodeCursor) do
  begin
    NodeCursorData := vtvSubsList.GetNodeData(NodeCursor);
    if (PreviousNodeData <> nil) and (NodeCursorData.Range.Style <> PreviousNodeData.Range.Style) then
    begin
      SeveralStylesSelected := True;
      Break;
    end;
    PreviousNodeData := NodeCursorData;
    NodeCursor := vtvSubsList.GetNextSelected(NodeCursor);
  end;

  if SeveralStylesSelected then
    cbStyles.ItemIndex := -1
  else
  begin
    NodeCursor := vtvSubsList.GetFirstSelected;
    if Assigned(NodeCursor) then
    begin
      NodeCursorData := vtvSubsList.GetNodeData(NodeCursor);
      cbStyles.ItemIndex := cbStyles.Items.IndexOf(NodeCursorData.Range.Style);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.RenameStyle(OldName, NewName : WideString);
var Node : PVirtualNode;
    NodeData : PTreeData;
    SubChanged : Boolean;
begin
  SubChanged := False;
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirst;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      if (NodeData.Range.Style = OldName) then
      begin
        NodeData.Range.Style := NewName;
        SubChanged := True;
      end;
      Node := vtvSubsList.GetNext(Node);
    end;
    if SubChanged then
    begin
      CurrentProject.IsDirty := True;
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  if SubChanged then
  begin
    vtvSubsList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPauseExecute(Sender: TObject);
begin
  if ConfigObject.ForceStopOnPausing then
   begin
    if WAVDisplayer.IsPlaying AND (LastCursorPositionOnPause = 0) then
    begin
      LastCursorPositionOnPause := WavDisplayer.GetPlayCursorPos;
      WAVDisplayer.Pause;
    end
    else
    begin
     WAVDisplayer.Stop;
     ActionPlay.Execute;
     LastCursorPositionOnPause := 0;
    end;
   end
  else
   begin
    if WAVDisplayer.IsPlaying then
     begin
      WAVDisplayer.Pause;
     end
    else
    begin
      ActionPlay.Execute;
    end;
   end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionToggleTimingModeExecute(Sender: TObject);
begin
  if IsNormalMode then
  begin
    bttWorkingMode.Caption := 'Timing';
    bttWorkingMode.Font.Color := clRed;
    bttWorkingMode.Tag := 1;
    if ConfigObject.MouseEnableSSATimingMode then
      WAVDisplayer.SelMode := smSSA
    else
      WAVDisplayer.SelMode := smCoolEdit;
  end
  else
  begin
    bttWorkingMode.Caption := 'Normal';
    bttWorkingMode.Tag := 0;
    bttWorkingMode.Font.Color := clWindowText;
    WAVDisplayer.SelMode := smCoolEdit;
  end;
  KeyPreview := IsTimingMode;
  SetShortcut(IsTimingMode);
  MemoSubtitleText.Enabled := MemoLinesCounter.Enabled and
    ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
  MemoSubtitleVO.Enabled := MemoLinesCounter.Enabled and
    ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateVolume;
begin
  AudioOnlyRenderer.SetVolume(tbVolume.Position);
  VideoRenderer.SetVolume(tbVolume.Position);
end;

//------------------------------------------------------------------------------

procedure TMainForm.tbVolumeChange(Sender: TObject);
begin
  UpdateVolume;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetEditorFocusExecute(Sender: TObject);
begin
  if MemoSubtitleText.Enabled then
    MemoSubtitleText.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetEditorFocusAndSelectExecute(Sender: TObject);
begin
  if MemoSubtitleText.Enabled then
    MemoSubtitleText.SetFocus;
  MemoSubtitleText.SelectAll;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateSubtitleForPreview(ForceUpdate : Boolean);
var TmpDstFilename : WideString;
    StartTime : Cardinal;
begin
  // TODO : Clean old files in temp directory
  
  if DisableVideoUpdatePreview
     or (Trim(CurrentProject.SubtitlesFile) = '')
     or (not VideoRenderer.IsOpen) then
  begin
    Exit;
  end;

  // Save current file as tmp file, reload subtitle if loaded
  StartTime := GetTickCount;
  TmpDstFilename := GetTemporaryFolder;
  if (Length(TmpDstFilename) > 0) then
  begin
    TmpDstFilename := TmpDstFilename + WideIncludeTrailingBackslash(RootAppData) ;
    // Create directory
    if not WideDirectoryExists(TmpDstFilename) then
      WideCreateDir(TmpDstFilename);
    TmpDstFilename := TmpDstFilename + WideExtractFileName(CurrentProject.SubtitlesFile);
  end
  else
  begin
    TmpDstFilename := WideExtractFilePath(CurrentProject.SubtitlesFile) + '~' +
      WideExtractFileName(CurrentProject.SubtitlesFile);
  end;
  if ShowingVideo then
  begin
    if (VideoRenderer.IsPlaying or VideoRenderer.IsPaused or ForceUpdate) then
    begin
      SaveSubtitles(TmpDstFilename, '', CurrentProject.IsUTF8, True, nil, False);
      VideoRenderer.SetSubtitleFilename(TmpDstFilename);
      if VideoRenderer.IsPaused then
      begin
        VideoRenderer.UpdateImage;
      end;
      VideoPreviewNeedSubtitleUpdate := False;
      //LogForm.SilentLogMsg('Save took ' + IntToStr(GetTickCount - StartTime) +' ms');
    end
  end
  else
  begin
    VideoPreviewNeedSubtitleUpdate := True;
  end
end;

//------------------------------------------------------------------------------

procedure TMainForm.CurrentProjectOnDirtySet(Sender: TObject);
begin
  UpdateSubtitleForPreview(False);
  VideoPreviewNeedSubtitleUpdate := True;
  CallJSOnSubtitleModification;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CurrentProjectSetDirty;

begin
  g_WebRWSynchro.BeginWrite;
  try
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNextErrorExecute(Sender: TObject);
begin
  ErrorReportForm.GoToNextError;
end;

//------------------------------------------------------------------------------

function TMainForm.GetVideoRendererFiltersList(list : TStrings) : Boolean;
begin
  Result := VideoRenderer.GetFilters(list);
end;

//------------------------------------------------------------------------------

procedure TMainForm.GetCurrentPreviousNextSubtitles(CurrentNode : PVirtualNode; var CurrentSub,
  PreviousSub, NextSub : TSubtitleRange);
var NextNode, PreviousNode : PVirtualNode;
    NodeData : PTreeData;
begin
  CurrentSub := nil;
  PreviousSub := nil;
  NextSub := nil;

  if Assigned(CurrentNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(CurrentNode);
    CurrentSub := NodeData.Range;

    NextNode := vtvSubsList.GetNext(CurrentNode);
    if Assigned(NextNode) then
    begin
      NodeData := vtvSubsList.GetNodeData(NextNode);
      NextSub := NodeData.Range;
    end;

    PreviousNode := vtvSubsList.GetPrevious(CurrentNode);
    if Assigned(PreviousNode) then
    begin
      NodeData := vtvSubsList.GetNodeData(PreviousNode);
      PreviousSub := NodeData.Range;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CallJSOnSubtitleModification;
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
    Selected : PVirtualNode;
begin
  Selected := nil;
  if Assigned(WAVDisplayer.SelectedRange) then
    Selected := TSubtitleRange(WAVDisplayer.SelectedRange).Node
  else if Assigned(vtvSubsList.FocusedNode) then
    Selected := vtvSubsList.FocusedNode;
  if Assigned(Selected) then
  begin
    GetCurrentPreviousNextSubtitles(Selected, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifySubtitleModification(CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CallJSOnSelectedSubtitle;
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    GetCurrentPreviousNextSubtitles(vtvSubsList.FocusedNode, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifySelectionModification(CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1RangeStartDblClick(Sender: TObject; Range : TRange);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(Range) then
  begin
    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    // Install handler to fill UndoableMultiChangeTask
    GeneralJSPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
    GeneralJSPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
    GeneralJSPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

    GetCurrentPreviousNextSubtitles(TSubtitleRange(Range).Node, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifyRangeStartDblClick(CurrentSub, PreviousSub, NextSub);

    GeneralJSPlugin.OnSubtitleChangeStart := nil;
    GeneralJSPlugin.OnSubtitleChangeStop := nil;
    GeneralJSPlugin.OnSubtitleChangeText := nil;

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1RangeStopDblClick(Sender: TObject; Range : TRange);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(Range) then
  begin
    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    // Install handler to fill UndoableMultiChangeTask
    GeneralJSPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
    GeneralJSPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
    GeneralJSPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

    GetCurrentPreviousNextSubtitles(TSubtitleRange(Range).Node, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifyRangeStopDblClick(CurrentSub, PreviousSub, NextSub);

    GeneralJSPlugin.OnSubtitleChangeStart := nil;
    GeneralJSPlugin.OnSubtitleChangeStop := nil;
    GeneralJSPlugin.OnSubtitleChangeText := nil;

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnJsSetStatusBarText(const Msg : WideString);
begin
  SetStatusBarPrimaryText(Msg);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ProcessParams;
var Ext : WideString;
    ProjectFileIni : TTntIniFile;
    ArgFilename, TmpFilename, NewProjectName : WideString;
    I : Integer;
const VideoExt : array[0..3] of string = ('.avi', '.mkv', '.mp4', '.mka');
begin
  if (WideParamCount = 1) then
  begin
    ArgFilename := WideParamStr(1);
    Ext := WideLowerCase(WideExtractFileExt(ArgFilename));
    if (Ext = '.vssprj') then
    begin
      MainForm.LoadProject(ArgFilename);
    end
    else if (Ext = '.srt') or (Ext = '.ssa') or (Ext = '.ass') then
    begin
      // First search if there is not a project with the same name
      NewProjectName := WideChangeFileExt(ArgFilename, '.vssprj');
      if WideFileExists(NewProjectName) then
      begin
        MainForm.LoadProject(NewProjectName);
        Exit;
      end;

      // Make project with existing data and load it
      ProjectFileIni := TTntIniFile.Create(NewProjectName);

      ProjectFileIni.WriteString('VisualSubsync','SubtitlesFile', ArgFilename);

      // Search for a matching video file
      for I := Low(VideoExt) to High(VideoExt) do
      begin
        TmpFilename := WideChangeFileExt(ArgFilename, VideoExt[i]);
        if WideFileExists(TmpFilename) then
        begin
          ProjectFileIni.WriteString('VisualSubsync','VideoSource', TmpFilename);
        end;
      end;

      // Search for a matching peak file
      TmpFilename := WideChangeFileExt(ArgFilename, '.peak');
      if WideFileExists(TmpFilename) then
      begin
        ProjectFileIni.WriteString('VisualSubsync','PeakFile', TmpFilename);
        ProjectFileIni.WriteInteger('VisualSubsync','WAVMode', Ord(pwmPeakOnly));
      end
      else
      begin
        ProjectFileIni.WriteInteger('VisualSubsync','WAVMode', Ord(pwmNoWaveform));
      end;

      ProjectFileIni.WriteBool('VisualSubsync','DetachedVideo', StartupDetachVideo);
      ProjectFileIni.WriteBool('VisualSubsync','ShowVideo', StartupShowVideo);
      ProjectFileIni.Free;

      MainForm.LoadProject(NewProjectName);
    end;
  end
  else if ConfigObject.LoadMostRecentProjectOnStartup then
  begin
    // Try to load most recent project
    ArgFilename := MRUList.GetMRUFilename;
    if WideFileExists(ArgFilename) then
    begin
      MainForm.LoadProject(ArgFilename);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsTXT(Filename, PreviousExt: WideString; InUTF8 : Boolean; ExcludeUnselected : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    Text : WideString;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;

    function IsSelected(StartTime, StopTime : Integer): Boolean;
    var Cursor : PVirtualNode; NodeData : PTreeData; Selected : Boolean;
    begin
      Selected := False;
      Cursor := vtvSubsList.GetFirstSelected;
      while (Cursor <> nil) do
        begin
          NodeData := vtvSubsList.GetNodeData(Cursor);
          if (NodeData.Range.StartTime = StartTime) And (NodeData.Range.StopTime = StopTime) then
            begin
              Selected := True;
              Break;
            end;
          Cursor := vtvSubsList.GetNextSelected(Cursor);
        end;

        Result := Selected;
    end;

begin
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);

    if ExcludeUnselected = True then
      if Not IsSelected(SubRange.StartTime, SubRange.StopTime) then Continue;

    Text := Subrange.Text;
    // Remove line breaks
    Text := Tnt_WideStringReplace(Text, ' ' + CRLF + ' ', ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, ' ' + CRLF, ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, CRLF + ' ', ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, CRLF, ' ', [rfReplaceAll]);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(Text), FS)
    else
      WriteStringLnStream(WC2MB(Text), FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsCSV(Filename, PreviousExt: WideString; InUTF8 : Boolean; ExcludeUnselected : Boolean);
var i : integer;
    SubRange : TSubtitleRange;
    FS : TTntFileStream;
    SubText, LineText : WideString;

    procedure WriteStringLnStream(str : string; Stream : TStream);
    begin
      str := str + #13#10;
      Stream.Write(str[1], Length(str));
    end;
begin
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  WriteStringLnStream('Index,Start,Stop,Text', FS);
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    SubText := Subrange.Text;
    // Remove line breaks
    SubText := Tnt_WideStringReplace(SubText, ' ' + CRLF + ' ', ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, ' ' + CRLF, ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, CRLF + ' ', ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, CRLF, ' ', [rfReplaceAll]);
    // Double quotes
    SubText := Tnt_WideStringReplace(SubText, '"', '""', [rfReplaceAll]);
    LineText := Format('%d,%s,%s,"%s"',[i+1, TimeMsToString(SubRange.StartTime),
      TimeMsToString(SubRange.StopTime),SubText]);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(LineText), FS)
    else
      WriteStringLnStream(WC2MB(LineText), FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyDelay(var Indexes : array of Integer;
  DelayInMs : Integer; DelayShiftType : TDelayShiftType);
var i, Idx : Integer;
    DelaySelectedRange : Boolean;
    Range : TRange;
    SubtitleRange : TSubtitleRange;
    DelayedRangeList : TList;

    procedure DelaySub(Range : TRange; DelayInMs : Integer;
      DelayShiftType : TDelayShiftType);
    var i : Integer;
    begin
      if (DelayShiftType = dstBothTime) or (DelayShiftType = dstStartTimeOnly) then
      begin
        Range.StartTime := Range.StartTime + DelayInMs;
      end;
      if (DelayShiftType = dstBothTime) or (DelayShiftType = dstStopTimeOnly) then
      begin
        Range.StopTime := Range.StopTime + DelayInMs;
      end;
      if (DelayShiftType = dstBothTime) then
      begin
        // Shift karaoke subtime
        for i:=0 to Length(Range.SubTime)-1 do
        begin
          Range.SubTime[i] := Range.SubTime[i] + DelayInMs;
        end;
      end;
    end;

begin
  DelayedRangeList := TList.Create;
  DelaySelectedRange := False;
  g_WebRWSynchro.BeginWrite;
  try
    // Apply the delay on each subtitle based on their current index
    for i := Low(Indexes) to High(Indexes) do
    begin
      Idx := Indexes[i];
      Range := WavDisplayer.RangeList[Idx];
      DelayedRangeList.Add(Range);
      DelaySub(Range, DelayInMs, DelayShiftType);
      if (Range = WavDisplayer.SelectedRange) then
      begin
        DelaySelectedRange := True;
      end;
    end;
    // Delay the selected range if necessary 
    if (DelaySelectedRange = True) and Assigned(WavDisplayer.SelectedRange) then
    begin
      DelaySub(WavDisplayer.Selection, DelayInMs, DelayShiftType);
      if (DelayShiftType in [dstStartTimeOnly, dstStopTimeOnly]) and
         (Length(WavDisplayer.SelectedRange.SubTime) > 0) then
      begin
        // TODO : check undo for this
        WAVDisplayer1KaraokeChanged(WAVDisplayer, WavDisplayer.SelectedRange, -1, -1);
      end;
    end;
    // Sort subtitles
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  // Update SearchNodeIndex  (TODO : SearchPos)  

  // Update indexes which have been changed during sorting
  for i := 0 to DelayedRangeList.Count-1 do
  begin
    SubtitleRange := TSubtitleRange(DelayedRangeList[i]);
    Indexes[i] := SubtitleRange.Node.Index;
  end;
  DelayedRangeList.Free;  

  UpdateDurationWithSubOnly;
  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.DeleteSubtitles(var Indexes : array of Integer);
var I, Idx : Integer;
    SubtitleRange : TSubtitleRange;
    NodeToFocus : PVirtualNode;
begin
  g_WebRWSynchro.BeginWrite;
  try
    for I := High(Indexes) downto Low(Indexes) do
    begin
      Idx := Indexes[I];
      SubtitleRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
      ErrorReportForm.DeleteError(SubtitleRange);
      vtvSubsList.DeleteNode(SubtitleRange.Node);
      WAVDisplayer.DeleteRangeAtIdx(Idx, False);

      // Update SearchNodeIndex  (TODO : SearchPos)
      if (SearchNodeIndex > Idx) then
        Dec(SearchNodeIndex);
      if (SearchNodeIndex >= Idx) then
        SearchPos := 1;
    end;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  WAVDisplayer.UpdateView([uvfRange]);

  // Set focus
  Idx := Indexes[0];
  if (Idx > 0) then
  begin
    // Get the subtitle range just before the first deleted one
    SubtitleRange := TSubtitleRange(WAVDisplayer.RangeList[Idx - 1]);
    // The node to focus is the node just after it
    NodeToFocus := vtvSubsList.GetNext(SubtitleRange.Node);
  end
  else
  begin
    NodeToFocus := vtvSubsList.GetFirst;
  end;

  if Assigned(NodeToFocus) then
  begin
    FocusNode(NodeToFocus, False);
  end;

  UpdateDurationWithSubOnly;
end;

//------------------------------------------------------------------------------

procedure TMainForm.DeleteSubtitle(Index : Integer);
var ParamArray : array[0..0] of Integer;
begin
  ParamArray[0] := Index;
  DeleteSubtitles(ParamArray);
end;

//------------------------------------------------------------------------------

procedure TMainForm.CloneSubtitles(var Indexes : array of Integer; List : TList);
var i, idx : integer;
    Range, RangeClone : TRange;
begin
  List.Clear;
  for i := Low(Indexes) to High(Indexes) do
  begin
    idx := Indexes[i];
    Range := WAVDisplayer.RangeList[Idx];
    RangeClone := SubRangeFactory.CreateRange;
    RangeClone.Assign(Range);
    List.Add(RangeClone);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.RestoreSubtitles(List : TList; Indexes : TIntegerDynArray);
var i : integer;
    Range : TSubtitleRange;
    Node : PVirtualNode;
begin
  for i := List.Count-1 downto 0 do
  begin
    Range := List[i];
    Node := AddSubtitle(Range, False);
    Range.Node := Node;
  end;

  if Assigned(Indexes) then
  begin
    for i := 0 to List.Count-1 do
    begin
      Range := List[i];
      Indexes[i] := Range.Node.Index;    
    end;
  end;

  CurrentProject.IsDirty := True;
  UpdateDurationWithSubOnly;
  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SplitSubtitle(Index, SplitTime, BlankTime : Integer);
var SubRange, NewSubRange : TSubtitleRange;
    NewNode : PVirtualNode;
    NodeData : PTreeData;
    SplitTime1, SplitTime2 : Integer;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  NewSubRange := TSubtitleRange(SubRangeFactory.CreateRange);
  NewSubRange.Assign(SubRange);

  SplitTime1 := SplitTime - BlankTime;
  SplitTime2 := SplitTime;
  // Make sure there is enough space for the blank time
  if (SplitTime1 < SubRange.StartTime) or (SplitTime2 > SubRange.StopTime) then
  begin
    // no blank time
    SplitTime1 := SplitTime - 1;
    SplitTime2 := SplitTime;
  end;
  // Make sur there is no overlapping
  if (SplitTime1 = SplitTime2) then
  begin
    SplitTime1 := SplitTime1 - 1;
  end;
  SubRange.StopTime := SplitTime1;
  NewSubRange.StartTime := SplitTime2;

  // Update SearchIdx and SearchPos

  NewNode := vtvSubsList.InsertNode(SubRange.Node, amInsertAfter);
  NodeData := vtvSubsList.GetNodeData(NewNode);
  NodeData.Range := NewSubRange;
  NewSubRange.Node := NewNode;
  g_WebRWSynchro.BeginWrite;
  try
    WAVDisplayer.AddRange(NewSubRange);
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsList.Repaint;
end;

// -----------------------------------------------------------------------------

function TMainForm.SetSubtitleTime(Index, NewStartTime, NewStopTime : Integer) : Integer;
var SubRange : TSubtitleRange;
    NewText : WIdeString;
begin
  try
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
    SubRange.StartTime := NewStartTime;
    SubRange.StopTime := NewStopTime;
    if (Length(SubRange.SubTime) > 0) then
    begin
      NewText := CalculateNewTextFromSubTime(SubRange);
      if (NewText <> SubRange.Text) then
        SubRange.Text := NewText;
    end;    
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  if (WAVDisplayer.SelectedRange = SubRange) then
  begin
    WAVDisplayer.Selection.StartTime := NewStartTime;
    WAVDisplayer.Selection.StopTime := NewStopTime;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  if (Length(SubRange.SubTime) > 0) then
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;

  UpdateDurationWithSubOnly;

  Result := SubRange.Node.Index;
end;

// -----------------------------------------------------------------------------

function TMainForm.MergeSubtitles(var FIndexes : array of Integer; MergeType : TMergeType) : TSubtitleRange;
var i, idx : integer;
    AccuText : WideString;
    LastStopTime : Integer;
    FirstRange, SubRange : TSubtitleRange;
begin
  idx := FIndexes[Low(FIndexes)];
  FirstRange := TSubtitleRange(WAVDisplayer.RangeList[idx]);
  if (MergeType = mtDialog) then
    AccuText := '- ' + FirstRange.Text
  else if (MergeType = mtOneLine) then
    AccuText := TrimRight(FirstRange.Text)
  else
    AccuText := FirstRange.Text;

  LastStopTime := FirstRange.StopTime;
  for i := Low(FIndexes) + 1 to High(FIndexes) do
  begin
    idx := FIndexes[i];
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[idx]);
    if (MergeType = mtDialog) then
      AccuText := AccuText + CRLF + '- ' + SubRange.Text
    else if (MergeType = mtOneLine) then
      AccuText := TrimRight(AccuText) + ' ' + TrimLeft(SubRange.Text)
    else
      AccuText := AccuText + CRLF + SubRange.Text;
    LastStopTime := Max(LastStopTime, SubRange.StopTime);
  end;
  Result := TSubtitleRange(SubRangeFactory.CreateRange);
  Result.Assign(FirstRange);
  Result.StopTime := LastStopTime;
  Result.Text := Trim(AccuText);
end;

// -----------------------------------------------------------------------------

procedure TMainForm.OnUndo(Sender: TTntRichEdit; UndoTask : TUndoableTask);
var UndoableSubTextTask : TUndoableSubTextTask;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    UndoableSubTextTask := TUndoableSubTextTask.Create;
    UndoableSubTextTask.SetData(vtvSubsList.FocusedNode.Index, UndoTask);
    PushUndoableTask(UndoableSubTextTask);
  end
  else
    PushUndoableTask(UndoTask);
end;

// -----------------------------------------------------------------------------

procedure TMainForm.ProcessMultiChangeSub(ChangeList : TList; IsUndo : Boolean);
var i : integer;
    ChangeSubData : TChangeSubData;
    SubtitleRange : TSubtitleRange;
    ModifiedRangeList : TList;
begin
  if (not Assigned(ChangeList)) or (ChangeList.Count = 0) then
    Exit;

  ModifiedRangeList := TList.Create;
  g_WebRWSynchro.BeginWrite;
  try
    for i := 0 to ChangeList.Count-1 do
    begin
      ChangeSubData := ChangeList[i];
      SubtitleRange := TSubtitleRange(WavDisplayer.RangeList[ChangeSubData.Index]);
      ModifiedRangeList.Add(SubtitleRange);
      // Apply subtitle change
      if (ChangeSubData.StartChanged) then
        SubtitleRange.StartTime := ChangeSubData.GetStart(IsUndo);
      if (ChangeSubData.StopChanged) then
        SubtitleRange.StopTime := ChangeSubData.GetStop(IsUndo);
      if (ChangeSubData.TextChanged) then
        SubtitleRange.Text := ChangeSubData.GetText(IsUndo);
      if (ChangeSubData.SubTimeChanged) then
        SubtitleRange.SubTime := ChangeSubData.GetSubTime(IsUndo);
      if (SubtitleRange = WavDisplayer.SelectedRange) then
      begin
        // Change selection
        if (ChangeSubData.StartChanged) then
          WavDisplayer.Selection.StartTime := SubtitleRange.StartTime;
        if (ChangeSubData.StopChanged) then
          WavDisplayer.Selection.StopTime := SubtitleRange.StopTime;
      end;
    end;
    // Sort subtitles
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  // Update indexes which have been changed during sorting
  for i := 0 to ModifiedRangeList.Count-1 do
  begin
    ChangeSubData := ChangeList[i];
    SubtitleRange := ModifiedRangeList[i];
    ChangeSubData.Index := SubtitleRange.Node.Index;
  end;
  ModifiedRangeList.Free;

  UpdateDurationWithSubOnly;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
end;

//==============================================================================

procedure TMainForm.ClearStack(Stack : TObjectStack);
begin
  while (Stack.Count > 0) do
  begin
    Stack.Pop.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PushUndoableTask(UndoableTask : TUndoableTask);
var TopTask : TUndoableTask;
    TaskMerged : Boolean;
begin
  ClearStack(RedoStack);
  ActionRedo.Enabled := False;
  TaskMerged := False;
  // Try to merge the new task with the task on top of stack
  if (UndoStack.Count > 0) then
  begin
    TopTask := TUndoableTask(UndoStack.Peek);
    TaskMerged := TopTask.Merge(UndoableTask);
  end;
  if (not TaskMerged) then
  begin
    UndoStack.Push(UndoableTask);
  end
  else
  begin
    // Free the undoable task it has been merged with the top task
    UndoableTask.Free;
  end;
  ActionUndo.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionUndoExecute(Sender: TObject);
var UndoableTask : TUndoableTask;
begin
  if MainForm.ActiveControl = MemoTextPipe then
   begin
    MemoTextPipe.Undo;
    Exit;
   end;

  if (UndoStack.Count > 0) then
  begin
    UndoableTask := TUndoableTask(UndoStack.Pop);
    UndoableTask.UndoTask;
    RedoStack.Push(UndoableTask);
    ActionUndo.Enabled := (UndoStack.Count > 0);
    ActionRedo.Enabled := (RedoStack.Count > 0);
    CheckResynchToolMenu;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionRedoExecute(Sender: TObject);
var UndoableTask : TUndoableTask;
begin
  if (RedoStack.Count > 0) then
  begin
    UndoableTask := TUndoableTask(RedoStack.Pop);
    UndoableTask.DoTask;
    UndoStack.Push(UndoableTask);
    ActionUndo.Enabled := (UndoStack.Count > 0);
    ActionRedo.Enabled := (RedoStack.Count > 0);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.IsTimingMode : Boolean;
begin
  Result := (bttWorkingMode.Tag = 1)
end;

function TMainForm.IsNormalMode : Boolean;
begin
  Result := (bttWorkingMode.Tag = 0)
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionWAVDisplayScrollRightExecute(Sender: TObject);
begin
  WAVDisplayer.Scroll(25);
end;

procedure TMainForm.ActionWAVDisplayScrollLeftExecute(Sender: TObject);
begin
  WAVDisplayer.Scroll(-25);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideSceneChangeExecute(Sender: TObject);
begin
  WAVDisplayer.SceneChangeEnabled := not WAVDisplayer.SceneChangeEnabled;
  WAVDisplayer.UpdateView([uvfRange]);
  ConfigObject.ShowSceneChange := WAVDisplayer.SceneChangeEnabled;
  MenuItemShowHideSceneChange.Checked := ConfigObject.ShowSceneChange;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagText(StartTag, StopTag : WideString);
begin
  if MemoSubtitleText.Focused then
    TagTextMemo(StartTag, StopTag)
  else
    TagTextLines(StartTag, StopTag);
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagTextMemo(StartTag, StopTag : WideString);
var SelStart, SelLength : Integer;
    NewText : WideString;
begin
  SelStart := MemoSubtitleText.SelStart;
  SelLength := MemoSubtitleText.SelLength;
  if (SelLength > 0) then
  begin
    // Put tag around selection
    NewText := Copy(MemoSubtitleText.Text, 1, SelStart) +
      StartTag + Copy(MemoSubtitleText.Text, SelStart + 1, SelLength) +
      StopTag + Copy(MemoSubtitleText.Text, SelStart + SelLength + 1, MaxInt);
  end
  else if (SelStart = 0) then
  begin
    // Put tag around the entire text
    NewText := StartTag + MemoSubtitleText.Text + StopTag;
  end
  else
  begin
    // Insert tag at the cursor place
    NewText := Copy(MemoSubtitleText.Text, 1, SelStart) + StartTag + StopTag +
      Copy(MemoSubtitleText.Text, SelStart + 1, MaxInt);
  end;

  TTntRichEditCustomUndo(MemoSubtitleText).SaveSelectionInfo;
  MemoSubtitleText.Text := NewText;

  if (SelLength > 0) then
  begin
    MemoSubtitleText.SelStart := SelStart;
    MemoSubtitleText.SelLength := SelLength + Length(StartTag) + Length(StopTag);
  end
  else if (SelStart = 0) then
  begin
    // Just place the cursor at the start
    MemoSubtitleText.SelStart := 0;
    MemoSubtitleText.SelLength := 0;
  end
  else
  begin
    // Place the cursor after the start tag
    MemoSubtitleText.SelStart := SelStart + Length(StartTag);
    MemoSubtitleText.SelLength := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagTextLines(StartTag, StopTag : WideString);
var Cursor : PVirtualNode;
    NodeData : PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    TaggedText : WideString;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Cursor);
    SubtitleRange := NodeData.Range;
    TaggedText := StartTag + SubtitleRange.Text + StopTag;
    if (TaggedText <> SubtitleRange.Text) then
    begin
      ChangeSubData := TChangeSubData.Create(Cursor.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.NewText := TaggedText;
      SubtitleRange.Text := TaggedText;
      MultiChangeTask.AddData(ChangeSubData);
    end;
    Cursor := vtvSubsList.GetNextSelected(Cursor);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagText(TagType : TTagType);
var Ext : WideString;
begin

  // italic: <i></i> or {\i1}{\i0}
  // bold: <b></b> or {\b1}{\b0}
  // underline: <u></u> or {\u1}{\u0}
  // font size: <font size="10"></font> {\fs10}
  // font color: <font color="#FF0000"></font> {\1c&H000000&}
  // {\an1} ...
  // {\pos  ...

  Ext := WideLowerCase(WideExtractFileExt(CurrentProject.SubtitlesFile));
  if (Ext = '.srt') then
  begin
    case TagType of
    ttItalic: TagText('<i>','</i>');
    ttBold: TagText('<b>','</b>');
    ttUnderline: TagText('<u>','</u>');
    ttColor: TagText('<font color="#FFFFFF">','</font>');
    ttSize: TagText('<font size="16">','</font>');
    ttPosBottomLeft: TagText('{\an1}','');
    ttPosBottomMiddle: TagText('{\an2}','');
    ttPosBottomRight: TagText('{\an3}','');
    ttPosMiddleLeft: TagText('{\an4}','');
    ttPosMiddleMiddle: TagText('{\an5}','');
    ttPosMiddleRight: TagText('{\an6}','');
    ttPosTopLeft: TagText('{\an7}','');
    ttPosTopMiddle: TagText('{\an8}','');
    ttPosTopRight: TagText('{\an9}','');
    end;
  end
  else if (Ext = '.ass') or (Ext = '.ssa') then
  begin
    case TagType of
    ttItalic: TagText('{\i1}','{\i0}');
    ttBold: TagText('{\b1}','{\b0}');
    ttUnderline: TagText('{\u1}','{\u0}');
    ttColor: TagText('{\1c&HFFFFFF&}','{\1c}');
    ttSize: TagText('{\fs16}','{\fs}');
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextItalicExecute(Sender: TObject);
begin
  TagText(ttItalic);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextBoldExecute(Sender: TObject);
begin
  TagText(ttBold);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextUnderlineExecute(Sender: TObject);
begin
  TagText(ttUnderline);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextColorExecute(Sender: TObject);
begin
  TagText(ttColor);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextSizeExecute(Sender: TObject);
begin
  TagText(ttSize);
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
    NewColor : TColor;
begin
  if GeneralJSPlugin.IsColumnBGColorized(Column) then
  begin
    GetCurrentPreviousNextSubtitles(Node, CurrentSub, PreviousSub, NextSub);
    NewColor := GeneralJSPlugin.GetColumnBgColor(Column, CurrentSub, PreviousSub, NextSub);
    TargetCanvas.Brush.Color := NewColor;
    TargetCanvas.FillRect(CellRect);
    if vtvSubsList.Selected[Node] then
    begin
      // No selection blending
      vtvSubsList.SelectionBlendFactor := 2;
    end
    else
    begin
      // default
      vtvSubsList.SelectionBlendFactor := 255;
    end;
  end
  else
  begin
    // default  
    vtvSubsList.SelectionBlendFactor := 255;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if GeneralJSPlugin.IsColumnBGColorized(Column) then
  begin
    // Force default text color
    TargetCanvas.Font.Color := clWindowText;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStripTagsExecute(Sender: TObject);
var Cursor : PVirtualNode;
    NodeData : PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    StrippedText : WideString;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Cursor);
    SubtitleRange := NodeData.Range;
    StrippedText := StripTags(SubtitleRange.Text);
    if (StrippedText <> SubtitleRange.Text) then
    begin
      ChangeSubData := TChangeSubData.Create(Cursor.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.NewText := StrippedText;
      SubtitleRange.Text := StrippedText;
      MultiChangeTask.AddData(ChangeSubData);
    end;
    Cursor := vtvSubsList.GetNextSelected(Cursor);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiToggleColumn(Sender: TObject);
var Column : TVirtualTreeColumn;
    SelectedMenuItem : TMenuItem;
begin
  if (Sender is TTntMenuItem) then
  begin
    SelectedMenuItem := (Sender as TMenuItem);
    Column := (TObject(SelectedMenuItem.Tag) as TVirtualTreeColumn);
    if (SelectedMenuItem.Checked) then
    begin
      ShowColumn(Column, False, True);
    end
    else
    begin
      ShowColumn(Column, True, True);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionOpenProjectFolderExecute(Sender: TObject);
var ProjectFolder : WideString;
begin
  ProjectFolder := WideExtractFilePath(CurrentProject.Filename);
  Tnt_ShellExecuteW(Handle, 'explore', PWideChar(ProjectFolder), nil,
    nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.InsertSceneChange(SrcSCArray : TIntegerDynArray);
var SCArray : TIntegerDynArray;
    SceneChangeFileName : WideString;
begin
  g_SceneChangeWrapper.Insert(SrcSCArray);
  SCArray := g_SceneChangeWrapper.GetSCArray;
  WAVDisplayer.SetSceneChangeList(SCArray);
  WAVDisplayer.UpdateView([uvfRange]);
  SceneChangeFileName := WideChangeFileExt(CurrentProject.VideoSource,
    '.scenechange');
  if WideFileExists(SceneChangeFileName) then
  begin
    SaveSceneChange(SceneChangeFileName, SCArray);
  end;
end;

function TMainForm.DeleteSceneChange(StartTimeMs, StopTimeMs : Integer) : TIntegerDynArray;
var SCArray :TIntegerDynArray;
    SceneChangeFileName : WideString;
begin
  Result := g_SceneChangeWrapper.Delete(StartTimeMs, StopTimeMs);
  SCArray := g_SceneChangeWrapper.GetSCArray;
  WAVDisplayer.SetSceneChangeList(SCArray);
  WAVDisplayer.UpdateView([uvfRange]);
  SceneChangeFileName := WideChangeFileExt(CurrentProject.VideoSource,
    '.scenechange');
  if WideFileExists(SceneChangeFileName) then
  begin
    SaveSceneChange(SceneChangeFileName, SCArray);
  end;
end;

procedure TMainForm.ActionDeleteSceneChangeExecute(Sender: TObject);
var UndoableDeleteSceneChange : TUndoableDeleteSceneChange;
begin
  if (WAVDisplayer.SelectionIsEmpty) then
    Exit;
  if (not g_SceneChangeWrapper.Contains(WAVDisplayer.Selection.StartTime,
      WAVDisplayer.Selection.StopTime)) then
    Exit;

  UndoableDeleteSceneChange := TUndoableDeleteSceneChange.Create;
  UndoableDeleteSceneChange.SetData(WAVDisplayer.Selection.StartTime,
    WAVDisplayer.Selection.StopTime);
  UndoableDeleteSceneChange.DoTask;
  PushUndoableTask(UndoableDeleteSceneChange);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionReloadExecute(Sender: TObject);
var Index : Integer;
begin
  if CheckSubtitlesAreSaved then
  begin
    if Assigned(vtvSubsList.FocusedNode) then
    begin
      Index := vtvSubsList.FocusedNode.Index;
    end
    else
    begin
      Index := -1;
    end;

    if WideFileExists(CurrentProject.SubtitlesVO) then
    begin
      ShowStatusBarMessage('Reloading VO reference subtitles...');
      LoadVO(CurrentProject.SubtitlesVO);
    end;
    ShowStatusBarMessage('Reloading subtitles...');
    LoadSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
    SelectNodeAtIndex(Index);
    ClearStack(RedoStack);
    ClearStack(UndoStack);
    ActionUndo.Enabled := False;
    ActionRedo.Enabled := False;
    CurrentProject.IsDirty := False;
  end;
  AdjustShowVO;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeWithPreviousExecute(Sender: TObject);
var Node : PVirtualNode;
begin
  Node := vtvSubsList.GetFirstSelected;
  if not Assigned(Node) then Exit;
  Node := vtvSubsList.GetPrevious(Node);
  if not Assigned(Node) then Exit;
  Merge(mtNormal, mrWithPrevious);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeWithNextExecute(Sender: TObject);
var Node : PVirtualNode;
begin
  Node := vtvSubsList.GetFirstSelected;
  if not Assigned(Node) then Exit;
  Node := vtvSubsList.GetNext(Node);
  if not Assigned(Node) then Exit;
  Merge(mtNormal, mrWithNext);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeDialogExecute(Sender: TObject);
begin
  if (vtvSubsList.SelectedCount > 1) then
  begin
    Merge(mtDialog, mrSelected);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeOnOneLineExecute(Sender: TObject);
begin
  if (vtvSubsList.SelectedCount > 1) then
  begin
    Merge(mtOneLine, mrSelected);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnJavascriptAction(Sender : TObject);
var JsAction : TJavascriptAction;
begin
  if (Sender is TJavascriptAction) then
  begin
    JsAction := (Sender as TJavascriptAction);

    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    GeneralJSPlugin.Eval(JsAction.JSObjectName + '.onExecute();');

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateAfterJSChange;
var SubtitleRange : TSubtitleRange;
    NodeData : PTreeData;
    SelStart, SelLength : Integer;
begin
  CallJSOnSubtitleModification;
  
  // Update MemoSubtitleText if changed
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    SubtitleRange := NodeData.Range;
    if (MemoSubtitleText.Text <> SubtitleRange.Text) then
    begin
      SelStart := MemoSubtitleText.SelStart;
      SelLength := MemoSubtitleText.SelLength;

      vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);

      MemoSubtitleText.SelStart := SelStart;
      MemoSubtitleText.SelLength := SelLength;
    end;
  end;
  // Update subtitle list and wavdisplay
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
  WAVDisplayer1SelectionChange(WAVDisplayer);
  CurrentProject.IsDirty := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.RegisterJavascriptAction(AName, ACaption, ADefaultShortcut : WideString);
var JSAction : TJavascriptAction;
    NewItem : TTntMenuItem;
begin
  JSAction := TJavascriptAction.Create(TntActionList1);
  JSAction.Caption := '[js] ' + ACaption;
  JSAction.Name := AName;
  JSAction.JSObjectName := AName;
  JSAction.ShortCut := TextToShortCut(ADefaultShortcut);
  JSAction.Tag := 1;
  JSAction.OnExecute := OnJavascriptAction;
  JSAction.ActionList := TntActionList1;

  NewItem := TTntMenuItem.Create(Self);
  NewItem.Action := JSAction;
  NewItem.Caption := Tnt_WideStringReplace(NewItem.Caption, '[js] ', '', [rfReplaceAll]);
  MenuItemJSTools.Add(NewItem);
end;

procedure TMainForm.DisableJavascriptItemMenu(ACaption : WideString);
 var NewItem : TMenuItem;
begin
 NewItem := MenuItemJSTools.Find(ACaption);
 NewItem.Enabled := False;
end;

procedure TMainForm.EnableJavascriptItemMenu(ACaption : WideString);
 var NewItem : TMenuItem;
begin
 NewItem := MenuItemJSTools.Find(ACaption);
 NewItem.Enabled := True;
end;

procedure TMainForm.InsertBreakBeforeJavascriptMenuItem(ACaption : WideString);
 var NewItem : TMenuItem; InsertedLine : Integer;
begin
 NewItem := MenuItemJSTools.Find(ACaption);
 InsertedLine := MenuItemJSTools.InsertNewLineBefore(NewItem);
end;

procedure TMainForm.InsertBreakAfterJavascriptMenuItem(ACaption : WideString);
 var NewItem : TMenuItem;
begin
 NewItem := MenuItemJSTools.Find(ACaption);
 MenuItemJSTools.InsertNewLineAfter(NewItem);
end;

function TMainForm.GetSubCount : Integer;
begin
  Result := vtvSubsList.RootNodeCount;
end;

function TMainForm.GetPluginParamValue(JsSection, JsParameter : WideString) : Integer;
var IniFile : TIniFile;
    IniFilename : WideString;
begin
  IniFilename := GetIniFilename;
  try
    IniFile := TIniFile.Create(IniFilename);
    Result := IniFile.ReadInteger(JsSection,JsParameter,0);
  except
    Result := 0;
  end;
  IniFile.Free;
end;

procedure TMainForm.SetPluginParamValue(JsSection, JsParameter : WideString; Value: Integer);
var IniFile : TIniFile;
    IniFilename : WideString;
begin
  IniFilename := GetIniFilename;
  try
    IniFile := TIniFile.Create(IniFilename);
    IniFile.WriteInteger(JsSection,JsParameter,Value);
  except
  end;
  IniFile.Free;
end;

function TMainForm.GetCursorPosition : Integer;
begin
 Result := WavDisplayerCursorPos;
end;

procedure TMainForm.SetSubColor(StartIndex,LastIndex : Integer; Color : TColor);
 var I : Integer;
begin
 try
  if g_GlobalContext.SubList.Count>0 then
   if StartIndex <= LastIndex then
    if (StartIndex >= 1) AND (LastIndex-1 <= g_GlobalContext.SubList.Count) then
     begin
      for I := StartIndex to LastIndex do
       begin
        g_GlobalContext.SubList[I-1].MarkColor := Color;
       end;
      WAVDisplayer.UpdateView([uvfRange]);
     end;
 except
 end;
end;

procedure TMainForm.ResetSubColor;
 var I : Integer;
begin
 try
  if g_GlobalContext.SubList.Count>0 then
   begin
    for I := 0 to g_GlobalContext.SubList.Count-1 do
     begin
      g_GlobalContext.SubList[I].MarkColor := $000000;
     end;
     WAVDisplayer.UpdateView([uvfRange]);
   end;
 except
 end;
end;

procedure TMainForm.SetSubIcon(Index : Integer; BitMapIndex : Integer);
begin
 try
  if g_GlobalContext.SubList.Count>0 then
  if (BitMapIndex>=0) AND (BitMapIndex <= 6) then
   begin
    g_GlobalContext.SubList[Index-1].IconIndex := BitMapIndex;
    WAVDisplayer.UpdateView([uvfRange]);
   end;
 except
 end;
end;

function TMainForm.GetFirst : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetFirst;
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetNext(SubtitleRange : TSubtitleRange) : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetNext(SubtitleRange.Node);
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetPrevious(SubtitleRange : TSubtitleRange) : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetPrevious(SubtitleRange.Node);
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetSelectedCount : Integer;
begin
  Result := vtvSubsList.SelectedCount;
end;

function TMainForm.GetFirstSelected : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetFirstSelected;
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetNextSelected(SubtitleRange : TSubtitleRange) : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetNextSelected(SubtitleRange.Node);
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetAt(Index : Integer) : TSubtitleRange;
begin
  if (Index >= 0) and (Index < WAVDisplayer.RangeList.Count) then
    Result := TSubtitleRange(WAVDisplayer.RangeList[Index])
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadPresetFile(Filename : WideString);
var IniFile : TIniFile;
    FilenameOnly : WideString;
    I : Integer;
begin
  if WideFileExists(Filename) then
  begin
    IniFile := TIniFile.Create(FileName);
    try
      ConfigObject.LoadIni(IniFile, True);
      SetShortcut(IsTimingMode);
      ApplyMouseSettings;
      ApplyFontSettings;
      ApplyMiscSettings;
    finally
      IniFile.Free;
    end;
    // Check the menu item
    FilenameOnly := WideExtractFileName(Filename);
    for i:= 0 to MenuItemLoadPresets.Count-1 do
    begin
      MenuItemLoadPresets[i].Checked := (MenuItemLoadPresets[i].Hint = FilenameOnly);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoadPresetsExecute(Sender: TObject);
begin
  OpenDialogPresets.Filter := 'Presets file|*.INI' + '|' + 'All files (*.*)|*.*';
  if not OpenDialogPresets.Execute then
    Exit;
  LoadPresetFile(OpenDialogPresets.FileName);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionCopyExecute(Sender: TObject);
var actCtrl : TWinControl;
begin
  actCtrl := Screen.ActiveControl;
  if (actCtrl is TCustomEdit) then
    TCustomEdit(actCtrl).CopyToClipboard
  else if (actCtrl is TVirtualStringTree) then
  begin
    CopyVTVToClipboard(TVirtualStringTree(actCtrl));
  end;
end;


//------------------------------------------------------------------------------

procedure TMainForm.ActionCopyUpdate(Sender: TObject);
var actCtrl : TWinControl;
begin
  actCtrl := Screen.ActiveControl;
  ActionCopy.Enabled := ((actCtrl is TCustomEdit) and (TCustomEdit(actCtrl).SelLength > 0))
    or ((actCtrl is TVirtualStringTree) and (TVirtualStringTree(actCtrl).SelectedCount > 0));
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnLoadPresetMenuItemClick(Sender: TObject);
var MenuItem : TTntMenuItem;
begin
  if (Sender is TTntMenuItem) then
  begin
    MenuItem := Sender as TTntMenuItem;
    LoadPresetFile(g_PresetsPath + MenuItem.Hint);
    CurrentProject.Presets := MenuItem.Hint;
    CurrentProject.IsDirty := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FillPresetsMenu;
var FFindFileAttrs : Integer;
    FSearchRec : TSearchRecW;
    FileList : TTntStringList;
    NewMenuItem : TTntMenuItem;
    i : Integer;
begin
  FFindFileAttrs := faAnyFile;
  FSearchRec.FindHandle := INVALID_HANDLE_VALUE;
  FileList := TTntStringList.Create;

  if (WideFindFirst(g_PresetsPath + '*.ini', FFindFileAttrs, FSearchRec) <> 0) then
    WideFindClose(FSearchRec)
  else
  begin
    while (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) do
    begin
      FileList.Add(FSearchRec.Name);
      if (WideFindNext(FSearchRec) <> 0) then
        WideFindClose(FSearchRec);
    end;
  end;

  FileList.Sort;
  
  for i:=0 to FileList.Count-1 do
  begin
    NewMenuItem := TTntMenuItem.Create(MenuItemLoadPresets);
    NewMenuItem.AutoHotkeys := maManual;
    NewMenuItem.Caption := FileList[i];
    NewMenuItem.Hint := FileList[i];
    NewMenuItem.OnClick := OnLoadPresetMenuItemClick;
    MenuItemLoadPresets.Add(NewMenuItem);
  end;
  // Add special "Clear list" menu
  NewMenuItem := TTntMenuItem.Create(MenuItemLoadPresets);
  NewMenuItem.Caption := '-';
  MenuItemLoadPresets.Add(NewMenuItem);

  NewMenuItem := TTntMenuItem.Create(MenuItemLoadPresets);
  NewMenuItem.Action := ActionLoadPresets;
  NewMenuItem.Caption := 'Browse...';
  MenuItemLoadPresets.Add(NewMenuItem);

  FileList.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetSelection(Start, Stop : Integer);
begin
  WAVDisplayer.ClearSelection;
  WAVDisplayer.Selection.StartTime := Start;
  WAVDisplayer.Selection.StopTime := Stop;
  WAVDisplayer.ZoomCenteredOn(Start + ((Stop - Start) div 2), 20000);
  WAVDisplayer1SelectionChange(WAVDisplayer);
  WAVDisplayer.UpdateView([uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowSilentZonesExecute(Sender: TObject);
var IniFile : TIniFile;
begin
  if (SilentZoneForm = nil) then
  begin
    SilentZoneForm := TSilentZoneForm.Create(Self, WAVDisplayer);
    SilentZoneForm.udDuration.Position := ConfigObject.SilentZoneDuration;
    SilentZoneForm.udThreshold.Position := ConfigObject.SilentZoneThreshold;
    IniFile := TIniFile.Create(GetIniFilename);
    LoadFormPosition(IniFile, SilentZoneForm);
    IniFile.Free;
  end;
  SilentZoneForm.Visible := True;
  SilentZoneForm.UpdateData;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnLoadDictionary(Sender: TObject);
begin
  SubMenuItemSpellcheck.Enabled:= False;
  FSpellChecker.Initialize((Sender as TBgThreadTask).Param);
end;

procedure TMainForm.OnLoadDictionaryTerminate(Sender: TObject);
var Event1, Event2 : TNotifyEvent;
    i : Integer;
begin
  SubMenuItemSpellcheck.Enabled:= True;
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;

  // Enable dictionary loading menus
  for i := 0 to SubMenuItemSpellcheck.Count-1 do
  begin
    Event1 := SubMenuItemSpellcheck.Items[i].OnClick;
    Event2 := Self.OnSpellcheckLanguageMenuItemClick;
    if (@Event1 = @Event2) then
    begin
      SubMenuItemSpellcheck.Items[i].Enabled := True;
    end;
  end;

  StartupDictionary := FSpellChecker.GetCurrentDictName;
  if Assigned(CurrentProject) and (CurrentProject.Filename <> '') then
  begin
    if (CurrentProject.Dictionary <> StartupDictionary) then
    begin
      CurrentProject.Dictionary := StartupDictionary;
      SaveProject(CurrentProject, True);
    end;
  end;
end;

procedure TMainForm.LoadDict(Idx : Integer);
var i, OldIdx : Integer;
    BgThreadTask : TBgThreadTask;
    Event1, Event2 : TNotifyEvent;
begin
  OldIdx := FSpellChecker.GetDictIdx(FSpellChecker.GetCurrentDictName);
  if (Idx = OldIdx) then
    Exit; // Already loaded

  // First remove previous dictionary from memory
  FSpellChecker.Cleanup;

  // Update display
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;

  // Disable dictionary loading menus
  for i := 0 to SubMenuItemSpellcheck.Count-1 do
  begin
    Event1 := SubMenuItemSpellcheck.Items[i].OnClick;
    Event2 := Self.OnSpellcheckLanguageMenuItemClick;
    if (@Event1 = @Event2) then
    begin
      SubMenuItemSpellcheck.Items[i].Enabled := False;
      if (SubMenuItemSpellcheck.Items[i].Tag = Idx) then
      begin
        SubMenuItemSpellcheck.Items[i].Checked := True;
      end;
    end;
  end;

  // Load new dictionary in background
  BgThreadTask := TBgThreadTask.Create(True, tpLowest);
  BgThreadTask.FreeOnTerminate := True;
  BgThreadTask.OnExecute := OnLoadDictionary;
  BgThreadTask.OnTerminate := OnLoadDictionaryTerminate;
  BgThreadTask.Param := Idx;
  BgThreadTask.Resume;
end;

procedure TMainForm.OnSpellcheckLanguageMenuItemClick(Sender: TObject);
begin
  LoadDict( (Sender as TMenuItem).Tag );
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLiveSpellCheckExecute(Sender: TObject);
begin
  MenuItemLiveSpellcheck.Checked := not MenuItemLiveSpellcheck.Checked;
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoSubPopupMenuPopup(Sender: TObject);
var P : TPoint;
    AWord : WideString;
    Suggestions: TTntStrings;
    i, ii, j, jc: Integer;
    mi : TTntMenuItem;
    WordInfo : TWordInfo;
begin
  // Remove all old item before undo if any
  while (MemoSubPopupMenu.Items[0] <> pmiMemoSubUndo) do
  begin
    MemoSubPopupMenu.Items.Delete(0);
  end;

  if (not MenuItemLiveSpellcheck.Checked) or (not FSpellChecker.IsInitialized) then
    Exit;

  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  AWord := Copy(MemoSubtitleText.Text, WordInfo.Position, WordInfo.Length);

  if FSpellChecker.Spell(AWord) AND FSpellChecker.IsThesaurusInitialized AND
    (FSpellChecker.GetTotalMeanings(AWord) > 0) then
  begin
    j := 0;
    mi := nil;
    for i := 0 to FSpellChecker.GetTotalMeanings(AWord)-1 do
    begin
      for ii := 0 to FSpellChecker.GetTotalSynonymsByMeaningIndex(AWord,i+1)-1 do
        begin
          mi := TTntMenuItem.Create(Self);
          mi.AutoHotkeys := maManual;
          mi.ImageIndex := 55;
          mi.Caption := FSpellChecker.GetSynonym(AWord,i+1,ii+1) + chr(32) + FSpellChecker.GetMeaning(AWord, i+1);
          mi.Hint := FSpellChecker.GetSynonym(AWord,i+1,ii+1);
          mi.OnClick := OnSpellcheckSuggestionMenuItemClick;
          MemoSubPopupMenu.Items.Insert(j, mi);
          Inc(j);
        end;

      // -
      mi := TTntMenuItem.Create(Self);
      mi.Caption := cLineCaption;
      MemoSubPopupMenu.Items.Insert(j, mi);
      Inc(j);

    end;
  end;

  if not FSpellChecker.Spell(AWord) then
  begin
    j := 0;
    mi := nil;
    Suggestions := TTntStringList.Create;
    FSpellChecker.Suggest(AWord, Suggestions);
    for i := 0 to Suggestions.Count-1 do
    begin
      mi := TTntMenuItem.Create(Self);
      mi.AutoHotkeys := maManual;
      mi.Caption := Suggestions[i];
      mi.Hint := Suggestions[i];
      mi.ImageIndex := 56;
      mi.OnClick := OnSpellcheckSuggestionMenuItemClick;
      MemoSubPopupMenu.Items.Insert(i, mi);
      Inc(j);
    end;
    if (Suggestions.Count = 0) then
    begin
      mi := TTntMenuItem.Create(Self);
      mi.Caption := '(No suggestions)';
      mi.ImageIndex := 56;
      mi.Enabled := False;
      MemoSubPopupMenu.Items.Insert(0, mi);
      Inc(j);
    end;
    Suggestions.Free;
    // Add to dictionary
    mi := TTntMenuItem.Create(Self);
    mi.Caption := '=> Add to dictionary';
    mi.ImageIndex := 19;
    mi.OnClick := OnSpellcheckAddToDictionaryMenuItemClick;
    MemoSubPopupMenu.Items.Insert(j, mi);
    Inc(j);
    // Ignore
    mi := TTntMenuItem.Create(Self);
    mi.Caption := '=> Ignore';
    mi.ImageIndex := 50;
    mi.OnClick := OnSpellcheckIgnoreMenuItemClick;
    MemoSubPopupMenu.Items.Insert(j, mi);
    Inc(j);
    // -
    mi := TTntMenuItem.Create(Self);
    mi.Caption := cLineCaption;
    MemoSubPopupMenu.Items.Insert(j, mi);
    Inc(j);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSpellcheckSuggestionMenuItemClick(Sender: TObject);
var TextBegin, TextEnd : WideString;
    P : TPoint;
    WordInfo : TWordInfo;
    SelStart : Integer;
begin
  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  TextBegin := Copy(MemoSubtitleText.Text, 1, WordInfo.Position - 1);
  TextEnd := Copy(MemoSubtitleText.Text, WordInfo.Position + WordInfo.Length, MaxInt);
  SelStart := MemoSubtitleText.SelStart;
  MemoSubtitleText.Text := TextBegin + (Sender as TTntMenuItem).Hint + TextEnd;
  MemoSubtitleText.SelStart := SelStart;
  CurrentProject.IsDirty := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSpellcheckAddToDictionaryMenuItemClick(Sender: TObject);
var P : TPoint;
    WordInfo : TWordInfo;
    AWord : WideString;
begin
  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  AWord := Copy(MemoSubtitleText.Text, WordInfo.Position, WordInfo.Length);
  FSpellChecker.Add(AWord);
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSpellcheckIgnoreMenuItemClick(Sender: TObject);
var P : TPoint;
    WordInfo : TWordInfo;
    AWord : WideString;
begin
  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  AWord := Copy(MemoSubtitleText.Text, WordInfo.Position, WordInfo.Length);
  FSpellChecker.Ignore(AWord);
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemGetMoreDictionariesClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PAnsiChar('http://extensions.openoffice.org/'), '', '', SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSpellCheckExecute(Sender: TObject);
var SpellUndoableTask : TUndoableMultiChangeTask;
    IniFile : TIniFile;
begin
  if (not FSpellChecker.IsInitialized) then
  begin
    Exit;
  end;

  if (SpellCheckForm = nil) then
  begin
    SpellCheckForm := TSpellCheckForm.Create(Self);
    IniFile := TIniFile.Create(GetIniFilename);
    LoadFormPosition(IniFile, SpellCheckForm);
    IniFile.Free;    
  end;
  SpellCheckForm.ShowModal;

  SpellUndoableTask := SpellCheckForm.GetUndoableTask;
  if Assigned(SpellUndoableTask) then
  begin
    if (SpellUndoableTask.GetCount > 0) then
    begin
      PushUndoableTask(SpellUndoableTask);
      CurrentProject.IsDirty := True;
      WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
      vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
      vtvSubsList.Repaint;
      SpellCheckForm.Reset;
    end
    else
    begin
      FreeAndNil(SpellUndoableTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.GetSpellChecker : THunspellChecker;
begin
  Result := FSpellChecker;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSpellCheckUpdate(Sender: TObject);
begin
  ActionSpellCheck.Enabled := FSpellChecker.IsInitialized;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionInsertSceneChangeExecute(Sender: TObject);
var UndoableInsertSceneChange : TUndoableInsertSceneChange;
    CursorPos : Integer;
begin
  CursorPos := WAVDisplayer.GetCursorPos;
  if g_SceneChangeWrapper.Contains(CursorPos,CursorPos) then
    Exit;

  UndoableInsertSceneChange := TUndoableInsertSceneChange.Create;
  UndoableInsertSceneChange.SetData(CursorPos);
  UndoableInsertSceneChange.DoTask;
  PushUndoableTask(UndoableInsertSceneChange);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPasteExecute(Sender: TObject);
var actCtrl : TWinControl;
    SubList : TList;
    UndoableMultiAddTask : TUndoableMultiAddTask;
begin
  actCtrl := Screen.ActiveControl;
  if (actCtrl is TCustomEdit) then
  begin
    TCustomEdit(actCtrl).PasteFromClipboard;
  end
  else if (actCtrl is TVirtualStringTree) then
  begin
    SubList := TList.Create;
    PasteClipboard(SubList, SubRangeFactory);
    if (SubList.Count > 0) then
    begin
      // TODO : maybe check for duplicate sub when pasting ?
      UndoableMultiAddTask := TUndoableMultiAddTask.Create;
      UndoableMultiAddTask.SetData(SubList);
      UndoableMultiAddTask.DoTask;
      PushUndoableTask(UndoableMultiAddTask);
      CurrentProject.IsDirty := True;
    end;
    SubList.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPasteUpdate(Sender: TObject);
var actCtrl : TWinControl;
begin
  actCtrl := Screen.ActiveControl;
  ActionPaste.Enabled := ((actCtrl is TCustomEdit) and TntClipboard.HasFormat(CF_TEXT))
    or ((actCtrl is TVirtualStringTree) and
      (TntClipboard.HasFormat(VSSClipBoardFORMAT) or TntClipboard.HasFormat(CF_TEXT)));
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPasteAtCursorExecute(Sender: TObject);
var SubList : TList;
    UndoableMultiAddTask : TUndoableMultiAddTask;
    SubRange : TSubtitleRange;
    Delay, i : Integer;
begin
  SubList := TList.Create;
  PasteClipboard(SubList, SubRangeFactory);
  if (SubList.Count > 0) then
  begin
    // Delay each subtitle so the start time of the first subtitle match
    // the cursor position
    SubRange := SubList[0];
    Delay := SubRange.StartTime - WAVDisplayer.GetCursorPos;
    for i:=0 to SubList.Count-1 do
    begin
      SubRange := SubList[i];
      SubRange.StartTime := SubRange.StartTime - Delay;
      SubRange.StopTime := SubRange.StopTime - Delay;
    end;
    // TODO : maybe check for duplicate sub when pasting ?
    UndoableMultiAddTask := TUndoableMultiAddTask.Create;
    UndoableMultiAddTask.SetData(SubList);
    UndoableMultiAddTask.DoTask;
    PushUndoableTask(UndoableMultiAddTask);
    CurrentProject.IsDirty := True;
  end;
  SubList.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionToggleVOExecute(Sender: TObject);
begin
  if not Assigned(CurrentProject) then // just to be sure
  begin
    TurnOffVO;
    AutoFitSubsListColumns;
    Exit;
  end;

  if WAVDisplayer.RangeListVO.Count > 0 then // VO are available
  begin
    if CurrentProject.ShowVO then
    begin
      TurnOffVO;
      CurrentProject.ShowVO := False;
      SaveProject(CurrentProject, True);
    end
    else
    begin
      TurnOnVO;
      CurrentProject.ShowVO := True;
      SaveProject(CurrentProject, True);
    end;
  end
  else // VO are unavailable
  begin
    // First of all try to reload
    if CurrentProject.SubtitlesVO <> '' then
      LoadVO(CurrentProject.SubtitlesVO);
    // If failed then ask for the file
    if WAVDisplayer.RangeListVO.Count <= 0 then
    begin
      TntOpenDialog1.FileName := CurrentProject.SubtitlesVO;
      TntOpenDialog1.Filter := 'SRT files (*.srt)|*.SRT' + '|' +
        'SSA/ASS files (*.ssa,*.ass)|*.SSA;*.ASS' + '|' +
        'All files (*.*)|*.*';
      SetOpenDialogPath(TntOpenDialog1);
      if TntOpenDialog1.Execute then
      begin
        if (CurrentProject.SubtitlesVO <> TntOpenDialog1.FileName) then
        begin
          CurrentProject.SubtitlesVO := TntOpenDialog1.FileName;
          SaveProject(CurrentProject, True);
        end;
        LoadVO(CurrentProject.SubtitlesVO);
      end;
    end;
    // Update preferences...
    if (WAVDisplayer.RangeListVO.Count > 0) and (not CurrentProject.ShowVO) then
    begin
      CurrentProject.ShowVO := True;
      SaveProject(CurrentProject, True);
    end;
    // ...and adjust the visual part
    AdjustShowVO;
  end;

  AutoFitSubsListColumns;

end;

//------------------------------------------------------------------------------

procedure TMainForm.TurnOffVO;
var VOColumn : TVirtualTreeColumn;
begin
  SplitterSubtitleVO.Visible := False;
  MemoSubtitleVO.Visible := False;
  pmiWAVDispCreateFromVO.Enabled := False;
  VOColumn := vtvSubsList.Header.Columns.Items[VO_COL_INDEX];
  ShowColumn(VOColumn, False);
  WAVDisplayer.ShowVO(False);
  WAVDisplayer.UpdateView([uvfPageSize]);
  TabSheetOneLook.TabVisible := False;
  TabSheetUrbanDictionary.TabVisible := False;
  TabSheetWordNet.TabVisible := False;
  TabSheetWordReference.TabVisible := False;
  TabSheetTheFreeDictionary.TabVisible := False;
end;

procedure TMainForm.TurnOnVO;
var VOColumn : TVirtualTreeColumn;
begin
  MemoSubtitleVO.Visible := True;
  pmiWAVDispCreateFromVO.Enabled := True;
  SplitterSubtitleVO.Visible := True;
  VOColumn := vtvSubsList.Header.Columns.Items[VO_COL_INDEX];
  ShowColumn(VOColumn, True);
  if ConfigObject.DisableVOShowTextInWAVDisplay = False then WAVDisplayer.ShowVO(True);
  WAVDisplayer.UpdateView([uvfPageSize]);
end;

procedure TMainForm.AdjustShowVO;
begin
  if WAVDisplayer.RangeListVO.Count > 0 then
    if CurrentProject.ShowVO then
      TurnOnVO
    else
      TurnOffVO
  else
    TurnOffVO
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideSubsExecute(Sender: TObject);
begin
  if ShowingSubs then
  begin // Hide subs
    VideoRenderer.HideSubtitle(True);
    ShowingSubs := False;
  end
  else
  begin // Show subs
    VideoRenderer.HideSubtitle(False);
    ShowingSubs := True;
  end;

  if ShowingVideo then
  begin
    if (VideoRenderer.IsPlaying or VideoRenderer.IsPaused) then
    begin
      UpdateSubtitleForPreview(True);
    end
    else
    begin
      ActionShowStartFrameExecute(nil);
    end;
  end;

  MenuShowHideSubtitles.Checked := ShowingSubs;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateDurationWithSubOnly;
var LastSub : TRange;
begin
  if (not (WAVDisplayer.IsPeakDataLoaded or VideoRenderer.IsOpen)) and (WAVDisplayer.RangeList.Count > 0) then
  begin
    // Use last subtitle stop time + 1 minute
    LastSub := WAVDisplayer.RangeList[WAVDisplayer.RangeList.Count - 1];
    WAVDisplayer.Length := LastSub.StopTime + (1 * 60 * 1000);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTranslationTemplateExecute(Sender: TObject);
var Ext, TranslatedFilename : WideString;
    Translator : TTranslator;
    SubtitleRange, NewRange : TSubtitleRange;
    i : integer;
    UndoableMultiAddTask : TUndoableMultiAddTask;
    SubList : TList;
begin
  if not CheckSubtitlesAreSaved then
  begin
    Exit;
  end;

  TranslateForm := TTranslateForm.Create(nil);

  // Prefill the dialog
  Ext := WideExtractFileExt(CurrentProject.SubtitlesFile);
  TranslatedFilename := Copy(CurrentProject.SubtitlesFile, 1,
    Length(CurrentProject.SubtitlesFile) - Length(Ext)) + '.translated' + Ext;
  TranslateForm.SetTargetFile(TranslatedFilename);
  TranslateForm.SetHasVO(CurrentProject.SubtitlesVO <> '');

  if TranslateForm.ShowModal = mrOk then
  begin
    if (TranslateForm.GetTranslateSelectionType = tstAll) then
    begin
      Translator := TranslateForm.GetTranslator;
      SaveSubtitles(TranslateForm.GetTargetFile, CurrentProject.SubtitlesFile, CurrentProject.IsUTF8,
        False, Translator, False);
      FreeAndNil(Translator);

      ErrorReportForm.Clear;

      CurrentProject.SubtitlesVO := CurrentProject.SubtitlesFile;
      CurrentProject.SubtitlesFile := TranslateForm.GetTargetFile;

      LoadSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
      LoadVO(CurrentProject.SubtitlesVO);

      if (VideoRenderer.IsOpen) then
        VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);

      SelectNode(vtvSubsList.GetFirst);

      TurnOnVO;
      CurrentProject.ShowVO := True;
      SaveProject(CurrentProject, True);

      ClearStack(RedoStack);
      ClearStack(UndoStack);
      ActionUndo.Enabled := False;
      ActionRedo.Enabled := False;
    end
    else if (TranslateForm.GetTranslateSelectionType = tstMissingOnly) then
    begin
      g_WebRWSynchro.BeginWrite;
      try
        Translator := TranslateForm.GetTranslator;
        SubList := TList.Create;
        for i:=0 to WAVDisplayer.RangeListVO.Count-1 do
        begin
          SubtitleRange := TSubtitleRange(WAVDisplayer.RangeListVO[i]);
          if WAVDisplayer.RangeList.GetRangeIdxAt(SubtitleRange.GetMiddle) = -1 then
          begin
            NewRange := TSubtitleRange(SubRangeFactory.CreateRange);
            NewRange.Assign(SubtitleRange);
            NewRange.Text := Translator.Translate(SubtitleRange.Text);
            SubList.Add(NewRange);
          end;
        end;
        FreeAndNil(Translator);

        if (SubList.Count > 0) then
        begin
          UndoableMultiAddTask := TUndoableMultiAddTask.Create;
          UndoableMultiAddTask.SetData(SubList);
          UndoableMultiAddTask.DoTask;
          PushUndoableTask(UndoableMultiAddTask);
          CurrentProject.IsDirty := True;
        end;
        FreeAndNil(SubList);
      finally
        g_WebRWSynchro.EndWrite;
      end;
      UpdateDurationWithSubOnly;
      WAVDisplayer.UpdateView([uvfRange]);
      vtvSubsList.Repaint;
    end;
  end;
  FreeAndNil(TranslateForm);
end;

procedure TMainForm.MoveToFromCurrentToEnd(DelayToApply : integer; CheckOverlap : boolean; UpdateIconIndex : Integer);
var Node, PreviousNode : PVirtualNode;
    NodeData, PreviousNodeData : PTreeData;
    UndoableMoveTask : TUndoableMultiPurposeTask;
    UndoableMinBlTask : TUndoableSetTimeTask;
    UndoableDelayTask : TUndoableDelayTask;
    UndoableIconTask : TUndoableIconTask;
begin
  UndoableMoveTask := TUndoableMultiPurposeTask.Create;

  Node := vtvSubsList.FocusedNode;
  NodeData := vtvSubsList.GetNodeData(Node);

  if CheckOverlap And (DelayToApply < 0) then
  begin
    PreviousNode := vtvSubsList.FocusedNode.PrevSibling;
    if Assigned(PreviousNode) then
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      PreviousNodeData := vtvSubsList.GetNodeData(PreviousNode);
      if ((PreviousNodeData.Range.StopTime + WAVDisplayer.MinimumBlank - DelayToApply >= NodeData.Range.StartTime) and
          (Copy(PreviousNodeData.Range.Text,1,1) <> '{') and
          (Copy(NodeData.Range.Text,1,1) <> '{')) then
      begin
        UndoableMinBlTask := TUndoableSetTimeTask.Create;
        UndoableMinBlTask.SetData(PreviousNodeData.Range.Node.Index,
                                  PreviousNodeData.Range.StartTime,
                                  PreviousNodeData.Range.StopTime,
                                  PreviousNodeData.Range.StartTime,
                                  NodeData.Range.StartTime - WAVDisplayer.MinimumBlank + DelayToApply);
        UndoableMoveTask.AddData(UndoableMinBlTask);
      end;
    end;
  end;

  UndoableDelayTask := TUndoableDelayTask.Create;
  UndoableDelayTask.SetDelayInMs(DelayToApply);
  UndoableDelayTask.SetDelayShiftType(TDelayShiftType(0));

  if Assigned(Node) then
  begin
    UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount - Node.Index);
    if (UpdateIconIndex >= 1) And (Node.Index >= 0) then
    begin
      UndoableIconTask := TUndoableIconTask.Create;
      UndoableIconTask.SetIcon(Node.Index, UpdateIconIndex);
      UndoableMoveTask.AddData(UndoableIconTask);
    end;
  end;
  while Assigned(Node) do
  begin
    UndoableDelayTask.AddSubtitleIndex(Node.Index);
    Node := vtvSubsList.GetNext(Node);
  end;
  UndoableMoveTask.AddData(UndoableDelayTask);
  UndoableMoveTask.DoTask;
  PushUndoableTask(UndoableMoveTask);

  Node := vtvSubsList.GetFirst;
  if Assigned(Node) then
   begin
     NodeData := vtvSubsList.GetNodeData(Node);
     if NodeData.Range.StartTime < 0 then
      begin
       ActionUndoExecute(self);
       MessageDlg('Subtitle with negative start time are not allowed (an undo Ctrl+Z has been applied)', mtWarning, [MbOk], 0);
      end;
   end;

end;

procedure TMainForm.FillZone(ZoneList : TList);
var i : Integer;
    SilentRange : TSilentRangeInfo;
    Str : WideString;
    SilentDuration, MinDuration : Integer;
    MinumiumNewSilentZone : integer;
begin
  MinDuration :=udDuration.Position;
  ComboBoxSilentZone.Items.BeginUpdate;
  MinumiumNewSilentZone := 0;
  for i:=0 to ZoneList.Count-1 do
  begin
    SilentRange := TSilentRangeInfo(ZoneList[i]);
    SilentDuration := SilentRange.Stop - SilentRange.Start;
    if (SilentDuration >= MinDuration) then
     begin
      if SilentRange.Start < MinumiumNewSilentZone then
        continue;
      MinumiumNewSilentZone := SilentRange.Stop + udMinimunBetweenZones.position * 1000;
      Str := Format('%s -> %s (%.1f)', [TimeMsToString(SilentRange.Start),
        TimeMsToString(SilentRange.Stop),
        (SilentRange.RmsSum / SilentRange.RmsCount)]);
      ComboBoxSilentZone.Items.AddObject(Str, SilentRange);
     end;
  end;
  ComboBoxSilentZone.Items.EndUpdate;
  SpeedButtonGoCurrentSilentZone.enabled := False;
  if ComboBoxSilentZone.Items.Count > 0 then SpeedButtonGoCurrentSilentZone.enabled := True;
end;

//------------------------------------------------------------------------------
{procedure TMainForm.bttFastSmartWavCursorClick(Sender: TObject);
 var
  Node,PreviousNode : PVirtualNode;
  NodeData,PreviousNodeData : PTreeData;
  CursorPos : integer;
  CheckOverlap : boolean; SetMaskIndex : Integer;
  UndoableDelayTask : TUndoableDelayTask;
begin


end;}

procedure TMainForm.SpeedButtonUpQuickSilentZoneClick(Sender: TObject);
begin
  ComboBoxSilentZone.Items.Clear;
  FZoneList.Clear;
  WAVDisplayer.DetectSilentZone(FZoneList, udThreshold.Position, 100);
  FillZone(FZoneList);
end;

procedure TMainForm.ComboBoxSilentZoneSelect(Sender: TObject);
 var SilentRange : TSilentRangeInfo;
begin
  if (ComboBoxSilentZone.ItemIndex <> -1) then
  begin
    SilentRange := TSilentRangeInfo(ComboBoxSilentZone.Items.Objects[ComboBoxSilentZone.ItemIndex]);
    SetSelection(SilentRange.Start, SilentRange.Stop);
    PanelWavDisplay.SetFocus;
  end;
end;

procedure TMainForm.PageQuickSmartSilentZonePanelsChange(Sender: TObject);
begin
 if PageQuickSmartSilentZonePanels.ActivePage = TabSheetSilentZone then
  begin
   if ComboBoxSilentZone.Items.Count = 0 then
    begin
     SpeedButtonUpQuickSilentZoneClick(self);
    end;
  end;
end;

procedure TMainForm.MemoSubtitleVOSelectionChange(Sender: TObject);
 var Selection,SelectionForIdiomDictionary : String;
begin

 if Not ConfigObject.DictionaryOnLineEnabled then Exit;

 Selection := MemoSubtitleVo.SelText;

 SaveCompleteSelectionForMemoSubtitleVO := Selection;

 if pos(chr(32), Selection) > 0 then
  Selection := copy(Selection,1,pos(chr(32), Selection));

 SaveSelectionForMemoSubtitleVO := Selection;

end;

procedure TMainForm.PageControlMainChange(Sender: TObject);
begin
 if PageControlMain.ActivePage = TabSheetWordReference then
  begin
   with WebBrowserWordReference do
    if Document <> nil then
      with Application as IOleobject do
       DoVerb(OLEIVERB_UIACTIVATE, nil, WebBrowserWordReference, 0, Handle, GetClientRect);
  end;
 if PageControlMain.ActivePage = TabSheetTheFreeDictionary then
  begin
   with WebBrowserTheFreeDictionary do
    if Document <> nil then
      with Application as IOleobject do
       DoVerb(OLEIVERB_UIACTIVATE, nil, WebBrowserTheFreeDictionary, 0, Handle, GetClientRect);
  end;
 if PageControlMain.ActivePage = TabSheetWordNet then
  begin
   with WebBrowserWordNet do
    if Document <> nil then
      with Application as IOleobject do
       DoVerb(OLEIVERB_UIACTIVATE, nil, WebBrowserWordNet, 0, Handle, GetClientRect);
  end;
 if PageControlMain.ActivePage = TabSheetUrbanDictionary then
  begin
   with WebBrowserUrbanDictionary do
    if Document <> nil then
      with Application as IOleobject do
       DoVerb(OLEIVERB_UIACTIVATE, nil, WebBrowserUrbanDictionary, 0, Handle, GetClientRect);
  end;
 if PageControlMain.ActivePage = TabSheetOneLook then
  begin
   with WebBrowserOneLook do
    if Document <> nil then
      with Application as IOleobject do
       DoVerb(OLEIVERB_UIACTIVATE, nil, WebBrowserOneLook, 0, Handle, GetClientRect);
  end;
end;

procedure TMainForm.MemoTextPipeSelectionChange(Sender: TObject);
 var Selection : String;
begin

 if Not ConfigObject.DictionaryOnLineEnabled Or ConfigObject.DictionaryOnLineExcludeTextPipe then Exit;

 Selection := MemoTextPipe.SelText;
 if pos(chr(32), Selection) > 0 then
  Selection := copy(Selection,1,pos(chr(32), Selection));
 SaveSelectionForMemoTextPipe := Selection;
end;

procedure TMainForm.MemoTextPipeEnter(Sender: TObject);
begin
  if (CurrentProject.Filename = '') Or Not ConfigObject.DictionaryOnLineEnabled Or ConfigObject.DictionaryOnLineExcludeTextPipe then Exit;
  if ConfigObject.OnLineSearchDict1 = true then begin TabSheetWordReference.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict2 = true then begin TabSheetWordNet.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict3 = true then begin TabSheetUrbanDictionary.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict4 = true then begin TabSheetOneLook.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict5 = true then begin TabSheetTheFreeDictionary.TabVisible := True; end;
end;

procedure TMainForm.MemoSubtitleVOEnter(Sender: TObject);
begin
  if (CurrentProject.Filename = '') OR Not ConfigObject.DictionaryOnLineEnabled then Exit;
  if ConfigObject.OnLineSearchDict1 = true then begin TabSheetWordReference.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict2 = true then begin TabSheetWordNet.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict3 = true then begin TabSheetUrbanDictionary.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict4 = true then begin TabSheetOneLook.TabVisible := True; end;
  if ConfigObject.OnLineSearchDict5 = true then begin TabSheetTheFreeDictionary.TabVisible := True; end;
end;

procedure TMainForm.vtvSubsListHeaderDragged(Sender: TVTHeader;
  Column: TColumnIndex; OldPosition: Integer);
begin
 AutoFitSubsListColumns;
end;

procedure TMainForm.ActionDelaySynchToCursorExecute(Sender: TObject);
 var
  Node,PreviousNode : PVirtualNode;
  NodeData,PreviousNodeData : PTreeData;
  CursorPos : integer;
  CheckOverlap : boolean; SetMaskIndex : Integer;
  UndoableDelayTask : TUndoableDelayTask;
begin

 try
  Node := vtvSubsList.FocusedNode;
  NodeData := vtvSubsList.GetNodeData(Node);
 except
  MessageDlg('Error retrieving current node data retrieve.', mtWarning, [MbOk], 0);
 end;

 try
  CursorPos := WAVDisplayer.GetCursorPos;
 except
  MessageDlg('Error retrieving cursor position.', mtWarning, [MbOk], 0);
 end;

 try
  PreviousNode := vtvSubsList.FocusedNode.PrevSibling;
  if Assigned(PreviousNode) then
   PreviousNodeData := vtvSubsList.GetNodeData(PreviousNode);
 except
  MessageDlg('Error retrieving previous node data.', mtWarning, [MbOk], 0);
 end;

 try
  if Assigned(PreviousNode) then
   begin
    if (CursorPos >= PreviousNodeData.Range.StartTime) AND (CursorPos <= PreviousNodeData.Range.StopTime) then
     begin
      if CursorPos - WAVDisplayer.MinimumBlank <= PreviousNodeData.Range.StartTime then
       Exit;
     end;
    if (CursorPos >= PreviousNodeData.Range.StopTime) AND (CursorPos <= PreviousNodeData.Range.StopTime + WAVDisplayer.MinimumBlank) then
     begin
       WAVDisplayer.SetCursorPos(PreviousNodeData.Range.StopTime + WAVDisplayer.MinimumBlank);
       CursorPos := WAVDisplayer.GetCursorPos;
     end;
   end;
 except
  MessageDlg('Error checking minimium blank.', mtWarning, [MbOk], 0);
 end;

 CheckOverlap := true;
 if Assigned(PreviousNode) then
  if (CursorPos >= PreviousNodeData.Range.StartTime) AND (CursorPos <= PreviousNodeData.Range.StopTime) then
   begin
     CheckOverlap := false;
  end;

 try
  if Assigned(PreviousNode) then
   if (chkFastSmartWavCursor.Checked = False ) AND(CursorPos >= PreviousNodeData.Range.StartTime) AND (CursorPos <= PreviousNodeData.Range.StopTime) then
    begin
      UndoableDelayTask := TUndoableDelayTask.Create;
      UndoableDelayTask.SetDelayInMs(CursorPos - PreviousNodeData.Range.StopTime - WAVDisplayer.MinimumBlank);
      UndoableDelayTask.SetDelayShiftType(dstStopTimeOnly);
      UndoableDelayTask.SetCapacity(1);
      UndoableDelayTask.AddSubtitleIndex(vtvSubsList.FocusedNode.Index-1);
      UndoableDelayTask.DoTask;
      PushUndoableTask(UndoableDelayTask);
    end;
 except
  MessageDlg('Error positioning previous subtitle.', mtWarning, [MbOk], 0);
 end;

 try
  if chkFastSmartWavCursor.Checked = False then
   begin
    MoveToFromCurrentToEnd(CursorPos - NodeData.Range.StartTime, CheckOverlap, 1);
   end
  else
   begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(CursorPos - NodeData.Range.StartTime);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);

    Node := vtvSubsList.GetFirst;
    if Assigned(Node) then
     begin
       NodeData := vtvSubsList.GetNodeData(Node);
       if NodeData.Range.StartTime < 0 then
        begin
         ActionUndoExecute(self);
         MessageDlg('Subtitle with negative start time are not allowed (an undo Ctrl+Z has been applied)', mtWarning, [MbOk], 0);
         Exit;
        end;
     end;

   end;
 except
  MessageDlg('Error positioning current subtitle.', mtWarning, [MbOk], 0);
 end;

 try
  if ConfigObject.QuickSmartSilentZonePanels and (ConfigObject.SpaceKeyMoveAfterSmartButtonIsUsed > 0) then
   begin
    if CursorPos - ConfigObject.SpaceKeyMoveAfterSmartButtonIsUsed>0 then
      WAVDisplayer.SetCursorPos(CursorPos - ConfigObject.SpaceKeyMoveAfterSmartButtonIsUsed)
    else
      WAVDisplayer.SetCursorPos(1);
    if ConfigObject.AutoPlaySmartButtonIsUsed then
     begin
      ActionPlayExecute(nil);
     end;
   end;
 except
  MessageDlg('Error positioning cursor and autoplay.', mtWarning, [MbOk], 0);
 end;

end;

procedure TMainForm.ActionDelayMoveNegative50Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(-50,true, 2);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(-50);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.ActionDelayMoveNegative10Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(-10,true, 2);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(-10);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.ActionDelayMoveNegative200Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(-200,true, 2);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(-200);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.ActionDelayMoveNegative100Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(-100,true, 2);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(-100);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.ActionDelayMovePositive10Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(10, true, 3);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(10);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.ActionDelayMovePositive50Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(50, true, 3);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(50);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.ActionDelayMovePositive100Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(100,true, 3);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(100);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.ActionDelayMovePositive200Execute(Sender: TObject);
var
  Node : PVirtualNode;
  UndoableDelayTask : TUndoableDelayTask;
begin
 if chkFastSmartWavCursor.Checked = False then
  begin
    MoveToFromCurrentToEnd(200, true, 3);
  end
 else
  begin
     UndoableDelayTask := TUndoableDelayTask.Create;
     UndoableDelayTask.SetDelayInMs(200);
     UndoableDelayTask.SetDelayShiftType(dstBothTime);
     UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
     Node := vtvSubsList.GetFirst;
     while Assigned(Node) do
     begin
       UndoableDelayTask.AddSubtitleIndex(Node.Index);
       Node := vtvSubsList.GetNext(Node);
     end;
    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
end;

procedure TMainForm.MemoSubtitleVOMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Not ConfigObject.DictionaryOnLineEnabled then Exit;
 MouseLeftButtonIsUpOnMemoSubtitleVO := false;
end;

procedure TMainForm.MemoSubtitleVOMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
 var I : word; SaveCompleteSelectionForMemoSubtitleVOLocal : String;
begin

 if Not ConfigObject.DictionaryOnLineEnabled then Exit;

 MouseLeftButtonIsUpOnMemoSubtitleVO := true;

 if SaveSelectionForMemoSubtitleVO = '' then Exit;

 if Length(SaveSelectionForMemoSubtitleVO)>=2 then
  begin

   try

    // Wordreference.com
    if ConfigObject.OnLineSearchDict1 = true then
     begin
      WebBrowserWordReference.Navigate('http://api.wordreference.com/f9b5b/'+lowercase(copy(ConfigObject.WordReferenceLanguage,1,4))+'/'+lowercase(SaveSelectionForMemoSubtitleVO));
      //WebBrowserWordReferenceThesaurus.Navigate('http://api.wordreference.com/f9b5b/thesaurus/'+lowercase(SaveSelectionForMemoSubtitleVO));
      if TabSheetWordReference.TabVisible = False then TabSheetWordReference.TabVisible := True;
      TabSheetWordReference.Caption := 'Word Reference [ ' + uppercase(SaveSelectionForMemoSubtitleVO) + ' ]';
     end;
    // -------------------------

    // TheFreeDictionary
    if ConfigObject.OnLineSearchDict5 = true then
     begin
      WebBrowserTheFreeDictionary.Navigate('http://www.thefreedictionary.com/' + lowercase(SaveSelectionForMemoSubtitleVO));
      if TabSheetTheFreeDictionary.TabVisible = False then TabSheetTheFreeDictionary.TabVisible := True;
      TabSheetTheFreeDictionary.Caption := 'TheFreeDictionary [ ' + uppercase(SaveSelectionForMemoSubtitleVO) + ' ]';
     end;
    // -------------------------

    // WordNet
    if ConfigObject.OnLineSearchDict2 = true then
     begin
      WebBrowserWordNet.Navigate('http://wordnetweb.princeton.edu/perl/webwn?s=' + lowercase(SaveSelectionForMemoSubtitleVO) + '&sub=Search+WordNet&o2=&o0=1&o7=&o5=&o1=1&o6=&o4=&o3=&h=');
      if TabSheetWordNet.TabVisible = False then TabSheetWordNet.TabVisible := True;
      TabSheetWordNet.Caption := 'WordNet [ ' + uppercase(SaveSelectionForMemoSubtitleVO) + ' ]';
     end;
    // -------------------------

    // Urban Dictionary
    if ConfigObject.OnLineSearchDict3 = true then
     begin
      SaveCompleteSelectionForMemoSubtitleVOLocal := SaveCompleteSelectionForMemoSubtitleVO;
      for I:= 33 to 47 do
       begin
        SaveCompleteSelectionForMemoSubtitleVOLocal := AnsiReplaceStr(SaveCompleteSelectionForMemoSubtitleVOLocal,chr(I),'%'+IntToHex(I,2));
       end;
      for I:= 58 to 64 do
       begin
        SaveCompleteSelectionForMemoSubtitleVOLocal := AnsiReplaceStr(SaveCompleteSelectionForMemoSubtitleVOLocal,chr(I),'%'+IntToHex(I,2));
       end;
      SaveCompleteSelectionForMemoSubtitleVOLocal := AnsiReplaceStr(SaveCompleteSelectionForMemoSubtitleVO,chr(32),'+');
      WebBrowserUrbanDictionary.Navigate('http://www.urbandictionary.com/define.php?term='+lowercase(SaveCompleteSelectionForMemoSubtitleVOLocal));
      if TabSheetUrbanDictionary.TabVisible = False then TabSheetUrbanDictionary.TabVisible := True;
      TabSheetUrbanDictionary.Caption := 'Urban Dictionary [ ' + uppercase(SaveCompleteSelectionForMemoSubtitleVO) + ' ]';
     end;
    // -------------------------

    // Onelook
    if ConfigObject.OnLineSearchDict4 = true then
     begin
      WebBrowserOneLook.Navigate('http://www.onelook.com/?w=' + lowercase(SaveSelectionForMemoSubtitleVO) + '&ls=a');
      if TabSheetOneLook.TabVisible = False then TabSheetOneLook.TabVisible := True;
      TabSheetOneLook.Caption := 'OneLook [ ' + uppercase(SaveSelectionForMemoSubtitleVO) + ' ]';
     end;
    // -------------------------

   except
   end;

  end;
end;

procedure TMainForm.MemoTextPipeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Not ConfigObject.DictionaryOnLineEnabled Or ConfigObject.DictionaryOnLineExcludeTextPipe then Exit;
 MouseLeftButtonIsUpOnMemoTextPipe := false;
end;

procedure TMainForm.MemoTextPipeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var I : word; SaveCompleteSelectionForMemoTextPipeVOLocal : String;
begin

 if Not ConfigObject.DictionaryOnLineEnabled Or ConfigObject.DictionaryOnLineExcludeTextPipe then Exit;

 MouseLeftButtonIsUpOnMemoTextPipe := true;

 if SaveSelectionForMemoTextPipe = '' then Exit;

 if Length(SaveSelectionForMemoTextPipe)>=3 then
  begin

   try

    // Wordreference.com
    if ConfigObject.OnLineSearchDict1 = true then
     begin
      WebBrowserWordReference.Navigate('http://api.wordreference.com/b396c/enit/'+lowercase(SaveSelectionForMemoTextPipe));
      //WebBrowserWordReferenceThesaurus.Navigate('http://api.wordreference.com/b396c/thesaurus/'+lowercase(SaveSelectionForMemoTextPipe));
      if TabSheetWordReference.TabVisible = False then TabSheetWordReference.TabVisible := True;
      TabSheetWordReference.Caption := 'Word Reference [ ' + uppercase(SaveSelectionForMemoTextPipe) + ' ]';
     end;
    // -------------------------

    // TheFreeDictionary
    if ConfigObject.OnLineSearchDict5 = true then
     begin
      WebBrowserTheFreeDictionary.Navigate('http://www.thefreedictionary.com/' + lowercase(SaveSelectionForMemoTextPipe));
      if TabSheetTheFreeDictionary.TabVisible = False then TabSheetTheFreeDictionary.TabVisible := True;
      TabSheetTheFreeDictionary.Caption := 'TheFreeDictionary [ ' + uppercase(SaveSelectionForMemoTextPipe) + ' ]';
     end;
    // -------------------------

    // WordNet
    if ConfigObject.OnLineSearchDict2 = true then
     begin
      WebBrowserWordNet.Navigate('http://wordnetweb.princeton.edu/perl/webwn?s=' + lowercase(SaveSelectionForMemoTextPipe) + '&sub=Search+WordNet&o2=&o0=1&o7=&o5=&o1=1&o6=&o4=&o3=&h=');
      if TabSheetWordNet.TabVisible = False then TabSheetWordNet.TabVisible := True;
      TabSheetWordNet.Caption := 'WordNet [ ' + uppercase(SaveSelectionForMemoTextPipe) + ' ]';
     end;
    // -------------------------

    // Urban Dictionary
    if ConfigObject.OnLineSearchDict3 = true then
     begin
      SaveCompleteSelectionForMemoTextPipeVOLocal := SaveSelectionForMemoTextPipe;
      for I:= 33 to 47 do
       begin
        SaveCompleteSelectionForMemoTextPipeVOLocal := AnsiReplaceStr(SaveCompleteSelectionForMemoTextPipeVOLocal,chr(I),'%'+IntToHex(I,2));
       end;
      for I:= 58 to 64 do
       begin
        SaveCompleteSelectionForMemoTextPipeVOLocal := AnsiReplaceStr(SaveCompleteSelectionForMemoTextPipeVOLocal,chr(I),'%'+IntToHex(I,2));
       end;
      SaveCompleteSelectionForMemoTextPipeVOLocal := AnsiReplaceStr(SaveCompleteSelectionForMemoTextPipeVOLocal,chr(32),'+');
      WebBrowserUrbanDictionary.Navigate('http://www.urbandictionary.com/define.php?term='+lowercase(SaveCompleteSelectionForMemoTextPipeVOLocal));
      if TabSheetUrbanDictionary.TabVisible = False then TabSheetUrbanDictionary.TabVisible := True;
      TabSheetUrbanDictionary.Caption := 'Urban Dictionary [ ' + uppercase(SaveCompleteSelectionForMemoTextPipeVOLocal) + ' ]';
     end;
    // -------------------------

    // Onelook
    if ConfigObject.OnLineSearchDict4 = true then
     begin
      WebBrowserOneLook.Navigate('http://www.onelook.com/?w=' + lowercase(SaveSelectionForMemoTextPipe) + '&ls=a');
      if TabSheetOneLook.TabVisible = False then TabSheetOneLook.TabVisible := True;
      TabSheetOneLook.Caption := 'OneLook [ ' + uppercase(SaveSelectionForMemoTextPipe) + ' ]';
     end;
    // -------------------------

   except
   end;

  end;


end;

procedure TMainForm.ActionScrollTextPipeDownExecute(Sender: TObject);
var
  ScrollMessage:TWMVScroll;
begin
  if MemoTextPipe.Visible = False then Exit;
  ScrollMessage.Msg:=WM_VScroll;
  ScrollMessage.ScrollCode:=sb_LineDown;
  ScrollMessage.Pos:=0;
  MemoTextPipe.Dispatch(ScrollMessage) ;
end;

procedure TMainForm.ActionScrollTextPipeUpExecute(Sender: TObject);
var
  ScrollMessage:TWMVScroll;
begin
  if MemoTextPipe.Visible = False then Exit;
  ScrollMessage.Msg:=WM_VScroll;
  ScrollMessage.ScrollCode:=sb_LineUp;
  ScrollMessage.Pos:=0;
  MemoTextPipe.Dispatch(ScrollMessage) ;
end;

procedure TMainForm.SpeedButtonGoCurrentSilentZoneClick(Sender: TObject);
begin
 ComboBoxSilentZoneSelect(nil);
end;

procedure TMainForm.ActionMoveForwardToClosestSilentZoneExecute(
  Sender: TObject);
var
  I,CursorPosition, StartSilence : Integer;
  SilentRange : TSilentRangeInfo;
  SilentDuration, MinDuration : Integer;
  Str : WideString;
begin
 MinDuration :=udDuration.Position;
 CursorPosition := GetCursorPosition;
 for i:=0 to FZoneList.Count-1 do
  begin
    SilentRange := TSilentRangeInfo(FZoneList[i]);
    StartSilence := SilentRange.Start;
    SilentDuration := SilentRange.Stop - SilentRange.Start;
    if (SilentDuration >= MinDuration) then
     if StartSilence>CursorPosition then
      begin
       Str := Format('%s -> %s (%.1f)', [TimeMsToString(SilentRange.Start),
         TimeMsToString(SilentRange.Stop),
         (SilentRange.RmsSum / SilentRange.RmsCount)]);
       if ComboBoxSilentZone.Items.IndexOf(Str)>-1 then
        begin
         WAVDisplayer.SetCursorPos(SilentRange.Start);
         ComboBoxSilentZone.ItemIndex:=ComboBoxSilentZone.Items.IndexOf(Str);
         ComboBoxSilentZoneSelect(nil);
         Break;
        end;
      end;
  end;
end;

procedure TMainForm.ActionMoveBackwardToClosestSilentZoneExecute(
  Sender: TObject);
var
  I,CursorPosition, StartSilence : Integer;
  SilentRange : TSilentRangeInfo;
  SilentDuration, MinDuration : Integer;
  Str : WideString;
begin
 MinDuration :=udDuration.Position;
 CursorPosition := GetCursorPosition;
 for i:=FZoneList.Count-1 downto 0 do
  begin
    SilentRange := TSilentRangeInfo(FZoneList[i]);
    StartSilence := SilentRange.Start;
    SilentDuration := SilentRange.Stop - SilentRange.Start;
    if (SilentDuration >= MinDuration) then
     if StartSilence<CursorPosition then
      begin
       Str := Format('%s -> %s (%.1f)', [TimeMsToString(SilentRange.Start),
         TimeMsToString(SilentRange.Stop),
         (SilentRange.RmsSum / SilentRange.RmsCount)]);
       if ComboBoxSilentZone.Items.IndexOf(Str)>-1 then
        begin
         WAVDisplayer.SetCursorPos(SilentRange.Start);
         ComboBoxSilentZone.ItemIndex:=ComboBoxSilentZone.Items.IndexOf(Str);
         ComboBoxSilentZoneSelect(nil);
         Break;
        end;
      end;
  end;
end;

procedure TMainForm.ActionShiftForwardWaveFormExecute(Sender: TObject);
begin
 WavDisplayer.ShiftPeakData(True);
end;

procedure TMainForm.ActionShiftBackwardWaveFormExecute(Sender: TObject);
begin
 WavDisplayer.ShiftPeakData(False)
end;

function TMainForm.GetSubIcon(Index : Integer) : Integer;
//var Icon : Integer;
begin
  if g_GlobalContext.SubList.Count > 0 then
  try
    Result := g_GlobalContext.SubList[Index].IconIndex;
  except
    Result := 0;
  end;
end;

procedure TMainForm.ActionResynchToolsSetPosExecute(Sender: TObject);
var UndoableResTask : TUndoableMultiPurposeTask;
    fs : TSubtitleRange;
begin
  fs := MainForm.GetFirstSelected;
  if fs = nil then fs := MainForm.GetFirst;
  UndoableResTask := TUndoableMultiPurposeTask.Create;
  UndoableResTask := ResynchToolForm.SetPos(fs, WAVDisplayer.GetCursorPos);
  if UndoableResTask <> nil then
   begin
    PushUndoableTask(UndoableResTask);
    CheckResynchToolMenu;
   end;
end;

procedure TMainForm.ActionResynchToolsDelPosExecute(Sender: TObject);
var UndoableDeleteTask : TUndoableMultiPurposeTask;
    fs : TSubtitleRange;
begin
  fs := MainForm.GetFirstSelected;
  if fs <> nil then
  begin
    UndoableDeleteTask := TUndoableMultiPurposeTask.Create;
    UndoableDeleteTask := ResynchToolForm.DeletePos(fs);
    if UndoableDeleteTask <> nil then
     begin
      PushUndoableTask(UndoableDeleteTask);
      CheckResynchToolMenu;
     end;
  end;
end;

procedure TMainForm.ActionResynchToolsResetExecute(Sender: TObject);
 var UndoableResetTask : TUndoableMultiPurposeTask;
begin
  UndoableResetTask := ResynchToolForm.Reset;
  if UndoableResetTask <> nil then
   begin
    PushUndoableTask(UndoableResetTask);
    CheckResynchToolMenu;
   end;
end;

procedure TMainForm.CheckResynchToolMenu;
begin
 ActionResynchToolsSetPos.Enabled := True;
 ActionResynchToolsDelPos.Enabled := False;
 ActionResynchToolsReset.Enabled := False;
 if ResynchToolForm.vtvPoSList.TotalCount > 0 then
  begin
   ActionResynchToolsSetPos.Enabled := True;
   ActionResynchToolsDelPos.Enabled := True;
   ActionResynchToolsReset.Enabled := True;
  end;
end;

procedure TMainForm.ChangeFps(FPSSource : double; FPSDestination : double);
var
  I : Integer;
  Node, NodeN : PVirtualNode;
  NodeData, NodeDataN : PTreeData;
  MultiChangeTask : TUndoableMultiChangeTask;
  ChangeSubData : TChangeSubData;
  TempStart : Integer;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Node := vtvSubsList.GetFirst;
  while Node <> nil do
  begin
    NodeN := vtvSubsList.GetNext(Node);
    NodeData := vtvSubsList.GetNodeData(Node);
    NodeDataN := vtvSubsList.GetNodeData(NodeN);

    ChangeSubData := TChangeSubData.Create(Node.Index);
    ChangeSubData.OldStart := NodeData.Range.StartTime;
    ChangeSubData.OldStop := NodeData.Range.StopTime;
    ChangeSubData.NewStart := Round( NodeData.Range.StartTime / FPSDestination * FPSSource);
    ChangeSubData.NewStop := Round( NodeData.Range.StopTime / FPSDestination * FPSSource);

    if MenuItemKeepDurationIntact.Checked then
    begin
      ChangeSubData.NewStop := ChangeSubData.NewStart + (NodeData.Range.StopTime-NodeData.Range.StartTime);
      if Assigned(NodeDataN) then
      begin
        TempStart := Round( NodeDataN.Range.StartTime / FPSDestination * FPSSource);
        if ((ChangeSubData.NewStop >= TempStart) and
            (Pos('{',NodeDataN.Range.Text) = 0) and
            (Pos('{',NodeData.Range.Text) = 0) and
            (Pos('}',NodeDataN.Range.Text) = 0) and
            (Pos('}',NodeData.Range.Text) = 0) )
        then ChangeSubData.NewStop := TempStart - ConfigObject.SpaceKeyBlankBetweenSubtitles;
      end;
    end;
    MultiChangeTask.AddData(ChangeSubData);
    Node := vtvSubsList.GetNext(Node);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    MultiChangeTask.DoTask;
    PushUndoableTask(MultiChangeTask);
    // Update subtitle list and wavdisplay
    vtvSubsList.Repaint;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    WAVDisplayer1SelectionChange(WAVDisplayer);
    CurrentProject.IsDirty := True;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

procedure TMainForm.ChangeFpsCinemaToPalClick(Sender: TObject);
begin
// ChangeFps(23.976,25);
 ChangeFps(24000/1001,25);
end;

procedure TMainForm.ChangeFpsPalToCinemaClick(Sender: TObject);
begin
// ChangeFps(25,23.976);
 ChangeFps(25,24000/1001);
end;

procedure TMainForm.ChangeFpsCinemaToNtscClick(Sender: TObject);
begin
// ChangeFps(23.976,29.97);
 ChangeFps(24000/1001,30000/1001);
end;

procedure TMainForm.ChangeFpsNtscToCinemaClick(Sender: TObject);
begin
// ChangeFps(29.97,23.976);
 ChangeFps(30000/1001,24000/1001);
end;

procedure TMainForm.ChangeFpsPalToNtscClick(Sender: TObject);
begin
// ChangeFps(25,29.97);
 ChangeFps(25,30000/1001);
end;

procedure TMainForm.ChangeFpsNtscToPalClick(Sender: TObject);
begin
// ChangeFps(29.97,25);
 ChangeFps(30000/1001,25);
end;

procedure TMainForm.ItasaResynchtoolsshowhideClick(Sender: TObject);
begin
  if not ResynchToolForm.Visible then
   begin
    ResynchToolForm.Visible := True;
   end
  else ResynchToolForm.Visible := False;
end;

procedure TMainForm.ActionTextBottomLeftExecute(Sender: TObject);
begin
 TagText(ttPosBottomLeft);
end;

procedure TMainForm.ActionTextBottomMiddleExecute(Sender: TObject);
begin
 TagText(ttPosBottomMiddle);
end;

procedure TMainForm.ActionTextBottomRightExecute(Sender: TObject);
begin
 TagText(ttPosBottomRight);
end;

procedure TMainForm.ActionTextTopLeftExecute(Sender: TObject);
begin
 TagText(ttPosTopLeft);
end;

procedure TMainForm.ActionTextTopMiddleExecute(Sender: TObject);
begin
 TagText(ttPosTopMiddle);
end;

procedure TMainForm.ActionTextTopRightExecute(Sender: TObject);
begin
 TagText(ttPosTopRight);
end;

procedure TMainForm.ActionTextMiddleLeftExecute(Sender: TObject);
begin
 TagText(ttPosMiddleLeft);
end;

procedure TMainForm.ActionTextMiddleRightExecute(Sender: TObject);
begin
 TagText(ttPosMiddleRight);
end;

procedure TMainForm.ActionTextMiddleMiddleExecute(Sender: TObject);
begin
 TagText(ttPosMiddleMiddle);
end;

//------------------------------------------------------------------------------

function TMainForm.GetTextSelectionStart : Integer;
begin
  Result := MemoSubtitleText.SelStart;
end;

function TMainForm.GetTextSelectionLength : Integer;
begin
  Result := MemoSubtitleText.SelLength;
end;

function TMainForm.GetAudioCursorPosition : Integer;
begin
  Result := WAVDisplayer.GetCursorPos;
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiMemoSubtitleCopyVOToSubtitleTextClick(Sender: TObject);
begin
 MemoSubtitleText.Text := MemoSubtitleVO.Text;
end;

procedure TMainForm.pmiWAVDispCreateFromVOClick(Sender: TObject);
var SelStart, SelStop, IdxStart, IdxStop, idx : Integer;
    UndoableMultiAddTask : TUndoableMultiAddTask;
    Sub : TRange;
    SubList : TList;
begin
  SelStart := WavDisplayer.Selection.StartTime;
  SelStop := WavDisplayer.Selection.StopTime;
  if ( (SelStart = 0) and (SelStop = 0) ) then
  begin
    SelStart := WavDisplayer.GetCursorPos;
    SelStop := WavDisplayer.GetCursorPos;
  end;

  IdxStart := WavDisplayer.RangeListVO.GetRangeIdxAt(SelStart);
  IdxStop := WavDisplayer.RangeListVO.GetRangeIdxAt(SelStop);
  idx := IdxStart;

  UndoableMultiAddTask := TUndoableMultiAddTask.Create;

  SubList := TList.Create;
  if (idx >= 0) then
  begin
    while (idx <= IdxStop) do
    begin
      Sub := SubRangeFactory.CreateRangeSS(WavDisplayer.RangeListVO.Ranges[idx].StartTime,
                                           WavDisplayer.RangeListVO.Ranges[idx].StopTime);
      TSubtitleRange(Sub).Text := TSubtitleRange(WAVDisplayer.RangeListVO[idx]).Text;
      SubList.Add(Sub);
      idx := idx + 1;
    end;
  end;
  if (SubList.Count > 0) then
  begin
    UndoableMultiAddTask.SetData(SubList);
    UndoableMultiAddTask.DoTask;
    PushUndoableTask(UndoableMultiAddTask);
    CurrentProject.IsDirty := True;
  end;
end;

procedure TMainForm.ActionMouseAntiOverlappingExecute(Sender: TObject);
begin
 if (ConfigObject.EnableMouseAntiOverlapping = true) then
  ConfigObject.EnableMouseAntiOverlapping := false
 else ConfigObject.EnableMouseAntiOverlapping := true;
 ApplyMouseSettings;
end;

procedure TMainForm.edThresholdChange(Sender: TObject);
begin
 ConfigObject.SilentZoneThreshold := StrToInt(edThreshold.Text);
end;

procedure TMainForm.edDurationChange(Sender: TObject);
begin
 ConfigObject.SilentZoneDuration := StrToInt(MainForm.edDuration.Text);
end;

procedure TMainForm.edMinimunBetweenZonesChange(Sender: TObject);
begin
 ConfigObject.SilentZoneMinimunBetweenZones := StrToInt(MainForm.edMinimunBetweenZones.Text);
end;

procedure TMainForm.ActionRangeStopDblClickExecute(Sender: TObject);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
   begin
    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    // Install handler to fill UndoableMultiChangeTask
    GeneralJSPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
    GeneralJSPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
    GeneralJSPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

    GetCurrentPreviousNextSubtitles(vtvSubsList.FocusedNode, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifyRangeStopDblClick(CurrentSub, PreviousSub, NextSub);

    GeneralJSPlugin.OnSubtitleChangeStart := nil;
    GeneralJSPlugin.OnSubtitleChangeStop := nil;
    GeneralJSPlugin.OnSubtitleChangeText := nil;

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
   end;
end;

procedure TMainForm.ActionRangeStartDblClickExecute(Sender: TObject);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
   begin
    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    // Install handler to fill UndoableMultiChangeTask
    GeneralJSPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
    GeneralJSPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
    GeneralJSPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

    GetCurrentPreviousNextSubtitles(vtvSubsList.FocusedNode, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifyRangeStartDblClick(CurrentSub, PreviousSub, NextSub);

    GeneralJSPlugin.OnSubtitleChangeStart := nil;
    GeneralJSPlugin.OnSubtitleChangeStop := nil;
    GeneralJSPlugin.OnSubtitleChangeText := nil;

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
   end;
end;

procedure TMainForm.MenuItemSetVideoResolutionAdd50PercClick(Sender: TObject);
begin
 if not MenuItemDetachVideoWindow.Checked then
  begin
   if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 2 + VideoRenderer.VideoWidth div 4;
     PanelVideo.Height := VideoRenderer.VideoHeight div 2 + VideoRenderer.VideoHeight div 4;
     PanelTop.Height := VideoRenderer.VideoHeight div 2 + VideoRenderer.VideoHeight div 4;
    end
   else
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth + VideoRenderer.VideoWidth div 2;
     PanelVideo.Height := VideoRenderer.VideoHeight + VideoRenderer.VideoHeight div 2;
     PanelTop.Height := VideoRenderer.VideoHeight + VideoRenderer.VideoHeight div 2;
    end;
  end
 else DetachedVideoForm.SetVideo125Size;
end;

procedure TMainForm.MenuItemSetVideoResolutionAdd25PercClick(Sender: TObject);
begin
 if not MenuItemDetachVideoWindow.Checked then
  begin
   if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 2 + VideoRenderer.VideoWidth div 8;
     PanelVideo.Height := VideoRenderer.VideoHeight div 2 + VideoRenderer.VideoHeight div 8;
     PanelTop.Height := VideoRenderer.VideoHeight div 2 + VideoRenderer.VideoHeight div 8;
    end
   else
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth + VideoRenderer.VideoWidth div 4;
     PanelVideo.Height := VideoRenderer.VideoHeight + VideoRenderer.VideoHeight div 4;
     PanelTop.Height := VideoRenderer.VideoHeight + VideoRenderer.VideoHeight div 4;
    end;
  end
 else DetachedVideoForm.SetVideo150Size;
end;

procedure TMainForm.MenuItemSetVideoResolutionNormalClick(Sender: TObject);
begin
 if not MenuItemDetachVideoWindow.Checked then
  begin
   if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 2;
     PanelVideo.Height := VideoRenderer.VideoHeight div 2;
     PanelTop.Height := VideoRenderer.VideoHeight div 2;
    end
   else
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth;
     PanelVideo.Height := VideoRenderer.VideoHeight;
     PanelTop.Height := VideoRenderer.VideoHeight;
    end;
  end
 else DetachedVideoForm.SetVideoFullSize;
end;

procedure TMainForm.MenuItemSetVideoResolutionTo75PercClick(Sender: TObject);
begin
 if not MenuItemDetachVideoWindow.Checked then
  begin
   if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
    begin
      PanelVideo.Width := VideoRenderer.VideoWidth div 2 * 3 div 4;
      PanelVideo.Height := VideoRenderer.VideoHeight div 2 * 3 div 4;
      PanelTop.Height := VideoRenderer.VideoHeight div 2 * 3 div 4;
    end
   else
    begin
      PanelVideo.Width := VideoRenderer.VideoWidth * 3 div 4;
      PanelVideo.Height := VideoRenderer.VideoHeight * 3 div 4;
      PanelTop.Height := VideoRenderer.VideoHeight * 3 div 4;
    end;
  end
 else DetachedVideoForm.SetVideo75Size;
end;

procedure TMainForm.MenuItemSetVideoResolutionTo50PercClick(Sender: TObject);
begin
 if not MenuItemDetachVideoWindow.Checked then
  begin
   if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 2 * 2 div 4;
     PanelVideo.Height := VideoRenderer.VideoHeight div 2 * 2 div 4;
     PanelTop.Height := VideoRenderer.VideoHeight div 2 * 2 div 4;
    end
   else
     begin
       PanelVideo.Width := VideoRenderer.VideoWidth * 2 div 4;
       PanelVideo.Height := VideoRenderer.VideoHeight * 2 div 4;
       PanelTop.Height := VideoRenderer.VideoHeight * 2 div 4;
     end;
  end
 else DetachedVideoForm.SetVideo50Size;
end;

procedure TMainForm.MenuItemSetVideoResolutionTo33PercClick(Sender: TObject);
begin
 if not MenuItemDetachVideoWindow.Checked then
  begin
   if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 2 div 3;
     PanelVideo.Height := VideoRenderer.VideoHeight div 2 div 3;
     PanelTop.Height := VideoRenderer.VideoHeight div 2 div 3;
    end
   else
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 3;
     PanelVideo.Height := VideoRenderer.VideoHeight div 3;
     PanelTop.Height := VideoRenderer.VideoHeight div 3;
    end;
  end
 else DetachedVideoForm.SetVideo33Size;
end;

procedure TMainForm.MenuItemSetVideoResolutionTo25PercClick(Sender: TObject);
begin
 if not MenuItemDetachVideoWindow.Checked then
  begin
   if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 2 div 4;
     PanelVideo.Height := VideoRenderer.VideoHeight div 2 div 4;
     PanelTop.Height := VideoRenderer.VideoHeight div 2 div 4;
    end
   else
    begin
     PanelVideo.Width := VideoRenderer.VideoWidth div 4;
     PanelVideo.Height := VideoRenderer.VideoHeight div 4;
     PanelTop.Height := VideoRenderer.VideoHeight div 4;
    end;
  end
 else DetachedVideoForm.SetVideo25Size;
end;

procedure TMainForm.ActionDiffSubFilesExecute(Sender: TObject);
begin
 DiffSubsForm.OpenFileText(CurrentProject.SubtitlesFile);
 DiffSubsForm.OpenFileVoOther(CurrentProject.SubtitlesVO);
 DiffSubsForm.Caption := ExtractFileName(CurrentProject.SubtitlesFile) + ' <--> ' + ExtractFileName(CurrentProject.SubtitlesVO);
 DiffSubsForm.Compare();
 DiffSubsForm.ShowModal;
end;

procedure TMainForm.ActionSendToItasaExecute(Sender: TObject);
begin
  if CurrentProject.IsDirty then
  begin
    SaveSubtitles(CurrentProject.SubtitlesFile, '', CurrentProject.IsUTF8, False, nil, False);
    SaveProject(CurrentProject, False);
  end;

  if SendToItasaForm.Inizializza(CurrentProject.SubtitlesFile) then
    SendToItasaForm.ShowModal;

end;

function TMainForm.GetUTF8Default : boolean;
begin
 Result := ConfigObject.UTF8AsDefault;
end;

{$IFNDEF enhanced}
procedure TMainForm.CheckUpdates(Silent:Boolean);
  var
    ChannelNode,StartItemNode : IXMLNode;
    ANode,AChannel : IXMLNode;
    sPubDate, sTitle, sDesc, sLink,sVersion,sCode : String;
    ANodeCodeItems : IXmlNodeList; ANodeCodeItem: IXMLNode; ANodeCodeIndex : Integer;
begin
   try
    begin

     SourceforgeRssFeedXml.Active:=True;

     ChannelNode:= SourceforgeRssFeedXml.DocumentElement.ChildNodes.FindNode('channel');

     ANode := ChannelNode.ChildNodes.FindNode('item');
     sTitle := ANode.ChildNodes['title'].Text;
     sLink := ANode.ChildNodes['link'].Text;
     sPubDate := ANode.ChildNodes['pubDate'].Text;
     sVersion := copy(sLink,pos('_',slink)+1,8);

     if copy(sVersion,8,1) = '.' then
      begin
        sVersion := copy(sVersion,1,7);
        if StrToInt(copy(sVersion,7,2)) <= 9 then
         begin
           sVersion := copy(sVersion,1,6) + '0' + copy(sVersion,7,1);
         end;
      end;

     if sversion > g_ApplicationVersion.VersionString then
      begin
        if MessageDlg('A new version of VisualSubSync for Italiansubs (' + sversion + ') is avaiable on Sourceforge project page. ' + chr(13) +
          'Do you want to download it?', mtInformation, [MbYes, MbNo], 0) = mrYes then
         ShellExecute(Handle, 'open', PChar(sLink),nil,nil, SW_SHOWNORMAL);
      end
     else
      begin
        if not Silent then
          MessageDlg('No new updates are avaiable.', mtInformation, [MbOk], 0);
      end;

     SourceforgeRssFeedXml.Active:=False;
     SourceforgeRssFeedCodeXml.Active:=False;

    end;
   except
   end;
end;
{$ELSE}
procedure TMainForm.CheckUpdates(Silent:Boolean);
var
  sJSonRelease : String;
  oReleases, oAssets, Item: TlkJSONBase;
  Release, Assets: Integer;
  DownloadLink, ReleaseNotes, ReleaseVersion, ReleaseDate : String;

  function WebGetData(const UserAgent: string; const URL: string): string;
  var
    hInet: HINTERNET;
    hURL: HINTERNET;
    Buffer: array[0..1023] of AnsiChar;
    BufferLen: cardinal;
  begin
    result := '';
    hInet := InternetOpen(PChar(UserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hInet = nil then RaiseLastOSError;
    try
      hURL := InternetOpenUrl(hInet, PChar(URL), nil, 0, 0, 0);
      if hURL = nil then RaiseLastOSError;
      try
        repeat
          if not InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen) then
            RaiseLastOSError;
          result := result + UTF8Decode(Copy(Buffer, 1, BufferLen))
        until BufferLen = 0;
      finally
        InternetCloseHandle(hURL);
      end;
    finally
      InternetCloseHandle(hInet);
    end;
  end;

  Function FormatVersionString(sOriginal : String) : String;
  var sFinal : String; sNumber:String;
  begin
   While Pos('.',sOriginal) > 0 do
   begin
    sNumber := copy(sOriginal,1,Pos('.',sOriginal)-1);
    sFinal := sFinal + RightPad(sNumber, '0', 2) + '.';
    sOriginal := copy(sOriginal,Pos('.',sOriginal)+1,100);
   end;
   sNumber := copy(sOriginal,1,100);
   sFinal := sFinal + RightPad(sNumber, '0', 2);
   Result := sFinal;
  end;

begin
   try
    begin

      if IsUniversalAppEnviroment = true then Exit;

      sJSonRelease := WebGetData('Visualsubsync Enhanced', 'https://api.github.com/repos/red5goahead/VisualSubSync-Enhanced/releases');

      oReleases := TlkJSON.ParseText(sJSonRelease);
      for Release := 0 to Pred(oReleases.Count) do
      begin
        oAssets := oReleases.Child[Release].Field['assets'];
        ReleaseNotes := VarToStr(oReleases.Child[Release].Field['body'].Value);
        ReleaseVersion := VarToStr(oReleases.Child[Release].Field['tag_name'].Value);
        ReleaseDate := Copy(VarToStr(oReleases.Child[Release].Field['published_at'].Value),9,2) + '-' +
          Copy(VarToStr(oReleases.Child[Release].Field['published_at'].Value),6,2) + '-' +
          Copy(VarToStr(oReleases.Child[Release].Field['published_at'].Value),1,4) + ' ' +
          Copy(VarToStr(oReleases.Child[Release].Field['published_at'].Value),12,5);


        for Assets := 0 to Pred(oAssets.Count) do
        begin
          DownloadLink := VarToStr(oAssets.Child[Assets].Field['browser_download_url'].Value);
          break;
        end;

        if DownloadLink > '' then
        begin
          break;
        end;

      end;

      if FormatVersionString(ReleaseVersion) > FormatVersionString(g_ApplicationVersion.VersionString) then
        begin
          if MessageDlg('A new version of VisualSubSync Enhanced (' + ReleaseVersion + ') is avaiable on Github repository. ' + chr(13) +
            'Do you want to download it? ' + chr(13) + chr(13) +
            'Release notes (' + ReleaseDate + ')' + chr(13) + chr(13) +
            ReleaseNotes + chr(13), mtInformation, [MbYes, MbNo], 0) = mrYes then
           ShellExecute(Handle, 'open', PChar(DownloadLink),nil,nil, SW_SHOWNORMAL);
        end
      else
        begin
          if not Silent then
            MessageDlg('No new updates are avaiable.', mtInformation, [MbOk], 0);
        end;

    end;
   except
   end;
end;
{$ENDIF}

procedure TMainForm.ChangeFpsTrueCinemaToPalClick(Sender: TObject);
begin
 ChangeFps(24,25);
end;

procedure TMainForm.ChangeFpsPalToTrueCinemaClick(Sender: TObject);
begin
 ChangeFps(25,24);
end;

procedure TMainForm.MenuItemKeepDurationIntactClick(Sender: TObject);
begin
  MenuItemKeepDurationIntact.Checked:= not MenuItemKeepDurationIntact.Checked;
  ConfigObject.KeepDurationIntact := MenuItemKeepDurationIntact.Checked;
end;

procedure TMainForm.LabelSelBeginClick(Sender: TObject);
 var OldStart, OldStop, NewStart, NewStop : Integer; SubRange : TSubtitleRange;
   UndoableSetTimeTask : TUndoableSetTimeTask;
begin
 if Assigned(WAVDisplayer.SelectedRange) then
  begin
   SubtitleTimingForm := TSubtitleTimingForm.Create(self);
   SubtitleTimingForm.edTime.Text := edSelBegin.Text;
   if SubtitleTimingForm.ShowModal = mrOk then
    begin
     edSelBegin.Text := SubtitleTimingForm.edTime.EditText;
     SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
     UndoableSetTimeTask := TUndoableSetTimeTask.Create;
     UndoableSetTimeTask.SetData(SubRange.Node.Index, SubRange.StartTime, SubRange.StopTime,
       TimeStringToMs(edSelBegin.Text), SubRange.StopTime);
     UndoableSetTimeTask.DoTask;
     PushUndoableTask(UndoableSetTimeTask);
     edSelLength.Text := TimeMsToString(WAVDisplayer.Selection.StopTime -
      WAVDisplayer.Selection.StartTime);
    end;
   SubtitleTimingForm.Close;
  end;
end;

procedure TMainForm.LabelSelEndClick(Sender: TObject);
 var OldStart, OldStop, NewStart, NewStop : Integer; SubRange : TSubtitleRange;
   UndoableSetTimeTask : TUndoableSetTimeTask;
begin
 if Assigned(WAVDisplayer.SelectedRange) then
  begin
   SubtitleTimingForm := TSubtitleTimingForm.Create(self);
   SubtitleTimingForm.edTime.Text := edSelEnd.Text;
   if SubtitleTimingForm.ShowModal = mrOk then
    begin
     edSelEnd.Text := SubtitleTimingForm.edTime.EditText;
     SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
     UndoableSetTimeTask := TUndoableSetTimeTask.Create;
     UndoableSetTimeTask.SetData(SubRange.Node.Index, SubRange.StartTime, SubRange.StopTime,
       SubRange.StartTime, TimeStringToMs(edSelEnd.Text));
     UndoableSetTimeTask.DoTask;
     PushUndoableTask(UndoableSetTimeTask);
     edSelLength.Text := TimeMsToString(WAVDisplayer.Selection.StopTime -
      WAVDisplayer.Selection.StartTime);
    end;
   SubtitleTimingForm.Close;
  end;
end;

procedure TMainForm.LabelSelLenghtClick(Sender: TObject);
 var OldStart, OldStop, NewStart, NewStop : Integer; SubRange : TSubtitleRange;
   UndoableSetTimeTask : TUndoableSetTimeTask;
begin
 if Assigned(WAVDisplayer.SelectedRange) then
  begin
   SubtitleTimingForm := TSubtitleTimingForm.Create(self);
   SubtitleTimingForm.edTime.Text := edSelLength.Text;
   if SubtitleTimingForm.ShowModal = mrOk then
    begin
     edSelLength.Text := SubtitleTimingForm.edTime.EditText;
     SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
     UndoableSetTimeTask := TUndoableSetTimeTask.Create;
     UndoableSetTimeTask.SetData(SubRange.Node.Index, SubRange.StartTime, SubRange.StopTime,
       SubRange.StartTime, TimeStringToMs(edSelBegin.Text)+TimeStringToMs(edSelLength.Text));
     UndoableSetTimeTask.DoTask;
     PushUndoableTask(UndoableSetTimeTask);
     edSelEnd.Text := TimeMsToString(WAVDisplayer.Selection.StopTime);
     edSelLength.Text := TimeMsToString(WAVDisplayer.Selection.StopTime - WAVDisplayer.Selection.StartTime);
    end;
   SubtitleTimingForm.Close;
  end;
end;

procedure TMainForm.extinlowercaseClick(Sender: TObject);
var Cursor : PVirtualNode;
    NodeData : PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    TaggedText : WideString;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Cursor);
    SubtitleRange := NodeData.Range;
    TaggedText := LowerCase(SubtitleRange.Text);
    if (TaggedText <> SubtitleRange.Text) then
    begin
      ChangeSubData := TChangeSubData.Create(Cursor.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.NewText := TaggedText;
      SubtitleRange.Text := TaggedText;
      MultiChangeTask.AddData(ChangeSubData);
    end;
    Cursor := vtvSubsList.GetNextSelected(Cursor);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

procedure TMainForm.extinuppercaseClick(Sender: TObject);
var Cursor : PVirtualNode;
    NodeData : PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    TaggedText : WideString;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Cursor);
    SubtitleRange := NodeData.Range;
    TaggedText := UpperCase(SubtitleRange.Text);
    if (TaggedText <> SubtitleRange.Text) then
    begin
      ChangeSubData := TChangeSubData.Create(Cursor.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.NewText := TaggedText;
      SubtitleRange.Text := TaggedText;
      MultiChangeTask.AddData(ChangeSubData);
    end;
    Cursor := vtvSubsList.GetNextSelected(Cursor);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

procedure TMainForm.extinuppercaseFirstLetterClick(Sender: TObject);
var Cursor : PVirtualNode;
    NodeData : PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    TaggedText : WideString;
    SubtitleRange : TSubtitleRange;
 FUNCTION Capitalize (CONST s: STRING): STRING;
  VAR
  flag: BOOLEAN;
  i : Byte;
  t : STRING;
  BEGIN
  flag := TRUE;
  t := '';
  FOR i := 1 TO LENGTH(s) DO
  BEGIN
  IF flag
  THEN AppendStr(t, UpCase(s[i]))
  ELSE AppendStr(t, s[i]);
  flag := (s[i] = ' ')
  END;
  RESULT := t
  END {Capitalize};
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Cursor);
    SubtitleRange := NodeData.Range;
    TaggedText := Capitalize(SubtitleRange.Text);
    if (TaggedText <> SubtitleRange.Text) then
    begin
      ChangeSubData := TChangeSubData.Create(Cursor.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.NewText := TaggedText;
      SubtitleRange.Text := TaggedText;
      MultiChangeTask.AddData(ChangeSubData);
    end;
    Cursor := vtvSubsList.GetNextSelected(Cursor);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

procedure TMainForm.TimerChkUpdatesTimer(Sender: TObject);
begin
  TimerChkUpdates.Enabled := False;
  CheckUpdates(True);
end;

procedure TMainForm.MenuItemUpdateClick(Sender: TObject);
begin
end;

//------------------------------------------------------------------------------

{

TODO UNDO:
undo text pipe text modification (modifiy, clear, load)

undo for style?

TODO : display karaoke sylables in wavdisplay

// -----------------------------------------------------------------------------

ScriptLog('todo = ' + VSSCore);
ScriptLog('VSSCore = ' + VSSCore.abc);

TODO : test wav extraction with divx installed
DivX Demux - 85516702-9C45-4A9C-861B-BC4492D355DC - C:\WINDOWS\system32\DivXMedia.ax ( 0.0.0.28)

}

procedure TMainForm.MenuItemCheckUpdatesClick(Sender: TObject);
begin
 CheckUpdates(false);
end;

procedure TMainForm.MenuItemRecentChangesClick(Sender: TObject);
  var
    ChannelNode,StartItemNode : IXMLNode;
    ANode,AChannel : IXMLNode;
    sPubDate, sTitle, sDesc, sLink,sVersion,sCode,sDescription : String;
    ANodeCodeItems : IXmlNodeList; ANodeCodeItem: IXMLNode; ANodeCodeIndex : Integer;
begin
  SourceforgeRssFeedCodeXml.Active := True;
  ChannelNode:= SourceforgeRssFeedCodeXml.DocumentElement.ChildNodes.FindNode('channel');

  ANodeCodeItems := ChannelNode.ChildNodes;

  if ANodeCodeItems.Count>0 then
   begin
    sDesc := 'RECENT CHANGES' + chr(13);
    sDesc := sDesc + '=============' + chr(13) + chr(13);
    for ANodeCodeIndex := 0 to ANodeCodeItems.Count-1 do
     begin
      ANodeCodeItem := ANodeCodeItems.Get(ANodeCodeIndex);
      sDescription := ANodeCodeItem.ChildNodes['description'].Text;
      if sDescription>'' then
       begin
        sDescription := AnsiReplaceStr(sDescription,'<div class="markdown_content">','');
        sDescription := AnsiReplaceStr(sDescription,'<p>','');
        sDesc := sDesc + '<' + copy(ANodeCodeItem.ChildNodes['pubDate'].Text,1,16) + '>' + chr(13);
        sDesc := sDesc + copy(sDescription,1,pos('<br',sDescription)-1) + chr(13) + chr(13);
       end;
     end;
   end;

   MessageDlg(sDesc, mtInformation, [MbOk], 0);

end;

procedure TMainForm.ChangeFpsCinemaToTrueCinemaClick(Sender: TObject);
begin
// ChangeFps(23.976,24);
 ChangeFps(24000/1001,24);
end;

procedure TMainForm.ChangeFpsTrueCinemaToCinemaClick(Sender: TObject);
begin
// ChangeFps(24,23.976);
 ChangeFps(24,24000/1001);
end;

procedure TMainForm.ChangeFpsTrueCinemaToNtscClick(Sender: TObject);
begin
 ChangeFps(24,30000/1001);
end;

procedure TMainForm.ChangeFpsNtscToTrueCinemaClick(Sender: TObject);
begin
// ChangeFps(29.97,24);
 ChangeFps(30000/1001,24);
end;

procedure TMainForm.ActionZipSubtitleExecute(Sender: TObject);
var NomeFile: TNomeFile;
    zip: TKAZip;
    i: Integer;
    tmp : string;
    skip: Boolean;
begin
  if CurrentProject.IsDirty then
  begin
    SaveSubtitles(CurrentProject.SubtitlesFile, '', CurrentProject.IsUTF8, False, nil, False);
    SaveProject(CurrentProject, False);
  end;
  if ItaliansubsAuthorization(MainForm.ConfigObject.Username, MainForm.ConfigObject.Passwd) then
  begin
    NomeFile := TNomeFile.Create;
    NomeFile.NewFile(ChangeFileExt(ExtractFileName(CurrentProject.SubtitlesFile),''));
    skip:=False;
    if not NomeFile.Valid then
    begin
      tmp := 'Filename not valid for Italiansubs Filename Rules.'+ #13#10 +
             'Zip anyway?';
      case Application.MessageBox(PAnsiChar(tmp),
             'Zip Subtitle...',
             MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) of
        IDYES: skip := True;
        IDNO: Exit;
      end;
    end;
  end
  else Skip := True;
  if skip or (not skip and NomeFile.FixSeriesName) then
  begin
    try
      zip := TKAZip.Create(nil);
      i := 0;
      if skip then
        tmp:=ChangeFileExt(CurrentProject.SubtitlesFile,'.zip')
      else
        tmp:=ExtractFilePath(CurrentProject.SubtitlesFile) + NomeFile.ZipFileName + '.zip';
      if FileExists(tmp) then
        while not RenameFile(tmp, ChangeFileExt(tmp,'.bak' + IntToStr(i) + '.zip')) do
          i := i + 1;
      zip.CreateZip(tmp);
      zip.Open(tmp);
      if skip then
        zip.AddFile(CurrentProject.SubtitlesFile, ExtractFileName(CurrentProject.SubtitlesFile))
      else
        zip.AddFile(CurrentProject.SubtitlesFile, NomeFile.GetFileName + ExtractFileExt(CurrentProject.SubtitlesFile));
      zip.Close;
    finally
      zip.Free;
    end;
  end;
end;

procedure TMainForm.pmUndoTPClick(Sender: TObject);
begin
 MemoTextPipe.Undo;
end;

procedure TMainForm.MenuItemGooglePageClick(Sender: TObject);
var s : string;
begin
  s := 'https://plus.google.com/u/0/b/112999295353890945638/112999295353890945638/posts';
  ShellExecute(Self.WindowHandle, 'open', PAnsiChar(s), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.ActionSaveSelectedAsExecute(Sender: TObject);
var InputExt : WideString;
    NewExt : WideString;
    Translator : TTranslator;
    Cursor : PVirtualNode;
    NodeData : PTreeData;
    SelectedStartTime, SelectedStopTime : Integer;
    sSelectedStartTime, sSelectedStopTime : String;
    Index : Integer;
begin

  Index := 0;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
    begin
      NodeData := vtvSubsList.GetNodeData(Cursor);
      if Index = 0 then SelectedStartTime := NodeData.Range.StartTime;
      SelectedStopTime := NodeData.Range.StopTime;
      Cursor := vtvSubsList.GetNextSelected(Cursor);
      Index := Index + 1;
    end;
  sSelectedStartTime := TimeMsToString(SelectedStartTime);
  sSelectedStartTime := AnsiReplaceStr(sSelectedStartTime, ':', '.');
  sSelectedStopTime := TimeMsToString(SelectedStopTime);
  sSelectedStopTime := AnsiReplaceStr(sSelectedStopTime, ':', '.');

  TntSaveDialog1.Filter :=
    'SRT files (*.srt)|*.SRT' + '|' +
    'TXT files (*.txt)|*.TXT' + '|' +
    'All files (*.*)|*.*';
  TntSaveDialog1.FileName := ChangeFileExt(CurrentProject.SubtitlesFile,'') +
    '_' + sSelectedStartTime + '-' + sSelectedStopTime;
  // Preselect format
  InputExt := WideLowerCase(WideExtractFileExt(CurrentProject.SubtitlesFile));
  if (InputExt = '.srt') then
    TntSaveDialog1.FilterIndex := 1
  else if (InputExt = '.ssa') then
    TntSaveDialog1.FilterIndex := 3
  else if (InputExt = '.ass') then
    TntSaveDialog1.FilterIndex := 4
  else
    TntSaveDialog1.FilterIndex := 7;

  if TntSaveDialog1.Execute then
  begin
    // Force extension filter
    NewExt := '';
    case TntSaveDialog1.FilterIndex of
      1 : NewExt := '.srt';
      2 : NewExt := '.srt';
      3 : NewExt := '.ssa';
      4 : NewExt := '.ass';
      5 : NewExt := '.cue';
      6 : NewExt := '.txt';
      7 : NewExt := '.csv';
    end;
    if (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.srt') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.ssa') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.ass') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.cue') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.txt') AND
       (WideLowerCase(WideExtractFileExt(TntSaveDialog1.FileName)) <> '.csv') Then
       begin
        TntSaveDialog1.FileName := TntSaveDialog1.FileName + NewExt;
       end;
    if NewExt <> '' then
      TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, NewExt);
    Translator := nil;
    if (TntSaveDialog1.FilterIndex = 2) then
      Translator := TTranslatorEmpty.Create;
    SaveSubtitles(TntSaveDialog1.FileName, CurrentProject.SubtitlesFile,
      CurrentProject.IsUTF8, False, Translator, True);
    if (Assigned(Translator)) then
      FreeAndNil(Translator);
  end;

end;

procedure TMainForm.ActionResynchShowHideExecute(Sender: TObject);
begin
  if not ResynchToolForm.Visible then
   begin
    ResynchToolForm.Visible := True;
   end
  else ResynchToolForm.Visible := False;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SmartAccents(RichEdit : TTntRichEdit);
var savSelStart, savSelLength : Integer;
    i, j, eventMask, len : Integer;
    WordArray : TWideStringDynArray;
    WordColor : TColor;
    TextToSpell, AWord, AWordAlt : WideString; PosAlt : Integer;
    Offset : Integer;
    SpellOk : Boolean; SmartAccentsOn : Boolean;
    WordInfo : TWordInfo;
begin
  RichEdit.Tag := 0;
  savSelStart := RichEdit.SelStart;
  savSelLength := RichEdit.SelLength;

  TagSplit(RichEdit.Text, WordArray);

  eventMask := RichEdit.Perform(EM_SETEVENTMASK, 0, 0);

  RichEdit.Lines.BeginUpdate;
  for i:=0 to Length(WordArray)-1 do
    begin
      Offset := 1;
      TextToSpell := WordArray[i];
      while (Offset <= Length(TextToSpell)) do
      begin
        GetNextWordPos(Offset, TextToSpell, WordInfo);
        AWord := Copy(TextToSpell, WordInfo.Position, WordInfo.Length);
        AWordAlt := '';

        SpellOk := FSpellChecker.Spell(AWord);
        if (not SpellOk) then
        begin
          SmartAccentsOn := false;
          if Copy(AWord,Length(AWord),1) = '''' then
            begin
              AWordAlt := Copy(AWord,1,Length(AWord)-1);

              // eccezioni
              if (AWordAlt = 'da') then Continue;

              // ì
              if Copy(AWordAlt,Length(AWord)-1,1) = 'i' then
                if FSpellChecker.Spell(Copy(AWordAlt,1,Length(AWordAlt)-1)+'ì') then
                 begin
                    AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'ì';
                    SmartAccentsOn := True;
                 end;
              // Ò (accentata maiuscola grave)
              if Copy(AWordAlt,Length(AWord)-1,1) = 'I' then
                if SmartAccentsOn = False then
                  if FSpellChecker.Spell(LowerCase(Copy(AWordAlt,1,Length(AWordAlt)-1)+'ì')) then
                   begin
                      AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'Ì';
                      SmartAccentsOn := True;
                   end;

              // ò
              if Copy(AWordAlt,Length(AWord)-1,1) = 'o' then
                if FSpellChecker.Spell(Copy(AWordAlt,1,Length(AWordAlt)-1)+'ò') then
                 begin
                    AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'ò';
                    SmartAccentsOn := True;
                 end;
              // Ò (accentata maiuscola grave)
              if Copy(AWordAlt,Length(AWord)-1,1) = 'O' then
                if SmartAccentsOn = False then
                  if FSpellChecker.Spell(LowerCase(Copy(AWordAlt,1,Length(AWordAlt)-1)+'ò')) then
                   begin
                      AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'Ò';
                      SmartAccentsOn := True;
                   end;

              // à
              if Copy(AWordAlt,Length(AWord)-1,1) = 'a' then
                if SmartAccentsOn = False then
                 if FSpellChecker.Spell(Copy(AWordAlt,1,Length(AWordAlt)-1)+'à') then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'à';
                     SmartAccentsOn := True;
                  end;
                // À (accentata maiuscola grave)
              if Copy(AWordAlt,Length(AWord)-1,1) = 'A' then
                if SmartAccentsOn = False then
                 if FSpellChecker.Spell(LowerCase(Copy(AWordAlt,1,Length(AWordAlt)-1)+'à')) then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'À';
                     SmartAccentsOn := True;
                  end;

              // ù
              if Copy(AWordAlt,Length(AWord)-1,1) = 'u' then
                if SmartAccentsOn = False then
                 if FSpellChecker.Spell(Copy(AWordAlt,1,Length(AWordAlt)-1)+'ù') then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'ù';
                     SmartAccentsOn := True;
                  end;
                // Ù (accentata maiuscola grave)
              if Copy(AWordAlt,Length(AWord)-1,1) = 'U' then
                if SmartAccentsOn = False then
                 if FSpellChecker.Spell(LowerCase(Copy(AWordAlt,1,Length(AWordAlt)-1)+'ù')) then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'Ù';
                     SmartAccentsOn := True;
                  end;

              // è
              if Copy(AWordAlt,Length(AWord)-1,1) = 'e' then
               if SmartAccentsOn = False then
                 if FSpellChecker.Spell(Copy(AWordAlt,1,Length(AWordAlt)-1)+'è') then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'è';
                     SmartAccentsOn := True;
                  end;
              // é
              if Copy(AWordAlt,Length(AWord)-1,1) = 'e' then
               if SmartAccentsOn = False then
                 if FSpellChecker.Spell(Copy(AWordAlt,1,Length(AWordAlt)-1)+'é') then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'é';
                     SmartAccentsOn := True;
                  end;
              // É (accentata maiuscola acuto)
              if Copy(AWordAlt,Length(AWord)-1,1) = 'E' then
                if SmartAccentsOn = False then
                 if FSpellChecker.Spell(LowerCase(Copy(AWordAlt,1,Length(AWordAlt)-1)+'é')) then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'É';
                     SmartAccentsOn := True;
                  end;
              // È (accentata maiuscola grave)
              if Copy(AWordAlt,Length(AWord)-1,1) = 'E' then
                if SmartAccentsOn = False then
                 if FSpellChecker.Spell(LowerCase(Copy(AWordAlt,1,Length(AWordAlt)-1)+'è')) then
                  begin
                     AWordAlt := Copy(AWordAlt,1,Length(AWordAlt)-1)+'È';
                     SmartAccentsOn := True;
                  end;

              if SmartAccentsOn = True then
                begin
                  PosAlt := RichEdit.SelStart;
                  RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
                  RichEdit.SelStart := PosAlt-1;
                  SmartAccentsOn := false;
                end;

            end;

          if Copy(AWord,Length(AWord),1) = 'è' then
            if FSpellChecker.Spell(Copy(AWord,1,Length(AWord)-1)+'é') then
              begin
                AWordAlt := Copy(AWord,1,Length(AWord)-1)+'é';
                PosAlt := RichEdit.SelStart;
                RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
                RichEdit.SelStart := PosAlt;
              end;

          if Copy(AWord,Length(AWord),1) = 'é' then
            if FSpellChecker.Spell(Copy(AWord,1,Length(AWord)-1)+'è') then
              begin
                AWordAlt := Copy(AWord,1,Length(AWord)-1)+'è';
                PosAlt := RichEdit.SelStart;
                RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
                RichEdit.SelStart := PosAlt;
              end;

        end;
        Inc(Offset);
      end;
    end;

  RichEdit.Lines.EndUpdate;

  RichEdit.Perform(EM_SETEVENTMASK, 0, eventMask);

  RichEdit.Tag := 1;

end;

//------------------------------------------------------------------------------

procedure TMainForm.SmartUppercase(RichEdit : TTntRichEdit; PreviousSubtitleText: String);
var savSelStart, savSelLength : Integer;
    i, j, eventMask, len : Integer;
    WordArray : TWideStringDynArray;
    WordColor : TColor;
    TextToSpell, AWord, AWordAlt : WideString; PosAlt : Integer;
    Offset : Integer; LeftSide, RightSide : string;
    SpellOk : Boolean; SmartUppercaseOn : Boolean;
    WordInfo : TWordInfo; Period : Boolean;
begin
  RichEdit.Tag := 0;
  savSelStart := RichEdit.SelStart;
  savSelLength := RichEdit.SelLength;

  if  (Length(RichEdit.Text) >= 1) AND
      (Ord(Copy(RichEdit.Text,1,1)[1]) >= 97) AND
      (Ord(Copy(RichEdit.Text,1,1)[1]) <= 122) then
  begin
    if  (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText)-4,1) = '.') AND not ( Copy(PreviousSubtitleText,length(PreviousSubtitleText)-5,2) = '..')  ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText)-4,1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText)-4,1) = '!') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
          begin
            PosAlt := RichEdit.SelStart;
            RichEdit.Text := UpperCase(Copy(RichEdit.Text,1,1)) + Copy(RichEdit.Text,2,Length(RichEdit.Text)-1);
            RichEdit.SelStart := PosAlt;
          end;
  end;

  if  (Length(RichEdit.Text) >= 1) and ( Copy(RichEdit.Text,1,1) = 'è')
      then
  begin
    if  (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
          begin
            PosAlt := RichEdit.SelStart;
            RichEdit.Text := 'È' + Copy(RichEdit.Text,2,Length(RichEdit.Text)-1);
            RichEdit.SelStart := PosAlt;
          end;
  end;

  if  ( (Length(RichEdit.Text) >= 3) AND (Copy(RichEdit.Text,1,3) = '- è') AND (Copy(RichEdit.Text,2,1) = Chr(32)) )
      then
  begin
    if  (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
          begin
            PosAlt := RichEdit.SelStart;
            RichEdit.Text := '- ' + 'È' + Copy(RichEdit.Text,4,Length(RichEdit.Text)-1);
            RichEdit.SelStart := PosAlt;
          end;
  end;

  if (Length(RichEdit.Text) >= 3) AND
      (Copy(RichEdit.Text,1,1) = '-') AND
      (Copy(RichEdit.Text,2,1) = Chr(32)) AND
      (Ord(Copy(RichEdit.Text,3,1)[1]) >= 97) AND
      (Ord(Copy(RichEdit.Text,3,1)[1]) <= 122) then
  begin
    if (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
        begin
          PosAlt := RichEdit.SelStart;
          RichEdit.Text := Copy(RichEdit.Text,1,2) + UpperCase(Copy(RichEdit.Text,3,1)) + Copy(RichEdit.Text,4,Length(RichEdit.Text)-3);
          RichEdit.SelStart := PosAlt;
        end;
  end;

  if (Length(RichEdit.Text) >= 4) AND
      (Copy(RichEdit.Text,1,1) = '<') AND
      (Copy(RichEdit.Text,3,1) = '>') AND
      (Ord(Copy(RichEdit.Text,4,1)[1]) >= 97) AND
      (Ord(Copy(RichEdit.Text,4,1)[1]) <= 122) then
  begin
    if (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
        begin
          PosAlt := RichEdit.SelStart;
          RichEdit.Text := Copy(RichEdit.Text,1,3) + UpperCase(Copy(RichEdit.Text,4,1)) + Copy(RichEdit.Text,5,Length(RichEdit.Text)-3);
          RichEdit.SelStart := PosAlt;
        end;
  end;

if (Length(RichEdit.Text) >= 6) AND
      (Copy(RichEdit.Text,1,1) = '<') AND
      (Copy(RichEdit.Text,3,1) = '>') AND
      (Copy(RichEdit.Text,4,1) = '-') AND
      (Ord(Copy(RichEdit.Text,6,1)[1]) >= 97) AND
      (Ord(Copy(RichEdit.Text,6,1)[1]) <= 122) then
  begin
    if (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
        begin
          PosAlt := RichEdit.SelStart;
          RichEdit.Text := Copy(RichEdit.Text,1,5) + UpperCase(Copy(RichEdit.Text,6,1)) + Copy(RichEdit.Text,7,Length(RichEdit.Text)-3);
          RichEdit.SelStart := PosAlt;
        end;
  end;

  if (Length(RichEdit.Text) >= 6) AND
      (Copy(RichEdit.Text,1,1) = '-') AND
      (Copy(RichEdit.Text,3,1) = '<') AND
      (Copy(RichEdit.Text,5,1) = '>') AND
      (Ord(Copy(RichEdit.Text,6,1)[1]) >= 97) AND
      (Ord(Copy(RichEdit.Text,6,1)[1]) <= 122) then
  begin
    if (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
        begin
          PosAlt := RichEdit.SelStart;
          RichEdit.Text := Copy(RichEdit.Text,1,5) + UpperCase(Copy(RichEdit.Text,6,1)) + Copy(RichEdit.Text,7,Length(RichEdit.Text)-3);
          RichEdit.SelStart := PosAlt;
        end;
  end;

  if (Length(RichEdit.Text) >= 4) AND
      (Copy(RichEdit.Text,1,1) = '-') AND
      (Copy(RichEdit.Text,3,1) = '"') AND
      (Ord(Copy(RichEdit.Text,4,1)[1]) >= 97) AND
      (Ord(Copy(RichEdit.Text,4,1)[1]) <= 122) then
  begin
    if (PreviousSubtitleText = '') OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '.') AND (Copy(PreviousSubtitleText,length(PreviousSubtitleText)-1,1) <> '.') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '?') ) OR
        ( (PreviousSubtitleText > '') AND (Length(PreviousSubtitleText) >= 2) AND ( Copy(PreviousSubtitleText,length(PreviousSubtitleText),1) = '!') ) Then
        begin
          PosAlt := RichEdit.SelStart;
          RichEdit.Text := Copy(RichEdit.Text,1,3) + UpperCase(Copy(RichEdit.Text,4,1)) + Copy(RichEdit.Text,5,Length(RichEdit.Text)-4);
          RichEdit.SelStart := PosAlt;
        end;
  end;

  LeftSide := Copy(RichEdit.Text,1,RichEdit.SelStart);
  RightSide := Copy(RichEdit.Text,RichEdit.SelStart+1,length(RichEdit.Text)-RichEdit.SelStart+1);

  //TagSplit(RichEdit.Text, WordArray);
  TagSplit(LeftSide, WordArray);

  eventMask := RichEdit.Perform(EM_SETEVENTMASK, 0, 0);

  RichEdit.Lines.BeginUpdate;
  for i:=0 to Length(WordArray)-1 do
    begin
      Offset := 1;
      TextToSpell := WordArray[i];

      if  ( (Copy(TextToSpell,Length(TextToSpell)-1,1) = Chr(10)) AND
            (Copy(TextToSpell,Length(TextToSpell)-2,1) = Chr(13)) AND
          ( (Copy(TextToSpell,Length(TextToSpell)-3,1) = '.') OR
            (Copy(TextToSpell,Length(TextToSpell)-3,1) = '?') OR
            (Copy(TextToSpell,Length(TextToSpell)-3,1) = '!') )
          ) then
      begin
        if not ( (Length(TextToSpell)> 3) AND (Copy(TextToSpell,Length(TextToSpell)-4,1) = '.') ) then
          if  (Ord(Copy(TextToSpell,Length(TextToSpell),1)[1]) >= 97) AND
              (Ord(Copy(TextToSpell,Length(TextToSpell),1)[1]) <= 122) then
            begin
              AWord := Copy(TextToSpell,Length(TextToSpell)-2,3);
              AWordAlt := UpperCase(Copy(TextToSpell,Length(TextToSpell)-2,3));
              PosAlt := RichEdit.SelStart;
              //RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
              LeftSide := AnsiReplaceStr(LeftSide, AWord, AWordAlt);
              RichEdit.Text := LeftSide + RightSide;
              RichEdit.SelStart := PosAlt;
            end
          else
            if (Copy(TextToSpell,Length(TextToSpell),1) = 'è') then
              begin
                AWord := Copy(TextToSpell,Length(TextToSpell)-2,3);
                AWordAlt := Copy(TextToSpell,Length(TextToSpell)-2,2) + 'È';
                PosAlt := RichEdit.SelStart;
                //RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
                LeftSide := AnsiReplaceStr(LeftSide, AWord, AWordAlt);
                RichEdit.Text := LeftSide + RightSide;
                RichEdit.SelStart := PosAlt;
              end;
      end;

      if  ( (Copy(TextToSpell,Length(TextToSpell)-3,1) = Chr(10)) AND
            (Copy(TextToSpell,Length(TextToSpell)-4,1) = Chr(13)) AND
            (Copy(TextToSpell,Length(TextToSpell)-2,2) = '- ') ) then
      begin
        if  (Ord(Copy(TextToSpell,Length(TextToSpell),1)[1]) >= 97) AND
            (Ord(Copy(TextToSpell,Length(TextToSpell),1)[1]) <= 122) then
            begin
              AWord := Copy(TextToSpell,Length(TextToSpell)-4,5);
              AWordAlt := UpperCase(Copy(TextToSpell,Length(TextToSpell)-4,5));
              PosAlt := RichEdit.SelStart;
              //RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
              LeftSide := AnsiReplaceStr(LeftSide, AWord, AWordAlt);
              RichEdit.Text := LeftSide + RightSide;
              RichEdit.SelStart := PosAlt;
            end
          else
            if (Copy(TextToSpell,Length(TextToSpell),1) = 'è') then
              begin
                AWord := Copy(TextToSpell,Length(TextToSpell)-4,5);
                AWordAlt := Copy(TextToSpell,Length(TextToSpell)-4,4) + 'È';
                PosAlt := RichEdit.SelStart;
                //RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
                LeftSide := AnsiReplaceStr(LeftSide, AWord, AWordAlt);
                RichEdit.Text := LeftSide + RightSide;
                RichEdit.SelStart := PosAlt;
              end;
      end;

      if  ( (Copy(TextToSpell,Length(TextToSpell)-1,1) = ' ') AND
          ( (Copy(TextToSpell,Length(TextToSpell)-2,1) = '.') OR
            (Copy(TextToSpell,Length(TextToSpell)-2,1) = '?') OR
            (Copy(TextToSpell,Length(TextToSpell)-2,1) = '!') )
          ) then
      begin
        if (Length(TextToSpell)< 3) OR not ( (Length(TextToSpell)> 3) AND (Copy(TextToSpell,Length(TextToSpell)-3,1) = '.') ) then
          if (Ord(Copy(TextToSpell,Length(TextToSpell),1)[1]) >= 97) AND
             (Ord(Copy(TextToSpell,Length(TextToSpell),1)[1]) <= 122) then
            begin
              AWord := Copy(TextToSpell,Length(TextToSpell)-2,3);
              AWordAlt := UpperCase(Copy(TextToSpell,Length(TextToSpell)-2,3));
              PosAlt := RichEdit.SelStart;
              //RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
              LeftSide := AnsiReplaceStr(LeftSide, AWord, AWordAlt);
              RichEdit.Text := LeftSide + RightSide;
              RichEdit.SelStart := PosAlt;
            end
          else
            if (Copy(TextToSpell,Length(TextToSpell),1) = 'è') then
              begin
                AWord := Copy(TextToSpell,Length(TextToSpell)-2,3);
                AWordAlt := Copy(TextToSpell,Length(TextToSpell)-2,2) + 'È';
                PosAlt := RichEdit.SelStart;
                //RichEdit.Text := AnsiReplaceStr(RichEdit.Text, AWord, AWordAlt);
                LeftSide := AnsiReplaceStr(LeftSide, AWord, AWordAlt);
                RichEdit.Text := LeftSide + RightSide;
                RichEdit.SelStart := PosAlt;
              end;

      end;

    end;

  //RichEdit.Text := LeftSide + RightSide;

  RichEdit.Lines.EndUpdate;

  RichEdit.Perform(EM_SETEVENTMASK, 0, eventMask);

  RichEdit.Tag := 1;

end;

procedure TMainForm.MenuCustomDictionaryCopyToClipboardClick(
  Sender: TObject);
var Words: TTntStringList;
begin
 Words := TTntStringList.Create;
 {$IFNDEF enhanced}
 Words.LoadFromFile(g_DictPath + 'perso.dic');
 {$ELSE}
 Words.LoadFromFile(WideIncludeTrailingBackslash(g_AppDataDictPath) + 'perso.dic');
 {$ENDIF}
 Clipboard.AsText := Words.Text;
 Words.Free;
end;

procedure TMainForm.MenuCustomDictionaryPasteFromClipboardClick(
  Sender: TObject);
var Words: TTntStringList;
begin
  Words := TTntStringList.Create;
  Words.Text := Clipboard.AsText;
  Words.SaveToFile(g_DictPath + 'perso.dic');
  Words.Free;
  {$IFNDEF enhanced}
  FSpellChecker.LoadPersonalDict(g_DictPath + 'perso.dic');
  {$ELSE}
  FSpellChecker.LoadPersonalDict(WideIncludeTrailingBackslash(g_AppDataDictPath) + 'perso.dic');
  {$ENDIF}
  MemoSubtitleTextChange(nil);
end;

procedure TMainForm.MenuCustomDictionaryAppendFromClipboardClick(
  Sender: TObject);
var Words: TTntStringList;
begin
  Words := TTntStringList.Create;
  Words.LoadFromFile(g_DictPath + 'perso.dic');
  Words.Text := Words.Text + Clipboard.AsText;
  Words.SaveToFile(g_DictPath + 'perso.dic');
  Words.Free;
  {$IFNDEF enhanced}
  FSpellChecker.LoadPersonalDict(g_DictPath + 'perso.dic');
  {$ELSE}
  FSpellChecker.LoadPersonalDict(WideIncludeTrailingBackslash(g_AppDataDictPath) + 'perso.dic');
  {$ENDIF}
  MemoSubtitleTextChange(nil);
end;

procedure TMainForm.SubMenuCustomDictionaryClick(Sender: TObject);
var Words: TTntStringList;
begin
 Words := TTntStringList.Create;
 {$IFNDEF enhanced}
 Words.LoadFromFile(g_DictPath + 'perso.dic');
 {$ELSE}
 Words.LoadFromFile(WideIncludeTrailingBackslash(g_AppDataDictPath) + 'perso.dic');
 {$ENDIF}
 MenuCustomDictionaryCopyToClipboard.Enabled := True;
 if Words.Count=0 then MenuCustomDictionaryCopyToClipboard.Enabled := False;
 Words.Free;
end;

end.



