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

unit Renderer;

interface

uses Classes, Windows, DirectShow9, Messages, DirectVobsubInterface, LavVideoSettingsInterface,
     LavSplitterSettingsInterface,LavAudioSettingsInterface,Graphics;

type
  TWaitCompletionThread = class;

  TRenderer = class
  private
    FOnStopPlaying : TNotifyEvent;
  public
    function PlayRange(Start, Stop : Cardinal; Loop : Boolean = False) : Boolean; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Resume; virtual; abstract;
    procedure UpdatePlayRange(Start, Stop : Cardinal); virtual;
    function GetPosition : Cardinal; virtual; abstract;
    function GetDuration : Cardinal; virtual; abstract;
    procedure Stop(CallOnStopPlaying : Boolean = True); virtual; abstract;
    function IsOpen : Boolean; virtual; abstract;
    procedure SetRate(Rate : Integer); virtual;
    procedure SetVolume(Vol : Integer); virtual; abstract;
    procedure CopyState(Renderer : TRenderer); virtual; abstract;
  published
    property OnStopPlaying : TNotifyEvent read FOnStopPlaying write FOnStopPlaying;
  end;

  TDShowRenderer = class(TRenderer)
  private
    FGraphBuilder : IGraphBuilder;
    FMediaControl : IMediaControl;
    FMediaSeeking : IMediaSeeking;
    FMediaEventEx : IMediaEventEx;
    FVideoWindow : IVideoWindow;
    FBasicAudio : IBasicAudio;
    FDisplayWindowProc, FDisplayWindowOldProc : TFarProc;
    FLastResult : HRESULT;
    FWaitThread : TWaitCompletionThread;
    FLoop : Boolean;
    FStart, FStop : Int64;
    FVideoWidth, FVideoHeight : Integer;
    FVideoTop, FVideoLeft : Integer;
    FDisplayWindow : THandle;
    FIsOpen : Boolean;
    FStartStopAccessCS : TRtlCriticalSection;
    FAutoInsertCustomVSFilter : Boolean;
    FHCustomVSFilterInst : THandle;
    FCustomVSFilterIntallOk : Boolean;

    FROTID : Integer;

    function FGetStart : Int64;
    function FGetStop : Int64;

    function GetDirectVobSubFilter : IBaseFilter;
    function GetDirectVobSubInterface : IDirectVobSub;
    function GetDirectLavVideoSettingsFilter : IBaseFilter;
    function GetLavVideoSettingsInterface : ILAVVideoSettings;
    function GetDirectLavAudioSettingsFilter : IBaseFilter;
    function GetLavAudioSettingsInterface : ILAVAudioSettings;
    function GetDirectLavSplitterSettingsFilter : IBaseFilter;
    function GetLavSplitterSettingsInterface : ILAVFSettings;

  protected
    procedure DisplayWindowProc(var Mesg : TMessage);

  public
    constructor Create;
    destructor Destroy; override;
    function GetLastErrorString : string;
    function Open(filename : WideString; AudioOnly : Boolean) : Boolean;
    function OpenLavCodecBasedFilterGraph(filename : WideString) : Boolean;
    function PlayRange(Start, Stop : Cardinal; Loop : Boolean = False) : Boolean; override;
    procedure Pause; override;
    procedure Resume; override;
    procedure UpdatePlayRange(Start, Stop : Cardinal); override;
    function GetPosition : Cardinal; override;
    function GetDuration : Cardinal; override;
    procedure Stop(CallOnStopPlaying : Boolean = True); override;
    function IsPlaying : Boolean;
    function IsPaused : Boolean;
    procedure Replay;
    procedure SetDisplayWindow(WinHwnd : THandle);
    procedure UpdateDisplayWindow;
    procedure KillVideo;
    procedure Close;
    function IsOpen : Boolean; override;
    procedure SetRate(Rate : Integer); override;
    procedure ShowImageAt(TimeMs : Cardinal);
    procedure SetVolume(Vol : Integer); override;
    procedure SetSubtitleFilename(Filename : WideString);
    procedure SetSubtitleUpscalingSettings;
    procedure SetSubtitleFlipSettings;
    procedure SetSubtitleFont(fontDescription : WideString; Transparency : Integer; Outline : Integer; OutLineColor : TColor; TransparencyOutline : Integer; Shadow : Integer; ShadowColor : TColor; TransparencyShadow : Integer);
    procedure HideSubtitle(Hide : boolean);
    procedure SetAutoInsertCustomVSFilter(AutoInsert : Boolean);
    procedure UpdateImage;
    procedure CopyState(Renderer : TRenderer); override;
    function GetVSFilterFPS : Double;
    function GetFilters(list : TStrings) : Boolean;
    function GetFiltersAsString : string;
    function RendererAllOutputAudioPin(Filter : IBaseFilter) : Integer;
    function RendererSelectedOutputAudioPin(Filter : IBaseFilter; Track : Integer) : Integer;
    function RendererAllOutputVideoPin(Filter : IBaseFilter) : Integer;
    function GetLavHwModeSupported : String;
    
  published
    property OnStopPlaying;
    property VideoWidth : Integer read FVideoWidth;
    property VideoHeight : Integer read FVideoHeight;
    property StartTime : Int64 read FGetStart;
    property StopTime : Int64 read FGetStop;
  end;

  TWaitCompletionThread = class(TThread)
  private
    FRenderer : TDShowRenderer;
    hTerminateEvent : THandle;
  public
    constructor Create(Renderer : TDShowRenderer);
    destructor Destroy; override;
    procedure Execute; override;
    procedure SignalTermination;
  end;

  TWAVExtractionType = (wetOnlyPeakFile, wetNoConversion, wetFastConversion);
  TDSWavExtractor = class
  private
    FGraphBuilder : IGraphBuilder;
    FMediaControl : IMediaControl;
    FMediaSeeking : IMediaSeeking;
    FMediaEventEx : IMediaEventEx;
    FLastResult : HRESULT;
    FSplitter : IBaseFilter;
    FAudioStreamCount : Integer;
    FDuration : Int64;
    FSourceFilename : WideString;
    FDestinationFilename : WideString;
    FHWavWriterInst : THandle;
    FWAVExtractionType : TWAVExtractionType;

    FROTID : Integer;

    function GetOutputAudioPinCount(Filter : IBaseFilter) : Integer;
    function RendererAllOutputAudioPin(Filter : IBaseFilter) : Integer;
    function RendererSelectedOutputAudioPin(Filter : IBaseFilter; Track : Integer) : Integer;
    function RendererAllOutputVideoPin(Filter : IBaseFilter) : Integer;
    function RendererAllOutputPin(Filter : IBaseFilter) : Integer;
    function GetPCMOutputPin(Filter : IBaseFilter) : IPin;
    function InstallWriter(Pin : IPin) : Boolean;
    function GetDirectLavAudioSettingsFilter : IBaseFilter;
    function GetLavAudioSettingsInterface : ILAVAudioSettings;
    function GetDirectLavSplitterSettingsFilter : IBaseFilter;
    function GetLavSplitterSettingsInterface : ILAVFSettings;

  public
    IsAc3Audio : Boolean;
    IsPcmAudio : Boolean;
    IsDtsAudio : Boolean;
    IsAacAudio : Boolean;
    IsMpegAudio : Boolean;
    IsFlacAudio : Boolean;
    IsMatroskaContainer : Boolean;
    IsAviContainer : Boolean;
    IsMp4Container : Boolean;
    IsFlvContainer : Boolean;
    IsMpegContainer : Boolean;
    constructor Create;
    destructor Destroy; override;
    function Open(Filename : WideString; AudioTrack : integer) : Boolean;
    function SelectAudioPin(Index : Integer) : Boolean;
    procedure Go;
    function GetLastErrorString : string;
    function GetProgress : Integer;
    function IsFinished : Boolean;
    procedure Close;
    function GetFilters(list : TStrings) : Boolean;
    function GetFiltersAsString : string;

  published
    property AudioStreamCount : Integer read FAudioStreamCount;
    property DestinationFilename : WideString read FDestinationFilename write FDestinationFilename;
    property WAVExtractionType : TWAVExtractionType read FWAVExtractionType write FWAVExtractionType;
  end;

  IWavWriterInterface = interface(IUnknown)
    function SetFastConversionMode(bFastConvertMode : DWORD) : HRESULT; stdcall;
    function GetFastConversionMode(out bFastConvertMode : DWORD) : HRESULT; stdcall;
    function SetSamplesPerPeakRatio(SamplePerPeakRatio : DWORD) : HRESULT; stdcall;
    function SetWritePeakFile(WritePeakFile : DWORD) : HRESULT; stdcall;
    function SetPeakFileName(pszFileName: PWideChar) : HRESULT; stdcall;
    function SetWriteWavFile(WriteWavFile : DWORD) : HRESULT; stdcall;
  end;

implementation

uses ActiveX, MMSystem, SysUtils, Types, MiscToolsUnit, Math, TntSysUtils, Main, Registry, ShellAPI, ProjectUnit;

const
  CLSID_WavWriter : TGUID = '{F3AFF1C3-ABBB-41f9-9521-988881D9D640}';
  IID_IWavWriter : TGUID = '{3B7A03CE-3D3B-4fef-8CDE-3D6C2709AFF1}';

//==============================================================================

function CreateFilterFromFile(hLibInst : THandle; Guid : TGUID; out Filter) : HRESULT;
var DllGetClassObject : function(const clsid, iid : TGUID; var Obj) : HRESULT; stdcall;
    ClassFactory : IClassFactory;
begin
  Result := S_FALSE;
  DllGetClassObject := GetProcAddress(hLibInst, 'DllGetClassObject');
  if Assigned(DllGetClassObject) then
  begin
    Result := DllGetClassObject(Guid, IClassFactory, ClassFactory);
    if Result = S_OK then
    begin
      Result := ClassFactory.CreateInstance(nil,IBaseFilter,Filter);
    end;
  end;
end;

function FindFilterByCLSID(GraphBuilder : IGraphBuilder; CLSID: TGUID): IBaseFilter;
var
  EnumFilter: IEnumFilters;
  Fetched: ULong;
  BaseFilter: IBaseFilter;
  FilterCLSID: TGuid;
begin
  Result := nil;

  if GraphBuilder.EnumFilters(EnumFilter) <> S_Ok then
    Exit;

  while EnumFilter.Next(1, BaseFilter, @Fetched) = S_Ok do
  begin
    BaseFilter.GetClassID(FilterCLSID);
    if IsEqualGUID(FilterCLSID, CLSID) then
    begin 
      Result := BaseFilter;
      Break;
    end;
    BaseFilter := nil;
  end;
  BaseFilter := nil;
  EnumFilter := nil;
end;

//------------------------------------------------------------------------------

function GetFilterPin(Filter : IBaseFilter; Direction : TPinDirection; out ResultPin : IPin) : HRESULT;
var EnumPins: IEnumPins;
    Pin: IPin;
    ul: ULONG;
    PinDir: TPinDirection;
begin
  Result := S_FALSE;
  if Filter = nil then
    Exit;

  if Filter.EnumPins(EnumPins) <> S_OK then
    Exit;

  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if PinDir = Direction then
    begin
      ResultPin := Pin;
      Break;
    end;
    Pin := nil;
  end;
  Pin := nil;
  EnumPins := nil;
end;

//------------------------------------------------------------------------------

function GetFilterPinByIndex(Filter : IBaseFilter; Direction : TPinDirection; Index : Integer; out ResultPin : IPin) : HRESULT;
var EnumPins: IEnumPins;
    Pin: IPin;
    ul: ULONG;
    PinDir: TPinDirection; I : Integer;
begin
  Result := S_FALSE;
  if Filter = nil then
    Exit;

  if Filter.EnumPins(EnumPins) <> S_OK then
    Exit;

  I := 0;
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if PinDir = Direction then
    begin
      I := I + 1;
      if I = Index then
       begin
        ResultPin := Pin;
        Break;
       end;
    end;
    Pin := nil;
  end;
  Pin := nil;
  EnumPins := nil;
end;

//------------------------------------------------------------------------------
function GetFilterPinByName(Filter : IBaseFilter; Direction : TPinDirection; PinName:String; out ResultPin : IPin) : HRESULT;
var EnumPins: IEnumPins;
    Pin: IPin;
    ul: ULONG;
    PinDir: TPinDirection;
    PinInfo: TPinInfo;
begin
  Result := S_FALSE;
  if Filter = nil then
    Exit;

  if Filter.EnumPins(EnumPins) <> S_OK then
    Exit;

  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    Pin.QueryPinInfo(PinInfo);
    if (PinDir = Direction) And (PinInfo.achName='Video') then
    begin
      ResultPin := Pin;
      Break;
    end;
    Pin := nil;
  end;
  Pin := nil;
  EnumPins := nil;
end;

//------------------------------------------------------------------------------

function NukeDownstream(GraphBuilder : IGraphBuilder; Pin : IPin) : Boolean;
var
  PinConnectedTo, PinOut : IPin;
  EnumPins : IEnumPins;
  ul: ULONG;
  Filter : IBaseFilter;
	PinInfo : TPinInfo;
  PinDir: TPinDirection;
begin
  Pin.ConnectedTo(PinConnectedTo);
  if PinConnectedTo = nil then
  begin
    Result := False;
    Exit;
  end;
  PinConnectedTo.QueryPinInfo(PinInfo);
  Filter := PinInfo.pFilter;
  Filter.EnumPins(EnumPins);

  while (EnumPins.Next(1,PinOut, @ul) = S_OK) do
  begin
    PinOut.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) then
    begin
      NukeDownstream(GraphBuilder,PinOut);
    end;
    PinOut := nil;
  end;
  EnumPins := nil;
  PinConnectedTo := nil;
  GraphBuilder.RemoveFilter(Filter);
  Filter := nil;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure DeleteMediaType(pmt: PAMMediaType);
begin
  if (pmt <> nil) then
  begin
    if (pmt^.cbFormat <> 0) then
      CoTaskMemFree(pmt^.pbFormat);
    if (pmt^.pUnk <> nil) then
      pmt^.pUnk := nil;
    CoTaskMemFree(pmt);
  end;
end;

//------------------------------------------------------------------------------

function IsAudioPin(Pin : IPin) : Boolean;
var
  EnumMT : IEnumMediaTypes;
  ul : ULONG;
  pMT : PAMMediaType;
begin
  Result := False;
  Pin.EnumMediaTypes(EnumMT);
  while (EnumMT.Next(1,pMT,@ul) = S_OK) do
  begin
    if IsEqualGUID(pMT.majortype, MEDIATYPE_Audio) then
    begin
      Result := True;
      DeleteMediaType(pMT);
      Break;
    end
    else
      DeleteMediaType(pMT);
  end;
  EnumMT := nil;
end;

//------------------------------------------------------------------------------

function IsVideoPin(Pin : IPin) : Boolean;
var
  EnumMT : IEnumMediaTypes;
  ul : ULONG;
  pMT : PAMMediaType;
begin
  Result := False;
  Pin.EnumMediaTypes(EnumMT);
  while (EnumMT.Next(1,pMT,@ul) = S_OK) do
  begin
    if IsEqualGUID(pMT.majortype, MEDIATYPE_Video) then
    begin
      Result := True;
      DeleteMediaType(pMT);
      Break;
    end
    else
      DeleteMediaType(pMT);
  end;
  EnumMT := nil;
end;

//------------------------------------------------------------------------------

function GetDllFilenameByCLSID(const GUID : TGUID) : string;
var KeyName, FileName : string;
    Key : HKEY;
    dwSize : DWORD;
begin
  Result := '';
  if IsEqualCLSID(GUID, GUID_NULL) then
    Exit;

  KeyName := Format('Software\Classes\CLSID\%s\InprocServer32', [GUIDToString(GUID)]);

  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(KeyName), 0, KEY_READ, Key) = ERROR_SUCCESS) then
  begin
    dwSize := 0;
    if (RegQueryValueEx(Key, nil, nil, nil, nil, @dwSize) = ERROR_SUCCESS) then
    begin
      SetLength(FileName, dwSize-1);
      if RegQueryValueEx(Key, nil, nil, nil, PBYTE(FileName), @dwSize) = ERROR_SUCCESS then
      begin
        Result := FileName;
      end;
    end;
    RegCloseKey(Key);
  end;
end;

// -----------------------------------------------------------------------------

function GetFriendlyNameByCLSID(const GUID : TGUID) : string;
var KeyName, FriendlyName : string;
    Key : HKEY;
    dwSize : DWORD;
begin
  Result := '';
  if IsEqualCLSID(GUID, GUID_NULL) then
    Exit;

  KeyName := Format('Software\Classes\CLSID\%s', [GUIDToString(GUID)]);
  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(KeyName), 0, KEY_READ, Key) = ERROR_SUCCESS) then
  begin
    dwSize := 0;
    if (RegQueryValueEx(Key, nil, nil, nil, nil, @dwSize) = ERROR_SUCCESS) then
    begin
      SetLength(FriendlyName, dwSize-1);
      if RegQueryValueEx(Key, nil, nil, nil, PBYTE(FriendlyName), @dwSize) = ERROR_SUCCESS then
      begin
        Result := FriendlyName;
      end;
    end;
    RegCloseKey(Key);
  end;
end;

// -----------------------------------------------------------------------------

function GetFiltersList(GraphBuilder : IGraphBuilder; list : TStrings) : Boolean;
var FilterEnum : IEnumFilters;
    Filter : IBaseFilter;
    ul: ULONG;
    FilterInfo : TFilterInfo;
    s : WideString;
    FilterGUID : TGUID;
    FileName, FriendlyName : WideString;
const
  IID_IPropertyBag : TGUID = '{55272A00-42CB-11CE-8135-00AA004BB851}';    
begin
  list.Clear;
  Result := False;
  if Assigned(GraphBuilder) then
  begin
    if Succeeded(GraphBuilder.EnumFilters(FilterEnum)) then
    begin
      while (FilterEnum.Next(1, Filter, @ul) = S_OK) do
      begin
        FilterInfo.achName[0] := #0;
        Filter.QueryFilterInfo(FilterInfo);
        s := FilterInfo.achName;
        FilterGUID := GUID_NULL;
        if Succeeded(Filter.GetClassID(FilterGUID)) then
        begin
          FriendlyName := GetFriendlyNameByCLSID(FilterGUID);
          if (Length(FriendlyName) > 0) and (s <> FriendlyName) then
          begin
            s := s + ' - ' + FriendlyName;
          end;
          s := s + ' - ' + GUIDToString(FilterGUID);
          FileName := GetDllFilenameByCLSID(FilterGUID);
          if (Length(FileName) > 0) then
          begin
            s := s + ' - ' + FileName;
            s := s + ' (' + GetFileVersionString(FileName) + ')';
          end;
        end;

        list.Add(s);
        Filter := nil;
      end;
      FilterEnum := nil;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function AddToRot(Graph: IFilterGraph; out dwRegister : Integer): HRESULT;
var Moniker : IMoniker;
    ROT : IRunningObjectTable;
    wsz : WideString;
begin
  //EXIT;
  Result := GetRunningObjectTable(0, ROT);
  if Failed(Result) then
    Exit;
  wsz := Format('FilterGraph %p pid %x',[Pointer(Graph), GetCurrentProcessId()]);
  Result  := CreateItemMoniker('!', PWideChar(wsz), Moniker);
  if Failed(Result) then
    Exit;
  Result := ROT.Register(0, Graph, Moniker, dwRegister);
  Moniker := nil;
end;

// -----------------------------------------------------------------------------

function RemoveFromRot(dwRegister: Integer): HRESULT;
var ROT: IRunningObjectTable;
begin
  Result := GetRunningObjectTable(0, ROT);
  if Failed(Result) then
    Exit;
  Result := ROT.Revoke(dwRegister);
  ROT := nil;
end;

// -----------------------------------------------------------------------------

procedure TRenderer.UpdatePlayRange(Start, Stop : Cardinal);
begin
  // do nothing
end;

procedure TRenderer.SetRate(Rate : Integer);
begin
  // do nothing
end;

//==============================================================================

constructor TDShowRenderer.Create;
begin
  FGraphBuilder := nil;
  FMediaControl := nil;
  FMediaSeeking := nil;
  FMediaEventEx := nil;
  FOnStopPlaying := nil;
  FWaitThread := nil;
  FVideoWindow := nil;
  FLoop := False;
  FLastResult := S_OK;
  FIsOpen := False;
  FDisplayWindow := 0;
  FDisplayWindowProc := nil;
  FDisplayWindowOldProc := nil;
  FVideoWidth := 0;
  FVideoHeight := 0;
  FCustomVSFilterIntallOk := False;
  FAutoInsertCustomVSFilter := False;
  FHCustomVSFilterInst := 0;
  FROTID := 0;

  InitializeCriticalSection(FStartStopAccessCS);
end;

//------------------------------------------------------------------------------

destructor TDShowRenderer.Destroy;
begin
  if Assigned(FWaitThread) then
  begin
    FWaitThread.SignalTermination;
    FWaitThread.WaitFor;
    FWaitThread.Free;
    FWaitThread := nil;
  end;
  Close;
  DeleteCriticalSection(FStartStopAccessCS);
  inherited;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetLastErrorString : string;
begin
  SetLength(Result,MAX_ERROR_TEXT_LEN);
  AMGetErrorText(FLastResult,@Result[1],MAX_ERROR_TEXT_LEN);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetFilters(list : TStrings) : Boolean;
begin
  Result := GetFiltersList(FGraphBuilder, list);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetFiltersAsString : string;
var strList : TStringList;
begin
  strList := TStringList.Create;
  GetFiltersList(FGraphBuilder, strList);
  Result := strList.Text;
  FreeAndNil(strList);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetAutoInsertCustomVSFilter(AutoInsert : Boolean);
begin
  if (FAutoInsertCustomVSFilter <> AutoInsert) then
  begin
    FAutoInsertCustomVSFilter := AutoInsert;
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.Open(filename : WideString; AudioOnly : Boolean) : Boolean;
Const
  CLSID_MPCVideoDecoder: TGuid = '{008BAC12-FBAF-497B-9670-BC6F6FBAE2C4}';
  CLSID_MPCMpeg2VideoDecoder: TGuid = '{39F498AF-1A09-4275-B193-673B0BA3D478}';
  CLSID_MPCAudioDecoder: TGuid = '{3D446B6F-71DE-4437-BE15-8CE47174340F}';
  CLSID_MadFlacAudioDecoder: TGuid = '{6B257121-CBB6-46B3-ABFA-B14DFA98C4A6}';
  CLSID_Reclock: TGuid = '{9DC15360-914C-46B8-B9DF-BFE67FD36C6A}';
  CLSID_VideoRenderer: TGuid = '{B87BEB7B-8D29-423F-AE4D-6582C10175AC}';
  CLSID_Vmr7Renderer: TGuid = '{B87BEB7B-8D29-423F-AE4D-6582C10175AC}';
  CLSID_Vmr9Renderer: TGuid = '{51B4ABF3-748F-4E3B-A276-C828330E926A}';
  CLSID_EnhRenderer: TGuid = '{FA10746C-9B63-4B6C-BC49-FC300EA5F256}';
  CLSID_MPCMatroskaSplitter: TGuid = '{149D2E01-C32E-4939-80F6-C07B81015A7A}';
  CLSID_MPCAviSplitter: TGuid = '{9736D831-9D6C-4E72-B6E7-560EF9181001}';
  CLSID_MPCMp4Splitter: TGuid = '{61F47056-E400-43D3-AF1E-AB7DFFD4C4AD}';
  CLSID_MPCFlvSplitter: TGuid = '{47E792CF-0BBE-4F7A-859C-194B0768650A}';
  CLSID_MPCMpegSplitter: TGuid = '{DC257063-045F-4BE2-BD5B-E12279C464F0}';
  CLSID_FileSourceAsync: TGuid = '{E436EBB5-524F-11CE-9F53-0020AF0BA770}';
  CLSID_LavAudioDecoder: TGuid = '{E8E73B6B-4CB3-44A4-BE99-4F7BCB96E491}';
  CLSID_LavSplitter: TGuid = '{171252A0-8820-4AFE-9DF8-5C92B2D66B04}';
  CLSID_LavSplitterSource: TGuid = '{B98D13E7-55DB-4385-A33D-09FD1BA26338}';

var
  BasicVideoI : IBasicVideo2;
  VSFilter : IBaseFilter;
  VSFilterVendorInfo : PWideChar;
  FHMPCVideoFilterInst,FHMPCAudioFilterInst, FHMPCFileSourceAsync, FHMPCSubtitleSourceAsync,
  FHMPCMatroskaSplitter, FHMPCAviSplitter, FHMPCMp4Splitter, FHMPCFlvSplitter, FHMPCMpegSplitter,
  FHLAVVideoFilterInst, FHLavAudioFilterInst, FHLavSplitter : THandle;
  VideoDecoderFilter, AudioDecoderFilter, FileSourceAsyncFilter, SourceAsyncFilter,
  SplitterFilter,Reclock,StandardRenderer,Renderer : IBaseFilter;
  PinOut,PinIn : IPin;
  PinInVideo,PinOutVideo,PinInVideoConnectedTo, PinOutVideoConnectedTo : IPin;
  PinInVsFilter,PinOutVsFilter,PinOutSubtitleSource,PinInVsFilterConnectedTo,PinOutVsFilterConnectedTo : IPin;
  PinInRenderer,PinInRendererConnectedTo : IPin;
  PinInStandardRenderer,PinInStandardRendererConnectedTo : IPin;
  LAVVideoSettings : ILAVVideoSettings;
  LAVSplitterSettings : ILAVFSettings;
  Pins : IEnumPins; ul : ULONG;
  PinFileSourceOutput : IPin;
  PinSplitterInput : IPin;
  LAVAudioSettings : ILAVAudioSettings;
  centerLevel, surroundLevel, LfeLevel : DWORD;
  CodecPath : WideString;
  pLanguages : WideString;
begin

  if not AudioOnly AND NOT (MainForm.CurrentProject.VideoStreamCount < '1') AND MainForm.ConfigObject.UseInternalFilters AND MainForm.ConfigObject.UseAlternativeInternalCodec then
  begin
    Result := OpenLavCodecBasedFilterGraph(filename);
    EXIT;
  end;

  if Assigned(FGraphBuilder) then
    Close;
  Result := False;
  if (filename = '') or (not WideFileExists(filename)) then
    Exit;

  if not AudioOnly AND (MainForm.CurrentProject.VideoStreamCount < '1') then
    Exit;

  FLastResult := CoCreateInstance(TGUID(CLSID_FilterGraph), nil, CLSCTX_INPROC,
    TGUID(IID_IGraphBuilder), FGraphBuilder);
  GetLastErrorString;
  if (FLastResult <> S_OK) then Exit;

  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEventEx);
  if (FLastResult <> S_OK) then Exit;

  FCustomVSFilterIntallOk := False;
  if (FAutoInsertCustomVSFilter) And not AudioOnly then
   begin
    CodecPath := ExtractFilePath(ParamStr(0))+'VSSCustomVSFilterV2a.dll';
    FHCustomVSFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
    if (FHCustomVSFilterInst <> 0) then
    begin
      FLastResult := CreateFilterFromFile(FHCustomVSFilterInst,
        CLSID_DirectVobSubFilter, VSFilter);
      if Succeeded(FLastResult) then
       begin
        FGraphBuilder.AddFilter(VSFilter, 'VSFilter Custom (internal)');
        SetSubtitleFont(MainForm.ConfigObject.SubVideoFont, Trunc(MainForm.ConfigObject.TransparencySubVideoFont / 10 * $FF),
          MainForm.ConfigObject.OutlineSubVideoFont,MainForm.ConfigObject.OutlineSubVideoColorFont, Trunc(MainForm.ConfigObject.TransparencyOutlineSubVideoFont / 10 * $FF),
          MainForm.ConfigObject.ShadowSubVideoFont,MainForm.ConfigObject.ShadowSubVideoColorFont, Trunc(MainForm.ConfigObject.TransparencyShadowSubVideoFont / 10 * $FF));
        SetSubtitleUpscalingSettings;
        SetSubtitleFlipSettings;
       end;
    end;
   end;

  // **************************************************
  // **************************************************
  // **************************************************
  // INTERNAL CODEC (DEFAULT)
  // **************************************************
  // **************************************************
  // **************************************************

  if MainForm.ConfigObject.UseInternalFilters AND MainForm.ConfigObject.UseDefaultInternalCodec AND
     NOT (AudioOnly AND (MainForm.CurrentProject.WAVMode = pwmExternal) ) then
   begin

    if not AudioOnly then
     begin

      if (pos('MPEG-2 VIDEO', uppercase(MainForm.CurrentProject.VideoFormat))>0) then
       begin
        FLastResult := CoCreateInstance(CLSID_MPCMpeg2VideoDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VideoDecoderFilter);
        if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
         begin
          FLastResult := FGraphBuilder.AddFilter(VideoDecoderFilter, 'MPC - MPEG-2 Video Decoder');
         end
        else
         begin
          CodecPath := ExtractFilePath(ParamStr(0))+'Codec\Mpeg2DecFilter.ax';
          FHMPCVideoFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
          if (FHMPCVideoFilterInst <> 0) then
           begin
            FLastResult := CreateFilterFromFile(FHMPCVideoFilterInst,
              CLSID_MPCMpeg2VideoDecoder, VideoDecoderFilter);
            if Succeeded(FLastResult) then
             FGraphBuilder.AddFilter(VideoDecoderFilter, 'MPC - MPEG-2 Video Decoder (internal)');
           end;
         end;
       end
      else
       begin
        FLastResult := CoCreateInstance(CLSID_MPCVideoDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VideoDecoderFilter);
        if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
         begin
          FLastResult := FGraphBuilder.AddFilter(VideoDecoderFilter, 'MPC Video decoder');
         end
        else
         begin
          CodecPath := ExtractFilePath(ParamStr(0))+'Codec\MPCVideoDec.ax';
          FHMPCVideoFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
          if (FHMPCVideoFilterInst <> 0) then
           begin
            FLastResult := CreateFilterFromFile(FHMPCVideoFilterInst,
              CLSID_MPCVideoDecoder, VideoDecoderFilter);
            if Succeeded(FLastResult) then
             FGraphBuilder.AddFilter(VideoDecoderFilter, 'MPC Video decoder (internal)');
           end;
         end;

       end;
      // ---------------------------

     end;

     // audio decoders
     if (pos('AC-3', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
        (pos('DTS', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
        (pos('MPEG AUDIO', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
        (pos('PCM', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
        (pos('AAC', uppercase(MainForm.CurrentProject.AudioFormat))>0) then
     begin
      FLastResult := CoCreateInstance(CLSID_MPCAudioDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioDecoderFilter);
      if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
       begin
        FLastResult := FGraphBuilder.AddFilter(AudioDecoderFilter, 'MPA Audio decoder');
       end
      else
       begin
        CodecPath := ExtractFilePath(ParamStr(0))+'Codec\MpaDecFilter.ax';
        FHMPCAudioFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
        if (FHMPCAudioFilterInst <> 0) then
         begin
          FLastResult := CreateFilterFromFile(FHMPCAudioFilterInst,
            CLSID_MPCAudioDecoder, AudioDecoderFilter);
          if Succeeded(FLastResult) then
            FGraphBuilder.AddFilter(AudioDecoderFilter, 'MPA Audio decoder (internal)');
         end;
       end;
     end;

     if pos('FLAC', uppercase(MainForm.CurrentProject.AudioFormat))>0 then
      begin
       FLastResult := CoCreateInstance(CLSID_MadFlacAudioDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioDecoderFilter);
       if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(AudioDecoderFilter, 'MadFLAC Audio decoder');
        end
       else
        begin
         CodecPath := ExtractFilePath(ParamStr(0))+'Codec\madFlac.ax';
         FHMPCAudioFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
         if (FHMPCAudioFilterInst <> 0) then
          begin
           FLastResult := CreateFilterFromFile(FHMPCAudioFilterInst,
             CLSID_MadFlacAudioDecoder, AudioDecoderFilter);
           if Succeeded(FLastResult) then
             FGraphBuilder.AddFilter(AudioDecoderFilter, 'MadFLAC Audio decoder (internal)');
          end;
        end;
      end;

   end;

  // **************************************************
  // **************************************************
  // **************************************************
  // INTERNAL CODEC (ALTERNATIVE, LAV)
  // **************************************************
  // **************************************************
  // **************************************************

  if MainForm.ConfigObject.UseInternalFilters AND MainForm.ConfigObject.UseAlternativeInternalCodec AND
     NOT (AudioOnly AND (MainForm.CurrentProject.WAVMode = pwmExternal) ) then
   begin

     if not AudioOnly then
      begin

       FLastResult := CoCreateInstance(CLSID_LavVideoDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VideoDecoderFilter);
       if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
        begin
          FGraphBuilder.AddFilter(VideoDecoderFilter, 'LAV Video Decoder');
        end
       else
        begin
         CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVVideo.ax';
         FHLAVVideoFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
         if (FHLAVVideoFilterInst <> 0) then
          begin
           FLastResult := CreateFilterFromFile(FHLAVVideoFilterInst,
             CLSID_LavVideoDecoder, VideoDecoderFilter);
           if Succeeded(FLastResult) then
            FGraphBuilder.AddFilter(VideoDecoderFilter, 'LAV Video Decoder (internal)');
          end;
        end;

        LAVVideoSettings := GetLavVideoSettingsInterface;
        LAVVideoSettings.SetTrayIcon(False);
        LAVVideoSettings.SetRuntimeConfig(True);
        if MainForm.ConfigObject.LavVHwNone then LAVVideoSettings.SetHWAccel(HWAccel_None);
        if MainForm.ConfigObject.LavVQuickSync then LAVVideoSettings.SetHWAccel(HWAccel_QuickSync);
        if MainForm.ConfigObject.LavVCuda then LAVVideoSettings.SetHWAccel(HWAccel_CUDA);
        if MainForm.ConfigObject.LavVDxva2 then  LAVVideoSettings.SetHWAccel(HWAccel_DXVA2CopyBack);

      end;
   // ---------------------------

   // audio decoders
   if (pos('AC-3', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
      (pos('DTS', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
      (pos('MPEG AUDIO', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
      (pos('PCM', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
      (pos('AAC', uppercase(MainForm.CurrentProject.AudioFormat))>0) OR
      (pos('FLAC', uppercase(MainForm.CurrentProject.AudioFormat))>0) then
    begin

     FLastResult := CoCreateInstance(CLSID_LavAudioDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioDecoderFilter);
     if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
      begin
       FGraphBuilder.AddFilter(AudioDecoderFilter, 'LAV Audio Decoder');
      end
     else
      begin
       CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVAudio.ax';
       FHLavAudioFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
       if (FHLavAudioFilterInst <> 0) then
        begin
         FLastResult := CreateFilterFromFile(FHLavAudioFilterInst,
           CLSID_LavAudioDecoder, AudioDecoderFilter);
         if Succeeded(FLastResult) then
          FGraphBuilder.AddFilter(AudioDecoderFilter, 'LAV Audio Decoder (internal)');
        end;
      end;

     LAVAudioSettings := GetLavAudioSettingsInterface;
     LAVAudioSettings.SetTrayIcon(False);
     LAVAudioSettings.SetRuntimeConfig(True);
     LAVAudioSettings.SetAudioDelay(False,0);
     if MainForm.ConfigObject.EnableLavAudioMixing then
      begin
       LAVAudioSettings.SetMixingEnabled(True);
       LAVAudioSettings.GetMixingLevels(centerLevel, surroundLevel, LfeLevel);
       LAVAudioSettings.SetMixingLevels(centerLevel, surroundLevel, LfeLevel);
       LAVAudioSettings.SetMixingFlags(LAV_MIXING_FLAG_UNTOUCHED_STEREO);
      end;

    end;

   end;

  if MainForm.ConfigObject.UseReclockAudioRenderer then
   begin
    if not AudioOnly then
     begin
      FLastResult := CoCreateInstance(CLSID_Reclock, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, Reclock);
      if Succeeded(FLastResult) then
       begin
        FLastResult := FGraphBuilder.AddFilter(Reclock, 'Reclock Audio Renderer');
       end;
     end;
   end;

  // MPC Matroska Splitter, Avi splitter, Mp4 Splitter, Mpeg Splitter
  if  MainForm.ConfigObject.UseInternalFilters AND NOT (AudioOnly AND (MainForm.CurrentProject.WAVMode = pwmExternal) ) AND
     (MainForm.CurrentProject.MatroskaContainer OR MainForm.CurrentProject.AviContainer OR MainForm.CurrentProject.Mp4Container OR
      MainForm.CurrentProject.FlvContainer OR MainForm.CurrentProject.MpegContainer) then
   begin

     if MainForm.CurrentProject.MatroskaContainer AND MainForm.ConfigObject.UseDefaultInternalCodec then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCMatroskaSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Matroska Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\MatroskaSplitter.ax';
           FHMPCMatroskaSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCMatroskaSplitter <> 0) then
             begin
              FLastResult := CreateFilterFromFile(FHMPCMatroskaSplitter,
               CLSID_MPCMatroskaSplitter, SplitterFilter);
              FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Matroska Splitter (internal)');
             end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          RendererSelectedOutputAudioPin(SplitterFilter,MainForm.CurrentProject.SelectedAudioTrackOnExtract);
          RendererAllOutputVideoPin(SplitterFilter);

        end;
      end;

     if MainForm.CurrentProject.AviContainer AND MainForm.ConfigObject.UseDefaultInternalCodec then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCAviSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Avi Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\AviSplitter.ax';
           FHMPCAviSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCAviSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCAviSplitter,
               CLSID_MPCAviSplitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Avi Splitter (internal)');
            end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          RendererSelectedOutputAudioPin(SplitterFilter,MainForm.CurrentProject.SelectedAudioTrackOnExtract);
          RendererAllOutputVideoPin(SplitterFilter);

        end;
      end;

     if MainForm.CurrentProject.Mp4Container AND MainForm.ConfigObject.UseDefaultInternalCodec then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCMp4Splitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mp4 Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\Mp4Splitter.ax';
           FHMPCMp4Splitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCMp4Splitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCMp4Splitter,
               CLSID_MPCMp4Splitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mp4 Splitter (internal)');
            end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          RendererSelectedOutputAudioPin(SplitterFilter,MainForm.CurrentProject.SelectedAudioTrackOnExtract);
          RendererAllOutputVideoPin(SplitterFilter);

        end;
      end;

     if MainForm.CurrentProject.FlvContainer AND MainForm.ConfigObject.UseDefaultInternalCodec then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCFlvSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Flv Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\FlvSplitter.ax';
           FHMPCFlvSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCFlvSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCFlvSplitter,
               CLSID_MPCFlvSplitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Flv Splitter (internal)');

            end;
          end;

         SplitterFilter.FindPin('Input', PinSplitterInput);
         PinFileSourceOutput.Connect(PinSplitterInput,nil);
         RendererSelectedOutputAudioPin(SplitterFilter,MainForm.CurrentProject.SelectedAudioTrackOnExtract);
         RendererAllOutputVideoPin(SplitterFilter);

        end;
      end;

     if MainForm.CurrentProject.MpegContainer AND MainForm.ConfigObject.UseDefaultInternalCodec then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCMpegSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mpeg Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\MpegSplitter.ax';
           FHMPCMpegSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCMpegSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCMpegSplitter,
               CLSID_MPCMpegSplitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mpeg Splitter (internal)');
           end;
         end;

         SplitterFilter.FindPin('Input', PinSplitterInput);
         PinFileSourceOutput.Connect(PinSplitterInput,nil);
         RendererSelectedOutputAudioPin(SplitterFilter,MainForm.CurrentProject.SelectedAudioTrackOnExtract);
         RendererAllOutputVideoPin(SplitterFilter);

        end;
      end;

     if MainForm.ConfigObject.UseAlternativeInternalCodec then
      begin

        FLastResult := CoCreateInstance(CLSID_LavSplitterSource, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
        if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
         begin
          FGraphBuilder.AddFilter(SplitterFilter, 'LAV Splitter');

          LAVSplitterSettings := GetLavSplitterSettingsInterface;
          LAVSplitterSettings.SetRuntimeConfig(True);
          LAVSplitterSettings.SetTrayIcon(False);
          pLanguages := MainForm.ConfigObject.LavPreferredLanguages;
          LAVSplitterSettings.SetPreferredLanguages(PWideChar(pLanguages));

          (SplitterFilter as IFileSourceFilter).Load(@Filename[1],nil);
         end
        else
         begin
          CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVSplitter.ax';
          FHLavSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
          if (FHLavSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHLavSplitter,
              CLSID_LavSplitterSource, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'LAV Splitter (internal)');

             LAVSplitterSettings := GetLavSplitterSettingsInterface;
             LAVSplitterSettings.SetRuntimeConfig(true);
             LAVSplitterSettings.SetTrayIcon(false);
             pLanguages := MainForm.ConfigObject.LavPreferredLanguages;
             LAVSplitterSettings.SetPreferredLanguages(PWideChar(pLanguages));

             (SplitterFilter as IFileSourceFilter).Load(@Filename[1],nil);
            end;
         end;

         RendererSelectedOutputAudioPin(SplitterFilter,MainForm.CurrentProject.SelectedAudioTrackOnExtract);
         RendererAllOutputVideoPin(SplitterFilter);

      end;

   end
  else
   begin
    FLastResult := FGraphBuilder.RenderFile(@filename[1], nil);
   end;

  // ------- substitute Video renderer with Vmr9
  if MainForm.ConfigObject.UseInternalFilters AND MainForm.ConfigObject.PreferVmr9VideoRenderer then
   begin
    if not AudioOnly then
     begin
      if Assigned(VSFilter) then
       begin
        StandardRenderer := FindFilterByCLSID(FGraphBuilder, CLSID_VideoRenderer);
        if not Assigned(StandardRenderer) then
        begin
         StandardRenderer := FindFilterByCLSID(FGraphBuilder, CLSID_EnhRenderer);
        end;
        if Assigned(StandardRenderer) then
         begin
          GetFilterPin(VSFilter,PINDIR_OUTPUT,PinOutVsFilter);
          GetFilterPin(VSFilter,PINDIR_INPUT,PinInVsFilter);
          GetFilterPin(StandardRenderer,PINDIR_INPUT,PinInStandardRenderer);
          if Assigned(PinOutVsFilter) then
           begin
            FLastResult := CoCreateInstance(CLSID_Vmr9Renderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, Renderer);
            if Succeeded(FLastResult) then
            begin
              FLastResult := FGraphBuilder.AddFilter(Renderer, 'Video Mixing Renderer 9');
              FGraphBuilder.Disconnect(PinOutVsFilter);
              FGraphBuilder.Disconnect(PinInStandardRenderer);
              GetFilterPin(Renderer,PINDIR_INPUT,PinInRenderer);
              FGraphBuilder.Connect(PinOutVsFilter,PinInRenderer);
            end;
           end;
         end;
       end;
     end;
   end;
  // -------------------------------------------

  // ------- substitute Video renderer with Vmr7
  if MainForm.ConfigObject.UseInternalFilters AND MainForm.ConfigObject.PreferVmr7VideoRenderer then
   begin
    if not AudioOnly then
     begin
      if Assigned(VSFilter) then
       begin
        StandardRenderer := FindFilterByCLSID(FGraphBuilder, CLSID_VideoRenderer);
        if not Assigned(StandardRenderer) then
        begin
         StandardRenderer := FindFilterByCLSID(FGraphBuilder, CLSID_EnhRenderer);
        end;
        if Assigned(StandardRenderer) then
         begin
          GetFilterPin(VSFilter,PINDIR_OUTPUT,PinOutVsFilter);
          GetFilterPin(VSFilter,PINDIR_INPUT,PinInVsFilter);
          GetFilterPin(StandardRenderer,PINDIR_INPUT,PinInStandardRenderer);
          if Assigned(PinOutVsFilter) then
           begin
            FLastResult := CoCreateInstance(CLSID_Vmr7Renderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, Renderer);
            if Succeeded(FLastResult) then
            begin
              FLastResult := FGraphBuilder.AddFilter(Renderer, 'Video Mixing Filter 7');
              FGraphBuilder.Disconnect(PinOutVsFilter);
              FGraphBuilder.Disconnect(PinInStandardRenderer);
              GetFilterPin(Renderer,PINDIR_INPUT,PinInRenderer);
              FGraphBuilder.Connect(PinOutVsFilter,PinInRenderer);
            end;
           end;
         end;
       end;
     end;
   end;
  // -------------------------------------------

  // ------- remove unused filters
  if Assigned(VideoDecoderFilter) then
   begin
    GetFilterPin(VideoDecoderFilter,PINDIR_OUTPUT,PinOut);
    Pinout.ConnectedTo(PinIn);
    if not Assigned(PinIn) then
     FGraphBuilder.RemoveFilter(VideoDecoderFilter);
   end;
  if Assigned(AudioDecoderFilter) then
   begin
    GetFilterPin(AudioDecoderFilter,PINDIR_OUTPUT,PinOut);
    Pinout.ConnectedTo(PinIn);
    if not Assigned(PinIn) then
     FGraphBuilder.RemoveFilter(AudioDecoderFilter);
   end;
  if Assigned(Reclock) then
   begin
    GetFilterPin(Reclock,PINDIR_INPUT,PinIn);
    PinIn.ConnectedTo(PinOut);
    if not Assigned(PinOut) then
     FGraphBuilder.RemoveFilter(Reclock);
   end;
  if Assigned(StandardRenderer) then
   begin
    GetFilterPin(StandardRenderer,PINDIR_INPUT,PinIn);
    PinIn.ConnectedTo(PinOut);
    if not Assigned(PinOut) then
     FGraphBuilder.RemoveFilter(StandardRenderer);
   end;
  // ----------------------------

  Result := (FLastResult = S_OK);
  FIsOpen := Result;

  FGraphBuilder.QueryInterface(IID_IBasicVideo2, BasicVideoI);
  BasicVideoI.get_VideoWidth(FVideoWidth);
  BasicVideoI.get_VideoHeight(FVideoHeight);
  BasicVideoI := nil;

  FGraphBuilder.QueryInterface(IID_IBasicAudio, FBasicAudio);

  VSFilter := GetDirectVobSubFilter;
  if Assigned(VSFilter) then
  begin
    FLastResult := VSFilter.QueryVendorInfo(VSFilterVendorInfo);
    if Succeeded(FLastResult) then
    begin
      FCustomVSFilterIntallOk := Pos('VSS Custom VSFilter', VSFilterVendorInfo) = 1;
      CoTaskMemFree(VSFilterVendorInfo);
    end;
  end;

  AddToRot(FGraphBuilder, FROTID);

  SetRate(100);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.IsOpen : Boolean;
begin
  Result := FIsOpen;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.PlayRange(Start, Stop : Cardinal; Loop : Boolean) : Boolean;
var StopDummy : Int64;
begin
  // First stop the graph in case it's running
  FMediaControl.Stop;
  if Assigned(FWaitThread) then
  begin
    FWaitThread.SignalTermination;
    FWaitThread.WaitFor;
    FWaitThread.Free;
  end;
  Result := False;
  FLoop := Loop;
  FStart := Int64(Start) * 10000;
  FStop := Int64(Stop) * 10000;
  StopDummy := 0;

  // We can't use the stop position
  // it cause problems with AVI (stop before the stop point :p),
  // and matroska splitter doesn't support stop

  FLastResult := FMediaSeeking.SetPositions(FStart,
    AM_SEEKING_AbsolutePositioning, StopDummy, AM_SEEKING_NoPositioning);
  if (FLastResult <> S_OK) then
    Exit;

  FWaitThread := TWaitCompletionThread.Create(Self);
  FLastResult := FMediaControl.Run;
  Result := (FLastResult = S_OK);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Pause;
var FilterState : TFilterState;
begin
  FilterState := State_Stopped;
  FMediaControl.GetState(1000, FilterState);
  if FilterState = State_Running then
  begin
    FMediaControl.Pause;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Resume;
var FilterState : TFilterState;
begin
  FilterState := State_Stopped; 
  FMediaControl.GetState(1000,FilterState);
  if FilterState = State_Paused then
  begin
    FMediaControl.Run;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.UpdatePlayRange(Start, Stop : Cardinal);
begin
  EnterCriticalSection(FStartStopAccessCS);
  FStart := Int64(Start) * 10000;
  FStop := Int64(Stop) * 10000;
  LeaveCriticalSection(FStartStopAccessCS);  
end;

//------------------------------------------------------------------------------

function TDShowRenderer.FGetStart : Int64;
begin
  EnterCriticalSection(FStartStopAccessCS);
  Result := FStart;
  LeaveCriticalSection(FStartStopAccessCS);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.FGetStop : Int64;
begin
  EnterCriticalSection(FStartStopAccessCS);
  Result := FStop;
  LeaveCriticalSection(FStartStopAccessCS);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Stop(CallOnStopPlaying : Boolean);
begin
  if not Assigned(FMediaControl) then
    Exit;

  FMediaControl.Stop;
  if Assigned(FWaitThread) then
  begin
    FWaitThread.SignalTermination;
    FWaitThread.WaitFor;
    FWaitThread.Free;
    FWaitThread := nil;
  end;
  if CallOnStopPlaying and Assigned(FOnStopPlaying) then
    FOnStopPlaying(Self);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetPosition : Cardinal;
var CurrentTime : Int64;
begin
  FMediaSeeking.GetCurrentPosition(CurrentTime);
  if (CurrentTime < 0) then
  begin
    OutputDebugString('WARN in TDShowRenderer.GetPosition : CurrentTime < 0');
  end;
  Result := CurrentTime div 10000;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetDuration : Cardinal;
var Duration : Int64;
begin
  FMediaSeeking.GetDuration(Duration);
  Result := Duration div 10000;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.IsPlaying : Boolean;
var State : TFilterState;
begin
  Result := False;
  if Assigned(FMediaControl) then
  begin
    FMediaControl.GetState(0,State);
    Result := (State = State_Running);
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.IsPaused : Boolean;
var State : TFilterState;
begin
  Result := False;
  if Assigned(FMediaControl) then
  begin
    FMediaControl.GetState(0, State);
    Result := (State = State_Paused);
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Replay;
begin
  PlayRange(FStart, FStop, FLoop);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.DisplayWindowProc(var Mesg : TMessage);
var xPos, yPos : Short;
    CursorPos : TPoint;
begin
  with Mesg do
  begin
    case Msg of
    WM_SIZE: UpdateDisplayWindow;
    WM_RBUTTONUP:
      // Translate coordinate
      begin
        if (GetCursorPos(CursorPos) = True) and (ScreenToClient(FDisplayWindow, CursorPos) = True) then
        begin
          // horizontal position of cursor : LOWORD(lParam)
          // vertical position of cursor : HIWORD(lParam)
          xPos := CursorPos.X;
          yPos := CursorPos.Y;
          lParam := MakeLParam(xPos, yPos);
        end;
      end;
    end;
    
    Result := CallWindowProc(FDisplayWindowOldProc, FDisplayWindow, Msg,
      WParam, LParam);
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetDisplayWindow(WinHwnd : THandle);
var State : TFilterState;
begin
  if (FDisplayWindow = WinHwnd) then
    Exit;

  FMediaControl.GetState(0, State);
  if (State = State_Running) or (State = State_Paused) then
    FMediaControl.Stop;

  if Assigned(FVideoWindow) then
  begin
    FVideoWindow.put_Visible(False);
    FVideoWindow.put_MessageDrain(0);

    // This steal the focus, so we do it only before releasing the graph
    if WinHwnd = 0 then
      FVideoWindow.put_Owner(0);
    FVideoWindow := nil;
  end;

  // Unsubclass old window
  if (FDisplayWindowOldProc <> nil) then
  begin
    SetWindowLong(FDisplayWindow, GWL_WNDPROC, LongInt(FDisplayWindowOldProc));
    FreeObjectInstance(FDisplayWindowProc);
    FDisplayWindowProc := nil;
    FDisplayWindowOldProc := nil;
  end;

  FDisplayWindow := WinHwnd;
  if (WinHwnd <> 0) then
  begin
    FLastResult := FGraphBuilder.QueryInterface(IID_IVideoWindow, FVideoWindow);
    if Succeeded(FLastResult) and Assigned(FVideoWindow) then
    begin
      // Subclass new window to handle resize
      FDisplayWindowProc := MakeObjectInstance(DisplayWindowProc);
      FDisplayWindowOldProc := Pointer(GetWindowLong(FDisplayWindow, GWL_WNDPROC));
      SetWindowLong(FDisplayWindow, GWL_WNDPROC, LongInt(FDisplayWindowProc));

      FVideoWindow.put_Owner(FDisplayWindow);
      //FVideoWindow.put_AutoShow(False);
      FVideoWindow.put_MessageDrain(FDisplayWindow);
      FVideoWindow.put_WindowStyle(WS_CHILD + WS_CLIPSIBLINGS + WS_CLIPCHILDREN);
      //FVideoWindow.put_Visible(True);

      UpdateDisplayWindow;
    end;

    if (State = State_Running) then
      FMediaControl.Run
    else if(State = State_Paused) then
      FMediaControl.Pause;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.UpdateDisplayWindow;
var Rect : TRect;
    WinWidth, WinHeight : Integer;
    NewWidth, NewHeight : Integer;
begin
  if (FDisplayWindow = 0) or (not Assigned(FVideoWindow)) or
     (FVideoWidth = 0) or (FVideoHeight = 0) then
    Exit;

  if (GetWindowRect(FDisplayWindow, Rect) = False) then
    Exit;
  WinWidth := Rect.Right - Rect.Left;
  WinHeight := Rect.Bottom - Rect.Top;

  if (FVideoWidth / FVideoHeight) > (WinWidth / WinHeight) then
  begin
    NewHeight := (WinWidth * FVideoHeight) div FVideoWidth;
    NewWidth := WinWidth;
  end
  else
  begin
    NewWidth := (FVideoWidth * WinHeight) div FVideoHeight;
    NewHeight := WinHeight;
  end;

  FVideoTop := (WinHeight - NewHeight) div 2;
  FVideoLeft := (WinWidth - NewWidth) div 2;
  FVideoWindow.SetWindowPosition(
    FVideoLeft,
    FVideoTop,
    NewWidth,
    NewHeight);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.KillVideo;
var FiltersEnum : IEnumFilters;
    Filter, Splitter : IBaseFilter;
    ul : ULONG;
    PinsEnum : IEnumPins;
    Pin, VideoPin : IPin;
    PinDir: TPinDirection;
    HasVideo, HasAudio : Boolean;
    MT : TAMMediaType;
begin
  // We search for a filter that has a video output and an audio output (that
  // should be the splitter ;-)
  FGraphBuilder.EnumFilters(FiltersEnum);
  while FiltersEnum.Next(1,Filter,@ul) = S_OK do
  begin
    HasVideo := False;
    HasAudio := False;
    Filter.EnumPins(PinsEnum);
    while PinsEnum.Next(1,Pin,@ul) = S_OK do
    begin
      Pin.QueryDirection(PinDir);
      if (PinDir = PINDIR_OUTPUT) then
      begin
        if Pin.ConnectionMediaType(MT) = S_OK then
        begin
          if IsEqualGUID(MT.majortype, MEDIATYPE_Video) then
          begin
            HasVideo := True;
            VideoPin := nil;
            VideoPin := Pin;
          end
          else if IsEqualGUID(MT.majortype, MEDIATYPE_Audio) then
          begin
            HasAudio := True;
          end;
          if HasAudio and HasVideo then
            Break;
        end;
      end;
      Pin := nil;
    end;
    if HasAudio and HasVideo then
    begin
      Splitter := Filter;
      Break;
    end;
    Filter := nil;
  end;
  Pin := nil;
  PinsEnum := nil;
  FiltersEnum := nil;
  Filter := nil;

  if Assigned(VideoPin) then
  begin
    NukeDownstream(FGraphBuilder,VideoPin);
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetRate(Rate : Integer);
begin
  if Assigned(FMediaSeeking) then
    FMediaSeeking.SetRate(Rate / 100);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetVolume(Vol : Integer);
var NewVolume : Integer;
begin
  if Assigned(FBasicAudio) then
  begin
    if (Vol <= 0) then
    begin
      FBasicAudio.put_Volume(-10000);
    end
    else
    begin
      NewVolume := Round(1000*Log2(Vol / 100.0));
      FBasicAudio.put_Volume(NewVolume);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.ShowImageAt(TimeMs : Cardinal);
var SeekToTime100NS, StopDummy : Int64;
begin
  if Assigned(FMediaSeeking) then
  begin
    FLastResult := FMediaControl.Stop;
    if (FLastResult <> S_OK) then
      Exit;
    SeekToTime100NS := Int64(TimeMs) * 10000;
    StopDummy := 0;

    FLastResult := FMediaSeeking.SetPositions(SeekToTime100NS,
      AM_SEEKING_AbsolutePositioning,
      StopDummy,
      AM_SEEKING_NoPositioning);
    if (FLastResult <> S_OK) then
      Exit;

    FMediaControl.StopWhenReady;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Close;
begin
  if (FROTID <> 0) then
  begin
    RemoveFromRot(FROTID);
    FROTID := 0;
  end;
  FVideoWidth := 0;
  FVideoHeight := 0;
  FIsOpen := False;
  if Assigned(FMediaControl) then FMediaControl.Stop;
  SetDisplayWindow(0);
  if Assigned(FBasicAudio) then FBasicAudio := nil;
  if Assigned(FMediaControl) then FMediaControl := nil;
  if Assigned(FMediaEventEx) then FMediaEventEx := nil;
  if Assigned(FMediaSeeking) then FMediaSeeking := nil;
  if Assigned(FGraphBuilder) then FGraphBuilder := nil;
  if (FHCustomVSFilterInst <> 0) then
  begin
    CoFreeLibrary(FHCustomVSFilterInst);
    FHCustomVSFilterInst := 0;
  end;

end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetDirectVobSubFilter : IBaseFilter;
begin
  Result := nil;
  if not Assigned(FGraphBuilder) then
    Exit;
  Result := FindFilterByCLSID(FGraphBuilder,
    CLSID_DirectVobSubFilter);
  if not Assigned(Result) then
  begin
    Result := FindFilterByCLSID(FGraphBuilder, CLSID_DirectVobSubFilterAuto);
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetDirectVobSubInterface : IDirectVobSub;
var DirectVobSubFilter : IBaseFilter;
begin
  Result := nil;
  DirectVobSubFilter := GetDirectVobSubFilter;
  if not Assigned(DirectVobSubFilter) then
    Exit;
  DirectVobSubFilter.QueryInterface(IID_IIDirectVobSub, Result);
  if not Assigned(Result) then
  begin
    DirectVobSubFilter := nil;
    Exit;
  end;
  DirectVobSubFilter := nil;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetDirectLavVideoSettingsFilter : IBaseFilter;
begin
  Result := nil;
  if not Assigned(FGraphBuilder) then
    Exit;
  Result := FindFilterByCLSID(FGraphBuilder,
    CLSID_LavVideoDecoder);
  if not Assigned(Result) then
  begin
    Result := FindFilterByCLSID(FGraphBuilder, CLSID_LavVideoDecoder);
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetLavVideoSettingsInterface : ILAVVideoSettings;
 var LavVideoSettingsFilter : IBaseFilter;
begin
  Result := nil;
  LavVideoSettingsFilter := GetDirectLavVideoSettingsFilter;
  if not Assigned(LavVideoSettingsFilter) then
    Exit;
  LavVideoSettingsFilter.QueryInterface(IID_ILAVVideoSettings, Result);
  if not Assigned(Result) then
  begin
    LavVideoSettingsFilter := nil;
    Exit;
  end;
  LavVideoSettingsFilter := nil;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetDirectLavAudioSettingsFilter : IBaseFilter;
begin
  Result := nil;
  if not Assigned(FGraphBuilder) then
    Exit;
  Result := FindFilterByCLSID(FGraphBuilder,
    CLSID_LavAudioDecoder);
  if not Assigned(Result) then
  begin
    Result := FindFilterByCLSID(FGraphBuilder, CLSID_LavAudioDecoder);
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetLavAudioSettingsInterface : ILAVAudioSettings;
 var LavAudioSettingsFilter : IBaseFilter;
begin
  Result := nil;
  LavAudioSettingsFilter := GetDirectLavAudioSettingsFilter;
  if not Assigned(LavAudioSettingsFilter) then
    Exit;
  LavAudioSettingsFilter.QueryInterface(IID_ILAVAudioSettings, Result);
  if not Assigned(Result) then
  begin
    LavAudioSettingsFilter := nil;
    Exit;
  end;
  LavAudioSettingsFilter := nil;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetDirectLavSplitterSettingsFilter : IBaseFilter;
begin
  Result := nil;
  if not Assigned(FGraphBuilder) then
    Exit;
  Result := FindFilterByCLSID(FGraphBuilder,
    CLSID_LavSplitterDecoder);
  if not Assigned(Result) then
  begin
    Result := FindFilterByCLSID(FGraphBuilder, CLSID_LavSplitterDecoder);
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetLavSplitterSettingsInterface : ILAVFSettings;
 var LavSplitterSettingsFilter : IBaseFilter;
begin
  Result := nil;
  LavSplitterSettingsFilter := GetDirectLavSplitterSettingsFilter;
  if not Assigned(LavSplitterSettingsFilter) then
    Exit;
  LavSplitterSettingsFilter.QueryInterface(IID_ILAVFSettings, Result);
  if not Assigned(Result) then
  begin
    LavSplitterSettingsFilter := nil;
    Exit;
  end;
  LavSplitterSettingsFilter := nil;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetSubtitleFilename(Filename : WideString);
var DirectVobSub :IDirectVobSub;
begin
  DirectVobSub := GetDirectVobSubInterface;
  if Assigned(DirectVobSub) then
  begin
    if FCustomVSFilterIntallOk then
     begin
      DirectVobSub.put_PreBuffering(FALSE);
      DirectVobSub.put_SubtitleReloader(TRUE);
      DirectVobSub.put_LoadSettings(2, FALSE, FALSE, FALSE);
      DirectVobSub.put_FileName(PWideChar(Filename));
     end;
     DirectVobSub := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetSubtitleUpscalingSettings;
var DirectVobSub :IDirectVobSub;
begin
  DirectVobSub := GetDirectVobSubInterface;
  if Assigned(DirectVobSub) then
  begin
    begin
      if MainForm.ConfigObject.DoubledSubResolution AND
         (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
         (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
       DirectVobSub.put_ExtendPicture(0,0,1,384,288)
      else DirectVobSub.put_ExtendPicture(0,0,0,384,288);
    end;
    DirectVobSub := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetSubtitleFlipSettings;
var DirectVobSub :IDirectVobSub;
begin
  DirectVobSub := GetDirectVobSubInterface;
  if Assigned(DirectVobSub) then
  begin
    begin
      DirectVobSub.put_Flip(MainForm.ConfigObject.FlippedPicture,
        MainForm.ConfigObject.flippedSubtitles);
    end;
    DirectVobSub := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetSubtitleFont(fontDescription : WideString; Transparency : Integer; Outline : Integer; OutLineColor : TColor; TransparencyOutline : Integer; Shadow : Integer; ShadowColor : TColor; TransparencyShadow : Integer);
var DirectVobSub :IDirectVobSub;
  lf: LOGFONT; lflen: integer; Color: COLORREF; fShadow: BOOL; fOutline: BOOL; fAdvancedRenderer: BOOL;
  DescriptionName : TFontName; DescriptionSize : Integer; DescriptionStyle : TFontStyles; DescriptionCharset : TFontCharset; DescriptionColor : TColor;
begin
  DirectVobSub := GetDirectVobSubInterface;
  if Assigned(DirectVobSub) then
  begin
    begin
      DirectVobSub.get_TextSettings(lf, SizeOf(lf), Color, fShadow, fOutline, fAdvancedRenderer);

      String2FontProperties(fontDescription, DescriptionName, DescriptionSize, DescriptionStyle, DescriptionCharset, DescriptionColor);
      lf.lfHeight := DescriptionSize;
      lf.lfWeight := 400;
      if fsBold in DescriptionStyle then lf.lfWeight := 700;
      if fsItalic in DescriptionStyle then lf.lfItalic := 1;
      if fsUnderline in DescriptionStyle then lf.lfUnderline := 1;
      if fsStrikeout in DescriptionStyle then lf.lfStrikeOut := 1;
      Color := DescriptionColor;
      StrPLCopy(lf.lfFaceName,DescriptionName,SizeOf(lf.lfFaceName));
      DirectVobSub.put_TextSettings(lf, SizeOf(lf), Color, fShadow, fOutline, fAdvancedRenderer);
      DirectVobSub.put_TextSettings2(lf, SizeOf(lf), Color, 0, OutLineColor, ShadowColor, Transparency, 0, TransparencyOutline, TransparencyShadow, Shadow, Shadow, Outline, Outline, 0);
    end;
    DirectVobSub := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.HideSubtitle(Hide : boolean);
var DirectVobSub :IDirectVobSub;
begin
  DirectVobSub := GetDirectVobSubInterface;
  if Assigned(DirectVobSub) then
  begin
    if FCustomVSFilterIntallOk then
     begin
      DirectVobSub.put_HideSubtitles(Hide);
     end;
    DirectVobSub := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.UpdateImage;
var SeekToTime100NS, StopDummy : Int64;
begin
  if Assigned(FMediaSeeking) then
  begin
    FMediaSeeking.GetCurrentPosition(SeekToTime100NS);
    StopDummy := 0;

    FLastResult := FMediaSeeking.SetPositions(SeekToTime100NS,
      AM_SEEKING_AbsolutePositioning,
      StopDummy,
      AM_SEEKING_NoPositioning);
    if (FLastResult <> S_OK) then
      Exit;
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.CopyState(Renderer : TRenderer);
var CurrentPosition, StopDummy : Int64;
    DShowRenderer : TDShowRenderer;
begin
  DShowRenderer := Renderer as TDShowRenderer;
  if Assigned(DShowRenderer) and Assigned(FMediaSeeking) and Assigned(DShowRenderer.FMediaSeeking) then
  begin
    DShowRenderer.FMediaSeeking.GetCurrentPosition(CurrentPosition);
    FMediaSeeking.SetPositions(CurrentPosition,
      AM_SEEKING_AbsolutePositioning,
      StopDummy,
      AM_SEEKING_NoPositioning);
    FLoop := DShowRenderer.FLoop;
    FStart := DShowRenderer.FStart;
    FStop := DShowRenderer.FStop;
    if DShowRenderer.IsPlaying or DShowRenderer.IsPaused then
    begin
      // Renderer will be in paused state after copy state
      if Assigned(FWaitThread) then
      begin
        FWaitThread.SignalTermination;
        FWaitThread.WaitFor;
        FWaitThread.Free;
      end;
      FWaitThread := TWaitCompletionThread.Create(Self);
      FLastResult := FMediaControl.Pause;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetVSFilterFPS : Double;
var DirectVobSub :IDirectVobSub;
    fpsEnabled : LongBool;
    fps : Double;
begin
  fps := -1.0;
  fpsEnabled := False;
  DirectVobSub := GetDirectVobSubInterface;
  if Assigned(DirectVobSub) then
  begin
    DirectVobSub.get_MediaFPS(fpsEnabled, fps);
  end;
  Result := fps;
end;

//==============================================================================

constructor TWaitCompletionThread.Create(Renderer : TDShowRenderer);
begin
  FRenderer := Renderer;
  hTerminateEvent := CreateEvent(nil,False,False,nil);
  inherited Create(False);
end;

//------------------------------------------------------------------------------

destructor TWaitCompletionThread.Destroy;
begin
  CloseHandle(hTerminateEvent);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TWaitCompletionThread.SignalTermination;
begin
  Terminate;
  SetEvent(hTerminateEvent);
end;

//------------------------------------------------------------------------------

procedure TWaitCompletionThread.Execute;
const WAIT_TIMEOUT_MS : Integer = 1;
const HALF_WAIT_TIMEOUT_100NS : Integer = 5000;
var
  hInQueueEvent : THandle;
  evCode, param1, param2 : Integer;
  bDone : Boolean;
  EventsArray : array[0..1] of THandle;
  WaitResult : Cardinal;
  CurrentPos, StartPos : Int64;
  StopDummy : Int64;
label TWaitCompletionThread_Restart;
begin
  if (FRenderer.FMediaEventEx.GetEventHandle(OAEVENT(hInQueueEvent)) <> S_OK) then
    // Insert failure-handling code here
    Exit;

  EventsArray[0] := hTerminateEvent;
  EventsArray[1] := hInQueueEvent;

TWaitCompletionThread_Restart:

  bDone := False;
  while (not bDone) and (not Self.Terminated) do
  begin
    WaitResult := WaitForMultipleObjects(2, @EventsArray, False, WAIT_TIMEOUT_MS);
    if (WaitResult = WAIT_OBJECT_0+1) then
    begin
      while (FRenderer.FMediaEventEx.GetEvent(evCode, param1, param2, 0) = S_OK) do
      begin
        FRenderer.FMediaEventEx.FreeEventParams(evCode, param1, param2);
        bDone := (evCode = EC_COMPLETE);
      end;
    end;
    // Check position
    FRenderer.FMediaSeeking.GetCurrentPosition(CurrentPos);
    if (CurrentPos >= FRenderer.StopTime - HALF_WAIT_TIMEOUT_100NS) or
       (CurrentPos < FRenderer.StartTime) then
    begin
      bDone := True;
    end;
  end;

  if bDone then
  begin
    if FRenderer.FLoop then
    begin
      StartPos := FRenderer.StartTime;
      FRenderer.FMediaSeeking.SetPositions(StartPos,
        AM_SEEKING_AbsolutePositioning, StopDummy, AM_SEEKING_NoPositioning);
      goto TWaitCompletionThread_Restart;
    end
    else
    begin
      FRenderer.FMediaControl.Stop;
      if Assigned(FRenderer.FOnStopPlaying) then
        FRenderer.FOnStopPlaying(FRenderer);
    end;
  end;
end;

//==============================================================================

constructor TDSWavExtractor.Create;
begin
  FLastResult := S_OK;
  FAudioStreamCount := 0;
  IsPcmAudio := false;
  IsAc3Audio := false;
  IsDtsAudio := false;
  IsMpegAudio := false;
  IsAacAudio := False;
  IsFlacAudio := False;
  IsMatroskaContainer := False;
  FHWavWriterInst := CoLoadLibrary('WavWriter.dll',True);
  if (FHWavWriterInst = 0) then
    MessageBox(0, 'Error while loading WavWriter.dll', 'Error', MB_OK);
end;

//------------------------------------------------------------------------------

destructor TDSWavExtractor.Destroy;
begin
  Close;
  if (FHWavWriterInst <> 0) then
  begin
    //CoFreeLibrary(FHWavWriterInst); // TODO : why it b0rks ?
    //FHWavWriterInst := 0;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetOutputAudioPinCount(Filter : IBaseFilter) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsAudioPin(Pin) then
      Inc(Result);
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.Open(Filename : WideString; AudioTrack : integer) : Boolean;
Const
  CLSID_MPCAudioDecoder: TGuid = '{3D446B6F-71DE-4437-BE15-8CE47174340F}';
  CLSID_MadFlacAudioDecoder: TGuid = '{6B257121-CBB6-46B3-ABFA-B14DFA98C4A6}';
  CLSID_MPCMatroskaSplitter: TGuid = '{149D2E01-C32E-4939-80F6-C07B81015A7A}';
  CLSID_MPCAviSplitter: TGuid = '{9736D831-9D6C-4E72-B6E7-560EF9181001}';
  CLSID_MPCMp4Splitter: TGuid = '{61F47056-E400-43D3-AF1E-AB7DFFD4C4AD}';
  CLSID_MPCFlvSplitter: TGuid = '{47E792CF-0BBE-4F7A-859C-194B0768650A}';
  CLSID_MPCMpegSplitter: TGuid = '{DC257063-045F-4BE2-BD5B-E12279C464F0}';
  CLSID_FileSourceAsync: TGuid = '{E436EBB5-524F-11CE-9F53-0020AF0BA770}';
  CLSID_LavVideoDecoder: TGuid = '{EE30215D-164F-4A92-A4EB-9D4C13390F9F}';
  CLSID_LavAudioDecoder: TGuid = '{E8E73B6B-4CB3-44A4-BE99-4F7BCB96E491}';
  CLSID_LavSplitter: TGuid = '{171252A0-8820-4AFE-9DF8-5C92B2D66B04}';
  CLSID_LavSplitterSource: TGuid = '{B98D13E7-55DB-4385-A33D-09FD1BA26338}';

var
  Filter : IBaseFilter;
  Pin1, Pin2, PinFileSourceOutput : IPin;
  PinSplitterInput : IPin;
  PinAudioDecoderInput, PinAudioDecoderOutput: IPin;
  PinInfo : TPinInfo;
  EnumPins : IEnumPins;
  ul: ULONG;
  FHMPCAudioFilterInst, FHMPCFileSourceAsync, FHMPCMatroskaSplitter, FHMPCAviSplitter, FHMPCMpegSplitter,
  FHMPCMp4Splitter, FHMPCFlvSplitter: THandle;
  FHLAVVideoFilterInst, FHLavAudioFilterInst, FHLavSplitter : THandle;
  FHGenericAudioFilterInst: THandle;
  AudioDecoderFilter, FileSourceAsyncFilter, SplitterFilter: IBaseFilter;
  LAVAudioSettings : ILAVAudioSettings;
  LAVSplitterSettings : ILAVFSettings;
  centerLevel, surroundLevel, LfeLevel : DWORD;
  CodecPath : WideString;
  pLanguages : WideString;
begin

  Result := False;
  if (FHWavWriterInst = 0) then
    Exit;

  if Assigned(FGraphBuilder) then
    Close;

  if AudioTrack = 0 then AudioTrack := 1;

  OutputDebugStringW('TDSWavExtractor.Open: Creating Graph builder');
  FLastResult := CoCreateInstance(TGUID(CLSID_FilterGraph), nil, CLSCTX_INPROC,
    TGUID(IID_IGraphBuilder), FGraphBuilder);
  GetLastErrorString;
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEventEx);
  if (FLastResult <> S_OK) then Exit;

  // INTERNAL CODEC (DEFAULT)

  if MainForm.ConfigObject.UseInternalFilters AND
     (MainForm.ConfigObject.UseDefaultInternalCodec OR MainForm.ConfigObject.UseDefaultCodecOnCreateProject) AND
     not MainForm.ConfigObject.UseAlternativeCodecOnCreateProject AND
     not MainForm.ConfigObject.DoNotUseInternalFiltersOnCreateProject AND
     (IsMpegAudio OR IsAc3Audio OR IsDtsAudio or IsPcmAudio OR IsAacAudio) Then
   begin

    FLastResult := CoCreateInstance(CLSID_MPCAudioDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioDecoderFilter);
    if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
     begin
      FGraphBuilder.AddFilter(AudioDecoderFilter, 'MPA Audio decoder');
     end
    else
     begin
      CodecPath := ExtractFilePath(ParamStr(0))+'Codec\MpaDecFilter.ax';
      FHMPCAudioFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
      if (FHMPCAudioFilterInst <> 0) then
       begin
        FLastResult := CreateFilterFromFile(FHMPCAudioFilterInst,
          CLSID_MPCAudioDecoder, AudioDecoderFilter);
        if Succeeded(FLastResult) then
          FGraphBuilder.AddFilter(AudioDecoderFilter, 'MPA Audio decoder (internal)');
       end;
     end;

     AudioDecoderFilter.FindPin('In',PinAudioDecoderInput);
     AudioDecoderFilter.FindPin('Out',PinAudioDecoderOutput);

   end;

  if MainForm.ConfigObject.UseInternalFilters AND
     (MainForm.ConfigObject.UseDefaultInternalCodec OR MainForm.ConfigObject.UseDefaultCodecOnCreateProject) AND
     not MainForm.ConfigObject.UseAlternativeCodecOnCreateProject AND
     not MainForm.ConfigObject.DoNotUseInternalFiltersOnCreateProject AND
     IsFlacAudio then
   begin

    FLastResult := CoCreateInstance(CLSID_MadFlacAudioDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioDecoderFilter);
    if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
     begin
      FGraphBuilder.AddFilter(AudioDecoderFilter, 'MadFLAC audio decoder');
     end
    else
     begin
      CodecPath := ExtractFilePath(ParamStr(0))+'Codec\madFlac.ax';
      FHMPCAudioFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
      if (FHMPCAudioFilterInst <> 0) then
       begin
        FLastResult := CreateFilterFromFile(FHMPCAudioFilterInst,
          CLSID_MadFlacAudioDecoder, AudioDecoderFilter);
        if Succeeded(FLastResult) then
          FGraphBuilder.AddFilter(AudioDecoderFilter, 'MadFLAC audio decoder (internal)');
       end;
     end;

     AudioDecoderFilter.FindPin('In',PinAudioDecoderInput);
     AudioDecoderFilter.FindPin('Out',PinAudioDecoderOutput);

   end;

  // INTERNAL CODEC (ALTERNATIVE, LAV)

  if MainForm.ConfigObject.UseInternalFilters AND
     (MainForm.ConfigObject.UseAlternativeInternalCodec OR MainForm.ConfigObject.UseAlternativeCodecOnCreateProject) AND
     not MainForm.ConfigObject.UseDefaultCodecOnCreateProject AND
     not MainForm.ConfigObject.DoNotUseInternalFiltersOnCreateProject AND
     (IsMpegAudio OR IsAc3Audio OR IsDtsAudio OR IsPcmAudio OR IsFlacAudio OR IsAacAudio) then
   begin

    FLastResult := CoCreateInstance(CLSID_LavAudioDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioDecoderFilter);
    if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
     begin
      FGraphBuilder.AddFilter(AudioDecoderFilter, 'LAV Audio Decoder');
     end
    else
     begin
      CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVAudio.ax';
      FHLavAudioFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
      if (FHLavAudioFilterInst <> 0) then
       begin
       FLastResult := CreateFilterFromFile(FHLavAudioFilterInst,
         CLSID_LavAudioDecoder, AudioDecoderFilter);
       if Succeeded(FLastResult) then
         FGraphBuilder.AddFilter(AudioDecoderFilter, 'LAV Audio decoder (internal)');
       end;
      end;

    AudioDecoderFilter.FindPin('In',PinAudioDecoderInput);
    AudioDecoderFilter.FindPin('Out',PinAudioDecoderOutput);

    LAVAudioSettings := GetLavAudioSettingsInterface;
    LAVAudioSettings.SetTrayIcon(false);
    LAVAudioSettings.SetRuntimeConfig(true);
    LAVAudioSettings.SetAudioDelay(False,0);
    if MainForm.ConfigObject.EnableLavAudioMixing then
     begin
      LAVAudioSettings.SetMixingEnabled(True);
      LAVAudioSettings.GetMixingLevels(centerLevel, surroundLevel, LfeLevel);
      LAVAudioSettings.SetMixingLevels(centerLevel, surroundLevel, LfeLevel);
      LAVAudioSettings.SetMixingFlags(LAV_MIXING_FLAG_UNTOUCHED_STEREO);
     end;

   end;

  // SPLITTER

  // MPC Matroska Splitter, Avi Splitter, Mp4 Splitter, Flv Splitter
  if MainForm.ConfigObject.UseInternalFilters AND not MainForm.ConfigObject.DoNotUseInternalFiltersOnCreateProject AND
      (IsMatroskaContainer OR IsAviContainer OR IsMp4Container OR IsFlvContainer OR IsMpegContainer) then
   begin

     if IsMatroskaContainer AND
        ( (MainForm.ConfigObject.UseDefaultInternalCodec AND not MainForm.ConfigObject.UseAlternativeCodecOnCreateProject) OR
        MainForm.ConfigObject.UseDefaultCodecOnCreateProject) Then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCMatroskaSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Matroska Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\MatroskaSplitter.ax';
           FHMPCMatroskaSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCMatroskaSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCMatroskaSplitter,
               CLSID_MPCMatroskaSplitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Matroska Splitter (internal)');
            end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          if GetOutputAudioPinCount(SplitterFilter)>0 then
           RendererSelectedOutputAudioPin(SplitterFilter,AudioTrack);

        end
       else
        begin
          OutputDebugStringW(PWideChar('TDSWavExtractor.Open: Adding source filter ' + Filename));
          FLastResult := FGraphBuilder.AddSourceFilter(@Filename[1],nil,Filter);
        end;
      end;

     if IsAviContainer AND
        ( (MainForm.ConfigObject.UseDefaultInternalCodec AND not MainForm.ConfigObject.UseAlternativeCodecOnCreateProject) OR
        MainForm.ConfigObject.UseDefaultCodecOnCreateProject) Then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCAviSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Avi Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\AviSplitter.ax';
           FHMPCAviSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCAviSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCAviSplitter,
               CLSID_MPCAviSplitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Avi Splitter (internal)');
            end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          if GetOutputAudioPinCount(SplitterFilter)>0 then
           RendererSelectedOutputAudioPin(SplitterFilter,AudioTrack);

        end
       else
        begin
          OutputDebugStringW(PWideChar('TDSWavExtractor.Open: Adding source filter ' + Filename));
          FLastResult := FGraphBuilder.AddSourceFilter(@Filename[1],nil,Filter);
        end;
      end;

     if IsMp4Container AND
        ( (MainForm.ConfigObject.UseDefaultInternalCodec AND not MainForm.ConfigObject.UseAlternativeCodecOnCreateProject) OR
        MainForm.ConfigObject.UseDefaultCodecOnCreateProject) Then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCMp4Splitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mp4 Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\Mp4Splitter.ax';
           FHMPCMp4Splitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCMp4Splitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCMp4Splitter,
               CLSID_MPCMp4Splitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mp4 Splitter (internal)');
            end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          if GetOutputAudioPinCount(SplitterFilter)>0 then
           RendererSelectedOutputAudioPin(SplitterFilter,AudioTrack);

        end
       else
        begin
          OutputDebugStringW(PWideChar('TDSWavExtractor.Open: Adding source filter ' + Filename));
          FLastResult := FGraphBuilder.AddSourceFilter(@Filename[1],nil,Filter);
        end;
      end;

     if IsFlvContainer AND
        ( (MainForm.ConfigObject.UseDefaultInternalCodec AND not MainForm.ConfigObject.UseAlternativeCodecOnCreateProject) OR
        MainForm.ConfigObject.UseDefaultCodecOnCreateProject) Then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCFlvSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Flv Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\FlvSplitter.ax';
           FHMPCFlvSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCFlvSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCFlvSplitter,
               CLSID_MPCFlvSplitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Flv Splitter (internal)');
            end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          if GetOutputAudioPinCount(SplitterFilter)>0 then
           RendererSelectedOutputAudioPin(SplitterFilter,AudioTrack);

        end
       else
        begin
          OutputDebugStringW(PWideChar('TDSWavExtractor.Open: Adding source filter ' + Filename));
          FLastResult := FGraphBuilder.AddSourceFilter(@Filename[1],nil,Filter);
        end;
      end;

     if IsMpegContainer AND
        ( (MainForm.ConfigObject.UseDefaultInternalCodec AND not MainForm.ConfigObject.UseAlternativeCodecOnCreateProject) OR
        MainForm.ConfigObject.UseDefaultCodecOnCreateProject) Then
      begin
       FLastResult := CoCreateInstance(CLSID_FileSourceAsync, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FileSourceAsyncFilter);
       if Succeeded(FLastResult) then
        begin
         FLastResult := FGraphBuilder.AddFilter(FileSourceAsyncFilter,'File Source (Async.) (internal)');
         (FileSourceAsyncFilter as IFileSourceFilter).Load(@Filename[1],nil);
         FileSourceAsyncFilter.FindPin('Output',PinFileSourceOutput);

         FLastResult := CoCreateInstance(CLSID_MPCMpegSplitter, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
         if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
          begin
           FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mpeg Splitter');
          end
         else
          begin
           CodecPath := ExtractFilePath(ParamStr(0))+'Codec\MpegSplitter.ax';
           FHMPCMpegSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
           if (FHMPCMpegSplitter <> 0) then
            begin
             FLastResult := CreateFilterFromFile(FHMPCMpegSplitter,
               CLSID_MPCMpegSplitter, SplitterFilter);
             FGraphBuilder.AddFilter(SplitterFilter, 'MPC - Mpeg Splitter (internal)');
            end;
          end;

          SplitterFilter.FindPin('Input', PinSplitterInput);
          PinFileSourceOutput.Connect(PinSplitterInput,nil);
          if GetOutputAudioPinCount(SplitterFilter)>0 then
           RendererSelectedOutputAudioPin(SplitterFilter,AudioTrack);

        end
       else
        begin
          OutputDebugStringW(PWideChar('TDSWavExtractor.Open: Adding source filter ' + Filename));
          FLastResult := FGraphBuilder.AddSourceFilter(@Filename[1],nil,Filter);
        end;
      end;

     if (MainForm.ConfigObject.UseAlternativeInternalCodec AND not MainForm.ConfigObject.UseDefaultCodecOnCreateProject)
        OR MainForm.ConfigObject.UseAlternativeCodecOnCreateProject then
      begin
        FLastResult := CoCreateInstance(CLSID_LavSplitterSource, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
        if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
         begin
          FGraphBuilder.AddFilter(SplitterFilter, 'LAV Splitter');

          LAVSplitterSettings := GetLavSplitterSettingsInterface;
          LAVSplitterSettings.SetRuntimeConfig(true);
          LAVSplitterSettings.SetTrayIcon(false);
          pLanguages := MainForm.ConfigObject.LavPreferredLanguages;
          LAVSplitterSettings.SetPreferredLanguages(PWideChar(pLanguages));

          (SplitterFilter as IFileSourceFilter).Load(@Filename[1],nil);
         end
        else
         begin
          CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVSplitter.ax';
          FHLavSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
          if (FHLavSplitter <> 0) then
           begin
            FLastResult := CreateFilterFromFile(FHLavSplitter,
              CLSID_LavSplitterSource, SplitterFilter);
            FGraphBuilder.AddFilter(SplitterFilter, 'LAV Splitter (internal)');

            LAVSplitterSettings := GetLavSplitterSettingsInterface;
            LAVSplitterSettings.SetRuntimeConfig(true);
            LAVSplitterSettings.SetTrayIcon(false);
            pLanguages := MainForm.ConfigObject.LavPreferredLanguages;
            LAVSplitterSettings.SetPreferredLanguages(PWideChar(pLanguages));

            (SplitterFilter as IFileSourceFilter).Load(@Filename[1],nil);
          end;
         end;

        if GetOutputAudioPinCount(SplitterFilter)>0 then
         RendererSelectedOutputAudioPin(SplitterFilter,AudioTrack);
      end;

   end
  else
   begin
    OutputDebugStringW(PWideChar('TDSWavExtractor.Open: Adding source filter ' + Filename));
    FLastResult := FGraphBuilder.AddSourceFilter(@Filename[1],nil,Filter);
   end;
  // ***************

  if (FLastResult <> S_OK) then Exit;

  // Try to render all pins on the filter
  OutputDebugStringW('TDSWavExtractor.Open: Rendering all source filter pins');
  if SplitterFilter = nil then
   begin
    Filter.EnumPins(EnumPins);
    while (FLastResult = S_OK) and (EnumPins.Next(1,Pin1, @ul) = S_OK) do
    begin
      FLastResult := FGraphBuilder.Render(Pin1);
      Pin1 := nil;
    end;
    EnumPins := nil;
    if Failed(FLastResult) then Exit;
  end;

  // Get the duration
  OutputDebugStringW('TDSWavExtractor.Open: Getting duration');
  FLastResult := FMediaSeeking.GetDuration(FDuration);

  // Check if this is a source/splitter filter
  OutputDebugStringW('TDSWavExtractor.Open: Getting audio output pin count');
  if SplitterFilter = nil then
   FAudioStreamCount := GetOutputAudioPinCount(Filter)
  else FAudioStreamCount := GetOutputAudioPinCount(SplitterFilter);
  if FAudioStreamCount = 0 then
  begin
    OutputDebugStringW('TDSWavExtractor.Open: Getting audio output pin count on next filter');
    // No audio stream available, this is probably the File source filter
    // Next filter is probably a splitter
    FLastResult := GetFilterPin(Filter,PINDIR_OUTPUT,Pin1);
    FLastResult := Pin1.ConnectedTo(Pin2);
    FLastResult := Pin2.QueryPinInfo(PinInfo);
    Filter := nil;
    Filter := PinInfo.pFilter;
    FAudioStreamCount := GetOutputAudioPinCount(Filter);
  end;
  AddToRot(FGraphBuilder, FROTID);
  if SplitterFilter = nil then
   FSplitter := Filter
  else FSplitter := SplitterFilter;
  Result := (FAudioStreamCount > 0);
  Filter := nil;
  Pin1 := nil;
  Pin2 := nil;
  FMediaSeeking := nil;
  FSourceFilename := Filename;
end;

//------------------------------------------------------------------------------

procedure TDSWavExtractor.Close;
begin
  if (FROTID <> 0) then
  begin
    RemoveFromRot(FROTID);
    FROTID := 0;
  end;
  FAudioStreamCount := 0;
  if Assigned(FSplitter) then FSplitter := nil;
  if Assigned(FMediaControl) then
  begin
    FMediaControl.Stop;
    FMediaControl := nil;
  end;
  if Assigned(FMediaEventEx) then FMediaEventEx := nil;
  if Assigned(FMediaSeeking) then FMediaSeeking := nil;
  if Assigned(FGraphBuilder) then FGraphBuilder := nil;
end;

//------------------------------------------------------------------------------

function GetFilterName(Filter : IBaseFilter) : WideString;
var FilterInfo : TFilterInfo;
    Res : HRESULT;
begin
  FilterInfo.achName[0] := #0;
  Res := Filter.QueryFilterInfo(FilterInfo);
  if Succeeded(Res) then
    Result := '<' + WideString(FilterInfo.achName) + '>'
  else
    Result := '<Filter = null>';
end;

function GetPinName(Pin : IPin) : WideString;
var PinInfo : TPinInfo;
    Res : HRESULT;
begin
  PinInfo.achName[0] := #0;
  Res := Pin.QueryPinInfo(PinInfo);
  if Succeeded(Res) then
  begin
    Result := GetFilterName(PinInfo.pFilter) + '.<' + PinInfo.achName + '>';
  end
  else
  begin
    Result := '<Pin = null>';
  end;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.SelectAudioPin(Index : Integer) : Boolean;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
  AudioPinCurrentIndex : Integer;
begin
  assert(FSplitter <> nil);
  Result := False;
  AudioPinCurrentIndex := 0;
  FSplitter.EnumPins(EnumPins);
  OutputDebugStringW(PWideChar('TDSWavExtractor.SelectAudioPin: Enumerating audio output pins on ' + GetFilterName(FSplitter)));
  while (EnumPins.Next(1, Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) then
    begin
      if IsAudioPin(Pin) then
      begin
        if (AudioPinCurrentIndex <> Index) then
         NukeDownstream(FGraphBuilder,Pin)
        else
         begin
           InstallWriter(Pin);
         end;
        Inc(AudioPinCurrentIndex);
      end
      else
        NukeDownstream(FGraphBuilder,Pin);
    end;
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function PinSupportPCMMediaType(Pin : IPin) : Boolean;
var
  EnumMT : IEnumMediaTypes;
  pMT : PAMMediaType;
  ul: ULONG;
const WAVE_FORMAT_IEEE_FLOAT : Integer = 3;
begin
  Result := False;
  Pin.EnumMediaTypes(EnumMT);
  while (EnumMT.Next(1, pMT, @ul) = S_OK) do
  begin
    if IsEqualGUID(pMT.majortype, MEDIATYPE_Audio) and
      (IsEqualGUID(pMT.subtype, MEDIASUBTYPE_PCM) or
      IsEqualGUID(pMT.subtype, MEDIASUBTYPE_IEEE_FLOAT)) and
      IsEqualGUID(pMT.formattype, FORMAT_WaveFormatEx) and
      ((PWaveFormatEx(pMT.pbFormat).wFormatTag = WAVE_FORMAT_PCM) or
       (PWaveFormatEx(pMT.pbFormat).wFormatTag = WAVE_FORMAT_IEEE_FLOAT) or
       (PWaveFormatEx(pMT.pbFormat).wFormatTag = WAVE_FORMAT_EXTENSIBLE))
       then
    begin
      Result := True;
      Break;
    end;
    DeleteMediaType(pMT);
  end;
  EnumMT := nil;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetPCMOutputPin(Filter : IBaseFilter) : IPin;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := nil;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and PinSupportPCMMediaType(Pin) then
    begin
      OutputDebugStringW(PWideChar('TDSWavExtractor.GetPCMOutputPin: Found PCM pin ' + GetPinName(Pin)));
      Result := Pin;
      Break;
    end;
    Pin := nil;
  end;
  Pin := nil;
  EnumPins := nil;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.InstallWriter(Pin : IPin) : Boolean;
var
  PCMPin, PinConnectedTo : IPin;
  Filter, WavWriterFilter : IBaseFilter;
  PinInfo : TPinInfo;
  FileSinkFilter : IFileSinkFilter;
  WavWriterInterface : IWavWriterInterface;
  DestinationFilenamePeak : WideString;
begin
  OutputDebugStringW(PWideChar('TDSWavExtractor.InstallWriter: InstallWriter on ' + GetPinName(Pin)));
  if PinSupportPCMMediaType(Pin) then
  begin
    OutputDebugStringW('TDSWavExtractor.InstallWriter: PCMPin OK');
    PCMPin := Pin;
  end
  else
  begin
    OutputDebugStringW('TDSWavExtractor.InstallWriter: No PCM support on this pin, getting next filter pin.');
    // We need a pin with wformattag which is probably after a decoder :)
    Pin.ConnectedTo(PinConnectedTo);
    if not Assigned(PinConnectedTo) then
    begin
      // TODO : Do something here or add more error checking
      OutputDebugStringW('TDSWavExtractor.InstallWriter: PinConnectedTo is nil');
    end;
    PinConnectedTo.QueryPinInfo(PinInfo);
    Filter := PinInfo.pFilter;
    OutputDebugStringW(PWideChar('TDSWavExtractor.SelectAudioPin: Get PCM output pin on ' + GetFilterName(Filter)));
    PCMPin := GetPCMOutputPin(Filter);
  end;
  PinConnectedTo := nil;
  Filter := nil;
  assert(PCMPin <> nil);
  // TODO : better error handling here
  NukeDownstream(FGraphBuilder, PCMPin);

  OutputDebugStringW('TDSWavExtractor.InstallWriter: Creating and adding WAVWriter to graph');
  // We are ready to connect, the WAV writer filter and the File Writer
  FLastResult := CreateFilterFromFile(FHWavWriterInst,CLSID_WavWriter, WavWriterFilter);
  FLastResult := WavWriterFilter.QueryInterface(IID_IFileSinkFilter, FileSinkFilter);
  FLastResult := FileSinkFilter.SetFileName(@DestinationFilename[1],nil);
  FileSinkFilter := nil;
  FLastResult := FGraphBuilder.AddFilter(WavWriterFilter,'WAV Writer');

  WavWriterFilter.QueryInterface(IID_IWavWriter,WavWriterInterface);
  if WavWriterInterface <> nil then
  begin
    OutputDebugStringW('TDSWavExtractor.InstallWriter: Configuring WAVWriter');
      if (FWAVExtractionType = wetOnlyPeakFile) then
        WavWriterInterface.SetWriteWavFile(0)
      else if (FWAVExtractionType = wetFastConversion) then
        WavWriterInterface.SetFastConversionMode(1);
      WavWriterInterface.SetWritePeakFile(1);
      DestinationFilenamePeak := WideChangeFileExt(DestinationFilename,'.peak');
      WavWriterInterface.SetPeakFileName(@DestinationFilenamePeak[1]);
      WavWriterInterface.SetSamplesPerPeakRatio(100);
      WavWriterInterface := nil;
  end;

  OutputDebugStringW('TDSWavExtractor.InstallWriter: Rendering PCMPin');
  FLastResult := FGraphBuilder.Render(PCMPin);
  if Failed(FLastResult) then
    OutputDebugStringW('TDSWavExtractor.InstallWriter: Rendering PCMPin failed.');

  FLastResult := PCMPin.ConnectedTo(PinConnectedTo);
  if Failed(FLastResult) then
    OutputDebugStringW('TDSWavExtractor.InstallWriter: PCMPin ConnectedTo failed.')
  else
    OutputDebugStringW(PWideChar('TDSWavExtractor.InstallWriter: PCMPin is now connected to ' + GetPinName(PinConnectedTo))) ;

  FLastResult := WavWriterFilter.QueryInterface(IID_IMediaSeeking, FMediaSeeking);

  PCMPin := nil;  
  Filter := nil;
  WavWriterFilter := nil;
  Result := (FLastResult = S_OK);
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetLastErrorString : string;
begin
  SetLength(Result,MAX_ERROR_TEXT_LEN);
  AMGetErrorText(FLastResult,@Result[1],MAX_ERROR_TEXT_LEN);
end;

//------------------------------------------------------------------------------

procedure TDSWavExtractor.Go;
begin
  FMediaControl.Run;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetProgress : Integer;
var Current, Duration : Int64;
begin
  if Assigned(FMediaSeeking) then
  begin
    FMediaSeeking.GetCurrentPosition(Current);
    FMediaSeeking.GetDuration(Duration);
    Result := Round(Current / Duration * 1000);
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.IsFinished : Boolean;
var evCode, param1, param2 : Integer;
begin
  Result := False;
  while (not Result) and (FMediaEventEx.GetEvent(evCode, param1, param2,0) = S_OK) do
  begin
    FMediaEventEx.FreeEventParams(evCode, param1, param2);
    Result := (evCode = EC_COMPLETE);
  end;
end;

// -----------------------------------------------------------------------------

function TDSWavExtractor.GetFilters(list : TStrings) : Boolean;
begin
  Result := GetFiltersList(FGraphBuilder, list);
end;

// -----------------------------------------------------------------------------

function TDSWavExtractor.GetFiltersAsString : string;
var strList : TStringList;
begin
  strList := TStringList.Create;
  GetFiltersList(FGraphBuilder, strList);
  Result := strList.Text;
  FreeAndNil(strList);
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.RendererSelectedOutputAudioPin(Filter : IBaseFilter; Track : Integer) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
  Index : Integer;
begin
  Result := 0; Index := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsAudioPin(Pin) then
     begin
      Index := Index + 1;
      if Index = Track then
       FGraphBuilder.Render(Pin);
     end;
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.RendererAllOutputAudioPin(Filter : IBaseFilter) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsAudioPin(Pin) then
      FGraphBuilder.Render(Pin);
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.RendererAllOutputVideoPin(Filter : IBaseFilter) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsVideoPin(Pin) then
      FGraphBuilder.Render(Pin);
    Pin := nil;
  end;
end;
//------------------------------------------------------------------------------

function TDSWavExtractor.GetDirectLavAudioSettingsFilter : IBaseFilter;
begin
  Result := nil;
  if not Assigned(FGraphBuilder) then
    Exit;
  Result := FindFilterByCLSID(FGraphBuilder,
    CLSID_LavAudioDecoder);
  if not Assigned(Result) then
  begin
    Result := FindFilterByCLSID(FGraphBuilder, CLSID_LavAudioDecoder);
  end;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetLavAudioSettingsInterface : ILAVAudioSettings;
 var LavAudioSettingsFilter : IBaseFilter;
begin
  Result := nil;
  LavAudioSettingsFilter := GetDirectLavAudioSettingsFilter;
  if not Assigned(LavAudioSettingsFilter) then
    Exit;
  LavAudioSettingsFilter.QueryInterface(IID_ILAVAudioSettings, Result);
  if not Assigned(Result) then
  begin
    LavAudioSettingsFilter := nil;
    Exit;
  end;
  LavAudioSettingsFilter := nil;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetDirectLavSplitterSettingsFilter : IBaseFilter;
begin
  Result := nil;
  if not Assigned(FGraphBuilder) then
    Exit;
  Result := FindFilterByCLSID(FGraphBuilder,
    CLSID_LavSplitterDecoder);
  if not Assigned(Result) then
  begin
    Result := FindFilterByCLSID(FGraphBuilder, CLSID_LavSplitterDecoder);
  end;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetLavSplitterSettingsInterface : ILAVFSettings;
 var LavSplitterSettingsFilter : IBaseFilter;
begin
  Result := nil;
  LavSplitterSettingsFilter := GetDirectLavSplitterSettingsFilter;
  if not Assigned(LavSplitterSettingsFilter) then
    Exit;
  LavSplitterSettingsFilter.QueryInterface(IID_ILAVFSettings, Result);
  if not Assigned(Result) then
  begin
    LavSplitterSettingsFilter := nil;
    Exit;
  end;
  LavSplitterSettingsFilter := nil;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.RendererAllOutputPin(Filter : IBaseFilter) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) then
      FGraphBuilder.Render(Pin);
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.RendererSelectedOutputAudioPin(Filter : IBaseFilter; Track : Integer) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
  Index : Integer;
begin
  Result := 0; Index := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsAudioPin(Pin) then
      begin
       Index := Index + 1;
       if Index = Track then
         FGraphBuilder.Render(Pin);
      end;
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.RendererAllOutputAudioPin(Filter : IBaseFilter) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsAudioPin(Pin) then
     FGraphBuilder.Render(Pin);
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.RendererAllOutputVideoPin(Filter : IBaseFilter) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsVideoPin(Pin) then
      FGraphBuilder.Render(Pin);
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetLavHwModeSupported: String;
 var LAVVideoSettings : ILAVVideoSettings;
   VideoDecoderFilter : IBaseFilter;
   codecPath : WideString; FHLAVVideoFilterInst : THandle;
begin

  try
   begin
    FLastResult := CoCreateInstance(TGUID(CLSID_FilterGraph), nil, CLSCTX_INPROC,
      TGUID(IID_IGraphBuilder), FGraphBuilder);
    GetLastErrorString;
    if (FLastResult <> S_OK) then Exit;

    FLastResult := FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
    if (FLastResult <> S_OK) then Exit;
    FLastResult := FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
    if (FLastResult <> S_OK) then Exit;
    FLastResult := FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEventEx);
    if (FLastResult <> S_OK) then Exit;

  	FLastResult := CoCreateInstance(CLSID_LavVideoDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VideoDecoderFilter);
  	if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
  	begin
  	  FGraphBuilder.AddFilter(VideoDecoderFilter, 'LAV Video Decoder');
  	end
  	else
  	begin
  	 CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVVideo.ax';
  	 FHLAVVideoFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
  	 if (FHLAVVideoFilterInst <> 0) then
  	  begin
  	   FLastResult := CreateFilterFromFile(FHLAVVideoFilterInst,
  		 CLSID_LavVideoDecoder, VideoDecoderFilter);
  	   if Succeeded(FLastResult) then
  	   	FGraphBuilder.AddFilter(VideoDecoderFilter, 'LAV Video Decoder (internal)');
  	  end;
  	end;

	  LAVVideoSettings := GetLavVideoSettingsInterface;

  	LAVVideoSettings.SetRuntimeConfig(true);

  	if LAVVideoSettings.CheckHWAccelSupport(HWAccel_QuickSync) = 1 then
     Result := Result + 'Intel Quick Sync;';
    if LAVVideoSettings.CheckHWAccelSupport(HWAccel_CUDA) = 1 then
     Result := Result + 'Cuda;';
    if LAVVideoSettings.CheckHWAccelSupport(HWAccel_DXVA2CopyBack) = 1 then
     Result := Result + 'Dxva2(copy-back);';

   end
  except
   Result := '';
  end;

end;

function TDShowRenderer.OpenLavCodecBasedFilterGraph(filename : WideString) : Boolean;
Const
  CLSID_Reclock: TGuid = '{9DC15360-914C-46B8-B9DF-BFE67FD36C6A}';
  CLSID_VideoRenderer: TGuid = '{70E102B0-5556-11CE-97C0-00AA0055595A}';
  CLSID_AudioRenderer: TGuid = '{79376820-07D0-11CF-A24D-0020AFD79767}';
  CLSID_Vmr7Renderer: TGuid = '{B87BEB7B-8D29-423F-AE4D-6582C10175AC}';
  CLSID_Vmr9Renderer: TGuid = '{51B4ABF3-748F-4E3B-A276-C828330E926A}';
  CLSID_MadVrRenderer: TGuid = '{E1A8B82A-32CE-4B0D-BE0D-AA68C772E423}';
  CLSID_EnhRenderer: TGuid = '{FA10746C-9B63-4B6C-BC49-FC300EA5F256}';
  CLSID_LavAudioDecoder: TGuid = '{E8E73B6B-4CB3-44A4-BE99-4F7BCB96E491}';
  CLSID_LavSplitter: TGuid = '{171252A0-8820-4AFE-9DF8-5C92B2D66B04}';
  CLSID_LavSplitterSource: TGuid = '{B98D13E7-55DB-4385-A33D-09FD1BA26338}';

var
  BasicVideoI : IBasicVideo2;
  VSFilter : IBaseFilter;
  VSFilterVendorInfo : PWideChar;
  FHLAVVideoFilterInst, FHLavAudioFilterInst, FHLavSplitter : THandle;
  VideoDecoderFilter, AudioDecoderFilter, FileSourceAsyncFilter, SourceAsyncFilter,
  SplitterFilter,StandardRenderer,Renderer : IBaseFilter;
  PinOut,PinIn : IPin;
  PinInVideo,PinOutVideo,PinInVideoConnectedTo, PinOutVideoConnectedTo : IPin;
  PinInVsFilter,PinOutVsFilter,PinOutSubtitleSource,PinInVsFilterConnectedTo,PinOutVsFilterConnectedTo : IPin;
  PinInRenderer,PinInRendererConnectedTo : IPin;
  PinInStandardRenderer,PinInStandardRendererConnectedTo : IPin;
  LAVVideoSettings : ILAVVideoSettings;
  LAVSplitterSettings : ILAVFSettings;
  Pins : IEnumPins; ul : ULONG;
  PinFileSourceOutput : IPin;
  PinSplitterInput : IPin;
  LAVAudioSettings : ILAVAudioSettings;
  centerLevel, surroundLevel, LfeLevel : DWORD;
  CodecPath : WideString;
  pLanguages : WideString;
  PinOutLavSplitterVideo, PinOutLavSplitterAudio,
  PinInLavCodecVideo, PinInLavCodecAudio,
  PinOutLavCodecVideo, PinOutLavCodecAudio,
  PinInVideoRenderer, PinInAudioRenderer, PinInReclockRenderer : IPin;
  AudioRenderer, VideoRenderer, ReclockRenderer : IBaseFilter;
begin

  if Assigned(FGraphBuilder) then
    Close;
  Result := False;
  if (filename = '') or (not WideFileExists(filename)) then
    Exit;

  FLastResult := CoCreateInstance(TGUID(CLSID_FilterGraph), nil, CLSCTX_INPROC,
    TGUID(IID_IGraphBuilder), FGraphBuilder);
  GetLastErrorString;
  if (FLastResult <> S_OK) then Exit;

  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEventEx);
  if (FLastResult <> S_OK) then Exit;

  FCustomVSFilterIntallOk := False;
  if (FAutoInsertCustomVSFilter) then
   begin
    CodecPath := ExtractFilePath(ParamStr(0))+'VSSCustomVSFilterV2a.dll';
    FHCustomVSFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
    if (FHCustomVSFilterInst <> 0) then
    begin
      FLastResult := CreateFilterFromFile(FHCustomVSFilterInst,
        CLSID_DirectVobSubFilter, VSFilter);
      if Succeeded(FLastResult) then
       begin
        FGraphBuilder.AddFilter(VSFilter, 'VSFilter Custom (internal)');
        SetSubtitleFont(MainForm.ConfigObject.SubVideoFont, Trunc(MainForm.ConfigObject.TransparencySubVideoFont / 10 * $FF),
          MainForm.ConfigObject.OutlineSubVideoFont,MainForm.ConfigObject.OutlineSubVideoColorFont, Trunc(MainForm.ConfigObject.TransparencyOutlineSubVideoFont / 10 * $FF),
          MainForm.ConfigObject.ShadowSubVideoFont,MainForm.ConfigObject.ShadowSubVideoColorFont, Trunc(MainForm.ConfigObject.TransparencyShadowSubVideoFont / 10 * $FF));
        SetSubtitleUpscalingSettings;
        SetSubtitleFlipSettings;
       end;
    end;
   end;

   // SPLITTER
   FLastResult := CoCreateInstance(CLSID_LavSplitterSource, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, SplitterFilter);
   if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
    begin
     FGraphBuilder.AddFilter(SplitterFilter, 'LAV Splitter');

     LAVSplitterSettings := GetLavSplitterSettingsInterface;
     LAVSplitterSettings.SetRuntimeConfig(True);
     LAVSplitterSettings.SetTrayIcon(False);
     pLanguages := MainForm.ConfigObject.LavPreferredLanguages;
     LAVSplitterSettings.SetPreferredLanguages(PWideChar(pLanguages));

     (SplitterFilter as IFileSourceFilter).Load(@Filename[1],nil);
    end
   else
   begin
    CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVSplitter.ax';
    FHLavSplitter := CoLoadLibrary(PWideChar(CodecPath),True);
    if (FHLavSplitter <> 0) then
      begin
       FLastResult := CreateFilterFromFile(FHLavSplitter,
        CLSID_LavSplitterSource, SplitterFilter);
       FGraphBuilder.AddFilter(SplitterFilter, 'LAV Splitter (internal)');

       LAVSplitterSettings := GetLavSplitterSettingsInterface;
       LAVSplitterSettings.SetRuntimeConfig(true);
       LAVSplitterSettings.SetTrayIcon(false);
       pLanguages := MainForm.ConfigObject.LavPreferredLanguages;
       LAVSplitterSettings.SetPreferredLanguages(PWideChar(pLanguages));

       (SplitterFilter as IFileSourceFilter).Load(@Filename[1],nil);
      end;
   end;

   // AUDIO RENDERER
   FLastResult := CoCreateInstance(CLSID_AudioRenderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioRenderer);
   if Succeeded(FLastResult) then
   begin
     FGraphBuilder.AddFilter(AudioRenderer, 'Default DirectSound Device');
   end;

   // VIDEO RENDERER (VMR-7)
   if MainForm.ConfigObject.PreferVmr7VideoRenderer then
    begin
     FLastResult := CoCreateInstance(CLSID_Vmr7Renderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VideoRenderer);
     if Succeeded(FLastResult) then
     begin
       FGraphBuilder.AddFilter(VideoRenderer, 'Video Mixing Filter 7');
     end;
    end;

   // VIDEO RENDERER (VMR-9)
   if MainForm.ConfigObject.PreferVmr9VideoRenderer then
    begin
     FLastResult := CoCreateInstance(CLSID_Vmr9Renderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VideoRenderer);
     if Succeeded(FLastResult) then
     begin
       FGraphBuilder.AddFilter(VideoRenderer, 'Video Mixing Renderer 9');
     end;
    end;

   // LAV CODEC VIDEO
   FLastResult := CoCreateInstance(CLSID_LavVideoDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VideoDecoderFilter);
   if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
    begin
      FGraphBuilder.AddFilter(VideoDecoderFilter, 'LAV Video Decoder');
    end
   else
    begin
     CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVVideo.ax';
     FHLAVVideoFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
     if (FHLAVVideoFilterInst <> 0) then
      begin
       FLastResult := CreateFilterFromFile(FHLAVVideoFilterInst,
         CLSID_LavVideoDecoder, VideoDecoderFilter);
       if Succeeded(FLastResult) then
        FGraphBuilder.AddFilter(VideoDecoderFilter, 'LAV Video Decoder (internal)');
      end;
    end;

   LAVVideoSettings := GetLavVideoSettingsInterface;
   LAVVideoSettings.SetTrayIcon(False);
   LAVVideoSettings.SetRuntimeConfig(True);
   if MainForm.ConfigObject.LavVHwNone then LAVVideoSettings.SetHWAccel(HWAccel_None);
   if MainForm.ConfigObject.LavVQuickSync then LAVVideoSettings.SetHWAccel(HWAccel_QuickSync);
   if MainForm.ConfigObject.LavVCuda then LAVVideoSettings.SetHWAccel(HWAccel_CUDA);
   if MainForm.ConfigObject.LavVDxva2 then  LAVVideoSettings.SetHWAccel(HWAccel_DXVA2CopyBack);

   // AUDIO
   FLastResult := CoCreateInstance(CLSID_LavAudioDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AudioDecoderFilter);
   if MainForm.ConfigObject.ForceRegisteredCodecs AND Succeeded(FLastResult) then
    begin
     FGraphBuilder.AddFilter(AudioDecoderFilter, 'LAV Audio Decoder');
    end
   else
    begin
     CodecPath := ExtractFilePath(ParamStr(0))+'CodecLav\LAVAudio.ax';
     FHLavAudioFilterInst := CoLoadLibrary(PWideChar(CodecPath),True);
     if (FHLavAudioFilterInst <> 0) then
      begin
       FLastResult := CreateFilterFromFile(FHLavAudioFilterInst,
         CLSID_LavAudioDecoder, AudioDecoderFilter);
       if Succeeded(FLastResult) then
        FGraphBuilder.AddFilter(AudioDecoderFilter, 'LAV Audio Decoder (internal)');
      end;
    end;

   LAVAudioSettings := GetLavAudioSettingsInterface;
   LAVAudioSettings.SetTrayIcon(False);
   LAVAudioSettings.SetRuntimeConfig(True);
   LAVAudioSettings.SetAudioDelay(False,0);
   if MainForm.ConfigObject.EnableLavAudioMixing then
    begin
     LAVAudioSettings.SetMixingEnabled(True);
     LAVAudioSettings.GetMixingLevels(centerLevel, surroundLevel, LfeLevel);
     LAVAudioSettings.SetMixingLevels(centerLevel, surroundLevel, LfeLevel);
     LAVAudioSettings.SetMixingFlags(LAV_MIXING_FLAG_UNTOUCHED_STEREO);
    end;

  // Detect PIN
  SplitterFilter.FindPin('Video', PinOutLavSplitterVideo);
  SplitterFilter.FindPin('Audio', PinOutLavSplitterAudio);

  GetFilterPin(VideoDecoderFilter,PINDIR_INPUT,PinInLavCodecVideo);
  GetFilterPin(VideoDecoderFilter,PINDIR_OUTPUT,PinOutLavCodecVideo);

  GetFilterPin(VSFilter,PINDIR_INPUT,PinInVsFilter);
  GetFilterPin(VSFilter,PINDIR_OUTPUT,PinOutVsFilter);

  GetFilterPin(AudioDecoderFilter,PINDIR_INPUT,PinInLavCodecAudio);
  GetFilterPin(AudioDecoderFilter,PINDIR_OUTPUT,PinOutLavCodecAudio);

  GetFilterPin(VideoRenderer,PINDIR_INPUT,PinInVideoRenderer);
  GetFilterPin(AudioRenderer,PINDIR_INPUT,PinInAudioRenderer);

  // CONNECT FILTERS VIDEO CHAIN
  FLastResult := FGraphBuilder.Connect(PinOutLavSplitterVideo,PinInLavCodecVideo); // SPLITTER VIDEO OUT -->CODEC VIDEO IN
  FLastResult := FGraphBuilder.Connect(PinOutLavCodecVideo,PinInVsFilter); // CODEC VIDEO OUT --> VSFILTER IN
  FLastResult := FGraphBuilder.Connect(PinOutVsFilter,PinInVideoRenderer); // CODEC VSFILTER OUT --> RENDERED VIDEO IN

  // CONNECT FILTERS AUDIO CHAIN
  FLastResult := FGraphBuilder.Connect(PinOutLavSplitterAudio,PinInLavCodecAudio); // SPLITTER AUDIO OUT -->CODEC AUDIO IN
  FLastResult := FGraphBuilder.Connect(PinOutLavCodecAudio,PinInAudioRenderer); // CODEC AUDIO OUT --> RENDERED AUDIO IN

  // ------- substitute Audio renderer with Reclock renderer
  if MainForm.ConfigObject.UseReclockAudioRenderer then
   begin
    ReclockRenderer := FindFilterByCLSID(FGraphBuilder, CLSID_AudioRenderer);
    if Assigned(ReclockRenderer) then
     begin
      if Assigned(PinOutLavCodecAudio) then
       begin
        FLastResult := CoCreateInstance(CLSID_Reclock, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, ReclockRenderer);
        if Succeeded(FLastResult) then
        begin
          FLastResult := FGraphBuilder.AddFilter(ReclockRenderer, 'Reclock Audio Renderer');
          FGraphBuilder.Disconnect(PinOutLavCodecAudio);
          FGraphBuilder.Disconnect(PinInAudioRenderer);
          GetFilterPin(ReclockRenderer,PINDIR_INPUT,PinInReclockRenderer);
          FGraphBuilder.Connect(PinOutLavCodecAudio,PinInReclockRenderer);
        end;
       end;
     end;
   end;
  // -------------------------------------------

  // ------- remove unused filters
  if Assigned(VideoDecoderFilter) then
   begin
    GetFilterPin(VideoDecoderFilter,PINDIR_OUTPUT,PinOut);
    Pinout.ConnectedTo(PinIn);
    if not Assigned(PinIn) then
     FGraphBuilder.RemoveFilter(VideoDecoderFilter);
   end;
  if Assigned(AudioDecoderFilter) then
   begin
    GetFilterPin(AudioDecoderFilter,PINDIR_OUTPUT,PinOut);
    Pinout.ConnectedTo(PinIn);
    if not Assigned(PinIn) then
     FGraphBuilder.RemoveFilter(AudioDecoderFilter);
   end;
  if Assigned(ReclockRenderer) then
   begin
    GetFilterPin(ReclockRenderer,PINDIR_INPUT,PinIn);
    PinIn.ConnectedTo(PinOut);
    if not Assigned(PinOut) then
     FGraphBuilder.RemoveFilter(ReclockRenderer);
   end;
  // ----------------------------

  Result := (FLastResult = S_OK);
  FIsOpen := Result;

  FGraphBuilder.QueryInterface(IID_IBasicVideo2, BasicVideoI);
  BasicVideoI.get_VideoWidth(FVideoWidth);
  BasicVideoI.get_VideoHeight(FVideoHeight);
  BasicVideoI := nil;

  FGraphBuilder.QueryInterface(IID_IBasicAudio, FBasicAudio);

  VSFilter := GetDirectVobSubFilter;
  if Assigned(VSFilter) then
  begin
    FLastResult := VSFilter.QueryVendorInfo(VSFilterVendorInfo);
    if Succeeded(FLastResult) then
    begin
      FCustomVSFilterIntallOk := Pos('VSS Custom VSFilter', VSFilterVendorInfo) = 1;
      CoTaskMemFree(VSFilterVendorInfo);
    end;
  end;

  AddToRot(FGraphBuilder, FROTID);

  SetRate(100);
end;

//------------------------------------------------------------------------------

end.

