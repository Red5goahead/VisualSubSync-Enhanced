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

unit ProjectUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, TntStdCtrls, TntDialogs, Renderer,
  TntButtons,
  ComCtrls, TntComCtrls, ExtCtrls;

type
  TProjectWAVMode = (pwmNoWaveform, pwmExternal, pwmPeakOnly);
  TVSSProject = class
  private
    FIsDirty : Boolean;
    FOnDirtyChange : TNotifyEvent;
    FOnDirtySet : TNotifyEvent;

    procedure SetIsDirty(Value : Boolean);
  public
    Filename : WideString;
    VideoSource : WideString;
    WAVFile : WideString;
    PeakFile : WideString;
    SubtitlesFile : WideString;
    SubtitlesVO : WideString;
    WAVMode : TProjectWAVMode;
    IsUTF8 : Boolean;
    TextPipeSource : WideString;
    TextPipePosition : Integer;
    ShowVideo : Boolean;
    ShowVO : Boolean;
    VideoPanelWidth : Integer;
    VideoPanelHeight : Integer;
    VideoWindowNormalLeft : Integer;
    VideoWindowNormalTop : Integer;
    VideoWindowNormalWidth : Integer;
    VideoWindowNormalHeight : Integer;
    DetachedVideo : Boolean;
    WAVDisplayerPositionStartMs : Integer;
    WAVDisplayerPositionStopMs : Integer;
    WAVDisplayerVerticalScaling : Integer;
    FocusedTimeMs : Integer;
    Dictionary : WideString;
    Presets : WideString;
    SelectedAudioTrackOnExtract : Integer;

    VideoWidth : Widestring;
    VideoHeight : Widestring;
    VideoFrameRate : Widestring;
    VideoFormat : WideString;
    VideoCodecProfile : WideString;
    AudioFormat : WideString;
    AudioBitRate : WideString;
    AudioSamplingRate : WideString;
    MatroskaContainer : boolean;
    AviContainer : boolean;
    Mp4Container : boolean;
    FlvContainer : boolean;
    MpegContainer : boolean;
    VideoStreamCount : string;
    AudioStreamCount : string;

    procedure LoadFromINIFile(ProjectFilename : WideString);
    procedure SaveSkippedSubOnErrorCheck(id: String);
    Function SearchSkippedSubOnErrorCheck(id: String):Boolean;
    constructor Create;
  published
    property IsDirty : Boolean read FIsDirty write SetIsDirty;
    property OnDirtyChange : TNotifyEvent read FOnDirtyChange write FOnDirtyChange;
    property OnDirtySet : TNotifyEvent read FOnDirtySet write FOnDirtySet;
  end;

  TProjectForm = class(TForm)
    bttCreateNewProject: TTntButton;
    bttCancel: TTntButton;
    gbVideoFile: TTntGroupBox;
    bttBrowseVideoFile: TSpeedButton;
    EditVideoFilename: TTntEdit;
    gbWAVFile: TTntGroupBox;
    bttBrowseWAVFile: TSpeedButton;
    EditWAVFilename: TTntEdit;
    gbSubtitleFile: TTntGroupBox;
    bttBrowseSubtitleFile: TSpeedButton;
    EditSubtitleFilename: TTntEdit;
    gbProjectFile: TTntGroupBox;
    bttBrowseProjectFile: TSpeedButton;
    EditProjectFilename: TTntEdit;
    TntOpenDialogBrowseGenericFile: TTntOpenDialog;
    bttOk: TTntButton;
    rbExternal: TRadioButton;
    rbPeakOnly: TRadioButton;
    bttBrowsePeakFile: TSpeedButton;
    EditPeakFilename: TTntEdit;
    cbSubtitleFormat: TTntComboBox;
    gbVO: TTntGroupBox;
    EditSubtitleVO: TTntEdit;
    bttBrowseSubtitleVO: TSpeedButton;
    rbNoWaveform: TTntRadioButton;
    bttExtractWAVFromVideo: TTntButton;
    chkSaveAsUTF8: TTntCheckBox;
    chkLaunchTraslateActionOnCreate: TTntCheckBox;
    gbVideoSourceOperation: TGroupBox;
    VideoSourceOperationRemux: TTntRadioButton;
    VideoSourceOperationRecodeAudio1: TTntRadioButton;
    VideoSourceOperationRecodeAudio2: TTntRadioButton;
    VideoSourceOperationSetSubtitleVO: TTntRadioButton;
    VideoSourceOperationSetSubtitleFile: TTntRadioButton;
    VideoSourceOperationMD5: TTntRadioButton;
    VideoSourceOperationExecute: TTntButton;
    cbbVideoSourceOperationTextTracks: TTntComboBox;
    VideoSourceOperationGeneratePeakFile: TTntRadioButton;
    cbbVideoSourceOperationAudioTracks: TTntComboBox;
    VideoSourceOperationAudioTracksOnlyCenter: TTntCheckBox;
    VideoSourceOperationNone: TTntRadioButton;
    VideoSourceOperationSceneChange: TTntRadioButton;
    procedure bttCreateNewProjectClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure bttBrowseVideoFileClick(Sender: TObject);
    procedure bttBrowseWAVFileClick(Sender: TObject);
    procedure bttBrowseSubtitleFileClick(Sender: TObject);
    procedure bttBrowseProjectFileClick(Sender: TObject);
    procedure bttBrowseVOFileClick(Sender: TObject);
    procedure bttExtractWAVFromVideoClick(Sender: TObject);
    procedure bttOkClick(Sender: TObject);
    procedure rbInternalWAVClick(Sender: TObject);
    procedure bttBrowsePeakFileClick(Sender: TObject);
    procedure cbSubtitleFormatChange(Sender: TObject);
    procedure EditPeakFilenameEnter(Sender: TObject);
    procedure EditPeakFilenameExit(Sender: TObject);
    procedure EditUpdateColor(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VideoSourceOperationRemuxClick(Sender: TObject);
    procedure VideoSourceOperationRecodeAudio1Click(Sender: TObject);
    procedure VideoSourceOperationRecodeAudio2Click(Sender: TObject);
    procedure VideoSourceOperationMD5Click(Sender: TObject);
    procedure VideoSourceOperationSetSubtitleFileClick(Sender: TObject);
    procedure VideoSourceOperationSetSubtitleVOClick(Sender: TObject);
    procedure VideoSourceOperationGeneratePeakFileClick(Sender: TObject);
    procedure cbbVideoSourceOperationAudioTracksSelect(Sender: TObject);
    procedure VideoSourceOperationExecuteClick(Sender: TObject);
    procedure VideoSourceOperationNoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure WAVSelectMode(WavMode : TProjectWAVMode);
    procedure UpdateFormatCombobox;
    function ShowExtractForm(extType : TWAVExtractionType) : Boolean;
    function CheckData(AskForExtraction : Boolean) : Boolean;
    procedure UpdateColor;
    procedure VideoSourceOperation(silent : Boolean);
  public
    { Public declarations }
    SelectedAudioTrackOnExtract : Integer;
    UTF8AsDefault : Boolean;
    procedure Clear;
    procedure ConfigureInNewProjectMode;
    procedure ConfigureInModifyProjectMode(Project : TVSSProject);
    function GetWAVMode : TProjectWAVMode;
    function GetPeakFilename : WideString;
    function GetWAVFilename : WideString;
  end;

var
  ProjectForm: TProjectForm;

const
  LEAVE_EMPTY : WideString = 'Leave empty for extraction';

implementation

{$R *.dfm}

uses WAVExtractFormUnit, TntSysUtils, MiscToolsUnit, TntIniFiles, TntWindows, MediaInfoDll, CursorManager,ShellApi,
     MD5,ClipBrd, Main, SourceFileOperationCancel, Types;

function GetFileSizeEx(hFile: THandle; var FileSize: Int64): BOOL; stdcall;
  external kernel32;

//==============================================================================

function CheckFileCanBeOpened(const fileName : WideString) : Boolean;
var fileHandle : THandle;
begin
  fileHandle := Tnt_CreateFileW(PWideChar(fileName), GENERIC_READ,
    FILE_SHARE_READ	, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (fileHandle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(fileHandle);
    Result := True;
  end
  else
    Result := False;
end;

function CheckFileCanBeCreated(const fileName : WideString) : Boolean;
var fileHandle : THandle;
begin
  if CheckFileCanBeOpened(fileName) then
  begin
    Result := True
  end
  else
  begin
    fileHandle := Tnt_CreateFileW(PWideChar(fileName), GENERIC_READ,
      FILE_SHARE_READ	, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_DELETE_ON_CLOSE, 0);
    if (fileHandle <> INVALID_HANDLE_VALUE) then
    begin
      CloseHandle(fileHandle);
      Result := True;
    end
    else
      Result := False;
  end;
end;

function ExecAndWait(const FileName, Params: string;
  WindowState: Word): Boolean;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
begin
  { Enclose filename in quotes to take care of
    long filenames with spaces. }
  CmdLine := '"' + FileName + '"' + Params;
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do
  begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;
  Result := CreateProcess(nil, PChar(CmdLine), nil, nil, False,
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS, nil,
    PChar(ExtractFilePath(FileName)),
    SUInfo, ProcInfo);
  { Wait for it to finish. }
  if Result then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);

end;

function ExecAndWaitConsoleExtended(const FileName, Params, Title: string; XChar, YChar, PosX, PosY : Integer): Boolean;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
begin
  { Enclose filename in quotes to take care of
    long filenames with spaces. }
  CmdLine := '"' + FileName + '"' + Params;
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do
  begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USECOUNTCHARS;
    dwXCountChars := XChar;
    dwYCountChars := YChar;
    if (PosX > 0) And (PosY > 0) Then
    begin
      dwFlags := dwFlags + STARTF_USEPOSITION;
      dwX := PosX;
      dwY := PosY;
    end;
    if Title <> '' then lpTitle := PChar(Title);
  end;
  Result := CreateProcess(nil, PChar(CmdLine), nil, nil, False,
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS, nil,
    PChar(ExtractFilePath(FileName)),
    SUInfo, ProcInfo);
  { Wait for it to finish. }
  if Result then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

//==============================================================================

constructor TVSSProject.Create;
begin
  inherited;
  FOnDirtyChange := nil;
  FOnDirtySet := nil;
  FIsDirty := False;
end;

procedure TVSSProject.SetIsDirty(Value : Boolean);
begin
  if FIsDirty <> Value then
  begin
    FIsDirty := Value;
    if Assigned(FOnDirtyChange) then
    begin
      FOnDirtyChange(Self);
    end;
  end;
  if Value and Assigned(FOnDirtySet) then
  begin
    FOnDirtySet(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TVSSProject.LoadFromINIFile(ProjectFilename : WideString);
var
  ProjectFileIni : TTntIniFile;
begin
    ProjectFileIni := TTntIniFile.Create(ProjectFilename);
    Filename := ProjectFilename;
    VideoSource := ProjectFileIni.ReadString('VisualSubsync','VideoSource','');
    WAVFile := ProjectFileIni.ReadString('VisualSubsync','WAVFile','');
    PeakFile := ProjectFileIni.ReadString('VisualSubsync','PeakFile','');
    WAVMode := TProjectWAVMode(ProjectFileIni.ReadInteger('VisualSubsync','WAVMode',1));
    SubtitlesFile := ProjectFileIni.ReadString('VisualSubsync','SubtitlesFile','');
    SubtitlesVO := ProjectFileIni.ReadString('VisualSubsync','SubtitlesVO','');
    TextPipeSource := ProjectFileIni.ReadString('VisualSubsync','TextPipeSource','');
    TextPipePosition := ProjectFileIni.ReadInteger('VisualSubsync','TextPipePosition',0);

    VideoPanelWidth := ProjectFileIni.ReadInteger('VisualSubsync','VideoPanelWidth', 0);
    VideoPanelHeight := ProjectFileIni.ReadInteger('VisualSubsync','VideoPanelHeight', 0);
    ShowVideo := ProjectFileIni.ReadBool('VisualSubsync','ShowVideo', True);
    ShowVO := ProjectFileIni.ReadBool('VisualSubsync','ShowVO', False);
    if (ShowVO = false) AND (SubtitlesVO <> '') then ShowVO := True;
    DetachedVideo := ProjectFileIni.ReadBool('VisualSubsync','DetachedVideo', False);
    VideoWindowNormalLeft := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalLeft', 0);
    VideoWindowNormalTop := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalTop', 0);
    VideoWindowNormalWidth := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalWidth', 0);
    VideoWindowNormalHeight := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalHeight', 0);

    WAVDisplayerPositionStartMs := ProjectFileIni.ReadInteger('VisualSubsync', 'WAVDisplayerPositionStartMs', -1);
    WAVDisplayerPositionStopMs := ProjectFileIni.ReadInteger('VisualSubsync', 'WAVDisplayerPositionStopMs', -1);
    WAVDisplayerVerticalScaling := ProjectFileIni.ReadInteger('VisualSubsync', 'WAVDisplayerVerticalScaling', 100);
    FocusedTimeMs := ProjectFileIni.ReadInteger('VisualSubsync', 'FocusedTimeMs', -1);

    Dictionary := ProjectFileIni.ReadString('VisualSubsync','Dictionary','');
    Presets := ProjectFileIni.ReadString('VisualSubsync','Presets','');

    SelectedAudioTrackOnExtract := ProjectFileIni.ReadInteger('VisualSubsync', 'SelectedAudioTrackOnExtract', 1);

    ProjectFileIni.Free;

    // Resolve relative path
    VideoSource := WideResolveRelativePath(Filename, VideoSource);
    WAVFile := WideResolveRelativePath(Filename, WAVFile);
    PeakFile := WideResolveRelativePath(Filename, PeakFile);
    SubtitlesFile := WideResolveRelativePath(Filename, SubtitlesFile);
    SubtitlesVO := WideResolveRelativePath(Filename, SubtitlesVO);
    TextPipeSource := WideResolveRelativePath(Filename, TextPipeSource);
end;


//------------------------------------------------------------------------------
Function TVSSProject.SearchSkippedSubOnErrorCheck(id: String):Boolean;
var
  ProjectFileIni : TTntIniFile; PrgSkippedOnErrorCheck, LastSkippedOnErrorCheck : Integer;
  Found : Boolean;
begin
 ProjectFileIni := TTntIniFile.Create(Filename);

 LastSkippedOnErrorCheck := ProjectFileIni.ReadInteger('VisualSubsync','LastSkippedOnErrorCheck',0);

 Found := False;
 for PrgSkippedOnErrorCheck := 0 to LastSkippedOnErrorCheck do
  begin
   if ProjectFileIni.ReadString('VisualSubsync','SkippedSub' + Format('%.3d',[PrgSkippedOnErrorCheck]),'<#@skipped@#>') = id then
    begin
      Found := True;
      break;
    end;
  end;

  Result := Found;

end;

//------------------------------------------------------------------------------

procedure TVSSProject.SaveSkippedSubOnErrorCheck(id: String);
var
  ProjectFileIni : TTntIniFile; PrgSkippedOnErrorCheck, LastSkippedOnErrorCheck : Integer;
  Found : Boolean;
begin
 ProjectFileIni := TTntIniFile.Create(Filename);

 LastSkippedOnErrorCheck := ProjectFileIni.ReadInteger('VisualSubsync','LastSkippedOnErrorCheck',0);

 Found := False;
 for PrgSkippedOnErrorCheck := 0 to LastSkippedOnErrorCheck do
  begin
   if ProjectFileIni.ReadString('VisualSubsync','SkippedSub' + Format('%.3d',[PrgSkippedOnErrorCheck]),'') = id then
    begin
      Found := True;
      break;
    end;
  end;

 if not Found then
  begin
    ProjectFileIni.WriteString('VisualSubsync','SkippedSub' + Format('%.3d',[LastSkippedOnErrorCheck+1]),id);
    ProjectFileIni.WriteInteger('VisualSubsync','LastSkippedOnErrorCheck',LastSkippedOnErrorCheck+1);
  end;

 ProjectFileIni.Free;
end;

//==============================================================================

function TProjectForm.CheckData(AskForExtraction : Boolean) : Boolean;
var Ext : WideString;
    VideoFileExists : Boolean;
begin
  Result := False;
  VideoFileExists := (Trim(EditVideoFilename.Text) <> '') and WideFileExists(EditVideoFilename.Text);

  // We need a valid extension subtitle filename  
  Ext := WideLowerCase(WideExtractFileExt(EditSubtitleFilename.Text));
  if (Trim(EditSubtitleFilename.Text) <> '') and
    (Ext <> '.srt') and (Ext <> '.ass') and (Ext <> '.ssa') then
  begin
    MessageBoxW(Handle, PWideChar(WideString('Invalid file extension.')),
      PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditSubtitleFilename.SetFocus;
    Exit;
  end;  

  // We need a least subtitle filename
  if not CheckFileCanBeCreated(EditSubtitleFilename.Text) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The subtitle file name is missing or is invalid.')),
      PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditSubtitleFilename.SetFocus;
    Exit;
  end;

  // External WAV file mode
  if (rbExternal.Checked) then
  begin

    if (not WideFileExists(GetWAVFilename)) then
    begin
      if ((AskForExtraction or (EditWAVFilename.Text = LEAVE_EMPTY)) and VideoFileExists) then
      begin
        if ShowExtractForm(wetFastConversion) = False then
          Exit;
      end
      else
      begin
        MessageBoxW(Handle, PWideChar(WideString('The WAV file "' + EditWAVFilename.Text +
          '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
        EditWAVFilename.SetFocus;
        Exit;
      end;
    end;
  end;

  // Peak file only mode
  if (rbPeakOnly.Checked) then
  begin
    if (not WideFileExists(GetPeakFilename)) then
    begin
      if ((AskForExtraction or (EditPeakFilename.Text = LEAVE_EMPTY)) and VideoFileExists) then
      begin
        if ShowExtractForm(wetOnlyPeakFile) = False then
          Exit;
      end
      else
      begin
        MessageBoxW(Handle, PWideChar(WideString('The peak file "' + EditPeakFilename.Text +
          '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
        EditPeakFilename.SetFocus;
        Exit;
      end;
    end;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttCreateNewProjectClick(Sender: TObject);
var res : Integer;
begin

  if VideoSourceOperationGeneratePeakFile.Enabled = True And
    VideoSourceOperationGeneratePeakFile.Checked = True And
    WideFileExists(EditVideoFilename.Text) AND
    rbPeakOnly.Checked = True AND not WideFileExists(EditPeakFilename.Text) Then
  begin
    VideoSourceOperation(False);
  end;

  if not CheckData(True) then
    Exit;

  if WideFileExists(EditProjectFilename.Text) then
  begin
    // ask for replacement or cancel
    res := MessageBoxW(Handle,
      PWideChar(WideString('The project file named ' +
      EditProjectFilename.Text + ' already exist.' + #13#10 +
      'Do you want to overwrite this file ?')),
      PWideChar(WideString('Please confirme :')),
      MB_YESNO or MB_ICONWARNING);
    if (res = IDNO) then
      Exit;
  end;

  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseVideoFileClick(Sender: TObject);
var WAVFilename, PeakFilename : WideString;
  MediaInfoHandle : Cardinal; AudioStreamCount, TextStreamCount, TextStreamAudio: WideString;
  iAudioStreamCount : Word;
  I, OrdinalSub, OrdinalAudio : Integer; Format,Language,Layout,Title:WideString;
begin

  TntOpenDialogBrowseGenericFile.FileName := EditVideoFilename.Text;
  TntOpenDialogBrowseGenericFile.Filter :=
    'Video files|*.AVI;*.OGM;*.MKV;*.MKA;*.MP4;*.DIVX;*.RM;' +
    '*.RMVB;*.MPEG;*.MPG;*.VOB;*.AVS;*.WMV;*.MOV;*.FLV;*.TS;*.M2TS' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin
    EditVideoFilename.Text := TntOpenDialogBrowseGenericFile.FileName;

    VideoSourceOperationNone.Enabled :=True;
    VideoSourceOperationNone.Checked :=False;

    // If external wav file exists with the same name use it
    WAVFilename := WideChangeFileExt(EditVideoFilename.Text,'.wav');
    PeakFilename := WideChangeFileExt(EditVideoFilename.Text,'.peak');
    if (Trim(GetWAVFilename) = '') and WideFileExists(WAVFilename) then
    begin
      EditWAVFilename.Text := WAVFilename;
      WAVSelectMode(pwmExternal);
    end
    // else if peak file exists with the same name use it
    else if (Trim(GetPeakFilename) = '') and WideFileExists(PeakFilename) then
    begin
      EditPeakFilename.Text := PeakFilename;
      WAVSelectMode(pwmPeakOnly);
    end
    // else choose peak file
    else
    begin
      WAVSelectMode(pwmPeakOnly);
    end;

    if Trim(EditSubtitleFilename.Text) = '' then
    begin
      EditSubtitleFilename.Text := WideChangeFileExt(EditVideoFilename.Text,'.srt');
      chkLaunchTraslateActionOnCreate.Enabled := True;
      UpdateFormatCombobox;
    end;

    if Trim(EditProjectFilename.Text) = '' then
    begin
      EditProjectFilename.Text := WideChangeFileExt(EditVideoFilename.Text,'.vssprj');
    end;

    // Check for audio tracks
    MediaInfoHandle := 0;
    if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
     begin
       MediaInfoHandle := MediaInfo_New();
       MediaInfo_Open(MediaInfoHandle, PWideChar(EditVideoFilename.Text));
       AudioStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'StreamCount', Info_Text, Info_Name);
       if AudioStreamCount <> '' then
        iAudioStreamCount := StrToInt(AudioStreamCount);

       cbbVideoSourceOperationAudioTracks.Items.Clear;

       For I := 0 to iAudioStreamCount-1 do
       begin
         OrdinalAudio := I + 1;

         Language := MediaInfo_Get(MediaInfoHandle, Stream_Audio, I, 'Language/String', Info_Text, Info_Name);
         if Language > '' then Language := '[' + Language + '] ';
         AudioStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Audio, I, 'StreamCount', Info_Text, Info_Name);
         TextStreamAudio := RightPad(IntToStr(OrdinalAudio),'0',2) + ': ' + MediaInfo_Get(MediaInfoHandle, Stream_Audio, I, 'Format', Info_Text, Info_Name) + ' ' +
           MediaInfo_Get(MediaInfoHandle, Stream_Audio, I, 'Channel(s)/String', Info_Text, Info_Name) + ' ' +
           MediaInfo_Get(MediaInfoHandle, Stream_Audio, I, 'BitRate/String', Info_Text, Info_Name) + ' ' +
           MediaInfo_Get(MediaInfoHandle, Stream_Audio, I, 'SamplingRate/String', Info_Text, Info_Name) + ' ' +
           Language;
         cbbVideoSourceOperationAudioTracks.Items.Add(TextStreamAudio);
       end;
       if cbbVideoSourceOperationAudioTracks.Items.Count > 0 then
       begin
         cbbVideoSourceOperationAudioTracks.ItemIndex := 0;
         if cbbVideoSourceOperationAudioTracks.Items.Count = 1 then
          begin
            cbbVideoSourceOperationAudioTracks.enabled := False;
            Layout := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'ChannelLayout', Info_Text, Info_Name);
            if (pos(' C ', Layout) > 0) OR (pos('C', Layout) > 0) OR (pos(' C', Layout) > 0) then
              VideoSourceOperationAudioTracksOnlyCenter.Enabled := True
            else
            begin
              VideoSourceOperationAudioTracksOnlyCenter.Enabled := False;
              VideoSourceOperationAudioTracksOnlyCenter.Checked := False;
            end;
          end
         else cbbVideoSourceOperationAudioTracks.enabled := True;
         VideoSourceOperationGeneratePeakFile.Checked := True;
       end;
     end;

    if iAudioStreamCount > 0 then
    begin
     VideoSourceOperationGeneratePeakFile.Enabled := True;
    end;

    VideoSourceOperationRemux.Enabled := False;
    VideoSourceOperationRemux.Checked :=False;
    if ( (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mkv') OR (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mp4') OR
         (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.ts')
       )
        AND (DirectoryExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix') ) AND
       (FileExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\mkvmerge.exe') )
        then
     begin
       VideoSourceOperationRemux.Enabled := True;
     end;

    VideoSourceOperationRecodeAudio1.Enabled := False;
    VideoSourceOperationRecodeAudio1.Checked :=False;
    VideoSourceOperationRecodeAudio2.Enabled := False;
    VideoSourceOperationRecodeAudio2.Checked :=False;
    if iAudioStreamCount > 0 Then
      if ( (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mkv') OR (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mp4') OR
           (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.ts') OR (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.m2ts') OR
           (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.avi'))
          AND (DirectoryExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix') ) AND
        (FileExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\ffmpeg.exe') )
         then
     begin
       VideoSourceOperationRecodeAudio1.Enabled := True;
       VideoSourceOperationRecodeAudio2.Enabled := True;
     end;

    // Check for text tracks ASS and UTF-8
    cbbVideoSourceOperationTextTracks.Items.Clear;
    cbbVideoSourceOperationTextTracks.Enabled := False;
    VideoSourceOperationSetSubtitleFile.Enabled := False;
    VideoSourceOperationSetSubtitleFile.Checked :=False;
    VideoSourceOperationSetSubtitleVO.Enabled := False;
    VideoSourceOperationSetSubtitleVO.Checked :=False;
    if DirectoryExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix') AND
       (FileExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\mkvmerge.exe')) then
       begin
        MediaInfoHandle := 0;
        if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
         begin
           MediaInfoHandle := MediaInfo_New();
           MediaInfo_Open(MediaInfoHandle, PWideChar(EditVideoFilename.Text));
           TextStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Text, 0, 'StreamCount', Info_Text, Info_Name);
           if (TextStreamCount <> '') And (StrToInt(TextStreamCount) > 0) Then
            begin
              For I := 0 to StrToInt(TextStreamCount)-1 do
               begin
                OrdinalSub := I;
                Format := MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Format', Info_Text, Info_Name);
                if (Format = 'ASS') Or
                   (Format = 'UTF-8') then
                 begin
                   Title := MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Title', Info_Text, Info_Name);
                   if Title = '' then
                    Language:= MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Language/String', Info_Text, Info_Name)
                   else Language := Title;

                   if Language = '' Then Language := '<not defined>'
                     else Language := '<' + Language + '>';

                   cbbVideoSourceOperationTextTracks.Items.Add(RightPad(IntToStr(OrdinalSub),'0',2) + ': ' + Language + ', ' + Format);
                   cbbVideoSourceOperationTextTracks.Enabled := True;
                   VideoSourceOperationSetSubtitleFile.Enabled := True;
                   VideoSourceOperationSetSubtitleVO.Enabled := True;

                 end;
               end;
              if cbbVideoSourceOperationTextTracks.Items.Count > 0 Then cbbVideoSourceOperationTextTracks.ItemIndex:=0;
            end;
           MediaInfo_Close(MediaInfoHandle);
         end;
       end;

     VideoSourceOperationMD5.Enabled := True;
     VideoSourceOperationMD5.Checked :=False;

     VideoSourceOperationSceneChange.Enabled := True;
     VideoSourceOperationSceneChange.Checked := False;

     bttCreateNewProject.Enabled := True;

   end;

  bttExtractWAVFromVideo.Enabled := WideFileExists(EditVideoFilename.Text);
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseWAVFileClick(Sender: TObject);
begin
  TntOpenDialogBrowseGenericFile.FileName := GetWAVFilename;
  TntOpenDialogBrowseGenericFile.Filter := 'WAV files (*.wav)|*.WAV' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin
    EditWAVFilename.Text := TntOpenDialogBrowseGenericFile.FileName;
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseSubtitleFileClick(Sender: TObject);
var Ext : WideString; 
begin
  TntOpenDialogBrowseGenericFile.FileName := EditSubtitleFilename.Text;
  TntOpenDialogBrowseGenericFile.Filter := 'SRT files (*.srt)|*.SRT' + '|' +
    'SSA/ASS files (*.ssa,*.ass)|*.SSA;*.ASS' + '|' +
    'NetflixTimedText (*.dfxp)|*.DFXP' + '|' +
    'YouTubeSbv (*.sbv)|*.SBV' + '|' +
    'WebVTT (*.vtt)|*.VTT' + '|' +
    'All files (*.*)|*.*';

  Ext := WideLowerCase(WideExtractFileExt(EditSubtitleFilename.Text));
  if (Ext = '.srt') then
    TntOpenDialogBrowseGenericFile.FilterIndex := 1
  else if (Ext = '.ssa') or (Ext = '.ass') then
    TntOpenDialogBrowseGenericFile.FilterIndex := 2
  else
    TntOpenDialogBrowseGenericFile.FilterIndex := 6;

  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin

    if (TntOpenDialogBrowseGenericFile.FilterIndex = 3) OR
       (TntOpenDialogBrowseGenericFile.FilterIndex = 4) OR
       (TntOpenDialogBrowseGenericFile.FilterIndex = 5) then
    begin

      if not NetFramework4Installed() then
      begin
        MessageDlg('Selected format requires the .NET Framework 4 to be installed in the system', mtWarning,[mbOk],0);
        exit;
      end;

      MainForm.ConvertFromSupportedFormat(TntOpenDialogBrowseGenericFile.FileName,
        EditSubtitleFilename.Text, cbSubtitleFormat.Text);
    end
    else
    begin
      EditSubtitleFilename.Text := TntOpenDialogBrowseGenericFile.FileName;
    end;

    chkLaunchTraslateActionOnCreate.Enabled := true;
    UpdateFormatCombobox;
    if Trim(EditProjectFilename.Text) = '' then
    begin
      EditProjectFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.vssprj');
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseProjectFileClick(Sender: TObject);
begin
  TntOpenDialogBrowseGenericFile.FileName := EditProjectFilename.Text;
  TntOpenDialogBrowseGenericFile.Filter := 'VSS project (*.vssprj)|*.VSSPRJ' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin
    EditProjectFilename.Text := TntOpenDialogBrowseGenericFile.FileName;
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseVOFileClick(Sender: TObject);
begin
  TntOpenDialogBrowseGenericFile.FileName := EditSubtitleVO.Text;
  TntOpenDialogBrowseGenericFile.Filter := 'SRT files (*.srt)|*.SRT' + '|' +
    'SSA/ASS files (*.ssa,*.ass)|*.SSA;*.ASS' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin
    EditSubtitleVO.Text := TntOpenDialogBrowseGenericFile.FileName;
    chkLaunchTraslateActionOnCreate.Enabled := false;
  end;
end;

//------------------------------------------------------------------------------

function TProjectForm.ShowExtractForm(extType : TWAVExtractionType) : Boolean;
var
  ExtractWAVForm: TExtractWAVForm;
  modalResult : Integer;
begin
  Result := False;
  if (Trim(EditVideoFilename.Text) = '') or (not WideFileExists(EditVideoFilename.Text)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The video file "' + EditVideoFilename.Text +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditVideoFilename.SetFocus;
    Exit;
  end;
  ExtractWAVForm := TExtractWAVForm.Create(nil);
  ExtractWAVForm.VideoFilename := EditVideoFilename.Text;
  ExtractWAVForm.DestinationFilename := WideChangeFileExt(EditVideoFilename.Text,'.wav');
  ExtractWAVForm.SetExtractionType(extType);
  modalResult := ExtractWAVForm.ShowModal;
  if modalResult = mrOk then
  begin
    if ExtractWAVForm.rbOnlyPeak.Checked then
    begin
      EditPeakFilename.Text := WideChangeFileExt(EditVideoFilename.Text,'.peak');
      WAVSelectMode(pwmPeakOnly);
    end
    else
    begin
      EditWAVFilename.Text := ExtractWAVForm.DestinationFilename;
      WAVSelectMode(pwmExternal);
    end;
    SelectedAudioTrackOnExtract := ExtractWAVForm.cbStreamIndex.ItemIndex + 1;
  end;
  Result := (modalResult = mrOk);
  ExtractWAVForm.Free;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttExtractWAVFromVideoClick(Sender: TObject);
begin
  ShowExtractForm(wetOnlyPeakFile);
end;

//------------------------------------------------------------------------------

procedure TProjectForm.Clear;
begin
  EditVideoFilename.Text := '';
  EditSubtitleFilename.Text := '';
  EditSubtitleVO.Text := '';
  EditProjectFilename.Text := '';
  chkSaveAsUTF8.Checked := False;
  cbSubtitleFormat.ItemIndex := 0;
  bttExtractWAVFromVideo.Enabled := False;

  EditPeakFilename.Text := LEAVE_EMPTY;
  EditPeakFilename.Font.Color := clInactiveCaption;

  EditWAVFilename.Text := LEAVE_EMPTY;
  EditWAVFilename.Font.Color := clInactiveCaption;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.ConfigureInNewProjectMode;
begin
  Self.Caption := 'New Project';
  bttCreateNewProject.Visible := True;
  bttOk.Visible := False;
  WAVSelectMode(pwmNoWaveform);
  cbbVideoSourceOperationTextTracks.Items.Clear;
  VideoSourceOperationNone.Enabled :=False;
  VideoSourceOperationGeneratePeakFile.Enabled :=False;
  VideoSourceOperationGeneratePeakFile.Checked :=False;
  cbbVideoSourceOperationAudioTracks.Visible:= False;
  VideoSourceOperationAudioTracksOnlyCenter.Visible:= False;
  VideoSourceOperationRemux.Enabled :=False;
  VideoSourceOperationRemux.Checked :=False;
  VideoSourceOperationRecodeAudio1.Enabled:=False;
  VideoSourceOperationRecodeAudio1.Checked :=False;
  VideoSourceOperationRecodeAudio2.Enabled:=False;
  VideoSourceOperationRecodeAudio2.Checked :=False;
  VideoSourceOperationMD5.Enabled:=False;
  VideoSourceOperationMD5.Checked :=False;
  VideoSourceOperationSetSubtitleFile.Enabled:=False;
  VideoSourceOperationSetSubtitleFile.Checked :=False;
  VideoSourceOperationSetSubtitleVO.Enabled:=False;
  VideoSourceOperationSetSubtitleVO.Checked :=False;
  cbbVideoSourceOperationTextTracks.Enabled:=False;
  cbbVideoSourceOperationTextTracks.Visible:= False;
  VideoSourceOperationExecute.Enabled:=False;
  VideoSourceOperationSceneChange.Enabled:=False;
  Self.Clear;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.ConfigureInModifyProjectMode(Project : TVSSProject);
begin
  Self.Caption := 'Project Properties';
  bttCreateNewProject.Visible := False;
  bttOk.Visible := True;
  EditVideoFilename.Text := Project.VideoSource;
  if (Project.WAVFile = '') then
    EditWAVFilename.Text := LEAVE_EMPTY
  else
    EditWAVFilename.Text := Project.WAVFile;
  EditSubtitleFilename.Text := Project.SubtitlesFile;
  EditSubtitleVO.Text := Project.SubtitlesVo;
  EditProjectFilename.Text := Project.Filename;
  if (Project.PeakFile = '') then
    EditPeakFilename.Text := LEAVE_EMPTY
  else
    EditPeakFilename.Text := Project.PeakFile;
  WAVSelectMode(Project.WAVMode);
  chkSaveAsUTF8.Checked := Project.IsUTF8;

  VideoSourceOperationNone.Enabled :=False;
  VideoSourceOperationGeneratePeakFile.Enabled :=False;
  VideoSourceOperationGeneratePeakFile.Checked :=False;
  cbbVideoSourceOperationAudioTracks.Visible:= False;
  VideoSourceOperationAudioTracksOnlyCenter.Visible:= False;
  VideoSourceOperationRemux.Enabled :=False;
  VideoSourceOperationRemux.Checked :=False;
  VideoSourceOperationRecodeAudio1.Enabled:=False;
  VideoSourceOperationRecodeAudio1.Checked :=False;
  VideoSourceOperationRecodeAudio2.Enabled:=False;
  VideoSourceOperationRecodeAudio2.Checked :=False;
  VideoSourceOperationMD5.Enabled:=False;
  VideoSourceOperationMD5.Checked :=False;
  VideoSourceOperationSetSubtitleFile.Enabled:=False;
  VideoSourceOperationSetSubtitleFile.Checked :=False;
  VideoSourceOperationSetSubtitleVO.Enabled:=False;
  VideoSourceOperationSetSubtitleVO.Checked :=False;
  cbbVideoSourceOperationTextTracks.Enabled:=False;
  cbbVideoSourceOperationTextTracks.Visible:= False;
  VideoSourceOperationExecute.Enabled:=False;

  UpdateFormatCombobox;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttOkClick(Sender: TObject);
begin
  if not CheckData(False) then
    Exit;
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.UpdateColor;
begin
  if (GetWAVFilename <> '') then
    EditWAVFilename.Font.Color := clWindowText
  else
    EditWAVFilename.Font.Color := clInactiveCaption;

  if (GetPeakFilename <> '') then
    EditPeakFilename.Font.Color := clWindowText
  else
    EditPeakFilename.Font.Color := clInactiveCaption;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.WAVSelectMode(WavMode : TProjectWAVMode);
begin
  rbNoWaveform.Checked := (WavMode = pwmNoWaveform);

  rbExternal.Checked := (WavMode = pwmExternal);
  EditWAVFilename.Enabled := (WavMode = pwmExternal);
  bttBrowseWAVFile.Enabled := (WavMode = pwmExternal);

  rbPeakOnly.Checked := (WavMode = pwmPeakOnly);
  EditPeakFilename.Enabled := (WavMode = pwmPeakOnly);
  bttBrowsePeakFile.Enabled := (WavMode = pwmPeakOnly);
end;

//------------------------------------------------------------------------------

procedure TProjectForm.rbInternalWAVClick(Sender: TObject);
begin
  WAVSelectMode(TProjectWAVMode((Sender as TRadioButton).Tag));
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowsePeakFileClick(Sender: TObject);
begin
  TntOpenDialogBrowseGenericFile.FileName := GetPeakFilename;
  TntOpenDialogBrowseGenericFile.Filter := 'Peak files (*.peak)|*.PEAK' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin
    EditPeakFilename.Text := TntOpenDialogBrowseGenericFile.FileName;
  end;
end;

//------------------------------------------------------------------------------

function TProjectForm.GetWAVMode : TProjectWAVMode;
begin
  if rbNoWaveform.Checked then
    Result := pwmNoWaveform
  else if rbExternal.Checked then
    Result := pwmExternal
  else
    Result := pwmPeakOnly
end;

//------------------------------------------------------------------------------

procedure TProjectForm.cbSubtitleFormatChange(Sender: TObject);
begin
  if Length(EditSubtitleFilename.Text) <= 0 then
    Exit;

  // Change subtitle format
  case cbSubtitleFormat.ItemIndex of
  0: EditSubtitleFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.srt');
  1: EditSubtitleFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.ssa');
  2: EditSubtitleFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.ass');
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.UpdateFormatCombobox;
var Ext : WideString;
begin
  Ext := WideLowerCase(WideExtractFileExt(EditSubtitleFilename.Text));
  if (Ext = '.srt') then
    cbSubtitleFormat.ItemIndex := 0
  else if (Ext = '.ssa') then
    cbSubtitleFormat.ItemIndex := 1
  else if (Ext = '.ass') then
    cbSubtitleFormat.ItemIndex := 2;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.EditPeakFilenameEnter(Sender: TObject);
var TntEdit : TTntEdit;
begin
  if (Sender is TTntEdit) then
  begin
    TntEdit := (Sender as TTntEdit);
    if (TntEdit.Text = LEAVE_EMPTY) then
    begin
      TntEdit.Text := '';
      TntEdit.Font.Color := clWindowText;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.EditPeakFilenameExit(Sender: TObject);
var TntEdit : TTntEdit;
begin
  if (Sender is TTntEdit) then
  begin
    TntEdit := (Sender as TTntEdit);
    if (TntEdit.Text = '') then
    begin
      TntEdit.Text := LEAVE_EMPTY;
      TntEdit.Font.Color := clInactiveCaption;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TProjectForm.GetPeakFilename : WideString;
begin
  if (EditPeakFilename.Text = LEAVE_EMPTY) then
    Result := ''
  else
    Result := EditPeakFilename.Text
end;

//------------------------------------------------------------------------------

function TProjectForm.GetWAVFilename : WideString;
begin
  if (EditWAVFilename.Text = LEAVE_EMPTY) then
    Result := ''
  else
    Result := EditWAVFilename.Text
end;

//------------------------------------------------------------------------------

procedure TProjectForm.EditUpdateColor(Sender: TObject);
begin
  UpdateColor;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

procedure TProjectForm.VideoSourceOperation(Silent : Boolean);
var
    SourceFileOperationCancel : TSourceFileOperationCancel;
    Language, Title, CodecID, StreamID, StreamOrder : String;
    ExitCode: DWORD;
    NewVideoFileName, NewAudioWavFileName, ParameterF : String;
    TempSCFileName, TempFileLine : String;
    MkvMergePath, MkvMergePathCommand: String;
    MkvExtractPath, MkvExtractPathCommand, NewSubtitleFile, TextStreamCount: String;
    MediaInfoHandle : Cardinal;
    FFMpegPath, FFMpegPathCommand : String;
    SubtitleParametersCommand : String;
    I, OrdinalSub, OrdinalFFMpeg: Integer;
    resultMd5Calculation : String;
    ExitForced, PeekFileGenerated : Boolean;
    Duration : String;
    DurationMs : LongWord;
    HH,MM,SS : Integer;
    SC_temp, SC_final : TextFile;
    SC_time : TDateTime;
    SC_current, SC_prev, score_current, score_prev : Extended;
    SC_FS : TFormatSettings;
    ep : Integer;
    FrameCorrection : Extended;
begin

 VideoSourceOperationExecute.Enabled := False;

 ExitForced := False;

 MediaInfoHandle := 0;
 if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
  begin
    MediaInfoHandle := MediaInfo_New();
    MediaInfo_Open(MediaInfoHandle, PWideChar(EditVideoFilename.Text));
    Duration := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Duration/String3', Info_Text, Info_Name);
    if (Duration = '') then
      Duration := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Duration/String3', Info_Text, Info_Name);
    HH := StrToInt(copy(Duration,1,2));
    MM := StrToInt(copy(Duration,4,2));
    SS := StrToInt(copy(Duration,7,2));
    DurationMs := (HH*3600+MM*60+SS)*1000;
  end;

 if VideoSourceOperationGeneratePeakFile.Checked then
 begin
  try
   VideoSourceOperationExecute.Enabled := False;
   FFMpegPath := uppercase(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\ffmpeg.exe');
   NewAudioWavFileName := ChangeFileExt(EditVideoFilename.Text,'') + '.wav';

   FFMpegPathCommand := Format('-y -i "%s" -map 0:a:%s -map_metadata -1 -ac 1 -ar 8k "%s"',[EditVideoFilename.Text,IntToStr(StrToInt(copy(cbbVideoSourceOperationAudioTracks.Text,1,2))-1),NewAudioWavFileName]);
   if VideoSourceOperationAudioTracksOnlyCenter.checked then
   begin
    FFMpegPathCommand := Format('-y -i "%s" -map 0:a:%s -map_metadata -1 -af "pan=mono|c0=FC" -ac 1 -ar 8k "%s"',[EditVideoFilename.Text,IntToStr(StrToInt(copy(cbbVideoSourceOperationAudioTracks.Text,1,2))-1),NewAudioWavFileName]);
   end;

   Screen.Cursor:=crHourglass;

   SourceFileOperationCancel := TSourceFileOperationCancel.Create(self);
   SourceFileOperationCancel.FFMpegPath := FFMpegPath;
   SourceFileOperationCancel.FFMpegPathCommand := FFMpegPathCommand;
   SourceFileOperationCancel.DurationMs := DurationMs;
   SourceFileOperationCancel.OutputFile := NewAudioWavFileName;
   SourceFileOperationCancel.GeneratePeek := True;
   SourceFileOperationCancel.ShowModal;
   if SourceFileOperationCancel.ExitForced then
    ExitForced := True
   else PeekFileGenerated := SourceFileOperationCancel.PeekFileGenerated;
   SourceFileOperationCancel.Free;

   if not ExitForced And FileExists(NewAudioWavFileName) then
   begin
    if PeekFileGenerated then
    begin
      rbPeakOnly.checked := True;
      rbInternalWAVClick(rbPeakOnly);
      EditPeakFilename.Text := WideChangeFileExt(NewAudioWavFileName,'.peak');
      DeleteFile(NewAudioWavFileName);
    end
    else
    begin
     rbExternal.checked := True;
     rbInternalWAVClick(rbExternal);
     EditWAVFilename.Text := NewAudioWavFileName;
    end;
    ProjectForm.SelectedAudioTrackOnExtract := cbbVideoSourceOperationAudioTracks.ItemIndex+1;
    bttCreateNewProject.Enabled := True;
    if Silent = true then
    begin
      MessageBoxW(Handle, PWideChar(WideString('Operation complete. The audio waveform peak file is being created.')),
       PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
    end;
   end
   else if ExitForced And FileExists(NewAudioWavFileName) then
   begin
    MessageBoxW(Handle, PWideChar(WideString('Operation aborted.')),
     PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
    DeleteFile(NewAudioWavFileName);
   end;

   Screen.Cursor:=crDefault;

  except
  end;
 end;

 if VideoSourceOperationRemux.Checked then
  begin

    try
      VideoSourceOperationExecute.Enabled := False;

      MkvMergePath := ExtractFileDir(ParamStr(0))+'\mkvtoolnix\mkvmerge.exe';

      NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '_re-muxed.mkv';

      MkvMergePathCommand := Format('-o "%s" "%s"',[NewVideoFileName,EditVideoFilename.Text]);

      SourceFileOperationCancel := TSourceFileOperationCancel.Create(self);
      SourceFileOperationCancel.MkvMergePath := MkvMergePath;
      SourceFileOperationCancel.MkvMergePathCommand := MkvMergePathCommand;
      SourceFileOperationCancel.ShowModal;
      if SourceFileOperationCancel.ExitForced then
       ExitForced := True;
      SourceFileOperationCancel.Free;

      Screen.Cursor:=crDefault;

      if not ExitForced And FileExists(NewVideoFileName) then
       begin
        EditVideoFilename.Text := NewVideoFileName;
        bttCreateNewProject.Enabled := True;
        EditVideoFilename.SetFocus;
        MessageBoxW(Handle, PWideChar(WideString('Operation complete. A new video source file is being created remuxing the original one.')),
         PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
       end
      else if ExitForced And FileExists(NewVideoFileName) then
       begin
         MessageBoxW(Handle, PWideChar(WideString('Operation aborted.')),
          PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
         DeleteFile(NewVideoFileName);
       end;

    except
     Screen.Cursor:=crDefault;
    end;

  end;

 if VideoSourceOperationRecodeAudio1.Checked Or VideoSourceOperationRecodeAudio2.Checked then
  begin

    try
      VideoSourceOperationExecute.Enabled := False;
      FFMpegPath := uppercase(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\ffmpeg.exe');

      ParameterF := '';
      if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.mkv' then
        NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '_re-encoded.mkv';
      if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.mp4' then
        NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '_re-encoded.mkv';

      if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.ts' then
       begin
        NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '_re-encoded.mkv';
       end;
      if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.m2ts' then
       begin
        NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '_re-encoded.mkv';
       end;
      if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.avi' then
       begin
        ParameterF := '-f avi';
        NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '_re-encoded.mkv';
       end;

      if VideoSourceOperationRecodeAudio1.Checked then
        begin
          FFMpegPathCommand := Format('-y -stats -i "%s" %s -c:v copy -c:a pcm_alaw -ac 1 -c:s copy "%s"',[EditVideoFilename.Text,ParameterF,NewVideoFileName]);
        end;
      if VideoSourceOperationRecodeAudio2.Checked then
        begin
          FFMpegPathCommand := Format('-y -stats -i "%s" %s -c:v copy -c:a pcm_s16le -ac 2 -c:s copy "%s"',[EditVideoFilename.Text,ParameterF,NewVideoFileName]);
        end;

      Screen.Cursor:=crHourglass;

      SourceFileOperationCancel := TSourceFileOperationCancel.Create(self);
      SourceFileOperationCancel.FFMpegPath := FFMpegPath;
      SourceFileOperationCancel.FFMpegPathCommand := FFMpegPathCommand;
      SourceFileOperationCancel.DurationMs := DurationMs;
      SourceFileOperationCancel.ShowModal;
      if SourceFileOperationCancel.ExitForced then
       ExitForced := True;
      SourceFileOperationCancel.Free; 

      Screen.Cursor:=crDefault;

      if not ExitForced And FileExists(NewVideoFileName) then
       begin
        EditVideoFilename.Text := NewVideoFileName;
        bttCreateNewProject.Enabled := True;
        MessageBoxW(Handle, PWideChar(WideString('Operation complete. A new video source file is being created enconding the original one.')),
         PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
       end
      else if ExitForced And FileExists(NewVideoFileName) then
       begin
         MessageBoxW(Handle, PWideChar(WideString('Operation aborted.')),
          PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
         DeleteFile(NewVideoFileName);
       end;
    except
      Screen.Cursor:=crDefault;
    end;
  end;

 if VideoSourceOperationMD5.Checked then
  begin
    try
      VideoSourceOperationExecute.Enabled := False;

      Screen.Cursor:=crHourglass;

      resultMd5Calculation := lowercase(FileMD5Digest(EditVideoFilename.Text));

      Screen.Cursor:=crDefault;

      if MessageDlg('The MD5 checksum for the selected video source file is ' + resultMd5Calculation + chr(13) +
        'Select Yes to copy it to the clipboard', mtConfirmation,[mbYes,mbNo],0) = mrYes then
       begin
        Clipboard.AsText := resultMd5Calculation;
       end;
    except
      Screen.Cursor:=crDefault;
    end;

  end;

 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked then
  begin

    try
      VideoSourceOperationExecute.Enabled := False;

      Screen.Cursor:=crHourglass;

      FFMpegPath := uppercase(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\ffmpeg.exe');

      MediaInfoHandle := 0;
      if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
       begin
         MediaInfoHandle := MediaInfo_New();
         MediaInfo_Open(MediaInfoHandle, PWideChar(EditVideoFilename.Text));
         TextStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Text, 0, 'StreamCount', Info_Text, Info_Name);
         if (TextStreamCount <> '') And (StrToInt(TextStreamCount) > 0) Then
          begin
            SubtitleParametersCommand := '';
            For I := 0 to StrToInt(TextStreamCount)-1 do
             begin
              OrdinalSub := I + 1;
              OrdinalFFMpeg := I;

              if RightPad(IntToStr(OrdinalFFMpeg),'0',2) <> Copy(cbbVideoSourceOperationTextTracks.Text,1,2) Then
                Continue;

              StreamOrder := MediaInfo_Get(MediaInfoHandle, Stream_Text, OrdinalFFMpeg, 'StreamOrder', Info_Text, Info_Name);
              StreamID := MediaInfo_Get(MediaInfoHandle, Stream_Text, OrdinalFFMpeg, 'ID', Info_Text, Info_Name);
              CodecID := MediaInfo_Get(MediaInfoHandle, Stream_Text, OrdinalFFMpeg, 'CodecID', Info_Text, Info_Name);
              Language := MediaInfo_Get(MediaInfoHandle, Stream_Text, OrdinalFFMpeg, 'Language/String', Info_Text, Info_Name);
              Title := MediaInfo_Get(MediaInfoHandle, Stream_Text, OrdinalFFMpeg, 'Title', Info_Text, Info_Name);

              if I > 0 then SubtitleParametersCommand := SubtitleParametersCommand + ' ';
              SubtitleParametersCommand := SubtitleParametersCommand + StreamOrder + ':"' +
                ChangeFileExt(EditVideoFilename.Text,'') + '-subtitle' + RightPad(IntToStr(OrdinalSub),'0',2);

              NewSubtitleFile := ChangeFileExt(EditVideoFilename.Text,'') + '-subtitle_';

              if Title <> '' then
                begin
                  SubtitleParametersCommand := SubtitleParametersCommand + '_' + Title;
                  NewSubtitleFile := NewSubtitleFile + '_' + Title;
                end
              else
                if Language <> '' then
                  begin
                    SubtitleParametersCommand := SubtitleParametersCommand + '_' + Language;
                    NewSubtitleFile := NewSubtitleFile + '_' + Language;
                  end;
              if CodecID = 'S_TEXT/ASS' then
                begin
                  SubtitleParametersCommand := SubtitleParametersCommand + '.ass';
                  NewSubtitleFile := NewSubtitleFile + '.ass';
                end;
              if CodecID = 'S_TEXT/UTF8' then
                begin
                  SubtitleParametersCommand := SubtitleParametersCommand + '.srt';
                  NewSubtitleFile := NewSubtitleFile + '.srt';
                end;

              SubtitleParametersCommand := SubtitleParametersCommand + '"';

              break;

             end;

            FFMpegPathCommand := Format('-y -stats -i "%s" -map 0:s:%s "%s"',[EditVideoFilename.Text,IntToStr(OrdinalFFMpeg),NewSubtitleFile]);

            Screen.Cursor:=crHourglass;

            SourceFileOperationCancel := TSourceFileOperationCancel.Create(self);
            SourceFileOperationCancel.FFMpegPath := FFMpegPath;
            SourceFileOperationCancel.FFMpegPathCommand := FFMpegPathCommand;
            SourceFileOperationCancel.DurationMs := DurationMs;
            SourceFileOperationCancel.ShowModal;
            if SourceFileOperationCancel.ExitForced then
             ExitForced := True;
            SourceFileOperationCancel.Free;

            Screen.Cursor:=crDefault;

            if not ExitForced And FileExists(NewSubtitleFile) then
              begin
                if VideoSourceOperationSetSubtitleFile.Checked Then
                  EditSubtitleFilename.Text := NewSubtitleFile;
                if VideoSourceOperationSetSubtitleVO.Checked Then
                  EditSubtitleVO.Text := NewSubtitleFile;
                if CodecID = 'S_TEXT/UTF8' then cbSubtitleFormat.ItemIndex := 0;
                if CodecID = 'S_TEXT/ASS' then cbSubtitleFormat.ItemIndex := 2;
                bttCreateNewProject.Enabled := True;
                if VideoSourceOperationSetSubtitleFile.Checked Then
                  MessageBoxW(Handle, PWideChar(WideString('Operation complete. A subtitle is being extracted from the video source file.')),
                    PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
                if VideoSourceOperationSetSubtitleVO.Checked Then
                  MessageBoxW(Handle, PWideChar(WideString('Operation complete. A subtitle VO/Other is being extracted from the video source file.')),
                    PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
              end
            else if ExitForced And FileExists(NewSubtitleFile) then
             begin
               MessageBoxW(Handle, PWideChar(WideString('Operation aborted.')),
               PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
               DeleteFile(NewSubtitleFile);
             end;

          end;
         MediaInfo_Close(MediaInfoHandle);
       end;
    except
      Screen.Cursor:=crHourglass;
    end;
  end;

  if VideoSourceOperationSceneChange.Checked then
  begin
    try
      VideoSourceOperationExecute.Enabled := False;
      FFMpegPath := uppercase(ExtractFileDir(ParamStr(0)) + '\mkvtoolnix\ffmpeg.exe');
      TempSCFileName := ExtractFileName(ChangeFileExt(EditVideoFilename.Text, '')) + '.scenechange.temp';
      FFMpegPathCommand := Format('-i "%s" -vf select=''gt(scene,0.1)'',metadata=print:file="%s" -an -f null -',
                                  [EditVideoFilename.Text, TempSCFileName]);

      MediaInfoHandle := 0;
      if (MediaInfoDLL_Load('MediaInfo.dll') = true) then
      begin
        MediaInfoHandle := MediaInfo_New();
        MediaInfo_Open(MediaInfoHandle, PWideChar(EditVideoFilename.Text));
        //using TextStreamCount instead of creating a new variable (intended as VideoStreamCount)
        TextStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'StreamCount', Info_Text, Info_Name);

        //get single frame time from fps
        // xvid encoded videos get pts_time shifted by 1 frame
        // probably video stream xvid start at frame 1, x264 start at frame 0 ???
        // so a correction of pts_time is necessary if xvid is found
        FrameCorrection := 0;
        if (WideLowerCase(MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Codec/String', Info_Text, Info_Name)) = 'xvid') then
        begin
          SC_FS.DecimalSeparator := '.';
          FrameCorrection := StrToFloat(MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'FrameRate', Info_Text, Info_Name), SC_FS);
          FrameCorrection := 1 / FrameCorrection;
        end;

        MediaInfo_Close(MediaInfoHandle);
        if (TextStreamCount <> '') and (StrToInt(TextStreamCount) > 0) then
        begin
          Screen.Cursor := crHourglass;
          SourceFileOperationCancel := TSourceFileOperationCancel.Create(self);
          SourceFileOperationCancel.FFMpegPath := FFMpegPath;
          SourceFileOperationCancel.FFMpegPathCommand := FFMpegPathCommand;
          SourceFileOperationCancel.DurationMs := DurationMs;
          SourceFileOperationCancel.ShowModal;

          if SourceFileOperationCancel.ExitForced then
            ExitForced := True;

          SourceFileOperationCancel.Free;
        end
        else
        begin
          ExitForced := True;
          MessageBoxW(Handle,
            PWideChar(WideString('No video stream found in this file. Scene-change can''t be generated.')),
            PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
        end;
      end;

      if not ExitForced and FileExists(TempSCFileName) then
      begin
        TempSCFileName := ChangeFileExt(TempSCFileName, '');

        if FileExists(TempSCFileName) then
          DeleteFile(TempSCFileName);

        AssignFile(SC_temp, TempSCFileName + '.temp');
        AssignFile(SC_final, TempSCFileName);
        Reset(SC_temp);
        ReWrite(SC_final);
        WriteLn(SC_final, 'SceneChangeFormatVersion=1');

        SC_FS.DecimalSeparator := '.';
        score_prev := -1;
        SC_prev := -1;
        while not Eof(SC_temp) do
        begin
          ReadLn(SC_temp, TempFileLine);
          ep := AnsiPos('pts_time:', TempFileLine);
          if ep > 0 then
          begin
            TempFileLine := Trim(Copy(TempFileLine, ep + 9, Length(TempFileLine)));
            SC_current := StrToFloat(TempFileLine, SC_FS) - FrameCorrection ;

            ReadLn(SC_temp, TempFileLine);
            ep := AnsiPos('scene_score=', TempFileLine);
            if ep > 0 then
            begin
              TempFileLine := Trim(Copy(TempFileLine, ep + 12, Length(TempFileLine)));
              score_current := StrToFloat(TempFileLine, SC_FS);

              //a bit of cleaning of very close scenechange
              //based on the scene_score value to determine
              //which one to keep
              if (SC_current - 0.1) < SC_prev then
              begin
                if score_current < score_prev then
                begin
                  SC_time := SC_prev / SecsPerDay;
                  WriteLn(SC_final, formatdatetime('hh:nn:ss.zzz', SC_time));
                end;
                Continue;
              end;
              SC_time := SC_prev / SecsPerDay;
              WriteLn(SC_final, formatdatetime('hh:nn:ss.zzz', SC_time));
              SC_prev := SC_current;
              score_prev := score_current;
            end;
          end;
        end;

        CloseFile(SC_temp);
        CloseFile(SC_final);

        if FileExists(TempSCFileName + '.temp') then
          DeleteFile(TempSCFileName + '.temp');

        MessageBoxW(Handle,
          PWideChar(WideString('Operation complete. A scene-change file has been generated.')),
          PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
      end;
      Screen.Cursor := crDefault;
    except
    end;
  end;

 VideoSourceOperationNone.Checked := True;
 VideoSourceOperationNoneClick(Nil);
end;

procedure TProjectForm.VideoSourceOperationRemuxClick(Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := True;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked Then
  begin
    cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.VideoSourceOperationRecodeAudio1Click(
  Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := True;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked Then
  begin
   cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.VideoSourceOperationRecodeAudio2Click(
  Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := True;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked Then
  begin
    cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.VideoSourceOperationMD5Click(Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := True;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked Then
  begin
    cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.VideoSourceOperationSetSubtitleFileClick(
  Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := True;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked Then
  begin
    cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.VideoSourceOperationSetSubtitleVOClick(
  Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := True;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
  begin
    cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.VideoSourceOperationGeneratePeakFileClick(
  Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := True;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 VideoSourceOperationAudioTracksOnlyCenter.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
   VideoSourceOperationAudioTracksOnlyCenter.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked Then
  begin
    cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.cbbVideoSourceOperationAudioTracksSelect(
  Sender: TObject);
var MediaInfoHandle : Cardinal;
  OrdinalSub : Integer; Layout:WideString;
begin
  MediaInfoHandle := 0;
  OrdinalSub := StrToInt(copy(cbbVideoSourceOperationAudioTracks.Text,1,2))-1;
  if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
   begin
     MediaInfoHandle := MediaInfo_New();
     MediaInfo_Open(MediaInfoHandle, PWideChar(EditVideoFilename.Text));
     Layout := MediaInfo_Get(MediaInfoHandle, Stream_Audio, OrdinalSub, 'ChannelLayout', Info_Text, Info_Name);
     if (pos(' C ', Layout) > 0) OR (pos('C', Layout) > 0) OR (pos(' C', Layout) > 0) then
       VideoSourceOperationAudioTracksOnlyCenter.Enabled := True
     else
     begin
       VideoSourceOperationAudioTracksOnlyCenter.Enabled := False;
       VideoSourceOperationAudioTracksOnlyCenter.Checked := False;
     end;
   end;
end;

procedure TProjectForm.VideoSourceOperationExecuteClick(Sender: TObject);
begin
 VideoSourceOperation(True);
end;

procedure TProjectForm.VideoSourceOperationNoneClick(Sender: TObject);
begin
 VideoSourceOperationExecute.Enabled := False;
 cbbVideoSourceOperationAudioTracks.Visible:= False;
 VideoSourceOperationAudioTracksOnlyCenter.Visible:= False;
 if VideoSourceOperationGeneratePeakFile.Checked Then
 begin
   cbbVideoSourceOperationAudioTracks.Visible:= True;
   VideoSourceOperationAudioTracksOnlyCenter.Visible:= True;
 end;
 cbbVideoSourceOperationTextTracks.Visible:= False;
 if VideoSourceOperationSetSubtitleFile.Checked Or VideoSourceOperationSetSubtitleVO.Checked Then
  begin
    cbbVideoSourceOperationTextTracks.Visible:= True;
  end;
end;

procedure TProjectForm.FormCreate(Sender: TObject);
begin
  if Screen.WorkAreaHeight < ProjectForm.Height then
  begin
    ProjectForm.Height := Screen.WorkAreaHeight;
    //to avoid orizontal scroll-bars 
    ProjectForm.Width := ProjectForm.Width + 20;
    ProjectForm.BorderStyle := bsSizeable;
  end;
end;

end.
//------------------------------------------------------------------------------
