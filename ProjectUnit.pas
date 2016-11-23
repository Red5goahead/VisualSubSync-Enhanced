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
  TntButtons;

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
    cbbVideoSourceOperation: TComboBox;
    bttVideoSourceOperation: TSpeedButton;
    chkSaveAsUTF8: TTntCheckBox;
    chkLaunchTraslateActionOnCreate: TTntCheckBox;
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
    procedure cbbVideoSourceOperationSelect(Sender: TObject);
    procedure bttVideoSourceOperationClick(Sender: TObject);
  private
    { Private declarations }
    procedure WAVSelectMode(WavMode : TProjectWAVMode);
    procedure UpdateFormatCombobox;
    function ShowExtractForm(extType : TWAVExtractionType) : Boolean;
    function CheckData(AskForExtraction : Boolean) : Boolean;
    procedure UpdateColor;
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
     MD5,ClipBrd;

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
   if ProjectFileIni.ReadString('VisualSubsync','SkippedSub' + Format('%.3d',[PrgSkippedOnErrorCheck]),'') = id then
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
  MediaInfoHandle : Cardinal; TextStreamCount : WideString;
  I : Integer;
begin
  cbbVideoSourceOperation.Visible := False;
  bttVideoSourceOperation.Visible := False;
  bttVideoSourceOperation.Enabled := False;
  cbbVideoSourceOperation.Clear();
  bttExtractWAVFromVideo.Top := 50;
  TntOpenDialogBrowseGenericFile.FileName := EditVideoFilename.Text;
  TntOpenDialogBrowseGenericFile.Filter :=
    'Video files|*.AVI;*.OGM;*.MKV;*.MKA;*.MP4;*.DIVX;*.RM;' +
    '*.RMVB;*.MPEG;*.MPG;*.VOB;*.AVS;*.WMV;*.MOV;*.FLV;*.TS;*.M2TS' + '|' +
    'All files (*.*)|*.*';
  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin
    EditVideoFilename.Text := TntOpenDialogBrowseGenericFile.FileName;

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

    if ( (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mkv') OR (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mp4') OR
         (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.ts')
       )
        AND (DirectoryExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix') ) AND
       (FileExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\mkvmerge.exe') )
        then
     begin
       cbbVideoSourceOperation.Items.Add('Remux a new video source file (using Mkvmerge / tnk to MKVToolNix project)');
     end;

    if ( (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mkv') OR (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.mp4') OR
         (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.ts') OR (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.m2ts') OR
         (ExtractFileExt(TntOpenDialogBrowseGenericFile.FileName) = '.avi'))
        AND (DirectoryExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix') ) AND
       (FileExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\ffmpeg.exe') )
        then
     begin
       cbbVideoSourceOperation.Items.Add('Create a new video source file + convert audio to Pcm G.711 / mono (using FFMpeg / tnk to Zeranoe FFmpeg project)');
       cbbVideoSourceOperation.Items.Add('Create a new video source file + convert audio to Pcm / stereo (using FFMpeg / tnk to Zeranoe FFmpeg project)');
     end;

    // Check for text tracks ASS and UTF-8
    if DirectoryExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix') AND
       (FileExists(ExtractFileDir(ParamStr(0))+'\mkvtoolnix\mkvmerge.exe')) then
       begin
        MediaInfoHandle := 0;
        if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
         begin
           MediaInfoHandle := MediaInfo_New();
           MediaInfo_Open(MediaInfoHandle, @TntOpenDialogBrowseGenericFile.FileName[1]);
           TextStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Text, 0, 'StreamCount', Info_Text, Info_Name);
           if (TextStreamCount <> '') And (StrToInt(TextStreamCount) > 0) Then
            begin
              For I := 0 to StrToInt(TextStreamCount)-1 do
               begin
                if (MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Format', Info_Text, Info_Name) = 'ASS') Or
                   (MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Format', Info_Text, Info_Name) = 'UTF-8') then
                 begin
                   if StrToInt(TextStreamCount) = 1 then
                    begin
                      cbbVideoSourceOperation.Items.Add('Extract subtitle track (using Mkvextract / tnk to MKVToolNix project)');
                    end;
                   if StrToInt(TextStreamCount) > 1 then
                    begin
                      cbbVideoSourceOperation.Items.Add('Extract subtitle track [' + TextStreamCount + ']' + '(using Mkvextract / tnk to MKVToolNix project)');
                    end;
                   break;
                 end;
               end;
            end;
           MediaInfo_Close(MediaInfoHandle);
         end;
       end;

     cbbVideoSourceOperation.Items.Add('Calculate the MD5 checksum');

     bttCreateNewProject.Enabled := True;

   end;

  bttExtractWAVFromVideo.Enabled := WideFileExists(EditVideoFilename.Text);
  if cbbVideoSourceOperation.Items.Count > 0 then
    begin
      cbbVideoSourceOperation.Items.Insert(0,'Please, choose an operation to execute for the selected video source file');
      cbbVideoSourceOperation.ItemIndex := 0;
    end;
  cbbVideoSourceOperation.Visible := (cbbVideoSourceOperation.Items.Count > 0);
  bttVideoSourceOperation.Visible := (cbbVideoSourceOperation.Items.Count > 0);
  if cbbVideoSourceOperation.Visible then bttExtractWAVFromVideo.Top := 63;
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
    'All files (*.*)|*.*';
    
  Ext := WideLowerCase(WideExtractFileExt(EditSubtitleFilename.Text));
  if (Ext = '.srt') then
    TntOpenDialogBrowseGenericFile.FilterIndex := 1
  else if (Ext = '.ssa') or (Ext = '.ass') then
    TntOpenDialogBrowseGenericFile.FilterIndex := 2
  else
    TntOpenDialogBrowseGenericFile.FilterIndex := 3;
  
  SetOpenDialogPath(TntOpenDialogBrowseGenericFile);
  if TntOpenDialogBrowseGenericFile.Execute then
  begin
    EditSubtitleFilename.Text := TntOpenDialogBrowseGenericFile.FileName;
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

procedure TProjectForm.cbbVideoSourceOperationSelect(Sender: TObject);
var
   bttVideoSourceOperationPoint: TPoint;
begin
 bttVideoSourceOperation.Enabled := False;
 if cbbVideoSourceOperation.ItemIndex>0 then
  begin
   bttCreateNewProject.Enabled := False;
   bttVideoSourceOperation.Enabled := True;
   bttVideoSourceOperation.Hint := 'Start selected operation ' + #13 + #10 +
    '(' + cbbVideoSourceOperation.Text + ')';
   bttVideoSourceOperationPoint := bttVideoSourceOperation.ClientToScreen(Point(bttVideoSourceOperation.Width div 2, bttVideoSourceOperation.Height div 2));
   SetCursorPos(bttVideoSourceOperationPoint.x, bttVideoSourceOperationPoint.y);
   Application.ActivateHint(bttVideoSourceOperationPoint);
  end
 else
  begin
   bttCreateNewProject.Enabled := True;
  end;

end;

procedure TProjectForm.bttVideoSourceOperationClick(Sender: TObject);
var CM : ICursorManager;
    Language, Title, CodecID, StreamOrder : String;
    ExitCode: DWORD;
    NewVideoFileName, NewAudioFileName, ParameterF : String;
    MkvMergePath, MkvMergePathCommand: String;
    MkvExtractPath, MkvExtractPathCommand, NewSubtitleFile, TextStreamCount: String;
    MediaInfoHandle : Cardinal;
    FFMpegPath, FFMpegPathCommand : String;
    SubtitleParametersCommand : String;
    I: Integer;
    resultMd5Calculation : String;
begin
 if Pos('Remux a new video source file',cbbVideoSourceOperation.Text) >= 1 then
  begin
    if CM = nil then CM := TCursorManager.Create(crHourGlass);

    MkvMergePath := ExtractFileDir(ParamStr(0))+'\mkvtoolnix\mkvmerge.exe';

    NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '-remuxed.mkv';

    MkvMergePathCommand := Format('-o "%s" "%s"',[NewVideoFileName,EditVideoFilename.Text]);

    ExecAndWait(MkvMergePath, MkvMergePathCommand, SW_NORMAL);

    if FileExists(NewVideoFileName) then
     begin
      EditVideoFilename.Text := NewVideoFileName;
      cbbVideoSourceOperation.ItemIndex := 0;
      bttVideoSourceOperation.Enabled := False;
      bttCreateNewProject.Enabled := True;
      MessageBoxW(Handle, PWideChar(WideString('Operation complete.')),
       PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
     end;

  end;

 if Pos('Create a new video source file + convert audio',cbbVideoSourceOperation.Text) >= 1 then
  begin
    if CM = nil then CM := TCursorManager.Create(crHourGlass);

    FFMpegPath := ExtractFileDir(ParamStr(0))+'\mkvtoolnix\ffmpeg.exe';

    ParameterF := '';
    if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.mkv' then
      NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '-remuxed.mkv';
    if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.mp4' then
      NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '-remuxed.mkv';

    if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.ts' then
     begin
      NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '-remuxed.mkv';
     end;
    if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.m2ts' then
     begin
      NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '-remuxed.mkv';
     end;
    if lowercase(ExtractFileExt(EditVideoFilename.Text)) = '.avi' then
     begin
      ParameterF := '-f avi';
      NewVideoFileName := ChangeFileExt(EditVideoFilename.Text,'') + '-remuxed.mkv';
     end;

    FFMpegPathCommand := Format('-y -i "%s" %s -vcodec copy -acodec pcm_s16le -ac 2 "%s"',[EditVideoFilename.Text,ParameterF,NewVideoFileName]);
    if Pos('G.711',cbbVideoSourceOperation.Text) >= 1 then
      FFMpegPathCommand := Format('-y -i "%s" %s -vcodec copy -acodec pcm_alaw -ac 1 "%s"',[EditVideoFilename.Text,ParameterF,NewVideoFileName]);

    ExecAndWait(FFMpegPath, FFMpegPathCommand, SW_NORMAL);

    if FileExists(NewVideoFileName) then
     begin
      EditVideoFilename.Text := NewVideoFileName;
      cbbVideoSourceOperation.ItemIndex := 0;
      bttVideoSourceOperation.Enabled := False;
      bttCreateNewProject.Enabled := True;
      MessageBoxW(Handle, PWideChar(WideString('Operation complete.')),
       PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
     end;
  end;

 if Pos('Extract subtitle track',cbbVideoSourceOperation.Text) >= 1 then
  begin
    if CM = nil then CM := TCursorManager.Create(crHourGlass);

    MkvExtractPath := ExtractFileDir(ParamStr(0))+'\mkvtoolnix\mkvextract.exe';

    MediaInfoHandle := 0;
    if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
     begin
       MediaInfoHandle := MediaInfo_New();
       MediaInfo_Open(MediaInfoHandle, @TntOpenDialogBrowseGenericFile.FileName[1]);
       TextStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Text, 0, 'StreamCount', Info_Text, Info_Name);
       if (TextStreamCount <> '') And (StrToInt(TextStreamCount) > 0) Then
        begin
          CM := TCursorManager.Create(crHourGlass);
          SubtitleParametersCommand := '';
          For I := 0 to StrToInt(TextStreamCount)-1 do
           begin
            StreamOrder := MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'StreamOrder', Info_Text, Info_Name);
            CodecID := MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'CodecID', Info_Text, Info_Name);
            Language := MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Language/String', Info_Text, Info_Name);
            Title := MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Title', Info_Text, Info_Name);

            if I > 0 then SubtitleParametersCommand := SubtitleParametersCommand + ' ';
            SubtitleParametersCommand := SubtitleParametersCommand + StreamOrder + ':"' +
              ChangeFileExt(EditVideoFilename.Text,'') + '-subtitle' + IntToStr(I);

            NewSubtitleFile := ChangeFileExt(EditVideoFilename.Text,'') + '-subtitle' + IntToStr(I);

            if Title <> '' then
              begin
                SubtitleParametersCommand := SubtitleParametersCommand + '_' + Title;
                NewSubtitleFile := NewSubtitleFile + '_' + Title;
              end;
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

           end;

          MkvExtractPathCommand := Format('tracks "%s" %s',[EditVideoFilename.Text,SubtitleParametersCommand]);
          ExecAndWait(MkvExtractPath, MkvExtractPathCommand, SW_NORMAL);

          if (TextStreamCount <> '') And (StrToInt(TextStreamCount) = 1) Then
          begin
            if FileExists(NewSubtitleFile) then
              begin
                EditSubtitleFilename.Text := NewSubtitleFile;
                if CodecID = 'S_TEXT/UTF8' then cbSubtitleFormat.ItemIndex := 0;
                if CodecID = 'S_TEXT/ASS' then cbSubtitleFormat.ItemIndex := 2;
                cbbVideoSourceOperation.ItemIndex := 0;
                bttVideoSourceOperation.Enabled := False;
                bttCreateNewProject.Enabled := True;
                MessageBoxW(Handle, PWideChar(WideString('Operation complete.')),
                 PWideChar(WideString('Information')), MB_OK or MB_ICONINFORMATION);
              end;
          end;

        end;
       MediaInfo_Close(MediaInfoHandle);
     end;

  end;

  if Pos('MD5',cbbVideoSourceOperation.Text) >= 1 then
   begin
    CM := TCursorManager.Create(crHourGlass);
    try
      resultMd5Calculation := lowercase(FileMD5Digest(EditVideoFilename.Text));
      if MessageDlg('The MD5 checksum for the selected video source file is ' + resultMd5Calculation + chr(13) +
        'Select Yes to copy it to the clipboard', mtConfirmation,[mbYes,mbNo],0) = mrYes then
       begin
        Clipboard.AsText := resultMd5Calculation;
       end;
    finally
    end;
   end;

end;

end.
//------------------------------------------------------------------------------
