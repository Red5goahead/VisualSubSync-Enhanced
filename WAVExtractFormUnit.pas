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

unit WAVExtractFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, Renderer, ExtCtrls, MediaInfoDll;

type
  TExtractWAVForm = class(TForm)
    TntGroupBox1: TTntGroupBox;
    MemoVideoInfo: TTntMemo;
    bttExtract: TTntButton;
    bttClose: TTntButton;
    ProgressBar1: TProgressBar;
    gbSettings: TTntGroupBox;
    TntLabelStreamToExtract: TTntLabel;
    cbStreamIndex: TComboBox;
    TntLabel3: TTntLabel;
    Timer1: TTimer;
    bttStop: TTntButton;
    rbFastConversion: TRadioButton;
    rbNoConversion: TRadioButton;
    rbOnlyPeak: TRadioButton;
    bttDebug: TTntButton;
    procedure bttExtractClick(Sender: TObject);
    procedure bttCloseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure bttStopClick(Sender: TObject);
    procedure rbOnlyPeakClick(Sender: TObject);
    procedure bttDebugClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    DSWavExtractor : TDSWavExtractor;
    AudioPinIsSelected : Boolean;
    CurrentExtractionType : TWAVExtractionType;
    SuccessfullExtraction : Boolean;

    procedure CancelAndClose;
  public
    { Public declarations }
    VideoFilename : WideString;
    DestinationFilename : WideString;
    procedure SetExtractionType(extType : TWAVExtractionType);
  end;

implementation

{$R *.dfm}

uses CursorManager, GlobalUnit,
  main;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.bttExtractClick(Sender: TObject);
begin
  MemoVideoInfo.Lines.Add('Extracting stream '+ IntToStr(cbStreamIndex.ItemIndex+1) + ', please wait...');
  bttExtract.Enabled := False;

  if cbStreamIndex.Enabled = true then
  begin
    DSWavExtractor.Close;
    DSWavExtractor.Open(VideoFilename, cbStreamIndex.ItemIndex + 1);
    g_WavExtractorGraphDebugInfo := DSWavExtractor.GetFiltersAsString;
  end;

  bttStop.Enabled := True;
  DSWavExtractor.DestinationFilename := DestinationFilename;
  DSWavExtractor.WAVExtractionType := CurrentExtractionType;
  DSWavExtractor.SelectAudioPin(cbStreamIndex.ItemIndex);
  AudioPinIsSelected := True;
  SuccessfullExtraction := False;

  g_WavExtractorGraphDebugInfo := DSWavExtractor.GetFiltersAsString;

  DSWavExtractor.Go;
  Timer1.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.CancelAndClose;
begin
  Timer1.Enabled := False;
  if Assigned(DSWavExtractor) then
  begin
    FreeAndNil(DSWavExtractor);
  end;
  ModalResult := mrCancel;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.bttCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.SetExtractionType(extType : TWAVExtractionType);
begin
  CurrentExtractionType := extType;
  case extType of
    wetOnlyPeakFile : rbOnlyPeak.Checked := True;
    wetFastConversion : rbFastConversion.Checked := True;
    wetNoConversion : rbNoConversion.Checked := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.FormActivate(Sender: TObject);
var i : integer;
    CM : ICursorManager;
    MediaInfoHandle : Cardinal;
    Stream_Video0:string; VideoStreamCount:String;
    Stream_Audio0:string; Stream_Audio1:string; AudioStreamCount:String;
    Stream_Text0:string; TextStreamCount:String;
    Language : String;

  function ExecAndWait(const FileName, Params: string;
    WindowState: Word): Boolean;
  var
    SUInfo: TStartupInfo;
    ProcInfo: TProcessInformation;
    CmdLine: string;
  begin
    { Enclose filename in quotes to take care of
      long filenames with spaces. }
    CmdLine := '"' + FileName + '" ' + Params;
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

begin
  CM := TCursorManager.Create(crHourGlass);

  SuccessfullExtraction := False;
  bttExtract.Enabled := False;
  bttStop.Enabled := False;
  bttClose.Enabled := False;
  gbSettings.Enabled := False;
  AudioPinIsSelected := False;

  // Get video info

  MemoVideoInfo.Clear;
  DSWavExtractor := TDSWavExtractor.Create;
  MemoVideoInfo.Lines.Add('Opening and analyzing file :');
  MemoVideoInfo.Lines.Add(VideoFilename);
  MemoVideoInfo.Lines.Add('Please wait...');
  Application.ProcessMessages;

  MediaInfoHandle := 0;
  if (MediaInfoDLL_Load('MediaInfo.dll') = True) Then
   begin
    MediaInfoHandle := MediaInfo_New();
    MediaInfo_Open(MediaInfoHandle, @VideoFilename[1]);
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name))='PCM') then
     DSWavExtractor.IsPcmAudio := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name))='AC-3') OR
       (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Format', Info_Text, Info_Name))='AC-3') OR
       (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name))='E-AC-3') OR
       (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Format', Info_Text, Info_Name))='E-AC-3') then
     DSWavExtractor.IsAc3Audio := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name))='DTS') OR
       (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Format', Info_Text, Info_Name))='DTS') then
     DSWavExtractor.IsDtsAudio := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name))='AAC') OR
       (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Format', Info_Text, Info_Name))='AAC') then
     DSWavExtractor.IsAacAudio := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name))='FLAC') OR
       (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Format', Info_Text, Info_Name))='FLAC') then
     DSWavExtractor.IsFlacAudio := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name))='MPEG AUDIO') OR
       (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Format', Info_Text, Info_Name))='MPEG AUDIO') then
     DSWavExtractor.IsMpegAudio := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name))='MATROSKA') then
     DSWavExtractor.IsMatroskaContainer := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name))='AVI') then
     DSWavExtractor.IsAviContainer := true;
    if ( (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name))='MPEG-4') OR
         (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name))='QUICKTIME') ) then
     DSWavExtractor.IsMp4Container := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name))='FlASH VIDEO') then
     DSWavExtractor.IsFlvContainer := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name))='MPEG-PS') then
     DSWavExtractor.IsMpegContainer := true;
    if (uppercase(MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name))='MPEG-TS') then
     DSWavExtractor.IsMpegContainer := true;
   end;

  DSWavExtractor.Open(VideoFilename, cbStreamIndex.ItemIndex + 1);
  g_WavExtractorGraphDebugInfo := DSWavExtractor.GetFiltersAsString;
  MemoVideoInfo.Lines.Add('');
  MemoVideoInfo.Lines.Add(IntToStr(DSWavExtractor.AudioStreamCount) + ' audio stream found.');
  MemoVideoInfo.Lines.Add('');

  if MediaInfoHandle <> 0 then
   begin
    MemoVideoInfo.Lines.Add('Container: ' + MediaInfo_Get(MediaInfoHandle, Stream_General, 0, 'Format', Info_Text, Info_Name));

    VideoStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'StreamCount', Info_Text, Info_Name);
    Stream_Video0:= 'Video: ' + MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Width', Info_Text, Info_Name) + 'x' +
        MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Height', Info_Text, Info_Name) + ' ' +
        MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'FrameRate/String', Info_Text, Info_Name) + ' ' +
       MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Codec/String', Info_Text, Info_Name) + ' ' +
       MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'DisplayAspectRatio/String', Info_Text, Info_Name) + ' (' +
       MediaInfo_Get(MediaInfoHandle, Stream_Video, 0, 'Duration/String2', Info_Text, Info_Name) + ')';
    if VideoStreamCount > '' then
      MemoVideoInfo.Lines.Add(Stream_Video0)
    else MemoVideoInfo.Lines.Add('Video: no video');

    Language := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Language/String', Info_Text, Info_Name);
    if Language > '' then Language := '[' + Language + '] ';
    AudioStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'StreamCount', Info_Text, Info_Name);
    Stream_Audio0 := 'Audio: ' + MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Format', Info_Text, Info_Name) + ' ' +
      MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'Channel(s)/String', Info_Text, Info_Name) + ' ' +
      MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'BitRate/String', Info_Text, Info_Name) + ' ' +
      MediaInfo_Get(MediaInfoHandle, Stream_Audio, 0, 'SamplingRate/String', Info_Text, Info_Name) + ' ' +
      Language;
    MemoVideoInfo.Lines.Add(Stream_Audio0);
    Stream_Audio1:=MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Format', Info_Text, Info_Name);
    if Stream_Audio1>'' then
     begin
      Language := MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Language/String', Info_Text, Info_Name);
      if Language > '' then Language := '[' + Language + '] ';
      Stream_Audio1 :=
        'Audio: ' + Stream_Audio1 + ' ' +
        MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'Channel(s)/String', Info_Text, Info_Name) + ' ' +
        MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'BitRate/String', Info_Text, Info_Name) + ' ' +
        MediaInfo_Get(MediaInfoHandle, Stream_Audio, 1, 'SamplingRate/String', Info_Text, Info_Name) + ' ' +
        Language;
      MemoVideoInfo.Lines.Add(Stream_Audio1);
     end;

     TextStreamCount := MediaInfo_Get(MediaInfoHandle, Stream_Text, 0, 'StreamCount', Info_Text, Info_Name);
     if (TextStreamCount <> '') And (StrToInt(TextStreamCount) > 0) Then
      begin
        For I := 0 to StrToInt(TextStreamCount)-1 do
         begin
          if MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Title', Info_Text, Info_Name) = '' then
           begin
              MemoVideoInfo.Lines.Add('Subtitle: ' +
                MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'CodecID', Info_Text, Info_Name)+ ' ' +
                MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Language/String', Info_Text, Info_Name));
           end
          else
           begin
              MemoVideoInfo.Lines.Add('Subtitle: ' + MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Title', Info_Text, Info_Name) + ' ' +
                MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'CodecID', Info_Text, Info_Name)+ ' ' +
                    MediaInfo_Get(MediaInfoHandle, Stream_Text, I, 'Language/String', Info_Text, Info_Name));
           end;
         end;
      end;

    MemoVideoInfo.Lines.Add('');

    MediaInfo_Close(MediaInfoHandle);

   end;

  cbStreamIndex.Clear;
  for i := 1 to DSWavExtractor.AudioStreamCount do
  begin
    if i = 1 then cbStreamIndex.AddItem(IntToStr(i) + ':' + Stream_Audio0,nil);
    if i = 2 then cbStreamIndex.AddItem(IntToStr(i) + ':' + Stream_Audio1,nil);
    if i > 2 then cbStreamIndex.AddItem(IntToStr(i),nil);
  end;
  if (DSWavExtractor.AudioStreamCount > 0) then
    cbStreamIndex.ItemIndex := 0;
  if (DSWavExtractor.AudioStreamCount = 1) then cbStreamIndex.Enabled := false
   else cbStreamIndex.Enabled := true;

  bttExtract.Enabled := (DSWavExtractor.AudioStreamCount > 0);
  bttClose.Enabled := True;
  gbSettings.Enabled := True;

  if pos('lav splitter source',lowercase(g_WavExtractorGraphDebugInfo))=0 then
   begin
    TntLabelStreamToExtract.Visible := True;
    cbStreamIndex.Visible := True;
    MemoVideoInfo.Lines.Add('Select a stream and press the ''Extract'' button.');
   end
  else
   begin
    MemoVideoInfo.Lines.Add('Audio stream will be selected according to preferred languages.');
   end;

end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.Position := DSWavExtractor.GetProgress;
  if DSWavExtractor.IsFinished then
  begin
    Timer1.Enabled := False;
    DSWavExtractor.Close;
    bttExtract.Enabled := True;
    bttStop.Enabled := False;
    MemoVideoInfo.Lines.Add('Extraction finished successfully.');
    ProgressBar1.Position := 0;

    if Assigned(DSWavExtractor) then
    begin
      FreeAndNil(DSWavExtractor);
    end;
    SuccessfullExtraction := True;
    ModalResult := mrOk;
  end;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.bttStopClick(Sender: TObject);
begin
  Timer1.Enabled := False;
  DSWavExtractor.Close;  
  bttExtract.Enabled := True;
  bttStop.Enabled := False;
  MemoVideoInfo.Lines.Add('Extraction aborted by user.');
  ProgressBar1.Position := 0;
  SuccessfullExtraction := False;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.rbOnlyPeakClick(Sender: TObject);
begin
  CurrentExtractionType := TWAVExtractionType(TComponent(Sender).Tag);
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.bttDebugClick(Sender: TObject);
var slist : TStringList;
    i : Integer;
begin
  MemoVideoInfo.Lines.Add('--- Filters list ---');

  if AudioPinIsSelected then
  begin
    DSWavExtractor.Close;
    DSWavExtractor.Open(VideoFilename, cbStreamIndex.ItemIndex + 1);
  end;

  DSWavExtractor.DestinationFilename := DestinationFilename;
  DSWavExtractor.WAVExtractionType := CurrentExtractionType;
  DSWavExtractor.SelectAudioPin(cbStreamIndex.ItemIndex);
  AudioPinIsSelected := True;

  slist := TStringList.Create;
  DSWavExtractor.GetFilters(slist);
  for i:=0 to slist.Count-1 do
  begin
    MemoVideoInfo.Lines.Add(slist[i]);
  end;
  slist.Free;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.FormCreate(Sender: TObject);
begin
  SuccessfullExtraction := False;
  SetExtractionType(wetOnlyPeakFile);
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not SuccessfullExtraction then
    CancelAndClose;
end;

// -----------------------------------------------------------------------------

procedure TExtractWAVForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

// -----------------------------------------------------------------------------

end.
// -----------------------------------------------------------------------------
