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

unit SourceFileOperationCancel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,
  StdCtrls, TntStdCtrls,
  ExtCtrls, TntComCtrls, WAVDisplayerUnit;

type
  TSourceFileOperationCancel = class(TForm)
    VideoSourceOperationExecuteCancel: TTntButton;
    StartTimer: TTimer;
    FFMpegOutputProgressBar: TTntProgressBar;
    FFMpegOutputMessage: TLabel;
    FakeWavDisplayerPanel: TPanel;
    procedure StartTimerTimer(Sender: TObject);
    procedure VideoSourceOperationExecuteCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
  private
    { Private declarations }
    WAVDisplayer : TWAVDisplayer;
    FlagCancelExecuteFFMpeg : Bool;
    Procedure ExecuteFFMpeg(const Command: string; const Parameters: string; TotalDuration: LongWord; const Timeout: DWORD);
    Procedure ExecuteMkvMerge(const Command: string; const Parameters: string; const Timeout: DWORD);
  public
    { Public declarations }
    ExitForced : Boolean;
    FFMpegPath, MkvMergePath : String;
    FFMpegPathCommand, MkvMergePathCommand : String;
    GeneratePeek, PeekFileGenerated : Boolean;
    OutputFile : String;
    DurationMs : LongWord;
  end;

implementation

Uses ProjectUnit;

{$R *.dfm}

function GetFileSizeEx(hFile: THandle; var FileSize: Int64): BOOL; stdcall;
  external kernel32;

Procedure TSourceFileOperationCancel.ExecuteFFMpeg(const Command: string; const Parameters: string;
  TotalDuration : LongWord; const Timeout: DWORD);

const
  InheritHandleSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(TSecurityAttributes); bInheritHandle: True);

var
  hReadStdout, hWriteStdout: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  FileSize: Int64;
  AnsiBuffer: array [0 .. 1024 - 1] of AnsiChar;
  TimePosition : LongWord;
  HH,MM,SS : Integer;
  LoadWAVOK : Boolean;

begin
  FlagCancelExecuteFFMpeg := False;
  PeekFileGenerated := False;

  Win32Check(CreatePipe(hReadStdout, hWriteStdout,
    @InheritHandleSecurityAttributes, 0));
  try
    //si := Default (TStartupInfo);
    FillChar(si, SizeOf(TStartupInfo), 0);
    si.lpTitle := PChar('FFMpeg');
    si.cb := SizeOf(TStartupInfo);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hWriteStdout;
    si.hStdError := hWriteStdout;
    Win32Check(CreateProcess(nil, PChar(Command + ' ' + Parameters), nil, nil,
      True, CREATE_NO_WINDOW, nil, nil, si, pi));
    try
      while True do
      begin
        WaitRes := WaitForSingleObject(pi.hProcess, Timeout);
        Win32Check(WaitRes <> WAIT_FAILED);
        while True do
        begin
          Win32Check(GetFileSizeEx(hReadStdout, FileSize));
          if FileSize = 0 then
          begin
            break;
          end;
          Win32Check(ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1,
            BytesRead, nil));
          if BytesRead = 0 then
          begin
            break;
          end;
          AnsiBuffer[BytesRead] := #0;
          OemToAnsi(AnsiBuffer, AnsiBuffer);
          if Pos('time=', LowerCase(string(AnsiBuffer))) > 0 then
            begin
              HH := StrToInt(copy(string(AnsiBuffer),Pos('time=', LowerCase(string(AnsiBuffer)))+5,2));
              MM := StrToInt(copy(string(AnsiBuffer),Pos('time=', LowerCase(string(AnsiBuffer)))+8,2));
              SS := StrToInt(copy(string(AnsiBuffer),Pos('time=', LowerCase(string(AnsiBuffer)))+11,2));
              TimePosition := (HH*3600+MM*60+SS)*1000;
              FFMpegOutputProgressBar.Position := Round(TimePosition / TotalDuration * 100);
              if FFMpegOutputProgressBar.Position > 0 then FFMpegOutputProgressBar.Visible := True;
              FFMpegOutputMessage.Caption := string(AnsiBuffer);
            end;
          Application.ProcessMessages;
        end;
        if (WaitRes = WAIT_OBJECT_0) Or FlagCancelExecuteFFMpeg then
        begin
          break;
        end;
      end;
    finally
      TerminateProcess(pi.hProcess,0);
      TerminateProcess(pi.hThread,0);
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    CloseHandle(hReadStdout);
    CloseHandle(hWriteStdout);
  end;

  ExitForced := False;
  if FlagCancelExecuteFFMpeg then
  begin
    ExitForced := True;
  end
  else
  if GeneratePeek = True then
    begin
     FFMpegOutputMessage.Caption := 'Creating peek file, please wait...';
     Application.ProcessMessages;
     WAVDisplayer := TWAVDisplayer.Create(nil);
     WAVDisplayer.Left := 0;
     WAVDisplayer.Top := 0;
     WAVDisplayer.Width := FakeWavDisplayerPanel.Width;
     WAVDisplayer.Height := FakeWavDisplayerPanel.Height - 12;
     WAVDisplayer.Align := alClient;
     WAVDisplayer.Parent := FakeWavDisplayerPanel;
     if WAVDisplayer.LoadWAV(Outputfile) then
     begin
      PeekFileGenerated := True;
     end;
     WAVDisplayer.Free;
  end;

  Close;
end;

Procedure TSourceFileOperationCancel.ExecuteMkvMerge(const Command: string; const Parameters: string; const Timeout: DWORD);

const
  InheritHandleSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(TSecurityAttributes); bInheritHandle: True);

var
  hReadStdout, hWriteStdout: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  FileSize: Int64;
  AnsiBuffer: array [0 .. 1024 - 1] of AnsiChar;
  ExitForced : Boolean;
  Progress : Integer;
  ProgressOutput : String;

begin
  FlagCancelExecuteFFMpeg := False;
  ExitForced := False;

  Win32Check(CreatePipe(hReadStdout, hWriteStdout,
    @InheritHandleSecurityAttributes, 0));
  try
    //si := Default (TStartupInfo);
    FillChar(si, SizeOf(TStartupInfo), 0);
    si.lpTitle := PChar('MkvMerge');
    si.cb := SizeOf(TStartupInfo);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hWriteStdout;
    si.hStdError := hWriteStdout;
    Win32Check(CreateProcess(nil, PChar(Command + ' ' + Parameters), nil, nil,
      True, CREATE_NO_WINDOW, nil, nil, si, pi));
    try
      while True do
      begin
        WaitRes := WaitForSingleObject(pi.hProcess, Timeout);
        Win32Check(WaitRes <> WAIT_FAILED);
        while True do
        begin
          Win32Check(GetFileSizeEx(hReadStdout, FileSize));
          if FileSize = 0 then
          begin
            break;
          end;
          Win32Check(ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1,
            BytesRead, nil));
          if BytesRead = 0 then
          begin
            break;
          end;
          AnsiBuffer[BytesRead] := #0;
          OemToAnsi(AnsiBuffer, AnsiBuffer);
          if (length(string(AnsiBuffer)) < 15) AND (Pos('progress:', LowerCase(string(AnsiBuffer))) > 0) then
            begin
              try
               ProgressOutput := LowerCase(string(AnsiBuffer));
               ProgressOutput := StringReplace(ProgressOutput,'progress:','',[rfIgnoreCase]);
               ProgressOutput := StringReplace(ProgressOutput,'%','',[rfIgnoreCase]);
               Progress := StrToInt(trim(ProgressOutput));
               FFMpegOutputProgressBar.Position := Round(Progress / 100 * 100);
              except
              end;
              if FFMpegOutputProgressBar.Position > 0 then FFMpegOutputProgressBar.Visible := True;
              FFMpegOutputMessage.Caption := string(AnsiBuffer);
            end;
          Application.ProcessMessages;
        end;
        if (WaitRes = WAIT_OBJECT_0) Or FlagCancelExecuteFFMpeg then
        begin
          FFMpegOutputProgressBar.Visible := False;
          break;
        end;
      end;
    finally
      TerminateProcess(pi.hProcess,0);
      TerminateProcess(pi.hThread,0);
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    CloseHandle(hReadStdout);
    CloseHandle(hWriteStdout);
  end;
  ExitForced := False;
  if FlagCancelExecuteFFMpeg then
  begin
    ExitForced := True;
  end;
  Close;
end;

procedure TSourceFileOperationCancel.StartTimerTimer(Sender: TObject);
begin
 StartTimer.Enabled := False;
 if (FFMpegPath <> '') AND (FFMpegPathCommand <> '') then
   ExecuteFFMpeg(FFMpegPath, FFMpegPathCommand, DurationMs, 100);
 if (MkvMergePath <> '') AND (MkvMergePathCommand <> '') then
   ExecuteMkvMerge(MkvMergePath, MkvMergePathCommand, 100);
end;

procedure TSourceFileOperationCancel.VideoSourceOperationExecuteCancelClick(
  Sender: TObject);
begin
  FlagCancelExecuteFFMpeg := True;
end;

procedure TSourceFileOperationCancel.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := caFree;
end;

end.


