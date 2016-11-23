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

unit WAVFileUnit;

interface

uses Windows, Classes, MMSystem, Sysutils, Math, TntClasses;

type
  TWAVFile = class
  private
    FFS: TTntFileStream;
    FHeaderSize, FDataSize : Integer;
    FWFX: TWaveFormatEx;
    FIsOpen : Boolean;

    function GetSamplesPerSecond : Cardinal;
    function GetChannels : Cardinal;
    function GetBitsPerSample : Cardinal;
    function GetDurationMs : Cardinal;
    function GetNbsSamples : Cardinal;
  public
    destructor Destroy; override;
    function Open(Filename : WideString) : Boolean;
    function Read(Buffer : Pointer; Count : Integer) : Integer;
    procedure Close;
    function GetWaveFormatEx : PWaveFormatEx;
    procedure SeekMs(PosMs : Cardinal);
    function ExtractToStream(StartMs,StopMs : Integer; Output : TStream) : Integer;
  published
    property SamplesPerSecond : Cardinal read GetSamplesPerSecond;
    property Channels : Cardinal read GetChannels;
    property BitsPerSample : Cardinal read GetBitsPerSample;
    property Duration : Cardinal read GetDurationMs;
    property SamplesCount : Cardinal read GetNbsSamples;
    property IsOpen : Boolean read FIsOpen;
  end;

  TWAVRenderer = class
  private
    FWaveOutHandle : HWAVEOUT;
    FHeaders : array[0..4] of WAVEHDR;
    FWAVFile : TWAVFile;
    FFreeBlocksCount : Integer;
    FBlockIdx : Integer; 
    FCriticalSection : TRTLCriticalSection;
    FBuffersLen : Integer;

  public
    constructor Create(WAVFile : TWAVFile);
    destructor Destroy; override;
    function Init : Boolean;
    function PlayRange(Start, Stop : Cardinal) : Boolean;
    function Deinit : Boolean;

    // don't call this one :p
    procedure IncreaseFreeBlockCount;
  end;


implementation

uses
  Dialogs, TntSysUtils;

// -----------------------------------------------------------------------------

destructor TWAVFile.Destroy;
begin
  if Assigned(FFS) then
  begin
    FFS.Free;
    FFS := nil;
  end;
  inherited;
end;

// -----------------------------------------------------------------------------

function TWAVFile.Open(filename : WideString) : Boolean;
var Buff : array[0..3] of Char;
    Size, Size2 : Integer;
begin
  Result := False;
  if FIsOpen then
    Close;

  if not WideFileExists(filename) then
    Exit;

  try
    FFS := TTntFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;

  // === RIFF Header ===
  if not (FFS.Read(Buff, 4) = 4) or not (StrLComp(Buff,'RIFF',4) = 0) then
    Exit;

  // Read size
  if not (FFS.Read(Size,4) = 4) then
    Exit;

  if (Size+8) <> FFS.Size then
  begin
    MessageDlg('Warning, ' + FileName + ' is damaged, loading may not work correctly.', mtWarning, [mbOK], 0);
  end;

  if not (FFS.Read(Buff, 4) = 4) or not (StrLComp(Buff,'WAVE',4) = 0) then
    Exit;

  // === FORMAT Header ===
  if not (FFS.Read(Buff, 4) = 4) or not (StrLComp(Buff,'fmt ',4) = 0) then
    Exit;

  // Read WAVFORMATEX struct size
  if not FFS.Read(Size,4) = 4 then
    Exit;

  // Read the WAVFORMATEX struct (discard any extra data)
  Size2 := Size - SizeOf(FWFX);
  Size := Min(SizeOf(FWFX), Size);
  ZeroMemory(@FWFX, SizeOf(FWFX));
  if not (FFS.Read(FWFX, Size) = Size) then
    Exit;

  // Skip extra data
  if Size2 > 0 then
    FFS.Seek(Size2,soFromCurrent);

  // === DATA Header ===
  if not (FFS.Read(Buff, 4) = 4) or not (StrLComp(Buff,'data',4) = 0) then
    Exit;

  // Read data size
  if not (FFS.Read(FDataSize,4) = 4) then
    Exit;

  FHeaderSize := FFS.Position;

  Result := True;
  FIsOpen := True;
end;

// -----------------------------------------------------------------------------

function TWAVFile.Read(Buffer : Pointer; Count : Integer) : Integer;
begin
  Result := FFS.Read(Buffer^, Count);
end;

// -----------------------------------------------------------------------------

procedure TWAVFile.Close;
begin
  if Assigned(FFS) then
  begin
    FFS.Free;
    FFS := nil;
  end;
  ZeroMemory(@FWFX, SizeOf(FWFX));
  FHeaderSize := 0;
  FDataSize := 0;
  FIsOpen := False;
end;

// -----------------------------------------------------------------------------

function TWAVFile.GetNbsSamples : Cardinal;
begin
  Result := FDataSize div (FWFX.wBitsPerSample div 8);
end;

// -----------------------------------------------------------------------------

function TWAVFile.GetBitsPerSample : Cardinal;
begin
  Result := FWFX.wBitsPerSample;
end;

// -----------------------------------------------------------------------------

function TWAVFile.GetChannels : Cardinal;
begin
  Result := FWFX.nChannels;
end;

// -----------------------------------------------------------------------------

function TWAVFile.GetSamplesPerSecond : Cardinal;
begin
  Result := FWFX.nSamplesPerSec;
end;

// -----------------------------------------------------------------------------

function TWAVFile.GetWaveFormatEx : PWaveFormatEx;
begin
  Result := @FWFX;
end;

// -----------------------------------------------------------------------------

function TWAVFile.GetDurationMs : Cardinal;
var Tmp : Extended;
begin
  if IsOpen then
  begin
    Tmp := (FDataSize / (FWFX.wBitsPerSample div 8) / FWFX.nChannels) * 1000;
    Result := Round(Tmp / FWFX.nSamplesPerSec);
  end
  else
    Result := 0
end;

// -----------------------------------------------------------------------------

procedure TWAVFile.SeekMs(PosMs : Cardinal);
var StartOffsetBytes : Cardinal;
begin
  StartOffsetBytes := Round(PosMs / 1000 * FWFX.nAvgBytesPerSec);
  // Make sure we are aligned on samples
  StartOffsetBytes := StartOffsetBytes - (StartOffsetBytes mod FWFX.nBlockAlign);
  FFS.Seek(Cardinal(FHeaderSize) + StartOffsetBytes,soFromBeginning);
end;

// -----------------------------------------------------------------------------

function TWAVFile.ExtractToStream(StartMs,StopMs : Integer; Output : TStream) : Integer;
var StartOffsetBytes, StopOffsetBytes, LengthBytes, TotalLen : Cardinal;
    WFXLen : Cardinal;
begin
  StartOffsetBytes := Round(StartMs / 1000 * FWFX.nAvgBytesPerSec);
  StartOffsetBytes := StartOffsetBytes - (StartOffsetBytes mod FWFX.nBlockAlign);
  StopOffsetBytes := Round(StopMs / 1000 * FWFX.nAvgBytesPerSec);
  StopOffsetBytes := StopOffsetBytes - (StopOffsetBytes mod FWFX.nBlockAlign);
  LengthBytes := StopOffsetBytes - StartOffsetBytes;

  TotalLen := Length('RIFF') + SizeOf(TotalLen) + Length('WAVEfmt ') +
    SizeOf(WFXLen) + SizeOf(FWFX) + Length('data') + SizeOf(LengthBytes) +
    LengthBytes;

  Result := TotalLen;

  // Write WAV header
  Output.Write('RIFF', 4);
  TotalLen := TotalLen - 8; // don't include 'RIFF'+Size in the total size
  Output.Write(TotalLen, SizeOf(TotalLen));

  Output.Write('WAVEfmt ',8);
  WFXLen := SizeOf(FWFX);
  Output.Write(WFXLen, SizeOf(WFXLen));
  Output.Write(FWFX, SizeOf(FWFX));
  Output.Write('data', 4);
  Output.Write(LengthBytes, SizeOf(LengthBytes));

  // Write PCM data
  FFS.Seek(Cardinal(FHeaderSize) + StartOffsetBytes, soFromBeginning);
  Output.CopyFrom(FFS, LengthBytes);
end;

// =============================================================================
// TWAVRenderer (UNFINISHED)
// =============================================================================

constructor TWAVRenderer.Create(WAVFile : TWAVFile);
begin
  InitializeCriticalSection(FCriticalSection);
  FWAVFile := WAVFile;
end;

//------------------------------------------------------------------------------

destructor TWAVRenderer.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TWAVRenderer.IncreaseFreeBlockCount;
begin
  EnterCriticalSection(FCriticalSection);
  Inc(FFreeBlocksCount);
  LeaveCriticalSection(FCriticalSection);
end;

//------------------------------------------------------------------------------

procedure TWAVRenderer_CallBack(hwo : HWAVEOUT; uMsg : UINT; dwInstance, dwParam1, dwParam2 : DWORD); stdcall;
var WAVRenderer : TWAVRenderer;
begin
  if (uMsg <> WOM_DONE) then
    Exit;
  WAVRenderer := TWAVRenderer(dwInstance);
  WAVRenderer.IncreaseFreeBlockCount;
end;

// -----------------------------------------------------------------------------

function TWAVRenderer.Init : Boolean;
var res : MMRESULT;
    i : Integer;
    pWaveFormat : PWaveFormatEx;
begin
  Result := False;
  pWaveFormat := FWAVFile.GetWaveFormatEx;
  // Open the waveform output device using function callback.
  res := waveOutOpen(@FWaveOutHandle, WAVE_MAPPER, pWaveFormat,
    Cardinal(@TWAVRenderer_CallBack), Cardinal(Self), CALLBACK_FUNCTION);
  if res <> MMSYSERR_NOERROR then
    Exit;

  FBuffersLen := pWaveFormat.nAvgBytesPerSec; // 1 seconds
  for i:=0 to Length(FHeaders)-1 do
  begin
    ZeroMemory(@FHeaders[i],SizeOf(WAVEHDR));
    FHeaders[i].lpData := GetMemory(FBuffersLen);
    FHeaders[i].dwBufferLength := FBuffersLen;
    res := waveOutPrepareHeader(FWaveOutHandle,@FHeaders[i],SizeOf(WAVEHDR));
    if res <> MMSYSERR_NOERROR then
      Exit;
  end;
  FBlockIdx := 0;
  FFreeBlocksCount := Length(FHeaders);
  Result := True;
end;

// -----------------------------------------------------------------------------

function TWAVRenderer.Deinit : Boolean;
var i : integer;
begin
  // Stop playing
  waveOutReset(FWaveOutHandle);
  waveOutClose(FWaveOutHandle);
  // Unprepare headers and free all buffers
  for i:=0 to Length(FHeaders)-1 do
  begin
    if (FHeaders[i].dwFlags and WHDR_PREPARED) = WHDR_PREPARED then
      waveOutUnprepareHeader(FWaveOutHandle, @FHeaders[i], sizeof(WAVEHDR));
    FreeMem(FHeaders[i].lpData);
  end;
  Result := True;
end;

// -----------------------------------------------------------------------------

function TWAVRenderer.PlayRange(Start, Stop : Cardinal) : Boolean;
var res : MMRESULT;
    pWaveFormat : PWaveFormatEx;
    pCurrentHDR : PWAVEHDR;
    BytesLeft : Integer;
    BytesRead : Integer;
begin
  // TODO : put this in a thread, add stop, loop
  Init;
  
  Result := False;
  pWaveFormat := FWAVFile.GetWaveFormatEx;
  BytesLeft := Round( ((Stop - Start) / 1000) * pWaveFormat.nAvgBytesPerSec );
  // Make sure we are aligned on samples
  BytesLeft := BytesLeft - (BytesLeft mod pWaveFormat.nBlockAlign);

  FWAVFile.SeekMs(Start);
  while( BytesLeft > 0 ) do
  begin
    pCurrentHDR := @FHeaders[FBlockIdx];

    BytesRead := FWAVFile.Read(pCurrentHDR.lpData, Min(FBuffersLen,BytesLeft));
    pCurrentHDR.dwBufferLength := BytesRead;
    Dec(BytesLeft,BytesRead);

    res := waveOutWrite(FWaveOutHandle,pCurrentHDR,SizeOf(WAVEHDR));
    if res <> MMSYSERR_NOERROR then
      Exit;

    EnterCriticalSection(FCriticalSection);
    Dec(FFreeBlocksCount);
    LeaveCriticalSection(FCriticalSection);

    // wait a free buffer    
    while(FFreeBlocksCount <= 0) do
      Sleep(50);

    Inc(FBlockIdx);
    FBlockIdx := FBlockIdx mod Length(FHeaders);
  end;

  // Wait for completion
  while (FFreeBlocksCount < Length(FHeaders)) do
    Sleep(50);

  Result := True;
  Deinit;      
end;

// -----------------------------------------------------------------------------
end.
// -----------------------------------------------------------------------------
