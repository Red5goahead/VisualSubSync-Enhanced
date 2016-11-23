// -----------------------------------------------------------------------------
//  A "simple" MP4 file parser to extract KF information
// -----------------------------------------------------------------------------
//  Copyright (C) 2012 Christophe Paris
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

unit MP4File;

interface

uses Windows, Classes, LogWriterIntf, FileReaderUnit;

type
  TMP4File = class;

  // A MP4 box
  //
  // Box Header :
  //   - Box length in bytes including header (4 bytes big-endian)
  //   - Box name (4 bytes)
  // Box data ...
  TMP4Box = class
  private
    FMP4File : TMP4File;
  public
    Position : UInt64; // Box position in file
    Len : UInt64; // Box length including header
    DataLen : UInt64; // Box length without header
    Name : string;  // Box name
    DataLeft : UInt64; // Data left to read.

    Next : TMP4Box; // To build linked list of TMP4Box

    constructor Create(MP4File : TMP4File);
    procedure Assign(const Source : TMP4Box);
    function ReadBoxHeader : Boolean;
    procedure SkipData;
    procedure Skip(Len : Cardinal) ;
    function ReadUINT64(const Name : string = '') : UInt64;
    function ReadUINT32(const Name : string = '') : Cardinal;
    function ReadUINT16(const Name : string = '') : Word;
    function ReadUINT8(const Name : string = '') : Byte;
    procedure Read(var Buffer; const Count: Longint; const Name : string = '');
  end;

  TMP4Sample = record
    Position : UInt64;
    Len : UInt64;
    Sync : Boolean;
    DecodingTime : UInt64;
    CompositionTime : UInt64;
    DecodeDelta : UInt64;
    CompositionOffset : UInt64;
  end;

  TMP4StscEntry = record
    first_chunk : Cardinal;
    samples_per_chunk : Cardinal;
  end;

  TMP4SttsEnty = record
    sample_count : Cardinal;
    sample_delta : Cardinal;
  end;

  TMP4CttsEnty = record
    sample_count : Cardinal;
    sample_offset : Cardinal;
  end;

  TMP4Elst = record
    segment_duration : UInt64;
    media_time : Int64;
    media_rate_integer : Word;
    media_rate_fraction : Word;
  end;

  TMP4Track = class
  public
    handlerType : string;
    timescale : UInt64;
    duration : UInt64;
    NALULenSize : Integer;
    samples : array of TMP4Sample;
    stss : array of Cardinal; // sync sample
    stco : array of Cardinal; // chunk offset
    stsc : array of TMP4StscEntry; // sample to chunk
    stsz : array of Cardinal; // sample size

    stts : array of TMP4SttsEnty; // time to sample
    ctts : array of TMP4CttsEnty; // composition offset

    edts : array of TMP4Elst; // edit list
  public
    function buildSamplesTable : Boolean;
  end;

  TMP4File = class
  private
    FFileReader : TFileReader;
    FLogWriter : TLogWriterIntf;
    FLevelList : TMP4Box;
    FLevelCount : Integer;
    FTracks : TList;
    FCurrentTrack : TMP4Track;

    function ParseMoov : Boolean;
    function ParseTrak : Boolean;
    function ParseMdia : Boolean;
    function ParseMinf : Boolean;
    function ParseStbl : Boolean;
    function ParseStsdEntry : Boolean;
    function ParseEdts : Boolean;
  public
    constructor Create(FileReader : TFileReader; LogWriter : TLogWriterIntf);
    destructor Destroy; override;

    function Parse : Boolean;
    
    procedure IncLevel(MP4Box : TMP4Box);
    procedure DecLevel;
    procedure ClearLevel(const UpToID : Int64 = 0);
    function HasMoreElement : Boolean;

    property TrackList : TList read FTracks;
  end;

  TBitStream = class
  private
    FBuffer: PByte;
    FLen : Integer;
    FBitPosition : Integer;
    FBytePosition : Integer;
  public
    constructor Create(Buffer : PByte; Len : Integer);
    function ReadBits(BitCount : Integer) : Cardinal;
    function ReadBit : Byte;
    function ReadGolomb: Cardinal;
  end;



implementation

uses SysUtils;

// =============================================================================

constructor TBitStream.Create(Buffer : PByte; Len : Integer);
begin
  FBuffer := Buffer;
  FLen := Len;
  FBytePosition := 0;
  FBitPosition := 0;
end;

function TBitStream.ReadBit : Byte;
var ByteCursor : PByte;
    BitMask : Byte;
begin
  if (FBytePosition >= FLen) then
    raise Exception.CreateFmt('TBitStream.ReadBit : read error no more bit left.', []);

  ByteCursor := FBuffer;
  Inc(ByteCursor, FBytePosition);
  BitMask := (1 shl (7 - FBitPosition));
  Result := (ByteCursor^ and BitMask) shr (7 - FBitPosition);

  Inc(FBitPosition);
  if (FBitPosition > 7) then
  begin
    Inc(FBytePosition);
    FBitPosition := 0;
  end;

end;

function TBitStream.ReadBits(BitCount : Integer) : Cardinal;
var Bit : Byte;
begin
  Result := 0;
  while (BitCount > 0) do
  begin
    Bit := ReadBit;
    Result := (Result or (Bit shl (BitCount - 1)));
    Dec(BitCount);
  end;
end;

function TBitStream.ReadGolomb: Cardinal;
var leading_zero : Cardinal;
begin
  leading_zero := 0;
  while (ReadBit = 0) do
  begin
    Inc(leading_zero);
  end;
  if (leading_zero > 0) then
    Result := (1 shl leading_zero) - 1 + ReadBits(leading_zero)
  else
    Result := 0;
end;

// =============================================================================

function Swap32(Value: LongInt): LongInt; register;
asm
  BSWAP  EAX
end;

function Swap64(Value: UInt64): UInt64;
begin
  Result:= Swap32(LongWord(Value));
  Result:= (Result shl 32) or Swap32(LongWord(Value shr 32));
end;

constructor TMP4Box.Create(MP4File : TMP4File);
begin
  FMP4File := MP4File;
  Position := 0;
  Len := 0;
  DataLen := 0;
  Name := '';
end;

procedure TMP4Box.Assign(const Source : TMP4Box);
begin
  Position := Source.Position;
  Len := Source.Len;
  DataLen := Source.DataLen;
  Name := Source.Name;
  DataLeft := Source.DataLeft;
end;

function TMP4Box.ReadBoxHeader : Boolean;
begin
  Result := False;
  Position := FMP4File.FFileReader.Tell;

  if FMP4File.FFileReader.Read(Len, 4) <> 4 then
    Exit;
  Len := Swap32(Len);

  SetLength(Name, 4);
  if FMP4File.FFileReader.Read(Name[1], 4) <> 4 then
    Exit;

  if (Len = 1) then
  begin
    // big box using a 64 bits length.
    Len := ReadUINT64;
  end
  else if (Len = 0) then
  begin
    // box extends to end of file.
    Len := FMP4File.FFileReader.Size - Position;
  end;
  
  DataLen := Len - 8;
  DataLeft := DataLen;
  Result := True;
end;

procedure TMP4Box.SkipData;
begin
  FMP4File.FFileReader.Seek(Position + Len, soBeginning);
  DataLeft := 0;
end;

procedure TMP4Box.Skip(Len : Cardinal);
begin
  FMP4File.FFileReader.Seek(Len, soCurrent);
  Dec(DataLeft, Len);
end;

function TMP4Box.ReadUINT64(const Name : string = '') : UInt64;
begin
  Read(Result, 8);
  Result := Swap64(Result);
end;

function TMP4Box.ReadUINT32(const Name : string) : Cardinal;
begin
  Read(Result, 4);
  Result := Swap32(Result);
end;

function TMP4Box.ReadUINT16(const Name : string ) : Word;
begin
  Read(Result, 2);
  Result := Swap(Result);
end;

function TMP4Box.ReadUINT8(const Name : string ) : Byte;
begin
  Read(Result, 1);
end;

procedure TMP4Box.Read(var Buffer; const Count: Longint; const Name : string = '');
begin
  if FMP4File.FFileReader.Read(Buffer, Count) <> Count then
    raise Exception.CreateFmt('ReadValue(%s) : read error at position %d', [Name, Position]);
  Dec(DataLeft, Count);
end;

// =============================================================================

function TMP4Track.buildSamplesTable : Boolean;
var i, j, k : Integer;
    chunkSPC : array of Cardinal;
    nbChunk, samplesPerChunk, samplePos : Cardinal;
    accuOffset : UInt64;
begin
  SetLength(samples, Length(stsz));
  // Set samples size
  for i := 0 to Length(stsz) - 1 do
  begin
    samples[i].Len := stsz[i];
  end;
  // Set samples sync point
  for i := 0 to Length(stss) - 1 do
  begin
    samples[stss[i] - 1].Sync := True;
  end;
  
  if (Length(stsc) = 1) and (stsc[0].samples_per_chunk = 1) then
  begin
    // 1 sample per chunk in all chunks
    for i := 0 to Length(stco) - 1 do
    begin
      samples[i].Position := stco[i];
    end;
  end
  else
  begin
    // Build a table with the number of samples for each chunk
    SetLength(chunkSPC, Length(stco));
    k := 0;
    for i := 1 to Length(stsc) - 1 do // Start at second element
    begin
      nbChunk := stsc[i].first_chunk - stsc[i - 1].first_chunk;
      samplesPerChunk := stsc[i - 1].samples_per_chunk;
      for j := 0 to nbChunk - 1 do
      begin
        chunkSPC[k] := samplesPerChunk;
        Inc(k);
      end;
    end;   
    chunkSPC[k] := stsc[i - 1].samples_per_chunk;

    // Calculate position of each samples
    k := 0;
    for i := 0 to Length(stco) - 1 do
    begin
      samplePos := stco[i];
      for j := 0 to chunkSPC[i] - 1 do
      begin
        samples[k].Position := samplePos;
        Inc(samplePos, samples[k].Len);
        Inc(k);
      end;
    end;

  end;

  // Fill decoding time in ticks
  accuOffset := 0;
  k := 0;
  for i :=0 to Length(stts) - 1 do
  begin
    for j := 0 to stts[i].sample_count - 1 do
    begin
      samples[k].DecodingTime := accuOffset;
      samples[k].DecodeDelta := stts[i].sample_delta;
      Inc(accuOffset, stts[i].sample_delta);
      Inc(k);
    end;
  end;

  // Fill composition time
  k := 0;
  for i :=0 to Length(ctts) - 1 do
  begin
    for j := 0 to ctts[i].sample_count - 1 do
    begin
      samples[k].CompositionTime := samples[k].DecodingTime + ctts[i].sample_offset;
      samples[k].CompositionOffset := ctts[i].sample_offset;
      Inc(k);
    end;
  end;

  // Some very basic edit list support
  if Length(edts) > 0 then
  begin
    if (edts[0].media_time <> 0) and (edts[0].media_rate_integer = 1) then
    begin
      for i := 0 to Length(samples) - 1 do
      begin
        samples[i].CompositionTime := samples[i].CompositionTime - edts[0].media_time;
      end;
    end;
  end;

  Result := True;
end;

// =============================================================================

constructor TMP4File.Create(FileReader : TFileReader; LogWriter : TLogWriterIntf);
begin
  FFileReader := FileReader;
  FLogWriter := LogWriter;
  FLevelList := nil;
  FLevelCount := 0;
  FTracks := TList.Create;
end;

// -----------------------------------------------------------------------------

destructor TMP4File.Destroy;
var i : Integer;
    track : TMP4Track;
begin
  for i := 0 to FTracks.Count-1 do
  begin
    track := FTracks.Items[i];
    track.Free;
  end;
  FTracks.Free;
end;

// -----------------------------------------------------------------------------

function ConvertTS(DecodingTime, TimeScale : Int64) : Cardinal;
var tmp : Extended;
begin
  tmp := DecodingTime;
  tmp := tmp / TimeScale;
  tmp := tmp * 1000.0;
  Result := Round(tmp);
end;

function TMP4File.Parse : Boolean;
var MP4Box : TMP4Box;
    i, trackIdx : integer;
    track : TMP4Track;
    sampleIdx : integer;
    sample : TMP4Sample;
    NALULen : Cardinal;
    nal_ref_idc, nal_unit_type, first_mb_in_slice, slice_type : Byte;
    buff : array of Byte;
    bs : TBitStream;
    kframe : Boolean;
    elst : TMP4Elst;
begin
  FLogWriter.AddTextLine('----- Parsing start:');
  MP4Box := TMP4Box.Create(Self);
  while (MP4Box.ReadBoxHeader = True) do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));
    if (MP4Box.Name = 'moov') then
    begin
      IncLevel(MP4Box);
      if ParseMoov = False then
        Break;
      DecLevel;
    end
    else
      MP4Box.SkipData;
  end;

  FLogWriter.AddTextLine('');
  FLogWriter.AddTextLine('----- List of tracks:');

  for trackIdx := 0 to FTracks.Count-1 do
  begin
    track := FTracks.Items[trackIdx];
    track.buildSamplesTable; 
    FLogWriter.AddTextLine(Format('track: %d, type: %s,  timescale: %d, duration: %d',
      [trackIdx, track.handlerType, track.timescale, track.duration]));

    for i := 0 to Length(track.edts) - 1 do
    begin
      elst := track.edts[i];
      FLogWriter.AddTextLine(Format('  edit list [%d] segment_duration: %d, media_time: %d,  media_rate_integer: %d, media_rate_fraction: %d',
        [i, elst.segment_duration, elst.media_time, elst.media_rate_integer, elst.media_rate_fraction]));
    end;
  end;
  {
  FLogWriter.AddTextLine('');
  FLogWriter.AddTextLine('----- Video track details:');

  for trackIdx := 0 to FTracks.Count-1 do
  begin
    track := FTracks.Items[trackIdx];
    
    if track.handlerType = 'vide' then
    begin

      // Show sync point
      for sampleIdx := 0 to Length(track.samples)-1 do
      begin
        sample := track.samples[sampleIdx];
        if sample.Sync then
            FLogWriter.AddTextLine(Format('%d %6d %10d %7d %s',
              [trackIdx, sampleIdx + 1, sample.Position, sample.Len,
              TimeMsToString(ConvertTS(sample.DecodingTime, track.timescale), '.')
              ]));
      end;
      

      // Parse frame data to extract the frame type
      //for sampleIdx := 0 to Length(track.samples)-1 do
      for sampleIdx := 0 to 300 do
      begin
        sample := track.samples[sampleIdx];
        FFileReader.Seek(sample.Position, soBeginning);
        FFileReader.Read(NALULen, track.NALULenSize);

        SetLength(buff, 8);
        FFileReader.Read(buff[1], 2);
        bs := TBitStream.Create(@buff[1], 2);

        bs.ReadBits(1); // forbidden_zero_bit f(1)
        nal_ref_idc := bs.ReadBits(2); // nal_ref_idc u(2)
        nal_unit_type := bs.ReadBits(5); // nal_unit_type u(5)

        if (nal_unit_type = 5) or (nal_unit_type = 1) then
        begin
          first_mb_in_slice :=  bs.ReadGolomb; // first_mb_in_slice ue(v)
          slice_type :=  bs.ReadGolomb; // slice_type ue(v)

          case slice_type of
          0, 5: kframe := False; // P
          1, 6: kframe := False; // B
          2, 7: kframe := True;  // I
          3, 8: kframe := False; // SP
          4, 9: kframe := False; // SI
          else kframe := False;
          end;

        end
        else
        begin
          first_mb_in_slice := 0;
          slice_type := 0;
        end;

          //if kframe then
          begin
            FLogWriter.AddTextLine(Format('%d %6d %10d %7d %5s / %d %d %d %d / %6d %6d / %6d %6d / %s',
              [trackIdx, sampleIdx, sample.Position, sample.Len, BoolToStr(sample.Sync, True),
              nal_ref_idc, nal_unit_type, first_mb_in_slice, slice_type,
              sample.DecodeDelta, sample.CompositionOffset, sample.DecodingTime, sample.CompositionTime,
              TimeMsToString(
                ConvertTS(sample.CompositionTime, track.timescale), '.')
                ]));
          end;

        end;
      end;
  end;
  }
  
  Result := True;
end;

// -----------------------------------------------------------------------------

function TMP4File.ParseMoov : Boolean;
var MP4Box : TMP4Box;
begin
  FLogWriter.AddTextLine('>>');
  MP4Box := TMP4Box.Create(Self);
  while HasMoreElement and MP4Box.ReadBoxHeader do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));
    if (MP4Box.Name = 'trak') then
    begin
      IncLevel(MP4Box);
      if ParseTrak = False then
        Break;
      DecLevel;
    end
    else
      MP4Box.SkipData;
  end;
  Result := (HasMoreElement = False);
  MP4Box.Free;
  FLogWriter.AddTextLine('<<');
end;

// -----------------------------------------------------------------------------

function TMP4File.ParseTrak : Boolean;
var MP4Box : TMP4Box;
begin
  FLogWriter.AddTextLine('>>');

  FCurrentTrack := TMP4Track.Create;
  FTracks.Add(FCurrentTrack);

  MP4Box := TMP4Box.Create(Self);
  while HasMoreElement and MP4Box.ReadBoxHeader do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));
    if (MP4Box.Name = 'mdia') then
    begin
      IncLevel(MP4Box);
      if ParseMdia = False then
        Break;
      DecLevel;
    end
    else if (MP4Box.Name = 'edts') then
    begin
      IncLevel(MP4Box);
      if ParseEdts = False then
        Break;
      DecLevel;
    end
    else
      MP4Box.SkipData;
  end;
  Result := (HasMoreElement = False);
  MP4Box.Free;

  FCurrentTrack := nil;

  FLogWriter.AddTextLine('<<');
end;

// -----------------------------------------------------------------------------

function TMP4File.ParseMdia : Boolean;
var MP4Box : TMP4Box;
    version : Byte;
begin
  FLogWriter.AddTextLine('>>');
  MP4Box := TMP4Box.Create(Self);
  while HasMoreElement and MP4Box.ReadBoxHeader do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));
    if (MP4Box.Name = 'minf') then
    begin
      IncLevel(MP4Box);
      if ParseMinf = False then
        Break;
      DecLevel;
    end
    else if (MP4Box.Name = 'hdlr') then
    begin
      MP4Box.Skip(8); // Skip version, flags, pre_defined
      SetLength(FCurrentTrack.handlerType, 4);
       // vide, soun, hint, meta
      MP4Box.Read(FCurrentTrack.handlerType[1], 4, 'handler_type');
      MP4Box.SkipData;
    end
    else if (MP4Box.Name = 'mdhd') then
    begin
      // Get timescale and duration
      MP4Box.Read(version, 1, 'mdhd.version');
      MP4Box.Skip(3); // skip flags
      if (version = 0) then
      begin
        MP4Box.Skip(4+4); // skip creation time, modification time
        FCurrentTrack.timescale := MP4Box.ReadUINT32('mdhd.timescale');
        FCurrentTrack.duration := MP4Box.ReadUINT32('mdhd.duration');
        MP4Box.SkipData;
      end
      else if (version = 1) then
      begin
        MP4Box.Skip(8+8); // skip creation time, modification time
        FCurrentTrack.timescale := MP4Box.ReadUINT32('mdhd.timescale');
        FCurrentTrack.duration := MP4Box.ReadUINT64('mdhd.duration');
        MP4Box.SkipData;
      end
      else
      begin
        FLogWriter.AddTextLine(Format('Invalid version %d in mdhd', [version]));
        MP4Box.SkipData;
      end;
    end
    else
      MP4Box.SkipData;
  end;
  Result := (HasMoreElement = False);
  MP4Box.Free;
  FLogWriter.AddTextLine('<<');
end;

// -----------------------------------------------------------------------------

function TMP4File.ParseMinf : Boolean;
var MP4Box : TMP4Box;
begin
  FLogWriter.AddTextLine('>>');
  MP4Box := TMP4Box.Create(Self);
  while HasMoreElement and MP4Box.ReadBoxHeader do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));
    if (MP4Box.Name = 'stbl') then
    begin
      IncLevel(MP4Box);
      if ParseStbl = False then
        Break;
      DecLevel;
    end
    else
      MP4Box.SkipData;
  end;
  Result := (HasMoreElement = False);
  MP4Box.Free;
  FLogWriter.AddTextLine('<<');
end;

// -----------------------------------------------------------------------------

function TMP4File.ParseStbl : Boolean;
var MP4Box : TMP4Box;
    i, entry_count, sample_number : Cardinal;
    sample_size, sample_count, entry_size : Cardinal;
    chunk_offset : Cardinal;
    first_chunk, samples_per_chunk : Cardinal;
    sample_delta : Cardinal;
    sample_offset : Cardinal;
begin
  FLogWriter.AddTextLine('>>');

  MP4Box := TMP4Box.Create(Self);
  while HasMoreElement and MP4Box.ReadBoxHeader do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));
    if (MP4Box.Name = 'stss') then
    begin
      // sync (key, I-frame) sample map
      MP4Box.Skip(4); // Skip version, flags
      entry_count := MP4Box.ReadUINT32('stss.entry_count');
      SetLength(FCurrentTrack.stss, entry_count);
      for i := 0 to entry_count - 1 do
      begin
        sample_number := MP4Box.ReadUINT32('stss.sample_number');
        FCurrentTrack.stss[i] := sample_number;
      end;
    end
    else if (MP4Box.Name = 'stsz') then
    begin
      // sample sizes (framing)
      MP4Box.Skip(4); // Skip version, flags
      sample_size := MP4Box.ReadUINT32('stss.sample_size');
      sample_count := MP4Box.ReadUINT32('stss.sample_count');
      SetLength(FCurrentTrack.stsz, sample_count);
      if (sample_size = 0) then
      begin
        // Samples have different sizes
        for i := 0 to sample_count - 1 do
        begin
          entry_size := MP4Box.ReadUINT32('stss.entry_size');
          FCurrentTrack.stsz[i] := entry_size;
        end;
      end
      else
      begin
        // All samples are the same size
        for i := 0 to sample_count - 1 do
          FCurrentTrack.stsz[i] := sample_size;
      end;
    end
    else if (MP4Box.Name = 'stco') then
    begin
      // chunk offset, partial data-offset information
      MP4Box.Skip(4); // Skip version, flags
      entry_count := MP4Box.ReadUINT32('stco.entry_count');
      SetLength(FCurrentTrack.stco, entry_count);
      for i := 0 to entry_count - 1 do
      begin
        chunk_offset := MP4Box.ReadUINT32('stco.chunk_offset');
        FCurrentTrack.stco[i] := chunk_offset;
      end;
    end
    else if (MP4Box.Name = 'stsd') then
    begin
      MP4Box.Skip(4); // Skip version, flags
      entry_count := MP4Box.ReadUINT32('stsd.entry_count');
      for i := 1 to entry_count do
      begin
        IncLevel(MP4Box);
        if ParseStsdEntry = False then
          Break;
        DecLevel;
      end;
    end
    else if (MP4Box.Name = 'stsc') then
    begin
      MP4Box.Skip(4); // Skip version, flags
      entry_count := MP4Box.ReadUINT32('stsc.entry_count');
      SetLength(FCurrentTrack.stsc, entry_count);
      for i := 0 to entry_count - 1 do
      begin
        first_chunk := MP4Box.ReadUINT32('stsc.first_chunk');
        samples_per_chunk := MP4Box.ReadUINT32('stsc.samples_per_chunk');

        FCurrentTrack.stsc[i].first_chunk := first_chunk;
        FCurrentTrack.stsc[i].samples_per_chunk := samples_per_chunk;
        MP4Box.ReadUINT32('stsc.sample_description_index');
      end;
    end
    else if (MP4Box.Name = 'stts') then
    begin
      MP4Box.Skip(4); // Skip version, flags
      entry_count := MP4Box.ReadUINT32('stts.entry_count');
      SetLength(FCurrentTrack.stts, entry_count);
      for i := 0 to entry_count - 1 do
      begin
        sample_count :=  MP4Box.ReadUINT32('stts.sample_count');
        sample_delta :=  MP4Box.ReadUINT32('stts.sample_delta');

        FCurrentTrack.stts[i].sample_count := sample_count;
        FCurrentTrack.stts[i].sample_delta := sample_delta;
      end;
    end
    else if (MP4Box.Name = 'ctts') then
    begin
      MP4Box.Skip(4); // Skip version, flags
      entry_count := MP4Box.ReadUINT32('ctts.entry_count');
      SetLength(FCurrentTrack.ctts, entry_count);
      for i := 0 to entry_count - 1 do
      begin
        sample_count :=  MP4Box.ReadUINT32('ctts.sample_count');
        sample_offset :=  MP4Box.ReadUINT32('ctts.sample_offset');

        FCurrentTrack.ctts[i].sample_count := sample_count;
        FCurrentTrack.ctts[i].sample_offset := sample_offset;
      end;
    end
    else
      MP4Box.SkipData;
  end;
  Result := (HasMoreElement = False);
  MP4Box.Free;
  FLogWriter.AddTextLine('<<');
end;

// -----------------------------------------------------------------------------

function TMP4File.ParseStsdEntry : Boolean;
var MP4Box : TMP4Box;
    SubBox : TMP4Box;
    width, height : Word;
    lengthSizeMinusOne : Byte;
    DataLeft : Integer;
begin
  FLogWriter.AddTextLine('>>');
  MP4Box := TMP4Box.Create(Self);
  while HasMoreElement and MP4Box.ReadBoxHeader do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));

    if (MP4Box.Name = 'avc1') then
    begin
      MP4Box.Skip(6); // Skip reserved uint8[6]
      MP4Box.Skip(2); // Skip data_reference_index uint16;
      MP4Box.Skip(2); // Skip pre_defined uint16;
      MP4Box.Skip(2); // Skip reserved uint16;
      MP4Box.Skip(12); // Skip reserved uint32[3];
      width := MP4Box.ReadUINT16();
      height := MP4Box.ReadUINT16();
      MP4Box.Skip(4); // Skip horizresolution uint32;
      MP4Box.Skip(4); // Skip vertresolution uint32;
      MP4Box.Skip(4); // Skip reserved uint32;
      MP4Box.Skip(2); // Skip frame_count uint16;
      MP4Box.Skip(32); // Skip compressorname string[32];
      MP4Box.Skip(2); // Skip depth uint16;
      MP4Box.Skip(2); // Skip pre_defined int16;

      // Any box left ?
      DataLeft := MP4Box.DataLeft;
      while (DataLeft > 8) do
      begin
        SubBox := TMP4Box.Create(Self);
        SubBox.ReadBoxHeader;
        if SubBox.Name = 'avcC' then
        begin
          SubBox.ReadUINT8('avcC.configurationVersion');
          SubBox.ReadUINT8('avcC.AVCProfileIndication');
          SubBox.ReadUINT8('avcC.profile_compatibility');
          SubBox.ReadUINT8('avcC.AVCLevelIndication');
          lengthSizeMinusOne := SubBox.ReadUINT8('avcC.reserved(6)+lengthSizeMinusOne(2)');
          lengthSizeMinusOne := (lengthSizeMinusOne and $03);
          FCurrentTrack.NALULenSize := lengthSizeMinusOne + 1;
          // bit(3) reserved = 111;
          // unsigned int(5) numOfSequenceParameterSets;
        end;
        SubBox.SkipData;
        Dec(DataLeft, SubBox.Len);
        SubBox.Free;
      end;
      MP4Box.SkipData;
    end
    else
      MP4Box.SkipData;
  end;
  Result := (HasMoreElement = False);
  MP4Box.Free;
  FLogWriter.AddTextLine('<<');
end;

// -----------------------------------------------------------------------------

function TMP4File.ParseEdts : Boolean;
var MP4Box : TMP4Box;
    version : Byte;
    i, entry_count : Cardinal;
    segment_duration : UInt64;
    media_time : Int64;
    media_rate_integer, media_rate_fraction : Word;
begin
  FLogWriter.AddTextLine('>>');
  MP4Box := TMP4Box.Create(Self);
  while HasMoreElement and MP4Box.ReadBoxHeader do
  begin
    FLogWriter.AddTextLine(Format('%10d %10d %s', [MP4Box.Position, MP4Box.DataLen, MP4Box.Name]));
    if (MP4Box.Name = 'elst') then
    begin
      MP4Box.Read(version, 1, 'elst.version');
      MP4Box.Skip(3); // skip flags
      entry_count := MP4Box.ReadUINT32('elst.entry_count');
      SetLength(FCurrentTrack.edts, entry_count);
      for i := 0 to entry_count - 1 do
      begin
        if (version = 0) then
        begin
          segment_duration := MP4Box.ReadUINT32('elst.segment_duration');
          media_time := Integer(MP4Box.ReadUINT32('elst.media_time'));
        end
        else if (version = 1) then
        begin
          segment_duration := MP4Box.ReadUINT64('elst.segment_duration');
          media_time := Int64(MP4Box.ReadUINT64('elst.media_time'));
        end
        else
        begin
          segment_duration := 0;
          media_time := 0;
        end;
        media_rate_integer := MP4Box.ReadUINT16('elst.media_rate_integer');
        media_rate_fraction := MP4Box.ReadUINT16('elst.media_rate_fraction');

        FCurrentTrack.edts[i].segment_duration := segment_duration;
        FCurrentTrack.edts[i].media_time := media_time;
        FCurrentTrack.edts[i].media_rate_integer := media_rate_integer;
        FCurrentTrack.edts[i].media_rate_fraction := media_rate_fraction;
      end;
    end
    else
      MP4Box.SkipData;
  end;
  Result := (HasMoreElement = False);
  MP4Box.Free;
  FLogWriter.AddTextLine('<<');
end;


// -----------------------------------------------------------------------------

procedure TMP4File.IncLevel(MP4Box : TMP4Box);
var NewMP4Box : TMP4Box;
begin
  NewMP4Box := TMP4Box.Create(Self);
  NewMP4Box.Assign(MP4Box);
  if Assigned(FLevelList) then
    NewMP4Box.Next := FLevelList;
  FLevelList := NewMP4Box;
  Inc(FLevelCount);
end;

// -----------------------------------------------------------------------------

procedure TMP4File.DecLevel;
var MP4Box : TMP4Box;
begin
  if Assigned(FLevelList) then
  begin
    MP4Box := FLevelList;
    FLevelList := MP4Box.Next;
    MP4Box.Free;
    Dec(FLevelCount);    
  end;
end;

// -----------------------------------------------------------------------------

procedure TMP4File.ClearLevel;
begin
  while Assigned(FLevelList) do
    DecLevel;
end;

// -----------------------------------------------------------------------------

function TMP4File.HasMoreElement : Boolean;
begin
  if Assigned(FLevelList) then
    Result := (FFileReader.Tell < (FLevelList.Position + FLevelList.Len))
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

end.
