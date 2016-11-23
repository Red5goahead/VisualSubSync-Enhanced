// -----------------------------------------------------------------------------
//  MatroskaDiag
// -----------------------------------------------------------------------------
//  Copyright (C) 2004 Christophe Paris
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

unit MatroskaHelper;

interface

uses Windows, Classes, LogWriterIntf, FileReaderUnit;

type
  TEbmlReader = class;

  TEbmlElement = class
  private
    FEbmlReader : TEbmlReader;
  public
    Position : Int64;
    Length : Int64;
    ID : Integer;
    IDLen : Integer;
    DataLength : Int64;
    DataLengthLen : Integer;
    Next : TEbmlElement;

    constructor Create(const EbmlReader : TEbmlReader);
    procedure Assign(const Source : TEbmlElement);
    function ReadID : Integer;
    function ReadDataLen : Integer;
    function ReadIDAndDataLen : Integer;

    function ReadValueUINT(const Name : string = '') : Int64;
    function ReadValueFloat(const Name : string = '') : Double;
    function ReadValueString(const Name : string = '') : string;
    procedure ReadValueBinary(var Buffer : PChar; const Name : string = '');
    procedure Skip;
  end;

  TEbmlHeader = record
    EBMLVersion : Cardinal;
    EBMLReadVersion : Cardinal;
    EBMLMaxIDLength : Cardinal;
    EBMLMaxSizeLength : Cardinal;
    DocType : string;
    DocTypeVersion : Cardinal;
    DocTypeReadVersion : Cardinal;
  end;

  TEbmlReader = class
  private
    FFileReader : TFileReader;
    FLevelList : TEbmlElement;
    FLevelCount : Integer;
    FLogWriter : TLogWriterIntf;

  public
    Header : TEbmlHeader;

    constructor Create(FileReader : TFileReader; LogWriter : TLogWriterIntf);
    destructor Destroy; override;

    function ReaderHeader : Boolean;
    procedure IncLevel(EbmlElement : TEbmlElement);
    procedure DecLevel;
    procedure ClearLevel(const UpToID : Int64 = 0);
    function HasMoreElement : Boolean;
    procedure AddLogPair(const AKey, AName : string);
    procedure AddLog(const AText : string);
  end;

  TMatroskaSegmentInfo = record
    Position : Int64;
    Length : Int64;
    MetaSeekOffset : Int64;
    TimecodeScale : Int64;
    Duration : Double;
    Title : UTF8String;
    MuxingApp : UTF8String;
    WritingApp : UTF8String;
    DateUTC : Int64;
    FirstClusterPosition : Int64;
    IsComplete : Boolean;
    SegmentFilename : UTF8String;
    PrevFilename : UTF8String;
    NextFilename : UTF8String;
  end;

  TMatroskaTrackType = (mttVideo = $1, mttAudio = $2, mttComplex = $3,
    mttLogo = $10, mttSubtitles = $11, mttControl = $20);

  PMatroskaTrackEntry = ^TMatroskaTrackEntry;
  TMatroskaTrackEntry = record
    TrackNumber : Cardinal;
    TrackUID : Cardinal;
    FlagEnabled : Cardinal;
    FlagDefault : Cardinal;
    FlagForced : Cardinal;
    FlagLacing : Cardinal;
    MinCache : Cardinal;
    MaxCache : Cardinal;
    DefaultDuration : Cardinal;
    TrackTimecodeScale : Double;
    MaxBlockAdditionID : Cardinal;
    Name : UTF8String;
    Language : string;
    CodecID : string;
    Fourcc : string;
    wFormatTag : Cardinal;
    CodecPrivate : PChar;
    CodecName : UTF8String;
    CodecSettings : UTF8String;
    CodecInfoURL : string;
    CodecDownloadURL : string;
    CodecDecodeAll : Cardinal;
    TrackOverlay : Cardinal;
    case TrackType : TMatroskaTrackType of
      mttVideo: (PixelWidth: Cardinal;
                 PixelHeight: Cardinal;
                 DisplayWidth: Cardinal;
                 DisplayHeight: Cardinal);
      mttAudio: (SamplingFrequency: Double;
                 OutputSamplingFrequency: Double;
                 Channels : Cardinal;
                 BitDepth : Cardinal);
  end;

  PMatroskaMetaSeek = ^TMatroskaMetaSeek;
  TMatroskaMetaSeek = record
    SeekID : Cardinal;
    SeekPosition : Int64;
  end;

  PMatroskaAttachedFile = ^TMatroskaAttachedFile;
  TMatroskaAttachedFile = record
    FileDescription : UTF8String;
    FileName : UTF8String;
    FileMimeType : string;
    FileDataPosition : Int64;
    FileDataLength : Int64;
    FileUID : Cardinal;
  end;

  PMatroskaEditionEntry = ^TMatroskaEditionEntry;
  TMatroskaEditionEntry = record
    EditionUID : Cardinal;
    EditionFlagHidden : Cardinal;
    EditionFlagDefault : Cardinal;
    EditionManaged : Cardinal;
    Chapters : TList;
  end;

  PMatroskaChapterDisplay = ^TMatroskaChapterDisplay;
  TMatroskaChapterDisplay = record
    ChapString : UTF8String;
    ChapLanguage : string;
    ChapCountry : string;
  end;

  PMatroskaChapterAtom = ^TMatroskaChapterAtom;
  TMatroskaChapterAtom = record
    ChapterUID : Cardinal;
    ChapterTimeStart : Int64;
    ChapterTimeEnd : Int64;
    ChapterFlagHidden : Cardinal;
    ChapterFlagEnabled : Cardinal;
    ChapterPhysicalEquiv : Int64;
    ChapterTrack : array[0..31] of Int64;
    ChapterTrackNumber : Integer;
    ChapterDisplay : TList;
    Chapters : TList;
  end;

  TMatroskaTagTarget = record
    TargetTypeValue : Cardinal;
    TargetType : string;
    TrackUID : Cardinal;
    EditionUID : Cardinal;
    ChapterUID : Cardinal;
    AttachmentUID : Cardinal;
  end;

  PMatroskaSimpleTag = ^TMatroskaSimpleTag;
  TMatroskaSimpleTag = record
    TagName : UTF8String;
    TagLanguage : string;
    TagDefault : Cardinal;
    TagString : UTF8String;
    SubTags : TList;
  end;

  PMatroskaTag = ^TMatroskaTag;
  TMatroskaTag = record
    Target : TMatroskaTagTarget;
    SimpleTags : TList;
  end;

  PMatroskaCueTrackPosition = ^TMatroskaCueTrackPosition;
  TMatroskaCueTrackPosition = record
    CueTrack : Cardinal;
  end;

  PMatroskaCuePoint = ^TMatroskaCuePoint;
  TMatroskaCuePoint = record
    CueTime : Int64;
    CueTrackPositions : TList;
  end;

  TMatroskaReader = class
  private
    FEbmlReader : TEbmlReader;
    FLogWriter : TLogWriterIntf;

    function ReadSegmentInfo : Boolean;
    function ReadTracks : Boolean;
    function ReadTrackEntry : Boolean;
    function ReadTrackEntryVideo(pTrack : PMatroskaTrackEntry) : Boolean;
    function ReadTrackEntryAudio(pTrack : PMatroskaTrackEntry) : Boolean;
    function ReadMetaSeekInfo : Boolean;
    function ReadMetaSeekSeekInfo : Boolean;
    function ReadAttachements : Boolean;
    function ReadAttachedFile : Boolean;
    function ReadChapters : Boolean;
    function ReadChaptersEditionEntry : Boolean;
    function ReadChapterAtom(pEditionEntry : PMatroskaEditionEntry; pParentAtom : PMatroskaChapterAtom) : Boolean;
    function ReadChapterDisplay(pChapterAtom : PMatroskaChapterAtom) : Boolean;
    function ReadChapterTrackNumber(pChapterAtom : PMatroskaChapterAtom) : Boolean;
    procedure FixChaptersTiming;
    function ReadCues : Boolean;
    function ReadCuePoint : Boolean;
    function ReadCueTrackPosition(pCuePoint : PMatroskaCuePoint) : Boolean;
    function ReadTags : Boolean;
    function ReadTag : Boolean;
    function ReadTagTargets(pTag : PMatroskaTag) : Boolean;
    function ReadSimpleTag(pParentTag : PMatroskaTag; pParentSimpleTag : PMatroskaSimpleTag) : Boolean;
    procedure Cleanup;
  public
    SegmentInfo : TMatroskaSegmentInfo;
    TrackList : TList; // List of TMatroskaTrackEntry
    MetaSeekList : TList;  // List of TMatroskaMetaSeek
    AttachedFiles : TList;  // List of TMatroskaAttachedFile
    EditionEntries : TList; // List of TMatroskaEditionEntry
    Tags : TList;  // List of TMatroskaTag
    Cues : TList; // List of TMatroskaCuePoint

    constructor Create(FileReader : TFileReader; LogWriter : TLogWriterIntf);
    destructor Destroy; override;

    function ReadHeader : Boolean;
    function GetMetaSeekPosition(const SearchID : Cardinal) : Int64;
    function SeekToWithMetaSeek(const SeekID : Cardinal) : Boolean;
  end;

  function TrackTypeToString(const TrackType : TMatroskaTrackType) : string;
  function IsMatroskaTrackVFW(pTrack : PMatroskaTrackEntry) : Boolean;
  function IsMatroskaTrackACM(pTrack : PMatroskaTrackEntry) : Boolean;
  function DateUTCToString(const DateUTC : Int64) : string;
  function TimeMsToString(const TimeMS: Cardinal) : string;

implementation

uses SysUtils;

const SUPPORTED_MATROSKA_DOC_TYPE_READ_VERSION = 2;

const EBML_ID_EBML = $1A45DFA3;
      EBML_ID_EBMLVersion	= $4286;
      EBML_ID_EBMLReadVersion = $42F7;
      EBML_ID_EBMLMaxIDLength = $42F2;
      EBML_ID_EBMLMaxSizeLength = $42F3;
      EBML_ID_DocType = $4282;
      EBML_ID_DocTypeVersion = $4287;
      EBML_ID_DocTypeReadVersion = $4285;


const MATROSKA_ID0_Segment = $18538067;

      MATROSKA_ID1_SeekHead = $114D9B74;

      MATROSKA_ID1_Segment_Info = $1549A966;
      MATROSKA_ID2_Segment_TimecodeScale = $2AD7B1;
      MATROSKA_ID2_Segment_Duration = $4489;
      MATROSKA_ID2_Segment_Title = $7BA9;
      MATROSKA_ID2_Segment_MuxingApp = $4D80;
      MATROSKA_ID2_Segment_WritingApp = $5741;
      MATROSKA_ID2_Segment_DateUTC = $4461;
      MATROSKA_ID2_Segment_SegmentUID = $73A4;
      MATROSKA_ID2_Segment_SegmentFilename = $7384;
      MATROSKA_ID2_Segment_PrevUID = $3CB923;
      MATROSKA_ID2_Segment_PrevFilename = $3C83AB;
      MATROSKA_ID2_Segment_NextUID = $3EB923;
      MATROSKA_ID2_Segment_NextFilename = $3E83BB;

      MATROSKA_ID1_MetaSeek = $114D9B74;
      MATROSKA_ID2_MetaSeek_Seek = $4DBB;
      MATROSKA_ID3_MetaSeek_SeekID = $53AB;
      MATROSKA_ID3_MetaSeek_SeekPosition = $53AC;

      MATROSKA_ID1_Track = $1654AE6B;
      MATROSKA_ID2_TrackEntry = $AE;
      MATROSKA_ID3_TrackEntry_TrackNumber = $D7;
      MATROSKA_ID3_TrackEntry_TrackUID = $73C5;
      MATROSKA_ID3_TrackEntry_TrackType = $83;
      MATROSKA_ID3_TrackEntry_FlagEnabled = $B9;
      MATROSKA_ID3_TrackEntry_FlagDefault = $88;
      MATROSKA_ID3_TrackEntry_FlagForced = $55AA;
      MATROSKA_ID3_TrackEntry_FlagLacing = $9C;
      MATROSKA_ID3_TrackEntry_MinCache = $6DE7;
      MATROSKA_ID3_TrackEntry_MaxCache = $6DF8;
      MATROSKA_ID3_TrackEntry_DefaultDuration = $23E383;
      MATROSKA_ID3_TrackEntry_TrackTimecodeScale = $23314F;
      MATROSKA_ID3_TrackEntry_MaxBlockAdditionID = $55EE;
      MATROSKA_ID3_TrackEntry_Name = $536E;
      MATROSKA_ID3_TrackEntry_Language = $22B59C;
      MATROSKA_ID3_TrackEntry_CodecID = $86;
      MATROSKA_ID3_TrackEntry_CodecPrivate = $63A2;
      MATROSKA_ID3_TrackEntry_CodecName = $258688;
      MATROSKA_ID3_TrackEntry_CodecSettings = $3A9697;
      MATROSKA_ID3_TrackEntry_CodecInfoURL = $3B4040;
      MATROSKA_ID3_TrackEntry_CodecDownloadURL = $26B240;
      MATROSKA_ID3_TrackEntry_CodecDecodeAll = $AA;
      MATROSKA_ID3_TrackEntry_TrackOverlay = $6FAB;

      MATROSKA_ID3_TrackEntry_Video = $E0;
      MATROSKA_ID4_Video_PixelWidth = $B0;
      MATROSKA_ID4_Video_PixelHeight = $BA;
      MATROSKA_ID4_Video_DisplayWidth = $54B0;
      MATROSKA_ID4_Video_DisplayHeight = $54BA;
      MATROSKA_ID3_TrackEntry_Audio = $E1;
      MATROSKA_ID4_Audio_SamplingFrequency = $B5;
      MATROSKA_ID4_Audio_OutputSamplingFrequency = $78B5;
      MATROSKA_ID4_Audio_Channels = $9F;
      MATROSKA_ID4_Audio_ChannelPositions = $7D7B;
      MATROSKA_ID4_Audio_BitDepth = $6264;

      MATROSKA_ID1_Attachments = $1941A469;
      MATROSKA_ID2_AttachedFile = $61A7;
      MATROSKA_ID3_AttachedFile_FileDescription = $467E;
      MATROSKA_ID3_AttachedFile_FileName = $466E;
      MATROSKA_ID3_AttachedFile_FileMimeType = $4660;
      MATROSKA_ID3_AttachedFile_FileData = $465C;
      MATROSKA_ID3_AttachedFile_FileUID = $46AE;

      MATROSKA_ID1_Cluster = $1F43B675;
      MATROSKA_ID2_Cluster_Timecode = $E7;
      MATROSKA_ID2_Cluster_BlockGroup = $A0;
      MATROSKA_ID3_BlockGroup_Block = $A1;
      MATROSKA_ID3_BlockGroup_ReferenceBlock = $FB;

      MATROSKA_ID1_Chapters = $1043A770;
      MATROSKA_ID2_Chapters_EditionEntry = $45B9;
      MATROSKA_ID3_EditionEntry_EditionUID = $45BC;
      MATROSKA_ID3_EditionEntry_EditionFlagHidden = $45BD;
      MATROSKA_ID3_EditionEntry_EditionFlagDefault = $45DB;
      MATROSKA_ID3_EditionEntry_EditionManaged = $45DD;
      MATROSKA_ID3_EditionEntry_ChapterAtom = $B6;
      MATROSKA_ID4_ChapterAtom_ChapterUID = $73C4;
      MATROSKA_ID4_ChapterAtom_ChapterTimeStart = $91;
      MATROSKA_ID4_ChapterAtom_ChapterTimeEnd = $92;
      MATROSKA_ID4_ChapterAtom_ChapterFlagHidden = $98;
      MATROSKA_ID4_ChapterAtom_ChapterFlagEnabled = $4598;
      MATROSKA_ID4_ChapterAtom_ChapterPhysicalEquiv = $63C3;
      MATROSKA_ID4_ChapterAtom_ChapterTrack = $8F;
      MATROSKA_ID5_ChapterAtom_ChapterTrackNumber = $89;
      MATROSKA_ID4_ChapterAtom_ChapterDisplay = $80;
      MATROSKA_ID5_ChapterDisplay_ChapString = $85;
      MATROSKA_ID5_ChapterDisplay_ChapLanguage = $437C;
      MATROSKA_ID5_ChapterDisplay_ChapCountry = $437E;

      MATROSKA_ID1_Tags = $1254C367;
      MATROSKA_ID2_Tag = $7373;
      MATROSKA_ID3_Tag_Targets = $63C0;
      MATROSKA_ID4_Targets_TargetTypeValue = $68CA;
      MATROSKA_ID4_Targets_TargetType = $63CA;
      MATROSKA_ID4_Targets_EditionUID = $63C9;
      MATROSKA_ID4_Targets_ChapterUID = $63C4;
      MATROSKA_ID4_Targets_TrackUID = $63C5;      
      MATROSKA_ID4_Targets_AttachmentUID = $63C6;
      MATROSKA_ID3_SimpleTag = $67C8;
      MATROSKA_ID4_SimpleTag_TagName = $45A3;
      MATROSKA_ID4_SimpleTag_TagLanguage = $447A;
      MATROSKA_ID4_SimpleTag_TagDefault = $44B4;
      MATROSKA_ID4_SimpleTag_TagString = $4487;
      MATROSKA_ID4_SimpleTag_TagBinary = $4485;

      MATROSKA_ID1_Cues = $1C53BB6B;
      MATROSKA_ID2_CuePoint = $BB;
      MATROSKA_ID3_CueTime = $B3;
      MATROSKA_ID3_CueTrackPosition = $B7;
      MATROSKA_ID4_CueTrack = $F7;
      MATROSKA_ID4_CueBlockNumber = $5378;

// =============================================================================

function IsMatroskaTrackVFW(pTrack : PMatroskaTrackEntry) : Boolean;
begin
  Result := (pTrack <> nil) and (pTrack^.TrackType = mttVideo) and (pTrack^.CodecID = 'V_MS/VFW/FOURCC');
end;

function IsMatroskaTrackACM(pTrack : PMatroskaTrackEntry) : Boolean;
begin
  Result := (pTrack <> nil) and (pTrack^.TrackType = mttAudio) and (pTrack^.CodecID = 'A_MS/ACM');
end;

function TrackTypeToString(const TrackType : TMatroskaTrackType) : string;
begin
  case TrackType of
    mttVideo: Result := 'Video';
    mttAudio: Result := 'Audio';
    mttComplex: Result := 'Complex';
    mttLogo: Result := 'Logo';
    mttSubtitles: Result := 'Subtitles';
    mttControl: Result := 'Control';
    else Result := '';
  end;
end;

function DateUTCToString(const DateUTC : Int64) : string;
var systime : TSystemTime;
    filetime : TFileTime;
    filetimeLI : ULARGE_INTEGER absolute filetime;
begin
  Result := '';
  ZeroMemory(@systime, SizeOf(systime));
  systime.wYear := 2000;
  systime.wMonth := 1;
  systime.wDay := 1;
  if SystemTimeToFileTime(systime, filetime) = True then
  begin
    filetimeLI.QuadPart := filetimeLI.QuadPart + (DateUTC div 100);
    if FileTimeToSystemTime(filetime, systime) = True then
      Result := Format('%d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.%3.3d',
        [systime.wYear, systime.wMonth, systime.wDay,
         systime.wHour, systime.wMinute, systime.wSecond, systime.wMilliseconds]);
  end;
end;

function TimeMsToString(const TimeMS: Cardinal) : string;
var
  hh, min, sec, ms: Cardinal;
begin
  ms := TimeMs div 1000;
  hh := ms div 3600;
  min := ms mod 3600 div 60;
  sec := ms mod 3600 mod 60;
  ms := TimeMs - (hh * 3600 * 1000) - (min * 60 * 1000) - (sec * 1000);
  Result := Format('%2.2d:%2.2d:%2.2d.%3.3d', [hh, min, sec, ms])
end;

// =============================================================================

constructor TEbmlElement.Create(const EbmlReader : TEbmlReader);
begin
  FEbmlReader := EbmlReader;
end;

// -----------------------------------------------------------------------------

function TEbmlElement.ReadID : Integer;
var BitMask : Int64;
    Len : Integer;
    B : BYTE;
begin
  Position := FEbmlReader.FFileReader.Tell;
  ID := 0;
  Len := 0;
  BitMask := (1 shl 7);
  Result := 0;

  while (Len < 4) do
  begin
    if FEbmlReader.FFileReader.Read(B, 1) = 0 then
      Exit;
    ID := ID or B;
    if ((ID and BitMask) = BitMask) then
      Break;
    BitMask := BitMask shl 7;
    ID := ID shl 8;
    Inc(Len);
  end;
  
  IDLen := Len+1;
  Result := IDLen;
end;

// -----------------------------------------------------------------------------

function TEbmlElement.ReadDataLen : Integer;
var BitMask : Int64;
    Len : Integer;
    B : BYTE;
begin
  DataLength := 0;
  Len := 0;
  BitMask := (1 shl 7);
  Result := 0;

  while (Len < 8) do
  begin
    if FEbmlReader.FFileReader.Read(B, 1) = 0 then
      Exit;
    DataLength := DataLength or B;
    if ((DataLength and BitMask) = BitMask) then
      Break;
    BitMask := BitMask shl 7;
    DataLength := DataLength shl 8;
    Inc(Len);
  end;

  if (DataLength = $FF) and (BitMask = (1 shl 7)) then
  begin
    // Unknown length, assume up to the end of file
    DataLength := FEbmlReader.FFileReader.Size - FEbmlReader.FFileReader.Tell;
    FEbmlReader.AddLog(Format('Element with ID %X at position %d is of unknown length', [ID, Position]));
  end
  else
    DataLength := DataLength and (not BitMask);

  DataLengthLen := Len + 1;
  Length := IDLen + DataLengthLen + DataLength;
  Result := DataLengthLen;
end;

// -----------------------------------------------------------------------------

function TEbmlElement.ReadIDAndDataLen : Integer;
begin
  if (ReadID > 0) and (ReadDataLen > 0) then
    Result := IDLen + DataLengthLen
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TEbmlElement.Skip;
begin
  FEbmlReader.AddLog(Format('Skipping element with ID %X at position %d', [ID, Position]));
  FEbmlReader.FFileReader.Seek(DataLength, soCurrent);
end;

// -----------------------------------------------------------------------------

procedure TEbmlElement.Assign(const Source : TEbmlElement);
begin
  Position := Source.Position;
  Length := Source.Length;
  ID := Source.ID;
  IDLen := Source.IDLen;
  DataLength := Source.DataLength;
  DataLengthLen := Source.DataLengthLen;
end;

// -----------------------------------------------------------------------------

function TEbmlElement.ReadValueUINT(const Name : string) : Int64;
var i : Integer;
    ResultAsByteArray : array[0..7] of BYTE absolute Result;
begin
  Result := 0;
  for i:=0 to DataLength-1 do
  begin
    Result := Result shl 8;
    if FEbmlReader.FFileReader.Read(ResultAsByteArray[0], 1) = 0 then
      raise Exception.CreateFmt('ReadValueUINT %s : read error at position %d', [Name, Position]);
  end;
  FEbmlReader.AddLogPair(Name, IntToStr(Result));
end;

// -----------------------------------------------------------------------------

function TEbmlElement.ReadValueString(const Name : string) : string;
var BytesRead : Integer;
begin
  SetLength(Result, DataLength);
  BytesRead := FEbmlReader.FFileReader.Read(Result[1], DataLength);
  if BytesRead <> DataLength then
    raise Exception.CreateFmt('ReadValueString %s : read error at position %d', [Name, Position]);
  Result := Trim(Result);  
  FEbmlReader.AddLogPair(Name, Result);
end;

// -----------------------------------------------------------------------------

function TEbmlElement.ReadValueFloat(const Name : string) : Double;
var LocalValue64 : Double;
    LocalValue32 : Single;
    ValueArray64 : array[0..7] of BYTE absolute LocalValue64;
    ValueArray32 : array[0..3] of BYTE absolute LocalValue32;
    i : integer;
    ResultAsCardinal : Cardinal absolute LocalValue32;
begin
  LocalValue32 := 0; LocalValue64 := 0;
  if (DataLength = 4) then
  begin
    for i:=0 to DataLength-1 do
    begin
      if FEbmlReader.FFileReader.Read(ValueArray32[DataLength-i-1], 1) = 0 then
        raise Exception.CreateFmt('ReadValueFloat %s : read error at position %d', [Name, Position]);
    end;
    Result := LocalValue32;
  end
  else if (DataLength = 8) then
  begin
    for i:=0 to DataLength-1 do
    begin
      if FEbmlReader.FFileReader.Read(ValueArray64[DataLength-i-1], 1) = 0 then
        raise Exception.CreateFmt('ReadValueFloat %s : read error at position %d', [Name, Position]);
    end;
    Result := LocalValue64;
  end
  else
    raise Exception.CreateFmt('ReadValueFloat %s : Float wrong length (%d) at position %d (length must be 4 or 8).', [Name, DataLength, Position]);

  FEbmlReader.AddLogPair(Name, FloatToStr(Result));
end;

// -----------------------------------------------------------------------------

procedure TEbmlElement.ReadValueBinary(var Buffer : PChar; const Name : string);
var BytesRead : Integer;
begin
  GetMem(Buffer, DataLength);
  BytesRead := FEbmlReader.FFileReader.Read(Buffer^, DataLength);
  if BytesRead <> DataLength then
    raise Exception.CreateFmt('ReadValueBinary %s : read error at position %d.', [Name, Position]);
end;

// =============================================================================

constructor TEbmlReader.Create(FileReader : TFileReader; LogWriter : TLogWriterIntf);
begin
  FFileReader := FileReader;
  FLogWriter := LogWriter;
end;

// -----------------------------------------------------------------------------

destructor TEbmlReader.Destroy;
begin
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TEbmlReader.AddLogPair(const AKey, AName : string);
begin
  FLogWriter.AddPair(StringOfChar(' ', (FLevelCount-1)*4) + AKey, AName);
end;

// -----------------------------------------------------------------------------

procedure TEbmlReader.AddLog(const AText : string);
begin
  FLogWriter.AddTextLine(StringOfChar(' ', (FLevelCount-1)*4) + AText);
end;

// -----------------------------------------------------------------------------

function TEbmlReader.ReaderHeader : Boolean;
var CurElement : TEbmlElement;
begin
  Result := False;
  CurElement := TEbmlElement.Create(Self);
  try
    if CurElement.ReadID = 0 then
      raise Exception.Create('TEbmlReader.ReaderHeader : Can''t read EBML header.');

    if(CurElement.ID <> EBML_ID_EBML) then
      raise Exception.CreateFmt('TEbmlReader.ReaderHeader : EBML header ID not found (found 0x%X)',[CurElement.ID]);

    if CurElement.ReadDataLen = 0 then
      raise Exception.Create('TEbmlReader.ReaderHeader : Can''t read EBML header length.');

    // Set default element value
    with Header do
    begin
      EBMLVersion := 1;
      EBMLReadVersion := 1;
      EBMLMaxIDLength := 4;
      EBMLMaxSizeLength := 8;
    end;

    IncLevel(CurElement);
    try
      while HasMoreElement do
      begin
        // Parse header level 1
        if CurElement.ReadIDAndDataLen = 0 then
          raise Exception.CreateFmt('TEbmlReader.ReaderHeader : Error while reading element at position %d.',[CurElement.Position]);

        case CurElement.ID of
          EBML_ID_EBMLVersion:
            Header.EBMLVersion := CurElement.ReadValueUINT('EBMLVersion');
          EBML_ID_EBMLReadVersion:
            Header.EBMLReadVersion := CurElement.ReadValueUINT('EBMLReadVersion');
          EBML_ID_EBMLMaxIDLength:
            Header.EBMLMaxIDLength := CurElement.ReadValueUINT('EBMLMaxIDLength');
          EBML_ID_EBMLMaxSizeLength:
            Header.EBMLMaxSizeLength := CurElement.ReadValueUINT('EBMLMaxSizeLength');
          EBML_ID_DocType:
            Header.DocType := CurElement.ReadValueString('DocType');
          EBML_ID_DocTypeVersion:
            Header.DocTypeVersion := CurElement.ReadValueUINT('DocTypeVersion');
          EBML_ID_DocTypeReadVersion:
            Header.DocTypeReadVersion := CurElement.ReadValueUINT('DocTypeReadVersion');
          else begin
            CurElement.Skip;
          end;
        end;
      end;
    finally
      DecLevel;
    end;
    Result := (HasMoreElement = False);

  finally
    CurElement.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TEbmlReader.IncLevel(EbmlElement : TEbmlElement);
var NewEbmlElement : TEbmlElement;
begin
  NewEbmlElement := TEbmlElement.Create(Self);
  NewEbmlElement.Assign(EbmlElement);
  if Assigned(FLevelList) then
    NewEbmlElement.Next := FLevelList;
  FLevelList := NewEbmlElement;
  Inc(FLevelCount);
end;

// -----------------------------------------------------------------------------

procedure TEbmlReader.DecLevel;
var EbmlElement : TEbmlElement;
begin
  if Assigned(FLevelList) then
  begin
    EbmlElement := FLevelList;
    FLevelList := EbmlElement.Next;
    EbmlElement.Free;
    Dec(FLevelCount);    
  end;
end;

// -----------------------------------------------------------------------------

procedure TEbmlReader.ClearLevel(const UpToID : Int64 = 0);
begin
  while Assigned(FLevelList) and (FLevelList.ID <> UpToID) do
    DecLevel;
end;

// -----------------------------------------------------------------------------

function TEbmlReader.HasMoreElement : Boolean;
begin
  if Assigned(FLevelList) then
    Result := (FFileReader.Tell < (FLevelList.Position + FLevelList.Length))
  else
    Result := False;
end;

// =============================================================================

procedure FreeSimpleTag(pSimpleTag : PMatroskaSimpleTag);
var i : integer;
begin
  if Assigned(pSimpleTag.SubTags) then
  begin
    for i:=0 to pSimpleTag.SubTags.Count-1 do
      FreeSimpleTag(pSimpleTag.SubTags[i]);
    FreeAndNil(pSimpleTag.SubTags);
  end;
  Dispose(pSimpleTag);
end;

// -----------------------------------------------------------------------------

procedure FreeTag(pTag : PMatroskaTag);
var i : integer;
begin
  if Assigned(pTag.SimpleTags) then
  begin
    for i:=0 to pTag.SimpleTags.Count-1 do
      FreeSimpleTag(pTag.SimpleTags[i]);
    FreeAndNil(pTag.SimpleTags);
  end;
  Dispose(pTag);
end;

// -----------------------------------------------------------------------------

procedure FreeChapterAtom(pChapterAtom : PMatroskaChapterAtom);
var i : integer;
begin
  if Assigned(pChapterAtom.Chapters) then
  begin
    for i:=0 to pChapterAtom.Chapters.Count-1 do
      FreeChapterAtom(pChapterAtom.Chapters[i]);
    FreeAndNil(pChapterAtom.Chapters);
  end;
  if Assigned(pChapterAtom.ChapterDisplay) then
  begin
    for i:=0 to pChapterAtom.ChapterDisplay.Count-1 do
      Dispose(PMatroskaChapterDisplay(pChapterAtom.ChapterDisplay[i]));
    FreeAndNil(pChapterAtom.ChapterDisplay);
  end;
  Dispose(pChapterAtom);
end;

// -----------------------------------------------------------------------------

procedure FreeEditionEntry(pEditionEntry : PMatroskaEditionEntry);
var i : integer;
begin
  if Assigned(pEditionEntry.Chapters) then
  begin
    for i:=0 to pEditionEntry.Chapters.Count-1 do
      FreeChapterAtom(pEditionEntry.Chapters[i]);
    FreeAndNil(pEditionEntry.Chapters);
  end;
  Dispose(pEditionEntry);
end;

// -----------------------------------------------------------------------------

procedure FreeTrackEntry(pTrack : PMatroskaTrackEntry);
begin
  if Assigned(pTrack.CodecPrivate) then
    FreeMemory(pTrack.CodecPrivate);
  Dispose(pTrack);
end;

// -----------------------------------------------------------------------------

procedure FreeCuePoint(pCuePoint : PMatroskaCuePoint);
var i : integer;
begin
  if Assigned(pCuePoint.CueTrackPositions) then
  begin
    for i:=0 to pCuePoint.CueTrackPositions.Count-1 do
      Dispose(pCuePoint.CueTrackPositions[i]);
    FreeAndNil(pCuePoint.CueTrackPositions);
  end;
  Dispose(pCuePoint);
end;

// -----------------------------------------------------------------------------

constructor TMatroskaReader.Create(FileReader : TFileReader; LogWriter : TLogWriterIntf);
begin
  FEbmlReader := TEbmlReader.Create(FileReader, LogWriter);
  TrackList := TList.Create;
  FLogWriter := LogWriter;
  MetaSeekList := TList.Create;
  AttachedFiles := TList.Create;
  EditionEntries := TList.Create;
  Tags := TList.Create;
  Cues := TList.Create;
end;

// -----------------------------------------------------------------------------

destructor TMatroskaReader.Destroy;
begin
  Cleanup;
  FEbmlReader.Free;
  TrackList.Free;
  MetaSeekList.Free;
  AttachedFiles.Free;
  EditionEntries.Free;
  Tags.Free;
  Cues.Free;
end;

// -----------------------------------------------------------------------------

procedure TMatroskaReader.Cleanup;
var i : integer;
begin
  for i:=0 to TrackList.Count-1 do
    FreeTrackEntry(PMatroskaTrackEntry(TrackList[i]));
  TrackList.Clear;

  for i:=0 to MetaSeekList.Count-1 do
    Dispose(PMatroskaMetaSeek(MetaSeekList[i]));
  MetaSeekList.Clear;

  for i:=0 to AttachedFiles.Count-1 do
    Dispose(PMatroskaAttachedFile(AttachedFiles[i]));
  AttachedFiles.Clear;

  for i:=0 to EditionEntries.Count-1 do
    FreeEditionEntry(PMatroskaEditionEntry(EditionEntries[i]));
  EditionEntries.Clear;

  for i:=0 to Tags.Count-1 do
    FreeTag(PMatroskaTag(Tags[i]));
  Tags.Clear;

  for i:=0 to Cues.Count-1 do
    FreeCuePoint(PMatroskaCuePoint(Cues[i]));
  Cues.Clear;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.GetMetaSeekPosition(const SearchID : Cardinal) : Int64;
var i : integer;
    pMetaSeek : PMatroskaMetaSeek;
begin
  Result := -1;
  for i:=0 to MetaSeekList.Count-1 do
  begin
    pMetaSeek := MetaSeekList[i];
    if pMetaSeek.SeekID = SearchID then
    begin
      Result := pMetaSeek.SeekPosition;
      Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.SeekToWithMetaSeek(const SeekID : Cardinal) : Boolean;
var MetaSeekPos : Int64;
begin
  Result := False;
  MetaSeekPos := GetMetaSeekPosition(SeekID);
  if MetaSeekPos <> -1 then
  begin
    FEbmlReader.FFileReader.Seek(SegmentInfo.MetaSeekOffset + MetaSeekPos, soBeginning);
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadHeader : Boolean;
var SegmentElem, Level1Elem : TEbmlElement;
begin
  Result := False;

  if FEbmlReader.ReaderHeader = False then
  begin
    FEbmlReader.AddLog('Incomplete header');
    Exit;
  end;

  if (FEbmlReader.Header.DocType <> 'matroska') then
  begin
    FEbmlReader.AddLog(Format('Wrong DocType (%s)',[FEbmlReader.Header.DocType]));
    Exit;
  end;

  if (FEbmlReader.Header.DocTypeReadVersion > SUPPORTED_MATROSKA_DOC_TYPE_READ_VERSION) then
  begin
    FEbmlReader.AddLog(Format('Wrong DocTypeReadVersion (%d)',
      [FEbmlReader.Header.DocTypeReadVersion]));
    Exit;
  end;

  SegmentElem := TEbmlElement.Create(FEbmlReader);

  if (SegmentElem.ReadIDAndDataLen = 0) or (SegmentElem.ID <> MATROSKA_ID0_Segment) then
  begin
    FEbmlReader.AddLog('Segment not found');
    SegmentElem.Free;
    Exit;
  end;

  SegmentInfo.Position := SegmentElem.Position;
  SegmentInfo.Length := SegmentElem.Length;
  SegmentInfo.MetaSeekOffset := SegmentElem.Position + SegmentElem.IDLen +
    SegmentElem.DataLengthLen;
  SegmentInfo.IsComplete := (FEbmlReader.FFileReader.Size >=
    (SegmentInfo.Position + SegmentInfo.Length));

  FEbmlReader.IncLevel(SegmentElem);
  SegmentElem.Free;

  Level1Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    // Parse header level 1
    if Level1Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level1Elem.ID of
      MATROSKA_ID1_Segment_Info:
        begin
          FEbmlReader.AddLog('Segment Info :');
          FEbmlReader.IncLevel(Level1Elem);
          if ReadSegmentInfo = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID1_MetaSeek:
        begin
          FEbmlReader.AddLog('MetaSeek Info :');
          FEbmlReader.IncLevel(Level1Elem);
          if ReadMetaSeekInfo = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID1_Track:
        begin
          FEbmlReader.AddLog('Tracks :');
          FEbmlReader.IncLevel(Level1Elem);
          if ReadTracks = False then
            Break;
          FEbmlReader.DecLevel;              
        end;
      MATROSKA_ID1_Cluster:
        begin
          // Stop parsing on first cluster found, save its position
          SegmentInfo.FirstClusterPosition := Level1Elem.Position;
          Result := True;
          Break;
        end;
      MATROSKA_ID1_Cues:
        begin
          FEbmlReader.AddLog('Cues :');
          FEbmlReader.IncLevel(Level1Elem);
          if ReadCues = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID1_Attachments:
        begin
          FEbmlReader.AddLog('Attachments :');
          FEbmlReader.IncLevel(Level1Elem);
          if ReadAttachements = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID1_Chapters:
        begin
          FEbmlReader.AddLog('Chapters :');
          FEbmlReader.IncLevel(Level1Elem);
          if ReadChapters = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID1_Tags:
        begin
          FEbmlReader.AddLog('Tags :');
          FEbmlReader.IncLevel(Level1Elem);
          if ReadTags = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level1Elem.Skip;
      end;
    end;
  end;

  if Result then
  begin
    try
      // Try to parse stuff at end of file
      if (EditionEntries.Count = 0) and
         SeekToWithMetaSeek(MATROSKA_ID1_Chapters) and
         (Level1Elem.ReadIDAndDataLen > 0) and
         (Level1Elem.ID = MATROSKA_ID1_Chapters) then
      begin
        FEbmlReader.AddLog('Chapters :');
        FEbmlReader.IncLevel(Level1Elem);
        ReadChapters;
        FEbmlReader.DecLevel;
      end;

      if (AttachedFiles.Count = 0) and
         SeekToWithMetaSeek(MATROSKA_ID1_Attachments) and
         (Level1Elem.ReadIDAndDataLen > 0) and
         (Level1Elem.ID = MATROSKA_ID1_Attachments) then
      begin
        FEbmlReader.AddLog('Attachments :');
        FEbmlReader.IncLevel(Level1Elem);
        ReadAttachements;
        FEbmlReader.DecLevel;
      end;

      if (Tags.Count = 0) and
         SeekToWithMetaSeek(MATROSKA_ID1_Tags) and
         (Level1Elem.ReadIDAndDataLen > 0) and
         (Level1Elem.ID = MATROSKA_ID1_Tags) then
      begin
        FEbmlReader.AddLog('Tags :');
        FEbmlReader.IncLevel(Level1Elem);
        ReadTags;
        FEbmlReader.DecLevel;
      end;

      if (Cues.Count = 0) and
         SeekToWithMetaSeek(MATROSKA_ID1_Cues) and
         (Level1Elem.ReadIDAndDataLen > 0) and
         (Level1Elem.ID = MATROSKA_ID1_Cues) then
      begin
        FEbmlReader.AddLog('Cues :');
        FEbmlReader.IncLevel(Level1Elem);
        ReadCues;
        FEbmlReader.DecLevel;
      end;

    except
      on E: Exception do FEbmlReader.AddLog(E.Message);
    end;
  end;
  Level1Elem.Free;
  FEbmlReader.DecLevel; // to match FEbmlReader.IncLevel(SegmentElem);

  // Scale the duration
  if (SegmentInfo.Duration > 0) then
  begin
    SegmentInfo.Duration := (SegmentInfo.Duration * SegmentInfo.TimecodeScale) / 1000000;
  end;

  // Seek to first cluster
  FEbmlReader.FFileReader.Seek(SegmentInfo.FirstClusterPosition, soBeginning);
  FEbmlReader.ClearLevel(MATROSKA_ID0_Segment);
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadSegmentInfo : Boolean;
var Level2Elem : TEbmlElement;
begin
  Level2Elem := TEbmlElement.Create(FEbmlReader);
  
	// Set default value
	SegmentInfo.TimecodeScale := 1000000;

  while FEbmlReader.HasMoreElement do
  begin
    if Level2Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level2Elem.ID of
      MATROSKA_ID2_Segment_TimecodeScale:
        SegmentInfo.TimecodeScale := Level2Elem.ReadValueUINT('TimecodeScale');
      MATROSKA_ID2_Segment_Duration:
        SegmentInfo.Duration := Level2Elem.ReadValueFloat('Duration');
      MATROSKA_ID2_Segment_Title:
        SegmentInfo.Title := Level2Elem.ReadValueString('Title');
      MATROSKA_ID2_Segment_MuxingApp:
        SegmentInfo.MuxingApp := Level2Elem.ReadValueString('MuxingApp');
      MATROSKA_ID2_Segment_WritingApp:
        SegmentInfo.WritingApp := Level2Elem.ReadValueString('WritingApp');
      MATROSKA_ID2_Segment_DateUTC:
        SegmentInfo.DateUTC := Level2Elem.ReadValueUINT('DateUTC');
      MATROSKA_ID2_Segment_SegmentFilename:
        SegmentInfo.SegmentFilename := Level2Elem.ReadValueString('SegmentFilename');
      MATROSKA_ID2_Segment_PrevFilename:
        SegmentInfo.PrevFilename := Level2Elem.ReadValueString('PrevFilename');
      MATROSKA_ID2_Segment_NextFilename:
        SegmentInfo.NextFilename := Level2Elem.ReadValueString('NextFilename');
      else Level2Elem.Skip;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);

  Level2Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadTracks : Boolean;
var Level2Elem : TEbmlElement;
begin
  Level2Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level2Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level2Elem.ID of
      MATROSKA_ID2_TrackEntry:
        begin
          FEbmlReader.AddLog('TrackEntry :');
          FEbmlReader.IncLevel(Level2Elem);
          if ReadTrackEntry = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level2Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  Level2Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadTrackEntry : Boolean;
var Level3Elem : TEbmlElement;
    pTrack : PMatroskaTrackEntry;
begin
  Level3Elem := TEbmlElement.Create(FEbmlReader);

  pTrack := New(PMatroskaTrackEntry);
  ZeroMemory(pTrack, SizeOf(TMatroskaTrackEntry));

	// Set default value
  with pTrack^ do
  begin
    FlagEnabled := 1;
    FlagDefault := 1;
    FlagLacing := 1;
    MinCache := 0;
    TrackTimecodeScale := 1.0;
    CodecDecodeAll := 1;
    Language := 'eng';
  end;

  while FEbmlReader.HasMoreElement do
  begin
    if Level3Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level3Elem.ID of
      MATROSKA_ID3_TrackEntry_TrackNumber:
        pTrack.TrackNumber := Level3Elem.ReadValueUINT('TrackNumber');
      MATROSKA_ID3_TrackEntry_TrackUID:
        pTrack.TrackUID := Level3Elem.ReadValueUINT('TrackUID');
      MATROSKA_ID3_TrackEntry_TrackType:
        pTrack.TrackType := TMatroskaTrackType(Level3Elem.ReadValueUINT('TrackType'));
      MATROSKA_ID3_TrackEntry_FlagEnabled:
        pTrack.FlagEnabled := Level3Elem.ReadValueUINT('FlagEnabled');
      MATROSKA_ID3_TrackEntry_FlagDefault:
        pTrack.FlagDefault := Level3Elem.ReadValueUINT('FlagDefault');
      MATROSKA_ID3_TrackEntry_FlagForced:
        pTrack.FlagForced := Level3Elem.ReadValueUINT('FlagForced');
      MATROSKA_ID3_TrackEntry_FlagLacing:
        pTrack.FlagLacing := Level3Elem.ReadValueUINT('FlagLacing');
      MATROSKA_ID3_TrackEntry_MinCache:
        pTrack.MinCache := Level3Elem.ReadValueUINT('MinCache');
      MATROSKA_ID3_TrackEntry_MaxCache:
        pTrack.MaxCache := Level3Elem.ReadValueUINT('MaxCache');
      MATROSKA_ID3_TrackEntry_DefaultDuration:
        pTrack.DefaultDuration := Level3Elem.ReadValueUINT('DefaultDuration');
      MATROSKA_ID3_TrackEntry_TrackTimecodeScale:
        pTrack.TrackTimecodeScale := Level3Elem.ReadValueFloat('TrackTimecodeScale');
      MATROSKA_ID3_TrackEntry_MaxBlockAdditionID:
        pTrack.MaxBlockAdditionID := Level3Elem.ReadValueUINT('MaxBlockAdditionID');
      MATROSKA_ID3_TrackEntry_Name:
        pTrack.Name := Level3Elem.ReadValueString('Name');
      MATROSKA_ID3_TrackEntry_Language:
        pTrack.Language := Level3Elem.ReadValueString('Language');
      MATROSKA_ID3_TrackEntry_CodecID:
        pTrack.CodecID := Level3Elem.ReadValueString('CodecID');
      MATROSKA_ID3_TrackEntry_CodecPrivate:
        Level3Elem.ReadValueBinary(pTrack.CodecPrivate, 'CodecPrivate');
      MATROSKA_ID3_TrackEntry_CodecName:
        pTrack.CodecName := Level3Elem.ReadValueString('CodecName');
      MATROSKA_ID3_TrackEntry_CodecSettings:
        pTrack.CodecSettings := Level3Elem.ReadValueString('CodecSettings');
      MATROSKA_ID3_TrackEntry_CodecInfoURL:
        pTrack.CodecInfoURL := Level3Elem.ReadValueString('CodecInfoURL');
      MATROSKA_ID3_TrackEntry_CodecDownloadURL:
        pTrack.CodecDownloadURL := Level3Elem.ReadValueString('CodecDownloadURL');
      MATROSKA_ID3_TrackEntry_CodecDecodeAll:
        pTrack.CodecDecodeAll := Level3Elem.ReadValueUINT('CodecDecodeAll');
      MATROSKA_ID3_TrackEntry_TrackOverlay:
        pTrack.TrackOverlay := Level3Elem.ReadValueUINT('TrackOverlay');
      MATROSKA_ID3_TrackEntry_Video:
        begin
          FEbmlReader.AddLog('Video :');
          FEbmlReader.IncLevel(Level3Elem);
          if ReadTrackEntryVideo(pTrack) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID3_TrackEntry_Audio:
        begin
          FEbmlReader.AddLog('Audio :');
          FEbmlReader.IncLevel(Level3Elem);
          if ReadTrackEntryAudio(pTrack) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level3Elem.Skip;
      end;
    end;
  end;

  if IsMatroskaTrackVFW(pTrack) then
  begin
    SetLength(pTrack.Fourcc, 4);
    CopyMemory(@pTrack.Fourcc[1], @pTrack.CodecPrivate[16], 4);
  end;

  if IsMatroskaTrackACM(pTrack) then
  begin
    pTrack.wFormatTag := Cardinal(pTrack.CodecPrivate[1]) shl 8;
    pTrack.wFormatTag := pTrack.wFormatTag or Cardinal(pTrack.CodecPrivate[0]);
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
    TrackList.Add(pTrack)
  else
    FreeTrackEntry(pTrack);
  Level3Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadTrackEntryVideo(pTrack : PMatroskaTrackEntry) : Boolean;
var Level4Elem : TEbmlElement;
begin
  Level4Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level4Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level4Elem.ID of
      MATROSKA_ID4_Video_PixelWidth:
        pTrack.PixelWidth := Level4Elem.ReadValueUINT('PixelWidth');
      MATROSKA_ID4_Video_PixelHeight:
        pTrack.PixelHeight := Level4Elem.ReadValueUINT('PixelHeight');
      MATROSKA_ID4_Video_DisplayWidth:
        pTrack.DisplayWidth := Level4Elem.ReadValueUINT('DisplayWidth');
      MATROSKA_ID4_Video_DisplayHeight:
        pTrack.DisplayHeight := Level4Elem.ReadValueUINT('DisplayHeight');
      else begin
        Level4Elem.Skip;
      end;
    end;
  end;

  if (pTrack.DisplayWidth = 0) and (pTrack.DisplayHeight = 0) then
  begin
    pTrack.DisplayWidth := pTrack.PixelWidth;
    pTrack.DisplayHeight := pTrack.PixelHeight;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  Level4Elem.Free;
end;

//------------------------------------------------------------------------------

function TMatroskaReader.ReadTrackEntryAudio(pTrack : PMatroskaTrackEntry) : Boolean;
var Level4Elem : TEbmlElement;
begin
  Level4Elem := TEbmlElement.Create(FEbmlReader);

	// Set default value
  with pTrack^ do
  begin
    SamplingFrequency := 8000.0;
    OutputSamplingFrequency := 0;
    Channels := 1;
  end;

  while FEbmlReader.HasMoreElement do
  begin
    if Level4Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level4Elem.ID of
      MATROSKA_ID4_Audio_SamplingFrequency:
        pTrack.SamplingFrequency := Level4Elem.ReadValueFloat('SamplingFrequency');
      MATROSKA_ID4_Audio_OutputSamplingFrequency:
        pTrack.OutputSamplingFrequency := Level4Elem.ReadValueFloat('OutputSamplingFrequency');
      MATROSKA_ID4_Audio_Channels:
        pTrack.Channels := Level4Elem.ReadValueUINT('Channels');
      MATROSKA_ID4_Audio_BitDepth:
        pTrack.BitDepth := Level4Elem.ReadValueUINT('BitDepth');
      else begin
        Level4Elem.Skip;
      end;
    end;
  end;

  if pTrack.OutputSamplingFrequency = 0 then
    pTrack.OutputSamplingFrequency := pTrack.SamplingFrequency;

  Result := (FEbmlReader.HasMoreElement = False);
  Level4Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadMetaSeekInfo : Boolean;
var Level2Elem : TEbmlElement;
begin
  Level2Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level2Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level2Elem.ID of
      MATROSKA_ID2_MetaSeek_Seek:
        begin
          FEbmlReader.AddLog('MetaSeek seek Info :');
          FEbmlReader.IncLevel(Level2Elem);
          if ReadMetaSeekSeekInfo = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level2Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  Level2Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadMetaSeekSeekInfo : Boolean;
var Level3Elem : TEbmlElement;
    pMetaSeek : PMatroskaMetaSeek;
begin
  Level3Elem := TEbmlElement.Create(FEbmlReader);

  pMetaSeek := New(PMatroskaMetaSeek);
  ZeroMemory(pMetaSeek, SizeOf(TMatroskaMetaSeek));

  while FEbmlReader.HasMoreElement do
  begin
    if Level3Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level3Elem.ID of
      MATROSKA_ID3_MetaSeek_SeekID:
        pMetaSeek.SeekID := Level3Elem.ReadValueUINT('SeekID');
      MATROSKA_ID3_MetaSeek_SeekPosition:
        pMetaSeek.SeekPosition := Level3Elem.ReadValueUINT('SeekPosition');
      else begin
        Level3Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
    MetaSeekList.Add(pMetaSeek)
  else
    Dispose(pMetaSeek);
  Level3Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadAttachements : Boolean;
var Level2Elem : TEbmlElement;
begin
  Level2Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level2Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level2Elem.ID of
      MATROSKA_ID2_AttachedFile:
        begin
          FEbmlReader.AddLog('Attached file :');
          FEbmlReader.IncLevel(Level2Elem);
          if ReadAttachedFile = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level2Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  Level2Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadAttachedFile : Boolean;
var Level3Elem : TEbmlElement;
    pAttachedFile : PMatroskaAttachedFile;
begin
  Level3Elem := TEbmlElement.Create(FEbmlReader);

  pAttachedFile := New(PMatroskaAttachedFile);
  ZeroMemory(pAttachedFile, SizeOf(TMatroskaAttachedFile));

  while FEbmlReader.HasMoreElement do
  begin
    if Level3Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level3Elem.ID of
      MATROSKA_ID3_AttachedFile_FileDescription:
        pAttachedFile.FileDescription := Level3Elem.ReadValueString('FileDescription');
      MATROSKA_ID3_AttachedFile_FileName:
        pAttachedFile.FileName := Level3Elem.ReadValueString('FileName');
      MATROSKA_ID3_AttachedFile_FileMimeType:
        pAttachedFile.FileMimeType := Level3Elem.ReadValueString('FileMimeType');
      MATROSKA_ID3_AttachedFile_FileData:
        begin
          pAttachedFile.FileDataLength := Level3Elem.DataLength;
          pAttachedFile.FileDataPosition := Level3Elem.Position +
            Level3Elem.IDLen + Level3Elem.DataLengthLen;
          Level3Elem.Skip;
        end;
      MATROSKA_ID3_AttachedFile_FileUID:
        pAttachedFile.FileUID := Level3Elem.ReadValueUINT('FileUID');
      else begin
        Level3Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
    AttachedFiles.Add(pAttachedFile)
  else
    Dispose(pAttachedFile);
  Level3Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadChapters : Boolean;
var Level2Elem : TEbmlElement;
begin
  Level2Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level2Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level2Elem.ID of
      MATROSKA_ID2_Chapters_EditionEntry:
        begin
          FEbmlReader.AddLog('Edition entry :');
          FEbmlReader.IncLevel(Level2Elem);
          if ReadChaptersEditionEntry = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level2Elem.Skip;
      end;
    end;
  end;

  FixChaptersTiming;

  Result := (FEbmlReader.HasMoreElement = False);
  Level2Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadChaptersEditionEntry : Boolean;
var Level3Elem : TEbmlElement;
    pEditionEntry : PMatroskaEditionEntry;
begin
  Level3Elem := TEbmlElement.Create(FEbmlReader);

  pEditionEntry := New(PMatroskaEditionEntry);
  ZeroMemory(pEditionEntry, SizeOf(TMatroskaEditionEntry));

  while FEbmlReader.HasMoreElement do
  begin
    if Level3Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level3Elem.ID of
      MATROSKA_ID3_EditionEntry_EditionUID:
        pEditionEntry.EditionUID := Level3Elem.ReadValueUINT('EditionUID');
      MATROSKA_ID3_EditionEntry_EditionFlagHidden:
        pEditionEntry.EditionFlagHidden := Level3Elem.ReadValueUINT('EditionFlagHidden');
      MATROSKA_ID3_EditionEntry_EditionFlagDefault:
        pEditionEntry.EditionFlagDefault := Level3Elem.ReadValueUINT('EditionFlagDefault');
      MATROSKA_ID3_EditionEntry_EditionManaged:
        pEditionEntry.EditionManaged := Level3Elem.ReadValueUINT('EditionManaged');
      MATROSKA_ID3_EditionEntry_ChapterAtom:
        begin
          FEbmlReader.AddLog('Chapter Atom :');
          FEbmlReader.IncLevel(Level3Elem);
          if ReadChapterAtom(pEditionEntry, nil) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level3Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
    EditionEntries.Add(pEditionEntry)
  else
    FreeEditionEntry(pEditionEntry);
  Level3Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadChapterAtom(pEditionEntry : PMatroskaEditionEntry;
  pParentAtom : PMatroskaChapterAtom) : Boolean;
var LevelNElem : TEbmlElement;
    pChapterAtom : PMatroskaChapterAtom;
begin
  LevelNElem := TEbmlElement.Create(FEbmlReader);

  pChapterAtom := New(PMatroskaChapterAtom);
  ZeroMemory(pChapterAtom, SizeOf(TMatroskaChapterAtom));

  while FEbmlReader.HasMoreElement do
  begin
    if LevelNElem.ReadIDAndDataLen = 0 then
      Break;

    case LevelNElem.ID of
      MATROSKA_ID4_ChapterAtom_ChapterUID:
        pChapterAtom.ChapterUID := LevelNElem.ReadValueUINT('ChapterUID');
      MATROSKA_ID4_ChapterAtom_ChapterTimeStart:
        pChapterAtom.ChapterTimeStart := LevelNElem.ReadValueUINT('ChapterTimeStart');
      MATROSKA_ID4_ChapterAtom_ChapterTimeEnd:
        pChapterAtom.ChapterTimeEnd := LevelNElem.ReadValueUINT('ChapterTimeEnd');
      MATROSKA_ID4_ChapterAtom_ChapterFlagHidden:
        pChapterAtom.ChapterFlagHidden := LevelNElem.ReadValueUINT('ChapterFlagHidden');
      MATROSKA_ID4_ChapterAtom_ChapterFlagEnabled:
        pChapterAtom.ChapterFlagEnabled := LevelNElem.ReadValueUINT('ChapterFlagEnabled');
      MATROSKA_ID4_ChapterAtom_ChapterPhysicalEquiv:
        pChapterAtom.ChapterPhysicalEquiv := LevelNElem.ReadValueUINT('ChapterPhysicalEquiv');
      MATROSKA_ID4_ChapterAtom_ChapterTrack:
        begin
          FEbmlReader.AddLog('Chapter track number :');
          FEbmlReader.IncLevel(LevelNElem);
          if ReadChapterTrackNumber(pChapterAtom) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID4_ChapterAtom_ChapterDisplay:
        begin
          FEbmlReader.AddLog('Chapter Display :');
          FEbmlReader.IncLevel(LevelNElem);
          if ReadChapterDisplay(pChapterAtom) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID3_EditionEntry_ChapterAtom:
        begin
          FEbmlReader.AddLog('Chapter Atom :');
          FEbmlReader.IncLevel(LevelNElem);
          if ReadChapterAtom(pEditionEntry, pChapterAtom) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        LevelNElem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
  begin
    if Assigned(pParentAtom) then
    begin
      if not Assigned(pParentAtom.Chapters) then
        pParentAtom.Chapters := TList.Create;
      pParentAtom.Chapters.Add(pChapterAtom);
    end
    else
    begin
      if not Assigned(pEditionEntry.Chapters) then
        pEditionEntry.Chapters := TList.Create;
      pEditionEntry.Chapters.Add(pChapterAtom);
    end;
  end
  else
    FreeChapterAtom(pChapterAtom);
  LevelNElem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadChapterDisplay(pChapterAtom : PMatroskaChapterAtom) : Boolean;
var LevelNElem : TEbmlElement;
    pChapterDisplay : PMatroskaChapterDisplay;
begin
  LevelNElem := TEbmlElement.Create(FEbmlReader);

  pChapterDisplay := New(PMatroskaChapterDisplay);
  ZeroMemory(pChapterDisplay, SizeOf(TMatroskaChapterDisplay));

  while FEbmlReader.HasMoreElement do
  begin
    if LevelNElem.ReadIDAndDataLen = 0 then
      Break;

    case LevelNElem.ID of
      MATROSKA_ID5_ChapterDisplay_ChapString:
         pChapterDisplay.ChapString := LevelNElem.ReadValueString('ChapString');
      MATROSKA_ID5_ChapterDisplay_ChapLanguage:
         pChapterDisplay.ChapLanguage := LevelNElem.ReadValueString('ChapLanguage');
      MATROSKA_ID5_ChapterDisplay_ChapCountry:
         pChapterDisplay.ChapCountry := LevelNElem.ReadValueString('ChapCountry');
      else begin
        LevelNElem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
  begin
    if not Assigned(pChapterAtom.ChapterDisplay) then
      pChapterAtom.ChapterDisplay := TList.Create;
    pChapterAtom.ChapterDisplay.Add(pChapterDisplay);
  end
  else
    Dispose(pChapterDisplay);
  LevelNElem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadChapterTrackNumber(pChapterAtom : PMatroskaChapterAtom) : Boolean;
var LevelNElem : TEbmlElement;
begin
  LevelNElem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if LevelNElem.ReadIDAndDataLen = 0 then
      Break;

    case LevelNElem.ID of
      MATROSKA_ID5_ChapterAtom_ChapterTrackNumber:
        begin
          if pChapterAtom.ChapterTrackNumber < Length(pChapterAtom.ChapterTrack) then
          begin
            pChapterAtom.ChapterTrack[pChapterAtom.ChapterTrackNumber] := LevelNElem.ReadValueUINT('ChapterTrackNumber');
            Inc(pChapterAtom.ChapterTrackNumber);
          end
          else
            LevelNElem.Skip;
        end;
      else begin
        LevelNElem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  LevelNElem.Free;
end;

// -----------------------------------------------------------------------------

procedure FixChaptersListTiming(ChaptersList : TList; EndTime : Int64);
var pLastChapter, pChapter, pNextChapter : PMatroskaChapterAtom;
    i : integer;
begin
  if Assigned(ChaptersList) and (ChaptersList.Count > 0) then
  begin
    pLastChapter := ChaptersList[ChaptersList.Count-1];
    if pLastChapter.ChapterTimeEnd = 0 then
      pLastChapter.ChapterTimeEnd := EndTime;
    for i:=0 to ChaptersList.Count-2 do
    begin
      pChapter := ChaptersList[i];
      pNextChapter := ChaptersList[i+1];
      if pChapter.ChapterTimeEnd = 0 then
        pChapter.ChapterTimeEnd := pNextChapter.ChapterTimeStart;
      FixChaptersListTiming(pChapter.Chapters, pChapter.ChapterTimeEnd);
    end;
  end;
end;

procedure TMatroskaReader.FixChaptersTiming;
var i : integer;
    pEditionEntry : PMatroskaEditionEntry;
begin
  for i:=0 to EditionEntries.Count-1 do
  begin
    pEditionEntry := EditionEntries[i];
    FixChaptersListTiming(pEditionEntry.Chapters,Round(SegmentInfo.Duration*1000000));
  end;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadTags : Boolean;
var Level2Elem : TEbmlElement;
begin
  Level2Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level2Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level2Elem.ID of
      MATROSKA_ID2_Tag:
        begin
          FEbmlReader.AddLog('Tag :');
          FEbmlReader.IncLevel(Level2Elem);
          if ReadTag = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level2Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  Level2Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadTag : Boolean;
var Level3Elem : TEbmlElement;
    pTag : PMatroskaTag;
begin
  Level3Elem := TEbmlElement.Create(FEbmlReader);

  pTag := New(PMatroskaTag);
  ZeroMemory(pTag, SizeOf(TMatroskaTag));

  while FEbmlReader.HasMoreElement do
  begin
    if Level3Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level3Elem.ID of
      MATROSKA_ID3_Tag_Targets:
        begin
          FEbmlReader.AddLog('Targets :');
          FEbmlReader.IncLevel(Level3Elem);
          if ReadTagTargets(pTag) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      MATROSKA_ID3_SimpleTag:
        begin
          FEbmlReader.AddLog('SimpleTag :');
          FEbmlReader.IncLevel(Level3Elem);
          if ReadSimpleTag(pTag, nil) = False then
            Break;
          FEbmlReader.DecLevel;
        end
      else begin
        Level3Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
    Tags.Add(pTag)
  else
    FreeTag(pTag);
  Level3Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadTagTargets(pTag : PMatroskaTag) : Boolean;
var Level4Elem : TEbmlElement;
begin
  Level4Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level4Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level4Elem.ID of
      MATROSKA_ID4_Targets_TargetTypeValue:
        pTag.Target.TargetTypeValue := Level4Elem.ReadValueUINT('TargetTypeValue');
      MATROSKA_ID4_Targets_TargetType:
        pTag.Target.TargetType := Level4Elem.ReadValueString('TargetType');
      MATROSKA_ID4_Targets_TrackUID:
        pTag.Target.TrackUID := Level4Elem.ReadValueUINT('TrackUID');
      MATROSKA_ID4_Targets_EditionUID:
        pTag.Target.EditionUID := Level4Elem.ReadValueUINT('EditionUID');
      MATROSKA_ID4_Targets_ChapterUID:
        pTag.Target.ChapterUID := Level4Elem.ReadValueUINT('ChapterUID');
      MATROSKA_ID4_Targets_AttachmentUID:
        pTag.Target.AttachmentUID := Level4Elem.ReadValueUINT('AttachmentUID');
      else begin
        Level4Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  Level4Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadSimpleTag(pParentTag : PMatroskaTag; pParentSimpleTag : PMatroskaSimpleTag) : Boolean;
var LevelNElem : TEbmlElement;
    pSimpleTag : PMatroskaSimpleTag;
begin
  LevelNElem := TEbmlElement.Create(FEbmlReader);

  pSimpleTag := New(PMatroskaSimpleTag);
  ZeroMemory(pSimpleTag, SizeOf(TMatroskaSimpleTag));

  while FEbmlReader.HasMoreElement do
  begin
    if LevelNElem.ReadIDAndDataLen = 0 then
      Break;

    case LevelNElem.ID of
      MATROSKA_ID4_SimpleTag_TagName:
        pSimpleTag.TagName := LevelNElem.ReadValueString('TagName');
      MATROSKA_ID4_SimpleTag_TagLanguage:
        pSimpleTag.TagLanguage := LevelNElem.ReadValueString('TagLanguage');
      MATROSKA_ID4_SimpleTag_TagDefault:
        pSimpleTag.TagDefault := LevelNElem.ReadValueUINT('TagDefault');
      MATROSKA_ID4_SimpleTag_TagString:
        pSimpleTag.TagString := LevelNElem.ReadValueString('TagString');
      MATROSKA_ID3_SimpleTag:
        begin
          FEbmlReader.AddLog('SimpleTag :');
          FEbmlReader.IncLevel(LevelNElem);
          if ReadSimpleTag(pParentTag, pSimpleTag) = False then
            Break;
          FEbmlReader.DecLevel;
        end
      else begin
        LevelNElem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
  begin
    if Assigned(pParentSimpleTag) then
    begin
      if not Assigned(pParentSimpleTag.SubTags) then
        pParentSimpleTag.SubTags := TList.Create;
      pParentSimpleTag.SubTags.Add(pSimpleTag);
    end
    else
    begin
      if not Assigned(pParentTag.SimpleTags) then
        pParentTag.SimpleTags := TList.Create;
      pParentTag.SimpleTags.Add(pSimpleTag);
    end
  end
  else
    FreeSimpleTag(pSimpleTag);
  LevelNElem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadCues : Boolean;
var Level2Elem : TEbmlElement;
begin
  Level2Elem := TEbmlElement.Create(FEbmlReader);

  while FEbmlReader.HasMoreElement do
  begin
    if Level2Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level2Elem.ID of
      MATROSKA_ID2_CuePoint:
        begin
          FEbmlReader.AddLog('Cue point :');
          FEbmlReader.IncLevel(Level2Elem);
          if ReadCuePoint = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level2Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  Level2Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadCuePoint : Boolean;
var Level3Elem : TEbmlElement;
    pCuePoint : PMatroskaCuePoint;
begin
  Level3Elem := TEbmlElement.Create(FEbmlReader);

  pCuePoint := New(PMatroskaCuePoint);
  ZeroMemory(pCuePoint, SizeOf(TMatroskaCuePoint));

  while FEbmlReader.HasMoreElement do
  begin
    if Level3Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level3Elem.ID of
      MATROSKA_ID3_CueTime:
        begin
          pCuePoint.CueTime := Level3Elem.ReadValueUINT('CueTime');
        end;
      MATROSKA_ID3_CueTrackPosition:
        begin
          FEbmlReader.AddLog('CueTrackPosition :');
          FEbmlReader.IncLevel(Level3Elem);
          if ReadCueTrackPosition(pCuePoint) = False then
            Break;
          FEbmlReader.DecLevel;
        end;
      else begin
        Level3Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
    Cues.Add(pCuePoint)
  else
    FreeCuePoint(pCuePoint);
  Level3Elem.Free;
end;

// -----------------------------------------------------------------------------

function TMatroskaReader.ReadCueTrackPosition(pCuePoint : PMatroskaCuePoint) : Boolean;
var Level4Elem : TEbmlElement;
    pCueTrackPosition : PMatroskaCueTrackPosition;
begin
  Level4Elem := TEbmlElement.Create(FEbmlReader);

  pCueTrackPosition := New(PMatroskaCueTrackPosition);
  ZeroMemory(pCueTrackPosition, SizeOf(PCueTrackPosition));

  while FEbmlReader.HasMoreElement do
  begin
    if Level4Elem.ReadIDAndDataLen = 0 then
      Break;

    case Level4Elem.ID of
      MATROSKA_ID4_CueTrack:
        begin
          pCueTrackPosition.CueTrack := Level4Elem.ReadValueUINT('CueTrack');
        end;
      else begin
        Level4Elem.Skip;
      end;
    end;
  end;

  Result := (FEbmlReader.HasMoreElement = False);
  if (Result) then
  begin
    if not Assigned(pCuePoint.CueTrackPositions) then
      pCuePoint.CueTrackPositions := TList.Create;
    pCuePoint.CueTrackPositions.Add(pCueTrackPosition);
  end
  else
    Dispose(pCueTrackPosition);
  Level4Elem.Free;
end;

// -----------------------------------------------------------------------------
end.
// -----------------------------------------------------------------------------
