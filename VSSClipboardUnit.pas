unit VSSClipboardUnit;

interface

uses VirtualTrees, Classes, SubStructUnit;

procedure CopyVTVToClipboard(vtv: TVirtualStringTree);
procedure PasteClipboard(SubList: TList; SubRangeFactory : TSubtitleRangeFactory);

var
  VSSClipBoardFORMAT : Cardinal;

const
  CopyPasteStreamVersion = 1;

implementation

uses Windows, SysUtils, MiscToolsUnit, TntClipBrd, TntSysUtils, SRTParserUnit, SSAParserUnit, TntSystem;

//==================================================================================================

procedure CopyVTVToClipboard(vtv: TVirtualStringTree);
var Node : PVirtualNode;
    NodeData : PTreeData;
    Msg : WideString;
    SubtitleRange : TSubtitleRange;
    MemStream : TMemoryStream;
    MemHandle : THandle;
    MemPointer : Pointer;
begin
  if (vtv.SelectedCount <= 0) then
    Exit;

  MemStream := TMemoryStream.Create;
  SaveToStreamInt(MemStream, CopyPasteStreamVersion);
  SaveToStreamInt(MemStream, vtv.SelectedCount);
  Node := vtv.GetFirstSelected;
  while Assigned(Node) do
  begin
    NodeData := vtv.GetNodeData(Node);
    SubtitleRange := NodeData.Range;
    // TODO : handle SSA format copy 
    Msg := Msg + Sub2SrtString(SubtitleRange) + CRLF + CRLF;
    SubtitleRange.SaveToStream(MemStream);
    Node := vtv.GetNextSelected(Node);
  end;
  if (Length(Msg) > 0) then
  begin
    TntClipboard.Open;
    try
      TntClipboard.Clear;
      // Set text format data
      TntClipboard.AsWideText := Trim(Msg);
      // Set custom format data
      MemHandle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, MemStream.Size);
      MemPointer := GlobalLock(MemHandle);
      MemStream.Position := 0;
      MemStream.ReadBuffer(MemPointer^, MemStream.Size);
      TntClipboard.SetAsHandle(VSSClipBoardFORMAT, MemHandle);
      GlobalUnLock(MemHandle);
    finally
      TntClipboard.Close;
    end;
  end;
  MemStream.Free;
end;

// -------------------------------------------------------------------------------------------------

procedure PasteClipboardVSS(SubList: TList);
var MemHandle : THandle;
    MemPointer : Pointer;
    MemStream : TMemoryStream;

    StreamVersion, SubCount, i : Integer;
    SubRange : TSubtitleRange;
begin
  MemStream := TMemoryStream.Create;
  TntClipboard.Open;
  MemHandle := TntClipboard.GetAsHandle(VSSClipBoardFORMAT);
  MemPointer := GlobalLock(MemHandle);
  MemStream.WriteBuffer(MemPointer^, GlobalSize(MemHandle));
  GlobalUnlock(MemHandle);
  TntClipboard.Close;

  MemStream.Position := 0;
  LoadFromStreamInt(MemStream, StreamVersion);
  if (StreamVersion = CopyPasteStreamVersion) then
  begin
    // Load each sub
    LoadFromStreamInt(MemStream, SubCount);
    for i := 0 to SubCount-1 do
    begin
      SubRange := TSubtitleRange.Create;
      SubRange.LoadFromStream(MemStream);
      SubList.Add(SubRange);
    end;
  end;
  MemStream.Free;
end;

// -------------------------------------------------------------------------------------------------

procedure TryPasteAsSRT(MemStream : TMemoryStream; SubList: TList; SubRangeFactory : TSubtitleRangeFactory);
var SRTParser : TSRTParser;
    i : Integer;
    SRTSub : TSRTSubtitle;
    Sub : TSubtitleRange;
begin
  if (MemStream.Size > 0) then
  begin
    MemStream.Position := 0;
    SRTParser := TSRTParser.Create;
    SRTParser.Load(MemStream);
    for i := 0 to SRTParser.GetCount - 1 do
    begin
      SRTSub := SRTParser.GetAt(i);
      Sub := TSubtitleRange(SubRangeFactory.CreateRangeSS(SRTSub.Start, SRTSub.Stop));
      Sub.Text := SRTSub.Text;
      SubList.Add(Sub);
    end;
    SRTParser.Free;
  end;
end;

procedure TryPasteAsSSA(MemStream : TMemoryStream; SubList: TList; SubRangeFactory : TSubtitleRangeFactory);
var SSAParser : TSSAParser;
    i, Start, Stop : Integer;
    SubText, Layer, Marked : WideString;
    SubRange : TSubtitleRange;
begin
  if (MemStream.Size > 0) then
  begin
    MemStream.Position := 0;
    // Try SSA
    SSAParser := TSSAParser.Create;
    // TODO : better partial SSA parsing, for now you need to parse the full events section
    // [Events]
    // Format: Marked, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
    // Dialogue: Marked=0,0:00:19.72,0:00:20.29,Default,,0000,0000,0000,,just a sample
    //
    // TODO : copy/paste styles
    //
    SSAParser.Load(MemStream);
    for i := 0 to SSAParser.GetDialoguesCount - 1 do
    begin
      Start := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'Start'));
      Stop := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'End'));
      if (Start <> -1) and (Stop <> -1) then
      begin
        SubRange := TSubtitleRange(SubRangeFactory.CreateRangeSS(Start, Stop));
        SubText := ssaParser.GetDialogueValueAsString(i, 'Text');
        SubRange.Text := Tnt_WideStringReplace(SubText, '\N', CRLF,[rfReplaceAll]);
        // SSA or ASS ? (SSA use 'Marked',  ASS use 'Layer')
        Marked := ssaParser.GetDialogueValueAsString(i, 'Marked');
        if Length(Marked) > 0 then
          SubRange.Marked := Marked;
        Layer := ssaParser.GetDialogueValueAsString(i, 'Layer');
        if Length(Layer) > 0 then
          SubRange.Layer := Layer;
        SubRange.Effect := ssaParser.GetDialogueValueAsString(i, 'Effect');
        SubRange.RightMarg := ssaParser.GetDialogueValueAsString(i, 'MarginR');
        SubRange.LeftMarg := ssaParser.GetDialogueValueAsString(i, 'MarginL');
        SubRange.VertMarg := ssaParser.GetDialogueValueAsString(i, 'MarginV');
        SubRange.Style := ssaParser.GetDialogueValueAsString(i, 'Style');
        SubRange.Actor := ssaParser.GetDialogueValueAsString(i, 'Name');
        SubRange.UpdateSubTimeFromText(SubRange.Text);
        SubList.Add(SubRange);
      end;
    end;
    SSAParser.Free;
  end;
end;


procedure PasteClipboardText(SubList: TList; SubRangeFactory : TSubtitleRangeFactory);
var MemStream : TMemoryStream;
    BOM: WideChar;
    CBText : WideString;
begin
  MemStream := TMemoryStream.Create;

  // Copy cliboard into a stream
  BOM := UNICODE_BOM;
  MemStream.Write(BOM, SizeOf(WideChar));
  CBText := TntClipboard.AsWideText;
  MemStream.Write(PWideChar(CBText)^, (Length(CBText) + 1) * SizeOf(WideChar));

  TryPasteAsSRT(MemStream, SubList, SubRangeFactory);
  if (SubList.Count = 0) then
  begin
    TryPasteAsSSA(MemStream, SubList, SubRangeFactory);
  end;
  
  MemStream.Free;
end;

procedure PasteClipboard(SubList: TList; SubRangeFactory : TSubtitleRangeFactory);
begin
  // First try VSS custom clipboard format 
  if TntClipboard.HasFormat(VSSClipBoardFORMAT) then
  begin
    PasteClipboardVSS(SubList);
  end;
  // If no subtitles found try text format
  if (SubList.Count = 0) and (TntClipboard.HasFormat(CF_TEXT) or TntClipboard.HasFormat(CF_UNICODETEXT)) then
  begin
    PasteClipboardText(SubList, SubRangeFactory);
  end;
end;

//==================================================================================================

initialization
  VSSClipBoardFORMAT := RegisterClipboardFormat(PChar('CF_VisualSubSync'));

//==================================================================================================
end.
//==================================================================================================
