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

unit SuggestionFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, VirtualTrees, SubStructUnit, Menus,
  TntMenus;

type
  TSuggestionTreeData = record
    Range : TSubtitleRange;
    StartTime, StopTime : Integer; // In case 'Range' doesn't exist
    Text : WideString;
    AddTime : Cardinal;
  end;
  PSuggestionTreeData = ^TSuggestionTreeData;

  TSuggestionForm = class(TForm)
    vtvSuggestionsLst: TVirtualDrawTree;
    MemoSuggPreview: TTntMemo;
    TntPopupMenu1: TTntPopupMenu;
    pmiClearAll: TTntMenuItem;
    pmiClearSelected: TTntMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtvSuggestionsLstDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure pmiClearAllClick(Sender: TObject);
    procedure vtvSuggestionsLstDblClick(Sender: TObject);
    procedure pmiClearSelectedClick(Sender: TObject);
    procedure vtvSuggestionsLstFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FCriticalSection : TRtlCriticalSection;
    FBoldFont : HFONT;
    FNormalFont : HFONT;
    
    function CalculateNodeHeight : Integer;
  public
    { Public declarations }
    procedure AddSuggestion(Range : TSubtitleRange;
      Start, Stop : Integer; Text : WideString);
    procedure Clear;
    procedure UpdateFonts;    
  end;

var
  SuggestionForm: TSuggestionForm;

const
  NodeLeftMargin : Integer = 10;
  NodeTopMargin : Integer = 2;
  NodeInterline : Double = 1.1;

implementation

uses MiscToolsUnit, Main;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TSuggestionForm.FormCreate(Sender: TObject);
begin
  with vtvSuggestionsLst do
  begin
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 8;
    NodeDataSize := SizeOf(TSuggestionTreeData);
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReportMode];
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines,toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
  end;
  UpdateFonts;
  vtvSuggestionsLst.DefaultNodeHeight := CalculateNodeHeight;
  InitializeCriticalSection(FCriticalSection);
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.UpdateFonts;
var FontLOG : LOGFONT;
begin
  if (FBoldFont <> 0) then
    DeleteObject(FBoldFont);
  if (FNormalFont <> 0) then
    DeleteObject(FNormalFont);
  ZeroMemory(@FontLOG, SizeOf(FontLOG));
  StrCopy(FontLOG.lfFaceName,PChar(vtvSuggestionsLst.Canvas.Font.Name));
  FontLOG.lfHeight := vtvSuggestionsLst.Canvas.Font.Height;
  FontLOG.lfCharSet := vtvSuggestionsLst.Canvas.Font.Charset;
  FontLOG.lfWeight := FW_BOLD;
  FBoldFont := CreateFontIndirect(FontLOG);
  FontLOG.lfWeight := FW_NORMAL;
  FNormalFont := CreateFontIndirect(FontLOG);
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.FormDestroy(Sender: TObject);
begin
  if (FBoldFont <> 0) then
    DeleteObject(FBoldFont);
  if (FNormalFont <> 0) then
    DeleteObject(FNormalFont);
  DeleteCriticalSection(FCriticalSection);
end;

//------------------------------------------------------------------------------

function TSuggestionForm.CalculateNodeHeight : Integer;
var OldFont : HFONT;
    DC : HDC;
    xysize : Size;
begin
  Result := 0;
  DC := vtvSuggestionsLst.Canvas.Handle;
  Result := Result + NodeTopMargin;
  OldFont := SelectObject(DC, FBoldFont);
  GetTextExtentPoint32W(DC, 'W', 1, xysize);
  Result := Round(Result + (xysize.cy * NodeInterline));
  SelectObject(DC, FNormalFont);
  GetTextExtentPoint32W(DC, 'W', 1, xysize);
  Result := Round(Result + (xysize.cy * NodeInterline));
  SelectObject(DC, OldFont);
  Result := Result + NodeTopMargin;
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.vtvSuggestionsLstDrawNode(
  Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var x, y : Integer;
    OldColor : Cardinal;
    OldFont : HFONT;
    DC : HDC;
    Msg : WideString;
    xysize : Size;
    pSuggestionData : PSuggestionTreeData;
begin
  pSuggestionData := vtvSuggestionsLst.GetNodeData(PaintInfo.Node);

  vtvSuggestionsLst.Canvas.Lock;
  DC := PaintInfo.Canvas.Handle;
  OldColor := GetTextColor(DC);

  x := NodeLeftMargin;
  y := NodeTopMargin;
  
  if (vtvSuggestionsLst.Focused) and (vtvSuggestionsLst.Selected[PaintInfo.Node]) then
    SetTextColor(DC, ColorToRGB(clHighlightText))
  else
    SetTextColor(DC, ColorToRGB(clWindowText));
  SetBKMode(DC, TRANSPARENT);

  OldFont := SelectObject(DC, FBoldFont);

  if Assigned(pSuggestionData.Range) then
  begin
    Msg := 'From ' + TimeMsToString(pSuggestionData.Range.StartTime) + ' to ' +
      TimeMsToString(pSuggestionData.Range.StopTime);
  end
  else
  begin
    Msg := 'From ' + TimeMsToString(pSuggestionData.StartTime) + ' to ' +
      TimeMsToString(pSuggestionData.StopTime);
  end;
  TextOutW(DC, x, y, PWideChar(Msg), Length(Msg));
  GetTextExtentPoint32W(DC, PWideChar(Msg), Length(Msg), xysize);
  y := Round(y + (xysize.cy * NodeInterline));

  SelectObject(DC, FNormalFont);
  Msg := StringConvertCRLFToPipe(pSuggestionData.Text);
  TextOutW(DC, x, y, PWideChar(Msg), Length(Msg));

  if (vtvSuggestionsLst.FocusedNode = PaintInfo.Node) then
    DrawFocusRect(DC, PaintInfo.CellRect);

  SelectObject(DC, OldFont);
  SetTextColor(DC, OldColor);
  vtvSuggestionsLst.Canvas.Unlock;
end;

//------------------------------------------------------------------------------

procedure TSuggestionForm.AddSuggestion(Range : TSubtitleRange;
  Start, Stop : Integer; Text : WideString);
var Node : PVirtualNode;
    pSuggestion : PSuggestionTreeData;
begin
  EnterCriticalSection(FCriticalSection);
  Node := vtvSuggestionsLst.AddChild(nil);
  pSuggestion := vtvSuggestionsLst.GetNodeData(Node);
  pSuggestion.StartTime := Start;
  pSuggestion.StopTime := Stop;
  pSuggestion.Range := Range;
  pSuggestion.Text := Text;
  pSuggestion.AddTime := GetTickCount;
  LeaveCriticalSection(FCriticalSection);
  vtvSuggestionsLst.Repaint;
  MainForm.ShowStatusBarMessage(
    Format('We just received a new suggestion from the network. (Total : %d)',
    [vtvSuggestionsLst.ChildCount[nil]]) );
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.pmiClearAllClick(Sender: TObject);
var Node : PVirtualNode;
    pSuggestion : PSuggestionTreeData;
    DeleteLst : TList;
    i : integer;
begin
  DeleteLst := TList.Create;
  EnterCriticalSection(FCriticalSection);
  Node := vtvSuggestionsLst.GetFirst;
  while Assigned(Node) do
  begin
    pSuggestion := vtvSuggestionsLst.GetNodeData(Node);
    if ((GetTickCount - pSuggestion.AddTime) > 2000) then
      DeleteLst.Add(Node);
    Node := vtvSuggestionsLst.GetNext(Node);
  end;
  for i:=DeleteLst.Count-1 downto 0 do
  begin
    vtvSuggestionsLst.DeleteNode(DeleteLst[i]);
  end;
  LeaveCriticalSection(FCriticalSection);
  vtvSuggestionsLst.FocusedNode := nil;
  vtvSuggestionsLst.Repaint;
  MemoSuggPreview.Text := '';
  DeleteLst.Free;
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.vtvSuggestionsLstDblClick(Sender: TObject);
var pSuggestion : PSuggestionTreeData;
begin
  if Assigned(vtvSuggestionsLst.FocusedNode) then
  begin
    pSuggestion := vtvSuggestionsLst.GetNodeData(vtvSuggestionsLst.FocusedNode);
    MainForm.SelectNodeFromTimestamps(pSuggestion.StartTime,pSuggestion.StopTime);
  end
end;

//------------------------------------------------------------------------------

procedure TSuggestionForm.Clear;
begin
  EnterCriticalSection(FCriticalSection);
  vtvSuggestionsLst.Clear;
  LeaveCriticalSection(FCriticalSection);
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.pmiClearSelectedClick(Sender: TObject);
var Node, NextNodeToSelect : PVirtualNode;
    pSuggestion : PSuggestionTreeData;
    DeleteLst : TList;
    i : integer;
    CurrentTime : Cardinal;
begin
  CurrentTime := GetTickCount;
  DeleteLst := TList.Create;
  NextNodeToSelect := nil;
  
  EnterCriticalSection(FCriticalSection);

  // Find next unselected node or a new node that we aren't going to delete 
  Node := vtvSuggestionsLst.GetFirstSelected;
  while Assigned(Node) do
  begin
    pSuggestion := vtvSuggestionsLst.GetNodeData(Node);
    if (not vtvSuggestionsLst.Selected[Node]) or
      ((CurrentTime - pSuggestion.AddTime) <= 2000) then
    begin
      NextNodeToSelect := Node;
      Break;
    end;
    Node := vtvSuggestionsLst.GetNext(Node);
  end;

  // If not find, then try previous node
  Node := vtvSuggestionsLst.GetFirstSelected;
  if (Assigned(Node) = True) and
     (Assigned(NextNodeToSelect) = False) then
  begin
    NextNodeToSelect := vtvSuggestionsLst.GetPrevious(Node);
  end;

  // Build a list of node to delete (older than 2s)
  while Assigned(Node) do
  begin
    pSuggestion := vtvSuggestionsLst.GetNodeData(Node);
    if ((CurrentTime - pSuggestion.AddTime) > 2000) then
      DeleteLst.Add(Node);
    Node := vtvSuggestionsLst.GetNextSelected(Node);
  end;

  // Delete nodes
  for i:=DeleteLst.Count-1 downto 0 do
  begin
    vtvSuggestionsLst.DeleteNode(DeleteLst[i]);
  end;
  
  LeaveCriticalSection(FCriticalSection);

  // Try to select a new node
  vtvSuggestionsLst.FocusedNode := NextNodeToSelect;
  if Assigned(vtvSuggestionsLst.FocusedNode) then
    vtvSuggestionsLst.Selected[vtvSuggestionsLst.FocusedNode] := True;
  vtvSuggestionsLst.Repaint;
  DeleteLst.Free;
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.vtvSuggestionsLstFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var Data : PSuggestionTreeData;
begin
  if Assigned(Node) then
  begin
    Data := vtvSuggestionsLst.GetNodeData(Node);
    MemoSuggPreview.Text := Data.Text;
  end
  else
    MemoSuggPreview.Text := '';
end;

// -----------------------------------------------------------------------------

procedure TSuggestionForm.FormKeyDown(Sender: TObject; var Key: Word;
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
