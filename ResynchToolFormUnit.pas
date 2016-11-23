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

unit ResynchToolFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ComCtrls, TntComCtrls, Menus, TntMenus, StdCtrls,
  TntStdCtrls, SubStructUnit, UndoableTaskUnit, UndoableSubTaskUnit,
  TntDialogs, Buttons, TntButtons, ExtCtrls, TntExtCtrls;

type

  TResynchToolForm = class(TForm)
    vtvPoSList: TVirtualDrawTree;
    TntStatusBar1: TTntStatusBar;
    RTPopupMenu: TTntPopupMenu;
    DeleteselectedPoS1: TTntMenuItem;
    Panel1: TPanel;
    GotoSelectedNodeButton: TButton;
    GotoSuggestedNodeButton: TButton;

    procedure FormCreate(Sender: TObject);
    procedure vtvPoSListDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure FormDestroy(Sender: TObject);
    procedure vtvPoSListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtvPoSListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure DeleteselectedPoS1Click(Sender: TObject);
    procedure GotoSelectedNodeButtonClick(Sender: TObject);
    procedure vtvPoSListDblClick(Sender: TObject);
    procedure GotoSuggestedNodeButtonClick(Sender: TObject);

  private
    { Private declarations }
    BoldFont : HFONT;
    NormalFont : HFONT;
    MinimumBlank : Integer;
    NullPen : HPEN;
    AlterBrush : HBRUSH;

    procedure CreateFont;
    function CalculateNodeHeight : Integer;

    function Resynch : TUndoableMultiPurposeTask;
    procedure Delete;

  public
    { Public declarations used by UndoableExtraTaskUnit }
    function AddPoSListNode( NewNodeData : PPosTreeData ) : Integer;
    function UpdatePoSListNode( NodeIndex, Cursor : Integer ) : Integer;
    function DeletePoSListNode( Node : Integer ) : PPosTreeData;

    procedure ColorSubtitles;

    { Public declarations }
    function SetPos( Range : TSubtitleRange; Cursor : Integer ) : TUndoableMultiPurposeTask;
    function DeletePos( Range : TSubtitleRange ) : TUndoableMultiPurposeTask;
    function Reset : TUndoableMultiPurposeTask;
    procedure Clear;
  end;

var
  ResynchToolForm: TResynchToolForm;

const
  //VirtualTree Draw Parameters
  NodeLeftMargin : Integer = 10;
  NodeRightMargin : Integer = 50;
  NodeTopMargin : Integer = 10;
  NodeInterline : Double = 1.1;

  //Subtitle coloration array
  Colors : Array [0..2] of TColor = ( TColor($0099CC00),
                                      TColor($00FF3232),
                                      TColor($0032FFFF));

implementation

uses
  Types, Main, MiscToolsUnit, GlobalUnit, LogWindowFormUnit,
  TntForms, TntIniFiles, TntClasses, TntSysUtils;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TResynchToolForm.Clear;
begin
  vtvPosList.Clear;
end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.FormCreate( Sender: TObject );
begin
  with vtvPoSList do
  begin
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 8;
    NodeDataSize := SizeOf( TPoSTreeData );
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReportMode];
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines, toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
  end;
  CreateFont;
  vtvPoSList.DefaultNodeHeight := CalculateNodeHeight;

  NullPen := CreatePen( PS_NULL, 0, 0 );
  AlterBrush := CreateSolidBrush( clMoneyGreen );
  MinimumBlank := MainForm.ConfigObject.SpaceKeyBlankBetweenSubtitles;
  Clear;
end;

//------------------------------------------------------------------------------

function TResynchToolForm.CalculateNodeHeight : Integer;
var OldFont : HFONT;
    DC : HDC;
    xysize : Size;
begin
  Result := 0;
  DC := vtvPoSList.Canvas.Handle;
  Result := Result + NodeTopMargin;
  OldFont := SelectObject( DC, BoldFont );
  GetTextExtentPoint32W( DC, 'W', 1, xysize );
  Result := Round(Result + (xysize.cy * NodeInterline));
  SelectObject(DC, NormalFont);
  GetTextExtentPoint32W(DC, 'W', 1, xysize);
  Result := Round(Result + (xysize.cy * NodeInterline));
  SelectObject(DC, OldFont);
  Result := Result + NodeTopMargin;
end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.CreateFont;
var FontLOG : LOGFONT;
begin
  if BoldFont <> 0 then
    DeleteObject(BoldFont);
  if NormalFont <> 0 then
    DeleteObject(NormalFont);
  ZeroMemory(@FontLOG, SizeOf(FontLOG));
  StrCopy(FontLOG.lfFaceName,PChar(vtvPoSList.Canvas.Font.Name));
  FontLOG.lfHeight := vtvPoSList.Canvas.Font.Height;
  FontLOG.lfCharSet := vtvPoSList.Canvas.Font.Charset;
  FontLOG.lfWeight := FW_BOLD;
  BoldFont := CreateFontIndirect(FontLOG);
  FontLOG.lfWeight := FW_NORMAL;
  NormalFont := CreateFontIndirect(FontLOG);
end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.vtvPoSListDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var x, y : Integer;
    OldColor : Cardinal;
    OldFont : HFONT;
    DC : HDC;
    Msg : WideString;
    xysize : Size;
    pPoSData : PPoSTreeData;
    OldBrush : HBRUSH;
    OldPen : HPEN;
    Cur,PrevCur : PVirtualNode;
    pPoSDataPrev : PPoSTreeData;
    SuggestedResynchSub : word;
    DimPreviousArea : word;
begin

  pPoSData := vtvPoSList.GetNodeData(PaintInfo.Node);

  Cur := PaintInfo.Node;
  PrevCur := vtvPoSList.GetPrevious(Cur);

  SuggestedResynchSub := 0;
  DimPreviousArea := 0;
  if PrevCur <> nil then
   begin
    pPoSDataPrev := vtvPoSList.GetNodeData(PrevCur);
    SuggestedResynchSub := (pPoSDataPrev.Range.Node.Index + 1) + ( (pPoSData.Range.Node.Index + 1) - (pPoSDataPrev.Range.Node.Index + 1) ) div 2;
    DimPreviousArea := pPoSData.Range.Node.Index - pPoSDataPrev.Range.Node.Index + 1;
   end;

  vtvPoSList.Canvas.Lock;
  DC := PaintInfo.Canvas.Handle;

  OldColor := GetTextColor(DC);

  x := NodeLeftMargin;
  y := NodeTopMargin + 6;

  if (vtvPoSList.Focused) and (vtvPoSList.Selected[PaintInfo.Node]) then
    SetTextColor(DC, ColorToRGB(clHighlightText))
  else
    SetTextColor(DC, ColorToRGB(clWindowText));

  //alternate background colors
  if (PaintInfo.Node.Index mod 2 = 1)
  And (vtvPoSList.FocusedNode <> PaintInfo.Node) then
  begin
    OldPen := SelectObject(DC, NullPen);
    OldBrush := SelectObject(DC, AlterBrush);
    Rectangle(DC, -1, -1, vtvPoSList.Width , vtvPoSList.DefaultNodeHeight);
    SelectObject(DC, OldPen);
    SelectObject(DC, OldBrush);
  end;

  SetBKMode(DC, TRANSPARENT);

  //First line - Point of synch index
  OldFont := SelectObject(DC, BoldFont);
  Msg := Format('%3d: ', [pPoSData.Range.Node.Index + 1]);
  GetTextExtentPoint32W(DC, PWideChar(Msg), Length(Msg), xysize);
  TextOutW(DC, x , y, PWideChar(Msg), Length(Msg));

  //First line - Point of synch old start -> new start
  SelectObject(DC, NormalFont);
  Msg := TimeMsToString(pPoSData.Range.StartTime) + ' -> ' + TimeMsToString(pPoSData.Cursor);
  TextOutW(DC, x + xysize.cx , y , PWideChar(Msg), Length(Msg));

  //Second line - Subtitle text
  y := Round(y + (xysize.cy * NodeInterline));
  Msg := StringConvertCRLFToPipe(pPoSData.Range.Text);
  TextOutW(DC, x, y, PWideChar(Msg), Length(Msg));

  if SuggestedResynchSub>0 then
   begin
    SetTextColor(DC, ColorToRGB(clNavy));
    SetBkColor(DC, ColorToRGB(clYellow)); SetBkMode(DC, Opaque);
    Msg := '** suggest to check subtitle ' + RightPad(IntToStr(SuggestedResynchSub),' ',3);

    SelectObject(DC, BoldFont);
    TextOutW(DC, NodeLeftMargin, NodeTopMargin - 8 , PWideChar(Msg), Length(Msg));
    SetTextColor(DC, ColorToRGB(clWhite));
    SetBkColor(DC, ColorToRGB(clBlack)); SetBkMode(DC, Opaque);
    Msg := RightPad(IntToStr(DimPreviousArea),' ',3);
    TextOutW(DC, vtvPoSList.Width-NodeRightMargin, 0 , PWideChar(Msg), Length(Msg));
   end;

  //Restore old things to prevent problems
  SelectObject(DC, OldFont);
  SetTextColor(DC, OldColor);
  DeleteObject(OldColor);
  vtvPoSList.Canvas.Unlock;
end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.FormDestroy(Sender: TObject);
begin
  if BoldFont <> 0 then
    DeleteObject(BoldFont);
  if NormalFont <> 0 then
    DeleteObject(NormalFont);
end;

//------------------------------------------------------------------------------

function CalculateShift(SubO, SubI, SubF, ShI, ShF : Integer) : Integer;
var shift : Integer;
begin
  if ShI = 0 then
  begin
    shift := Round(( SubO - SubI ) * (ShF / (SubF - SubI) ));
    if ShF < 0 then
      if shift < ShF
      then Result := ShF
      else Result := shift
    else
      if shift < ShF
      then Result := shift
      else Result := ShF;
  end
  else
  begin
    shift := Round(( SubF - SubO ) * (ShI / (SubF - SubI) ));
    if ShI < 0 then
      if shift < ShI
      then Result := ShI
      else Result := shift
    else
      if shift < ShI
      then Result := shift
      else Result := ShI;
  end;
end;

//------------------------------------------------------------------------------

//TODO: rewrite this procedure with undoable stuff
procedure TResynchToolForm.ColorSubtitles;
var i, iC : Integer;
    Node, NodeN : PVirtualNode;
    pPoS, pPoSN : PPoSTreeData;
begin
  MainForm.ResetSubColor();

  Node := vtvPoSList.GetFirst;
  iC := 0; i := 1;
  while i < vtvPoSList.TotalCount do
  begin
    pPos := vtvPoSList.GetNodeData(Node);
    NodeN := vtvPoSList.GetNext(Node);
    pPoSN := vtvPoSList.GetNodeData(NodeN);
    MainForm.SetSubColor( pPoS.Range.Node.Index+1, pPoSN.Range.Node.Index+1, Colors[iC] );

    if High(Colors) = iC then iC := 0
    else iC := iC + 1;

    Node := vtvPoSList.GetNext(Node);
    i := i + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.Delete;
var FirstSel, Cur : PVirtualNode;
    pPoS : PPoSTreeData;
begin
  FirstSel := vtvPoSList.GetFirstSelected;
  Cur := FirstSel;
  while Assigned(Cur) do
  begin
    if (not (vsSelected in Cur.States)) then
      Break;
    Cur := vtvPoSList.GetNext(Cur);
  end;

  if not Assigned(Cur) and Assigned(FirstSel) then
  begin
    Cur := vtvPoSList.GetPrevious(FirstSel);
  end;
  if MainForm.ConfigObject.DesynchUseColorsForSubtitles then
  begin
    pPoS := vtvPoSList.GetNodeData(FirstSel);
    MainForm.SetSubIcon(pPoS.Range.Node.Index+1, 0);
  end;
  vtvPoSList.DeleteSelectedNodes;

  if Assigned(Cur) then
  begin
    vtvPoSList.FocusedNode := Cur;
    vtvPoSList.Selected[Cur] := True;
  end;

end;

//------------------------------------------------------------------------------

function TResynchToolForm.DeletePoS(Range : TSubtitleRange) : TUndoableMultiPurposeTask;
var MultiUndoableTask : TUndoableMultiPurposeTask;
    UndoableDelete : TUndoablePoSListTask;
    UndoableIcon : TUndoableIconTask;
    UndoableColor : TUndoableColorSubTask;
    Node : PVirtualNode;
    pPoS : PPoSTreeData;
begin
  MultiUndoableTask := TUndoableMultiPurposeTask.Create;
  Node := vtvPoSList.GetFirst;
  while Assigned(Node) do
  begin
    pPoS := vtvPoSList.GetNodeData(Node);
    if pPoS.Range.Node.Index = Range.Node.Index then
    begin
      UndoableDelete := TUndoablePoSListTask.Create;
      UndoableDelete.DeletePoS(Node.Index);
      MultiUndoableTask.AddData(UndoableDelete);
      if MainForm.ConfigObject.DesynchUseIconsForSubtitles then
      begin
        UndoableIcon := TUndoableIconTask.Create;
        UndoableIcon.SetIcon(Range.Node.Index, 0);
        MultiUndoableTask.AddData(UndoableIcon);
      end;
      if MainForm.ConfigObject.DesynchUseColorsForSubtitles then
      begin
        UndoableColor := TUndoableColorSubTask.Create;
        UndoableColor.DoTask;
        MultiUndoableTask.AddData(UndoableColor);
      end;
      Break;
    end;
    Node := vtvPoSList.GetNext(Node);
  end;


  if MultiUndoableTask.GetCount > 0 then
  begin
    MultiUndoableTask.DoTask;
    Result := MultiUndoableTask;
  end
  else
  begin
    FreeAndNil(MultiUndoableTask);
    Result := nil;
  end;

end;

//------------------------------------------------------------------------------

function TResynchToolForm.AddPoSListNode(NewNodeData : PPosTreeData) : Integer;
var Node : PVirtualNode;
    pPoS, TpPoS : PPoSTreeData;
    NewSub : TSubtitleRange;
begin
  Node := vtvPoSList.AddChild(nil);
  pPos := vtvPoSList.GetNodeData(Node);
  NewSub := MainForm.GetAt(NewNodeData.Range.Node.Index);
  pPos.Range := NewSub;
  if (vtvPoSList.TotalCount = 1) And (MainForm.ConfigObject.DesynchAssumeFirstSubtitleSynched)
  then pPoS.Cursor := NewSub.StartTime
  else pPoS.Cursor := NewNodeData.Cursor;

  vtvPoSList.SortTree(0, sdAscending);

  Node := vtvPoSList.GetFirst;
  //while Node <> nil do
  while Assigned(Node) do
  begin
    TpPoS := vtvPoSList.GetNodeData(Node);
    if TpPoS.Range.Node.Index = pPos.Range.Node.Index then
    begin
      Result := Node.Index;
      Exit;
    end;
    Node := vtvPoSList.GetNext(Node);
  end;
  Result := -1;

end;

function TResynchToolForm.UpdatePoSListNode(NodeIndex, Cursor : Integer) : Integer;
var Node : PVirtualNode;
    pPoS : PPoSTreeData;
begin
  Node := vtvPoSList.GetFirst;
  //while Node <> nil do
  while Assigned(Node) do
  begin
    if Node.Index = NodeIndex then
    begin
      pPoS := vtvPoSList.GetNodeData(Node);
      pPoS.Cursor := Cursor;
      Result := pPoS.Range.StartTime;
      vtvPoSList.RepaintNode(Node);
      Exit;
    end;
    Node := vtvPoSList.GetNext(Node);
  end;
  Result := -1;
end;

function TResynchToolForm.DeletePoSListNode(Node : Integer) : PPosTreeData;
var NodeI : PVirtualNode;
    pPoS, TempPoS : PPoSTreeData;
begin
  NodeI := vtvPoSList.GetFirst;
  //while NodeI <> nil do
  while Assigned(NodeI) do
  begin
    if NodeI.Index = Node then
    begin
      vtvPoSList.FocusedNode := NodeI;
      vtvPoSList.Selected[NodeI] := True;
      pPoS := vtvPoSList.GetNodeData(NodeI);
      New(TempPoS);
      TempPoS.Range := MainForm.GetAt(pPoS.Range.Node.Index);
      TempPoS.Cursor := pPoS.Cursor;
      Delete;
      Result := TempPoS;
      Exit;
    end;
    NodeI := vtvPoSList.GetNext(NodeI);
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

function TResynchToolForm.SetPoS(Range : TSubtitleRange; Cursor : Integer) : TUndoableMultiPurposeTask;
var Node : PVirtualNode;
    pPoS : PPoSTreeData;
    Found : Boolean;
    MultiUndoableTask : TUndoableMultiPurposeTask;
    UndoableIcon : TUndoableIconTask;
    UndoableColor : TUndoableColorSubTask;
    UndoablePoS : TUndoablePoSListTask;
begin
  MultiUndoableTask := TUndoableMultiPurposeTask.Create;

  //1st task: check if there is a pos on the same subtitle
  Found := false;
  Node := vtvPoSList.GetFirst;
  while Assigned(Node) And (Not Found) do
  begin
    pPoS := vtvPoSList.GetNodeData(Node);
    //if exist update it with the new cursor
    if pPoS.Range.Node.Index = Range.Node.Index then
    begin
      UndoablePoS := TUndoablePoSListTask.Create;
      UndoablePoS.UpdatePoS(Node.Index, Cursor);
      UndoablePoS.DoTask;
      MultiUndoableTask.AddData(UndoablePoS);
      Found := True;
    end;
    Node := vtvPoSList.GetNext(Node);
  end;

  //if not found add a new one
  if not Found then
  begin
    pPoS := vtvPoSList.GetNodeData(vtvPoSList.GetLast);
    //but first check if there is the need of autoreset
    if (MainForm.ConfigObject.DesynchToolsAutoReset) And
       (vtvPoSList.TotalCount > 1) And
       (pPoS.Range.Node.Index < Range.Node.Index) then
    begin
      MainForm.ActionResynchToolsReset.Execute;
      MainForm.SelectNode(Range.Node);
      MainForm.ActionResynchToolsSetPos.Execute;
    end
    //else proceed adding the new pos
    else
    begin
      UndoablePoS := TUndoablePoSListTask.Create;
      UndoablePoS.AddPoS(Range, Cursor);
      UndoablePos.DoTask;
      MultiUndoableTask.AddData(UndoablePoS);
      //set the right icon for the newly added pos
      if MainForm.ConfigObject.DesynchUseIconsForSubtitles then
      begin
        //if it's the first pos
        if vtvPoSList.TotalCount = 1 then
        begin
          UndoableIcon := TUndoableIconTask.Create;
          UndoableIcon.SetIcon(Range.Node.Index, 4);
          UndoableIcon.DoTask;
          MultiUndoableTask.AddData(UndoableIcon);
        end
        else
        begin
          Node := vtvPoSList.GetLast;
          pPoS := vtvPoSList.GetNodeData(Node);
          if pPoS.Range.Node.Index = Range.Node.Index then
          begin
            UndoableIcon := TUndoableIconTask.Create;
            UndoableIcon.SetIcon(pPoS.Range.Node.Index, 5);
            UndoableIcon.DoTask;
            MultiUndoableTask.AddData(UndoableIcon);
            //change the provious icon too
            if vtvPoSList.TotalCount > 2 then
            begin
              UndoableIcon := TUndoableIconTask.Create;
              Node := vtvPoSList.GetPrevious(Node);
              pPoS := vtvPoSList.GetNodeData(Node);
              UndoableIcon.SetIcon(pPoS.Range.Node.Index, 1);
              UndoableIcon.DoTask;
              MultiUndoableTask.AddData(UndoableIcon);
            end;
          end
          else
          begin
            UndoableIcon := TUndoableIconTask.Create;
            UndoableIcon.SetIcon(Range.Node.Index, 1);
            UndoableIcon.DoTask;
            MultiUndoableTask.AddData(UndoableIcon);
          end;
        end;
      end;
      //color the subtitles
      if MainForm.ConfigObject.DesynchUseColorsForSubtitles then
      begin
        UndoableColor := TUndoableColorSubTask.Create;
        UndoableColor.DoTask;
        MultiUndoableTask.AddData(UndoableColor);
      end;
    end;
  end; //end IF not Found

  MultiUndoableTask.AddData(Resynch);

  if (MultiUndoableTask.GetCount > 0) then
  begin
    Result := MultiUndoableTask
  end
  else
  begin
    FreeAndNil(MultiUndoableTask);
    Result := nil;
  end;

end;

//------------------------------------------------------------------------------

function TResynchToolForm.Resynch : TUndoableMultiPurposeTask;
var ShI, ShF, TShI, TShF, NTShI: Integer;
    SubI, SubF : PPoSTreeData;
    NodeI, NodeF : PVirtualNode;
    sub, subN : TSubtitleRange;
    LoopExit : Boolean;
    MultiUndoableTask : TUndoableMultiPurposeTask;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
begin
  MinimumBlank := MainForm.ConfigObject.SpaceKeyBlankBetweenSubtitles;
  TntStatusBar1.Panels[0].Text := 'Resyncing...';
  if MainForm.ConfigObject.DesynchLog then LogForm.Clear;
  if MainForm.ConfigObject.DesynchLog then LogForm.LogMsg('Starting Resynch...');
  NodeI := vtvPoSList.GetFirst;
  MultiUndoableTask := TUndoableMultiPurposeTask.Create;
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Repeat
    SubI := vtvPoSList.GetNodeData(NodeI);
    ShI := SubI.Cursor - SubI.Range.StartTime;
    if MainForm.ConfigObject.DesynchLog then
      LogForm.LogMsg( Format('ShI:%3d,%d',[SubI.Range.Node.Index, ShI]) );
    NodeF := vtvPoSList.GetNext(NodeI);
    SubF := nil; ShF := 0;
    if Assigned(NodeF) then
    begin
      SubF := vtvPoSList.GetNodeData(NodeF);
      ShF := SubF.Cursor - SubF.Range.StartTime;
      if MainForm.ConfigObject.DesynchLog then
        LogForm.LogMsg( Format('ShF:%3d,%d',[SubF.Range.Node.Index, ShF]) );
    end;
    sub := MainForm.GetAt(SubI.Range.Node.Index);
    if MainForm.ConfigObject.DesynchLog then
      LogForm.LogMsg( Format('Sub:%3d',[sub.Node.Index]) );
    LoopExit := False;

    if (ShI <> 0) Or (ShF <> 0) then
    begin

      if NodeI.Index = 0 then
      begin
        subN := MainForm.GetPrevious(sub);
        if (subN <> nil) and (subN.StopTime > (sub.StartTime - MinimumBlank + ShI)) then
        begin
          ChangeSubData := TChangeSubData.Create( subN.Node.Index );
          ChangeSubData.OldStart := subN.StartTime;
          ChangeSubData.OldStop := subN.StopTime;
          ChangeSubData.NewStart := subN.StartTime;
          ChangeSubData.NewStop := sub.StartTime - MinimumBlank + ShI;
          MultiChangeTask.AddData( ChangeSubData );
        end;
      end;

      while (sub <> nil) And (not LoopExit) do
      begin
        if SubF = nil then
        begin
          if ShI <> 0 then
          begin
            if MainForm.ConfigObject.DesynchLog then
              LogForm.LogMsg( Format('Sub: %d to end; Shift: %d',[sub.Node.Index, ShI]) );
            Repeat
              ChangeSubData := TChangeSubData.Create( sub.Node.Index );
              ChangeSubData.OldStart := sub.StartTime;
              ChangeSubData.OldStop := sub.StopTime;
              ChangeSubData.NewStart := sub.StartTime + ShI;
              ChangeSubData.NewStop := sub.StopTime + ShI;
              MultiChangeTask.AddData( ChangeSubData );
              sub := MainForm.GetNext( sub );
            Until ( sub = nil );
            Break;
          end;
        end
        else
        begin
          TShI := CalculateShift( sub.StartTime, SubI.Range.StartTime,
                                      SubF.Range.StartTime, ShI, ShF ) + sub.StartTime;
          TShF := CalculateShift( sub.StopTime, SubI.Range.StartTime,
                                      SubF.Range.StartTime, ShI, ShF ) + sub.StopTime;
          //Minimum Blank check
          subN := MainForm.GetNext(sub);

          try
            if (sub.Text[1] <> '{') AND (subN.Text[1] <> '{') AND (subN <> nil) then
            begin
              NTShI := CalculateShift( subN.StartTime, SubI.Range.StartTime,
                                        SubF.Range.StartTime, ShI, ShF ) + subN.StartTime;
              if NTShI < TShF + MinimumBlank
              then TShF := NTShI - MinimumBlank;
            end;
          except
            //empty subtitle
            NTShI := CalculateShift( subN.StartTime, SubI.Range.StartTime,
                                     SubF.Range.StartTime, ShI, ShF ) + subN.StartTime;
            if NTShI < TShF + MinimumBlank
            then TShF := NTShI - MinimumBlank;
          end;

          if MainForm.ConfigObject.DesynchLog then
            LogForm.LogMsg( Format( 'Sub:%3d %5d:%5d %5d:%5d - %4d',[ sub.Node.Index, TShI, TShF,
                            sub.StartTime, sub.StopTime, TShI - sub.StartTime] ) );

          ChangeSubData := TChangeSubData.Create( sub.Node.Index );
          ChangeSubData.OldStart := sub.StartTime;
          ChangeSubData.OldStop := sub.StopTime;
          ChangeSubData.NewStart := TShI;
          ChangeSubData.NewStop := TShF;
          MultiChangeTask.AddData( ChangeSubData );

          if sub.Node.Index = SubF.Range.Node.Index - 1
          then LoopExit := True;
        end;
        sub := MainForm.GetNext( sub );
      end;
      
    end;
    NodeI := vtvPoSList.GetNext( NodeI );
  Until ( sub = nil ) Or ( NodeI = nil );
  
  MultiUndoableTask.AddData( MultiChangeTask );

  if ( MultiUndoableTask.GetCount > 0 ) then
  begin
    MultiUndoableTask.DoTask;
    Result := MultiUndoableTask
  end
  else
  begin
    FreeAndNil( MultiUndoableTask );
    Result := nil;
  end;
  
  TntStatusBar1.Panels[0].Text := 'Resynch.';
  if MainForm.ConfigObject.DesynchLog then LogForm.LogMsg( 'Resynch Done.' );
end;

//------------------------------------------------------------------------------

function TResynchToolForm.Reset : TUndoableMultiPurposeTask;
var Node : PVirtualNode;
    pPos : PPosTreeData;
    i : Integer;
    UndoableReset : TUndoableMultiPurposeTask;
    UndoableColor : TUndoableColorSubTask;
    UndoableClear : TUndoablePoSListClearTask;
    UndoableIcon : TUndoableIconTask;
begin
  UndoableReset := TUndoableMultiPurposeTask.Create;
  if vtvPoSList.TotalCount > 0 then
  begin
    if MainForm.ConfigObject.DesynchUseIconsForSubtitles then
    begin
      Node := vtvPoSList.GetFirst;
      for i:=1 to vtvPoSList.TotalCount - 1 do
      begin
        pPoS := vtvPoSList.GetNodeData(Node);
        UndoableIcon := TUndoableIconTask.Create;
        UndoableIcon.SetIcon(pPoS.Range.Node.Index, 0);
        UndoableIcon.DoTask;
        UndoableReset.AddData(UndoableIcon);
        Node := vtvPoSList.GetNext(Node);
      end;
      Node := vtvPoSList.GetLast;
      pPoS := vtvPoSList.GetNodeData(Node);
      UndoableIcon := TUndoableIconTask.Create;
      UndoableIcon.SetIcon(pPoS.Range.Node.Index, 3);
      UndoableClear := TUndoablePoSListClearTask.Create;
      UndoableClear.DoTask;
      UndoableReset.AddData(UndoableClear);
      UndoableIcon.DoTask;
      UndoableReset.AddData(UndoableIcon);
    end
    else
    begin
      UndoableClear := TUndoablePoSListClearTask.Create;
      UndoableClear.DoTask;
      UndoableReset.AddData(UndoableClear);
    end;

    if MainForm.ConfigObject.DesynchUseColorsForSubtitles then
    begin
      UndoableColor := TUndoableColorSubTask.Create;
      UndoableColor.DoTask;
      UndoableReset.AddData(UndoableColor);
    end;
  end;

  if UndoableReset.GetCount > 0 then
  begin
    Result := UndoableReset;
  end
  else
  begin
    FreeAndNil(UndoableReset);
    Result := nil;
  end;
  TntStatusBar1.Panels[0].Text := 'Reset.';
end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.vtvPoSListFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var Cur,PrevCur : PVirtualNode;
    pPoSData : PPoSTreeData;
    pPoSDataPrev : PPoSTreeData;
    SuggestedResynchSub : word;
begin
  if Assigned(vtvPoSList.FocusedNode) then
   begin
    pPoSData := vtvPoSList.GetNodeData(vtvPoSList.FocusedNode);

    Cur := vtvPoSList.FocusedNode;
    PrevCur := vtvPoSList.GetPrevious(Cur);

    SuggestedResynchSub := 0;
    if PrevCur <> nil then
     begin
      pPoSDataPrev := vtvPoSList.GetNodeData(PrevCur);
      SuggestedResynchSub := (pPoSDataPrev.Range.Node.Index + 1) + ( (pPoSData.Range.Node.Index + 1) - (pPoSDataPrev.Range.Node.Index + 1) ) div 2;
     end;

    if SuggestedResynchSub = 0 then GotoSuggestedNodeButton.Enabled := false
     else GotoSuggestedNodeButton.Enabled := true;

   end;

end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.vtvPoSListCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pPoS1, pPoS2 : PPoSTreeData;
begin
  pPoS1 := Sender.GetNodeData(Node1);
  pPoS2 := Sender.GetNodeData(Node2);
  if (pPoS1.Range <> nil) And (pPos2.Range <> nil) then
  begin
    if pPos1.Range.Node.Index > pPos2.Range.Node.Index
    then Result := 1
    else Result := -1;
  end;
end;

//------------------------------------------------------------------------------

procedure TResynchToolForm.FormDeactivate(Sender: TObject);
begin
  vtvPoSList.FocusedNode := nil;
  vtvPoSList.Selected[vtvPoSList.GetFirstSelected] := False;
end;

procedure TResynchToolForm.DeleteselectedPoS1Click(Sender: TObject);
var Node : PVirtualNode;
begin
  Node := vtvPoSList.GetFirstSelected;
  if Assigned(Node) then MainForm.ActionResynchToolsDelPoS.Execute;
end;

procedure TResynchToolForm.GotoSelectedNodeButtonClick(Sender: TObject);
var pPoS : PPoSTreeData;
begin
  if Assigned(vtvPoSList.FocusedNode) then
  begin
    pPoS := vtvPoSList.GetNodeData(vtvPoSList.FocusedNode);
    MainForm.SelectNode(pPoS.Range.Node);
  end
end;

procedure TResynchToolForm.vtvPoSListDblClick(Sender: TObject);
begin
 GotoSelectedNodeButtonClick(self);
end;

procedure TResynchToolForm.GotoSuggestedNodeButtonClick(Sender: TObject);
var Cur,PrevCur : PVirtualNode;
    pPoSData : PPoSTreeData;
    pPoSDataPrev : PPoSTreeData;
    SuggestedResynchSub : word;
begin

  if Assigned(vtvPoSList.FocusedNode) then
   begin
    pPoSData := vtvPoSList.GetNodeData(vtvPoSList.FocusedNode);

    Cur := vtvPoSList.FocusedNode;
    PrevCur := vtvPoSList.GetPrevious(Cur);

    SuggestedResynchSub := 0;
    if PrevCur <> nil then
     begin
      pPoSDataPrev := vtvPoSList.GetNodeData(PrevCur);
      SuggestedResynchSub := (pPoSDataPrev.Range.Node.Index + 1) + ( (pPoSData.Range.Node.Index + 1) - (pPoSDataPrev.Range.Node.Index + 1) ) div 2;
     end;

    MainForm.SelectNodeAtIndex(SuggestedResynchSub - 1);

   end;

end;

end.
//------------------------------------------------------------------------------

