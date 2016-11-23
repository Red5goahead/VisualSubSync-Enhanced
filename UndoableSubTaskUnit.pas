// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2007 Christophe Paris
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
unit UndoableSubTaskUnit;

interface

uses UndoableTaskUnit, SubStructUnit, Classes, Contnrs, Graphics, TntComCtrls,
  Types;

type

  TUndoableTaskIndexed = class(TUndoableTask)
  private
    FIndexes : TIntegerDynArray; // Indexes of subtitles in FWavDisplayer
    FCount : Integer; // Number of indexes
  public
    destructor Destroy; override;
    procedure AddSubtitleIndex(Index : Integer);
    procedure SetCapacity(Capacity : Integer);
  end;

  TUndoableDelayTask = class(TUndoableTaskIndexed)
  private
    FDelayInMs : Integer;
    FDelayShiftType : TDelayShiftType;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetDelayInMs(DelayInMs : Integer);
    procedure SetDelayShiftType(DelayShiftType : TDelayShiftType);
  end;

  TUndoableAddTask = class(TUndoableTask)
  private
    FStartTime, FStopTime : Integer;
    FText : WideString;
    FAutoSelect : Boolean;
    FIndex : Integer;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetTime(StartTime, StopTime : Integer);
    procedure SetText(Text : WideString);
    procedure SetAutoSelect(AutoSelect : Boolean);
  end;

  TUndoableDeleteTask = class(TUndoableTaskIndexed)
  private
    FDeletedSubs : TObjectList;
    FSearchIdx : Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(SearchIdx : Integer);
  end;

  TUndoableMultiAddTask = class(TUndoableTaskIndexed)
  private
    FDeletedSubs : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(SubList : TList);
  end;

  TUndoableSetTimeTask = class(TUndoableTask)
  private
    FNewStartTime, FNewStopTime : Integer;
    FOldStartTime, FOldStopTime : Integer;
    FIndex : Integer;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(Index, OldStartTime, OldStopTime,
      NewStartTime, NewStopTime : Integer);
  end;

  TUndoableSplitTask = class(TUndoableTask)
  private
    FIndex : Integer;
    FStartTime, FStopTime, FSplitTime, FBlankTime : Integer;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(Index, StartTime, StopTime, SplitTime, BlankTime : Integer);
  end;

  TMergeType = (mtNormal, mtDialog, mtOneLine);
  TMergeRange = (mrWithPrevious, mrWithNext, mrSelected);
  TUndoableMergeTask = class(TUndoableTaskIndexed)
  private
    FDeletedSubs : TObjectList;
    FSelectedSubIndex : Integer;
    FMergeType : TMergeType;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(SelectedSubIndex : Integer; MergeType : TMergeType);
  end;

  TUndoableSubTextTask = class(TUndoableTask)
  private
    FUndoableTextTask : TUndoableTask;
    FIndex : Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    function Merge(UndoableTask : TUndoableTask) : Boolean; override;
    procedure UndoTask; override;

    procedure SetData(Index : Integer; UndoableTextTask : TUndoableTask);
    function GetSubTask : TUndoableTask;    
  end;

  TUndoableCompositeTask = class(TUndoableTask)
  private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure AddTask(UndoableTask : TUndoableTask);
  end;

  TChangeSubData = class
  private
    FIndex : Integer;
    FNewStart, FOldStart : Integer;
    FNewStop, FOldStop : Integer;
    FNewText, FOldText : WideString;
    FOldSubTime, FNewSubTime : TIntegerDynArray;
    FOldStartChanged, FOldStopChanged, FOldTextChanged, FOldSubTimeChanged : Boolean;

    procedure SetOldStart(Value: Integer);
    procedure SetOldStop(Value: Integer);
    procedure SetOldText(Value: WideString);
    procedure SetOldSubTime(Value: TIntegerDynArray);
    procedure SetNewSubTime(Value: TIntegerDynArray);
  public
    constructor Create(Index : Integer);
    function GetStart(ForUndo : Boolean) : Integer;
    function GetStop(ForUndo : Boolean) : Integer;
    function GetText(ForUndo : Boolean) : WideString;
    function GetSubTime(ForUndo : Boolean) : TIntegerDynArray;
  published
    property Index : Integer read FIndex write FIndex;
    property OldStart : Integer read FOldStart write SetOldStart;
    property OldStop : Integer read FOldStop write SetOldStop;
    property OldText : WideString read FOldText write SetOldText;
    property OldSubTime : TIntegerDynArray read FOldSubTime write SetOldSubTime;

    property NewStart : Integer read FNewStart write FNewStart;
    property NewStop : Integer read FNewStop write FNewStop;
    property NewText : WideString read FNewText write FNewText;
    property NewSubTime : TIntegerDynArray read FNewSubTime write SetNewSubTime;

    property StartChanged : Boolean read FOldStartChanged;
    property StopChanged : Boolean read FOldStopChanged;
    property TextChanged : Boolean read FOldTextChanged;
    property SubTimeChanged : Boolean read FOldSubTimeChanged;
  end;

  // Used for error correction
  TUndoableMultiChangeTask = class(TUndoableTask)
  private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure AddData(ChangeSubData : TChangeSubData);
    function GetData(Index : Integer) : TChangeSubData;
    function GetCount : Integer;
  end;

  TUndoablePipeTask = class(TUndoableTask)
  private
    FActionType, FOldSelStart, FOldSelLength : Integer;
    FOldSelText : WideString;
    FNewSelColor, FOldSelColor : TColor;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(ActionType : Integer; RichEdit : TTntRichEdit; NewColor : TColor);
  end;

  TUndoableDeleteSceneChange = class(TUndoableTask)
  private
    FStartTimeMs, FStopTimeMs : Integer;
    FDeletedSC : TIntegerDynArray;

  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(StartTimeMs, StopTimeMs : Integer);
  end;

  TUndoableInsertSceneChange = class(TUndoableTask)
  private
    FTimeMs : Integer;

  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(TimeMs : Integer);
  end;

  TPoSTreeData = record
    Range : TSubtitleRange;
    Cursor : Integer;
  end;
  PPoSTreeData = ^TPoSTreeData;

  TUndoableIconTask = class(TUndoableTask)
  private
    FNewIcon, FOldIcon : Integer;
    FIndex : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
    function GetName : WideString; override;

    procedure SetIcon(Index, NewIcon : Integer);
  end;

  TUndoablePoSListTask = class(TUndoableTask)
  private
    FNodeData : PPoSTreeData;
    FNodeIndex, FOldCursor : Integer;
    FAdd, FUpdate, FDelete, Done : Boolean;
  public
    constructor Create;
    procedure DoTask; override;
    procedure UndoTask; override;
    function GetName : WideString; override;

    function AddPoS(Range : TSubtitleRange; Cursor : Integer) : Boolean;
    function UpdatePoS(Node, NewCursor : Integer) : Boolean;
    function DeletePoS(Node : Integer) : Boolean;
  end;

  TUndoablePoSListClearTask = class(TUndoableTask)
  private
    FPPoSList : array of PPoSTreeData;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
    function GetName : WideString; override;
  end;

  TUndoableColorSubTask = class(TUndoableTask)
  public
    procedure DoTask; override;
    procedure UndoTask; override;
    function GetName : WideString; override;
  end;

  TUndoableMultiPurposeTask = class(TUndoableTask)
  private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    procedure UndoTask; override;
    function GetName : WideString; override;

    procedure AddData(UndoTask : TUndoableTask);
    function GetCount : Integer;
  end;

implementation

uses Main, MiscToolsUnit, VirtualTrees, Windows, JavascriptPluginUnit, ResynchToolFormUnit, SysUtils;

//==============================================================================

destructor TUndoableTaskIndexed.Destroy;
begin
  SetLength(FIndexes, 0);
  FIndexes := nil;
  inherited;
end;

procedure TUndoableTaskIndexed.AddSubtitleIndex(Index : Integer);
begin
  FIndexes[FCount] := Index;
  Inc(FCount);
end;

procedure TUndoableTaskIndexed.SetCapacity(Capacity : Integer);
begin
  SetLength(FIndexes, Capacity);
end;

//-------------------------------------------------------------------------------

procedure TUndoableDelayTask.UndoTask;
begin
  MainForm.ApplyDelay(FIndexes, -FDelayInMs, FDelayShiftType);
end;

procedure TUndoableDelayTask.DoTask;
begin
  MainForm.ApplyDelay(FIndexes, FDelayInMs, FDelayShiftType);
end;

function TUndoableDelayTask.GetName : WideString;
begin
  Result := 'Delay ' + TimeMsToString(FDelayInMs);
end;

procedure TUndoableDelayTask.SetDelayInMs(DelayInMs : Integer);
begin
  FDelayInMs := DelayInMs;
end;

procedure TUndoableDelayTask.SetDelayShiftType(DelayShiftType : TDelayShiftType);
begin
  FDelayShiftType := DelayShiftType;
end;

//------------------------------------------------------------------------------

procedure TUndoableAddTask.DoTask;
var NewNode : PVirtualNode;
begin
  NewNode := MainForm.AddSubtitle(FStartTime, FStopTime, FText, True, FAutoSelect);
  FIndex := NewNode.Index;
end;

function TUndoableAddTask.GetName : WideString;
begin
  Result := 'Add subtitle';
end;

procedure TUndoableAddTask.UndoTask;
begin
  MainForm.DeleteSubtitle(FIndex);
end;

procedure TUndoableAddTask.SetTime(StartTime, StopTime : Integer);
begin
  FStartTime := StartTime;
  FStopTime := StopTime;
end;

procedure TUndoableAddTask.SetText(Text : WideString);
begin
  FText := Text;
end;

procedure TUndoableAddTask.SetAutoSelect(AutoSelect : Boolean);
begin
  FAutoSelect := AutoSelect;
end;

//------------------------------------------------------------------------------

constructor TUndoableMultiAddTask.Create;
begin
  inherited;
  FDeletedSubs := TObjectList.Create;
  FDeletedSubs.OwnsObjects := True;
end;

destructor TUndoableMultiAddTask.Destroy;
begin
  FDeletedSubs.Free;
  inherited;
end;

procedure TUndoableMultiAddTask.DoTask;
begin
  // Lazy task
  MainForm.RestoreSubtitles(FDeletedSubs, FIndexes);
  FDeletedSubs.Clear;
end;

function TUndoableMultiAddTask.GetName : WideString;
begin
  Result := 'MultiAdd';
end;

procedure TUndoableMultiAddTask.UndoTask;
begin
  // First clone subtitles
  MainForm.CloneSubtitles(FIndexes, FDeletedSubs);
  // Now delete them
  MainForm.DeleteSubtitles(FIndexes);
end;

procedure TUndoableMultiAddTask.SetData(SubList : TList);
begin
  SetCapacity(SubList.Count);
  FCount := SubList.Count;
  FDeletedSubs.Assign(SubList);
end;

//------------------------------------------------------------------------------

constructor TUndoableDeleteTask.Create;
begin
  inherited;
  FDeletedSubs := TObjectList.Create;
  FDeletedSubs.OwnsObjects := True;
end;

destructor TUndoableDeleteTask.Destroy;
begin
  FDeletedSubs.Free;
  inherited;
end;

procedure TUndoableDeleteTask.DoTask;
begin
  // First clone subtitles
  MainForm.CloneSubtitles(FIndexes, FDeletedSubs);
  // Now delete them
  MainForm.DeleteSubtitles(FIndexes);
end;

function TUndoableDeleteTask.GetName : WideString;
begin
  Result := 'Delete';
end;

procedure TUndoableDeleteTask.UndoTask;
begin
  MainForm.RestoreSubtitles(FDeletedSubs);
  FDeletedSubs.Clear;
end;

procedure TUndoableDeleteTask.SetData(SearchIdx : Integer);
begin
  FSearchIdx := SearchIdx;
end;

//------------------------------------------------------------------------------

procedure TUndoableSetTimeTask.DoTask;
begin
  FIndex := MainForm.SetSubtitleTime(FIndex, FNewStartTime, FNewStopTime);
end;

function TUndoableSetTimeTask.GetName : WideString;
begin
  Result := 'Set time';
end;

procedure TUndoableSetTimeTask.UndoTask;
begin
  FIndex := MainForm.SetSubtitleTime(FIndex, FOldStartTime, FOldStopTime);
end;

procedure TUndoableSetTimeTask.SetData(Index, OldStartTime, OldStopTime,
  NewStartTime, NewStopTime : Integer);
begin
  FIndex := Index;
  FOldStartTime := OldStartTime;
  FOldStopTime := OldStopTime;
  FNewStartTime := NewStartTime;
  FNewStopTime := NewStopTime;
end;

//------------------------------------------------------------------------------

procedure TUndoableSplitTask.DoTask;
begin
  MainForm.SplitSubtitle(FIndex, FSplitTime, FBlankTime);
end;

function TUndoableSplitTask.GetName : WideString;
begin
  Result := 'Split';
end;

procedure TUndoableSplitTask.UndoTask;
begin
  MainForm.DeleteSubtitle(FIndex + 1);
  MainForm.SetSubtitleTime(FIndex, FStartTime, FStopTime);
end;

procedure TUndoableSplitTask.SetData(Index, StartTime, StopTime, SplitTime, BlankTime : Integer);
begin
  FIndex := Index;
  FStartTime := StartTime;
  FStopTime := StopTime;
  FSplitTime := SplitTime;
  FBlankTime := BlankTime;
end;

//------------------------------------------------------------------------------

constructor TUndoableMergeTask.Create;
begin
  inherited;
  FDeletedSubs := TObjectList.Create;
  FDeletedSubs.OwnsObjects := True;
  FSelectedSubIndex := -1;
  FMergeType := mtNormal;
end;

destructor TUndoableMergeTask.Destroy;
begin
  FDeletedSubs.Free;
  inherited;
end;

procedure TUndoableMergeTask.DoTask;
var SubRange : TSubtitleRange;
    Node : PVirtualNode;
begin
  // Calculate the merged subtitle
  SubRange := MainForm.MergeSubtitles(FIndexes, FMergeType);
  // Clone subtitles
  MainForm.CloneSubtitles(FIndexes, FDeletedSubs);
  // Now delete them
  MainForm.DeleteSubtitles(FIndexes);
  // Add back the merged subtitle
  Node := MainForm.AddSubtitle(SubRange, False);
  // Focus the merged node
  MainForm.FocusNode(Node, False);
  // Free temporarily created merged sub
  SubRange.Free;
  // If Select the new merged subtitle
  if (FSelectedSubIndex <> -1) then
    MainForm.SelectNode(Node);
end;

function TUndoableMergeTask.GetName : WideString;
begin
  Result := 'Merge';
end;

procedure TUndoableMergeTask.UndoTask;
begin
  // Delete the merged subtitle
  MainForm.DeleteSubtitle(FIndexes[0]);
  // Restaure deleted subtitles
  MainForm.RestoreSubtitles(FDeletedSubs);
  FDeletedSubs.Clear;
  // Restaure selection
  if (FSelectedSubIndex <> -1) then
    MainForm.SelectNodeAtIndex(FSelectedSubIndex);
end;

procedure TUndoableMergeTask.SetData(SelectedSubIndex : Integer; MergeType : TMergeType);
begin
  FSelectedSubIndex := SelectedSubIndex;
  FMergeType := MergeType;
end;

// -----------------------------------------------------------------------------

constructor TUndoableSubTextTask.Create;
begin
  FUndoableTextTask := nil;
end;

destructor TUndoableSubTextTask.Destroy;
begin
  if Assigned(FUndoableTextTask) then
  begin
    FUndoableTextTask.Free;
  end
end;

function TUndoableSubTextTask.GetSubTask : TUndoableTask;
begin
  Result := FUndoableTextTask;
end;

procedure TUndoableSubTextTask.DoTask;
begin
  MainForm.FocusNodeAt(FIndex);
  FUndoableTextTask.DoTask;
end;

function TUndoableSubTextTask.GetName : WideString;
begin
  FUndoableTextTask.GetName;
end;

procedure TUndoableSubTextTask.UndoTask;
begin
  MainForm.FocusNodeAt(FIndex);
  FUndoableTextTask.UndoTask;
end;

procedure TUndoableSubTextTask.SetData(Index : Integer; UndoableTextTask : TUndoableTask);
begin
  FIndex := Index;
  FUndoableTextTask := UndoableTextTask;
end;

function TUndoableSubTextTask.Merge(UndoableTask : TUndoableTask) : Boolean;
begin
  if (UndoableTask is TUndoableSubTextTask) then
  begin
    Result := FUndoableTextTask.Merge(TUndoableSubTextTask(UndoableTask).FUndoableTextTask);
  end else begin
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

constructor TUndoableCompositeTask.Create;
begin
  FList := TObjectList.Create;
  FList.OwnsObjects := True;
end;

destructor TUndoableCompositeTask.Destroy;
begin
  FList.Free;
end;

procedure TUndoableCompositeTask.DoTask;
var i : Integer;
begin
  for i := 0 to FList.Count-1 do
  begin
    TUndoableTask(FList[i]).DoTask;
  end;
end;

function TUndoableCompositeTask.GetName : WideString;
begin
  Result := 'TUndoableCompositeTask';
end;

procedure TUndoableCompositeTask.UndoTask;
var i : Integer;
begin
  for i := FList.Count-1 downto 0 do
  begin
    TUndoableTask(FList[i]).UndoTask;
  end;
end;

procedure TUndoableCompositeTask.AddTask(UndoableTask : TUndoableTask);
begin
  FList.Add(UndoableTask);
end;

// -----------------------------------------------------------------------------

constructor TChangeSubData.Create(Index : Integer);
begin
  FIndex := Index;
  FNewStart := -1; FOldStart := -1;
  FNewStop := -1; FOldStop := -1;
  FNewText := ''; FOldText := '';
  SetLength(FNewSubTime, 0);
  SetLength(FOldSubTime, 0);
  FOldStartChanged := False;
  FOldStopChanged := False;
  FOldTextChanged := False;
  FOldSubTimeChanged := False;
end;

function TChangeSubData.GetStart(ForUndo : Boolean) : Integer;
begin
  if ForUndo then Result := FOldStart else Result := FNewStart;
end;

function TChangeSubData.GetStop(ForUndo : Boolean) : Integer;
begin
  if ForUndo then Result := FOldStop else Result := FNewStop;
end;

function TChangeSubData.GetText(ForUndo : Boolean) : WideString;
begin
  if ForUndo then Result := FOldText else Result := FNewText;
end;

function TChangeSubData.GetSubTime(ForUndo : Boolean) : TIntegerDynArray;
begin
  if ForUndo then Result := FOldSubTime else Result := FNewSubTime;
end;

procedure TChangeSubData.SetOldStart(Value: Integer);
begin
  if not FOldStartChanged then
  begin
    FOldStart := Value;
    FOldStartChanged := True;
  end;
end;

procedure TChangeSubData.SetOldStop(Value: Integer);
begin
  if not FOldStopChanged then
  begin
    FOldStop := Value;
    FOldStopChanged := True;
  end;
end;

procedure TChangeSubData.SetOldText(Value: WideString);
begin
  if not FOldTextChanged then
  begin
    FOldText := Value;
    FOldTextChanged := True;
  end;
end;

procedure TChangeSubData.SetOldSubTime(Value: TIntegerDynArray);
begin
  if not FOldSubTimeChanged then
  begin
    SetLength(FOldSubTime, Length(Value));
    CopyMemory(@FOldSubTime[0], @Value[0], Length(Value) * SizeOf(Integer));
    FOldSubTimeChanged := True;
  end;
end;

procedure TChangeSubData.SetNewSubTime(Value: TIntegerDynArray);
begin
  SetLength(FNewSubTime, Length(Value));
  CopyMemory(@FNewSubTime[0], @Value[0], Length(Value) * SizeOf(Integer));
end;


constructor TUndoableMultiChangeTask.Create;
begin
  FList := TObjectList.Create;
  FList.OwnsObjects := True;
end;

destructor TUndoableMultiChangeTask.Destroy;
begin
  FList.Free;
end;

procedure TUndoableMultiChangeTask.DoTask;
begin
  MainForm.ProcessMultiChangeSub(FList, False);
end;

function TUndoableMultiChangeTask.GetName : WideString;
begin
  Result := 'TUndoableMultiChangeTask';
end;

procedure TUndoableMultiChangeTask.UndoTask;
begin
  MainForm.ProcessMultiChangeSub(FList, True);
end;

procedure TUndoableMultiChangeTask.AddData(ChangeSubData : TChangeSubData);
begin
  FList.Add(ChangeSubData);
end;

function TUndoableMultiChangeTask.GetData(Index : Integer) : TChangeSubData;
var i : Integer;
    ChangeSubData : TChangeSubData;
begin
  for i:=0 to FList.Count-1 do
  begin
    ChangeSubData := TChangeSubData(FList[i]);
    if (ChangeSubData.FIndex = Index) then
    begin
      Result := ChangeSubData;
      Exit;
    end;
  end;
  Result := nil;
end;

function TUndoableMultiChangeTask.GetCount : Integer;
begin
  Result := FList.Count;
end;

// -----------------------------------------------------------------------------

procedure TUndoablePipeTask.DoTask;
begin
  if (FActionType = 0) then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
    MainForm.MemoTextPipe.SelAttributes.Color := FNewSelColor;
  end
  else if (FActionType = 1) then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
    MainForm.MemoTextPipe.ClearSelection;
  end
  else if (FActionType = 2) then
  begin
    MainForm.MemoTextPipe.SelStart := 0;
    MainForm.MemoTextPipe.SelLength := FOldSelStart + FOldSelLength;
    FOldSelText := MainForm.MemoTextPipe.SelText;
    MainForm.MemoTextPipe.ClearSelection;
  end
end;

function TUndoablePipeTask.GetName : WideString;
begin
  Result := 'TUndoablePipeTask';
end;

procedure TUndoablePipeTask.UndoTask;
begin
  if FActionType = 0 then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
    MainForm.MemoTextPipe.SelAttributes.Color := FOldSelColor;
  end
  else if FActionType = 1 then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := 0;
    MainForm.MemoTextPipe.SelText := FOldSelText;
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
  end
  else if FActionType = 2 then
  begin
    MainForm.MemoTextPipe.SelStart := 0;
    MainForm.MemoTextPipe.SelLength := 0;
    MainForm.MemoTextPipe.SelText := FOldSelText;
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
  end;
end;

procedure TUndoablePipeTask.SetData(ActionType : Integer;
  RichEdit : TTntRichEdit; NewColor : TColor);
begin
  FActionType := ActionType;
  FOldSelStart := RichEdit.SelStart;
  FOldSelLength := RichEdit.SelLength;
  FOldSelText := RichEdit.SelText;
  FOldSelColor := RichEdit.SelAttributes.Color;
  FNewSelColor := NewColor;
end;

// -----------------------------------------------------------------------------

procedure TUndoableDeleteSceneChange.DoTask;
begin
  FDeletedSC := MainForm.DeleteSceneChange(FStartTimeMs, FStopTimeMs);
end;

function TUndoableDeleteSceneChange.GetName : WideString;
begin
  Result := 'TUndoableDeleteSceneChange';
end;

procedure TUndoableDeleteSceneChange.UndoTask;
begin
  MainForm.InsertSceneChange(FDeletedSC);
  SetLength(FDeletedSC, 0);
end;

procedure TUndoableDeleteSceneChange.SetData(StartTimeMs, StopTimeMs : Integer);
begin
  FStartTimeMs := StartTimeMs;
  FStopTimeMs := StopTimeMs;
end;


// -----------------------------------------------------------------------------

procedure TUndoableInsertSceneChange.DoTask;
var InsertSC : TIntegerDynArray;
begin
  SetLength(InsertSC, 1);
  InsertSC[0] := FTimeMs;
  MainForm.InsertSceneChange(InsertSC);
end;

function TUndoableInsertSceneChange.GetName : WideString;
begin
  Result := 'TUndoableInsertSceneChange';
end;

procedure TUndoableInsertSceneChange.UndoTask;
begin
  MainForm.DeleteSceneChange(FTimeMs, FTimeMs);
end;

procedure TUndoableInsertSceneChange.SetData(TimeMs : Integer);
begin
  FTimeMs := TimeMs;
end;

procedure TUndoableIconTask.DoTask;
begin
  MainForm.SetSubIcon(FIndex, FNewIcon);
end;

procedure TUndoableIconTask.UndoTask;
begin
  MainForm.SetSubIcon(FIndex, FOldIcon);
end;

procedure TUndoableIconTask.SetIcon(Index, NewIcon : Integer);
begin
  FIndex := Index+1;
  FNewIcon := NewIcon;
  FOldIcon := MainForm.GetSubIcon(Index);
end;

function TUndoableIconTask.GetName : WideString;
begin
  Result := 'UndoableIcon';
end;

// -----------------------------------------------------------------------------

constructor TUndoableMultiPurposeTask.Create;
begin
  FList := TObjectList.Create;
  FList.OwnsObjects := True;
end;

destructor TUndoableMultiPurposeTask.Destroy;
begin
  FList.Free;
end;

procedure TUndoableMultiPurposeTask.DoTask;
var i : Integer;
begin
  for i:=0 to FList.Count-1 do
  begin
    TUndoableTask(FList[i]).DoTask;
  end;
end;

procedure TUndoableMultiPurposeTask.UndoTask;
var i : Integer;
begin
  for i:=0 to FList.Count-1 do
  begin
    TUndoableTask(FList[i]).UndoTask;
  end;
end;

procedure TUndoableMultiPurposeTask.AddData(UndoTask : TUndoableTask);
begin
  if UndoTask <> nil then FList.Add(UndoTask);
end;

function TUndoableMultiPurposeTask.GetCount : Integer;
begin
  Result := FList.Count;
end;

function TUndoableMultiPurposeTask.GetName : WideString;
begin
  Result := 'UndoableMultiPurpose';
end;

// -----------------------------------------------------------------------------

constructor TUndoablePoSListTask.Create;
begin
  FAdd := False;
  FUpdate := False;
  FDelete := False;
  Done := False;
  New(FNodeData);
end;

function TUndoablePoSListTask.AddPoS(Range : TSubtitleRange; Cursor : Integer) : Boolean;
begin
  if (not FUpdate) and (not FDelete) then
  begin
    FNodeData.Range := Range;
    FNodeData.Cursor := Cursor;
    FAdd := True;
    Result := True
  end
  else Result:= False;
end;

function TUndoablePoSListTask.UpdatePoS(Node, NewCursor : Integer) : Boolean;
var Nod : PVirtualNode;
    pPoS : PPoSTreeData;    
begin
  if (not FAdd) and (not FDelete) then
  begin
    FNodeIndex := Node;
    Nod := ResynchToolForm.vtvPoSList.GetNodeAt(0,FnodeIndex);
    pPoS := ResynchToolForm.vtvPoSList.GetNodeData(Nod);
    FNodeData.Range := pPoS.Range;
    FNodeData.Cursor := NewCursor;
    FUpdate := True;
    Result := True
  end
  else Result:= False;
end;

function TUndoablePoSListTask.DeletePoS(Node : Integer) : Boolean;
begin
  if (not FAdd) and (not FUpdate) then
  begin
    FNodeIndex := Node;
    FDelete := True;
    Result := True
  end
  else Result:= False;
end;

procedure TUndoablePoSListTask.DoTask;
var TempNodeData : PPoSTreeData;
begin
  if not Done And FAdd then
  begin
    FNodeIndex := ResynchToolForm.AddPoSListNode(FNodeData);
    Done := True;
  end
  else if not Done And FUpdate then
  begin
    FOldCursor := ResynchToolForm.UpdatePoSListNode(FNodeIndex, FNodeData.Cursor);
    Done := True;
  end
  else if not Done And FDelete then
  begin
    FNodeData := ResynchToolForm.DeletePoSListNode(FNodeIndex);
    Done := True;
  end;
end;

procedure TUndoablePoSListTask.UndoTask;
var TempNodeData : PPoSTreeData;
begin
  if Done and FAdd then
  begin
    FNodeData := ResynchToolForm.DeletePoSListNode(FNodeIndex);
    Done := False;
  end
  else if Done and FUpdate then
  begin
    FNodeData.Cursor := ResynchToolForm.UpdatePoSListNode(FNodeIndex, FOldCursor);
    Done := False;
  end
  else if Done and FDelete then
  begin
    FNodeIndex := ResynchToolForm.AddPoSListNode(FNodeData);
    Done := False;
  end;
end;

function TUndoablePoSListTask.GetName : WideString;
begin
  Result := 'UndoableAddUpdateDeletePoS';
end;

// -----------------------------------------------------------------------------

procedure TUndoableColorSubTask.DoTask;
begin
  ResynchToolForm.ColorSubtitles;
end;

procedure TUndoableColorSubTask.UndoTask;
begin
  ResynchToolForm.ColorSubtitles;
end;

function TUndoableColorSubTask.GetName : WideString;
begin
  Result := 'UndoableColorSubtitles';
end;

// -----------------------------------------------------------------------------

procedure TUndoablePoSListClearTask.DoTask;
var pPoS : PPoSTreeData;
    Node : PVirtualNode;
    Last : Integer;
begin
  Node := ResynchToolForm.vtvPoSList.GetFirst;
  while Node <> nil do
  begin
    SetLength(FPPoSList, Length(FPPoSList) + 1);
    Last := High(FPPoSList);
    New(FPPoSList[Last]);
    pPoS := ResynchToolForm.DeletePoSListNode(Node.Index);
    FPPoSList[Last].Range := MainForm.GetAt(pPoS.Range.Node.Index);
    FPPoSList[Last].Cursor := pPoS.Cursor;
    Node := ResynchToolForm.vtvPoSList.GetFirst;
  end;
end;

procedure TUndoablePoSListClearTask.UndoTask;
begin
  while Length(FPPoSList) > 0 do
  begin
    ResynchToolForm.AddPoSListNode(FPPoSList[High(FPPoSList)]);
    SetLength(FPPoSList, Length(FPPoSList) - 1);
  end;
  FreeAndNil(FPPoSList);
end;

function TUndoablePoSListClearTask.GetName : WideString;
begin
  Result := 'UndoableClearPoSList';
end;

// -----------------------------------------------------------------------------

end.
