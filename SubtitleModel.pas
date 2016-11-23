unit SubtitleModel;

interface

uses Classes, Contnrs, DeCAL;
  
type
  // Forward Declarations
  TSubtitleModel = class;

  TSubtitleItem = class(TObject)
  private
    Index : Integer;   
    Start : Integer;
    Stop : Integer;
    Text : WideString;
  public
    constructor Create;
      
    function GetIndex : Integer;
    function GetStart : Integer;
    function GetStop : Integer;
    function GetText : WideString;
  end;

  
  TUndoableSubTask = class(TObject)
  private
    FSubtitleModel : TSubtitleModel;
  public
    constructor Create(SubtitleModel : TSubtitleModel); virtual;
    
    procedure DoTask; virtual; abstract;
    procedure UndoTask; virtual; abstract;
  end;

  TCreateSubTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetStartTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldStart, FNewStart : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetStopTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldStop, FNewStop : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetTextTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldText, FNewText : WideString;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TDeleteSubTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldSubtitle : TSubtitleItem;
  public
    destructor Destroy; override;
    
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TCompositeSubTask = class(TUndoableSubTask)
  private
    FTasks : TObjectList;
  public
    constructor Create(SubtitleModel : TSubtitleModel); override;
    destructor Destroy; override;

    procedure Add(Task : TUndoableSubTask);

    procedure DoTask; override;
    procedure UndoTask; override;
  end;



  TSubtitleModel = class(TObject)
  private
    FSortedSubs : DArray;
    FTransaction : TCompositeSubTask; // Current transaction

  protected
    function InternalCreateSubtitle : TSubtitleItem;
    procedure InternalDeleteSubtitle(const Index : Integer; FreeMemory : Boolean = True);
    procedure InternalInsertSubtitle(SubtitleItem : TSubtitleItem);
    procedure InternalSetStart(Sub : TSubtitleItem; const Value : Integer);
    procedure InternalSetStop(Sub : TSubtitleItem; const Value : Integer);
    procedure InternalSetText(Sub : TSubtitleItem; const Value : WideString);
    procedure InternalReindex(const FromIndex : Integer = 0);


  public
    constructor Create;
    destructor Destroy; override;
    
    function CreateSubtitle : TSubtitleItem;
    procedure DeleteSubtitle(Sub : TSubtitleItem); overload;
    procedure DeleteSubtitle(const Idx : Integer); overload;
    
    procedure BeginTransaction;
    function EndTransaction : TCompositeSubTask;
    
    procedure SetSubtitleStart(Sub : TSubtitleItem; const Value : Integer);
    procedure SetSubtitleStop(Sub : TSubtitleItem; const Value : Integer);
    //procedure SetSubtitleStartStop(Sub : TSubtitleItem; Start, Stop : Integer);
    procedure SetSubtitleText(Sub : TSubtitleItem; const Value : WideString);

    function GetCount : Integer;
    function GetAt(const Idx : Integer) : TSubtitleItem;

    function GetFirst : TSubtitleItem;
    function GetNext(const Sub : TSubtitleItem) : TSubtitleItem;
    function GetPrevious(const Sub : TSubtitleItem) : TSubtitleItem;


    function FindInsertPosition(const Start, Stop : Integer) : Integer; overload;
    function FindInsertPosition(const Sub : TSubtitleItem) : Integer; overload;
    function GetBetween(const Start, Stop : Integer) : Integer;
  end;

  function SubtitleTimeComparator(const Item1, Item2 : TSubtitleItem) : Integer;

implementation

uses SysUtils;

// -----------------------------------------------------------------------------

function SubtitleTimeDComparator(ptr : Pointer; const obj1, obj2 : DObject) : Integer;
begin
  Result := SubtitleTimeComparator(TSubtitleItem(asObject(obj1)), TSubtitleItem(asObject(obj2))); 
end;

function SubtitleTimeComparator(const Item1, Item2 : TSubtitleItem) : Integer;
begin
  if (Item1.Start < Item2.Start ) then
    Result := -1
  else if (Item1.Start > Item2.Start) then
    Result := 1
  else
  begin
    if (Item1.Stop < Item2.Stop) then
      Result := -1
    else if (Item1.Stop > Item2.Stop) then
      Result := 1
    else
      Result := 0
  end
end;

// -----------------------------------------------------------------------------

constructor TSubtitleItem.Create;
begin
  Start := 0;
  Stop := 0;
end;

function TSubtitleItem.GetIndex : Integer;
begin
  Result := Index;
end;

function TSubtitleItem.GetStart : Integer;
begin
  Result := Start;
end;

function TSubtitleItem.GetStop : Integer;
begin
  Result := Stop;
end;

function TSubtitleItem.GetText : WideString;
begin
  Result := Text;
end;

// -----------------------------------------------------------------------------

constructor TUndoableSubTask.Create(SubtitleModel : TSubtitleModel);
begin
  FSubtitleModel := SubtitleModel;
end;

// ------

procedure TCreateSubTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.InternalCreateSubtitle;
  FSubtitleIndex := Sub.GetIndex;
end;

procedure TCreateSubTask.UndoTask;
begin
  FSubtitleModel.InternalDeleteSubtitle(FSubtitleIndex);
end;

// ------

procedure TSetStartTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStart(Sub, FNewStart);
end;

procedure TSetStartTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStart(Sub, FOldStart);
end;

// ------

procedure TSetStopTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStop(Sub, FNewStop);
end;

procedure TSetStopTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStop(Sub, FOldStop);
end;

// ------

procedure TSetTextTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetText(Sub, FNewText);
end;

procedure TSetTextTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetText(Sub, FOldText);
end;

// ------

procedure TDeleteSubTask.DoTask;
begin
  FOldSubtitle := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalDeleteSubtitle(FSubtitleIndex, False);
end;

procedure TDeleteSubTask.UndoTask;
begin
  FSubtitleModel.InternalInsertSubtitle(FOldSubtitle);
  FOldSubtitle := nil;
end;

destructor TDeleteSubTask.Destroy;
begin
  inherited;
  if Assigned(FOldSubtitle) then
  begin
    FreeAndNil(FOldSubtitle);
  end;
end;

// -----------------------------------------------------------------------------

constructor TCompositeSubTask.Create(SubtitleModel : TSubtitleModel);
begin
  inherited Create(SubtitleModel);
  FTasks := TObjectList.Create(True);
end;

destructor TCompositeSubTask.Destroy;
begin
  FreeAndNil(FTasks);
end;

procedure TCompositeSubTask.Add(Task : TUndoableSubTask);
begin
  FTasks.Add(Task);
end;

procedure TCompositeSubTask.DoTask;
var I : Integer;
begin
  for I := 0 to FTasks.Count-1 do
  begin
    TUndoableSubTask(FTasks[i]).DoTask;
  end;
end;

procedure TCompositeSubTask.UndoTask;
var I : Integer;
begin
  for I := FTasks.Count-1 downto 0 do
  begin
    TUndoableSubTask(FTasks[i]).UndoTask;
  end;
end;

// -----------------------------------------------------------------------------

constructor TSubtitleModel.Create;
var SubComparator : DComparator; 
begin
  SubComparator := MakeComparator(SubtitleTimeDComparator);
  FSortedSubs := DArray.CreateWith(SubComparator);
  FTransaction := nil;
end;

destructor TSubtitleModel.Destroy;
begin
  FTransaction.Free;
  if (FTransaction <> nil) then
    FreeAndNil(FTransaction);
  objFree(FSortedSubs);
  FreeAndNil(FSortedSubs);
  inherited;
end;

function TSubtitleModel.InternalCreateSubtitle : TSubtitleItem;
begin
  Result := TSubtitleItem.Create;
  InternalInsertSubtitle(Result);
end;

procedure TSubtitleModel.InternalInsertSubtitle(SubtitleItem : TSubtitleItem);
begin
  FSortedSubs.add([SubtitleItem]);
  InternalReindex;
end;

procedure TSubtitleModel.InternalDeleteSubtitle(const Index : Integer; FreeMemory : Boolean = True);
var Sub : TSubtitleItem;
begin
  Sub := GetAt(Index);
  if Assigned(Sub) then
  begin
    FSortedSubs.removeAt(Index);
    InternalReindex(Sub.Index);
    if (FreeMemory) then
    begin
      FreeAndNil(Sub);
    end;
  end;
end;

procedure TSubtitleModel.InternalSetStart(Sub : TSubtitleItem; const Value : Integer);
var NextSub, PreviousSub : TSubtitleItem;
begin
  Sub.Start := Value;

  // Check with next and previous subtitle to see if subtitle order has changed
  PreviousSub := GetPrevious(Sub);
  if Assigned(PreviousSub) and (SubtitleTimeComparator(Sub, PreviousSub) < 0) then
  begin
    // Need resort
    // TODO optimize sort only the modified subtitle ?
    sort(FSortedSubs);
    InternalReindex;
    Exit;
  end;


  NextSub := GetNext(Sub);
  if Assigned(NextSub) and (SubtitleTimeComparator(Sub, NextSub) > 0) then
  begin
    // Need resort
    // TODO optimize sort only the modified subtitle ?
    sort(FSortedSubs);
    InternalReindex;
    Exit;
  end;
end;

procedure TSubtitleModel.InternalSetStop(Sub : TSubtitleItem; const Value : Integer);
var NextSub, PreviousSub : TSubtitleItem;
begin
  Sub.Stop := Value;

  // Check with next and previous subtitle to see if subtitle order has changed
  NextSub := GetNext(Sub);
  if Assigned(NextSub) and (SubtitleTimeComparator(Sub, NextSub) > 0) then
  begin
    // Need resort
    // TODO optimize sort only the modified subtitle ?
    sort(FSortedSubs);
    InternalReindex;
    Exit;
  end;

  // Check with next and previous subtitle to see if subtitle order has changed
  PreviousSub := GetPrevious(Sub);
  if Assigned(PreviousSub) and (SubtitleTimeComparator(Sub, PreviousSub) < 0) then
  begin
    // Need resort
    // TODO optimize sort only the modified subtitle ?
    sort(FSortedSubs);
    InternalReindex;
    Exit;
  end;
end;

procedure TSubtitleModel.InternalSetText(Sub : TSubtitleItem; const Value : WideString);
begin
  Sub.Text := Value;
end;

procedure TSubtitleModel.InternalReindex(const FromIndex : Integer = 0);
var I : Integer;
    Sub : TSubtitleItem;
begin
  for I := FromIndex to FSortedSubs.size-1 do
  begin
    Sub := TSubtitleItem(FSortedSubs.atAsObject(I));
    Sub.Index := I;
  end;
end;

function TSubtitleModel.CreateSubtitle : TSubtitleItem;
var Task : TCreateSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TCreateSubTask.Create(Self);
  Task.DoTask;
  FTransaction.Add(Task);
  Result := GetAt(Task.FSubtitleIndex);
end;

procedure TSubtitleModel.DeleteSubtitle(Sub : TSubtitleItem);
var Task : TDeleteSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TDeleteSubTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.DoTask;
  FTransaction.Add(Task);
end;

procedure TSubtitleModel.DeleteSubtitle(const Idx : Integer);
var Sub : TSubtitleItem;
begin
  Sub := GetAt(Idx);
  DeleteSubtitle(Sub);
end;

procedure TSubtitleModel.BeginTransaction;
begin
  if Assigned(FTransaction) then
    raise Exception.Create('Nested transaction is not suported.');
  FTransaction := TCompositeSubTask.Create(Self);
end;

function TSubtitleModel.EndTransaction : TCompositeSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Calling EndTransaction without BeginTransaction.');
  Result := FTransaction;
  FTransaction := nil;
end;

procedure TSubtitleModel.SetSubtitleStart(Sub : TSubtitleItem; const Value : Integer);
var Task : TSetStartTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TSetStartTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.FNewStart := Value;
  Task.FOldStart := Sub.Start;
  Task.FSubtitleModel := Self;
  Task.DoTask;
  FTransaction.Add(Task);
end;

procedure TSubtitleModel.SetSubtitleStop(Sub : TSubtitleItem; const Value : Integer);
var Task : TSetStopTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TSetStopTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.FNewStop := Value;
  Task.FOldStop := Sub.Stop;
  Task.FSubtitleModel := Self;
  Task.DoTask;
  FTransaction.Add(Task);
end;

procedure TSubtitleModel.SetSubtitleText(Sub : TSubtitleItem; const Value : WideString);
var Task : TSetTextTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TSetTextTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.FNewText := Value;
  Task.FOldText := Sub.Text;
  Task.FSubtitleModel := Self;
  Task.DoTask;
  FTransaction.Add(Task);
end;

function TSubtitleModel.GetCount : Integer;
begin
  Result := FSortedSubs.size;
end;

function TSubtitleModel.GetAt(const Idx : Integer) : TSubtitleItem;
begin
  if FSortedSubs.legalIndex(Idx) then
    Result := TSubtitleItem(FSortedSubs.atAsObject(Idx))
  else
    Result := nil
end;

function TSubtitleModel.GetFirst : TSubtitleItem;
begin
  Result := GetAt(0);
end;

function TSubtitleModel.GetNext(const Sub : TSubtitleItem) : TSubtitleItem;
var NextIndex : Integer;
begin
  if (Sub <> nil) then
  begin
    NextIndex := Sub.Index + 1;
    Result := GetAt(NextIndex);
  end
  else
    Result := nil
end;

function TSubtitleModel.GetPrevious(const Sub : TSubtitleItem) : TSubtitleItem;
var PreviousIndex : Integer;
begin
  if (Sub <> nil) then
  begin
    PreviousIndex := Sub.Index - 1;
    Result := GetAt(PreviousIndex);
  end
  else
    Result := nil
end;


function _myBinarySearchInWith(_start, _end : DIterator; compare : DComparator;
  const obj : DObject; var ExactMatch : Boolean) : DIterator;
var dist, comp : Integer;
    last : DIterator;
begin

  Assert(diRandom in _start.flags, 'Binary search only on random access iterators');
  last := _end;
  Result := last;

  repeat
    dist := distance(_start, _end) div 2;
    if dist <= 0 then
      begin
        if atEnd(_start) then
        begin
          Result := last;
          Break;
        end;
        comp := compare(getRef(_start)^, obj);
        if comp = 0 then
        begin
          Result := _start;
          ExactMatch := True;
        end
        else if comp > 0 then
          Result := _start
        else
          Result := _end;
        Break;
      end;
    Result := advanceByF(_start, dist);
    comp := compare(getRef(result)^, obj);
    if comp = 0 then
    begin
      ExactMatch := True;
      Break;
    end
    else if comp < 0 then
      _start := result
    else
      _end := result
  until False;
end;

function myBinarySearch(con : DContainer; const obj : array of const; var ExactMatch : Boolean) : DIterator;
begin
  Assert(Low(obj) = High(obj), 'Can only pass one object.');
  Result := _myBinarySearchInWith(con.start, con.finish, con.comparator,
    obj[Low(obj)], ExactMatch);
end;

function TSubtitleModel.FindInsertPosition(
  const Sub: TSubtitleItem): Integer;
var Iter : DIterator;
    ExactMatch : Boolean;
begin
  ExactMatch := False;
  Iter := myBinarySearch(FSortedSubs, [Sub], ExactMatch);
  if not atEnd(Iter) then
    Result := TSubtitleItem(getObject(Iter)).Index
  else
    Result := FSortedSubs.size;
end;

function TSubtitleModel.FindInsertPosition(const Start, Stop : Integer) : Integer;
var Sub : TSubtitleItem;
begin
  Sub := TSubtitleItem.Create;
  Sub.Start := Start;
  Sub.Stop := Stop;
  Result := FindInsertPosition(Sub);
  Sub.Free;
end;

function TSubtitleModel.GetBetween(const Start, Stop: Integer): Integer;
begin
  // TODO
  Result := -1;
end;



end.
