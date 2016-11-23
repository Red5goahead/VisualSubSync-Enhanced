unit SubtitleModelTests;

interface

uses
  SubtitleModel,
  TestFrameWork;

type
  TSubtitleModelTests = class(TTestCase)
  private
    FSubtitleModel : TSubtitleModel;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestCreateSubtitle;
    procedure TestSetStart;
    procedure TestSetStop;
    procedure TestSetText;
    procedure TestDeleteSubtitle;
    procedure TestSubtitleTimeComparator;
    procedure TestSubtitleSorted;
    procedure TestFindInsertPosition;

    procedure TestBenchmarkSortedOrder;
    procedure TestBenchmarkRandomOrder;


  end;

  TSubtitleItemHack = class(TObject)
  private
    Index : Integer;
    Start : Integer;
    Stop : Integer;
    Text : WideString;
  end;

implementation

uses Classes, SysUtils;

procedure TSubtitleModelTests.SetUp;
begin
  FSubtitleModel := TSubtitleModel.Create;
end;

procedure TSubtitleModelTests.TearDown;
begin
  FreeAndNil(FSubtitleModel);
end;

procedure TSubtitleModelTests.TestCreateSubtitle;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
    SubIndex : Integer;
begin
  CheckEquals(0, FSubtitleModel.GetCount, 'No subtitle yet.');
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  CheckNotNull(Sub, 'Subtitle exists.');
  CheckEquals(1, FSubtitleModel.GetCount, '1 subtitle.');
  
  SubIndex := Sub.GetIndex;
  Transaction.UndoTask;
  CheckEquals(0, FSubtitleModel.GetCount, '0 subtitle.');
  Sub := FSubtitleModel.GetAt(SubIndex);
  CheckNull(Sub, 'No subtitle anymore.');

  Transaction.DoTask;
  CheckEquals(1, FSubtitleModel.GetCount, '1 subtitle again.');
  Sub := FSubtitleModel.GetAt(SubIndex);
  CheckNotNull(Sub, 'Subtitle exists again.');
  CheckEquals(SubIndex, Sub.GetIndex, 'Same index.');
    
  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestSetStart;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.SetSubtitleStart(Sub, 10);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  
  CheckEquals(10, Sub.GetStart, 'Start = 10.');
  Transaction.UndoTask;
  CheckEquals(0, Sub.GetStart, 'Start = 0.');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestSetStop;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.SetSubtitleStop(Sub, 10);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  
  CheckEquals(10, Sub.GetStop, 'Stop = 10.');
  Transaction.UndoTask;
  CheckEquals(0, Sub.GetStop, 'Stop = 0.');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestSetText;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.SetSubtitleText(Sub, '1234 text');
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  
  CheckEquals('1234 text', Sub.GetText, 'Stop = 1234 text.');
  Transaction.UndoTask;
  CheckEquals('', Sub.GetText, 'Stop = .');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestDeleteSubtitle;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
    SubIndex : Integer;
begin
  CheckEquals(0, FSubtitleModel.GetCount, 'No subtitle yet.');
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  CheckNotNull(Sub, 'Subtitle exists.');
  CheckEquals(1, FSubtitleModel.GetCount, 'One subtitle.');

  SubIndex := Sub.GetIndex;
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.DeleteSubtitle(Sub);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  CheckEquals(0, FSubtitleModel.GetCount, 'No subtitle.');

  Transaction.UndoTask;

  CheckEquals(1, FSubtitleModel.GetCount, 'One subtitle again.');
  Sub := FSubtitleModel.GetAt(SubIndex);
  CheckNotNull(Sub, 'Subtitle exists again.');
  CheckEquals(SubIndex, Sub.GetIndex, 'Same index.');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestSubtitleTimeComparator;
var SubItem1, SubItem2 : TSubtitleItem;
begin
  SubItem1 := TSubtitleItem.Create;
  SubItem2 := TSubtitleItem.Create;

  TSubtitleItemHack(SubItem1).Start := 0;
  TSubtitleItemHack(SubItem1).Stop := 0;
  TSubtitleItemHack(SubItem2).Start := 0;
  TSubtitleItemHack(SubItem2).Stop := 0;
  CheckEquals(0, SubtitleTimeComparator(SubItem1, SubItem2), 'strictly =');

  TSubtitleItemHack(SubItem1).Start := 10;
  TSubtitleItemHack(SubItem1).Stop := 15;
  CheckEquals(1, SubtitleTimeComparator(SubItem1, SubItem2), 'strictly >');

  TSubtitleItemHack(SubItem2).Start := 20;
  TSubtitleItemHack(SubItem2).Stop := 25;
  CheckEquals(-1, SubtitleTimeComparator(SubItem1, SubItem2), 'strictly <');

  TSubtitleItemHack(SubItem1).Start := 20;
  TSubtitleItemHack(SubItem1).Stop := 22;
  CheckEquals(-1, SubtitleTimeComparator(SubItem1, SubItem2), 'stop <');

  TSubtitleItemHack(SubItem1).Start := 20;
  TSubtitleItemHack(SubItem1).Stop := 28;
  CheckEquals(1, SubtitleTimeComparator(SubItem1, SubItem2), 'stop >');

  FreeAndNil(SubItem1);
  FreeAndNil(SubItem2);
end;

procedure TSubtitleModelTests.TestSubtitleSorted;
var Sub, Sub2 : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  Sub := FSubtitleModel.GetFirst;
  CheckNull(Sub, 'No sub, GetFirst => nil');

  Sub := FSubtitleModel.GetNext(nil);
  CheckNull(Sub, 'GetNext(nil) => nil');

  Sub := FSubtitleModel.GetPrevious(nil);
  CheckNull(Sub, 'GetPrevious(nil) => nil');

  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 100);
    FSubtitleModel.SetSubtitleStop(Sub, 110);
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 10);
    FSubtitleModel.SetSubtitleStop(Sub, 20);
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 50);
    FSubtitleModel.SetSubtitleStop(Sub, 60);
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 200);
    FSubtitleModel.SetSubtitleStop(Sub, 210);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  CheckEquals(4, FSubtitleModel.GetCount, '4 subtitles.');

  Sub := FSubtitleModel.GetAt(-1);
  CheckNull(Sub, '[-1] => nil');

  // Around 0
  Sub := FSubtitleModel.GetAt(0);
  CheckEquals(10, Sub.GetStart, '[0] start => 10');

  Sub2 := FSubtitleModel.GetPrevious(Sub);
  CheckNull(Sub2, 'GetPrevious([0]) => nil');

  Sub2 := FSubtitleModel.GetNext(Sub);
  CheckEquals(50, Sub2.GetStart, 'GetNext([0]) start => 50');

  // Around 1
  Sub := FSubtitleModel.GetAt(1);
  CheckEquals(50, Sub.GetStart, '[1] start => 50');

  Sub2 := FSubtitleModel.GetPrevious(Sub);
  CheckEquals(10, Sub2.GetStart, 'GetPrevious([1]) => 10');

  Sub2 := FSubtitleModel.GetNext(Sub);
  CheckEquals(100, Sub2.GetStart, 'GetNext([1]) start => 100');

  // Around 2
  Sub := FSubtitleModel.GetAt(2);
  CheckEquals(100, Sub.GetStart, '[2] start => 100');

  Sub2 := FSubtitleModel.GetPrevious(Sub);
  CheckEquals(50, Sub2.GetStart, 'GetPrevious([2]) => 50');

  Sub2 := FSubtitleModel.GetNext(Sub);
  CheckEquals(200, Sub2.GetStart, 'GetNext([2]) start => 200');

  // Around 3
  Sub := FSubtitleModel.GetAt(3);
  CheckEquals(200, Sub.GetStart, '[3] start => 200');

  Sub2 := FSubtitleModel.GetPrevious(Sub);
  CheckEquals(100, Sub2.GetStart, 'GetPrevious([3]) => 100');

  Sub2 := FSubtitleModel.GetNext(Sub);
  CheckNull(Sub2, 'GetNext([3]) => nil');

  // Around 4
  Sub := FSubtitleModel.GetAt(4);
  CheckNull(Sub, '4 => nil');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestBenchmarkSortedOrder;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
    I, t, Duration : Integer;
begin
  FSubtitleModel.BeginTransaction;
  try
    t := 0;
    RandSeed := 0;
    for I := 1 to 1000 do
    begin
      Sub := FSubtitleModel.CreateSubtitle;
      Inc(t, Random(4000));
      FSubtitleModel.SetSubtitleStart(Sub, t);
      Inc(t, Random(4000));
      FSubtitleModel.SetSubtitleStop(Sub, t);
    end;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);  
end;

procedure TSubtitleModelTests.TestBenchmarkRandomOrder;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
    I, t, Duration : Integer;
begin
  FSubtitleModel.BeginTransaction;
  try
    RandSeed := 0;
    for I := 1 to 1000 do
    begin
      Sub := FSubtitleModel.CreateSubtitle;
      t := Random(3600000);
      FSubtitleModel.SetSubtitleStart(Sub, t);
      Inc(t, Random(3000));
      FSubtitleModel.SetSubtitleStop(Sub, t);
    end;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);  
end;

procedure TSubtitleModelTests.TestFindInsertPosition;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 10);
    FSubtitleModel.SetSubtitleStop(Sub, 20);
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 50);
    FSubtitleModel.SetSubtitleStop(Sub, 60);
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 100);
    FSubtitleModel.SetSubtitleStop(Sub, 110);
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 200);
    FSubtitleModel.SetSubtitleStop(Sub, 210);
    Sub := FSubtitleModel.CreateSubtitle;
    FSubtitleModel.SetSubtitleStart(Sub, 240);
    FSubtitleModel.SetSubtitleStop(Sub, 250);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;

  CheckEquals(0, FSubtitleModel.FindInsertPosition(2,4), '0a');
  CheckEquals(0, FSubtitleModel.FindInsertPosition(10,20), '0b');
  CheckEquals(1, FSubtitleModel.FindInsertPosition(30,40), '1a');
  CheckEquals(1, FSubtitleModel.FindInsertPosition(50,60), '1b');
  CheckEquals(2, FSubtitleModel.FindInsertPosition(80,90), '2a');
  CheckEquals(2, FSubtitleModel.FindInsertPosition(100,110), '2b');
  CheckEquals(3, FSubtitleModel.FindInsertPosition(150,160), '3a');
  CheckEquals(3, FSubtitleModel.FindInsertPosition(200,210), '3b');
  CheckEquals(4, FSubtitleModel.FindInsertPosition(220,130), '4a');
  CheckEquals(4, FSubtitleModel.FindInsertPosition(240,250), '4b');
  CheckEquals(5, FSubtitleModel.FindInsertPosition(260,270), '5');

  FreeAndNil(Transaction);
end;

initialization
  TestFramework.RegisterTest('SubtitleModelTests Suite',
    TSubtitleModelTests.Suite);

end.
