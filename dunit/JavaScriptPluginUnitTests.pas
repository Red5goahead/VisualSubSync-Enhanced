unit JavaScriptPluginUnitTests;

interface

uses
  JavaScriptPluginUnit,
  TestFrameWork;

type
  TSceneChangeWrapperTests = class(TTestCase)
  private
    SceneChangeWrapper : TSceneChangeWrapper;
    EmptySceneChangeWrapper : TSceneChangeWrapper;
    
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    // Test methods
    procedure TestSetStartAndStopOffset;
    procedure TestSetVisible;
    procedure TestContains;
    procedure TestGetCount;
    procedure TestGetAt;
    procedure TestGetNext;
    procedure TestGetPrevious;
    procedure TestDelete;
    procedure TestInsert;

  end;

implementation

uses Types;

// -----------------------------------------------------------------------------

procedure TSceneChangeWrapperTests.SetUp;
var SceneChangeList : TIntegerDynArray;
begin
  EmptySceneChangeWrapper := TSceneChangeWrapper.Create;
  SceneChangeWrapper := TSceneChangeWrapper.Create;
  SetLength(SceneChangeList, 4);
  SceneChangeList[0] := 10;
  SceneChangeList[1] := 20;
  SceneChangeList[2] := 30;
  SceneChangeList[3] := 40;
  SceneChangeWrapper.SetSceneChangeList(SceneChangeList);  
end;

procedure TSceneChangeWrapperTests.TearDown;
begin
  SceneChangeWrapper.Free;
  EmptySceneChangeWrapper.Free;
end;

procedure TSceneChangeWrapperTests.TestGetCount;
begin
  CheckEquals(0, EmptySceneChangeWrapper.GetCount, 'EmptySceneChangeWrapper.GetCount');
  CheckEquals(4, SceneChangeWrapper.GetCount, 'SceneChangeWrapper.GetCount');
end;

procedure TSceneChangeWrapperTests.TestGetAt;
begin
  CheckEquals(-1, EmptySceneChangeWrapper.GetAt(-1), 'EmptySceneChangeWrapper.GetAt(-1)');
  CheckEquals(-1, EmptySceneChangeWrapper.GetAt(0), 'EmptySceneChangeWrapper.GetAt(0)');
  CheckEquals(-1, EmptySceneChangeWrapper.GetAt(1), 'EmptySceneChangeWrapper.GetAt(1)');
  CheckEquals(-1, SceneChangeWrapper.GetAt(-1), 'SceneChangeWrapper.GetAt(-1)');
  CheckEquals(10, SceneChangeWrapper.GetAt(0), 'SceneChangeWrapper.GetAt(0)');
  CheckEquals(40, SceneChangeWrapper.GetAt(3), 'SceneChangeWrapper.GetAt(3)');
  CheckEquals(-1, SceneChangeWrapper.GetAt(4), 'SceneChangeWrapper.GetAt(4)');
end;

procedure TSceneChangeWrapperTests.TestContains;
begin
  CheckFalse(EmptySceneChangeWrapper.Contains(5,10), 'EmptySceneChangeWrapper.Contains(5,10)');

  CheckFalse(SceneChangeWrapper.Contains(0,5), 'Contains(0,5)');
  CheckTrue(SceneChangeWrapper.Contains(5,10), 'Contains(5,10)');
  CheckTrue(SceneChangeWrapper.Contains(5,15), 'Contains(5,15)');
  CheckTrue(SceneChangeWrapper.Contains(10,10), 'Contains(10,10)');
  CheckTrue(SceneChangeWrapper.Contains(10,15), 'Contains(10,15)');
  CheckFalse(SceneChangeWrapper.Contains(12,15), 'Contains(12,15)');
  CheckTrue(SceneChangeWrapper.Contains(35,40), 'Contains(35,40)');
  CheckTrue(SceneChangeWrapper.Contains(35,45), 'Contains(35,45)');
  CheckTrue(SceneChangeWrapper.Contains(40,45), 'Contains(40,45)');
  CheckFalse(SceneChangeWrapper.Contains(42,45), 'Contains(42,45)');
  CheckTrue(SceneChangeWrapper.Contains(0,45), 'Contains(0,45)');
end;

procedure TSceneChangeWrapperTests.TestGetNext;
begin
  CheckEquals(-1, EmptySceneChangeWrapper.GetNext(10), 'EmptySceneChangeWrapper.GetNext(10)');
  
  CheckEquals(10, SceneChangeWrapper.GetNext(0), 'SceneChangeWrapper.GetNext(0)');
  CheckEquals(10, SceneChangeWrapper.GetNext(10), 'SceneChangeWrapper.GetNext(10)');
  CheckEquals(20, SceneChangeWrapper.GetNext(15), 'SceneChangeWrapper.GetNext(15)');
  CheckEquals(-1, SceneChangeWrapper.GetNext(45), 'SceneChangeWrapper.GetNext(45)');
end;

procedure TSceneChangeWrapperTests.TestGetPrevious;
begin
  CheckEquals(-1, EmptySceneChangeWrapper.GetPrevious(10), 'EmptySceneChangeWrapper.GetPrevious(10)');

  CheckEquals(-1, SceneChangeWrapper.GetPrevious(0), 'SceneChangeWrapper.GetPrevious(0)');
  CheckEquals(10, SceneChangeWrapper.GetPrevious(10), 'SceneChangeWrapper.GetPrevious(10)');
  CheckEquals(20, SceneChangeWrapper.GetPrevious(20), 'SceneChangeWrapper.GetPrevious(20)');
  CheckEquals(10, SceneChangeWrapper.GetPrevious(15), 'SceneChangeWrapper.GetPrevious(15)');
  CheckEquals(40, SceneChangeWrapper.GetPrevious(45), 'SceneChangeWrapper.GetPrevious(45)');
end;

procedure TSceneChangeWrapperTests.TestSetVisible;
begin
  CheckFalse(SceneChangeWrapper.Visible, 'SceneChangeWrapper.Visible = False');
  SceneChangeWrapper.SetVisible(True);
  CheckTrue(SceneChangeWrapper.Visible, 'SceneChangeWrapper.Visible = True');
end;

procedure TSceneChangeWrapperTests.TestSetStartAndStopOffset;
begin
  CheckEquals(0, SceneChangeWrapper.StartOffset, 'SceneChangeWrapper.StartOffset = 0');
  CheckEquals(0, SceneChangeWrapper.StopOffset, 'SceneChangeWrapper.StopOffset = 0');
  CheckEquals(0, SceneChangeWrapper.FilterOffset, 'SceneChangeWrapper.FilterOffset = 0');
  SceneChangeWrapper.SetOffsets(123,456,789);
  CheckEquals(123, SceneChangeWrapper.StartOffset, 'SceneChangeWrapper.StartOffset = 123');
  CheckEquals(456, SceneChangeWrapper.StopOffset, 'SceneChangeWrapper.StartOffset = 456');
  CheckEquals(789, SceneChangeWrapper.FilterOffset, 'SceneChangeWrapper.FilterOffset = 789');
end;

procedure TSceneChangeWrapperTests.TestDelete;
var SCArray : TIntegerDynArray;
begin
  SCArray := EmptySceneChangeWrapper.Delete(10, 20);
  CheckEquals(0, Length(SCArray), 'EmptySceneChangeWrapper.Delete(10, 20)');

  SCArray := SceneChangeWrapper.Delete(-5, 5);
  CheckEquals(0, Length(SCArray), 'SceneChangeWrapper.Delete(-5, 5)');

  SCArray := SceneChangeWrapper.Delete(45, 50);
  CheckEquals(0, Length(SCArray), 'SceneChangeWrapper.Delete(45, 50)');

  SCArray := SceneChangeWrapper.Delete(5, 10);
  CheckEquals(1, Length(SCArray), 'SceneChangeWrapper.Delete(5, 10)');
  CheckEquals(10, SCArray[0], 'SCArray[0] = 10');
  CheckEquals(3, Length(SceneChangeWrapper.GetSCArray), 'Length(SceneChangeWrapper.GetSCArray) = 3');
  CheckEquals(20, SceneChangeWrapper.GetSCArray[0], 'SceneChangeWrapper.GetSCArray[0] = 20');
  CheckEquals(30, SceneChangeWrapper.GetSCArray[1], 'SceneChangeWrapper.GetSCArray[1] = 30');
  CheckEquals(40, SceneChangeWrapper.GetSCArray[2], 'SceneChangeWrapper.GetSCArray[2] = 40');

  SCArray := SceneChangeWrapper.Delete(25, 35);
  CheckEquals(1, Length(SCArray), 'SceneChangeWrapper.Delete(25, 35)');
  CheckEquals(30, SCArray[0], 'SCArray[0] = 30');
  CheckEquals(2, Length(SceneChangeWrapper.GetSCArray), 'Length(SceneChangeWrapper.GetSCArray) = 2');
  CheckEquals(20, SceneChangeWrapper.GetSCArray[0], 'SceneChangeWrapper.GetSCArray[0] = 20');
  CheckEquals(40, SceneChangeWrapper.GetSCArray[1], 'SceneChangeWrapper.GetSCArray[1] = 40');

  SCArray := SceneChangeWrapper.Delete(40, 45);
  CheckEquals(1, Length(SCArray), 'SceneChangeWrapper.Delete(40, 45)');
  CheckEquals(40, SCArray[0], 'SCArray[0] = 40');
  CheckEquals(1, Length(SceneChangeWrapper.GetSCArray), 'Length(SceneChangeWrapper.GetSCArray) = 1');
  CheckEquals(20, SceneChangeWrapper.GetSCArray[0], 'SceneChangeWrapper.GetSCArray[0] = 20');

  SCArray := SceneChangeWrapper.Delete(20, 20);
  CheckEquals(1, Length(SCArray), 'SceneChangeWrapper.Delete(20, 20)');
  CheckEquals(20, SCArray[0], 'SCArray[0] = 20');
  CheckEquals(0, Length(SceneChangeWrapper.GetSCArray), 'Length(SceneChangeWrapper.GetSCArray) = 0');
end;

procedure TSceneChangeWrapperTests.TestInsert;
var SCArray : TIntegerDynArray;
begin
  SetLength(SCArray, 0);
  SceneChangeWrapper.Insert(SCArray);
  CheckEquals(4, Length(SceneChangeWrapper.GetSCArray), 'Length(SceneChangeWrapper.GetSCArray) = 4');

  SetLength(SCArray, 3);
  SCArray[0] := 5;
  SCArray[1] := 20;
  SCArray[2] := 45;
  SceneChangeWrapper.Insert(SCArray);
  CheckEquals(6, Length(SceneChangeWrapper.GetSCArray), 'Length(SceneChangeWrapper.GetSCArray) = 7');
  CheckEquals(5, SceneChangeWrapper.GetSCArray[0], 'SceneChangeWrapper.GetSCArray[0] = 5');
  CheckEquals(45, SceneChangeWrapper.GetSCArray[5], 'SceneChangeWrapper.GetSCArray[5] = 45');
end;

// -----------------------------------------------------------------------------

initialization
  TestFramework.RegisterTest('JavaScriptPluginUnitTests Suite',
    TSceneChangeWrapperTests.Suite);

end.

