unit DiffSubsFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math, Diff, ExtCtrls, Grids, Menus, ComCtrls, TntMenus;

type
  TDiffSubsForm = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    lblFile1: TLabel;
    lblFile2: TLabel;
    StatusBar1: TStatusBar;
    ResultGrid: TStringGrid;
    Options1: TMenuItem;
    mnuIgnoreCase: TMenuItem;
    mnuIgnoreWhiteSpace: TMenuItem;
    mnuView: TMenuItem;
    Exit1: TTntMenuItem;
    NextChanges1: TTntMenuItem;
    PreviousChanges1: TTntMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ResultGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure mnuIgnoreCaseClick(Sender: TObject);
    procedure mnuIgnoreWhiteSpaceClick(Sender: TObject);
    procedure PreviousChanges1Click(Sender: TObject);
    procedure NextChanges1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    Diff: TDiff;
    source1, source2: TStringList;
    result1, result2: TStringList;
    hashlist1, hashlist2: TList;
    procedure Clear(left,right: boolean);
    procedure BuildHashList(left,right: boolean);
  public
    { Public declarations }
    procedure OpenFileText(const filename: string);
    procedure OpenFileVoOther(const filename: string);
    procedure Compare();
  end;

var
  DiffSubsForm: TDiffSubsForm;

implementation

uses HashUnit, SRTParserUnit;

{$R *.dfm}

procedure TDiffSubsForm.FormCreate(Sender: TObject);
begin
  Diff := TDiff.Create(self);
  source1 := TStringList.Create;
  source2 := TStringList.Create;
  result1 := TStringList.Create;
  result2 := TStringList.Create;
  hashlist1 := TList.Create;
  hashlist2 := TList.Create;

  ResultGrid.ColWidths[0] := 40;
  ResultGrid.ColWidths[2] := 40;
  ResultGrid.Canvas.Font := ResultGrid.Font;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.FormActivate(Sender: TObject);
begin
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.FormDestroy(Sender: TObject);
begin
  source1.Free;
  source2.Free;
  result1.Free;
  result2.Free;
  hashlist1.Free;
  hashlist2.Free;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.Clear(left, right: boolean);
begin
  if left then
  begin
    source1.Clear;
    result1.Clear;
    hashlist1.Clear;
    lblFile1.Caption := ' File1: ';
  end;
  if right then
  begin
    source2.Clear;
    result2.Clear;
    hashlist2.Clear;
    lblFile2.Caption := ' File2: ';
  end;
  ResultGrid.RowCount := 0;
  Diff.Clear;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StatusBar1.Panels[3].Text := '';
  mnuView.Enabled := false;
end;
//------------------------------------------------------------------------------

//Because it's SO MUCH EASIER AND FASTER comparing hashes (integers) than
//comparing whole lines of text, we'll build a list of hashes for each line
//in the source files. Each line is represented by a (virtually) unique
//hash that is based on the contents of that line. Also, since the
//likelihood of 2 different lines generating the same hash is so small,
//we can safely ignore that possibility.

procedure TDiffSubsForm.BuildHashList(left,right: boolean);
var
  i: integer;
begin
  if left then
  begin
    hashlist1.Clear;
    for i := 0 to source1.Count -1 do
      hashlist1.Add(HashLine(source1[i],
        mnuIgnoreCase.Checked, mnuIgnoreWhiteSpace.checked));
  end;
  if right then
  begin
    hashlist2.Clear;
    for i := 0 to source2.Count -1 do
      hashlist2.Add(HashLine(source2[i],
        mnuIgnoreCase.Checked, mnuIgnoreWhiteSpace.checked));
  end;

end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.OpenFileText(const filename: string);
var
  i: integer;
  SRTParser : TSRTParser;
begin
  if not fileExists(fileName) then exit;
  Clear(true,false);
  source1.LoadFromFile(fileName);

  SRTParser := TSRTParser.Create;
  SRTParser.Load(Filename);

  if SRTParser.IsUTF8 then
  begin
    source1.Delete(0);
    for i := 0 to source1.Count-1 do
    begin
      source1[i] := Utf8ToAnsi(source1[i]);
    end;
  end;
  FreeAndNil(SRTParser);

  lblFile1.Caption := ' Text: ' + ExtractFileName(fileName);
  hashlist1.Capacity := source1.Count;
  BuildHashList(true,false);

  ResultGrid.RowCount := max(source1.Count, source2.Count);
  for i := 0 to 3 do ResultGrid.Cols[i].BeginUpdate;
  try
    for i := 0 to source1.Count -1 do
    begin
      ResultGrid.Cells[0,i] := inttostr(i+1);
      ResultGrid.Cells[1,i] := source1[i];
    end;
  finally
    for i := 0 to 3 do ResultGrid.Cols[i].EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.OpenFileVoOther(const filename: string);
var
  i: integer;
  SRTParser : TSRTParser;
begin
  if not fileExists(fileName) then exit;
  Clear(false,true);
  source2.LoadFromFile(fileName);

  SRTParser := TSRTParser.Create;
  SRTParser.Load(Filename);

  if SRTParser.IsUTF8 then
  begin
    source2.Delete(0);
    for i := 0 to source2.Count-1 do
    begin
      source2[i] := Utf8ToAnsi(source2[i]);
    end;
  end;
  FreeAndNil(SRTParser);

  lblFile2.Caption := ' VO/Other: ' + ExtractFileName(fileName);
  BuildHashList(false,true);

  ResultGrid.RowCount := max(source1.Count, source2.Count);
  for i := 0 to 3 do ResultGrid.Cols[i].BeginUpdate;
  try
    for i := 0 to source2.Count -1 do
    begin
      ResultGrid.Cells[2,i] := inttostr(i+1);
      ResultGrid.Cells[3,i] := source2[i];
    end;
  finally
    for i := 0 to 3 do ResultGrid.Cols[i].EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.Exit1Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.FormResize(Sender: TObject);
var
  i: integer;
begin
  with ResultGrid do
  begin
    i := (ClientWidth -80) div 2;
    ResultGrid.ColWidths[1] := i;
    ResultGrid.ColWidths[3] := i;
  end;
  lblFile2.Left := Panel1.ClientWidth div 2;
end;
//------------------------------------------------------------------------------

procedure AddCharToStr(var s: string; c: char; kind, lastkind: TChangeKind);
begin
  if (Kind = lastKind) then s := s + c
  else
  case kind of
    ckNone: s := s + '<BC:------>' + c;
    else s := s + '<BC:33FFFF>' + c;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.ResultGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const
  PaleGreen: TColor = $AAFFAA;
  PaleYellow: TColor = $05CBEF;
  PaleGray: TColor = $D0D0D0;
var
  clr: Tcolor;
begin

  if (Diff.Count = 0) then clr := clWhite else clr := clBtnFace;

  if (ACol in [1,3]) and (ARow < Diff.Count) then
    case Diff.Compares[ARow].Kind of
      ckNone: clr := clWhite;
      ckModify,ckDelete: clr := PaleYellow;
      ckAdd: clr := PaleGray;
    end;

  with ResultGrid.Canvas do
  begin
    Brush.Color := clr;
    FillRect(Rect);
    TextRect(Rect, Rect.Left+3,Rect.Top+2, ResultGrid.Cells[ACol,ARow]);

    if (source1.Count = 0) and (source2.Count = 0) then exit;

    //now just some fancy coloring ...
    if (ACol in [0,2]) then
    begin
      Pen.Color := clWhite;
      MoveTo(Rect.Right-1,0);
      LineTo(Rect.Right-1,Rect.Bottom);
    end else
    begin
      if (ACol = 1) then
      begin
        Pen.Color := $333333;
        MoveTo(Rect.Right-1,0);
        LineTo(Rect.Right-1,Rect.Bottom);
      end;
      Pen.Color := clSilver;
      MoveTo(Rect.Left,0);
      LineTo(Rect.Left,Rect.Bottom);
    end;
    //finally, draw the focusRect ...
    if (gdSelected in State) and (ACol = 3) then
    begin
      rect.Left := 0;
      DrawFocusRect(Rect);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.mnuIgnoreCaseClick(Sender: TObject);
begin
  mnuIgnoreCase.Checked := not mnuIgnoreCase.Checked;
  Clear(false,false);
  BuildHashList(true,true);
  Compare();
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.mnuIgnoreWhiteSpaceClick(Sender: TObject);
begin
  mnuIgnoreWhiteSpace.Checked := not mnuIgnoreWhiteSpace.Checked;
  Clear(false,false);
  BuildHashList(true,true);
  Compare();
end;
//------------------------------------------------------------------------------

function GridRect(Coord1, Coord2: TGridCoord): TGridRect;
begin
  with Result do
  begin
    Left := Coord2.X;
    if Coord1.X < Coord2.X then Left := Coord1.X;
    Right := Coord1.X;
    if Coord1.X < Coord2.X then Right := Coord2.X;
    Top := Coord2.Y;
    if Coord1.Y < Coord2.Y then Top := Coord1.Y;
    Bottom := Coord1.Y;
    if Coord1.Y < Coord2.Y then Bottom := Coord2.Y;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.PreviousChanges1Click(Sender: TObject);
var
  row: integer;
  Kind: TChangeKind;
begin
  row := ResultGrid.Selection.Top;
  if row = 0 then exit;
  Kind := Diff.Compares[row].Kind;
  while (row > 0) and (Diff.Compares[row].Kind = Kind) do dec(row);
  if Diff.Compares[row].Kind = ckNone then
  begin
    Kind := ckNone;
    while (row > 0) and
      (Diff.Compares[row].Kind = Kind) do dec(row);
  end;
  ResultGrid.Selection := TGridRect(Rect(0, row, 3, row));
  If row < ResultGrid.TopRow then
    ResultGrid.TopRow := Max(0, row - ResultGrid.VisibleRowCount +1);
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.NextChanges1Click(Sender: TObject);
var
  row: integer;
  Kind: TChangeKind;
begin
  row := ResultGrid.Selection.Top;
  if row = ResultGrid.RowCount -1 then exit;
  Kind := Diff.Compares[row].Kind;
  while (row < ResultGrid.RowCount -1) and
    (Diff.Compares[row].Kind = Kind) do inc(row);
  if Diff.Compares[row].Kind = ckNone then
  begin
    Kind := ckNone;
    while (row < ResultGrid.RowCount -1) and
      (Diff.Compares[row].Kind = Kind) do inc(row);
  end;
  ResultGrid.Selection := TGridRect(Rect(0, row, 3, row));
  if row > ResultGrid.TopRow + ResultGrid.VisibleRowCount -1 then
    ResultGrid.TopRow :=
      max(0,min(row, ResultGrid.RowCount - ResultGrid.VisibleRowCount));
end;
//------------------------------------------------------------------------------

procedure TDiffSubsForm.Compare();
var
  i: integer;
begin
  if (hashlist1.Count = 0) or (hashlist2.Count = 0) then exit;
  screen.Cursor := crHourGlass;
  try
    //this is where it all happens  ...

    //nb: TList.list is a pointer to the bottom of the list's integer array
    Diff.Execute(
      PInteger(hashlist1.list),
      PInteger(hashlist2.list),
      hashlist1.Count,
      hashlist2.Count);

    if Diff.Cancelled then exit;

    //now fill ResultGrid with the differences ...
    for i := 0 to 3 do
    begin
      ResultGrid.Cols[i].BeginUpdate;
      ResultGrid.Cols[i].Clear;
    end;
    try
      ResultGrid.RowCount := Diff.Count;
      for i := 0 to Diff.Count -1 do
        with Diff.Compares[i], ResultGrid do
        begin
          if Kind <> ckAdd then
          begin
            Cells[0,i] := inttostr(oldIndex1+1);
            Cells[1,i] := source1[oldIndex1];
          end;
          if Kind <> ckDelete then
          begin
            Cells[2,i] := inttostr(oldIndex2+1);
            Cells[3,i] := source2[oldIndex2];
          end;
        end;
    finally
      for i := 0 to 3 do ResultGrid.Cols[i].EndUpdate;
    end;

    with Diff.DiffStats do
    begin
      StatusBar1.Panels[0].Text := ' Matches: ' + inttostr(matches);
      StatusBar1.Panels[1].Text := ' Modifies: ' + inttostr(modifies);
      StatusBar1.Panels[2].Text := ' Adds: ' + inttostr(adds);
      StatusBar1.Panels[3].Text := ' Deletes: ' + inttostr(deletes);
    end;

  finally
    screen.Cursor := crDefault;
  end;
  mnuView.Enabled := true;

end;
//------------------------------------------------------------------------------

end.
