unit KAZipListView;

interface

uses
  Windows,
  Masks,
  Messages,
  SysUtils,
  ShellAPI,
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  CommCtrl;

type
  TLVSortType      = (Ascending, Descending);
  Columns          = (scPath, scDate, scUnCompressedSize, scCompressedSize, scRatio, scComment, scCRC, scEncryption, scAttributes);
  TShowColumns     = SET OF Columns;
  TOnRename        = Procedure (Sender:TObject) Of Object;

  TKAZipListView = class(TListView)
  private
    { Private declarations }
    FShowIcons          : Boolean;
    FImageList          : TImageList;
    FDirList            : TStringList;
    FColList            : TStringList;
    FAutoSizeColumns    : Boolean;

    FInCreate           : Boolean;

    FLastSortType       : TLVSortType;
    FZipItems           : TStringList;
    FExtList            : TStringList;
    FSortColumn         : Integer;
    FLastColumn         : Integer;
    BMP_Up              : TBitmap;
    BMP_Down            : TBitmap;
    BMP_Folder_Small    : TBitmap;
    BMP_Folder_Big      : TBitmap;
    FShowSortGlyph      : Boolean;
    FMultiselect        : Boolean;
    FFilterFolder       : String;
    FFiltered           : Boolean;
    FShowFolders        : Boolean;
    FShowColumns        : TShowColumns;
    FFilterWildcard     : String;
    FOnRename           : TOnRename;
    procedure SetShowColumns(const Value: TShowColumns);
    procedure SetShowFolders(const Value: Boolean);
    procedure SetFiltered(const Value: Boolean);
    procedure SetFilterFolder(const Value: String);
    procedure SetShowIcons(const Value: Boolean);
    procedure SetShowSortGlyph(const Value: Boolean);
    procedure SetAutoSizeColumns(const Value: Boolean);
    function  GetViewStyle: TViewStyle;
    procedure SetFilterWildcard(const Value: String);
    procedure SetOnRename(const Value: TOnRename);
  protected
    { Protected declarations }
    Procedure InitColumns;
    Function  GetID(I:Integer):Integer;
    function  GetDelphiTempFileName: String;
    Function  GetFileIcon(FileName:String;SmallIcon: Boolean):TIcon;
    Procedure BuildIcons;
    Procedure ClearIcons;
    Procedure ResizeListView;
    Procedure DoCompare(Sender: TObject; Item1, Item2: TListItem; Data : Integer; var Compare: Integer);
    Procedure DoSort;

    procedure   SetMultiSelect(Value: Boolean); override;
    procedure   ColClick(Column: TListColumn); override;
    procedure   WndProc(var Message: TMessage); override;
    procedure   SetViewStyle(Value: TViewStyle);override;
    procedure   Edit(const Item: TLVItem); override;
    Procedure   Loaded;Override;
  public
    { Public declarations }
    Procedure   FillListView(List:TStringList);
    Constructor Create(AOwner:TComponent);Override;
    Destructor  Destroy;Override;
  published
    { Published declarations }
    Property ShowIcons         : Boolean      read FShowIcons          write SetShowIcons;
    Property ShowSortGlyph     : Boolean      read FShowSortGlyph      write SetShowSortGlyph;
    Property ShowFolders       : Boolean      read FShowFolders        write SetShowFolders;
    Property ShowColumns       : TShowColumns read FShowColumns        write SetShowColumns;
    Property MultiSelect       : Boolean      read FMultiselect        write SetMultiSelect;
    Property AutoSizeColumns   : Boolean      read FAutoSizeColumns    write SetAutoSizeColumns;
    Property FilterFolder      : String       Read FFilterFolder       Write SetFilterFolder;
    Property FilterWildcard    : String       Read FFilterWildcard     Write SetFilterWildcard;
    Property Filtered          : Boolean      Read FFiltered           Write SetFiltered;
    Property ViewStyle         : TViewStyle   Read GetViewStyle        Write SetViewStyle;
    Property OnRename          : TOnRename    read FOnRename           write SetOnRename;
    Property Items             Stored False;
  end;

procedure Register;

implementation
{$R KAZipListViewRC.res}

procedure Register;
begin
  RegisterComponents('KA', [TKAZipListView]);
end;

{ TKAZipListView }


constructor TKAZipListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInCreate        := True;

  FShowIcons       := True;
  FImageList       := Nil;
  FAutoSizeColumns := False;
  FLastSortType    := Ascending;
  FSortColumn      := -1;
  FLastColumn      := -1;
  FShowSortGlyph   := True;
  FMultiselect     := False;
  BMP_Up           := TBitmap.Create;
  BMP_Down         := TBitmap.Create;
  BMP_Folder_Small := TBitmap.Create;
  BMP_Folder_Big   := TBitmap.Create;

  FShowColumns     := [scPath, scDate, scUnCompressedSize, scCompressedSize, scRatio, scComment, scCRC, scEncryption, scAttributes];

  FZipItems        := TStringList.Create;
  FExtList         := TStringList.Create;
  FDirList         := TStringList.Create;
  FColList         := TStringList.Create;
  FDirList.Sorted  := True;

  BMP_Up.LoadFromResourceName(Hinstance,'LVUP');
  BMP_Down.LoadFromResourceName(Hinstance,'LVDOWN');
  BMP_Folder_Small.LoadFromResourceName(Hinstance,'FOLDER_SMALL');
  BMP_Folder_Big.LoadFromResourceName(Hinstance,'FOLDER_BIG');

  HideSelection    := False;
  RowSelect        := True;
  FInCreate        := False;
end;

destructor TKAZipListView.Destroy;
begin
  BMP_Up.Free;
  BMP_Down.Free;
  BMP_Folder_Small.Free;
  BMP_Folder_Big.Free;
  FExtList.Free;
  FZipItems.Free;
  FDirList.Free;
  FColList.Free;
  inherited Destroy;
end;

procedure TKAZipListView.Loaded;
begin
  inherited Loaded;
  if Columns.Count=0 Then InitColumns;
  if FShowIcons Then BuildIcons;
  SetMultiSelect(FMultiSelect); 
end;


procedure TKAZipListView.WndProc(var Message: TMessage);
Var
  Draw     : TNMLVCustomDraw;
  Rect     : TRect;
  DC       : HDC;
  ColNo    : Integer;
  X        : Integer;
  Y        : Integer;
Begin
  if Message.Msg = WM_ERASEBKGND then
    DefaultHandler(Message)
  Else
    Inherited WndProc(Message);
  if (Message.Msg=WM_NOTIFY) Then
     Begin
        Try
         with TWMNotify(Message) do
           Begin
             Case NMHdr.code of
               NM_CUSTOMDRAW     : Begin
                                     Message.Result := CDRF_DODEFAULT;
                                     Draw := PNMLVCustomDraw(Message.lParam)^;
                                     Case Draw.nmcd.dwDrawStage of
                                          CDDS_PREPAINT     : Begin
                                                                Message.Result := CDRF_NOTIFYITEMDRAW;
                                                              End;
                                          CDDS_ITEMPREPAINT : Begin
                                                                Message.Result :=  CDRF_NOTIFYPOSTPAINT;
                                                              End;
                                          CDDS_ITEMPOSTPAINT : Begin
                                                                  ColNo := Draw.nmcd.dwItemSpec;
                                                                  if (FSortColumn=ColNo) And (FShowSortGlyph) Then
                                                                     Begin
                                                                       Rect  := Draw.nmcd.rc;
                                                                       DC    := Draw.nmcd.hdc;
                                                                       if FLastSortType = Descending Then
                                                                          Begin
                                                                            X := Rect.Right-(BMP_Up.Width+4);
                                                                            Y := ((Rect.Bottom-Rect.Top)-BMP_Up.Height) Div 2;
                                                                            if (Draw.nmcd.uItemState and CDIS_SELECTED > 0) Then
                                                                               Begin
                                                                                 Inc(X,2);
                                                                                 Inc(Y,2);
                                                                               End;
                                                                            BitBlt(DC,X,Y,BMP_Up.Width,BMP_Up.Height,BMP_UP.Canvas.Handle,0,0,SRCCOPY);
                                                                          End;
                                                                       if FLastSortType = Ascending Then
                                                                          Begin
                                                                            X := Rect.Right-(BMP_Down.Width+4);
                                                                            Y := ((Rect.Bottom-Rect.Top)-BMP_Down.Height) Div 2;
                                                                            if (Draw.nmcd.uItemState and CDIS_SELECTED > 0) Then
                                                                               Begin
                                                                                 Inc(X,2);
                                                                                 Inc(Y,2);
                                                                               End;
                                                                            BitBlt(DC,X,y,BMP_Down.Width,BMP_Down.Height,BMP_Down.Canvas.Handle,0,0,SRCCOPY);
                                                                          End;
                                                                     End;
                                                                  Message.Result := CDRF_NOTIFYSUBITEMDRAW;
                                                               End;
                                     End;
                                   End;
              End;
          End;
        Finally
        End;
     End;
End;

Procedure TKAZipListView.InitColumns;
var
  Col  : TListColumn;
Begin
  Columns.Clear;
  FColList.Clear;
  Col          := Columns.Add;
  Col.Caption  := 'File Name';
  Col.Width    := 150;
  Col.Tag      := 0;

  if scPath In FShowColumns Then
     Begin
       Col          := Columns.Add;
       Col.Caption  := 'File Path';
       Col.Width    := 150;
       Col.Tag      := 1;
     End;

  if scDate In FShowColumns Then
     Begin
        Col          := Columns.Add;
        Col.Caption  := 'File Date';
        Col.Width    := 120;
        Col.Tag      := 2;
     End;

  if scUnCompressedSize In FShowColumns Then
     Begin
        Col          := Columns.Add;
        Col.Caption  := 'File Size';
        Col.Width    := 100;
        Col.Tag      := 3;
     End;

  if scCompressedSize In FShowColumns Then
     Begin
       Col          := Columns.Add;
       Col.Caption  := 'Compressed Size';
       Col.Width    := 100;
       Col.Tag      := 4;
     End;

  if scRatio In FShowColumns Then
     Begin
        Col          := Columns.Add;
        Col.Caption  := 'Ratio';
        Col.Width    := 100;
        Col.Tag      := 5;
     End;

  if scComment In FShowColumns Then
     Begin
       Col          := Columns.Add;
       Col.Caption  := 'Comment';
       Col.Width    := 100;
       Col.Tag      := 6;
     End;

  if scCRC In FShowColumns Then
     Begin
       Col          := Columns.Add;
       Col.Caption  := 'CRC';
       Col.Width    := 100;
       Col.Tag      := 7;
     End;

  if scEncryption In FShowColumns Then
     Begin
        Col          := Columns.Add;
        Col.Caption  := 'Encrypted';
        Col.Width    := 100;
        Col.Tag      := 8;
     End;

  if scAttributes In FShowColumns Then
     Begin
       Col          := Columns.Add;
       Col.Caption  := 'Attributes';
       Col.Width    := 50;
       Col.Tag      := 9;
     End;
End;

function TKAZipListView.GetDelphiTempFileName: String;
Var
 TmpDir : Array[0..1000] of Char;
 TmpFN  : Array[0..1000] of Char;
Begin
 Result := GetCurrentDir;
 if GetTempPath(1000,TmpDir) <> 0 Then
    Begin
     if GetTempFileName(TmpDir,'',0,TmpFN) <> 0 Then Result := StrPas(TmpFN);
    End;
End;


Function TKAZipListView.GetFileIcon(FileName:String;SmallIcon: Boolean):TIcon;
Var
  SHFI    : TSHFileInfo;
Begin
  Result := TIcon.Create;
  if SmallIcon then
     Begin
      Try
        ShGetFileInfo(PChar(Filename), FILE_ATTRIBUTE_NORMAL, SHFI, SizeOf(TShFileInfo), SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
        Result.Handle := SHFI.hIcon;
       Except
        Result.Handle := 0;
       End;
     End
  Else
     Begin
      Try
        ShGetFileInfo(PChar(Filename), FILE_ATTRIBUTE_NORMAL, SHFI, SizeOf(TShFileInfo), SHGFI_ICON or SHGFI_USEFILEATTRIBUTES);
        Result.Handle := SHFI.hIcon;
      Except
       Result.Handle := 0;
      End;
     End;
End;

procedure TKAZipListView.BuildIcons;
Var
  X    : Integer;
  I    : Integer;
  Icon : TIcon;
  Ext  : String;
begin
   if NOT Assigned(LargeImages) Then Exit;
   if NOT Assigned(SmallImages) Then Exit;
   if ViewStyle=vsIcon Then
     Begin
        FImageList        :=  TImageList(LargeImages);
        FImageList.Clear;
        FExtList.Clear;
        FImageList.Width  := 32;
        FImageList.Height := 32;
        FImageList.AddMasked(BMP_Folder_Big,clWhite);
     End
  Else
     Begin
       FImageList        :=  TImageList(SmallImages);
       FImageList.Clear;
       FExtList.Clear;
       FImageList.Width  := 16;
       FImageList.Height := 16;
       FImageList.AddMasked(BMP_Folder_Small,clWhite);
     End;

   For X := 0 To Items.Count-1 do
      Begin
        if Items[X].Data=TObject(-1) Then
           Begin
             Items[X].ImageIndex := 0;
           End
        Else
           Begin
              Ext := ExtractFileExt(Items[X].Caption);
              I   := FExtList.IndexOf(Ext);
              If I = -1 Then
                 Begin
                   Icon                := GetFileIcon(Items[X].Caption, ViewStyle<>vsIcon);
                   Items[X].ImageIndex := FImageList.AddIcon(Icon);
                   Icon.Free;
                 End
              Else
                 Begin
                   Items[X].ImageIndex := I;
                 End;
           End;
      End;
end;

procedure TKAZipListView.ClearIcons;
Var
  X : Integer;
begin
  For X := 0 To Items.Count-1 do
      Begin
        Items[X].ImageIndex := -1;
      End;
end;


Function TKAZipListView.GetID(I:Integer):Integer;
Var
  X : Integer;
Begin
  Result := -1;
  For X := 0 To Columns.Count-1 do
      Begin
        if Column[X].Tag=I Then
           Begin
             Result := X-1;
             Exit;
           End;
      End;
End;

procedure TKAZipListView.FillListView(List:TStringList);
var
  X    : Integer;
  Y    : Integer;
  P    : Integer;
  BR   : Integer;
  ID   : Integer;
  Item : TListItem;
  SL   : TStringList;
  S    : String;
  FF   : String;
  FN   : String;
  Can  : Boolean;

  SC   : Double;
  SU   : Double;
begin
  FDirList.Clear;
  SL  := TStringList.Create;
  LockWindowUpdate(Handle);
  Try
    FF := AnsiLowerCase(FFilterFolder);
    FZipItems.Assign(List);
    For X := 0 To FZipItems.Count-1 do
      Begin
        Can := True;
        if (FFiltered) Then
             Begin
               If AnsiCompareText(FZipItems.Names[X],FFilterFolder)<>0 Then Can := False;
               if (NOT Can) And (FShowFolders) Then
                  Begin
                    If (Pos(FF,AnsiLowerCase(FZipItems.Names[X]))=1) Then
                       Begin
                         S := FZipItems.Strings[X];
                         P := Pos('=',S);
                         S := Copy(S,1,P-1);
                         System.Delete(S,1,Length(FF));
                         S := ExcludeTrailingBackslash(S);
                         P := Pos('\',S);
                         if P > 0 Then S := Copy(S,1,P-1);
                         FDirList.Add(S);
                       End
                    Else
                    If (FF='') Then
                       Begin
                         S := FZipItems.Strings[X];
                         P := Pos('=',S);
                         S := Copy(S,1,P-1);
                         P := Pos('\',S);
                         if P > 0 Then S := Copy(S,1,P-1);
                         FDirList.Add(S);
                       End;
                  End;
             End;
        if (Can) And (FFilterWildCard <> '') And (FFilterWildCard <> '*.*') Then
           Begin
             S   := FZipItems.Strings[X];
             P   := Pos('=',S);
             System.Delete(S,1,P);
             S   := Copy(S,1,Pos(#9,S)-1);
             Can := MatchesMask(S,FFilterWildCard);
           End;
        if Can Then
           Begin
             S   := FZipItems.Strings[X];
             P   := Pos('=',S);
             System.Delete(S,1,P);
             SL.AddObject(S,TObject(X));
           End;
      End;
    Items.Clear;
    For X := 0 To SL.Count-1 do
        Begin
           S  := SL.Strings[X];
           P  := Pos(#9,S);
           if P > 0 Then
              Begin
                FN           := Copy(S,1,P-1);
                IF FN<>'' Then
                   Begin
                    Item         := Items.Add;
                    Item.Caption := Copy(S,1,P-1);
                    Item.Data    := SL.Objects[X];
                    System.Delete(S,1,P);
                    For Y := 0 To Columns.Count-1 do Item.SubItems.Add('');
                    P  := Pos(#9,S);
                    BR := 1;
                    SU := 0;
                    SC := 0;
                    While P > 0 Do
                      Begin
                        ID := GetID(BR);
                        Case BR Of
                             1 : if scPath In FShowColumns Then
                                    Begin
                                      Item.SubItems.Strings[ID] := Copy(S,1,P-1);
                                    End;
                             2 :  if scDate In FShowColumns Then
                                    Begin
                                      Item.SubItems.Strings[ID] := Copy(S,1,P-1);
                                      Item.SubItems.Objects[ID] := FZipItems.Objects[X];
                                    End;
                             3 :  Begin
                                    if scUnCompressedSize In FShowColumns Then
                                        Begin
                                          Item.SubItems.Strings[ID] := Copy(S,1,P-1);
                                          SU := StrToFloat(StringReplace(Item.SubItems.Strings[ID],' ','',[rfReplaceAll]));
                                        End
                                    Else
                                        Begin
                                          SU  := StrToFloat(StringReplace(Item.SubItems.Strings[ID],' ','',[rfReplaceAll]));
                                        End;
                                  End;
                             4 :  Begin
                                    if scCompressedSize In FShowColumns Then
                                       Begin
                                         Item.SubItems.Strings[ID] := Copy(S,1,P-1);
                                         SC := StrToFloat(StringReplace(Copy(S,1,P-1),' ','',[rfReplaceAll]));
                                       End
                                    Else
                                       Begin
                                         SC := StrToFloat(StringReplace(Copy(S,1,P-1),' ','',[rfReplaceAll]));
                                       End;
                                  End;
                             6 :  if scComment In FShowColumns Then
                                    Begin
                                      Item.SubItems.Strings[ID] := Copy(S,1,P-1);
                                    End;
                             7 :  if scCRC In FShowColumns Then
                                    Begin
                                      Item.SubItems.Strings[ID] := Copy(S,1,P-1);
                                    End;
                             8 :  if scEncryption In FShowColumns Then
                                    Begin
                                      Item.SubItems.Strings[ID] := Copy(S,1,P-1);
                                    End;
                        End;
                        System.Delete(S,1,P);
                        Inc(BR);
                        P := Pos(#9,S);
                      End;
                    if scRatio In FShowColumns Then
                       Begin
                          ID := GetID(5);
                          if SU > 0 Then
                             Item.SubItems.Strings[ID] := Format('%2.f%%',[(1-(SC/SU))*100])
                          Else
                             Item.SubItems.Strings[ID] := '0%';
                       End;
                    if scAttributes In FShowColumns Then
                       Begin
                         ID := GetID(BR);
                         Item.SubItems.Strings[ID] := S;
                       End;
                  End;
              End;
        End;
    If FShowFolders Then
       Begin
          if (FF <> '') Then FDirList.Add('..');
          FDirList.Sort;
          For X := FDirList.Count-1 downto 0 do
             Begin
               Item         := Items.Insert(0);
               Item.Caption := FDirList.Strings[X];
               Item.Data    := TObject(-1);
             End;
       End;
    FLastSortType    := Ascending;
    FSortColumn      := 0;
    FLastColumn      := 0;
    DoSort;
    FSortColumn      := -1;
    FLastColumn      := -1;
    if FShowIcons Then BuildIcons;
    if (FAutoSizeColumns) And (Items.Count > 0) Then ResizeListView;
  Finally
    LockWindowUpdate(0);
    SL.Free;
  End;
end;

procedure TKAZipListView.SetFilterFolder(const Value: String);
Var
  SL : TStringList;
begin
  FFilterFolder := Value;
  if (FFiltered) Then
     Begin
       SL := TStringList.Create;
       Try
         SL.Assign(FZipItems);
         FillListView(SL);
       Finally
         SL.Free;
       End;
     End;
end;

procedure TKAZipListView.SetFilterWildcard(const Value: String);
Var
  SL : TStringList;
begin
  FFilterWildcard := Value;
  SL := TStringList.Create;
  Try
    SL.Assign(FZipItems);
    FillListView(SL);
  Finally
    SL.Free;
  End;
end;


procedure TKAZipListView.SetFiltered(const Value: Boolean);
Var
  SL : TStringList;
begin
  FFiltered := Value;
  SL := TStringList.Create;
  Try
    SL.Assign(FZipItems);
    FillListView(SL);
  Finally
    SL.Free;
  End;
end;

procedure TKAZipListView.SetShowFolders(const Value: Boolean);
Var
  SL : TStringList;
begin
  FShowFolders := Value;
  if (FFiltered) Then
     Begin
       SL := TStringList.Create;
       Try
         SL.Assign(FZipItems);
         FillListView(SL);
       Finally
         SL.Free;
       End;
     End;
end;


procedure TKAZipListView.ResizeListView;
Var
  X            : Integer;
  Y            : Integer;
  W            : Integer;
  MaxWidth     : TList;
  TempCanvas   : TControlCanvas;
  Column       : TListColumn;
  GlyphAdd     : Integer;
begin
 MaxWidth                           := TList.Create;
 TempCanvas                         := TControlCanvas.Create;
 TControlCanvas(TempCanvas).Control := Self;
 GlyphAdd                           := 4;
 Try
   For X := 0 To Columns.Count-1 do
       Begin
         MaxWidth.Add(TObject(Columns.Items[X].Width));
       End;
   For X := 0 To Columns.Count-1 do
       Begin
         Column  := Columns.Items[X];
         W := TempCanvas.TextWidth('W'+Column.Caption+'W');
         if FShowSortGlyph Then W := W+BMP_Up.Width+GlyphAdd;
         if W > Integer(MaxWidth.Items[X]) Then MaxWidth.Items[X] := TObject(W);
       End;
   For Y := 0 To Items.Count-1 do
       Begin
         For X := 0 To Columns.Count-1 do
             Begin
               if X = 0 Then
                  Begin
                    W := TempCanvas.TextWidth('W'+Items[Y].Caption+'W')
                  End
               Else
                  Begin
                    if X-1 < Items[Y].Subitems.Count Then
                       W := TempCanvas.TextWidth('W'+Items[Y].Subitems[X-1]+'W')
                    Else
                       W := 0;
                  End;
               if FShowSortGlyph Then W := W+BMP_Up.Width+GlyphAdd;
               if W > Integer(MaxWidth.Items[X]) Then MaxWidth.Items[X] := TObject(W);
             End;
       End;
   if (Checkboxes) Then MaxWidth.Items[0] := TObject(Integer(MaxWidth.Items[0])+30);
   For X := 0 To Columns.Count-1 do
       Begin
         If FAutoSizeColumns Then Columns[X].Width := Integer(MaxWidth.Items[X]);
       End;
  Finally
    MaxWidth.Free;
    TempCanvas.Free;
  End;
end;


Procedure TKAZipListView.DoCompare(Sender: TObject; Item1, Item2: TListItem; Data : Integer; var Compare: Integer);
Var
  S1  : String;
  S2  : String;
  D1  : Double;
  D2  : Double;
  C1  : Cardinal;
  C2  : Cardinal;
  Err : Integer;
Begin
  S1 := '';
  S2 := '';
  if  (Item1.Data=TObject(-1))
  And (Item2.Data<>TObject(-1)) Then
      Begin
        Compare := 0;
        Exit;
      End
  Else
  if  (Item1.Data<>TObject(-1))
  And (Item2.Data=TObject(-1)) Then
      Begin
        Compare := 0;
      End
  Else
  if  (Item1.Data=TObject(-1))
  And (Item2.Data=TObject(-1)) Then
      Begin
        if FSortColumn=0 Then
           Begin
             S1      := Item1.Caption;
             S2      := Item2.Caption;
             if S1='..' Then S2:='..';
             if S2='..' Then S1:='..';
             Compare := WideCompareStr(S1, S2);
             if FLastSortType=Descending Then Compare := Compare*-1;
           End
        Else
           Begin
             Compare := 0;
           End;
        Exit;
      End;

  if FSortColumn=0 Then
     Begin
       S1      := Item1.Caption;
       S2      := Item2.Caption;
       Compare := WideCompareStr(S1, S2);
     End
  Else
     Begin
       S1 := StringReplace(Item1.SubItems.Strings[FSortColumn-1],' ','',[rfReplaceAll]);
       S2 := StringReplace(Item2.SubItems.Strings[FSortColumn-1],' ','',[rfReplaceAll]);
       if Columns[FSortColumn].Tag In [3,4,5] Then
          Begin
             Val(S1,D1,Err);
             Val(S2,D2,Err);
             if D1 > D2 Then Compare :=  1;
             if D2 > D1 Then Compare := -1;
             if D2 = D1 Then Compare :=  0;
          End
       Else
       if Columns[FSortColumn].Tag = 7 Then
          Begin
             C1 := StrToInt('$'+S1);
             C2 := StrToInt('$'+S2);
             if C1 > C2 Then Compare :=  1;
             if C2 > C1 Then Compare := -1;
             if C2 = C1 Then Compare :=  0;
          End
       Else
          Begin
            Compare := WideCompareStr(S1, S2);
          End;
     End;
  if FLastSortType=Descending Then Compare := Compare*-1;
End;



Procedure TKAZipListView.DoSort;
Var
 OCT  : TLVCompareEvent;
Begin
    Begin
      OCT       := OnCompare;
      OnCompare := DoCompare;
      AlphaSort;
      OnCompare := OCT;
    End;
End;

procedure TKAZipListView.ColClick(Column: TListColumn);
Var
  OS : Integer;
  C  : String;
begin
  inherited ColClick(Column);
  OS             := FSortColumn;
  FSortColumn    := Column.Index;
  if FLastColumn = Column.Index Then
     Begin
        if FLastSortType=Ascending Then FLastSortType  := Descending
        Else
        if FLastSortType=Descending Then FLastSortType := Ascending
     End
  Else
     Begin
       FLastSortType   := Ascending;
     End;
  FLastColumn := Column.Index;
  DoSort;
  if (OS <> FSortColumn) And (OS > -1) Then
     Begin
       C := Columns.Items[OS].Caption;
       Columns.Items[OS].Caption := C+' ';
       Columns.Items[OS].Caption := C;
     End;
end;



procedure TKAZipListView.SetShowIcons(const Value: Boolean);
begin
  if FShowIcons <> Value Then
     Begin
       FShowIcons := Value;
       if FShowIcons Then
          Begin
            BuildIcons;
          End
       Else
          Begin
            ClearIcons;
          End;
     End;
end;


procedure TKAZipListView.SetAutoSizeColumns(const Value: Boolean);
begin
  FAutoSizeColumns := Value;
  if csLoading in ComponentState Then Exit;
end;



procedure TKAZipListView.SetShowSortGlyph(const Value: Boolean);
begin
  FShowSortGlyph := Value;
end;


procedure TKAZipListView.SetMultiSelect(Value: Boolean);
Var
 WL : LongInt;
Begin
  FMultiselect := Value;
  if (HandleAllocated) Then
     Begin
       WL           := GetWindowLong(Handle,GWL_STYLE);
       if FMultiselect Then
          WL           := WL - LVS_SINGLESEL
       Else
          WL           := WL + LVS_SINGLESEL;
       SetWindowLong(Handle,GWL_STYLE,WL);
     End;
End;


function TKAZipListView.GetViewStyle: TViewStyle;
begin
  Result := Inherited ViewStyle;
end;

procedure TKAZipListView.SetViewStyle(Value: TViewStyle);
Var
  OVS : TViewStyle;
begin
  OVS := Inherited ViewStyle;
  Inherited SetViewStyle(Value);
  if OVS <> Value Then
     Begin
       if FShowIcons Then BuildIcons;
     End;
end;

procedure TKAZipListView.SetShowColumns(const Value: TShowColumns);
Var
  SL  : TStringList;
  OSC : TShowColumns;
begin
  OSC          := FShowColumns;
  FShowColumns := Value;
  if OSC <> FShowColumns Then
     Begin
       if Not (csLoading in ComponentState) Then
          Begin
            SL           := TStringList.Create;
            Try
              SL.Assign(FZipItems);
              FillListView(SL);
            Finally
              SL.Free;
            End;
          End;
     End;
end;

procedure TKAZipListView.SetOnRename(const Value: TOnRename);
begin
  FOnRename := Value;
end;

procedure TKAZipListView.Edit(const Item: TLVItem);
begin
  inherited Edit(Item);
  if Assigned(FOnRename) Then FOnRename(Self);
end;


end.
