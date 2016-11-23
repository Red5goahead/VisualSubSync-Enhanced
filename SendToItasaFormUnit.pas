unit SendToItasaFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls, SendToItasaMiscUnit,
  IniFiles, ComCtrls, TntComCtrls, Grids, TntGrids,
  Buttons, TntButtons, ImgList, VirtualTrees, Menus, TntMenus, TntDialogs, RegExpr;

type
  TSendToItasaForm = class(TForm)
    EditRename: TTntEdit;
    ButtonSend: TTntButton;
    Shape1: TTntShape;
    ButtonAddResynch: TTntButton;
    TntLabel1: TTntLabel;
    vdtQueue: TVirtualDrawTree;
    rbPannello: TTntRadioButton;
    rbTopic: TTntRadioButton;
    TntPopupMenu1: TTntPopupMenu;
    Removeselected1: TTntMenuItem;
    MemoPreviewMsg: TTntMemo;
    ButtonMsgOk: TTntButton;
    ChkPreviewMsg: TTntCheckBox;
    ButtonBold: TTntButton;
    ButtonItalic: TTntButton;
    ButtonUnderline: TTntButton;
    OpenDialog1: TTntOpenDialog;
    ButtonPacket: TTntSpeedButton;
    rbManual: TTntRadioButton;
    EditTopicID: TTntEdit;
    Shape2: TTntShape;
    procedure FormShow(Sender: TObject);
    procedure EditRenameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonAddResynchClick(Sender: TObject);
    procedure vdtQueueDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vdtQueueCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure Removeselected1Click(Sender: TObject);
    procedure ButtonMsgOkClick(Sender: TObject);
    procedure AddTag(var Msg: string; tag:string; ids, len:Integer);
    procedure ButtonBoldClick(Sender: TObject);
    procedure ButtonItalicClick(Sender: TObject);
    procedure ButtonUnderlineClick(Sender: TObject);
    procedure ButtonPacketClick(Sender: TObject);
    procedure rbPannelloClick(Sender: TObject);
    procedure EditTopicIDChange(Sender: TObject);
    procedure rbManualClick(Sender: TObject);
    procedure rbTopicClick(Sender: TObject);
  private
    BoldFont : HFONT;
    NormalFont : HFONT;
    NullPen : HPEN;
    AlterBrush : HBRUSH;

    procedure CreateFont;
    function CalculateNodeHeight : Integer;
    function ApriFileSub(NomeFile: string): Boolean;
    function CountPannello : Integer;
    function CountTopic : Integer;
    procedure AfterUpdate;
    procedure UpdateComponent;
    function AddToExisting : Boolean;
    procedure AddResynchPannello(TopicId: string);
    procedure AddResynchTopic;
  public
    function Inizializza(Filename: WideString): Boolean;
    procedure SetText(Filename: WideString);
    { Public declarations }
  end;


var
  SendToItasaForm: TSendToItasaForm;
  CompletePacket: Boolean;
  //variable to prevent infinite loops while sending to topic
  TopicLoop: Boolean;

  NomeFile: TNomeFile;
  OpenedFile: TOpenedFile;
  Temp: TTemp;
  Reg: TRegExpr;

  UserInfo: TUserInfo;
  InfoTopic: TInfoTopic;
  InfoPannello: TPannello;
  InfoPannelloInvia: TPannelloInvia;

  ItasaUrls: TItasaUrls =
  (
    URL_ITASA: 'http://www.italiansubs.net/';
    URL_LOGIN: 'index.php';
  {$ifdef DEBUG}
    URL_TOPIC_ID: 'forum/index.php?topic=171';
  {$else}
    URL_TOPIC_ID :'forum/index.php?topic=57033';
  {$endif}
    URL_POST_ID: '/?action=post2';
    URL_PANNELLO: 'forum/index.php?action=traduzioni';
    URL_MANUALE:'index.php?option=com_news&Itemid=13&task=upload&mode=resynch&topic=';
  );

const
  //VirtualTree Draw Parameters
  NodeLeftMargin : Integer = 6;
  NodeTopMargin : Integer = 4;
  NodeInterline : Double = 1.1;


implementation
{$R *.dfm}

uses Main, MiscToolsUnit, PreferencesFormUnit, GlobalUnit, TntSysUtils;

procedure TSendToItasaForm.FormCreate(Sender: TObject);
begin

  NomeFile := TNomeFile.Create;

  Temp.COOKIE := TStringList.Create;
  Temp.TEMP := TStringList.Create;
  UserInfo.Messaggio  := TStringList.Create;
  UserInfo.MessaggioP := TStringList.Create;

  Temp.TEMP_DIR := GetTemporaryFolder;
  if (Length(Temp.TEMP_DIR) > 0) then
  begin
    Temp.TEMP_DIR := Temp.TEMP_DIR + WideIncludeTrailingBackslash(RootAppData);
    Temp.USE_TEMP_DIR := True;
    if not DirectoryExists(Temp.TEMP_DIR) then
      Temp.USE_TEMP_DIR := CreateDir(Temp.TEMP_DIR);
  end;

  //initialize the random function
  Random(MaxInt);

  //initialize the virtualtree
  with vdtQueue do
  begin
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 7;
    NodeDataSize := SizeOf( TFileSendTreeData );
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReportMode];
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
      [toFullRowSelect] + [toRightClickSelect];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines, toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
    DefaultNodeHeight := CalculateNodeHeight;
    Clear;
  end;

  //initialize font, brush and pen
  CreateFont;
  NullPen := CreatePen( PS_NULL, 0, 0 );
  AlterBrush := CreateSolidBrush( clMoneyGreen );
  
  Reg := TregExpr.Create;
  Reg.Expression := '^\d{5,6}$';
  {$ifdef DEBUG}
  ShowMessage('DEBUG VERSION');
  {$endif}
end;

function TSendToItasaForm.CalculateNodeHeight : Integer;
var OldFont : HFONT;
    DC : HDC;
    xysize : Size;
begin
  Result := 0;
  DC := vdtQueue.Canvas.Handle;
  Result := Result + NodeTopMargin;
  OldFont := SelectObject( DC, NormalFont );
  GetTextExtentPoint32W( DC, 'W', 1, xysize );
  Result := Round(Result + (xysize.cy * NodeInterline));
  SelectObject(DC, OldFont);
  Result := Result + NodeTopMargin;
end;

procedure TSendToItasaForm.CreateFont;
var FontLOG : LOGFONT;
begin
  if BoldFont <> 0 then
    DeleteObject(BoldFont);
  if NormalFont <> 0 then
    DeleteObject(NormalFont);
  ZeroMemory(@FontLOG, SizeOf(FontLOG));
  StrCopy(FontLOG.lfFaceName,PChar(vdtQueue.Canvas.Font.Name));
  FontLOG.lfHeight := vdtQueue.Canvas.Font.Height;
  FontLOG.lfCharSet := vdtQueue.Canvas.Font.Charset;
  FontLOG.lfWeight := FW_BOLD;
  BoldFont := CreateFontIndirect(FontLOG);
  FontLOG.lfWeight := FW_NORMAL;
  NormalFont := CreateFontIndirect(FontLOG);
end;

procedure TSendToItasaForm.FormShow(Sender: TObject);
begin
  HttpInit(True);

  UserInfo.Username   := MainForm.ConfigObject.Username;
  UserInfo.Passwd     := MainForm.ConfigObject.Passwd;
  UserInfo.Messaggio  := MainForm.ConfigObject.Msg;
  UserInfo.Log        := False;
  if MainForm.ConfigObject.ZipPath <> '' then
    if not MainForm.ConfigObject.ZipPathOnlyForCP then
      if DirectoryExists(MainForm.ConfigObject.ZipPath) then
        Temp.TEMP_DIR := MainForm.ConfigObject.ZipPath + '\';

  CompletePacket := False;

  UpdateComponent;

  EditRenameChange(nil);
  //set the form height to hide the MemoPreviewMsg component

  ClientHeight:=MemoPreviewMsg.Top-5;
  //Height:=340;
end;

function TSendToItasaForm.Inizializza(Filename: WideString): Boolean;
const
  VerArr : array[1..9] of string = ('720p','1080i','1080p','WEB-DL','WEB.DL','DVDRip','BDRip','Bluray','BRRip');
  Ver2Arr: array[1..2,1..6] of string = (('P','R','PR','RR','RP','I'),('PROPER','REPACK','PROPER.REPACK','REAL.REPACK','REAL.PROPER','INTERNAL'));
  Ver3Arr: array[1..1] of string = ('extended');
var
   i : Integer;
begin
  Result := False;
  if ApriFileSub(FileName) then
  begin
    //TODO: read credit
    SetText(OpenedFile.NOME_SUB);
    //look for video version
    if NomeFile.Version = '' then
      for i := 1 to Length(VerArr) do
        if Pos(UpperCase(VerArr[i]),UpperCase(ExtractFileName(MainForm.CurrentProject.VideoSource))) <> 0 then
          SetText(StringReplace(NomeFile.SetVersion(VerArr[i]),'WEB.DL','WEB-DL',[rfReplaceAll]));

    //look for video version2
    if NomeFile.Version2 = '' then
      for i := 1 to Length(Ver2Arr[2]) do
        if Pos(UpperCase(Ver2Arr[2][i]),UpperCase(ExtractFileName(MainForm.CurrentProject.VideoSource))) <> 0 then
          SetText(NomeFile.SetVersion2(Ver2Arr[1][i]));

    //look for video version3
    if NomeFile.Version3 = '' then
      for i := 1 to Length(Ver3Arr) do
        if Pos(UpperCase(Ver3Arr[i]),UpperCase(ExtractFileName(MainForm.CurrentProject.VideoSource))) <> 0 then
          SetText(NomeFile.SetVersion3(Ver3Arr[i]));

    Result := True;
  end;
end;

procedure TSendToItasaForm.SetText(Filename: WideString);
var i:Integer;
begin
  i := EditRename.SelStart;
  EditRename.Text := Filename;
  EditRename.SelStart :=i ;
end;

//open the resynch
function TSendToItasaForm.ApriFileSub(NomeFile: string): Boolean;
begin
  Result := False;
  if FileExists(NomeFile) then
    with OpenedFile do
    begin
      DIR_SUB := ExtractFilePath(NomeFile);
      FILE_SUB := ExtractFileName(NomeFile);
      EXT_SUB := ExtractFileExt(NomeFile);
      if (EXT_SUB = '.srt') or (EXT_SUB = '.ass') then
      begin
        NOME_SUB := Copy(FILE_SUB, 0, Pos(EXT_SUB, FILE_SUB) - 1);
        Result := True;
      end;
    end;
end;

procedure TSendToItasaForm.EditRenameChange(Sender: TObject);
begin
  NomeFile.NewFile(EditRename.Text);
  if NomeFile.Valid
  then Shape1.Pen.Color := clGreen
  else Shape1.Pen.Color := clRed;
  if rbManual.Checked
  then ButtonAddResynch.Enabled := NomeFile.Valid and Reg.Exec(EditTopicID.Text)
  else
  begin
    if rbPannello.Checked or rbTopic.Checked
    then ButtonAddResynch.Enabled := NomeFile.Valid
    else ButtonAddResynch.Enabled := False;
  end;
end;

procedure TSendToItasaForm.EditTopicIDChange(Sender: TObject);
begin
  if Reg.Exec(EditTopicID.Text)
  then Shape2.Pen.Color := clGreen
  else Shape2.Pen.Color := clRed;
  ButtonAddResynch.Enabled:=Reg.Exec(EditTopicID.Text) and NomeFile.Valid;
end;

function TSendToItasaForm.CountPannello : Integer;
var
  Node : PVirtualNode;
  pFS : PFileSendTreeData;
begin
  Result:=0;
  Node := vdtQueue.GetFirst;
  while Assigned(Node) do
  begin
    pFS := vdtQueue.GetNodeData(Node);
    if pFS.Pannello and not pFS.Sended then Result := Result + 1;
    Node := vdtQueue.GetNext(Node);
  end;
end;

function TSendToItasaForm.CountTopic : Integer;
var
  Node : PVirtualNode;
  pFS : PFileSendTreeData;
begin
  Result:=0;
  Node := vdtQueue.GetFirst;
  while Assigned(Node) do
  begin
    pFS := vdtQueue.GetNodeData(Node);
    if not pFS.Pannello and not pFS.Sended then Result := Result + 1;
    Node := vdtQueue.GetNext(Node);
  end;
end;

procedure TSendToItasaForm.ButtonSendClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  LogMsg('Start sending resynch...');
  try
    if ItasaLogin then
    begin
      if CountPannello > 0 then
        ItasaPostPannello;
      AfterUpdate;

      if CountTopic > 0 then
      begin
        CreateMessage;
        if ChkPreviewMsg.Checked then
        begin
          MemoPreviewMsg.Text:=UserInfo.MessaggioP.Text;
          ButtonSend.Enabled:=False;
          ButtonMsgOk.Enabled:=True;
          ClientHeight:=ButtonMsgOk.Top+45;
          //Height:=510;
        end
        else
        begin
          TopicLoop := False;
          if not ItasaDownloadInfoTopic then
            ShowMessage('info error')
          else
            ItasaPostTopic;
        end;
      end;
    end;
  except
    on E : Exception do
      ShowMessage(E.Message);
  end;
  UpdateComponent;
  Screen.Cursor:=crDefault;
end;

procedure TSendToItasaForm.ButtonMsgOkClick(Sender: TObject);
begin
  try
    Screen.Cursor:=crHourGlass;
    UserInfo.MessaggioP.Text:=MemoPreviewMsg.Text;
    TopicLoop := False;
    if not ItasaDownloadInfoTopic then
      ShowMessage('info error')
    else
      ItasaPostTopic; 
    UpdateComponent;
    Screen.Cursor:=crDefault;
  except
    on E : Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TSendToItasaForm.UpdateComponent;
begin
  vdtQueue.SortTree(0, sdAscending);

  if ( CountPannello + CountTopic > 0 )
  then ButtonSend.Enabled:= True
  else ButtonSend.Enabled:= False;

  if CountTopic > 0
  then ChkPreviewMsg.Enabled:= True
  else ChkPreviewMsg.Enabled:= False;
end;

procedure TSendToItasaForm.AfterUpdate;
begin
  ButtonAddResynch.Enabled := False;
  EditRename.Clear;
  EditTopicID.Clear;
  UpdateComponent;
end;

function TSendToItasaForm.AddToExisting : Boolean;
var
  Node : PVirtualNode;
  pFS : PFileSendTreeData;
  i : Integer;
  Realname, Realname2:string;
  PosBr:Integer;
begin
  Screen.Cursor := crHourGlass;
  Result:=False;
  //check if the newly added is already present in the queue
  Node := vdtQueue.GetFirst;
  while Assigned(Node) do
  begin
    pFS := vdtQueue.GetNodeData(Node);

    PosBr := Pos('(',pFS.Name);
    if PosBr > 0
    then RealName := Trim(Copy(pFS.Name, 1, PosBr-1))
    else Realname := pFS.Name;
    Realname2 := Trim(NomeFile.NamePannello + ' ' + NomeFile.Version);

    //same name and same sending method
    if (Realname = Realname2) and ((pFS.Pannello = rbPannello.Checked) or (pFS.Pannello = rbManual.Checked)) then
    begin
      Screen.Cursor := crDefault;
      if CompletePacket then
      begin
        ShowMessage('This packet is already present'+CRLF+'and connot be added to the queue.');
        Result:= True;
      end
      else
      begin
        case Application.MessageBox(PAnsiChar('Add to ' + Realname + '?'),
               PAnsiChar('Add to zip'), MB_YESNO + MB_ICONQUESTION) of
          IDYES:
            begin
              Screen.Cursor := crHourGlass;
              LogMsg('Adding v2 version to zip.');
              i:= ItasaZipv2(EditRename.Text,pFS.FileName);
              if i = 0 then
                LogMsg('Zipv2 error.')
              else if i > 0 then
              begin
                pFS.Name := Realname + ' ('+ IntToStr(i)+')';
                AfterUpdate;
                Result:=True;
                LogMsg('v2 resynch added.')
              end
              else Result:=True;
            end;
          IDNO: LogMsg('v2 version not added. Exit.');
        end;
        Screen.Cursor := crDefault;
      end;
      Exit;
    end
    //same name but different sending method
    else if (Realname = Realname2) and pFS.Pannello then
    begin
      Screen.Cursor := crDefault;
      if Application.MessageBox('This version is already present in queue' +
           #13#10 + 'with a different sending method.' + #13#10 +
           'Change sending method?', 'Different sending method', MB_YESNO +
           MB_ICONQUESTION) = IDYES
      then
      begin
        Screen.Cursor := crHourGlass;
        pFS.Pannello:= not pFS.Pannello;
        AfterUpdate;
        Result:=True;
      end;
    end;
    Node := vdtQueue.GetNext(Node);
  end;

  Screen.Cursor := crDefault;
end;

procedure TSendToItasaForm.AddResynchTopic;
var
  Node : PVirtualNode;
  pFS : PFileSendTreeData;
  i : Integer;
begin
  Screen.Cursor := crHourGlass;
  LogMsg('Adding resynch '+NomeFile.NamePannello + ' ' + NomeFile.Version+' to topic.');
  rbTopic.Checked:=True;
  try
    if AddToExisting then Exit;

    if not CompletePacket then Temp.SEND_FILE := '';

    if not CompletePacket then
      ItasaZip(EditRename.Text)
    else
    begin
      i := 0;
      //check if exist a file with the same name
      if FileExists(ExtractFilePath(Temp.SEND_FILE) + EditRename.Text+'.zip') then
        while not RenameFile(ExtractFilePath(Temp.SEND_FILE) + EditRename.Text+'.zip',
                             ExtractFilePath(Temp.SEND_FILE) + EditRename.Text+'.bak' + IntToStr(i) + '.zip') do
          i := i + 1;
      //rename the temp packet with the new name
      RenameFile( Temp.SEND_FILE, ExtractFilePath(Temp.SEND_FILE) + EditRename.Text+'.zip' );
      //add the file path to queue
      Temp.SEND_FILE := ExtractFilePath(Temp.SEND_FILE) + EditRename.Text+'.zip';
    end;

    Node := vdtQueue.AddChild(nil);
    pFS := vdtQueue.GetNodeData(Node);

    pFS.FileName := Temp.SEND_FILE;
    pFS.Name     := Trim(NomeFile.NamePannello + ' ' + NomeFile.Version);
    if CompletePacket then
    begin
      i:=ItasaZipCountEntries(pFS.FileName);
      if i > 1 then pFS.Name := pFS.Name + ' ('+ IntToStr(i)+')';
    end;
    pFS.MsgSub1  := StringReplace(NomeFile.ZipFileName, '.sub.itasa', '', [rfReplaceAll]);
    pFS.MsgSub1  := StringReplace(pFS.MsgSub1, '.italiansubs', '', [rfReplaceAll]);
    pFS.MsgSub2  := Trim(NomeFile.NamePannello + ' ' + NomeFile.Version);
    pFS.MsgVideo := ChangeFileExt(ExtractFileName(MainForm.CurrentProject.VideoSource), '');
    pFS.Pannello := False;
    pFS.Sending  := False;
    pFS.Sended   := False;
  except
    on E : Exception do
      ShowMessage(E.Message);
  end;
  AfterUpdate;
  Screen.Cursor := crDefault;
end;

procedure TSendToItasaForm.AddResynchPannello(TopicId: string);
var
  Node : PVirtualNode;
  pFS : PFileSendTreeData;
  chk : Boolean;
begin
  Screen.Cursor := crHourGlass;
  //if the version is not NULL or v2
  if NomeFile.Version = '' then
  begin
    Screen.Cursor := crDefault;
    if Application.MessageBox('This version is not allowed in Pannello.' +
      #13#10 + 'Upload to Topic?', '', MB_YESNO + MB_ICONWARNING) = IDYES then
      begin
        AddResynchTopic;
      end;
    Exit;
  end;
  
  if AddToExisting then Exit;

  Temp.SEND_FILE:='';
  Node := vdtQueue.AddChild(nil);
  pFS := vdtQueue.GetNodeData(Node);
  pFS.Name    := 'Adding resynch...';
  pFS.Pannello:= True;
  vdtQueue.RepaintNode(Node);
  try
    if ItasaLogin then
    if TopicId = '' then
    begin
      if ItasaDownloadInfoPannello then
      if not ItasaDownloadInfoPannelloInvio then
      begin
        Screen.Cursor := crDefault;
        vdtQueue.DeleteNode(Node);
        if Application.MessageBox('Resynch not found in Pannello.' + #13#10
          + 'Upload to Topic?', 'Pannello error', MB_YESNO + MB_ICONWARNING) = IDYES then
          AddResynchTopic;
      end;
    end;
    if ItasaSelectCartella(InfoPannelloInvia,TopicID) = '' then
    begin
      Screen.Cursor := crDefault;
      vdtQueue.DeleteNode(Node);
      if Application.MessageBox('Folder not found for this version.' + #13#10
        + 'Upload to Topic?', 'Folder error', MB_YESNO + MB_ICONWARNING) = IDYES then
        AddResynchTopic;
    end
    else if ItasaZip(EditRename.Text) then
    begin
      pFS.FileName  := Temp.SEND_FILE;
      pFS.Name      := NomeFile.NamePannello + ' ' + NomeFile.Version;
      pFS.Post      := InfoPannelloInvia.Post;
      pFS.Serie     := InfoPannelloInvia.Serie;
      pFS.Topic     := InfoPannelloInvia.Topic;
      pFS.Episodio  := InfoPannelloInvia.Episodio;
      pFS.Traduttori:= InfoPannelloInvia.Traduttori;
      pFS.Revisore  := InfoPannelloInvia.Revisore;
      pFS.Resynch   := InfoPannelloInvia.Resynch;
      pFS.Cartella  := ItasaSelectCartella(InfoPannelloInvia,TopicId);
      pFS.Pannello  := True;
      pFS.Sending   := False;
      pFS.Sended    := False;
    end;
  except
    on E : Exception do
    begin
      vdtQueue.DeleteNode(Node);
      if E.Message <> '' then
        ShowMessage(E.Message);
    end;
  end;

  if Assigned(InfoPannello) then
    InfoPannello.Free;
  if Assigned(InfoPannelloInvia) then
    InfoPannelloInvia.Free;
  
  AfterUpdate;
  Screen.Cursor := crDefault;
end;


procedure TSendToItasaForm.ButtonAddResynchClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  if not Temp.USE_TEMP_DIR then
    Temp.TEMP_DIR := OpenedFile.DIR_SUB;

  if NomeFile.FixSeriesName then
  begin
    if rbPannello.Checked then
      AddResynchPannello('')
    else if rbTopic.Checked then
      AddResynchTopic
    else if rbManual.Checked and (EditTopicID.Text <> '') then
      AddResynchPannello(EditTopicID.Text);
  end;
  Screen.Cursor := crDefault;
end;

procedure TsendToItasaForm.AddTag(var Msg: string; tag:string; ids, len:Integer);
var
  tmsg:string;
begin
  tmsg:= Copy( Msg, 1, ids) + '['+tag+']'
       + Copy( Msg, ids + 1, len) + '[/'+tag+']'
       + Copy( Msg, ids + len + 1, Length(Msg));
  Msg:=tmsg;
end;

procedure TSendToItasaForm.ButtonBoldClick(Sender: TObject);
var tmsg : string;
begin
  tmsg:=MemoPreviewMsg.Text;
  AddTag(tmsg,'b',MemoPreviewMsg.SelStart,MemoPreviewMsg.SelLength);
  MemoPreviewMsg.Text:=tmsg;
end;

procedure TSendToItasaForm.ButtonItalicClick(Sender: TObject);
var tmsg : string;
begin
  tmsg:=MemoPreviewMsg.Text;
  AddTag(tmsg,'i',MemoPreviewMsg.SelStart,MemoPreviewMsg.SelLength);
  MemoPreviewMsg.Text:=tmsg;
end;

procedure TSendToItasaForm.ButtonUnderlineClick(Sender: TObject);
var tmsg : string;
begin
  tmsg:=MemoPreviewMsg.Text;
  AddTag(tmsg,'u',MemoPreviewMsg.SelStart,MemoPreviewMsg.SelLength);
  MemoPreviewMsg.Text:=tmsg;
end;


//custom node draw for the virtualtree
procedure TSendToItasaForm.vdtQueueDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var x, y : Integer;
    OldColor : Cardinal;
    OldFont : HFONT;
    DC : HDC;
    Msg : WideString;
    xysize : Size;
    pFileSendData : PFileSendTreeData;
    OldBrush : HBRUSH;
    OldPen : HPEN;
begin

  pFileSendData := vdtQueue.GetNodeData(PaintInfo.Node);

  vdtQueue.Canvas.Lock;
  DC := PaintInfo.Canvas.Handle;

  OldColor := GetTextColor(DC);

  x := NodeLeftMargin;
  y := NodeTopMargin;

  //selected node color
  if (vdtQueue.Focused) and (vdtQueue.Selected[PaintInfo.Node]) then
    SetTextColor(DC, ColorToRGB(clHighlightText))
  else
    SetTextColor(DC, ColorToRGB(clWindowText));

  //alternate background colors
  if (PaintInfo.Node.Index mod 2 = 1)
  And (vdtQueue.FocusedNode <> PaintInfo.Node) then
  begin
    OldPen := SelectObject(DC, NullPen);
    OldBrush := SelectObject(DC, AlterBrush);
    Rectangle(DC, -1, -1, vdtQueue.Width , vdtQueue.DefaultNodeHeight);
    SelectObject(DC, OldPen);
    SelectObject(DC, OldBrush);
  end;

  SetBKMode(DC, TRANSPARENT);

  //first node item (send mode: pannello or topic)
  OldFont := SelectObject(DC, BoldFont);
  if pFileSendData.Pannello
  then Msg := 'Pannello'
  else Msg := 'Topic';
  GetTextExtentPoint32W(DC, PWideChar(Msg), Length(Msg), xysize);
  TextOutW(DC, x , y, PWideChar(Msg), Length(Msg));

  //second node item (resynch name)
  SelectObject(DC, NormalFont);
  Msg := pFileSendData.Name;
  TextOutW(DC, x + 46, y, PWideChar(Msg), Length(Msg));

  //third node item (resynch status: ready, sending... or sended)
  Msg:='Ready';
  if pFileSendData.Sending then Msg:='Sending...';
  if pFileSendData.Sended  then Msg:='Sent';
  TextOutW(DC, vdtQueue.Width - 60, y, PWideChar(Msg), Length(Msg));

  //Restore old things to prevent problems
  SelectObject(DC, OldFont);
  SetTextColor(DC, OldColor);
  DeleteObject(OldColor);
  vdtQueue.Canvas.Unlock;
end;

//custom sorting method for the virtualtree (pannello > topic)
procedure TSendToItasaForm.vdtQueueCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pFS1, pFS2 : PFileSendTreeData;
begin
  pFS1 := Sender.GetNodeData(Node1);
  pFS2 := Sender.GetNodeData(Node2);

  if pFS1.Pannello = pFS2.Pannello then
  begin
    if pFS1.Name > pFS2.Name
    then Result := 1
    else Result := -1;
  end
  else begin
    if pFS1.Pannello
    then Result := -1
    else Result := 1;
  end;
end;

//remove the selected node from virtualtree
procedure TSendToItasaForm.Removeselected1Click(Sender: TObject);
var Node : PVirtualNode; 
  pFS : PFileSendTreeData;
begin
  Node := vdtQueue.GetFirstSelected;
  if Assigned(Node) then
  begin
    pFS := vdtQueue.GetNodeData(Node);
    if FileExists(pFS.FileName) then DeleteFile(pFS.FileName);
    vdtQueue.DeleteSelectedNodes;
  end;
  UpdateComponent;
end;
  
procedure TSendToItasaForm.FormDestroy(Sender: TObject);
begin
  NomeFile.Free;
  Temp.COOKIE.Free;
  Temp.TEMP.Free;
  UserInfo.Messaggio.Free;
  UserInfo.MessaggioP.Free;  
  HttpInit(False);
  Reg.Free;

  if BoldFont <> 0 then
    DeleteObject(BoldFont);
  if NormalFont <> 0 then
    DeleteObject(NormalFont);
end;

procedure TSendToItasaForm.ButtonPacketClick(Sender: TObject);
var
  i:Integer;
  FileList : TStringList;
  TempName:string;
  TempPath:string;
  TempSeries, TempVersion : string;
  Check:Boolean;
begin
  OpenDialog1.Title := 'Select subtitles for the complete pack';
  OpenDialog1.InitialDir := OpenedFile.DIR_SUB;
  OpenDialog1.Options := OpenDialog1.Options
    + [ofFileMustExist, ofAllowMultiSelect, ofNoNetworkButton];
  OpenDialog1.Filter := 'Subtitles|*.srt';
  OpenDialog1.FilterIndex := 1;
  if OpenDialog1.Execute then
  begin
    if OpenDialog1.Files.Count > 0 then
    begin
      Temp.SEND_FILE:='';
      FileList := TStringList.Create;
      FileList.Text := OpenDialog1.Files.Text;

      TempName:='Series.Name.Season.01.Completa.italiansubs';
      Check:=False;
      for i:=0 to FileList.Count - 1 do
      begin
        NomeFile.NewFile(StringReplace(
          ExtractFileName(FileList.Strings[i]),
          ExtractFileExt(FileList.Strings[i]),
          '',
          [rfReplaceAll]));

        if NomeFile.Valid then
        begin
          //Check if selected files has same series name
          if TempSeries = '' then
            TempSeries := NomeFile.SeriesName
          else if TempSeries <> NomeFile.SeriesName then
          begin
            CompletePacket := False;
            rbTopic.Checked:= False;
            ShowMessage('Different serie names found in subtitles.' + CRLF
              + ExtractFileName(FileList.Strings[i]) + ' differs from' + CRLF
              + ExtractFileName(FileList.Strings[i-1]));
            Exit;
          end;
          //Check if selected file has the same version
          if TempVersion = '' then
            TempVersion := NomeFile.Version
          else if TempVersion <> NomeFile.Version then
          begin
            CompletePacket := False;
            rbTopic.Checked:= False;
            ShowMessage('Different version found in subtitles.' + CRLF
              + ExtractFileName(FileList.Strings[i]) + ' differs from' + CRLF
              + ExtractFileName(FileList.Strings[i-1]));
            Exit;
          end;

          if Check then Continue;
          TempName := Format('%s.Season.%.2d.Completa.',[NomeFile.OriginalSeriesName,NomeFile.Season]);
          if NomeFile.Version <> '' then
            TempName := TempName + NomeFile.Version + '.';
          TempName := TempName + 'italiansubs';
          CompletePacket := True;
          rbTopic.Checked:= True;
          Check:=True;
        end
        else
        begin
          CompletePacket := False;
          rbTopic.Checked:= False;
          ShowMessage('Error in file name.' + CRLF
            + ExtractFileName(FileList.Strings[i]) + ' is not valid.');
          Exit;
        end;
      end;


      TempPath := Temp.TEMP_DIR;
      if MainForm.ConfigObject.ZipPath <> '' then
        if MainForm.ConfigObject.ZipPathOnlyForCP and
          DirectoryExists(MainForm.ConfigObject.ZipPath) then
            Temp.TEMP_DIR := MainForm.ConfigObject.ZipPath;

      if not ItasaZipPacket(FileList) then
        ShowMessage('packet zip error');

      //restore old path
      Temp.TEMP_DIR:=TempPath;

      EditRename.Text := TempName;
      FileList.Free;
    end;
  end;
end;

procedure TSendToItasaForm.rbPannelloClick(Sender: TObject);
begin
  if CompletePacket then
  begin
    rbPannello.Checked := False;
    rbTopic.Checked := True;
  end;
  EditTopicID.Visible := False;
  Shape2.Visible := False;
  EditRename.OnChange(Sender);
end;

procedure TSendToItasaForm.rbManualClick(Sender: TObject);
begin
  if CompletePacket then
  begin
    rbManual.Checked := False;
    rbTopic.Checked := True;
  end
  else
  begin
    Shape2.Visible := True;
    EditTopicID.Visible := True;
    SendToItasaForm.EditTopicIDChange(Sender);
  end;
end;

procedure TSendToItasaForm.rbTopicClick(Sender: TObject);
begin
  EditTopicID.Visible := False;
  Shape2.Visible := False;
  EditRename.OnChange(Sender);
end;

end.
