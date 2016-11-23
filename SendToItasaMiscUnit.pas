unit SendToItasaMiscUnit;

interface

uses
  Classes, SysUtils, Math, HTTPSend, RegExpr, KAZip, synachar;

type
  TOpenedFile = record
    DIR_SUB, FILE_SUB, EXT_SUB, NOME_SUB: String;
  end;

  TNomeFile = class
  private
    Reg,RegC,RegN: TRegExpr;
    FileName: string;
    NameBC : TStringList;
    tValidE, tValidC: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NewFile(NewFileName: String);
    function Valid: Boolean;
    function ValidName: Boolean;
    function FixSeriesName: Boolean;
    function Multiepisode: Boolean;
    function OriginalSeriesName: string;
    function SeriesName: string;
    function Season: Integer;
    function Episode: Integer;
    function Episode2: Integer;
    function Version: string;
    function Version2: string;
    function Version3: string;
    function ItasaTag: string;
    function SetVersion(NewVersion: string): string;
    function SetVersion2(NewVersion: string): string;
    function SetVersion3(NewVersion: string): string;
    function SetSeriesName(NewName: string): string;
    function NamePannello: string;
    function NameFolder: string;
    function ZipFileName: string;
    function GetFileName: string;
  end;

  //Estrazione informazioni dal topic
  TInfoTopic = class
  private
    ttopic, tsubject, tlast_msg, tsessionid, tsessionvar: string;
    tvalido: Boolean;
  public
    constructor Create;
    function Topic: string;
    function Subject: string;
    function LastMsg: string;
    function SessionId: string;
    function SessionVar: string;
    function Valid: Boolean;
  end;

  TTopicPost = class
  private
    tvalido: Boolean;
  public
    constructor Create;
    function Valido: Boolean;
  end;

  //estrazione informazioni dal pannello
  TPannello = class
  private
    tSerie, tStatus, tUrl: TStringList;
    tvalido: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function Serie(index: Integer): string;
    function Status(index: Integer): string;
    function Url(index: Integer): string;
    function Valido: Boolean;
  end;

  //estrazione informazioni dal modulo di invio
  //resynch tramite pannello
  TPannelloInvia = class
  private
    tpost,
      tnome_serie, tid_topic, tepisodio,
      ttraduttori, trevisore, tresynch: string;
    cartella_value, cartella_voce: TStringList;
    tvalido: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function CartelleCount: Integer;
    function CartellaValue(index: Integer): string;
    function CartellaVoce(index: Integer): string;
    function Valido: Boolean;
    function Post: string;
    function Serie: string;
    function Topic: string;
    function Episodio: string;
    function Traduttori: string;
    function Revisore: string;
    function Resynch: string;
  end;

  //estrazione informazioni dalla pagina risultante
  //dell'invio tramite pannello per verificare
  //l'avvenuto invio
  TPannelloPost = class
  private
    tvalido: Boolean;
  public
    constructor Create;
    function Valido: Boolean;
  end;

  TUserInfo = record
    Username  : string;
    Passwd    : string;
    Messaggio : TStringList;
    MessaggioP: TStringList;
    Log       : Boolean;
  end;

  TTemp = record
    TEMP, COOKIE: TStringList;
    SEND_FILE : string;
    TEMP_DIR : string;
    USE_TEMP_DIR:Boolean;
  end;

  TItasaUrls = record
    URL_ITASA, URL_LOGIN, URL_TOPIC_ID, URL_TOPIC, URL_POST_ID, URL_POST, URL_PANNELLO, URL_MANUALE: string;
  end;

  TFileSendTreeData = record
    FileName : string;
    Name     : string;
    Pannello : Boolean;
    Sending  : Boolean;
    Sended   : Boolean;
    //Topic var
    MsgSub1   : string;
    MsgSub2   : string;
    MsgVideo  : string;
    //Pannello var
    Post      : string;
    Serie     : string;
    Topic     : string;
    Episodio  : string;
    Traduttori: string;
    Revisore  : string;
    Resynch   : string;
    Cartella  : string;
  end;
  PFileSendTreeData = ^TFileSendTreeData;

procedure HttpInit(Create: Boolean);
function LogMsg(Msg: string):Boolean;
function PostBuilder(Bound, FieldName, Data: string): string;
function LevenshteinDistance(const s, t: string): integer;
function CleanName(ToClean: string): string;
function CheckExclusion(ExWord: string;i,y:Integer): string;
function ItasaZip(Rename: string): Boolean;
function ItasaZipv2(Rename, ZipFile: string): Integer;
function ItasaZipCountEntries(ZipFile: string): Integer;
function ItasaZipPacket(const FileList : TStringList): Boolean;
procedure CreateMessage;
function ItasaLogin: Boolean;
function ItasaDownloadPagina(Url: string): Boolean;
function ItasaDownloadInfoTopic: Boolean;
function ItasaDownloadInfoPannello: Boolean;
function ItasaDownloadInfoPannelloInvio: Boolean;
function ItasaSelectCartella(SendResynch:TPannelloInvia; TopicID: string): string;
procedure ItasaPostPannello;
procedure ItasaPostTopic;

const
  CRLF = #$0D + #$0A;

const
  //exp to validate filenames
  //old ExpEpisode = '^(\S+?)\.s(\d{2})e(\d{2})(\-(\d{2})|.{0})\.((v[1-9]|P|R|PR|RR|RP|I)\.|(.{0}))((720p|WEB-DL|1080[ip]|DVDRip|BDRip|BRRip|Bluray)\.|(.{0}))((extended|preair|commenti|tag|karaoke)\.|(.{0}))sub\.itasa$';
  ExpEpisode = '^(\S+?)\.s(\d{2})e(\d{2})(\-(\d{2})|.{0})\.(v[1-9]\.|P\.|R\.|PR\.|RR\.|RP\.|I\.|(.{0}))(720p\.|WEB-DL\.|1080[ip]\.|DVDRip\.|BDRip\.|BRRip\.|Bluray\.|(.{0}))(extended\.|preair\.|commenti\.|tag\.|karaoke\.|(.{0}))(sub\.itasa|italiansubs)$';

  //(Nome.Serie).Season.(XX).Completa.[(Versione).]sub.itasa
  ExpSerie = '^(\S+?)\.Season\.(\d{2})\.Completa\.((720p|WEB-DL|1080[ip]|DVDRip|BDRip|BRRip|Bluray)\.|(.{0}))(sub\.itasa|italiansubs)$';

const //TInfoTopic
  ExpTopic      = '<meta property="og:url" content=".*?=(.*?)\..*?"/>';
  ExpSessionVar = 'sSessionVar: \'+#39+'(.*?)\'+#39;
  ExpSessionId  = 'sSessionId: \'+#39+'(.*?)\'+#39;
  ExpSubject    = '<a href=".*?"><span>(.*?)</span></a>';
  ExpLastMsg    = 'href=".*?\?action=post;last_msg=(.*?)\"><span>';
  ExpTopicPost  = '<div class="errorbox" id="errors">';

const //TPannello
  //exp to extract pannello's resynch
  ExpPannello = '<tr>.*?/new/">(.*?)</a>.*?">(In revisione|In traduzione|Rilasciata).*?(center"><img alt="Upload|center"><a href="(.*?)"><img alt="Upload).*?</tr>';
  //exp to extract values from send form
  ExpPannelloInviaFolder = '<option value="(\d{1,6})">(.*?)</option>';
  ExpPannelloInviatpost =  'multipart\/form-data\" action=\"(.*?)\" method\=\"post\"\>';
  ExpPannelloInviatnome_serie = 'name\=\"nome\_serie\" value=\"(.*?)\"';
  ExpPannelloInviatid_topic = 'name=\"id\_topic\" value=\"(\d{1,6})\"\>';
  ExpPannelloInviatepisodio = 'name=\"episodio\" value=\"(.*?)\"';
  ExpPannelloInviattraduttori = 'name=\"traduttori\" value=\"(.*?)\"';
  ExpPannelloInviatrevisore = 'name=\"revisore\" value=\"(.*?)\"';
  ExpPannelloInviatresynch = 'name=\"resynch\" size=\"70\" value=\"(.*?)\"';
  //exp to validate the upload
  ExpPannelloPost = 'Invio resynch<span style="color:.*?;"> (.*?)</span>';

var
  HTTP: THTTPSend;

implementation

uses Windows, Forms, Dialogs, Main, SendToItasaFormUnit, LogWindowFormUnit, VirtualTrees;

procedure HttpInit(Create:Boolean);
begin
  if (Create and not Assigned(HTTP)) then
    HTTP := THTTPSend.Create
  else if (not Create and Assigned(HTTP)) then
    HTTP.Free;
end;

function LogMsg(Msg: string): Boolean;
begin
  If UserInfo.Log then
    LogForm.LogMsg('SF- ' + Msg);
  Result:=True;
end;

function CleanName(ToClean: string): string;
begin
  Result:=StringReplace(ToClean, '&nbsp;', ' ',    [rfReplaceAll]);
  Result:=StringReplace(Result,  '&amp;',  'and',  [rfReplaceAll]);
  Result:=StringReplace(Result,  ' & ',    ' and ',[rfReplaceAll]);
  Result:=StringReplace(Result,  ':',      '',     [rfReplaceAll]);
  Result:=StringReplace(Result,  #39,      '',     [rfReplaceAll]);
  Result:=StringReplace(Result,  #44,      '',     [rfReplaceAll]);
  Result:=StringReplace(Result,  #46+#32,  ' ',     [rfReplaceAll]);
  //specific series name
  Result:=StringReplace(Result,'The Office US','The Office',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'CSI Crime Scene Investigation','CSI',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'The Michael J. Fox Show','The Michael J Fox Show',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Star-Crossed','Star Crossed',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Inside No. 9','Inside No 9',[rfReplaceAll,rfIgnoreCase]);
end;

function CheckExclusion(ExWord: string;i,y:Integer): string;
begin
  Result:= '';
  //check various exclusion
  if ((AnsiCompareText(ExWord,'US')=0)        and (i = y-1)) then Result:='US';
  if ((AnsiCompareText(ExWord,'UK')=0)        and (i = y-1)) then Result:='UK';
  if ((AnsiCompareText(ExWord,'SVU')=0)       and (i = y-1)) then Result:='SVU';
  if ((AnsiCompareText(ExWord,'NCIS')=0)      and (i = 0))   then Result:='NCIS';
  if ((AnsiCompareText(ExWord,'CSI')=0)       and (i = 0))   then Result:='CSI';
  if ((AnsiCompareText(ExWord,'VEEP')=0)      and (i = 0))   then Result:='VEEP';
  if ((AnsiCompareText(ExWord,'NTSF')=0)      and (i = 0))   then Result:='NTSF';
  if ((AnsiCompareText(ExWord,'SD')=0)        and (i = 1))   then Result:='SD';
  if ((AnsiCompareText(ExWord,'SUV')=0)       and (i = 2))   then Result:='SUV';
  if ((AnsiCompareText(ExWord,'SHIELD')=0)    and (i = y-1)) then Result:='SHIELD';
  if ((AnsiCompareText(ExWord,'iZombie')=0)   and (i = 0))   then Result:='iZombie';
end;

function LevenshteinDistance(const s, t: string): integer;
var
  d : array of array of integer;
  n, m, i, j : integer;
begin
  n := length(s);
  m := length(t);

  if n=0 then
  begin
    Result := m;
    Exit;
  end;

  if m=0 then
  begin
    Result := n;
    Exit;
  end;

  SetLength(d, n + 1, m + 1);
  for i := 0 to n do d[i, 0] := i;
  for j := 0 to m do d[0, j] := j;

  for i := 1 to n do
    for j := 1 to m do
      d[i, j] := Min(Min(d[i-1, j]+1, d[i,j-1]+1), d[i-1,j-1]+Integer(s[i] <> t[j]));

  Result := d[n, m];
end;

//----------------------------------------------//
//    TNomeFile Class                           //
//----------------------------------------------//

constructor TNomeFile.Create;
begin
  Reg := TregExpr.Create;
  RegC:= TregExpr.Create;
  RegN:= TRegExpr.Create;
  NameBC := TStringList.Create;
  NameBC.Delimiter := '.';
  Reg.Expression := ExpEpisode;
  RegC.Expression:= ExpSerie;
  RegN.Expression := '\x2E';
end;

destructor TNomeFile.Destroy;
begin
  Reg.Free;
  RegC.Free;
  RegN.Free;
  NameBC.Free;
end;

procedure TNomeFile.NewFile(NewFileName: string);
begin
  FileName := NewFileName;
end;

function TNomeFile.Valid: Boolean;
begin
  tValidE := Reg.Exec(FileName);
  tValidC := RegC.Exec(FileName);
  if tValidE and Multiepisode then
    if Episode >= Episode2 then
      tValidE := False;
  if tValidE or tValidC then
    Result := ValidName
  else
    Result := False;
end;

function TNomeFile.ValidName: Boolean;
var
  y :Integer;
begin
  Result:=True;
  NameBC.Clear;
  if tValidE then
    RegN.Split(Reg.Match[1],NameBC);
  if tValidC then
    RegN.Split(RegC.Match[1],NameBC);
  if NameBC.Count > 0 then
  begin
    for y:=0 to NameBC.Count-1 do
      if NameBC.Strings[y] = '' then
      begin
        Result:=False;
        Break;
      end;
  end;
end;

//Function to check the validity of the series name
//and capitalize all the word in it
function TNomeFile.FixSeriesName: Boolean;
var
  temp,temp1:string;
  y :Integer;
begin
  Result:=True;
  NameBC.Clear;
  if tValidE then
    RegN.Split(Reg.Match[1],NameBC);
  if tValidC then
    RegN.Split(RegC.Match[1],NameBC);

  //RegN.Expression := '^[A-Z\d]{1}[a-z\d]*$';
  temp:=NameBC.DelimitedText;
  for y:=0 to NameBC.Count-1 do
  begin
    temp1 := CheckExclusion(NameBC.Strings[y],y,NameBC.Count);
    if temp1 <> '' then NameBC.Strings[y] := temp1
    else
    begin
      NameBC.Strings[y] := LowerCase(NameBC.Strings[y]);
      temp1 := NameBC.Strings[y];
      temp1[1] := UpCase(temp1[1]);
      NameBC.Strings[y] := temp1;
    end
  end;
  //RegN.Expression := '\x2E';
  if temp <> NameBC.DelimitedText then
  begin
    temp:= 'Name need attention. Fix' + #13#10 +
           'from: ' + FileName + #13#10 +
           'to: ' + SetSeriesName(NameBC.DelimitedText);
    case Application.MessageBox(PAnsiChar(temp),
           'Fixed Name Question',
           MB_ICONQUESTION or MB_YESNOCANCEL or MB_DEFBUTTON1) of
      IDYES:
      begin
        NewFile(SetSeriesName(NameBC.DelimitedText));
        if SendToItasaForm.Showing then
          SendToItasaForm.SetText(SetSeriesName(NameBC.DelimitedText));
      end;
      IDCANCEL: Result:=False;
    end;
  end;
end;

function TNomeFile.Multiepisode: Boolean;
begin
  Result := False;
  if tValidE then
    if Reg.Match[4] <> Reg.Match[5] then
      Result := True;
end;

function TNomeFile.SeriesName: string;
begin
  Result := '';
  if Valid and tValidE then
    Result := StringReplace(CleanName(Reg.Match[1]),  '.', ' ', [rfReplaceAll]);
  if Valid and tValidC then
    Result := StringReplace(CleanName(RegC.Match[1]), '.', ' ', [rfReplaceAll]);
end;

function TNomeFile.OriginalSeriesName: string;
begin
  Result := '';
  if Valid and tValidE then
    Result := Reg.Match[1];
  if Valid and tValidC then
    Result := RegC.Match[1];
end;

function TNomeFile.Season: Integer;
begin
  Result := 0;
  if tValidE then
    Result := StrToInt(Reg.Match[2]);
  if tValidC then
    Result := StrToInt(RegC.Match[2]);
end;

function TNomeFile.Episode: Integer;
begin
  Result := 0;
  if tValidE then
    Result := StrToInt(Reg.Match[3]);
end;

function TNomeFile.Episode2: Integer;
begin
  Result := 0;
  if tValidE then
    if Multiepisode
      then Result := StrToInt(Reg.Match[5])
    else Result := StrToInt(Reg.Match[3]);
end;

function TNomeFile.Version: string;
begin
  Result := '';
  if tValidE then
    Result := Copy(Reg.Match[8], 1, length(Reg.Match[8])-1)
  else if tValidC then
    Result := RegC.Match[4];
end;

function TNomeFile.Version2: string;
begin
  Result := '';
  if tValidE then
    Result := Copy(Reg.Match[6], 1, length(Reg.Match[6])-1);
end;

function TNomeFile.Version3: string;
begin
  Result := '';
  if tValidE then
    Result := Copy(Reg.Match[10], 1, length(Reg.Match[10])-1);
end;

function TNomeFile.ItasaTag: string;
begin
  Result := '';
  if tValidE then
    Result := Reg.Match[12];
  if tValidC then
    Result := RegC.Match[6];
end;

function TNomeFile.SetVersion(NewVersion: string): string;
begin
  Result := Format('%s.s%.2de%.2d', [OriginalSeriesName, Season, Episode]);
  if Multiepisode then
    Result := Result + Format('-%.2d', [Episode2]);
  if Version2 <> '' then  Result := Result + '.' + Version2;
  if NewVersion <> '' then  Result := Result + '.' + NewVersion;
  if Version3 <> '' then  Result := Result + '.' + Version3;
  Result := Result + '.' + ItasaTag;
end;

function TNomeFile.SetVersion2(NewVersion: string): string;
begin
  Result := Format('%s.s%.2de%.2d', [OriginalSeriesName, Season, Episode]);
  if Multiepisode then
    Result := Result + Format('-%.2d', [Episode2]);
  if NewVersion <> '' then  Result := Result + '.' + NewVersion;
  if Version <> '' then  Result := Result + '.' + Version;
  if Version3 <> '' then  Result := Result + '.' + Version3;
  Result := Result + '.' + ItasaTag;
end;

function TNomeFile.SetVersion3(NewVersion: string): string;
begin
  Result := Format('%s.s%.2de%.2d', [OriginalSeriesName, Season, Episode]);
  if Multiepisode then
    Result := Result + Format('-%.2d', [Episode2]);
  if Version2 <> '' then  Result := Result + '.' + Version2;
  if Version <> '' then  Result := Result + '.' + Version;
  if NewVersion <> '' then  Result := Result + '.' + NewVersion;
  Result := Result + '.' + ItasaTag;
end;

function TNomeFile.SetSeriesName(NewName: string): string;
begin
  if tValidE then Result := Format('%s.s%.2de%.2d', [NewName, Season, Episode]);
  if tValidC then Result := Format('%s.Stagione.%.2d.Completa', [NewName, Season, Episode]);
  if Multiepisode then Result := Result + Format('-%.2d', [Episode2]);
  if Version2 <> '' then  Result := Result + '.' + Version2;
  if Version  <> '' then  Result := Result + '.' + Version;
  if Version3 <> '' then  Result := Result + '.' + Version3;
  Result := Result + '.' + ItasaTag;
end;

function TNomeFile.NamePannello: string;
begin
  Result := '';
  if Valid then
    if tValidE then
    begin
      Result := Format('%s %dx%.2d', [SeriesName, Season, Episode]);
      if Multiepisode then
        Result := Result + Format('-%.2d', [Episode2]);
    end
    else if tValidC then
    begin
      Result := Format('%s S%.2d', [SeriesName, Season]);
    end;
end;

function TNomeFile.NameFolder: string;
begin
  Result := '';
  if Valid then
    Result := Format('%s/Stagione %d/%s', [SeriesName, Season, Version]);
end;

function TNomeFile.ZipFileName: string;
begin
  Result := '';
  if Valid and tValidE then
  begin
    Result := Format('%s.s%.2de%.2d', [OriginalSeriesName, Season, Episode]);
    if Multiepisode then
      Result := Result + Format('-%.2d',[Episode2]);
    if Version <> '' then
      Result := Result + Format('.%s',[Version]);
    Result := Result + '.' + ItasaTag;
  end;
  if Valid and tValidC then
  begin
    Result := Format('%s.Season.%.2d.Completa', [OriginalSeriesName, Season]);
    if Version <> '' then
      Result := Result + Format('.%s',[Version]);
    Result := Result + '.' + ItasaTag;
  end;
end;

function TNomeFile.GetFileName: string;
begin
  Result := FileName;
end;

//----------------------------------------------//
//    TInfoTopic Class                          //
//----------------------------------------------//

constructor TInfoTopic.Create;
var
  buffer: string;
  Reg: TRegExpr;
  i: Integer;
  redo: Boolean;
  tempurl: TStringList;
  {$ifdef DEBUG}
  tempdata:TStringList;
  {$endif}
begin
  tvalido := False;
  redo := False;
  Reg := TRegExpr.Create;
  {$ifdef DEBUG}
    tempdata := TStringList.Create;
    Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'top_temp.html');
  {$endif}
  repeat
    for i := 0 to Temp.TEMP.Count - 1 do
    begin
      buffer := Temp.TEMP.Strings[i];
      if ttopic = '' then
      begin
        Reg.Expression := ExpTopic;
        if Reg.Exec(buffer) then
          ttopic := Reg.Match[1];
      end   
      else if tsessionvar = '' then
      begin
        {$ifdef DEBUG}
        if not (Pos('topic: ',tempdata.Text) > 0) then
          tempdata.Add(IntToStr(i-1)+' topic: '+ttopic);{$endif}
        Reg.Expression := ExpSessionVar;
        if Reg.Exec(buffer) then
          tsessionvar := Reg.Match[1];
      end
      else if tsessionid = '' then
      begin
        {$ifdef DEBUG}
        if not (Pos('svar: ',tempdata.Text) > 0) then
          tempdata.Add(IntToStr(i-1)+' svar: '+tsessionvar);{$endif}
        Reg.Expression := ExpSessionId;
        if Reg.Exec(buffer) then
          tsessionid := Reg.Match[1];
      end
      else if tsubject = '' then
      begin
        {$ifdef DEBUG}
        if not (Pos('sid: ',tempdata.Text) > 0) then
          tempdata.Add(IntToStr(i-1)+' sid: '+tsessionid);{$endif}
        if Pos('<li class="last">',buffer) > 0 then
        begin
          Reg.Expression := ExpSubject;
          if Reg.Exec(Temp.TEMP.Strings[i+1]) then
            tsubject := Reg.Match[1];
        end;
      end
      else if tlast_msg = '' then
      begin
        {$ifdef DEBUG}
        if not (Pos('subject: ',tempdata.Text) > 0) then
          tempdata.Add(IntToStr(i-1)+' subject: '+tsubject);{$endif}
        Reg.Expression := ExpLastMsg;
        if Reg.Exec(buffer) then
          tlast_msg := Reg.Match[1];
      end
      else
      begin
        {$ifdef DEBUG}tempdata.Add(IntToStr(i-1)+' last_msg: '+tlast_msg);{$endif}
        tempurl := TStringList.Create;
        tempurl.Clear;
        tempurl.Delimiter := '/';
        Reg.Expression := '\x2F';
        reg.Split(ItasaUrls.URL_TOPIC,tempurl);
        tempurl.Delete(tempurl.Count-1);
        tempurl.Delete(tempurl.Count-1);
        ItasaUrls.URL_POST := tempurl.DelimitedText + ItasaUrls.URL_POST_ID;
        tvalido := True;
        Break;
      end;
    end;
    if (not tvalido) and (not redo) then
      redo := True
    else
      redo := False;
  until not redo;

  {$ifdef DEBUG}
  tempdata.SaveToFile(OpenedFile.DIR_SUB+'top_extracted.txt');
  tempdata.Free;
  {$endif}
  Reg.Free;
end;

function TInfoTopic.Topic: string;
begin
  Result := ttopic;
end;

function TInfoTopic.Subject: string;
begin
  Result := tsubject;
end;

function TInfoTopic.LastMsg: string;
begin
  Result := tlast_msg;
end;

function TInfoTopic.SessionId: string;
begin
  Result := tsessionid;
end;

function TInfoTopic.SessionVar: string;
begin
  Result := tsessionvar;
end;

function TInfoTopic.Valid: Boolean;
begin
  Result := tvalido;
end;

//------------------------------------------------

constructor TTopicPost.Create;
var
  buffer: string;
  Reg: TRegExpr;
  i: Integer;
begin
  tvalido := True;
  Reg := TregExpr.Create;
  Reg.Expression := ExpTopicPost;
  for i := 0 to Temp.TEMP.Count - 1 do
  begin
    buffer := Temp.TEMP.Strings[i];
    if Reg.Exec(buffer) then
      tvalido := False;
  end;
end;

function TTopicPost.Valido: Boolean;
begin
  Result := tvalido;
end;


//----------------------------------------------//
//    TPannello Class                           //
//----------------------------------------------//

constructor TPannello.Create;
var
  buffer: string;
  Reg: TRegExpr;
  i: Integer;
begin
  tSerie := TStringList.Create;
  tStatus := TStringList.Create;
  tUrl := TStringList.Create;
  Reg := TregExpr.Create;
  tvalido := false;
  Reg.Expression := ExpPannello;
  {$ifdef DEBUG}
    Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'pan_temp.html');
  {$endif}
  for i := 0 to Temp.TEMP.Count - 1 do
  begin
    buffer := Temp.TEMP.Strings[i];
    if Reg.Exec(buffer) then
      repeat
        tSerie.Add(CleanName(Reg.Match[1]));
        tStatus.Add(Reg.Match[2]);
        if Reg.SubExprMatchCount > 3
          then tUrl.Add(Reg.Match[4])
        else tUrl.Add('');
        tvalido := True;
      until not Reg.ExecNext;
  end;
  Reg.Free;

  {$ifdef DEBUG}
    tSerie.SaveToFile(OpenedFile.DIR_SUB+'pan_serie.txt');
    tStatus.SaveToFile(OpenedFile.DIR_SUB+'pan_status.txt');
    tUrl.SaveToFile(OpenedFile.DIR_SUB+'pan_urls.txt');
  {$endif}
end;

destructor TPannello.Destroy;
begin
  tSerie.Free;
  tStatus.Free;
  tUrl.Free;
  inherited;
end;

function TPannello.Count: Integer;
begin
  Result := tSerie.Count;
end;

function TPannello.Serie(index: Integer): string;
begin
  Result := '';
  if index < Count then
    Result := tSerie.Strings[index];
end;

function TPannello.Status(index: Integer): string;
begin
  Result := '';
  if index < Count then
    Result := tStatus.Strings[index];
end;

function TPannello.Url(index: Integer): string;
begin
  Result := '';
  if index < Count then
    Result := tUrl.Strings[index];
end;

function TPannello.Valido: Boolean;
begin
  Result := tvalido;
end;

//----------------------------------------------//
//    TPannolloInvia Class                      //
//----------------------------------------------//

constructor TPannelloInvia.Create;
var
  buffer: string;
  Reg: TRegExpr;
  i: Integer;
  {$ifdef DEBUG}
    tempdata:TStringList;
  {$endif}
begin
  tvalido := False;
  cartella_value := TStringList.Create;
  cartella_voce := TStringList.Create;
  Reg := TregExpr.Create;
  {$ifdef DEBUG}
    tempdata:=TStringList.Create;
    Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'pform_temp.html');
  {$endif}
  for i := 0 to Temp.TEMP.Count - 1 do
  begin
    buffer := Temp.TEMP.Strings[i];
    if tpost = '' then
    begin
      Reg.Expression := ExpPannelloInviatpost;
      if Reg.Exec(buffer) then tpost := Reg.Match[1];
    end;
    if tnome_serie = '' then
    begin
      Reg.Expression := ExpPannelloInviatnome_serie;
      if Reg.Exec(buffer) then tnome_serie := Reg.Match[1];
      Reg.Expression := ExpPannelloInviatid_topic;
      if Reg.Exec(buffer) then tid_topic := Reg.Match[1];
      Reg.Expression := ExpPannelloInviatepisodio;
      if Reg.Exec(buffer) then tepisodio := Reg.Match[1];
      Reg.Expression := ExpPannelloInviattraduttori;
      if Reg.Exec(buffer) then ttraduttori := Reg.Match[1];
      Reg.Expression := ExpPannelloInviatrevisore;
      if Reg.Exec(buffer) then trevisore := Reg.Match[1];
      Reg.Expression := ExpPannelloInviatresynch;
      if Reg.Exec(buffer) then tresynch := Reg.Match[1];
      {$ifdef DEBUG}
        if tnome_serie <> '' then
        begin
          tempdata.Add('post: '+tpost);
          tempdata.Add('serie: '+tnome_serie);
          tempdata.Add('top: '+tid_topic);
          tempdata.Add('ep: '+tepisodio);
          tempdata.Add('trad: '+ttraduttori);
          tempdata.Add('rev: '+trevisore);
          tempdata.Add('res: '+tresynch);
        end;
      {$endif}
    end;
    if not tvalido then
    begin
      Reg.Expression := ExpPannelloInviaFolder;
      if Reg.Exec(buffer) then
      repeat
        cartella_value.Add(Reg.Match[1]);
        cartella_voce.Add(CleanName(Reg.Match[2]));
        tvalido := True;
      until not Reg.ExecNext;
    end;
  end;

  {$ifdef DEBUG}
    tempdata.SaveToFile(OpenedFile.DIR_SUB+'pform_extracted.txt');
    tempdata.Free;
    cartella_value.SaveToFile(OpenedFile.DIR_SUB+'pform_cval.txt');
    cartella_voce.SaveToFile(OpenedFile.DIR_SUB+'pform_cvoc.txt');
  {$endif}

  Reg.Free;
end;

destructor TPannelloInvia.Destroy;
begin
  cartella_value.Free;
  cartella_voce.Free;
  inherited;
end;

function TPannelloInvia.CartelleCount: Integer;
begin
  Result := cartella_value.Count;
end;

function TPannelloInvia.CartellaValue(index: Integer): string;
begin
  Result := '';
  if index < CartelleCount then
    Result := cartella_value.Strings[index];
end;

function TPannelloInvia.CartellaVoce(index: Integer): string;
begin
  Result := '';
  if index < CartelleCount then
    Result := cartella_voce.Strings[index];
end;

function TPannelloInvia.Serie: string;
begin
  Result := tnome_serie;
end;

function TPannelloInvia.Post: string;
begin
  Result := tpost;
end;

function TPannelloInvia.Topic: string;
begin
  Result := tid_topic;
end;

function TPannelloInvia.Episodio: string;
begin
  Result := tepisodio;
end;

function TPannelloInvia.Traduttori: string;
begin
  Result := ttraduttori;
end;

function TPannelloInvia.Revisore: string;
begin
  Result := trevisore;
end;

function TPannelloInvia.Resynch: string;
begin
  Result := tresynch;
end;

function TPannelloInvia.Valido: Boolean;
begin
  Result := tvalido;
end;

//------------------------------------------------

constructor TPannelloPost.Create;
var
  buffer: string;
  Reg: TRegExpr;
  i: Integer;
begin
  tvalido := False;
  Reg := TregExpr.Create;
  Reg.Expression := ExpPannelloPost;
  for i := 0 to Temp.TEMP.Count - 1 do
  begin
    buffer := Temp.TEMP.Strings[i];
    if Reg.Exec(buffer) then
      if Reg.Match[1] = 'ESEGUITO' then
      begin
        tvalido := True;
        Break;
      end
      else ShowMessage(Reg.Match[1]);
  end;
end;

function TPannelloPost.Valido: Boolean;
begin
  Result := tvalido;
end;


//----------------------------------------
// Operazioni invio

function PostBuilder(Bound, FieldName, Data: string): string;
var s: string;
begin
  s := '--' + Bound + CRLF
    + 'Content-Disposition: form-data; name="' + FieldName + '"'
    + CRLF + CRLF + Data + CRLF;
  Result := s;
end;

function ItasaZip(Rename: string): Boolean;
var
  i: Integer;
  zip: TKAZip;
begin
  Result := False;
  zip := TKAZip.Create(nil);
  try
    i := 0;
    if FileExists(Temp.TEMP_DIR + NomeFile.ZipFileName + '.zip') then
      while not RenameFile(Temp.TEMP_DIR + NomeFile.ZipFileName + '.zip',
                           Temp.TEMP_DIR + NomeFile.ZipFileName + '.bak' + IntToStr(i) + '.zip') do
        i := i + 1;
    zip.CreateZip(Temp.TEMP_DIR + NomeFile.ZipFileName + '.zip');
    zip.Open(Temp.TEMP_DIR + NomeFile.ZipFileName + '.zip');
    zip.AddFile(OpenedFile.DIR_SUB + OpenedFile.FILE_SUB, Rename + OpenedFile.EXT_SUB);
    Temp.SEND_FILE:=zip.FileName;
    zip.Close;
    Result := True;
  finally
    zip.Free;
  end;
  if not Result then
    raise Exception.Create('Error while zipping the file.');
end;


function ItasaZipv2(Rename, Zipfile: string): Integer;
var
  zip: TKAZip;
  i:Integer;
  Exist:Boolean;
begin
  Result := 0;
  zip := TKAZip.Create(nil);
  try
    zip.Open(ZipFile);
    Exist:=False;
    for i:=0 to zip.Entries.Count - 1 do
    begin
      if zip.Entries.Items[i].FileName = Rename + OpenedFile.EXT_SUB then
      begin
        ShowMessage('This resynch is already present in the zip.');
        Result:=-1;
        Exist:=True;
      end;
    end;
    if not Exist then
    begin
      zip.AddFile(OpenedFile.DIR_SUB + OpenedFile.FILE_SUB, Rename + OpenedFile.EXT_SUB);
      Result := zip.Entries.Count;
    end;
    zip.Close;
  finally
    //zipe.Free;
    zip.Free;
  end;
end;



function ItasaZipCountEntries(Zipfile: string): Integer;
var
  zip: TKAZip;
begin
  Result := 0;
  zip := TKAZip.Create(nil);
  try
    zip.Open(ZipFile);
    Result := zip.Entries.Count;
    zip.Close;
  finally
    zip.Free;
  end;
end;


function ItasaZipPacket(const FileList : TStringList): Boolean;
var
  i: Integer;
  zip: TKAZip;
begin
  Result := False;
  zip := TKAZip.Create(nil);
  try
    i := 0;
    if FileExists(Temp.TEMP_DIR+'temp_packet.zip') then
      while not RenameFile(Temp.TEMP_DIR+'temp_packet.zip',
                           Temp.TEMP_DIR+'temp_packet.bak' + IntToStr(i) + '.zip') do
        i := i + 1;
    zip.CreateZip(Temp.TEMP_DIR+'temp_packet.zip');
    zip.Open(Temp.TEMP_DIR+'temp_packet.zip');
    for i:=0 to FileList.Count - 1 do
      zip.AddFile(FileList.Strings[i],ExtractFileName(FileList.Strings[i]));
    Temp.SEND_FILE:=zip.FileName;
    zip.Close;
    Result := True;
  finally
    zip.Free;
  end;
end;



procedure CreateMessage;
var
  Node : PVirtualNode;
  NodeData : PFileSendTreeData;
  msgsub1, msgsub2, msgvid: string;
  i:Integer;
begin
  Node := SendToItasaForm.vdtQueue.GetFirst;
  i:=0;
  while Assigned(Node) do
  begin
    NodeData:=SendToItasaForm.vdtQueue.GetNodeData(Node);
    if not NodeData.Pannello and not NodeData.Sended then
    begin
      if i > 0 then
      begin
        msgsub1:= msgsub1 + ', ';
        msgsub2:= msgsub2 + ', ';
        msgvid := msgvid + ', ';
      end;
      msgsub1:=msgsub1+NodeData.MsgSub1;
      msgsub2:=msgsub2+NodeData.MsgSub2;
      msgvid :=msgvid+NodeData.MsgVideo;
      i:=i+1;
    end;
    Node:=SendToItasaForm.vdtQueue.GetNext(Node);
  end;
  //sostituzione tag nel corpo del messaggio
  UserInfo.MessaggioP.Text := StringReplace(UserInfo.Messaggio.Text, '#sub1#', msgsub1, [rfReplaceAll]);
  UserInfo.MessaggioP.Text := StringReplace(UserInfo.MessaggioP.Text, '#sub2#', msgsub2, [rfReplaceAll]);
  UserInfo.MessaggioP.Text := StringReplace(UserInfo.MessaggioP.Text, '#video#', msgvid, [rfReplaceAll]);
end;


function ItasaLogin: Boolean;
var
  login: string;
begin
  Result := False;
  login := 'username=' + UserInfo.Username
    + '&passwd=' + UserInfo.Passwd
    + '&option=com_user&Submit=Login&task=login';
  try
    HTTP.Clear;
    HTTP.Document.Write(Pointer(login)^, Length(login));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTP.HTTPMethod('POST', ItasaUrls.URL_ITASA + ItasaUrls.URL_LOGIN);
    case HTTP.ResultCode of
    100..200,301,302:
      begin
        Temp.COOKIE.Text := HTTP.Cookies.Text;
        if Pos('SMFCookie', Temp.COOKIE.Text) > 0 then
          Result := True;
      end;
    400..499:
      raise Exception.Create('[Login] Error in connection: Page not found');
    500..599:
      raise Exception.Create('[Login] Error in connection: No connection');
    else
      raise Exception.Create('[Login] Error in connection: HTTP Result Code ' + IntToStr(HTTP.ResultCode));
    end;
  finally
    HTTP.Clear;
  end;
end;


function ItasaDownloadPagina(Url: string): Boolean;
var
  i:Integer;
  loc:string;
begin
  Result := False;
  try
    Temp.TEMP.Clear;
    HTTP.Clear;
    HTTP.Cookies.Text := Temp.COOKIE.Text;
    HTTP.KeepAlive:=True;
    HTTP.HTTPMethod('GET', Url);
    case HTTP.ResultCode of
    100..200:
      begin
        Temp.TEMP.LoadFromStream(HTTP.Document);
        Result := True;
      end;
    301,302,307:
      begin
       for i := 0 to HTTP.Headers.Count - 1 do
         if (Pos('Location: ', HTTP.Headers.Strings[i]) > 0) or
            (Pos('location: ', HTTP.Headers.Strings[i]) > 0) then
         begin
           loc:=StringReplace(HTTP.Headers.Strings[i],'Location: ','', [rfReplaceAll,rfIgnoreCase]);
           ItasaUrls.URL_TOPIC := loc;
           Result:=ItasaDownloadPagina(loc);
           Break;
         end;
      end;
    400..499:
      raise Exception.Create('[IDP] Error in connection: Page not found');
    500..599:
      raise Exception.Create('[IDP] Error in connection: No connection');
    else
      raise Exception.Create('[IDP] Error in connection: HTTP Result Code ' + IntToStr(HTTP.ResultCode));
    end;
  finally
    HTTP.Clear;
  end;
end;


function ItasaDownloadInfoTopic: Boolean;
begin
  try
    if ItasaDownloadPagina(ItasaUrls.URL_ITASA + ItasaUrls.URL_TOPIC_ID) then
    //Temp.TEMP.LoadFromFile(OpenedFile.DIR_SUB+'in_top_temp.html');
      InfoTopic := TInfoTopic.Create;
    Result := InfoTopic.Valid;

    if not Result then
    if MessageDlg('Error found. Save debug info?',  mtConfirmation, mbOKCancel, 0) = 1 then
    begin
      Temp.TEMP.Add('#####');
      Temp.TEMP.Add(InfoTopic.Topic);
      Temp.TEMP.Add(InfoTopic.Subject);
      Temp.TEMP.Add(InfoTopic.LastMsg);
      Temp.TEMP.Add(InfoTopic.SessionId);
      Temp.TEMP.Add(InfoTopic.SessionVar);
      Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'debug_top.log');
    end;
  except
    on E : Exception do
      raise Exception.Create(E.Message);
  end;
end;


function ItasaDownloadInfoPannello: Boolean;
begin
  try
    if ItasaDownloadPagina(ItasaUrls.URL_ITASA + ItasaUrls.URL_PANNELLO) then
    //Temp.TEMP.LoadFromFile(OpenedFile.DIR_SUB+'debug_snf.log');
      InfoPannello := TPannello.Create;
    Result := InfoPannello.Valido;

    if not Result then
    begin
      if MessageDlg('Error found. Save debug info?',  mtConfirmation, mbOKCancel, 0) = 1 then
      begin
        Temp.TEMP.Add('#####');
        Temp.TEMP.AddStrings(InfoPannello.tSerie);
        Temp.TEMP.Add('#####');
        Temp.TEMP.AddStrings(InfoPannello.tStatus);
        Temp.TEMP.Add('#####');
        Temp.TEMP.AddStrings(InfoPannello.tUrl);
        Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'debug_pan.log');
      end;
    end;
  except
    on E : Exception do
      raise Exception.Create(E.Message);
  end;
end;


function ItasaDownloadInfoPannelloInvio: Boolean;
var
  i: Integer;
begin
  try
    Result := False;

    //Temp.TEMP.LoadFromFile(OpenedFile.DIR_SUB+'debug_fnf.html');
    //InfoPannelloInvia := TPannelloInvia.Create;
    //Result := InfoPannelloInvia.Valido;
    
    for i := 0 to InfoPannello.Count - 1 do
    begin
      if (AnsiCompareText(NomeFile.NamePannello, InfoPannello.Serie(i)) = 0) then
      begin
        //if Url <> nil then the resynch is active
        if InfoPannello.Url(i) <> '' then
        begin
          if ItasaDownloadPagina(InfoPannello.Url(i)) then
            InfoPannelloInvia := TPannelloInvia.Create;
          Result := InfoPannelloInvia.Valido;
        end
        else
        begin
          Result := True;
          MessageDlg('Series found but the resynch is not enabled by the Revisore.', mtInformation, [mbOK], 0);
        end;
        Break;
      end;
    end;

    if not Result then
    begin
      for i := 0 to InfoPannello.Count - 1 do
      begin
        if LevenshteinDistance(NomeFile.NamePannello, InfoPannello.Serie(i)) < 3 then
        begin
          if MessageDlg('Error found. Save debug info?',  mtConfirmation, mbOKCancel, 0) = 1 then
          begin
            Temp.TEMP.Add('#####');
            Temp.TEMP.Add(NomeFile.FileName);
            Temp.TEMP.Add('#####');
            Temp.TEMP.AddStrings(InfoPannello.tSerie);
            Temp.TEMP.Add('#####');
            Temp.TEMP.AddStrings(InfoPannello.tStatus);
            Temp.TEMP.Add('#####');
            Temp.TEMP.AddStrings(InfoPannello.tUrl);
            Temp.TEMP.Add('#####');
            Temp.TEMP.Add(NomeFile.NamePannello);
            Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'debug_snf.log');
          end;
          Break;
        end;
      end;
    end;
  except
    on E : Exception do
      raise Exception.Create(E.Message);
  end;
end;


function ItasaSelectCartella(SendResynch:TPannelloInvia;TopicID:string): string;
var
  i: Integer;
begin
  Result := '';

  if TopicID <> '' then
  begin
    if ItasaDownloadPagina(ItasaUrls.URL_ITASA+ItasaUrls.URL_MANUALE+TopicID) then
    begin
      InfoPannelloInvia := TPannelloInvia.Create;
      if InfoPannelloInvia.Valido then
        SendResynch := InfoPannelloInvia
      else Exit;
    end;
  end;
  
  for i := 0 to SendResynch.CartelleCount - 1 do
  begin
    if (AnsiCompareText(NomeFile.NameFolder, SendResynch.CartellaVoce(i)) = 0) then
    begin
      Result := SendResynch.CartellaValue(i);
      Exit;
    end;
  end;

  if AnsiCompareText(NomeFile.SeriesName,
                     Copy(SendResynch.CartellaVoce(0),1,Length(NomeFile.SeriesName))) <> 0 then
  if MessageDlg('Error found. Save debug info?',  mtConfirmation, mbOKCancel, 0) = 1 then
  begin
    Temp.TEMP.Add('#####');
    Temp.TEMP.Add(NomeFile.FileName);
    Temp.TEMP.Add('#####');
    Temp.TEMP.AddStrings(InfoPannelloInvia.cartella_value);
    Temp.TEMP.Add('#####');
    Temp.TEMP.AddStrings(InfoPannelloInvia.cartella_voce);
    Temp.TEMP.Add('#####');
    Temp.TEMP.Add(NomeFile.NameFolder);
    Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'debug_fnf.log');
  end;
end;

procedure ItasaPostPannello;
var
  SendFile: TFileStream;
  Bound, s: string;
  Risultato: TPannelloPost;
  Node : PVirtualNode;
  NodeData : PFileSendTreeData;
begin
  try
    Node := SendToItasaForm.vdtQueue.GetFirst;
    while Assigned(Node) do
    begin
      NodeData:=SendToItasaForm.vdtQueue.GetNodeData(Node);
      if NodeData.Pannello and not NodeData.Sended then
      begin
        NodeData.Sending:=True;
        SendToItasaForm.vdtQueue.RepaintNode(Node);
        HTTP.Clear;
        HTTP.Cookies.Text := Temp.COOKIE.Text;
        SendFile := TFileStream.Create(NodeData.FileName, fmOpenRead or fmShareDenyWrite);
        if SendFile.Size = 0 then
          raise Exception.Create('[Pannello] File open error.');
        Bound := '---------------------------' + IntToHex(Random(MaxInt), 11);
        s := PostBuilder(Bound, 'nome_serie', NodeData.Serie)
           + PostBuilder(Bound, 'id_topic',   NodeData.Topic)
           + PostBuilder(Bound, 'episodio',   NodeData.Episodio)
           + PostBuilder(Bound, 'traduttori', NodeData.Traduttori)
           + PostBuilder(Bound, 'revisore',   NodeData.Revisore)
           + PostBuilder(Bound, 'resynch',    NodeData.Resynch)
           + PostBuilder(Bound, 'cartella',   NodeData.Cartella)
           + PostBuilder(Bound, 'MAX_FILE_SIZE', '300000')
           + '--' + Bound + CRLF
           + 'Content-Disposition: form-data; name="userfile"; '
           + 'filename="' + ExtractFileName(NodeData.FileName) + '"' + CRLF
           + 'Content-Type: application/zip' + CRLF + CRLF;
        HTTP.Document.Write(Pointer(s)^, Length(s));
        HTTP.Document.CopyFrom(SendFile, 0);
        s := CRLF + '--' + Bound + '--' + CRLF;
        HTTP.Document.Write(Pointer(s)^, Length(s));
        HTTP.MimeType := 'multipart/form-data, boundary=' + Bound;
        HTTP.HTTPMethod('POST', ItasaUrls.URL_ITASA + NodeData.Post);

        Temp.TEMP.LoadFromStream(HTTP.Document);
        Risultato := TPannelloPost.Create;
        if Risultato.Valido then
          NodeData.Sended:=True
        else
          NodeData.Pannello:=False;
        Risultato.Free;
        SendToItasaForm.vdtQueue.RepaintNode(Node);
      end;
      Node:=SendToItasaForm.vdtQueue.GetNext(Node);
    end;
  finally
    HTTP.Clear;
    SendFile.Free;
  end;
end;

procedure ItasaPostTopic;
var
  SendFile: TFileStream;
  Bound, s: string;
  Node : PVirtualNode;
  NodeData : PFileSendTreeData;
  Risultato : TTopicPost;
begin
  try
    HTTP.Clear;
    Bound := '---------------------------' + IntToHex(Random(MaxInt), 11);
    HTTP.Cookies.Text := Temp.COOKIE.Text;
    s := PostBuilder(Bound, 'topic', InfoTopic.Topic)
       + PostBuilder(Bound, 'subject', InfoTopic.Subject)
       + PostBuilder(Bound, 'last_msg', InfoTopic.LastMsg)
       + PostBuilder(Bound, InfoTopic.SessionVar, InfoTopic.SessionId);

    HTTP.Document.Write(Pointer(s)^, Length(s));
    Node := SendToItasaForm.vdtQueue.GetFirst;
    while Assigned(Node) do
    begin
      NodeData:=SendToItasaForm.vdtQueue.GetNodeData(Node);
      if not NodeData.Pannello and not NodeData.Sended and not TopicLoop then
      begin
        SendFile := TFileStream.Create(NodeData.FileName, fmOpenRead or fmShareDenyWrite);
        if SendFile.Size = 0 then
          raise Exception.Create('[Topic] File open error.');
        s := '--' + Bound + CRLF
          + 'Content-Disposition: form-data; name="attachment[]"; '
          + 'filename="' + ExtractFileName(NodeData.FileName) + '"' + CRLF
          + 'Content-Type: application/zip' + CRLF + CRLF;
        HTTP.Document.Write(Pointer(s)^, Length(s));
        HTTP.Document.CopyFrom(SendFile, 0);
        HTTP.Document.Write(CRLF, Length(CRLF));
        NodeData.Sending:=True;
        SendToItasaForm.vdtQueue.RepaintNode(Node);
        SendFile.Free;
      end;
      Node:=SendToItasaForm.vdtQueue.GetNext(Node);
    end;
    //Check in message is not empty
    if (Trim(UserInfo.MessaggioP.Text) = '') then
      UserInfo.MessaggioP.Text := ':ciao:';
    if NeedcharsetConversion(UserInfo.MessaggioP.Text)then
      UserInfo.MessaggioP.Text:= CharsetConversion(UserInfo.MessaggioP.Text,GetCurCP,UTF_8);
    s := PostBuilder(Bound, 'message', UserInfo.MessaggioP.Text)
      + '--' + Bound + '--' + CRLF;
    HTTP.Document.Write(Pointer(s)^, Length(s));
    HTTP.MimeType := 'multipart/form-data, boundary=' + Bound;
    HTTP.HTTPMethod('POST', ItasaUrls.URL_POST);

    case HTTP.ResultCode of
    100..200,302:
      begin
        Temp.TEMP.LoadFromStream(HTTP.Document);
        Risultato := TTopicPost.Create;
        if Risultato.Valido then
        begin
          Node := SendToItasaForm.vdtQueue.GetFirst;
          while Assigned(Node) do
          begin
            NodeData:=SendToItasaForm.vdtQueue.GetNodeData(Node);
            if not NodeData.Pannello and NodeData.Sending then
            begin
              NodeData.Sended:=True;
              SendToItasaForm.vdtQueue.RepaintNode(Node);
            end;
            Node:=SendToItasaForm.vdtQueue.GetNext(Node);
          end;
        end
        else
        begin
          if not TopicLoop then
          begin
            TopicLoop := True;
            if not ItasaDownloadInfoTopic then
              ShowMessage('info error')
            else
              ItasaPostTopic;
          end
          else if MessageDlg('Error found. Save debug info?',  mtConfirmation, mbOKCancel, 0) = 1 then
          begin
            Temp.TEMP.SaveToFile(OpenedFile.DIR_SUB+'debug_topic.log');
            Node := SendToItasaForm.vdtQueue.GetFirst;
            while Assigned(Node) do
            begin
              NodeData:=SendToItasaForm.vdtQueue.GetNodeData(Node);
              if not NodeData.Pannello and
                 not NodeData.Sended and
                 NodeData.Sending then
              begin
                NodeData.Sending:=False;
                SendToItasaForm.vdtQueue.RepaintNode(Node);
              end;
              Node:=SendToItasaForm.vdtQueue.GetNext(Node);
            end;
          end;
        end;
        Risultato.Free;
      end;
    else
      begin
        Node := SendToItasaForm.vdtQueue.GetFirst;
        while Assigned(Node) do
        begin
          NodeData:=SendToItasaForm.vdtQueue.GetNodeData(Node);
          if not NodeData.Pannello and
             not NodeData.Sended and
             NodeData.Sending then
          begin
            NodeData.Sending:=False;
            SendToItasaForm.vdtQueue.RepaintNode(Node);
          end;
          Node:=SendToItasaForm.vdtQueue.GetNext(Node);
        end;
        ShowMessage('[IPT] Error: ' + IntToStr(HTTP.ResultCode));
      end;
    end;

  finally
    SendToItasaForm.Height:=340;
    HTTP.Clear;
  end;
end;

end.
