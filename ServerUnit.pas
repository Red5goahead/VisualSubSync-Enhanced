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

unit ServerUnit;

interface

uses Classes, WinSock2, Windows, Contnrs, WAVDisplayerUnit, IniFiles;

type
  TMyNamedThread = class(TThread)
  private
    FName : string;
    procedure SetName;
  public
    constructor Create(CreateSuspended : Boolean; Name : string = '');
  end;

  THTTPServer = class(TMyNamedThread)
  private
    FRootDir : string;
    FRunning : Boolean;
    FListenSock : TSocket;
    FPort : Integer;
    FEnableCompression : Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(RootDir : string; Port : Integer = 80);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property EnableCompression : Boolean read FEnableCompression write FEnableCompression;
  end;

  THTTPRequestThreadProcessor = class(TMyNamedThread)
  private
    FRequest : TStringList;
    FRequestHeader : TStringList; // "field = value"
    FPostVars : TStringList; // "field = value"
    FAnswerHeader : TStringList; // ready to send "Field: value"
    FAnswerStream : TMemoryStream;    
    FFileToSend : string;
    FTokenizer : TStringList;
    FEnvVars : THashedStringList;
    FCanUseGzip : Boolean;
    FUseGzip : Boolean;

    function ReadLine : string;
    procedure ReceiveRequest;
    procedure ProcessRequestHeaderFields;    
    procedure ProcessRequest;
    procedure ProcessGet(Path : string);
    procedure ProcessPost(Path : string);
    procedure CreateErrorPage(ErrorCode : Integer; Msg : string);
    procedure ProcessDynamicPage(Filename : string);
    procedure SendAnswer;
    procedure SendFile(Filename : string);
    procedure SendStream(Stream : TStream);
    function ProcessVirtualWav(VirtualPath : string) : Boolean;
    procedure AddStats(Start : Cardinal);    
  protected
    procedure Execute; override;
  public
    ClientSock : TSocket;
    ClientAddrIn : TSockAddrIn;
    RootDir : string;
    EnableCompression : Boolean;

    constructor Create;
    destructor Destroy; override;    
  end;

  MIME_REC = record
    ext : PChar;
    mime : PChar;
  end;

  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;

// =============================================================================

implementation

uses SysUtils, StrUtils, PageProcessorUnit, GlobalUnit, WAVFileUnit,
  MiscToolsUnit, SuggestionFormUnit, ZLib;

const
  CR : Char = #13;
  LF : Char = #10;
  SP : Char = #20;
  CRLF : string = #13#10;

  MIMETable: array[0..11] of MIME_REC = (
    (ext: '.html'; mime: 'text/html'),
    (ext: '.htm'; mime: 'text/html'),
    (ext: '.shtml'; mime: 'text/html'),
    (ext: '.css'; mime: 'text/css'),
    (ext: '.xml'; mime: 'text/xml'),
    (ext: '.txt'; mime: 'text/plain'),
    (ext: '.gif'; mime: 'image/gif'),
    (ext: '.jpg'; mime: 'image/jpeg'),
    (ext: '.jpeg'; mime: 'image/jpeg'),
    (ext: '.png'; mime: 'image/png'),
    (ext: '.wav'; mime: 'audio/x-wav'),
    (ext: '.mp3'; mime: 'audio/mpeg')
  );

// =============================================================================

function ParseTimeHexa(TimeStr : string; var Start,Stop : Integer) : Boolean;
var i : integer;
    StrStart, StrStop : string;
begin
  Result := False;
  Start := 0;
  Stop := 0;
  i := Pos('-',TimeStr);
  if (i > 0) then
  begin
    StrStart := '$' + Copy(TimeStr, 1, i-1);
    StrStop := '$' + Copy(TimeStr, i+1, Length(TimeStr));
    Start := StrToIntDef(StrStart,-1);
    Stop := StrToIntDef(StrStop,-1);
    if (Start <> -1) and (Stop <> -1) or (Start < Stop) then
      Result := True;
  end;
end;

// =============================================================================

constructor TMyNamedThread.Create(CreateSuspended : Boolean; Name : string);
begin
  inherited Create(CreateSuspended);
  
  if (Length(Name) = 0) then
    FName := Self.ClassName
  else
    FName := Name;
end;

procedure TMyNamedThread.SetName;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PAnsiChar(FName);
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
end;

// =============================================================================

constructor THTTPServer.Create(RootDir : string; Port : Integer);
begin
  inherited Create(True);
  FRootDir := IncludeTrailingPathDelimiter(RootDir);
  FRunning := False;
  FPort := Port;
end;

//------------------------------------------------------------------------------

destructor THTTPServer.Destroy;
begin
  Stop;
end;

//------------------------------------------------------------------------------

procedure THTTPServer.Start;
var WSData : TWSAData;
begin
  if WSAStartup(WINSOCK_VERSION, WSData) <> 0 then
    Exit;
  FRunning := True;
  Resume;
end;

//------------------------------------------------------------------------------

procedure THTTPServer.Stop;
begin
  if FRunning then
  begin
    Terminate;
    closesocket(FListenSock);
    WSACleanup;
  end;
  WaitFor;
  FRunning := False;
end;

//------------------------------------------------------------------------------

procedure THTTPServer.Execute;
var ClientSock : TSocket;
    ListenAddrIn, ClientAddrIn : TSockAddrIn;
    SizeOfClientAddrIn : Integer;
    RequestThreadProcessor : THTTPRequestThreadProcessor;
begin
  SetName;

  FListenSock := socket(AF_INET, SOCK_STREAM, 0);
  if (FListenSock = INVALID_SOCKET) then
    Exit;

  ZeroMemory(@ListenAddrIn, SizeOf(ListenAddrIn));
  ListenAddrIn.sin_family := AF_INET;
  ListenAddrIn.sin_port := htons(FPort);
  ListenAddrIn.sin_addr.S_addr := ADDR_ANY;

  if bind(FListenSock, @ListenAddrIn, SizeOf(ListenAddrIn)) < 0 then
    Exit;

  if listen(FListenSock,4) < 0 then
    Exit;

  while not Terminated do
  begin
    SizeOfClientAddrIn := SizeOf(ClientAddrIn);
    ClientSock := accept(FListenSock, @ClientAddrIn, @SizeOfClientAddrIn);
    OutputDebugString('THTTPServer.Execute : new connection');
    if (ClientSock = INVALID_SOCKET) then
      Continue;
    RequestThreadProcessor := THTTPRequestThreadProcessor.Create();
    RequestThreadProcessor.ClientSock := ClientSock;
    RequestThreadProcessor.ClientAddrIn := ClientAddrIn;
    RequestThreadProcessor.RootDir := FRootDir;
    RequestThreadProcessor.EnableCompression := FEnableCompression;
    RequestThreadProcessor.Resume;
  end;
end;

// =============================================================================

constructor THTTPRequestThreadProcessor.Create;
begin
  inherited Create(True);
  FRequest := TStringList.Create;
  FRequestHeader := TStringList.Create;
  FPostVars := TStringList.Create;
  FAnswerHeader := TStringList.Create;
  FAnswerStream := TMemoryStream.Create;
  FTokenizer := TStringList.Create;
  Self.FreeOnTerminate := True;
  FEnvVars := THashedStringList.Create;
end;

//------------------------------------------------------------------------------

destructor THTTPRequestThreadProcessor.Destroy;
begin
  FRequest.Free;
  FRequestHeader.Free;
  FPostVars.Free;
  FAnswerHeader.Free;
  FAnswerStream.Free;
  FTokenizer.Free;
  FEnvVars.Free;
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.Execute;
begin
  SetName;
  
  FRequest.Clear;
  FRequestHeader.Clear;
  FAnswerHeader.Clear;
  FAnswerStream.Clear;
  FEnvVars.Clear;
  FPostVars.Clear;
  FUseGzip := False;
  FCanUseGzip := False;

  ReceiveRequest;
  ProcessRequestHeaderFields;
  ProcessRequest;
  SendAnswer;

  OutputDebugString('THTTPRequestThreadProcessor.Execute : Closing socket.');
  closesocket(ClientSock);
end;

//------------------------------------------------------------------------------

function THTTPRequestThreadProcessor.ReadLine : string;
var c : Char;
    FinishedNormally : Boolean;
begin
  Result := '';
  FinishedNormally := False;
  while (recv(ClientSock, c, 1, 0) > 0) do
  begin
    if (c = CR) then
    begin
      FinishedNormally := (recv(ClientSock, c, 1, 0) > 0); // read the LF
      Break;
    end;
    Result := Result + c;
  end;
  if not FinishedNormally then
    Result := '';
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.ReceiveRequest;
var Line : string;
begin
  OutputDebugString('THTTPRequestThreadProcessor.ReceiveRequest');
  while True do
  begin
    Line := ReadLine;
    if Line = '' then
      Break;
    FRequest.Add(Trim(Line));
  end;
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.ProcessRequestHeaderFields;
var i, p : Integer;
    key, value : string;
begin
  for i:=1 to FRequest.Count-1 do
  begin
    p := Pos(':', FRequest[i]);
    if (p > 0) then
    begin
      key := Trim(Copy(FRequest[i],1,p-1));
      value := Trim(Copy(FRequest[i],p+1,Length(FRequest[i])));
      FRequestHeader.Add(LowerCase(key) + '=' + value);
      FEnvVars.Values['HEADER['+key+']'] := value;
    end;
  end;
end;

//------------------------------------------------------------------------------

function MoreDataOnSocket(Socket : TSocket) : Boolean;
var ReadFDS : TFDSet;
    TimeOut : TTimeVal;
    SelectRet : Integer;
begin
  FD_ZERO(ReadFDS);
  FD_SET(Socket,ReadFDS);
  TimeOut.tv_sec := 0;
  TimeOut.tv_usec := 0;
  SelectRet := select(1,@ReadFDS,nil,nil,@TimeOut);
  Result := (SelectRet > 0) and FD_ISSET(Socket,ReadFDS);
end;

//----------

procedure FlushSocket(Socket : TSocket);
var c : Char;
    RecvOK : Boolean;
begin
  RecvOK := True;
  while MoreDataOnSocket(Socket) and RecvOK do
  begin
    RecvOK := (recv(Socket, c, 1, 0) > 0);
  end;
end;

//----------

procedure THTTPRequestThreadProcessor.ProcessRequest;
var Line : string;
    Error : Boolean;
begin
  Error := True;
  if FRequest.Count > 0 then
  begin
    Line := FRequest[0];
    // GET, POST ...
    FTokenizer.Clear;
    FTokenizer.Delimiter := SP;
    FTokenizer.DelimitedText := Line;

    if (FTokenizer.Count > 0) then
    begin
      if (FTokenizer.Count >= 3) then
      begin
        // GET SP URI SP HTTPVERSION
        if (FTokenizer[0] = 'GET') then
        begin
          Error := False;
          FlushSocket(ClientSock); // should not be usefull but we never know :D
          ProcessGet(FTokenizer[1]);
        end
        else if (FTokenizer[0] = 'POST') then
        begin
          Error := False;
          ProcessPost(FTokenizer[1]);
          FlushSocket(ClientSock); // IE fix :p
          ProcessGet(FTokenizer[1]);
        end;
      end;
    end;
  end;

  if Error then
  begin
    FAnswerHeader.Add('HTTP/1.0 501 Not Implemented');
    FAnswerHeader.Add('');
    CreateErrorPage(501, 'Not Implemented');
  end;
end;

//------------------------------------------------------------------------------

function ExtToMIMEType(Ext : string) : string;
var i : integer;
begin
  Result := 'text/html';
  for i := 0 to Length(MIMETable)-1 do
  begin
    if MIMETable[i].ext = Ext then
    begin
      Result := MIMETable[i].mime;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function ResolvePath(Filename : string) : string;
var i, j, p : integer;
    PathElem : string;
begin
  i := 1;
  p := 1;
  while (p <> 0) do
  begin
    p := PosEx('\', Filename, i);
    if (p > 0) then
      PathElem := Copy(Filename,i,p-i)
    else
      PathElem := Copy(Filename,i,Length(Filename));

    if PathElem = '..' then
    begin
      j := Length(Result);
      while (j > 0) and (Result[j] <> '\') do
        Dec(j);
      Delete(Result,j,Length(Result));
    end
    else if (PathElem <> '.') and (PathElem <> '') then
      Result := Result + '\' + PathElem;
    i := p+1;
  end;
  // Remove slash at start
  Delete(Result,1,1);
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.AddStats(Start : Cardinal);
var Stop : Cardinal;
    s : string;
    i : Integer;
begin
  for i:=0 to FEnvVars.Count-1 do
  begin
    s := Format ('%s = %s#13#10', [FEnvVars.Names[i], FEnvVars.ValueFromIndex[i]]);
    FAnswerStream.Write(s[1], Length(s));
  end;
  Stop := GetTickCount;
  s := Format('Generated in %d ms.', [Stop-Start]);
  FAnswerStream.Write(s[1], Length(s));
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.ProcessGet(Path : string);
var Filename, PossibleFilename, Ext, Params : string;
    i : Integer;
    DirectoryIndexFound : Boolean;
    Start : Cardinal;
const
  DirectoryIndexLst : array[0..2] of PChar = (
    'index.shtml',
    'index.html',
    'index.htm'
  );
begin
  if (LeftStr(Path,5) = '/wav/') and (RightStr(Path,4) = '.wav') then
  begin
    // Special virtual WAV handling
    if ProcessVirtualWav(Path) then
    begin
      FAnswerHeader.Add('HTTP/1.0 200 OK');
      Ext := LowerCase(ExtractFileExt(Path));
      FAnswerHeader.Add('Content-Type: ' + ExtToMIMEType(Ext));
      FAnswerHeader.Add('');
      Exit;
    end;
  end;

  // 'Accept-Encoding: gzip, deflate';
  FCanUseGzip := EnableCompression and
    (Pos('deflate',FRequestHeader.Values['accept-encoding']) <> 0);

  // Fix slashes
  Filename := StringReplace(Path,'/','\',[rfReplaceAll]);


  // Remove anchor (TODO : anchors + parameters)
  i := Pos('#', Filename);
  if (i > 0) then
    Delete(Filename, i, Length(Filename));

  // Remove and parse parameters
  i := Pos('?', Filename);
  if (i > 0) then
  begin
    // parse url parameters (?key1=value1&key2=value2....)
    Params := Copy(Filename,i+1,Length(Filename));
    Delete(Filename,i,Length(Filename));
    FEnvVars.Delimiter := '&';
    FEnvVars.DelimitedText := Params;
  end;

  // Prefix with root dir
  Filename := RootDir + Filename;

  Filename := ResolvePath(Filename);
  if Pos(ExcludeTrailingPathDelimiter(RootDir), Filename) <> 1 then
  begin
    FAnswerHeader.Add('HTTP/1.0 403 Forbidden');
    CreateErrorPage(403, 'Forbidden : RootDir = ' + RootDir + '<br>' +
      'Access path = ' + Path + '<br>' +
      'Filename = ' + Filename
    );
    FAnswerHeader.Add('');
    Exit;
  end;

  if DirectoryExists(Filename) then
  begin
    DirectoryIndexFound := False;
    for i := 0 to Length(DirectoryIndexLst)-1 do
    begin
      PossibleFilename := IncludeTrailingPathDelimiter(Filename) + DirectoryIndexLst[i];
      if FileExists(PossibleFilename) then
      begin
        DirectoryIndexFound := True;
        Filename := PossibleFilename;
        Break;
      end;
    end;
    if (not DirectoryIndexFound) then
    begin
      // TODO : Create directory listing
      FAnswerHeader.Add('HTTP/1.0 501 Not Implemented');
      CreateErrorPage(501, 'Directory listing not implemented.');
      FAnswerHeader.Add('');
      Exit;
    end;
  end;

  if FileExists(Filename) then
  begin
    FAnswerHeader.Add('HTTP/1.0 200 OK');
    Ext := LowerCase(ExtractFileExt(Filename));
    FAnswerHeader.Add('Content-Type: ' + ExtToMIMEType(Ext));
    if (Ext = '.shtml') then
    begin
      Start := GetTickCount;
      ProcessDynamicPage(Filename);
      // AddStats(Start);
    end
    else
      FFileToSend := Filename;
  end
  else
  begin
    FAnswerHeader.Add('HTTP/1.0 404 Not Found');
    CreateErrorPage(404, 'Not Found : ' + Filename);
  end;
  FAnswerHeader.Add('');
end;

//------------------------------------------------------------------------------

function MyGetFileSize(Filename : string) : Int64;
var F : THandle;
    FileSizeL, FileSizeH : DWORD;
begin
  Result := 0;
  F := CreateFile(PAnsiChar(Filename), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (F <> INVALID_HANDLE_VALUE) then
  begin
    FileSizeL := GetFileSize(F,@FileSizeH);
    if FileSizeL <> INVALID_FILE_SIZE then
    begin
      Result := FileSizeH;
      Result := Result shl 32;
      Result := Result or FileSizeL;
    end;
    CloseHandle(F);
  end;
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.SendFile(Filename : string);
var F : THandle;
    Buffer : array[0..4095] of Char;
    BytesRead : Cardinal;
    SendOk : Boolean;
begin
  OutputDebugString(PAnsiChar('THTTPRequestThreadProcessor.SendFile' + Filename));
  SendOk := True;
  F := CreateFile(PAnsiChar(Filename), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (F <> INVALID_HANDLE_VALUE) then
  begin
    while ReadFile(F, Buffer, 4096, BytesRead, nil) and (BytesRead > 0) and SendOk do
    begin
      SendOk := (send(ClientSock, Buffer, BytesRead, 0) > 0);
    end;
    CloseHandle(F);
  end;
end;

//------------------------------------------------------------------------------

function GetDateString : string;
const
  EngDayName : array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  EngMonthName : array[1..12] of string = ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
var
  Present: TDateTime;
  Year, Month, Day, DOW, Hour, Min, Sec, MSec : Word;
begin
  //Date: Thu, 01 Jan 2004 17:11:47 GMT
  Present:= Now;
  DOW := DayOfWeek(Present);
  DecodeDate(Present, Year, Month, Day);
  DecodeTime(Present, Hour, Min, Sec, MSec);
  Result := Format('%s, %2.2d %s %4.4d %2.2d:%2.2d:%2.2d GMT',
    [EngDayName[DOW], Day, EngMonthName[Month], Year,
      Hour, Min, Sec ]);
end;

procedure THTTPRequestThreadProcessor.SendAnswer;
var s : string;
    MessageSize : Integer;
begin
  if (FFileToSend <> '') then
    MessageSize := MyGetFileSize(FFileToSend)
  else
    MessageSize := FAnswerStream.Size;

  // Send answer header  
  if (FUseGzip) then
  begin
    FAnswerHeader.Insert(1, 'Content-encoding: deflate');
    FAnswerHeader.Insert(2, 'Vary: Accept-Encoding');
  end;

  FAnswerHeader.Insert(1, 'Content-Length: ' + IntToStr(MessageSize));
  FAnswerHeader.Insert(1, 'Server: VisualSubSync/'+ g_ApplicationVersion.VersionString);
  //FAnswerHeader.Insert(1, 'Connection: Keep-Alive');
  //FAnswerHeader.Insert(1, 'Connection: close');
  //FAnswerHeader.Insert(1, 'Pragma: no-cache');
  FAnswerHeader.Insert(1, 'Date: ' + GetDateString);

  s := FAnswerHeader.Text;
  send(ClientSock, s[1], Length(s), 0);

  if (FFileToSend <> '') then
    SendFile(FFileToSend)
  else
    SendStream(FAnswerStream)
end;

//------------------------------------------------------------------------------

procedure WriteStreamLn(Stream : TStream; s : string);
begin
  s := s + #13#10;
  Stream.Write(s[1],Length(s));
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.CreateErrorPage(ErrorCode : Integer; Msg : string);
begin
  WriteStreamLn(FAnswerStream, '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">');
  WriteStreamLn(FAnswerStream, '<html><head>');
  WriteStreamLn(FAnswerStream, Format('<title>%d %s</title>',[ErrorCode,Msg]));
  WriteStreamLn(FAnswerStream, '</head><body>');
  WriteStreamLn(FAnswerStream, Format('<h2>%d ERROR : %s</h2>',[ErrorCode,Msg]));
  WriteStreamLn(FAnswerStream, '</body></html>');
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.ProcessDynamicPage(Filename : string);
var PP : TPageProcessor;
    CStream : TCompressionStream;
begin
  PP := TPageProcessor.Create;
  if (FCanUseGzip) then
  begin
    FUseGzip := True;
    CStream := TCompressionStream.Create(clDefault,FAnswerStream);
    g_WebRWSynchro.BeginRead;
    try
      PP.ProcessPage(Filename, CStream, FEnvVars);
    finally
      g_WebRWSynchro.EndRead;
    end;
    CStream.Free;
  end
  else
  begin
    g_WebRWSynchro.BeginRead;
    try
      PP.ProcessPage(Filename, FAnswerStream, FEnvVars);
    finally
      g_WebRWSynchro.EndRead;
    end;
  end;
  PP.Free;
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.SendStream(Stream : TStream);
var Buffer : array[0..4095] of Char;
    BytesRead : Cardinal;
    SendOk : Boolean;
begin
  OutputDebugString(PAnsiChar('THTTPRequestThreadProcessor.SendStream'));
  Stream.Seek(0,0);
  BytesRead := 4096;
  SendOk := True;
  while (BytesRead = 4096) and SendOk do
  begin
    BytesRead := Stream.Read(Buffer, 4096);
    SendOk := (send(ClientSock, Buffer, BytesRead, 0) > 0);
  end;
end;

//------------------------------------------------------------------------------

procedure THTTPRequestThreadProcessor.ProcessPost(Path : string);
var i, ContentLen : Integer;
    Content : string;
    Start, Stop : Integer;
begin
  // 'Content-Type' must be application/x-www-form-urlencoded
  if (FRequestHeader.Values['content-type'] = 'application/x-www-form-urlencoded') then
  begin
    ContentLen := StrToIntDef(FRequestHeader.Values['content-length'],0);
    if (ContentLen > 0) then
    begin
      // Read rest of data
      SetLength(Content, ContentLen);
      recv(ClientSock, Content[1], ContentLen, 0);
      FPostVars.Delimiter := '&';
      FPostVars.DelimitedText := Content;
    end;
  end;
  // urldecode each field
  for i:=0 to FPostVars.Count-1 do
  begin
    FEnvVars.Values['POST['+FPostVars.Names[i]+']'] := FPostVars.ValueFromIndex[i];
    FPostVars.Strings[i] := URLDecode(FPostVars.Strings[i]);
  end;

  if (FPostVars.Values['vss-command'] = 'SUGGESTION') then
  begin
    ParseTimeHexa(FPostVars.Values['time-index'],Start,Stop);
    SuggestionForm.AddSuggestion(nil, Start, Stop, UTF8Decode(FPostVars.Values['text']));
    FEnvVars.Values['post-result'] := 'Suggestion sent successfully.';
  end
  else
    FEnvVars.Values['post-result'] := 'Unknown vss-command.';
end;

//------------------------------------------------------------------------------

function THTTPRequestThreadProcessor.ProcessVirtualWav(VirtualPath : string) : Boolean;
var s, StrStart, StrStop : string;
    i : Integer;
    Start, Stop : Integer;
    WAVFile : TWAVFile;
begin
  OutputDebugString('THTTPRequestThreadProcessor.ProcessVirtualWav');
  Result := False;
  s := Copy(VirtualPath, 6, Length(VirtualPath)-9);
  i := Pos('-',s);
  if (i > 0) then
  begin
    StrStart := '$' + Copy(s,1,i-1);
    StrStop := '$' + Copy(s,i+1,Length(s));
    Start := StrToIntDef(StrStart,-1);
    Stop := StrToIntDef(StrStop,-1);
    if (Start <> -1) and (Stop <> -1) or (Start < Stop) then
    begin
      WAVFile := TWAVFile.Create;
      g_WebRWSynchro.BeginRead;
      try
        s := g_GlobalContext.CurrentProject.WAVFile;
      finally
        g_WebRWSynchro.EndRead;
      end;
      if WAVFile.Open(s) then
      begin
        // TODO : send the wav directly on the socket
        WAVFile.ExtractToStream(Start,Stop,FAnswerStream);
        Result := True;
      end;
      WAVFile.Free;
    end
  end;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------

