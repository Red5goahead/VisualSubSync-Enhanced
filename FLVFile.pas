{FLV_parser

this is a basic file parser that only extract keyframes.

written by nixxo
c.nixxo@hush.ai
}

unit FLVFile;

interface        

uses Windows, SysUtils, Classes, StrUtils;

type
  FLV = class
  private
    flv_file : THandle;
    flv_fp : Integer;

    flv_header : array[1..9] of byte;
    flv_tag : array[1..11] of byte;
    type_v:Boolean;
    tag_time : Integer;
    flv_next : Integer;

    procedure parseTag;
    function isKeyframe : Boolean;
  public
    constructor Create(filename : WideString);
    procedure Free;
    function isValid : Boolean;
    function nextKeyframe : Integer;
  end;

implementation

constructor FLV.Create(filename : WideString);
begin
  flv_file := CreateFileW(PWideChar(filename), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
end;

procedure FLV.Free;
begin
  if (flv_file <> INVALID_HANDLE_VALUE) then
    CloseHandle(flv_file);
  flv_file := INVALID_HANDLE_VALUE;
end;

function FLV.isValid : Boolean; 
var
  dwNumberOfBytesRead : DWORD;
begin
  Result:=False;
  if (flv_file <> INVALID_HANDLE_VALUE) then
  begin
    SetFilePointer(flv_file, 0, nil, 0);
    if not ReadFile(flv_file, flv_header, 9, dwNumberOfBytesRead, nil) then
      Exit;
    if (dwNumberOfBytesRead = 9) then
    begin
      if (flv_header[1] = 70) and
         (flv_header[2] = 76) and
         (flv_header[3] = 86) then
        Result:=True;
      flv_next := 13;
    end;
  end;
end;

procedure FLV.parseTag;
var bodylength : Integer;
  dwNumberOfBytesRead : DWORD;
begin
  flv_next :=-1;
  if (flv_file <> INVALID_HANDLE_VALUE) then
  begin
    SetFilePointer(flv_file, flv_fp, nil, 0);
    if not ReadFile(flv_file, flv_tag, 11, dwNumberOfBytesRead, nil) then
      Exit;
    if (dwNumberOfBytesRead = 11) then
    begin
      type_v:=False;
      if flv_tag[1] = 9 then
      begin
        type_v := True;
        tag_time := flv_tag[5];
        tag_time := tag_time*256 + flv_tag[6];
        tag_time := tag_time*256 + flv_tag[7];
      end;

      bodylength := flv_tag[2];
      bodylength := bodylength*256 + flv_tag[3];
      bodylength := bodylength*256 + flv_tag[4];
      flv_next := flv_fp+11+bodylength+4;
    end;
  end;
end;

function FLV.isKeyframe : Boolean;
var temp : Byte;
  dwNumberOfBytesRead : DWORD;
begin
  Result := False;
  if (flv_file <> INVALID_HANDLE_VALUE) then
  begin
    SetFilePointer(flv_file, flv_fp+11, nil, 0);
    if not ReadFile(flv_file, temp, 1, dwNumberOfBytesRead, nil) then
      Exit;
    if (dwNumberOfBytesRead = 1) then
    begin
      temp := (temp and $00FF) shr 4;
      if temp = 1 then
        Result:= True;
    end;
  end;
end;

function FLV.nextKeyframe : Integer;
begin
  flv_fp := flv_next;
  parseTag;
  if type_v then
    if isKeyframe then
    begin
      Result := tag_time;
      Exit;
    end;
  if flv_next > 0 then
    Result := nextKeyframe
  else
    Result := -1;
end;

end.
