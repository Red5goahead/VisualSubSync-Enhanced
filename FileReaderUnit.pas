unit FileReaderUnit;

interface

uses Classes;

type
  TFileReader = class
  private
    FFileHandle : THandle;
  public
    constructor Create(const Filename : WideString);
    destructor Destroy; override;
    function Read(var Buffer; const Count: Longint) : Longint;
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;
    function Tell : Int64;
    function Size : Int64;
  end;

implementation

uses Windows, SysUtils;

// =============================================================================


constructor TFileReader.Create(const Filename : WideString);
begin
  FFileHandle := CreateFileW(PWideChar(Filename), GENERIC_READ,
    FILE_SHARE_READ+FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  if FFileHandle = INVALID_HANDLE_VALUE then
    raise Exception.CreateFmt('Can''t open file %s',[Filename]);
end;

// -----------------------------------------------------------------------------

function TFileReader.Read(var Buffer; const Count: Longint) : Longint;
var dwNumberOfBytesRead : DWORD;
begin
  Result := 0;
  dwNumberOfBytesRead := 0;
  if not ReadFile(FFileHandle, Buffer, Count, dwNumberOfBytesRead, nil) then
    Exit;
  Result := dwNumberOfBytesRead;
end;

// -----------------------------------------------------------------------------

function TFileReader.Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;
var
  LIOffset : LARGE_INTEGER;
begin
  LIOffset.QuadPart := Offset;
  LIOffset.LowPart := SetFilePointer (FFileHandle, LIOffset.LowPart,
    @LIOffset.HighPart, Ord(Origin));
  if ((LIOffset.LowPart = INVALID_HANDLE_VALUE) and (GetLastError <> NO_ERROR)) then
    LIOffset.QuadPart := -1;
  Result := LIOffset.QuadPart;
end;

// -----------------------------------------------------------------------------

function TFileReader.Tell : Int64;
var
  LIOffset : LARGE_INTEGER;
begin
  LIOffset.QuadPart := 0;
  LIOffset.LowPart := SetFilePointer (FFileHandle, 0,  @LIOffset.HighPart, FILE_CURRENT);
  if ((LIOffset.LowPart = INVALID_HANDLE_VALUE) and (GetLastError <> NO_ERROR)) then
    LIOffset.QuadPart := -1;
  Result := LIOffset.QuadPart;
end;

// -----------------------------------------------------------------------------

function TFileReader.Size : Int64;
var
  FileSize: array[0..1] of Cardinal absolute Result;
begin
  Result := -1;
  FileSize[0] := GetFileSize(FFileHandle, @FileSize[1]);
  if (FileSize[0] = INVALID_HANDLE_VALUE) then
    Result := -1;
end;

// -----------------------------------------------------------------------------

destructor TFileReader.Destroy;
begin
  if (FFileHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(FFileHandle);
  FFileHandle := INVALID_HANDLE_VALUE;
end;

// =============================================================================


end.
