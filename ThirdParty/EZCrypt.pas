unit EZCrypt;

{modeled by Ben Hochstrasser(bhoc@surfeu.ch) after some code snippet from borland}

interface

uses Windows, Classes;

type
  TWordTriple = Array[0..2] of Word;

function FileEncrypt(InFile, OutFile: String; Key: TWordTriple): boolean;
function FileDecrypt(InFile, OutFile: String; Key: TWordTriple): boolean;
function TextEncrypt(const s: string; Key: TWordTriple): string;
function TextDecrypt(const s: string; Key: TWordTriple): string;
function MemoryEncrypt(Src: Pointer; SrcSize: Cardinal; Target: Pointer; TargetSize: Cardinal; Key: TWordTriple): boolean;
function MemoryDecrypt(Src: Pointer; SrcSize: Cardinal; Target: Pointer; TargetSize: Cardinal; Key: TWordTriple): boolean;

implementation

function MemoryEncrypt(Src: Pointer; SrcSize: Cardinal; Target: Pointer; TargetSize: Cardinal; Key: TWordTriple): boolean;
var
  pIn, pOut: ^byte;
  i : Cardinal;
begin
  if SrcSize = TargetSize then
  begin
    pIn := Src;
    pOut := Target;
    for i := 1 to SrcSize do
    begin
      pOut^ := pIn^ xor (Key[2] shr 8);
      Key[2] := Byte(pIn^ + Key[2]) * Key[0] + Key[1];
      inc(pIn);
      inc(pOut);
    end;
    Result := True;
  end else
    Result := False;
end;

function MemoryDecrypt(Src: Pointer; SrcSize: Cardinal; Target: Pointer; TargetSize: Cardinal; Key: TWordTriple): boolean;
var
  pIn, pOut: ^byte;
  i : Cardinal;
begin
  if SrcSize = TargetSize then
  begin
    pIn := Src;
    pOut := Target;
    for i := 1 to SrcSize do
    begin
      pOut^ := pIn^ xor (Key[2] shr 8);
      Key[2] := byte(pOut^ + Key[2]) * Key[0] + Key[1];
      inc(pIn);
      inc(pOut);
    end;
    Result := True;
  end else
    Result := False;
end;

function TextCrypt(const s: string; Key: TWordTriple; Encrypt: Boolean): string;
var
  bOK: Boolean;
begin
  SetLength(Result, Length(s));
  if Encrypt then
    bOK := MemoryEncrypt(PChar(s), Length(s), PChar(Result), Length(Result), Key)
  else
    bOK := MemoryDecrypt(PChar(s), Length(s), PChar(Result), Length(Result), Key);
  if not bOK then Result := '';
end;

function FileCrypt(InFile, OutFile: String; Key: TWordTriple; Encrypt: Boolean): boolean;
var
  MIn, MOut: TMemoryStream;
begin
  MIn := TMemoryStream.Create;
  MOut := TMemoryStream.Create;
  Try
    MIn.LoadFromFile(InFile);
    MOut.SetSize(MIn.Size);
    if Encrypt then
      Result := MemoryEncrypt(MIn.Memory, MIn.Size, MOut.Memory, MOut.Size, Key)
    else
      Result := MemoryDecrypt(MIn.Memory, MIn.Size, MOut.Memory, MOut.Size, Key);
    MOut.SaveToFile(OutFile);
  finally
    MOut.Free;
    MIn.Free;
  end;
end;

function TextEncrypt(const s: string; Key: TWordTriple): string;
begin
  Result := TextCrypt(s, Key, True);
end;

function TextDecrypt(const s: string; Key: TWordTriple): string;
begin
  Result := TextCrypt(s, Key, False);
end;

function FileEncrypt(InFile, OutFile: String; Key: TWordTriple): boolean;
begin
  Result := FileCrypt(InFile, OutFile, Key, True);
end;

function FileDecrypt(InFile, OutFile: String; Key: TWordTriple): boolean;
begin
  Result := FileCrypt(InFile, OutFile, Key, False);
end;

end.