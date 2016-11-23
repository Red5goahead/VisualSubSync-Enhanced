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

unit DynamicPageProcessorUnit;

interface

uses Classes, Windows, Contnrs;

type
  // Order must be the same as in KeywordLst
  TKeywordId = (kIdVersion, kIdCount, kIdIndex, kIdStart, kIdStop, kIdText,
    kIdForEachBegin, kIdForEachEnd,
    // Other type not in KeywordLst at end
    kIdData);

const
  KeywordLst : array[0..7] of string = (
    '$|version|$',
    '$|count|$',
    '$|index|$',
    '$|start|$',
    '$|stop|$',
    '$|text|$',
    '$$ FOREACH * $$',
    '$$ ENDFOREACH $$'
  );

type
  TInstructionNode = class
    Keyword : string;
    Id : TKeywordId;
    Instructions : TList;
    destructor Destroy; override;
    procedure Execute(OutStream : TStream); virtual;
    procedure AddInst(InstNode : TInstructionNode);
  end;

  TDataNode = class(TInstructionNode)
    Data : string;  
    constructor Create(Data : string);
    procedure Execute(OutStream : TStream); override;
  end;

  TForEachNode = class(TInstructionNode)
    procedure Execute(OutStream : TStream); override;  
  end;

  TDynamicPageProcessor = class
  private
    FOutputStream : TMemoryStream;
    FStack : TStack;
    FInstructionTree : TInstructionNode;

    procedure CreateInstructionTree(InputStream : TStream);
    procedure AddInstruction(DataNode : TDataNode; Instruction : TInstructionNode);
    procedure ExecuteTree(InstructionTree : TInstructionNode; OutputStream : TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessPage(Filename : string);
    function GetOutputStream : TStream;
  end;

//==============================================================================

implementation

procedure TInstructionNode.Execute(OutStream : TStream);
begin
  OutStream.Write(Keyword[1],Length(Keyword));
end;

destructor TInstructionNode.Destroy;
var i : integer;
begin
  if Assigned(Instructions) then
  begin
    for i:=0 to Instructions.Count-1 do
    begin
      TObject(Instructions[i]).Free;
    end;
    Instructions.Free;
  end;
end;

procedure TInstructionNode.AddInst(InstNode : TInstructionNode);
begin
  if not Assigned(Instructions) then
    Instructions := TList.Create;
  Instructions.Add(InstNode);
end;

function CreateInstruction(Keyword : string; Id : TKeywordId) : TInstructionNode;
begin
  if (Id = kIdForEachBegin) then
    Result := TForEachNode.Create
  else
    Result := TInstructionNode.Create;
  Result.Keyword := Keyword;
  Result.Id := Id;
end;

constructor TDataNode.Create(Data : string);
begin
  inherited Create;
  Self.Data := Data;
  Id := kIdData;
end;

procedure TDataNode.Execute(OutStream : TStream);
begin
  OutStream.Write(Data[1],Length(Data));
end;

procedure TForEachNode.Execute(OutStream : TStream);
var i,j : integer;
begin
  if Assigned(Instructions) then
  begin
    for i := 0 to 10 do
    begin
      for j := 0 to Instructions.Count-1 do
      begin
        TInstructionNode(Instructions[j]).Execute(OutStream);
      end;
    end;
  end;
end;

//==============================================================================

constructor TDynamicPageProcessor.Create;
begin
  inherited;
  FOutputStream := TMemoryStream.Create;
  FStack := TStack.Create;
  FInstructionTree := TInstructionNode.Create;
end;

//------------------------------------------------------------------------------

destructor TDynamicPageProcessor.Destroy;
begin
  FOutputStream.Free;
  FStack.Free;
  FInstructionTree.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDynamicPageProcessor.ProcessPage(Filename : string);
var InputStream : TMemoryStream;
begin
  InputStream := TMemoryStream.Create;
  InputStream.LoadFromFile(Filename);
  InputStream.Seek(0,0);
  CreateInstructionTree(InputStream);
  ExecuteTree(FInstructionTree,FOutputStream);
  InputStream.Free;
end;

//------------------------------------------------------------------------------

function IsKeyword(Keyword : string; var OnlyStart : Boolean; var Instruction : TInstructionNode) : Boolean;
var
  i,j,k : Integer;
begin
  Result := False;
  OnlyStart := False;
  for i:=0 to Length(KeywordLst)-1 do
  begin
    j := 1;
    k := 1;
    while (k <= Length(Keyword)) do
    begin
      if (KeywordLst[i][j] = '*') then
      begin
        Inc(j);
        while (k <= Length(Keyword)) and (Keyword[k] <> KeywordLst[i][j]) do
          Inc(k);
      end
      else if (Keyword[k] = KeywordLst[i][j]) then
      begin
        Inc(k);
        Inc(j);
      end
      else
        Break;
    end;
    if (k > Length(Keyword)) then
    begin
      Result := True;
      OnlyStart := not (Result and (j > Length(KeywordLst[i])));
      if (not OnlyStart) then
      begin
        // Create
        Instruction := CreateInstruction(Keyword, TKeywordId(i));
      end;
      Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TDynamicPageProcessor.CreateInstructionTree(InputStream : TStream);
var OnKeyword : Boolean;
    Keyword : string;
    Data : string;
    OnlyStart : Boolean;
    c : Char;
    Instruction : TInstructionNode;
    DataNode : TDataNode;
begin
  OnKeyword := False;
  while (InputStream.Read(c,1) = 1) do
  begin
    if (c = '$') then
      OnKeyword := True;
    if OnKeyword then
    begin
      Keyword := Keyword + c;
      if IsKeyword(Keyword,OnlyStart,Instruction) then
      begin
        if OnlyStart then
        begin
          // We continue
        end
        else
        begin
          // We have a Keyword
          DataNode := TDataNode.Create(Data);
          AddInstruction(DataNode,Instruction);
          Data := '';
          Keyword := '';
          OnKeyword := False;
        end;
      end
      else
      begin
        // This was not a Keyword
        OnKeyword := False;
        Data := Data + Keyword[1];
        InputStream.Seek(-(Length(Keyword)-1),soFromCurrent);
        Keyword := '';
      end;
    end
    else
      Data := Data + c;
  end;
end;

//------------------------------------------------------------------------------

procedure TDynamicPageProcessor.AddInstruction(DataNode : TDataNode; Instruction : TInstructionNode);
begin
  if (Instruction.Id = kIdForEachBegin) then
  begin
    if (FStack.Count > 0) then
    begin
      TInstructionNode(FStack.Peek).AddInst(DataNode);
      TInstructionNode(FStack.Peek).AddInst(Instruction);
    end
    else
    begin
      FInstructionTree.AddInst(DataNode);
      FInstructionTree.AddInst(Instruction);
    end;
    FStack.Push(Instruction);
  end
  else if (Instruction.Id = kIdForEachEnd) then
  begin
    TInstructionNode(FStack.Pop).AddInst(DataNode);
    Instruction.Free;
  end
  else
  begin
    if (FStack.Count > 0) then
    begin
      TInstructionNode(FStack.Peek).AddInst(DataNode);
      TInstructionNode(FStack.Peek).AddInst(Instruction);
    end
    else
    begin
      FInstructionTree.AddInst(DataNode);
      FInstructionTree.AddInst(Instruction);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDynamicPageProcessor.ExecuteTree(InstructionTree : TInstructionNode; OutputStream : TStream);
var i : integer;
    InstructionNode : TInstructionNode;
begin
  if Assigned(InstructionTree.Instructions) then
  begin
    for i:=0 to InstructionTree.Instructions.Count-1 do
    begin
      InstructionNode := InstructionTree.Instructions[i];
      InstructionNode.Execute(OutputStream);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDynamicPageProcessor.GetOutputStream : TStream;
begin
  Result := FOutputStream;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
