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

unit PageProcessorUnit;

interface

uses Classes, GlobalUnit, IniFiles;

type
  TPageProcessor = class
  private
    FEnvVars : TStringList;
    FOutputStream : TStream;
    procedure ProcessField(Name : string; OutputStream : TStream; Context : TContext);
    procedure CopyAndReplaceField(InputStream : TStream;
      StartPos, StopPos : Integer; OutputStream : TStream;
      Context : TContext);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessPage(Filename : string; OutputStream : TStream;
      EnvVars : TStringList);
  end;

const
  START_FIELD_MARK : string = '$|';
  END_FIELD_MARK : string = '|$';
  START_FIELD_MARK_LEN : Integer = 2;
  END_FIELD_MARK_LEN : Integer = 2;
  
implementation

uses SysUtils, MiscToolsUnit, SubStructUnit;

const
  KnownKeywordLst : array[0..15] of string = (
    '$|version|$',
    '$|count|$',
    '$|index|$',
    '$|start|$',
    '$|stop|$',
    '$|text|$',
    '$$ FOREACH * $$',
    '$$ ENDFOREACH $$',
    '$|time-index|$',
    '$|html-text|$',
    '$|raw-text|$',
    '$|stripped-text|$',
    '$|html-text8|$',
    '$|raw-text8|$',
    '$|wav-size|$',
    '$|index-padded|$'
  );

//------------------------------------------------------------------------------

constructor TPageProcessor.Create;
begin
  FOutputStream := TMemoryStream.Create;
end;

//------------------------------------------------------------------------------

destructor TPageProcessor.Destroy;
begin
  FOutputStream.Free;
end;

//------------------------------------------------------------------------------

procedure TPageProcessor.ProcessField(Name : string; OutputStream : TStream; Context : TContext);
var s : string;
    ContextCursor : TContext;
    Found : Boolean; 
begin
  Found := False;
  ContextCursor := Context;
  while (ContextCursor <> nil) and (Length(s) = 0) and (Found <> True) do
  begin
    s := ContextCursor.GetFieldValue(Name, Found);
    ContextCursor := ContextCursor.Parent;
  end;
  if (Length(s) = 0) and (Found = False) then
  begin
    s := FEnvVars.Values[Name];
    if (Length(s) = 0) then
      s := START_FIELD_MARK + Name + END_FIELD_MARK;
  end;
  OutputStream.Write(s[1], Length(s));
end;

//------------------------------------------------------------------------------

procedure TPageProcessor.CopyAndReplaceField(InputStream : TStream;
  StartPos, StopPos : Integer; OutputStream : TStream; Context : TContext);
var
    Pos1, Pos2 : Integer;
    fieldname : string;
begin
  InputStream.Position := StartPos;
  Pos2 := StartPos;
  while (Pos2 < StopPos) do
  begin
    Pos1 := PosStreamEx(START_FIELD_MARK, InputStream, Pos2, StopPos);
    if (Pos1 <> -1) then
    begin
      InputStream.Seek(Pos2, soFromBeginning);
      OutputStream.CopyFrom(InputStream, Pos1-Pos2);
      Pos1 := Pos1 + START_FIELD_MARK_LEN;
    end
    else
    begin
      InputStream.Seek(Pos2, soFromBeginning);
      OutputStream.CopyFrom(InputStream, StopPos - Pos2);
      Break;
    end;
    // TODO : maybe we could limit search here
    Pos2 := PosStreamEx(END_FIELD_MARK, InputStream, Pos1, StopPos);
    if (Pos2 <> -1) then
    begin
      InputStream.Seek(Pos1, soFromBeginning);
      SetLength(fieldname, Pos2-Pos1);
      InputStream.Read(fieldname[1], Pos2-Pos1);
      Pos2 := Pos2 + END_FIELD_MARK_LEN;
      ProcessField(fieldname, OutputStream, Context);
    end
    else
    begin
      OutputStream.Write(START_FIELD_MARK, START_FIELD_MARK_LEN);
      Pos2 := Pos1;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageProcessor.ProcessPage(Filename : string; OutputStream : TStream;
  EnvVars : TStringList);
var InputStream : TMemoryStream;
    Pos1, Pos2 : Integer;
    BlockStart, BlockStop : Integer;
    blockname, blockend : string;
    i : integer;
    ErrorMsg : string;
    SubRangeWrapp : TSubtitleRangeWrapper;
begin
  FEnvVars := EnvVars;
  SubRangeWrapp := TSubtitleRangeWrapper.Create;
  SubRangeWrapp.Parent := g_GlobalContext;
  InputStream := TMemoryStream.Create;
  InputStream.LoadFromFile(Filename);
  InputStream.Seek(0,0);
  Pos2 := 0;
  while True do
  begin
    Pos1 := PosStream('<!--$TMPL_', InputStream, Pos2);
    if (Pos1 <> -1) then
    begin
      InputStream.Seek(Pos2, soFromBeginning);
      CopyAndReplaceField(InputStream, Pos2, Pos1, OutputStream, g_GlobalContext);
      Pos1 := Pos1 + Length('<!--$TMPL_');
    end
    else
    begin
      // Normal termination is here :)
      InputStream.Seek(Pos2, soFromBeginning);
      CopyAndReplaceField(InputStream, Pos2, InputStream.Size, OutputStream, g_GlobalContext);
      Break;
    end;
    Pos2 := PosStream('$-->', InputStream, Pos1);
    if (Pos2 <> -1) then
    begin
      InputStream.Seek(Pos1, soFromBeginning);
      SetLength(blockname, Pos2-Pos1);
      InputStream.Read(blockname[1], Pos2-Pos1);
      Pos2 := Pos2 + Length('$-->');
      // Read block
      blockend := '<!--$TMPL_'+ blockname + '_END$-->';
      Pos1 := PosStream(blockend, InputStream, Pos2);
      if (Pos1 <> -1) then
      begin
        BlockStart := Pos2;
        BlockStop := Pos1;
        if (blockname = 'SUBTITLES_LIST') then
        begin
          for i:=0 to g_GlobalContext.SubList.Count-1 do
          begin
            SubRangeWrapp.Index := i+1;
            SubRangeWrapp.SubRange := TSubtitleRange(g_GlobalContext.SubList[i]);
            CopyAndReplaceField(InputStream, BlockStart, BlockStop, OutputStream, SubRangeWrapp);
          end;
        end
        else if (blockname = 'SELECTED_SUB') then
        begin
          i := StrToIntDef(EnvVars.Values['idx'],0);
          Dec(i);
          if (i >= 0) and (i < g_GlobalContext.SubList.Count) then
          begin
            SubRangeWrapp.Index := i+1;
            SubRangeWrapp.SubRange := TSubtitleRange(g_GlobalContext.SubList[i]);
            CopyAndReplaceField(InputStream, BlockStart, BlockStop, OutputStream, SubRangeWrapp);
          end
          else
            CopyAndReplaceField(InputStream, BlockStart, BlockStop, OutputStream, g_GlobalContext);
        end
        else
        begin
          ErrorMsg := 'ERROR : Unkown block ' + blockname;
          OutputStream.Write(ErrorMsg[1], Length(ErrorMsg));
          Break;
        end;
        Pos1 := Pos1 + Length(blockend);
        Pos2 := Pos1;
      end
      else
      begin
        // blockend for blockname at Pos2 not found
        ErrorMsg := 'ERROR : "'+ blockend + '" not found for block "' + blockname +
          '" at pos ' + IntToStr(Pos2);
        OutputStream.Write(ErrorMsg[1], Length(ErrorMsg));
        Break;
      end;
    end
    else
    begin
      // end of '<!--$TMPL_' at Pos1 not found
      ErrorMsg := 'ERROR : End of "<!--$TMPL_" not found at pos ' + IntToStr(Pos1);
      OutputStream.Write(ErrorMsg[1], Length(ErrorMsg));
      Break;
    end;
  end;
  InputStream.Free;
  SubRangeWrapp.Free;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
