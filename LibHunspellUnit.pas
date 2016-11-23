// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003-2008 Christophe Paris
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
unit LibHunspellUnit;

interface

uses Windows, SysUtils, TntClasses, TntComCtrls, SyncObjs, DBClient, DB;

// -----------------------------------------------------------------------------

const
  HUNSPELL_DLL = 'libhunspell.dll';

type
  THunspell = THandle;

{* create *}
function Hunspell_create(const affpath, dpath : PAnsiChar) : THunspell; cdecl; external HUNSPELL_DLL;

{* create_key *}
function Hunspell_create_key(const affpath, dpath, key : PAnsiChar) : THunspell; cdecl; external HUNSPELL_DLL;

{* destroy *}
procedure Hunspell_destroy(pHunspell : THunspell); cdecl; external HUNSPELL_DLL;

{* spell(word) - spellcheck word
 * output: 0 = bad word, not 0 = good word
 *}
function Hunspell_spell(pHunspell : THunspell; word : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* get_dic_encoding *}
function Hunspell_get_dic_encoding(pHunspell : THunspell) : PAnsiChar; cdecl; external HUNSPELL_DLL;

{* suggest(suggestions, word) - search suggestions
 * input: pointer to an array of strings pointer and the (bad) word
 *   array of strings pointer (here *slst) may not be initialized
 * output: number of suggestions in string array, and suggestions in
 *   a newly allocated array of strings (*slts will be NULL when number
 *   of suggestion equals 0.)
 *}
function Hunspell_suggest(pHunspell : THunspell; var slst : PPAnsiChar; const word : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* analyze(result, word) - morphological analysis of the word *}
function Hunspell_analyze(pHunspell : THunspell; var slst : PPAnsiChar; const word : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* stem(result, word) - stemmer function *}
function Hunspell_stem(pHunspell : THunspell; var slst : PPAnsiChar; const word : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* generate(result, word, word2) - morphological generation by example(s) *}
function Hunspell_generate(pHunspell : THunspell; var slst : PPAnsiChar; const word, word2 : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* add word to the run-time dictionary *}
function Hunspell_add(pHunspell : THunspell; const word : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* add word to the run-time dictionary with affix flags of
 * the example (a dictionary word): Hunspell will recognize
 * affixed forms of the new word, too.
 *}
function Hunspell_add_with_affix(pHunspell : THunspell; const word, example : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* remove word from the run-time dictionary *}
function Hunspell_remove(pHunspell : THunspell; const word : PAnsiChar) : Integer; cdecl; external HUNSPELL_DLL;

{* free suggestion lists *}
function Hunspell_free_list(pHunspell : THunspell; var slst : PPAnsiChar; const n : Integer) : Integer; cdecl; external HUNSPELL_DLL;

// -----------------------------------------------------------------------------

type
  THunspellChecker = class
  private
    FHunspell : THunspell;
    FDictEncoding : string;
    FDictCodePage : Cardinal;
    FDictList : TTntStringList;
    FDictListPath : WideString;
    FInitCC : TCriticalSection;
    FCurrentDictName : WideString;
    FThesaurusDict : Boolean;
    FCurrentThesaurusDictName : WideString;
    FThesaurusDbWord : TClientDataSet;
    FThesaurusDbMeanings : TClientDataSet;
    FThesaurusDbSynonyms : TClientDataSet;
    FThesaurusDb : TClientDataSet;
    FIgnoreList : TTntStringList;
    FAddList : TTntStringList;

    procedure LoadAddListInHunspell;

  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(DictAff, DictDic : WideString) : Boolean; overload;
    function Initialize(DictIndex : Integer) : Boolean; overload;
    function IsInitialized : Boolean;
    procedure Cleanup;
    function Spell(Word : WideString) : Boolean;
    function Suggest(Word : WideString; Suggestions : TTntStrings) : Integer;
    function Analyze(Word : WideString; Suggestions : TTntStrings) : Integer;
    function Stem(Word : WideString; Suggestions : TTntStrings) : Integer;

    procedure Add(Word : WideString);
    procedure Remove(Word : WideString);

    function FindDict(Path : WideString) : Integer;
    procedure GetDict(List : TTntStrings); overload;
    function GetDictCount : Integer;
    function GetDict(Index : Integer) : WideString; overload;
    function GetDictIdx(Name : WideString) : Integer; overload;

    function GetCurrentDictName : WideString;

    procedure Ignore(Word : WideString);
    procedure LoadPersonalDict(Filename : WideString);
    procedure SavePersonalDict(Filename : WideString);

    Function IsThesaurusInitialized:Boolean;
    Function GetTotalMeanings(Word : string): Integer;
    Function GetTotalSynonymsByMeaningIndex(Word : string; MeaningIndex : Integer): Integer;
    Function GetMeaning(Word : string; MeaningIndex : Integer): String;
    Function GetSynonym(Word : string; MeaningIndex : Integer; SynonymIndex : Integer): String;

  end;

  TWordInfo = record
    Position : Integer;
    Length : Integer;
  end;

// -----------------------------------------------------------------------------

function IsWordDelimiter(wc : WideChar) : Boolean;
procedure SkipDelimiters(var Index : Integer; Text : WideString);
function GetNextWordPos(var Index : Integer; Text : WideString; var WordInfo : TWordInfo) : Boolean;
function ContainDigit(Text : WideString) : Boolean;
function IsUpperCase(Text : WideString) : Boolean;
function GetWordAt(CursorIndex : Integer; Text : WideString; var WordInfo : TWordInfo) : Boolean; overload;
function GetWordAt(RichEdit : TTntRichEdit; X,Y : Integer; var WordInfo : TWordInfo) : Boolean; overload;

// -----------------------------------------------------------------------------


implementation

uses TntSysUtils, Types, Messages, TntWideStrings, StrUtils;

// -----------------------------------------------------------------------------

function IsWordDelimiter(wc : WideChar) : Boolean;
const WordDelimiters : array [0..39] of WideChar = (
  ' ', '.', ',', '?', '!', ':', ';', '(', ')', '[', ']', '{', '}',
  #13, #10, #9, '"', '/', '\', '|', '+', '-', '*', '=', '%', '&', '>', '<', '¡', '¿',
  #160, #8239,              // Non-breaking spaces
  #8230,                    // Suspension points
  #8216, //#8217,             // Single quotes
  #171, #187, #8220, #8221, // Double quotes
  #8211, #8212              // Dashes
  );
var i : Integer;
begin
  for i := Low(WordDelimiters) to High(WordDelimiters) do
  begin
    if (wc = WordDelimiters[i]) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure SkipDelimiters(var Index : Integer; Text : WideString);
begin
  while (Index <= Length(Text)) and IsWordDelimiter(Text[Index]) do
    Inc(Index);
end;

function GetNextWordPos(var Index : Integer; Text : WideString; var WordInfo : TWordInfo) : Boolean;
begin
  SkipDelimiters(Index, Text);
  WordInfo.Position := Index;
  while (Index <= Length(Text)) and (not IsWordDelimiter(Text[Index])) do
  begin
    Inc(Index);
  end;
  WordInfo.Length := Index - WordInfo.Position;
  Result := (WordInfo.Length > 0);
end;

function ContainDigit(Text : WideString) : Boolean;
var i : Integer;
begin
  for i := 1 to Length(Text) do
  begin
    if IsWideCharDigit(Text[i]) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function IsUpperCase(Text : WideString) : Boolean;
var i : Integer;
begin
  for i := 1 to Length(Text) do
  begin
    if not IsWideCharUpper(Text[i]) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function GetWordAt(CursorIndex : Integer; Text : WideString; var WordInfo : TWordInfo) : Boolean;
var Index : Integer;
begin
  // Look backward
  Index := CursorIndex - 1;
  while (Index > 0) and (not IsWordDelimiter(Text[Index])) do
  begin
    Dec(Index);
  end;
  WordInfo.Position := Index + 1;
  // Look forward
  Index := CursorIndex;
  while (Index <= Length(Text)) and (not IsWordDelimiter(Text[Index])) do
  begin
    Inc(Index);
  end;
  WordInfo.Length := Index - WordInfo.Position;
  Result := (WordInfo.Length > 0);
end;

function GetWordAt(RichEdit : TTntRichEdit; X,Y : Integer; var WordInfo : TWordInfo) : Boolean;
var CharIndex : Integer;
    P : TPoint;
begin
  P := RichEdit.ScreenToClient(Point(X,Y));
  CharIndex := RichEdit.Perform(EM_CHARFROMPOS, 0, longint(@P));
  CharIndex := RichEdit.EmulatedCharPos(CharIndex);
  Result := GetWordAt(CharIndex + 1, RichEdit.Text, WordInfo);
end;

// =============================================================================

type
 TDicoEncodingItem = record
   Name : string;
   Code : Cardinal;
 end;

const
   DicoEncodings : array[1..17] of TDicoEncodingItem =
   (
     (Name : 'ISO8859-1'; Code : 28591),
     (Name : 'ISO8859-2'; Code : 28592),
     (Name : 'ISO8859-3'; Code : 28593),
     (Name : 'ISO8859-4'; Code : 28594),
     (Name : 'ISO8859-5'; Code : 28595),
     (Name : 'ISO8859-6'; Code : 28596),
     (Name : 'ISO8859-7'; Code : 28597),
     (Name : 'ISO8859-8'; Code : 28598),
     (Name : 'ISO8859-9'; Code : 28599),
     (Name : 'ISO8859-10'; Code : 28600),
     (Name : 'ISO8859-13'; Code : 28603),
     (Name : 'ISO8859-14'; Code : 28604),
     (Name : 'ISO8859-15'; Code : 28605),
     (Name : 'KOI8-R'; Code : 20866),
     (Name : 'KOI8-U'; Code : 21866),
     (Name : 'microsoft-cp1251'; Code : 1251),
     (Name : 'ISCII-DEVANAGARI'; Code : 57002)
   );

function GetDicoCodePage(DicoEncodingName : string) : Integer;
var i : Integer;
    DicoEncodingItem : TDicoEncodingItem;
begin
  for i := Low(DicoEncodings) to High(DicoEncodings) do
  begin
    DicoEncodingItem := DicoEncodings[i];
    if (DicoEncodingItem.Name = DicoEncodingName) then
    begin
      Result := DicoEncodingItem.Code;
      Exit;
    end;
  end;
  // Use UTF-8 as default
  Result := CP_UTF8;
end;

function ConvertToHunspell(Text: WideString; CodePage : Cardinal) : string;
var Len : Integer;
begin
  Len := WideCharToMultiByte(CodePage, 0, PWideChar(Text), Length(Text),
    nil, 0, nil, nil);
  SetLength(Result, Len);
  WideCharToMultiByte(CodePage, 0, PWideChar(Text), Length(Text),
    @Result[1], Len, nil, nil);
end;

function ConvertFromHunspell(Text: string; CodePage : Cardinal) : WideString;
var Len : Integer;
begin
  Len := MultiByteToWideChar(CodePage, 0, PChar(Text), Length(Text), nil, 0);
  SetLength(Result, Len);
  MultiByteToWideChar(CodePage, 0, PChar(Text), Length(Text),
    @Result[1], Len);
end;

// =============================================================================

constructor THunspellChecker.Create;
begin
  FDictList := TTntStringList.Create;
  FIgnoreList := TTntStringList.Create;
  FAddList := TTntStringList.Create;
  FInitCC := TCriticalSection.Create;
  FCurrentDictName := '';

  FThesaurusDict := False;
  FCurrentThesaurusDictName := '';
  FThesaurusDbWord := TClientDataSet.Create(nil);
  FThesaurusDbMeanings := TClientDataSet.Create(nil);
  FThesaurusDbSynonyms := TClientDataSet.Create(nil);

  FHunspell := 0;
end;

// -----------------------------------------------------------------------------

function THunspellChecker.Initialize(DictAff, DictDic : WideString) : Boolean;
var
  ThesaurusIndex, ThesaurusNumOfMeanings, ThesaurusMeaningsIndex, ThesaurusDbIndex, IndexSplitObj: Integer;
  ThesaurusWord, ThesaurusMeaning, ThesaurusSynonyms : String;
  FThesaurus : TTntStringList; SplitObj : TTntStringList;
begin
  Cleanup;
  FInitCC.Acquire;
  try
    // TODO : Check for unicode filename support
    FHunspell := Hunspell_create(PAnsiChar(string(DictAff)),
      PAnsiChar(string((DictDic))));
    if (FHunspell <> 0) then
    begin
      FDictEncoding := Hunspell_get_dic_encoding(FHunspell);
      FDictCodePage := GetDicoCodePage(FDictEncoding);
      FCurrentDictName := WideExtractFileName(DictDic);
      FCurrentThesaurusDictName := 'th_'+AnsiReplaceStr(FCurrentDictName, WideExtractFileExt(FCurrentDictName), '')+'_v2';
      if (FileExists(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'.dat') = True) AND
         (FileExists(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'.idx') = True) then
      begin
        if (FileExists(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'w.xml') = False) or
           (FileExists(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'m.xml') = False) or
           (FileExists(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'s.xml') = False) Then
        begin
          FThesaurusDbWord.FieldDefs.Add('Word', ftString, 80, True);
          FThesaurusDbWord.FieldDefs.Add('Id', ftInteger, 0, True);
          FThesaurusDbWord.FieldDefs.Add('Meanings', ftInteger, 0, True);
          FThesaurusDbWord.IndexDefs.Add('Word', 'Word', [ixPrimary, ixUnique]);
          FThesaurusDbWord.CreateDataSet;
          FThesaurusDbWord.LogChanges:=False;
          FThesaurusDbMeanings.FieldDefs.Add('Id', ftInteger, 0, True);
          FThesaurusDbMeanings.FieldDefs.Add('MeaningIndex', ftInteger, 0, True);
          FThesaurusDbMeanings.FieldDefs.Add('Meaning', ftString, 80, True);
          FThesaurusDbMeanings.FieldDefs.Add('Synonyms', ftInteger, 0, True);
          FThesaurusDbMeanings.IndexDefs.Add('MeaningIndex', 'Id;MeaningIndex', [ixPrimary, ixUnique]);
          FThesaurusDbMeanings.CreateDataSet;
          FThesaurusDbMeanings.LogChanges:=False;
          FThesaurusDbSynonyms.FieldDefs.Add('Id', ftInteger, 0, True);
          FThesaurusDbSynonyms.FieldDefs.Add('MeaningIndex', ftInteger, 0, True);
          FThesaurusDbSynonyms.FieldDefs.Add('SynonymIndex', ftInteger, 0, True);
          FThesaurusDbSynonyms.FieldDefs.Add('Synonym', ftString, 80, True);
          FThesaurusDbSynonyms.IndexDefs.Add('SynonymIndex', 'Id;MeaningIndex;SynonymIndex', [ixPrimary, ixUnique]);
          FThesaurusDbSynonyms.CreateDataSet;
          FThesaurusDbSynonyms.LogChanges:=False;

          FThesaurus := TTntStringList.Create;
          FThesaurus.LoadFromFile(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'.dat');
          FThesaurusDbWord.EmptyDataSet;
          FThesaurusDbMeanings.EmptyDataSet;
          SplitObj := TTntStringList.Create;
          ThesaurusIndex := 1; ThesaurusDbIndex := 0;
          While ThesaurusIndex <= FThesaurus.Count-1 do
           begin
            ThesaurusWord := Copy(FThesaurus[ThesaurusIndex],1,AnsiPos('|',FThesaurus[ThesaurusIndex])-1);
            ThesaurusNumOfMeanings := StrToInt(Copy(FThesaurus[ThesaurusIndex],AnsiPos('|',FThesaurus[ThesaurusIndex])+1,Length(FThesaurus[ThesaurusIndex])-AnsiPos('|',FThesaurus[ThesaurusIndex])+1));
            ThesaurusDbIndex := ThesaurusDbIndex+1;
            FThesaurusDbWord.Append;
            FThesaurusDbWord.FieldByName('Word').AsString:=ThesaurusWord;
            FThesaurusDbWord.FieldByName('Id').AsInteger:=ThesaurusDbIndex;
            FThesaurusDbWord.FieldByName('Meanings').AsInteger:=ThesaurusNumOfMeanings;
            FThesaurusDbWord.Post;
            ThesaurusMeaningsIndex := 1;
            While ThesaurusMeaningsIndex <= ThesaurusNumOfMeanings do
            begin
             SplitObj.Clear;
             SplitObj.Delimiter := '|';
             SplitObj.DelimitedText := AnsiReplaceStr(FThesaurus[ThesaurusIndex+ThesaurusMeaningsIndex],chr(32),'_');
             ThesaurusMeaning := AnsiReplaceStr(Copy(SplitObj[0],1,Length(SplitObj[0])),'_', chr(32));

             FThesaurusDbMeanings.Append;
             FThesaurusDbMeanings.FieldByName('Id').AsInteger:=ThesaurusDbIndex;
             FThesaurusDbMeanings.FieldByName('MeaningIndex').AsInteger:=ThesaurusMeaningsIndex;
             FThesaurusDbMeanings.FieldByName('Meaning').AsString:=ThesaurusMeaning;
             FThesaurusDbMeanings.FieldByName('Synonyms').AsInteger:=SplitObj.Count-1;
             FThesaurusDbMeanings.Post;

             for IndexSplitObj:=1 to SplitObj.Count-1 do
             begin
              FThesaurusDbSynonyms.Append;
              FThesaurusDbSynonyms.FieldByName('Id').AsInteger:=ThesaurusDbIndex;
              FThesaurusDbSynonyms.FieldByName('MeaningIndex').AsInteger:=ThesaurusMeaningsIndex;
              FThesaurusDbSynonyms.FieldByName('SynonymIndex').AsInteger:=IndexSplitObj;
              FThesaurusDbSynonyms.FieldByName('Synonym').AsString:=AnsiReplaceStr(SplitObj[IndexSplitObj],'_',chr(32));
              FThesaurusDbSynonyms.Post;
             end;

             ThesaurusMeaningsIndex := ThesaurusMeaningsIndex + 1;
            end;

            ThesaurusIndex := ThesaurusIndex + ThesaurusNumOfMeanings + 1;
           end;
          SplitObj.Destroy;
          FThesaurus.Destroy;
          FThesaurusDict := False;
          if FThesaurusDbWord.RecordCount>0 then FThesaurusDict := True;
          FThesaurusDbWord.SaveToFile(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'w.xml', dfXMLUTF8);
          FThesaurusDbMeanings.SaveToFile(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'m.xml', dfXMLUTF8);
          FThesaurusDbSynonyms.SaveToFile(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'s.xml', dfXMLUTF8);
        end
        else
        begin
         FThesaurusDbWord.LoadFromFile(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'w.xml');
         if FThesaurusDbWord.IndexDefs.IndexOf('Word') <> -1 then
           FThesaurusDbWord.IndexDefs.Delete(FThesaurusDbWord.IndexDefs.IndexOf('Word'));
         FThesaurusDbWord.IndexDefs.Add('Word', 'Word', [ixPrimary, ixUnique]);
         FThesaurusDbMeanings.LoadFromFile(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'m.xml');
         if FThesaurusDbMeanings.IndexDefs.IndexOf('MeaningIndex') <> -1 then
           FThesaurusDbMeanings.IndexDefs.Delete(FThesaurusDbWord.IndexDefs.IndexOf('Word'));
         FThesaurusDbMeanings.IndexDefs.Add('MeaningIndex', 'Id;MeaningIndex', [ixPrimary, ixUnique]);
         FThesaurusDbSynonyms.LoadFromFile(WideExtractFileDir(DictDic)+'\'+FCurrentThesaurusDictName+'s.xml');
         if FThesaurusDbSynonyms.IndexDefs.IndexOf('SynonymIndex') <> -1 then
           FThesaurusDbSynonyms.IndexDefs.Delete(FThesaurusDbWord.IndexDefs.IndexOf('Word'));
         FThesaurusDbSynonyms.IndexDefs.Add('SynonymIndex', 'Id;MeaningIndex;SynonymIndex', [ixPrimary, ixUnique]);
         if FThesaurusDbWord.RecordCount>0 then FThesaurusDict := True;
        end;
      end;
      LoadAddListInHunspell;
    end;
    Result := (FHunspell <> 0);
  finally
    FInitCC.Release;
  end;
end;

function THunspellChecker.Initialize(DictIndex : Integer) : Boolean;
var DictAff, DictDic : WideString;
begin
  if (DictIndex >= 0) and (DictIndex < FDictList.Count) then
  begin
    DictDic := FDictListPath + FDictList.Strings[DictIndex];
    DictAff := WideChangeFileExt(DictDic, '.aff');
    Result := Initialize(DictAff, DictDic);
  end
  else
    Result := False;
end;

function THunspellChecker.IsInitialized : Boolean;
begin
  FInitCC.Acquire;
  try
    Result := (FHunspell <> 0);
  finally
    FInitCC.Release;
  end;
end;

procedure THunspellChecker.Cleanup;
begin
  FInitCC.Acquire;
  try
    if (FHunspell <> 0) then
    begin
      Hunspell_destroy(FHunspell);
      FHunspell := 0;
      FCurrentDictName := '';
    end;
  finally
    FInitCC.Release;
  end;
end;

destructor THunspellChecker.Destroy;
begin
  Cleanup;
  FreeAndNil(FThesaurusDbWord);
  FreeAndNil(FThesaurusDbMeanings);
  FreeAndNil(FThesaurusDbSynonyms);
  FreeAndNil(FInitCC);
  FreeAndNil(FIgnoreList);
  FreeAndNil(FDictList);
  FreeAndNil(FAddList);
end;

// -----------------------------------------------------------------------------

function THunspellChecker.Spell(Word : WideString) : Boolean;
var WordEncoded : string;
begin
  Word := Tnt_WideStringReplace(Word, #8217, #39, [rfReplaceAll]);  // typographic apostrophe to vertical apostrophe
  if (FIgnoreList.IndexOf(Word) <> -1) then
  begin
    Result := True;
    Exit;
  end;
  WordEncoded := ConvertToHunspell(Word, FDictCodePage);
  Result := Hunspell_spell(FHunspell, PAnsiChar(WordEncoded)) <> 0;
end;

function THunspellChecker.Suggest(Word : WideString; Suggestions : TTntStrings) : Integer;
var slst, slstCursor: PPAnsiChar;
var i : Integer;
var WordEncoded : string;
begin
  WordEncoded := ConvertToHunspell(Word, FDictCodePage);
  Result := Hunspell_suggest(FHunspell, slst, PChar(WordEncoded));
  if (Result > 0) then
  begin
    slstCursor := slst;
    for i := 1 to Result do
    begin
      Suggestions.Add(ConvertFromHunspell(slstCursor^, FDictCodePage));
      Inc(Integer(slstCursor), sizeOf(Pointer));
    end;
    Hunspell_free_list(FHunspell, slst, Result);
  end;
end;

function THunspellChecker.Analyze(Word : WideString; Suggestions : TTntStrings) : Integer;
var slst, slstCursor: PPAnsiChar;
var i : Integer;
var WordEncoded : string;
begin
  WordEncoded := ConvertToHunspell(Word, FDictCodePage);
  Result := Hunspell_analyze(FHunspell, slst, PChar(WordEncoded));
  if (Result > 0) then
  begin
    slstCursor := slst;
    for i := 1 to Result do
    begin
      Suggestions.Add(ConvertFromHunspell(slstCursor^, FDictCodePage));
      Inc(Integer(slstCursor), sizeOf(Pointer));
    end;
    Hunspell_free_list(FHunspell, slst, Result);
  end;
end;

function THunspellChecker.Stem(Word : WideString; Suggestions : TTntStrings) : Integer;
var slst, slstCursor: PPAnsiChar;
var i : Integer;
var WordEncoded : string;
begin
  WordEncoded := ConvertToHunspell(Word, FDictCodePage);
  Result := Hunspell_stem(FHunspell, slst, PChar(WordEncoded));
  if (Result > 0) then
  begin
    slstCursor := slst;
    for i := 1 to Result do
    begin
      Suggestions.Add(ConvertFromHunspell(slstCursor^, FDictCodePage));
      Inc(Integer(slstCursor), sizeOf(Pointer));
    end;
    Hunspell_free_list(FHunspell, slst, Result);
  end;
end;

procedure THunspellChecker.Add(Word : WideString);
var WordEncoded : string;
begin
  FAddList.Add(Word);
  WordEncoded := ConvertToHunspell(Word, FDictCodePage);
  Hunspell_add(FHunspell, PChar(WordEncoded));
end;

procedure THunspellChecker.Remove(Word : WideString);
var WordEncoded : string;
begin
  WordEncoded := ConvertToHunspell(Word, FDictCodePage);
  Hunspell_remove(FHunspell, PChar(WordEncoded));
end;

function THunspellChecker.FindDict(Path : WideString) : Integer;
var FFindFileAttrs : Integer;
    FSearchRec : TSearchRecW;
    FAffFilename : WideString;
begin
  FFindFileAttrs := faAnyFile;
  FSearchRec.FindHandle := INVALID_HANDLE_VALUE;
  FDictList.Clear;
  FDictListPath := WideIncludeTrailingBackslash(Path);

  if (WideFindFirst(FDictListPath + '*.dic', FFindFileAttrs, FSearchRec) <> 0) then
    WideFindClose(FSearchRec)
  else
  begin
    while (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) do
    begin
      // Check if we have a matching "*.aff" file
      FAffFilename := FDictListPath + WideChangeFileExt(FSearchRec.Name, '.aff');
      if WideFileExists(FAffFilename) then
      begin
        FDictList.Add(FSearchRec.Name);
      end;
      if (WideFindNext(FSearchRec) <> 0) then
        WideFindClose(FSearchRec);
    end;
  end;
  Result := FDictList.Count;
end;

procedure THunspellChecker.GetDict(List : TTntStrings);
begin
  List.AddStrings(FDictList);
end;

function THunspellChecker.GetDictCount : Integer;
begin
  Result := FDictList.Count;
end;

function THunspellChecker.GetDict(Index : Integer) : WideString;
begin
  Result := FDictList.Strings[Index];
end;

function THunspellChecker.GetDictIdx(Name : WideString) : Integer;
begin
  Result := FDictList.IndexOf(Name);
end;

function THunspellChecker.GetCurrentDictName : WideString;
begin
  Result := FCurrentDictName;
end;

procedure THunspellChecker.Ignore(Word : WideString);
begin
  FIgnoreList.Add(Word);
end;

procedure THunspellChecker.LoadPersonalDict(Filename : WideString);
begin
  if WideFileExists(Filename) then
  begin
    try
      FAddList.LoadFromFile(Filename);
    except
    end;
    LoadAddListInHunspell;
  end;
end;

procedure THunspellChecker.SavePersonalDict(Filename : WideString);
begin
  try
    FAddList.SaveToFile(Filename);
  except
  end;
end;

procedure THunspellChecker.LoadAddListInHunspell;
var i : Integer;
    WordEncoded : string;
begin
  if IsInitialized then
  begin
    for i := 0 to FAddList.Count-1 do
    begin
      WordEncoded := ConvertToHunspell(FAddList[i], FDictCodePage);
      Hunspell_add(FHunspell, PChar(WordEncoded));
    end;
  end;
end;

Function THunspellChecker.IsThesaurusInitialized:Boolean;
begin
  Result := FThesaurusDict;
end;

Function THunspellChecker.GetTotalMeanings(word : string): Integer;
begin
 FThesaurusDbWord.IndexName:='Word';
 if IsUpperCase(word[1]) and (IsUpperCase(Copy(Word,2,Length(Word)-1)) = False) then
 begin
   Word := LowerCase(Word);
 end;
 if FThesaurusDbWord.FindKey([Word]) then
 begin
   Result := FThesaurusDbWord.FieldByName('Meanings').AsInteger;
   Exit;
 end;
 Result := 0;
end;

Function THunspellChecker.GetTotalSynonymsByMeaningIndex(Word : string; MeaningIndex : Integer): Integer;
var Id : Integer;
begin
 FThesaurusDbWord.IndexName:='Word';
 if IsUpperCase(word[1]) and (IsUpperCase(Copy(Word,2,Length(Word)-1)) = False) then
 begin
   Word := LowerCase(Word);
 end;
 if FThesaurusDbWord.FindKey([Word]) then
 begin
   Id := FThesaurusDbWord.FieldByName('Id').AsInteger;
   FThesaurusDbMeanings.IndexName:='MeaningIndex';
   if FThesaurusDbMeanings.FindKey([Id,MeaningIndex]) then
   begin
    Result := FThesaurusDbMeanings.FieldByName('Synonyms').AsInteger;
    Exit;
   end;
   Exit;
 end;
 Result := 0;
end;

Function THunspellChecker.GetMeaning(Word : string; MeaningIndex : Integer): String;
var Id : Integer;
begin
 FThesaurusDbWord.IndexName:='Word';
 if IsUpperCase(word[1]) and (IsUpperCase(Copy(Word,2,Length(Word)-1)) = False) then
 begin
   Word := LowerCase(Word);
 end;
 if FThesaurusDbWord.FindKey([Word]) then
 begin
  Id := FThesaurusDbWord.FieldByName('Id').AsInteger;
  FThesaurusDbMeanings.IndexName:='MeaningIndex';
  if FThesaurusDbMeanings.FindKey([Id,MeaningIndex]) then
  begin
    Result := FThesaurusDbMeanings.FieldByName('Meaning').AsString;
    Exit;
  end;
 end;
 Result := '';
end;

Function THunspellChecker.GetSynonym(Word : string; MeaningIndex : Integer; SynonymIndex : Integer): String;
var Id : Integer; FirstLetterUppercase : Boolean;
begin
 FThesaurusDbWord.IndexName:='Word';
 FirstLetterUppercase := False;
 if IsUpperCase(word[1]) and (IsUpperCase(Copy(Word,2,Length(Word)-1)) = False) then
 begin
   FirstLetterUppercase := True;
   Word := LowerCase(Word);
 end;
 if FThesaurusDbWord.FindKey([Word]) then
 begin
   Id := FThesaurusDbWord.FieldByName('Id').AsInteger;
   FThesaurusDbSynonyms.IndexName:='SynonymIndex';
   if FThesaurusDbSynonyms.FindKey([Id,MeaningIndex,SynonymIndex]) then
   begin
    Result := FThesaurusDbSynonyms.FieldByName('Synonym').AsString;
    if FirstLetterUppercase then
    begin
      Result := UpperCase(Copy(FThesaurusDbSynonyms.FieldByName('Synonym').AsString,1,1))+
        Copy(FThesaurusDbSynonyms.FieldByName('Synonym').AsString,2,Length(FThesaurusDbSynonyms.FieldByName('Synonym').AsString))
    end;
    Exit;
   end;
 end;
 Result := '';
end;

end.
