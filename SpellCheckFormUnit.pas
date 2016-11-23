unit SpellCheckFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, SubStructUnit,
  LibHunspellUnit, UndoableSubTaskUnit, TntClasses, ExtCtrls, TntExtCtrls;

type
  TSpellCheckForm = class(TForm)
    TntPanel1: TTntPanel;
    bttReplace: TTntButton;
    bttReplaceAll: TTntButton;
    bttIgnore: TTntButton;
    bttIgnoreAll: TTntButton;
    bttAdd: TTntButton;
    TntPanel2: TTntPanel;
    TntLabel1: TTntLabel;
    reSubtitleText: TTntRichEdit;
    TntLabel3: TTntLabel;
    edReplaceBy: TTntEdit;
    TntLabel2: TTntLabel;
    lbSuggestions: TTntListBox;
    lblSub: TTntLabel;
    TntPanel3: TTntPanel;
    bttCancel: TTntButton;
    TntBevel1: TTntBevel;
    TntBevel2: TTntBevel;
    TntBevel3: TTntBevel;
    TntBevel4: TTntBevel;
    TntBevel5: TTntBevel;
    TntBevel6: TTntBevel;
    TntBevel7: TTntBevel;
    pbSubs: TTntProgressBar;
    TntBevel8: TTntBevel;
    bttReset: TTntButton;
    procedure bttCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bttIgnoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bttIgnoreAllClick(Sender: TObject);
    procedure bttAddClick(Sender: TObject);
    procedure bttReplaceClick(Sender: TObject);
    procedure bttReplaceAllClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbSuggestionsClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure bttResetClick(Sender: TObject);
  private
    { Private declarations }
    Sub : TSubtitleRange;
    TextToSpellOffset : Integer;
    TextToSpellWithTagOffset : Integer;
    WordIdx : Integer;
    WordInfo : TWordInfo;
    CurrentWord : WideString;
    MultiChangeTask : TUndoableMultiChangeTask;
    ReplaceAllWords : TTntStringList;
    EndOfCheck : Boolean;

    procedure NextError;
    procedure ReplaceWordBy(Subtitle : TSubtitleRange; AWordInfo : TWordInfo; NewText : WideString);
    procedure UpdateButton;
  public
    { Public declarations }
    function GetUndoableTask : TUndoableMultiChangeTask;
    procedure Reset;
  end;

var
  SpellCheckForm: TSpellCheckForm;

implementation

uses main, MiscToolsUnit, Types;

{$R *.dfm}

procedure TSpellCheckForm.FormCreate(Sender: TObject);
begin
  reSubtitleText.Font.Assign(MainForm.MemoSubtitleText.Font);
  lbSuggestions.Font.Assign(MainForm.MemoSubtitleText.Font);
  ReplaceAllWords := TTntStringList.Create;
  MultiChangeTask := nil;
end;

procedure TSpellCheckForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ReplaceAllWords);
end;

procedure TSpellCheckForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSpellCheckForm.Reset;
begin
  TextToSpellOffset := 1;
  TextToSpellWithTagOffset := 0;
  WordIdx := 0;
  Sub := nil;
  MultiChangeTask := nil;
  CurrentWord := '';
  EndOfCheck := False;
  ReplaceAllWords.Clear;
  UpdateButton;
end;

procedure TSpellCheckForm.UpdateButton;
begin
  bttReplace.Enabled := (Length(edReplaceBy.Text) > 0) and (EndOfCheck = False);
  bttReplaceAll.Enabled := (Length(edReplaceBy.Text) > 0) and (EndOfCheck = False);
  bttIgnore.Enabled := (EndOfCheck = False);
  bttIgnoreAll.Enabled := (EndOfCheck = False);
  bttAdd.Enabled := (EndOfCheck = False);
end;

procedure TSpellCheckForm.NextError;
var WordArray : TWideStringDynArray;
    TextToSpell : WideString;
    SpellOk : Boolean;
    Suggestions: TTntStrings;
    replaceBy : WideString;
begin
  pbSubs.Max := MainForm.GetSubCount;
  if not Assigned(Sub) then
  begin
    if (MainForm.GetSelectedCount <= 1) then
      //Sub := MainForm.GetFirst
      Sub := MainForm.GetFirstSelected
    else
      Sub := MainForm.GetFirstSelected;
  end;
  while Assigned(Sub) do
  begin
    pbSubs.Position := Sub.Node.Index;
    TagSplit(Sub.Text, WordArray);
    while (WordIdx < Length(WordArray)) do
    begin
      if (WordIdx mod 2) = 0 then
      begin
        TextToSpell := WordArray[WordIdx];
        while (TextToSpellOffset <= Length(TextToSpell)) do
        begin
          GetNextWordPos(TextToSpellOffset, TextToSpell, WordInfo);
          CurrentWord := Copy(TextToSpell, WordInfo.Position, WordInfo.Length);
          
          replaceBy := ReplaceAllWords.Values[CurrentWord];
          if (Length(replaceBy) > 0) then
          begin
            ReplaceWordBy(Sub, WordInfo, replaceBy);
            // Update the text to spell
            TagSplit(Sub.Text, WordArray);
            TextToSpell := WordArray[WordIdx];
          end
          else
          begin
            SpellOk := ContainDigit(CurrentWord) or IsUpperCase(CurrentWord)
              or MainForm.GetSpellChecker.Spell(CurrentWord);
            if (not SpellOk) then
            begin
              MainForm.FocusNode(Sub.Node, False);

              reSubtitleText.Text := Sub.Text;
              // Remove all colors
              reSubtitleText.SelectAll;
              reSubtitleText.SelAttributes.Color := clWindowText;
              // Put mispelled word in red
              reSubtitleText.SelStart := TextToSpellWithTagOffset + WordInfo.Position - 1;
              reSubtitleText.SelLength := WordInfo.Length;
              reSubtitleText.SelAttributes.Color := clRed;
              reSubtitleText.SelLength := 0;

              // Fill suggestions list
              Suggestions := TTntStringList.Create;
              MainForm.GetSpellChecker.Suggest(CurrentWord, Suggestions);
              lbSuggestions.Items.Assign(Suggestions);
              Suggestions.Free;
              // Select first suggestion
              if (lbSuggestions.Items.Count > 0) then
              begin
                lbSuggestions.ItemIndex := 0;
                edReplaceBy.Text := lbSuggestions.Items.Strings[0];
              end
              else
              begin
                edReplaceBy.Text := '';
              end;
              UpdateButton;

              // Show which subtitle it is
              lblSub.Caption := Format('Subtitle %d/%d : %s -> %s',
                [Sub.Node.Index+1, MainForm.GetSubCount, TimeMsToString(Sub.StartTime),
                TimeMsToString(Sub.StopTime)]);
              Exit;
            end;
          end;
          Inc(TextToSpellOffset);
        end;
        TextToSpellOffset := 1;
      end;
      Inc(TextToSpellWithTagOffset, Length(WordArray[WordIdx]));
      Inc(WordIdx);
    end;
    WordIdx := 0;
    TextToSpellWithTagOffset := 0;

    if (MainForm.GetSelectedCount <= 1) then
      Sub := MainForm.GetNext(Sub)
    else
      Sub := MainForm.GetNextSelected(Sub)
  end;
  if not Assigned(Sub) then
  begin
    // End of check
    lblSub.Caption := 'End of check.';
    reSubtitleText.Clear;
    edReplaceBy.Text := '';
    lbSuggestions.Clear;
    EndOfCheck := True;
    UpdateButton;
  end;
end;

procedure TSpellCheckForm.FormShow(Sender: TObject);
begin
  Reset;
  NextError;
end;

procedure TSpellCheckForm.bttIgnoreClick(Sender: TObject);
begin
  NextError;
end;

procedure TSpellCheckForm.bttIgnoreAllClick(Sender: TObject);
begin
  MainForm.GetSpellChecker.Ignore(CurrentWord);
  NextError;
end;

procedure TSpellCheckForm.bttAddClick(Sender: TObject);
begin
  MainForm.GetSpellChecker.Add(CurrentWord);
  NextError;
end;

procedure TSpellCheckForm.ReplaceWordBy(Subtitle : TSubtitleRange; AWordInfo : TWordInfo; NewText : WideString);
var ChangeSubData : TChangeSubData;
    TextBegin, TextEnd : WideString;
    DeltaLen : Integer;
begin
  ChangeSubData := TChangeSubData.Create(Subtitle.Node.Index);
  ChangeSubData.OldText := Subtitle.Text;
  TextBegin := Copy(Subtitle.Text, 1, TextToSpellWithTagOffset + AWordInfo.Position - 1);
  TextEnd := Copy(Subtitle.Text, TextToSpellWithTagOffset + AWordInfo.Position + AWordInfo.Length, MaxInt);
  Subtitle.Text := TextBegin + NewText + TextEnd;
  ChangeSubData.NewText := Subtitle.Text;
  if (MultiChangeTask = nil) then
  begin
    MultiChangeTask := TUndoableMultiChangeTask.Create
  end;
  MultiChangeTask.AddData(ChangeSubData);
  // Fix offset when replacing by a word of different length
  DeltaLen := AWordInfo.Length - Length(NewText);
  TextToSpellOffset := TextToSpellOffset - DeltaLen;
end;

procedure TSpellCheckForm.bttReplaceClick(Sender: TObject);
begin
  ReplaceWordBy(Sub, WordInfo, edReplaceBy.Text);
  NextError;
end;

function TSpellCheckForm.GetUndoableTask : TUndoableMultiChangeTask;
begin
  Result := MultiChangeTask;
end;

procedure TSpellCheckForm.bttReplaceAllClick(Sender: TObject);
var NewText : WideString;
begin
  NewText := edReplaceBy.Text;
  ReplaceAllWords.Add(CurrentWord + '=' + NewText);
  ReplaceWordBy(Sub, WordInfo, NewText);
  NextError;  
end;

procedure TSpellCheckForm.lbSuggestionsClick(Sender: TObject);
begin
  if (lbSuggestions.ItemIndex <> -1) then
  begin
    edReplaceBy.Text := lbSuggestions.Items.Strings[lbSuggestions.ItemIndex];
  end;
end;

procedure TSpellCheckForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

procedure TSpellCheckForm.bttResetClick(Sender: TObject);
begin
  Reset;
  Sub := MainForm.GetFirst;
  NextError;
end;

end.
