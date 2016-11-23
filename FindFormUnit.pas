// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003-2007 Christophe Paris
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

unit FindFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, Buttons, TntButtons, ExtCtrls;

type
  TFindForm = class(TForm)
    TntLabel1: TTntLabel;
    cbFind: TTntComboBox;
    cbReplaceWith: TTntComboBox;
    TntLabel2: TTntLabel;
    TntGroupBox1: TTntGroupBox;
    chkMatchCase: TTntCheckBox;
    chkFromCursor: TCheckBox;
    chkMatchingOnly: TTntCheckBox;
    chkRegExp: TTntCheckBox;
    chkWholeWord: TCheckBox;
    Panel1: TPanel;
    bttFind: TTntButton;
    bttReplaceFind: TTntButton;
    bttReplace: TTntButton;
    bttReplaceAll: TTntButton;
    bttClose: TTntButton;
    chkDisableVOSearch: TTntCheckBox;
    procedure bttCloseClick(Sender: TObject);
    procedure cbFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure bttFindClick(Sender: TObject);
    procedure cbFindChange(Sender: TObject);
    procedure chkMatchingOnlyClick(Sender: TObject);
    procedure chkMatchCaseClick(Sender: TObject);
    procedure bttReplaceClick(Sender: TObject);
    procedure bttReplaceFindClick(Sender: TObject);
    procedure bttReplaceAllClick(Sender: TObject);
    procedure chkRegExpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure HistorizeComboBox(ComboBox : TTntComboBox);    
  public
    { Public declarations }
    function GetFindText : WideString;
    procedure SetFindText(Selected : WideString);
    function GetReplaceText : WideString;
    function MatchCase : Boolean;
    function FromCursor : Boolean;
    function UseRegExp : Boolean;
    function WholeWord : Boolean;
    procedure SetMemoMode(MemoMode : Boolean);
  end;

var
  FindForm: TFindForm;

implementation

uses TntClasses, main;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFindForm.bttCloseClick(Sender: TObject);
begin
  if chkMatchingOnly.Enabled then
    MainForm.ShowMatchingSubOnly(True);
  Close;
end;

//------------------------------------------------------------------------------

function TFindForm.GetFindText : WideString;
begin
  Result := cbFind.Text;
end;

//------------------------------------------------------------------------------

procedure TFindForm.SetFindText(Selected : WideString);
begin
  cbFind.Text := Selected;
end;

//------------------------------------------------------------------------------

function TFindForm.GetReplaceText : WideString;
begin
  Result := cbReplaceWith.Text;
end;

//------------------------------------------------------------------------------

function TFindForm.MatchCase : Boolean;
begin
  Result := chkMatchCase.Checked;
end;

//------------------------------------------------------------------------------

function TFindForm.FromCursor : Boolean;
begin
  Result := chkFromCursor.Checked;
end;

//------------------------------------------------------------------------------

function TFindForm.UseRegExp : Boolean;
begin
  Result := chkRegExp.Checked;
end;

//------------------------------------------------------------------------------

function TFindForm.WholeWord : Boolean;
begin
  Result := chkWholeWord.Enabled and chkWholeWord.Checked;
end;

//------------------------------------------------------------------------------

procedure TFindForm.cbFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    bttFind.Click
  else if Key = VK_ESCAPE then
    Close
end;

//------------------------------------------------------------------------------

procedure TFindForm.FormActivate(Sender: TObject);
begin
  cbFind.SetFocus;
  chkDisableVOSearch.Enabled := false;
  if MainForm.CurrentProject.SubtitlesVO <> '' then
    chkDisableVOSearch.Enabled := true;
end;

//------------------------------------------------------------------------------

procedure TFindForm.HistorizeComboBox(ComboBox : TTntComboBox);
var i, Idx : integer;
begin
  // Search in historic
  Idx := -1;
  for i:=0 to ComboBox.Items.Count-1 do
  begin
    if (ComboBox.Text = ComboBox.Items[i]) then
    begin
      Idx := i;
      Break;
    end;
  end;
  if (Idx = -1) then
  begin
    // Add it to the historic
    ComboBox.Items.Insert(0, ComboBox.Text);
  end
  else
  begin
    // Move item to top of historic
    ComboBox.Items.Move(Idx,0);
    ComboBox.Text := ComboBox.Items[0];
  end;
end;

//------------------------------------------------------------------------------

procedure TFindForm.bttFindClick(Sender: TObject);
begin
  HistorizeComboBox(cbFind);
  MainForm.ActionFindNext.Execute;
  bttReplace.Enabled := MainForm.LastFindSucceded;
  bttReplaceFind.Enabled := MainForm.LastFindSucceded;
end;

//------------------------------------------------------------------------------

procedure TFindForm.cbFindChange(Sender: TObject);
begin
  MainForm.ResetFind;
  if chkMatchingOnly.Enabled and chkMatchingOnly.Checked then
    MainForm.ShowMatchingSubOnly(not chkMatchingOnly.Checked);
  bttReplace.Enabled := MainForm.LastFindSucceded;
  bttReplaceFind.Enabled := MainForm.LastFindSucceded;
end;

//------------------------------------------------------------------------------

procedure TFindForm.chkMatchingOnlyClick(Sender: TObject);
begin
  if chkMatchingOnly.Enabled then
    MainForm.ShowMatchingSubOnly(not chkMatchingOnly.Checked);
end;

//------------------------------------------------------------------------------

procedure TFindForm.chkMatchCaseClick(Sender: TObject);
begin
  if chkMatchingOnly.Enabled and chkMatchingOnly.Checked then
    MainForm.ShowMatchingSubOnly(not chkMatchingOnly.Checked);
end;

//------------------------------------------------------------------------------

procedure TFindForm.bttReplaceClick(Sender: TObject);
begin
  MainForm.ReplaceText;
  bttReplace.Enabled := False;
  bttReplaceFind.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TFindForm.bttReplaceFindClick(Sender: TObject);
begin
  bttReplaceClick(nil);
  bttFindClick(nil);
end;

//------------------------------------------------------------------------------

procedure TFindForm.bttReplaceAllClick(Sender: TObject);
begin
  MainForm.ReplaceAllText;
end;

//------------------------------------------------------------------------------

procedure TFindForm.chkRegExpClick(Sender: TObject);
begin
  chkWholeWord.Enabled := not chkRegExp.Checked;
end;

//------------------------------------------------------------------------------

procedure TFindForm.FormShow(Sender: TObject);
begin
  Resize;
end;

//------------------------------------------------------------------------------

procedure TFindForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if chkMatchingOnly.Enabled then
    MainForm.ShowMatchingSubOnly(True);
end;

//------------------------------------------------------------------------------

procedure TFindForm.SetMemoMode(MemoMode : Boolean);
begin
  chkMatchingOnly.Enabled := not MemoMode;
//  chkFromCursor.Checked := MemoMode;
  if MemoMode then
    Self.Caption := 'Find text'
  else
    Self.Caption := 'Find subtitle';
end;

//------------------------------------------------------------------------------

procedure TFindForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
