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

unit GotoFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, StdCtrls, TntStdCtrls, ExtCtrls;

type
  TGotoForm = class(TForm)
    Panel1: TPanel;
    bttGo: TTntButton;
    bttCancel: TTntButton;
    rbGotoLine: TTntRadioButton;
    rbGotoTime: TTntRadioButton;
    EditLine: TEdit;
    MaskEditTime: TMaskEdit;
    procedure bttGoClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MaskEditTimeChange(Sender: TObject);
    procedure EditLineChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GotoForm: TGotoForm;

implementation

{$R *.dfm}

procedure TGotoForm.bttGoClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TGotoForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TGotoForm.FormActivate(Sender: TObject);
begin
  EditLine.SetFocus;
end;

procedure TGotoForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    ModalResult := mrOk
  else if (Key = VK_ESCAPE) then
    Close
end;

procedure TGotoForm.MaskEditTimeChange(Sender: TObject);
begin
  rbGotoTime.Checked := True;
end;

procedure TGotoForm.EditLineChange(Sender: TObject);
begin
  rbGotoLine.Checked := True;
end;

end.
