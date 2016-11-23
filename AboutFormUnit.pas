// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003-2009 Christophe Paris
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

unit AboutFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls;

type
  TAboutForm = class(TForm)
    TntLabel1: TTntLabel;
    Bevel1: TBevel;
    bttOk: TTntButton;
    TntLabel2: TTntLabel;
    TntLabel3: TTntLabel;
    LabelVersion: TTntLabel;
    Bevel2: TBevel;
    TntLabel4: TTntLabel;
    lblVTV: TTntLabel;
    TntLabel6: TTntLabel;
    lblJS: TTntLabel;
    TntLabel8: TTntLabel;
    TntLabel5: TTntLabel;
    TntLabel9: TTntLabel;
    TntLabel11: TTntLabel;
    TntLabel10: TTntLabel;
    TntLabel12: TTntLabel;
    TntLabel13: TTntLabel;
    TntLabel14: TTntLabel;
    TntLabel15: TTntLabel;
    TntLabel16: TTntLabel;
    TntLabel17: TTntLabel;
    Label19: TTntLabel;
    TntLabel18: TTntLabel;
    TntLabel19: TTntLabel;
    procedure bttOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TntLabel3Click(Sender: TObject);
    procedure TntLabel1Click(Sender: TObject);
    procedure panelCrashTestClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses ShellAPI, MiscToolsUnit, GlobalUnit, js15decl, VirtualTrees;

{$R *.dfm}

procedure TAboutForm.bttOkClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  LabelVersion.Caption := 'Version : ' + g_ApplicationVersion.VersionString;
  lblVTV.Hint := VTVersion;
  lblVTV.ShowHint := True;
  lblJS.Hint := JS_GetImplementationVersion;
  lblJS.ShowHint := True;
end;

procedure TAboutForm.TntLabel3Click(Sender: TObject);
var s : string;
begin
  s := 'mailto:' +
    'christophe.paris' +
    '@free.fr?' +
    'subject=' +
    ApplicationName +
    ' ' +
    LabelVersion.Caption;
  ShellExecute(Handle, 'open', PAnsiChar(s), '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.TntLabel1Click(Sender: TObject);
var s : string;
begin
  if Sender = TntLabel1 then
  	s := 'http://www.visualsubsync.org'
  else
    s := TTntLabel(Sender).Caption;
  ShellExecute(Handle, 'open', PAnsiChar(s), '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.panelCrashTestClick(Sender: TObject);
var DummyPointer : PInteger;
begin
  // To test the exception hook
  DummyPointer := nil;
  DummyPointer^ := 0;
end;

procedure TAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

end.
