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

unit DelayFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, TntExtCtrls, Mask, SubStructUnit;

type
  TDelayForm = class(TForm)
    meDelay: TMaskEdit;
    rgApplyTo: TTntRadioGroup;
    rgType: TTntRadioGroup;
    bttApply: TTntButton;
    bttCancel: TTntButton;
    rgShift: TTntRadioGroup;
    procedure bttApplyClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetDelayShiftType : TDelayShiftType;
    procedure SetDelayShiftType(DelayShiftType : TDelayShiftType);

    function GetDelayApplyToType : TDelayApplyToType;
    procedure SetDelayApplyToType(DelayApplyToType : TDelayApplyToType);

    function GetDelayInMs : Integer;
    procedure SetDelayInMS(DelayInMs : Integer);
  end;

var
  DelayForm: TDelayForm;

implementation

uses MiscToolsUnit;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TDelayForm.bttApplyClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

// -----------------------------------------------------------------------------

procedure TDelayForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

// -----------------------------------------------------------------------------

function TDelayForm.GetDelayShiftType : TDelayShiftType;
begin
  Result := TDelayShiftType(rgShift.ItemIndex);
end;

// -----------------------------------------------------------------------------

procedure TDelayForm.SetDelayShiftType(DelayShiftType : TDelayShiftType);
begin
  rgShift.ItemIndex := Ord(DelayShiftType);
end;

// -----------------------------------------------------------------------------

function TDelayForm.GetDelayApplyToType : TDelayApplyToType;
begin
  Result := TDelayApplyToType(rgApplyTo.ItemIndex);
end;

// -----------------------------------------------------------------------------

procedure TDelayForm.SetDelayApplyToType(DelayApplyToType : TDelayApplyToType);
begin
  rgApplyTo.ItemIndex := Ord(DelayApplyToType);
end;

// -----------------------------------------------------------------------------

function TDelayForm.GetDelayInMs : Integer;
var unlocalizedText : string;
begin
  // Force text to use the following format
  // 123456789012
  // 00:00:00.000
  unlocalizedText := meDelay.EditText;
  unlocalizedText[3] := ':';
  unlocalizedText[6] := ':';
  Result := TimeStringToMs(unlocalizedText);
  if rgType.ItemIndex = 1 then
    Result := -Result;
end;

// -----------------------------------------------------------------------------

procedure TDelayForm.SetDelayInMS(DelayInMs : Integer);
var localizedText : string;
begin
  // Convert to localized text
  localizedText := TimeMsToString(DelayInMs);
  localizedText[3] := TimeSeparator;
  localizedText[6] := TimeSeparator;
  meDelay.Text := localizedText;
end;

// -----------------------------------------------------------------------------

procedure TDelayForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

// -----------------------------------------------------------------------------
end.
// -----------------------------------------------------------------------------
