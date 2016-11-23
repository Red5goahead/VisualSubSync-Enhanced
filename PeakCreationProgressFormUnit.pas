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

unit PeakCreationProgressFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls;

type
  TPeakCreationProgressForm = class(TForm)
    ProgressBar1: TProgressBar;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetProgress(Value : Integer);
  end;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TPeakCreationProgressForm.SetProgress(Value : Integer);
begin
  ProgressBar1.Position := Value;
end;

// -----------------------------------------------------------------------------

procedure TPeakCreationProgressForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
end;

// -----------------------------------------------------------------------------

procedure TPeakCreationProgressForm.WMSysCommand(var Msg: TWMSysCommand);
begin
  if Msg.CmdType = sc_minimize then
  begin
    Application.Minimize;
  end;
  inherited;
end;

// -----------------------------------------------------------------------------
end.
// -----------------------------------------------------------------------------
