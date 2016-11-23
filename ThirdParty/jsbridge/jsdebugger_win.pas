(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is JavaScript Bridge.
 *
 * The Initial Developer of the Original Code is
 * Sterling Bates.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Theo Lustenberger <theo@theo.ch>
 *   Dominique Louis <Dominique@SavageSoftware.com.au>
 *   Bram Kuijvenhoven, Eljakim IT BV (bkuijvenhoven at eljakim.nl)
 *     - Rewrote implementation for TJSArray.Reverse (May 14, 2004) for
 *       FreePascal compatibility and speed
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** *)

(*
 * File notes:
 *  - The expression evaluator is not available at run-time.  I need a way of
 *    evaluating without triggering additional errors.  I have some commented-
 *    out code that would store the current exception and replace it after
 *    evaluation, but I haven't tested to see if that would work.
 *)

unit jsdebugger_win;

{$I delphi.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF D6OR7}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, js15decl, ExtCtrls;

type
  TDebugType = (dtError, dtBreakpoint, dtTrace);
  TDebugResult = (dtContinue, dtStepInto, dtStepOver, dtAbort);
  TDebugExecuteProc = function(const Code: String; Frame: PJSStackFrame): String of object;

  PStackItem = ^TStackItem;
  TStackItem = record
    Line: Integer;
    Script: String;
    Frame: PJSStackFrame;
  end;

  TDebugMain = class(TForm)
    pnMsg: TPanel;
    pnData: TPanel;
    lbCode: TListBox;
    pnEval: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    mmResult: TMemo;
    btRun: TButton;
    mmEval: TMemo;
    pnError: TPanel;
    Label1: TLabel;
    lbMsg: TLabel;
    pnBreakpoint: TPanel;
    Label4: TLabel;
    pnStateVars: TPanel;
    tvVars: TTreeView;
    Label6: TLabel;
    lvCallstack: TListView;
    Label5: TLabel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pnTrace: TPanel;
    Label7: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btRunClick(Sender: TObject);
    procedure lvCallstackSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FExecute: TDebugExecuteProc;
    FProps: TTreeNode;
    FResult: TDebugResult;
    FStopLine: Integer;
  public
    property ExecuteProc: TDebugExecuteProc read FExecute write FExecute;
    property Result: TDebugResult read FResult;

    procedure AddCallStack(const Fun, Script: String; Frame: PJSStackFrame; Line: Integer);
    function  AddProperty(const Name: String): TTreeNode;
    function  AddSubProperty(Parent: TTreeNode; const Value: String): TTreeNode;
    procedure FindLine(const Line: String);
    procedure GotoLine(Line: Integer);
    procedure SetCode(const Code: String);
    procedure SetMessage(const Msg: String);
    procedure SetType(_Type: TDebugType);
  end;

var
  DebugMain: TDebugMain;

implementation

{$R *.dfm}

{ TDebugMain }

procedure TDebugMain.SetCode(const Code: String);
begin
  lbCode.Items.Clear;
  lbCode.Items.SetText(PChar(Code));
end;

procedure TDebugMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TDebugMain.FindLine(const Line: String);
begin
  FStopLine := lbCode.Items.IndexOf(Line);
  lbCode.ItemIndex := FStopLine;
end;

procedure TDebugMain.GotoLine(Line: Integer);
begin
  FStopLine := Line;
  lbCode.ItemIndex := Line;
  if (Line >= 0) then
    lbCode.TopIndex := Line;
end;

procedure TDebugMain.SetMessage(const Msg: String);
begin
  lbMsg.Caption := Msg;
end;

procedure TDebugMain.FormCreate(Sender: TObject);
begin
  FProps := nil;
end;

procedure TDebugMain.AddCallStack(const Fun, Script: String; Frame: PJSStackFrame; Line: Integer);
var
  p: PStackItem;
  Item: TListItem;
begin
  New(p);
  p^.Line := Line;
  p^.Script := Script;
  p^.Frame := Frame;
  Item := lvCallstack.Items.Add;

  Item.Caption := Fun;
  Item.Data := p;

  if (lvCallstack.Selected = nil) then
    lvCallstack.Selected := Item;
end;

function TDebugMain.AddProperty(const Name: String): TTreeNode;
begin
  Result := tvVars.Items.AddChild(FProps, Name);
end;

procedure TDebugMain.SetType(_Type: TDebugType);
begin
  case _Type of
    dtError: pnError.BringToFront;
    dtBreakpoint: pnBreakpoint.BringToFront;
    dtTrace: pnTrace.BringToFront;
  end;
end;

procedure TDebugMain.btRunClick(Sender: TObject);
var
  Data: PStackItem;
begin
  if (not Assigned(FExecute)) then exit;

  if (lvCallstack.Selected = nil) then
    raise Exception.Create('No frame selected in the call stack window');
  
  Data := lvCallstack.Selected.Data;
  mmResult.Text := FExecute(mmEval.Text, Data^.Frame);
end;

function TDebugMain.AddSubProperty(Parent: TTreeNode; const Value: String): TTreeNode;
begin
  Result := tvVars.Items.AddChild(Parent, Value);
end;

procedure TDebugMain.lvCallstackSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Data: PStackItem;
begin
  if (not Selected) then exit;

  Data := Item.Data;
  (* The line below will show the script where the selected call item resides.
   * Individual functions decompile to their own script, so we don't do this right now,
   * but is useful if scripts span multiple files.
   *
   * lbCode.Items.SetText(PChar(Data^.Script));
   *)
  lbCode.ItemIndex := Data^.Line;
end;

end.
