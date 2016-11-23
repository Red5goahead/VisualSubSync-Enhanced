// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2007 Christophe Paris
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

unit TntRichEditCustomUndoUnit;

interface

uses Windows, Messages, Classes, Controls, ActiveX, Richedit, RichOle,
  TntComCtrls, UndoableTaskUnit;

type
  TTntRichEditCustomUndo = class;

  TUndoableTextTask = class(TUndoableTask)
  private
    FRichEdit : TTntRichEditCustomUndo;

    // Selection before modification
    FOriginalSelStart, FOriginalSelLength : Integer;
    // Part of text to be replaced before the modification
    FOriginalText : WideString;

    // Selection after modification
    FSelStart, FSelLength : Integer;
    // Start position of replacement
    FReplaceStart : Integer;
    // Part of the text replaced after modification
    FReplaceByText : WideString;
    
    FMerged : Boolean;
    FDisableMerge : Boolean;

  public
    procedure DoTask; override;
    function GetName : WideString; override;
    function GetMemSize : Integer;
    function Merge(UndoableTask : TUndoableTask) : Boolean; override;
    procedure DisableMerge;
    procedure UndoTask; override;
  end;

  TNotifyUndo = procedure(Sender: TTntRichEdit; UndoData : TUndoableTask) of object;

  TRichEditOleCallback = class(TInterfacedObject, IRichEditOleCallback)
  private
    FOwner: TTntRichEditCustomUndo;
  public
    constructor Create(AOwner: TTntRichEditCustomUndo);

    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
         out Doc: IOleInPlaceUIWindow; var FrameInfo: TOleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; stg: IStorage; cp: longint): HRESULT; stdcall;
    function DeleteObject(oleobj: IOLEObject): HRESULT; stdcall;
    function QueryAcceptData(dataobj: IDataObject; var cfFormat: TClipFormat;
         reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
         out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
         var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
         const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;

  TTntRichEditCustomUndo = class(TTntRichEdit)
  private
    FSelStartBeforeChange : Integer;
    FSelLengthBeforeChange : Integer;
    FUndoDisabled : Boolean;
    FRichEditOleCallback : TRichEditOleCallback;
    FOldText : WideString;
    FOnUndo : TNotifyUndo;
    
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure DisableWindowsUndo;
    procedure SaveSelectionInfo;
    function HasTextChanged : Boolean;

  published
    property UndoDisabled: Boolean read FUndoDisabled write FUndoDisabled;
    property OnUndo : TNotifyUndo read FOnUndo write FOnUndo;
  end;

implementation

uses SysUtils, tom_TLB;

// =============================================================================

procedure TUndoableTextTask.DoTask;
var TxtStart, TxtEnd, NewText : WideString;
    LenOText : Integer;
begin
  LenOText := Length(FOriginalText);
  // Get the new text start
  TxtStart := Copy(FRichEdit.Text, 1, FReplaceStart - 1);
  // Get the new text end
  TxtEnd := Copy(FRichEdit.Text, FReplaceStart + LenOText, MaxInt);
  // Recalculate the new text (We are replacing "FReplaceByText" at "FReplaceStart" by "FOriginalText")
  NewText := TxtStart + FReplaceByText + TxtEnd;

  // Disable undo
  FRichEdit.UndoDisabled := True;
  // Set the content of the rich edit to the new text
  FRichEdit.Text := NewText;
  // Restore undo
  FRichEdit.UndoDisabled := False;

  // Restore selection
  FRichEdit.SelStart := FSelStart;
  FRichEdit.SelLength := FSelLength;
end;

// -----------------------------------------------------------------------------

function TUndoableTextTask.GetName : WideString;
begin
  Result := 'TUndoableTextTask';
end;

// -----------------------------------------------------------------------------

procedure TUndoableTextTask.UndoTask;
var TxtStart, TxtEnd, OriginalText : WideString;
  LenRText : Integer;
begin
  LenRText := Length(FReplaceByText);
  // Get the old text start
  TxtStart := Copy(FRichEdit.Text, 1, FReplaceStart - 1);
  // Get the old text end
  TxtEnd := Copy(FRichEdit.Text, FReplaceStart + LenRText, MaxInt);
  // Recalculate the original text (We are replacing "FReplaceByText" at "FReplaceStart" by "FOriginalText")
  OriginalText := TxtStart + FOriginalText + TxtEnd;

  // Disable undo
  FRichEdit.UndoDisabled := True;
  // Set the content of the rich edit back to the original text
  FRichEdit.Text := OriginalText;
  // Restore undo
  FRichEdit.UndoDisabled := False;

  // Restore selection
  FRichEdit.SelStart := FOriginalSelStart;
  FRichEdit.SelLength := FOriginalSelLength;

  // Don't merge any more task after undo
  DisableMerge;
end;

// -----------------------------------------------------------------------------

function TUndoableTextTask.Merge(UndoableTask : TUndoableTask) : Boolean;
var TaskToMerge : TUndoableTextTask;
begin
  Result := False;
  if FDisableMerge then
    Exit;

  // Merge 2 undoable tasks (for example insertion/deletion of 1 char)
  if (UndoableTask is TUndoableTextTask) then
  begin
    TaskToMerge := TUndoableTextTask(UndoableTask);
    // Insertion of 1 new char
    if (Length(TaskToMerge.FReplaceByText) = 1)
      and (Length(TaskToMerge.FOriginalText) = 0)
      and (Length(FOriginalText) = 0)
      and (TaskToMerge.FReplaceStart = FReplaceStart + Length(FReplaceByText))
      and ((Length(FReplaceByText) = 1) or FMerged) then
    begin
      FReplaceByText := FReplaceByText + TaskToMerge.FReplaceByText;
      FSelStart := TaskToMerge.FSelStart;
      FMerged := True;
      Result := True;
      Exit;
    end;

    // Backward deletion
    if (Length(TaskToMerge.FOriginalText) = 1)
      and (Length(TaskToMerge.FReplaceByText) = 0)
      and (TaskToMerge.FReplaceStart = (FReplaceStart - 1))
      and ((Length(FOriginalText) = 1) or FMerged) then
    begin
      FOriginalText := TaskToMerge.FOriginalText + FOriginalText;
      FReplaceStart := TaskToMerge.FReplaceStart;
      FSelStart := TaskToMerge.FSelStart;
      FMerged := True;
      Result := True;
      Exit;
    end;

    // Forward deletion
    if (Length(TaskToMerge.FOriginalText) = 1)
      and (Length(TaskToMerge.FReplaceByText) = 0)    
      and (TaskToMerge.FReplaceStart = FReplaceStart)
      and ((Length(FOriginalText) = 1) or FMerged) then
    begin
      FOriginalText := FOriginalText + TaskToMerge.FOriginalText;
      FOriginalSelLength := FOriginalSelLength + TaskToMerge.FOriginalSelLength;
      FMerged := True;
      Result := True;
      Exit;
    end;

  end;
end;

function TUndoableTextTask.GetMemSize : Integer;
begin
  // TODO
  Result := 0;
end;

procedure TUndoableTextTask.DisableMerge;
begin
  FDisableMerge := True;
end;

// =============================================================================

constructor TRichEditOleCallback.Create(AOwner : TTntRichEditCustomUndo);
begin
  inherited Create;
  FOwner:= AOwner;
end;

function TRichEditOleCallback.GetNewStorage(out stg: IStorage): HRESULT;
begin
  Result:= E_OUTOFMEMORY;
end;

function TRichEditOleCallback.GetInPlaceContext(out Frame: IOleInPlaceFrame;
  out Doc: IOleInPlaceUIWindow; var FrameInfo: TOleInPlaceFrameInfo): HRESULT;
begin
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.ShowContainerUI(fShow: BOOL): HRESULT;
begin
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.QueryInsertObject(const clsid: TCLSID;
  stg: IStorage; cp: longint): HRESULT;
begin
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.DeleteObject(oleobj: IOLEObject): HRESULT;
begin
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.QueryAcceptData(dataobj: IDataObject;
  var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
  hMetaPict: HGLOBAL): HRESULT;
{var stgmed : STGMEDIUM;
    format : FORMATETC;
    pastedData : PWideChar;}
begin
  // Force plain text when pasting
  cfFormat := CF_UNICODETEXT;

  {
  // Save the pasted text
  if fReally then
  begin
    format.cfFormat := CF_UNICODETEXT;
    format.ptd := nil;
    format.dwAspect := DVASPECT_CONTENT;
    format.lindex := -1;
    format.tymed := TYMED_HGLOBAL;
    if Succeeded(dataobj.GetData(format, stgmed)) then
    begin
      pastedData := GlobalLock(stgmed.hGlobal);
      FOwnerData.FPastedText := pastedData;
      Form1.MemoLogs.Lines.Add('data = ' + FOwnerData.FPastedText);
      GlobalUnlock(stgmed.hGlobal);
      ReleaseStgMedium(stgmed);
    end;
  end;
  }

  if (not fReally) then
  begin
    // Save the current selection
    FOwner.SaveSelectionInfo;
  end;
  
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.ContextSensitiveHelp(fEnterMode: BOOL): HRESULT;
begin
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD;
  out dataobj: IDataObject): HRESULT;
begin
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
  var dwEffect: DWORD): HRESULT;
begin
  Result:= E_NOTIMPL;
end;

function TRichEditOleCallback.GetContextMenu(seltype: Word; oleobj: IOleObject;
  const chrg: TCharRange; var menu: HMENU): HRESULT;
var CursorPos : TPoint;
begin
  if Assigned(FOwner.PopupMenu) then
  begin
    GetCursorPos(CursorPos);
    FOwner.PopupMenu.Popup(CursorPos.X, CursorPos.Y);
  end;
  Result:= E_NOTIMPL;
end;

// -----------------------------------------------------------------------------

constructor TTntRichEditCustomUndo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUndoDisabled := False;
end;

procedure TTntRichEditCustomUndo.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  // Set the call back here to make sur the window handle is created
  FRichEditOleCallback := TRichEditOleCallback.Create(Self);
  if not RichEdit_SetOleCallback(Self.Handle, FRichEditOleCallback) then
    raise Exception.Create('Unable to set callback');
end;

procedure TTntRichEditCustomUndo.WMClear(var Msg: TMessage);
begin
  // Save selection position
  SaveSelectionInfo;
  inherited;
end;

procedure TTntRichEditCustomUndo.WMCut(var Msg: TMessage);
begin
  // Save selection position
  SaveSelectionInfo;
  inherited;
end;

procedure TTntRichEditCustomUndo.WMPaste(var Msg: TMessage);
begin
  // Save selection position
  SaveSelectionInfo;
  inherited;
end;

procedure TTntRichEditCustomUndo.WMUndo(var Msg: TMessage);
begin
  // Do not call inherited method, we handle the undo ourself
  // inherited;
  Msg.Result := 1;
end;

procedure DiffText(TextNew, TextOld : WideString; var UndoTask : TUndoableTextTask);
var i, j, pos1 : integer;
    lenOld, lenNew : integer;
begin
  lenOld := Length(TextOld);
  lenNew := Length(TextNew);

  // Search first change from start of string
  pos1 := 1;
  while (pos1 <= lenNew) and (pos1 <= lenOld) do
  begin
    if (TextNew[pos1] <> TextOld[pos1]) then
      Break;
    Inc(pos1);
  end;

  // Search first change from end of string
  i := lenNew;
  j := lenOld;
  while (i >= pos1) and (j >= pos1) do
  begin
    if (TextNew[i] <> TextOld[j]) then
      Break;
    Dec(i);
    Dec(j);
  end;

  UndoTask.FReplaceStart := pos1;
  UndoTask.FOriginalText := Copy(TextOld, pos1, j - pos1 + 1);
  UndoTask.FReplaceByText := Copy(TextNew, pos1, i - pos1 + 1);

  {
  MemoLogs.Lines.Add(TextOld + ' => ' + TextNew);

  MemoDebugPos.Clear;
  MemoDebugPos.Lines.Add('0123456789');
  MemoDebugPos.Lines.Add(StringOfChar(' ', pos1) + 'p');
  MemoDebugPos.Lines.Add(' '+TextNew);
  MemoDebugPos.Lines.Add(' '+TextOld);
  MemoDebugPos.Lines.Add(StringOfChar(' ', i) + 'i');
  MemoDebugPos.Lines.Add(StringOfChar(' ', j) + 'j');
  MemoDebugPos.Lines.Add('p = ' + IntToStr(pos1) +  ' i = ' + IntToStr(i) + ' j = ' + IntToStr(j));

  MemoLogs.Lines.Add('Replace (' + Copy(TextOld, pos1, j - pos1 + 1) + ') by ('
    + Copy(TextNew, pos1, i - pos1 + 1) + ') at ' + IntToStr(pos1));
  }
end;

procedure TTntRichEditCustomUndo.Change;
var UndoTask : TUndoableTextTask;
begin
  if (FUndoDisabled = False) and Assigned(FOnUndo) and (Text <> FOldText) then
  begin
    // Make a very simple diff
    UndoTask := TUndoableTextTask.Create;
    DiffText(Text, FOldText, UndoTask);
    UndoTask.FOriginalSelStart := FSelStartBeforeChange;
    UndoTask.FOriginalSelLength := FSelLengthBeforeChange;
    UndoTask.FSelStart := SelStart;
    UndoTask.FSelLength := SelLength;
    UndoTask.FRichEdit := Self;
    FOnUndo(Self, UndoTask);
  end;
  FOldText := Text;
  inherited;
end;

procedure TTntRichEditCustomUndo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // Save selection position
  SaveSelectionInfo;
  inherited;
end;

// Can cause a crash in riched20.dll on some system
procedure TTntRichEditCustomUndo.DisableWindowsUndo;
var RichEditOle: IUnknown;
    TxtDocI : ITextDocument;
const
  tomSuspend	= -9999995;
	tomResume	= -9999994;
begin
  if SendMessage(Handle, EM_GETOLEINTERFACE, 0, Longint(@RichEditOle)) <> 0 then
  begin
    if RichEditOle.QueryInterface(IID_ITextDocument, TxtDocI) = S_OK then
    begin
      TxtDocI.Undo(tomSuspend);
      TxtDocI := nil;
    end;
    RichEditOle := nil;
  end;
end;

procedure TTntRichEditCustomUndo.SaveSelectionInfo;
begin
  // Save selection position
  FSelStartBeforeChange := SelStart;
  FSelLengthBeforeChange := SelLength;
end;

function TTntRichEditCustomUndo.HasTextChanged : Boolean;
begin
  Result := (FOldText <> Text);
end;

// -----------------------------------------------------------------------------

end.
