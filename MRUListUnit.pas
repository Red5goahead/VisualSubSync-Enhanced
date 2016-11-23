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

unit MRUListUnit;

interface

uses Classes, Menus, IniFiles, TntClasses, TntMenus;

type
  TMRUList = class
  private
    FFileList : TTntStrings;
    FLimit : Integer;
    FRootMenuItem : TTntMenuItem;
    FOnRecentMenuItemClick : TNotifyEvent;

    procedure FillMenuItem(MenuItem : TTntMenuItem);
    procedure OnClearItemClick(Sender: TObject);
    procedure OnClearDeadEntriesItemClick(Sender: TObject);
  public
    constructor Create(RootMenuItem : TTntMenuItem);
    destructor Destroy; override;
    procedure AddFile(Filename : WideString);
    procedure SaveIni(IniFile : TIniFile; SectionName : string);
    procedure LoadIni(IniFile : TIniFile; SectionName : string);
    function GetMRUFilename : WideString;
  published
    property OnRecentMenuItemClick : TNotifyEvent read FOnRecentMenuItemClick write FOnRecentMenuItemClick;
  end;

implementation

uses SysUtils, TntSysUtils;

// -----------------------------------------------------------------------------

constructor TMRUList.Create(RootMenuItem : TTntMenuItem);
begin
  FFileList := TTntStringList.Create;
  FLimit := 8;
  FRootMenuItem := RootMenuItem;
end;

// -----------------------------------------------------------------------------

destructor TMRUList.Destroy;
begin
  FFileList.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TMRUList.AddFile(Filename : WideString);
var Idx : Integer;
begin
  // Search if file is already present
  Idx := FFileList.IndexOf(Filename);
  if (Idx = -1) then
    FFileList.Insert(0, Filename)
  else
    FFileList.Move(Idx, 0);
  while (FFileList.Count > FLimit) do
    FFileList.Delete(FLimit);
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.FillMenuItem(MenuItem : TTntMenuItem);
var NewItem : TTntMenuItem;
    i : Integer;
begin
  MenuItem.Clear;
  MenuItem.Enabled := (FFileList.Count > 0);
  for i:=0 to FFileList.Count-1 do
  begin
    NewItem := TTntMenuItem.Create(MenuItem);
    NewItem.Caption := FFileList[i];
    NewItem.OnClick := FOnRecentMenuItemClick;
    MenuItem.Add(NewItem);
  end;
  // Add special "Clear list" menu
  NewItem := TTntMenuItem.Create(MenuItem);
  NewItem.Caption := '-';
  MenuItem.Add(NewItem);

  NewItem := TTntMenuItem.Create(MenuItem);
  NewItem.Caption := 'Clear list';
  NewItem.OnClick := OnClearItemClick;
  MenuItem.Add(NewItem);

  NewItem := TTntMenuItem.Create(MenuItem);
  NewItem.Caption := 'Clear dead entries';
  NewItem.OnClick := OnClearDeadEntriesItemClick;
  MenuItem.Add(NewItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.SaveIni(IniFile : TIniFile; SectionName : string);
var i : Integer;
begin
  for i:=0 to FLimit-1 do
  begin
    if (i < FFileList.Count) then
      IniFile.WriteString(SectionName, 'Recent' + IntToStr(i), UTF8Encode(FFileList[i]))
    else
      IniFile.WriteString(SectionName, 'Recent' + IntToStr(i), '')
  end;
end;

// -----------------------------------------------------------------------------

procedure TMRUList.LoadIni(IniFile : TIniFile; SectionName : string);
var i : Integer;
    s : WideString;
begin
  FFileList.Clear;
  for i:=0 to FLimit-1 do
  begin
    s := UTF8Decode(IniFile.ReadString(SectionName, 'Recent' + IntToStr(i), ''));
    s := Trim(s);
    if (s <> '') then
      FFileList.Add(s);
  end;
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.OnClearItemClick(Sender: TObject);
begin
  FFileList.Clear;
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.OnClearDeadEntriesItemClick(Sender: TObject);
var i : Integer;
begin
  for i:=FFileList.Count-1 downto 0 do
  begin
    if (not WideFileExists(FFileList[i])) then
    begin
      FFileList.Delete(i);
    end;
  end;
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------

function TMRUList.GetMRUFilename : WideString;
begin
  if (FFileList.Count > 0) then
  begin
    Result := FFileList[0];
  end
  else
  begin
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------
end.
// -----------------------------------------------------------------------------
