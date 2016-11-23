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

unit namedarray;

interface

type
  TNamedArray = class
  private
    FCount: Integer;
    FKeys: array of string;
    FOwnsValues: Boolean;
    FSize: Integer;
    FValues: array of Pointer;

    function GetItem(const Key: string): Pointer;
    procedure Grow;
    function IndexOf(const Key: string): Integer;
    procedure SetItem(const Key: string; const Value: Pointer);
    function GetIndex(Idx: Integer): Pointer;
  public
    property Count: Integer read FCount;
    property Item[const Key: string]: Pointer read GetItem write SetItem; default;
    property ItemByIndex[Idx: Integer]: Pointer read GetIndex;
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;

    constructor Create;
    destructor Destroy; override;

    procedure Add(const Key: string; const Value: Pointer);
  end;

implementation

{ TNamedArray }

procedure TNamedArray.Add(const Key: string; const Value: Pointer);
begin
  Inc(FCount);
  if (FCount >= FSize) then
    Grow;
  FKeys[FCount - 1] := Key;
  UniqueString(FKeys[FCount - 1]);
  FValues[FCount - 1] := Value;
end;

constructor TNamedArray.Create;
begin
  FSize := 0;
  FCount := 0;
  SetLength(FKeys, 0);
  SetLength(FValues, 0);
end;

destructor TNamedArray.Destroy;
var
  i: Integer;
begin
  if (FOwnsValues) then
  begin
    for i := 0 to FCount - 1 do
      if (FValues[i] <> nil) then
        Dispose(FValues[i]);
  end;

  SetLength(FKeys, 0);
  SetLength(FValues, 0);

  inherited;
end;

function TNamedArray.GetIndex(Idx: Integer): Pointer;
begin
  Result := FValues[Idx];
end;

function TNamedArray.GetItem(const Key: string): Pointer;
var
  idx: Integer;
begin
  idx := IndexOf(Key);
  if (idx <> -1) then
    Result := FValues[idx]
  else
    Result := nil;
end;

procedure TNamedArray.Grow;
begin
  Inc(FSize, 16);
  SetLength(FKeys, FSize);
  SetLength(FValues, FSize);
end;

function TNamedArray.IndexOf(const Key: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(FKeys) - 1 do
    if (FKeys[i] = Key) then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

procedure TNamedArray.SetItem(const Key: string; const Value: Pointer);
var
  idx: Integer;
begin
  idx := IndexOf(Key);
  if (idx <> -1) then
    FValues[idx] := Value
  else
    Add(Key, Value);
end;

end.

