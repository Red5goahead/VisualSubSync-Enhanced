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
 *  Please send comments for enhancements, fixes, etc, to whoelse@sterlingbates.com
 *
 *    Many thanks to Les Pawelczyk and Carlo Kok (of Innerfuse Pascal Script) for
 *    their assistance with Bridge_MethodCall code.
 *
 *  Contents: Delphi<->Javascript class
 *
 *   Revision history:
 * October 30, 2003 - Initial release
 *      Nov 7, 2003 - Fixed major method call bug
 *     Nov 10, 2003 - TJSBridge no longer descends from TPersistent
 *     Nov 22, 2003 - Added disconnect method to avoid mem leaks when connecting multiple times
 *     Nov 24, 2003 - Fixed a major memory leak
 *     Nov 27, 2003 - Fixed another leak that caused AVs
 *      Dec 9, 2003 - Removed constructor which accepted a PJSContext -- that is no longer necessary
 *                  - Added FEngine as a property of a TJSBridge
 *     Dec 10, 2003 - Removed outdated methods from TJSBridge
 *                  - Modified internal properties to _<property> to avoid potential conflicts with descending classes
 *)

unit jsbridge;

(*
 *  Notes:
 *    - Only 32-bit parameters are supported as of this revision
 *    - Returning function results to SpiderMonkey is the next todo on the list
 *    - Javascript-only objects are converted to string before passing them on to native code
 *    - There are bound to be bugs.  Please report them!
 *)

{$I jsconfig.inc}
{$I delphi.inc}

interface

uses jsbridge_pvt, js15decl, jsintf, namedarray, Classes, TypInfo, SysUtils;

type
  PWord = ^Word;
  PInteger = ^Integer;

  TNativeParam = (npUnknown, npReal, npInteger, npString, npObject, npPointer, npArray);

  PNativeMethod = ^TNativeMethod;
  TNativeMethod = record
    Name: TBridgeString;                    // Method name
    Count: Integer;                         // # of parameters
    JS: PJSObject;                          // Corresponding JS object
    Params: array[0..127] of TNativeParam;  // Parameter types
  end;

  PBridgeData = ^TBridgeData;
  TBridgeData = record
    container: Pointer;
    data: Pointer;
    name: TBridgeString;
  end;

{$M+}
  TJSBridge = class
  private
    _FMethodData: TNamedArray;
    _FMethods: TNamedArray;
    _FPtrs: array of Pointer;

    procedure AddPtr(const Ptr: Pointer);
    procedure ClearMethods;
    procedure ClearPtrs;
    procedure ScanMethods(AClass: TClass);
  protected
    _FConnected: Boolean;
    _FContext: PJSContext;
    _FEngine: TJSEngine;
    _FParent: TJSObject;
    _FJSObject: PJSObject;
    _FName: TBridgeString;

    function Connect(AEngine: TJSEngine; const AInstanceName: TBridgeString): Boolean; overload;
    function Connect(AEngine: TJSEngine; const AInstanceName: TBridgeString; AParent: TJSObject): Boolean; overload;
    procedure Disconnect;
  public
    property JSObject: PJSObject read _FJSObject;

    constructor Create; overload; virtual;
    constructor Create(AEngine: TJSEngine; const AInstanceName: TBridgeString); overload; virtual;
    constructor Create(AEngine: TJSEngine; AParent: TJSObject; const AInstanceName: TBridgeString); overload; virtual;
    destructor Destroy; override;

    function _GetMethodResult(const AName: TBridgeString): TResultType;
    function _GetParamCount(const AName: TBridgeString): Integer;
    function _GetProperty(AName: TBridgeString): jsval;
    function _HasMethodInfo(const AName: TBridgeString): Boolean;
    procedure _SetMethodInfo(const AName: TBridgeString; ParamCount: Integer; ResultType: TResultType);
    function _SetProperty(AName, AValue: TBridgeString): JSBool;
  end;
{$M-}

implementation

{ TJSBridge }

procedure TJSBridge.AddPtr(const Ptr: Pointer);
begin
  SetLength(_FPtrs, Length(_FPtrs)+1);
  _FPtrs[Length(_FPtrs)-1] := Ptr;
end;

procedure TJSBridge.ClearMethods;
begin
  _FMethods.Free;
  _FMethodData.Free;
end;

procedure TJSBridge.ClearPtrs;
var
  i: Integer;
begin
  for i := 0 to Length(_FPtrs)-1 do
    Dispose(_FPtrs[i]);
  SetLength(_FPtrs, 0);
end;

function TJSBridge.Connect(AEngine: TJSEngine; const AInstanceName: TBridgeString): Boolean;
begin
  Result := Connect(AEngine, AInstanceName, AEngine.Global);
end;

function TJSBridge.Connect(AEngine: TJSEngine; const AInstanceName: TBridgeString; AParent: TJSObject): Boolean;
var
  tinfo: PTypeInfo;
  plist: PPropList;
  i: Integer;
  count: Integer;
  LName: TBridgeString;
  data: PBridgeData;
  ptypes: TTypeKinds;
begin
  if (_FConnected) then
    Disconnect;

  if (AParent = nil) then
    AParent := AEngine.Global;

  _FEngine := AEngine;
  _FContext := _FEngine.Context;
  _FParent := AParent;
  _FName := AInstanceName;

  {$IFNDEF JSUnicode}
  _FJSObject := JS_DefineObject(_FContext, _FParent.JSObject, CreateAnsiString(_FName), @bridge_class, nil, 0);
  {$ELSE}
  _FJSObject := JS_NewObject(_FContext, @bridge_class, nil, _FParent.JSObject);
  JS_DefineUCProperty(_FContext, _FParent.JSObject, CreateWideString(_FName), Length(_FName), JSObjectToJSVal(_FJSObject), nil, nil, 0);
  {$ENDIF}

  // Considered a native object by the engine
  _FEngine.AddNativeObject(Self, _FJSObject);

  New(data);
  data^.data := self;
  AddPtr(data);

  JS_SetPrivate(_FContext, _FJSObject, data);

  tinfo := PTypeInfo(self.ClassInfo);

  ptypes := [tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkWChar, tkLString, tkWString, tkInt64];
  {$IFNDEF D6OR7}
  GetMem(plist, SizeOf(Pointer)*GetTypeData(tinfo)^.PropCount);
  {$ELSE}
  plist := nil;
  {$ENDIF}

  count := GetPropList(tinfo, {$IFNDEF D6OR7}ptypes, {$ENDIF}plist);
  for i := 0 to count-1 do
    if (plist^[i]^.PropType^.Kind in ptypes) then
    begin
      LName := String(plist[i]^.Name);
      {$IFNDEF JSUnicode}
      JS_DefineProperty(_FContext, _FJSObject, CreateAnsiString(LName), JSVAL_NULL, nil, nil, JSPROP_ENUMERATE or JSPROP_PERMANENT);
      {$ELSE}
      JS_DefineUCProperty(_FContext, _FJSObject, CreateWideString(LName), Length(LName), JSVAL_NULL, nil, nil, JSPROP_ENUMERATE or JSPROP_PERMANENT);
      {$ENDIF}
    end;
  FreeMem(plist);

  _FMethods := TNamedArray.Create;
  _FMethods.OwnsValues := false;
  _FMethodData := TNamedArray.Create;
  _FMethodData.OwnsValues := true;
  ScanMethods(ClassType);

  _FConnected := true;
  Result := _FConnected;
end;

constructor TJSBridge.Create;
begin
  _FConnected := false;
end;

constructor TJSBridge.Create(AEngine: TJSEngine; const AInstanceName: TBridgeString);
begin
  Connect(AEngine, AInstanceName);
end;

constructor TJSBridge.Create(AEngine: TJSEngine; AParent: TJSObject; const AInstanceName: TBridgeString);
begin
  Connect(AEngine, AInstanceName, AParent);
end;

destructor TJSBridge.Destroy;
begin
  inherited;
  Disconnect;
end;

procedure TJSBridge.Disconnect;
begin
  if (not _FConnected) then
    exit;
  ClearPtrs;
  ClearMethods;

  _FContext := nil;
  _FParent := nil;
  _FJSObject := nil;
  _FConnected := false;
end;

function TJSBridge._GetMethodResult(const AName: TBridgeString): TResultType;
var
  meth: PMethodData;
begin
  meth := _FMethodData[AName];
  if (meth <> nil) then
    Result := meth^.ResultType
  else
    Result := rtNone;
end;

function TJSBridge._GetParamCount(const AName: TBridgeString): Integer;
var
  meth: PMethodData;
begin
  meth := _FMethodData[AName];
  if (meth <> nil) then
    Result := meth^.ParamCount
  else
    Result := -1;
end;

function TJSBridge._GetProperty(AName: TBridgeString): jsval;
var
  ts: TBridgeString;
  PInfo: PPropInfo;
  obj: PJSObject;
begin
  PInfo := GetPropInfo(self.ClassInfo, AName);
  if (PInfo <> nil) then
    case PInfo^.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
      tkString,
      tkLString,
      tkWString:
        begin
          ts := TBridgeString(GetStrProp(self, AName));
          {$IFNDEF JSUnicode}
          Result := jsval(JS_NewStringCopyN(_FContext, PBridgeChar(ts), Length(ts))) or JSVAL_STRING;
          {$ELSE}
          Result := jsval(JS_NewUCStringCopyN(_FContext, PBridgeChar(ts), Length(ts))) or JSVAL_STRING;
          {$ENDIF}
        end;
      tkInteger, tkChar, tkWChar, tkClass:
        Result := IntToJSVal(GetOrdProp(self, AName));
      tkFloat:
        Result := DoubleToJSVal(_FContext, GetFloatProp(self, AName));
    else
      Result := JSVAL_NULL;
    end
  else
  begin
    obj := _FMethods[AName];
    if (obj = nil) then
    begin
      // Raise a JS error here
      Result := JS_FALSE;
    end
    else
      Result := jsval(obj);
  end;
end;

function TJSBridge._HasMethodInfo(const AName: TBridgeString): Boolean;
begin
  Result := (_FMethodData[AName] <> nil);
end;

procedure TJSBridge.ScanMethods(AClass: TClass);
type
  TMethodtableEntry = packed record
    len: Word;
    addr: Pointer;
    name: ShortString;
  end;
var
  pp: ^Pointer;
  pMethodTable: Pointer;
  pMethodEntry: ^TMethodTableEntry;
  i, numEntries: Word;
  str: TBridgeString;
  obj: PJSObject;
  data: PBridgeData;
begin
  if AClass = nil then exit;

  pp := Pointer(Integer(AClass) + vmtMethodtable);
  pMethodTable := pp^;
  if (pMethodtable <> nil) then
  begin
    numEntries := PWord(pMethodTable)^;
    pMethodEntry := Pointer(Integer(pMethodTable) + 2);

    for i := 1 to numEntries do
    begin
      str := pMethodEntry^.name;

      {$IFNDEF JSUnicode}
      obj := JS_DefineObject(_FContext, _FJSObject, CreateAnsiString(str), @bridge_method_class, nil, JSPROP_ENUMERATE);
      {$ELSE}
      obj := JS_NewObject(_FContext, @bridge_method_class, nil, _FJSObject);
      JS_DefineUCProperty(_FContext, _FJSObject, CreateWideString(str), Length(str), JSObjectToJSVal(obj), nil, nil, JSPROP_ENUMERATE);
      {$ENDIF}

      New(data);
      data^.container := Self;
      data^.data := pMethodEntry^.addr;
      data^.name := str;

      JS_SetPrivate(_FContext, obj, data);
      AddPtr(data);
      _FMethods[str] := obj;

      pMethodEntry := Pointer(Integer(pMethodEntry) + pMethodEntry^.len);
    end;
  end;
  ScanMethods(AClass.ClassParent);
end;

procedure TJSBridge._SetMethodInfo(const AName: TBridgeString; ParamCount: Integer; ResultType: TResultType);
var
  meth: PMethodData;
begin
  New(meth);
  meth^.ParamCount := ParamCount;
  meth^.ResultType := ResultType;

  _FMethodData.Add(AName, meth);
end;

function TJSBridge._SetProperty(AName, AValue: TBridgeString): JSBool;
var
  PInfo: PPropInfo;
begin
  PInfo := GetPropInfo(self.ClassInfo, AName);
  if (PInfo <> nil) then
  begin
    SetPropValue(self, AName, AValue);
    Result := JS_TRUE;
  end
  else
  begin
    // Set a JS error here
    Result := JS_FALSE;
  end;
end;

end.

