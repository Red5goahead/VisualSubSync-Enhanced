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

unit jsintf_bridge;

{$I jsconfig.inc}
{$I delphi.inc}

interface

uses js15decl, SysUtils;

procedure IntfBridge_ErrorReporter(cx: PJSContext; message: PChar; report: PJSErrorReport); cdecl;
function  IntfBridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  IntfBridge_ConvertOp(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
function  IntfBridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  IntfBridge_EnumerateOp(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
procedure IntfBridge_FinalizeOp(cx: PJSContext; obj: PJSObject); cdecl;
function  IntfBridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  IntfBridge_ResolveOp(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl;
function  IntfBridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  IntfBridge_MethodCall(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;

function  ArrayBridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  ArrayBridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  ArrayBridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  ArrayBridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  ArrayBridge_GetLength(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  ArrayBridge_SetLength(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function  ArrayBridge_Push(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
function  ArrayBridge_Pop(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
function  ArrayBridge_Splice(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
function  ArrayBridge_Reverse(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
function  ArrayBridge_Join(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;

const
  intf_bridge_class: JSClass = (name: 'CBridge'; flags: JSCLASS_HAS_PRIVATE; addProperty: IntfBridge_AddProperty;
                                delProperty: IntfBridge_DeleteProperty; getProperty: IntfBridge_GetProperty; setProperty: IntfBridge_SetProperty;
                                enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
                                finalize: IntfBridge_FinalizeOp);

  intf_bridge_method_class: JSClass = (name: 'CBridgeMethod'; flags: JSCLASS_HAS_PRIVATE; addProperty: JS_PropertyStub;
                                       delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_PropertyStub;
                                       enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
                                       finalize: JS_FinalizeStub; call: IntfBridge_MethodCall);

  intf_general_class: JSClass = (name: 'TJSObject'; flags: JSCLASS_HAS_PRIVATE; addProperty: JS_PropertyStub;
                                 delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_PropertyStub;
                                 enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
                                 finalize: JS_FinalizeStub);

  base_bridge_array: JSClass = (name: 'TJSArray'; flags: JSCLASS_HAS_PRIVATE; addProperty: ArrayBridge_AddProperty;
                                delProperty: ArrayBridge_DeleteProperty; getProperty: ArrayBridge_GetProperty; setProperty: ArrayBridge_SetProperty;
                                enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
                                finalize: JS_FinalizeStub);

  base_bridge_array_methods: Array[0..5] of JSFunctionSpec = ((name: 'push'; call: ArrayBridge_Push; nargs: 0; flags: 0; extra: 0),
                                                              (name: 'pop'; call: ArrayBridge_Pop; nargs: 0; flags: 0; extra: 0),
                                                              (name: 'splice'; call: ArrayBridge_Splice; nargs: 0; flags: 0; extra: 0),
                                                              (name: 'reverse'; call: ArrayBridge_Reverse; nargs: 0; flags: 0; extra: 0),
                                                              (name: 'join'; call: ArrayBridge_Join; nargs: 0; flags: 0; extra: 0),
                                                              (name: nil; call: nil; nargs: 0; flags: 0; extra: 0));

type
  PInteger = ^Integer;

function  GetParamName(cx: PJSContext; id: jsval): TBridgeString;

implementation

uses jsintf, Windows;

function IsArray(cx: PJSContext; var val: jsval; arrcls: PJSClass): Boolean;
var
  cls: PJSClass;
  pobj : PJSObject;
begin
  JS_ValueToObject(cx, val, pobj);
  cls := JS_GetClass(pobj);
  if (cls <> nil) then
    Result := (cls = arrcls)
  else
    Result := false;
end;

function GetDelphiObject(cx: PJSContext; obj: PJSObject): TObject;
var
  data: PBridgeData;
begin
  data := PBridgeData(JS_GetPrivate(cx, obj));
  if (data <> nil) then
    Result := data^.data
  else
    Result := nil;
end;

function GetObject(cx: PJSContext; obj: PJSObject): TJSObject;
var
  data: PBridgeData;
begin
  data := PBridgeData(JS_GetPrivate(cx, obj));
  if (data <> nil) then
    Result := data^.container
  else
    Result := nil;
end;

function GetParamName(cx: PJSContext; id: jsval): TBridgeString;
begin
  Result := JS_GetStringChars(JS_ValueToString(cx, id));
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var pch: TBridgeString); overload;
begin
  {$IFNDEF JSUnicode}
  pch := TBridgeString(CreateAnsiString(JS_GetStringChars(JS_ValueToString(cx, vp^))));
  {$ELSE}
  pch := TBridgeString(CreateWideString(JS_GetStringChars(JS_ValueToString(cx, vp^))));
  {$ENDIF}
end;

procedure GetParamVarValue(cx: PJSContext; vp: pjsval; var pv: Variant); overload;
var
  d: jsdouble;
  b: Boolean;
begin
  case JS_TypeOfValue(cx, vp^) of
    JSTYPE_STRING,
    JSTYPE_OBJECT: pv := Variant(TBridgeString(JS_GetStringChars(JS_ValueToString(cx, vp^))));
    JSTYPE_NUMBER:
      begin
        JS_ValueToNumber(cx, vp^, @d);
        if (d = Round(d)) then
          pv := Integer(Round(d))
        else
          pv := d;
      end;
    JSTYPE_BOOLEAN:
      begin
        JS_ValueToBoolean(cx, vp^, @b);
        if (b) then
          pv := 1
        else
          pv := 0;
      end;
  end;
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var int: Integer); overload;
begin
  JS_ValueToInt32(cx, vp^, @int);
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var dbl: Double); overload;
begin
  JS_ValueToNumber(cx, vp^, @dbl);
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var res: PJSObject); overload;
begin
  JS_ValueToObject(cx, vp^, res);
end;

procedure IntfBridge_ErrorReporter(cx: PJSContext; message: PChar; report: PJSErrorReport); cdecl;
var
  msg: String;
begin
  if (report^.flags and JSREPORT_EXCEPTION <> 0) then  // ignore js-catchable exceptions
    exit;
    
  msg := 'Notice type: ';
  if (report^.flags and JSREPORT_WARNING <> 0) then
    msg := msg +'Warning'
  else
    msg := msg +'Error';

  msg := msg +#10 +'Message: ' +message +#10'Line: ' +IntToStr(report^.lineno);
end;

function IntfBridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function IntfBridge_ConvertOp(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function IntfBridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
begin
 // Need to raise a JS error here
  Result := JS_FALSE;
end;

function IntfBridge_EnumerateOp(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

procedure IntfBridge_FinalizeOp(cx: PJSContext; obj: PJSObject); cdecl;
begin
end;

function IntfBridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  jsobj: TObject;
  delphiobj: TJSObject;
  str: TBridgeString;
begin
  jsobj := GetDelphiObject(cx, obj);
  if (jsobj = nil) then
    raise Exception.Create('Unable to find TObject instance.');
  delphiobj := GetObject(cx, obj);
  if (delphiobj = nil) then
    raise Exception.Create('Unable to find TJSObject instance.');
  str := GetParamName(cx, id);

  // Always return true otherwise it stops any following operation.
  // The real result is in vp, we just don't touch it.
  Result := JS_TRUE;
  if (delphiobj.HasNativeProperty(jsobj, str, false)) then
    vp^ := delphiobj.GetNativeProperty(jsobj, str)
end;

function IntfBridge_ResolveOp(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function IntfBridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  jsobj: TObject;
  dobj: TJSObject;
  str: Variant;
  paramName: TBridgeString;
  msg : String;
begin
  jsobj := GetDelphiObject(cx, obj);
  if (jsobj = nil) then
    raise Exception.Create('Unable to find TObject instance.');
  dobj := GetObject(cx, obj);
  if (dobj = nil) then
    raise Exception.Create('Unable to find TJSObject instance.');

  paramName := GetParamName(cx, id);
  GetParamVarValue(cx, vp, str);
  if (dobj.HasNativeProperty(jsobj, paramName, true)) then
  begin
    dobj.SetNativeProperty(jsobj, paramName, str);
    Result := JS_TRUE;    
  end
  else
  begin
    msg := Format('Can''t set none existing property %s on a native object.', [paramName]);
    JS_ReportError(cx, PAnsiChar(msg), id);
    Result := JS_FALSE;
  end;
end;

function IntfBridge_MethodCall(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
type
  TParm = record
    case Integer of
      1: (int: Integer);
      2: (dbl: Double);
      3: (obj: Pointer);
      4: (bool: LongBool);
  end;
var
  bridge: PJSObject;
  con: TJSObject;
  meth: Pointer;
  data: PBridgeData;
  jsobj: TObject;
  i: Integer;
  arglist: array of Pointer;
  tmpv: pjsval;
  val: jsval;
  str: TBridgeString;
  parm: ^TParm;
  jscls: PJSClass;
  hasdata: Boolean;
  origargc: Cardinal;
  rt: TResultType;
  pc: Cardinal;
  _al: Byte;
  _eax: Pointer;
  _edx: Pointer;
  _ecx: Pointer;
  _var: Pointer;    // Used when a var param needs to be appended (needvar = true)
  _dbl: Double;
  _obj: TObject;
  _jsobj: PJSObject;
  needvar: Boolean;
  strlist : array of TBridgeString; // Copy the string parameter in this array so it's no freed
begin
  rval^ := JSVAL_NULL;

  origargc := argc;
  tmpv := argv;
  Dec(tmpv, 2); // access argv[-2] to get the value of |this|
  bridge := PJSObject(tmpv^);

  data := JS_GetPrivate(cx, obj);
  if (data = nil) then
    raise Exception.Create('Object does not have private data.');
  jsobj := data^.data;

  data := JS_GetPrivate(cx, bridge);
  if (data = nil) then
    raise Exception.Create('Object does not have private data.');
  con := TJSObject(data^.container);
  meth := data^.data;
  str := data^.name;

  needvar := false;
  hasdata := con.HasMethodInfo(str);
  if (hasdata) then
  begin
    rt := con.GetMethodResult(str);
    pc := con.GetParamCount(str);
    if (argc <> pc) then
    begin
      {$IFDEF D6OR7}
      if (argc < pc) then
        JS_ReportError(cx, 'Not enough parameters')
      else
        JS_ReportError(cx, 'Too many parameters');
      {$ENDIF}
      Result := JSVAL_FALSE;
      exit;
    end;

    if (rt = rtString) then
      needvar := true;
  end
  else
    rt := rtNone;

  _ecx := nil;
  _edx := nil;
  SetLength(arglist, 0);
  for i := 1 to argc do
  begin
    val := argv^;
    New(parm);
    case JS_TypeOfValue(cx, val) of
      JSTYPE_NUMBER:
        if (JSValIsInt(val)) then
          JS_ValueToInt32(cx, val, @parm^.int)
        else
          JS_ValueToNumber(cx, val, @parm^.dbl);
      JSTYPE_STRING:
        begin
          // Here we convert the javascript parameter to a Delphi string
          str := JS_GetStringChars(JS_ValueToString(cx, val));
          // Copy the string parameter in this array so it's no freed          
          SetLength(strlist, Length(strlist) + 1);
          strlist[Length(strlist) - 1] := str;
          // Finally get a pointer on this delphi string
          if Length(str) = 0 then
          begin
            parm^.obj := nil
          end
          else
          begin
            parm^.obj := PBridgeChar(strlist[Length(strlist) - 1]);
            // for the record, here is the syntax :
            // Params[0]:=@s1;           //VAR string parameter
            // Params[1]:=@s2[1];        //CONST string parameter
            // Params[3]:=PWideChar(s3); //CONST string parameter
          end;
        end;
      JSTYPE_BOOLEAN:
        parm^.bool := JSValToBoolean(val);
      JSTYPE_OBJECT:
        begin
          if (val = 0) then
          begin
            parm^.int := 0;
          end
          else
          begin
            jscls := JS_GetClass(Pointer(val));
            if (jscls^.flags and JSCLASS_HAS_PRIVATE = JSCLASS_HAS_PRIVATE) then
            begin
              if (JS_ValueToObject(cx, val, _jsobj) = JS_TRUE)  then
              begin
                parm^.obj := GetDelphiObject(cx, Pointer(val));
              end else begin
                parm^.obj := JS_GetPrivate(cx, Pointer(val));
              end;
            end;

            if (jscls^.flags and JSCLASS_HAS_PRIVATE = 0) or (parm^.obj = nil) then
            begin
              str := JS_GetStringChars(JS_ValueToString(cx, val));
              SetLength(strlist, Length(strlist) + 1);
              strlist[Length(strlist) - 1] := str;
              if Length(str) = 0 then
                parm^.obj := nil
              else
                parm^.obj := PBridgeChar(strlist[Length(strlist) - 1]);
            end;
          end;
        end;
    end;

    if (i = 1) then
    begin
      _edx := Pointer(parm^.int);
      Dec(argc);
    end
    else if (i = 2) then
    begin
      _ecx := Pointer(parm^.int);
      Dec(argc);
    end
    else
    begin
      SetLength(arglist, Length(arglist)+1);
      arglist[Length(arglist)-1] := Pointer(parm^.int);
    end;
    Dispose(parm);

    Inc(argv);
  end;

  if (needvar) then
  begin
    New(_var);
    case origargc of
      0: _edx := @_var;
      1: _ecx := @_var;
      else
        begin
          Inc(argc);
          SetLength(arglist, Length(arglist)+1);
          arglist[Length(arglist)-1] := @_var;
        end;
    end;
  end;

  asm
    mov esi,argc
    mov edx,arglist

    cmp argc,0
    je @docall

  @loop:                // push parameters on the stack, if any
    mov eax,[edx]
    push eax
    add edx,$04
    dec esi
    jnz @loop

  @docall:
    mov   eax,jsobj     // set up |Self|
    mov   edx,_edx      // first param
    mov   ecx,_ecx      // second param
    call  meth
    mov   _eax,eax      // required for Integer results
    mov   _al,al        // convenience for Boolean results
    cmp   rt,2          // skip to end if result is not Double
    jne   @end

    fstp  qword ptr [_dbl]
    wait

  @end:
  end;

  case rt of
    rtString:
      rval^ := StringToJSVal(cx, String(_var));
    rtInteger:
      rval^ := IntToJSVal(Integer(_eax));
    rtBoolean:
      if (Boolean(_al)) then
        rval^ := JSVAL_TRUE
      else
        rval^ := JSVAL_FALSE;
    rtNumber:
      JS_NewDoubleValue(cx, _dbl, rval);
    rtObject:
      begin
        _obj := _eax;
        rval^ := JSVAL_NULL;

        if (_obj is TJSObject) then
          rval^ := TJSObject(_obj).JScriptVal
        else if (_obj is TObject) then
        begin
          _jsobj := con.Engine.GetNativeObject(_obj);

          if (_jsobj <> nil) then
            rval^ := JSObjectToJSVal(_jsobj);
        end;
      end;
  end;

  SetLength(arglist, 0);
  SetLength(strlist, 0);
  Result := JS_TRUE;
end;

function  ArrayBridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  if (JSValIsInt(id)) then
  begin
    arr := TJSArray(GetDelphiObject(cx,obj));
    if (arr = nil) then
      raise Exception.Create('Unable to find TJSArray instance.');
    arr.AddJSValAt(JSValToInt(id),vp^);
  end;
  Result := JS_TRUE;
end;

function  ArrayBridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  if (JSValIsInt(id)) then
  begin
    arr := TJSArray(GetDelphiObject(cx,obj));
    if (arr = nil) then
      raise Exception.Create('Unable to find TJSArray instance.');
    arr.RemoveItemAt(JSValToInt(id));
  end;
  Result := JS_TRUE;
end;

function  ArrayBridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  if (JSValIsInt(id)) then
  begin
    arr := TJSArray(GetDelphiObject(cx,obj));
    if (arr = nil) then
      raise Exception.Create('Unable to find TJSArray instance.');
    vp^ := arr.GetJSValAt(JSValToInt(id));
  end;
  Result := JS_TRUE;
end;

function  ArrayBridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  if (JSValIsInt(id)) then
  begin
    arr := TJSArray(GetDelphiObject(cx,obj));
    if (arr = nil) then
      raise Exception.Create('Unable to find TJSArray instance.');
    arr.AddJSValAt(JSValToInt(id),vp^);
  end;
  Result := JS_TRUE;
end;

function  ArrayBridge_GetLength(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  arr := TJSArray(GetDelphiObject(cx,obj));
  if (arr = nil) then
    raise Exception.Create('Unable to find TJSArray instance.');
  vp^ := IntToJSVal(arr.Len);
  Result := JS_TRUE;
end;

function  ArrayBridge_SetLength(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  arr := TJSArray(GetDelphiObject(cx,obj));
  if (arr = nil) then
    raise Exception.Create('Unable to find TJSArray instance.');
  arr.SetSize(JSValToInt(vp^));
  Result := JS_TRUE;
end;

function  ArrayBridge_Push(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
  b: TJSBase;
  len: Cardinal;
  jsobj: PJSObject;
  i: Cardinal;
  val: jsval;
begin
  if (argc <> 1) then
  begin
    // Set a JS error here
    Result := JS_FALSE;
    exit;
  end;

  arr := TJSArray(GetDelphiObject(cx,obj));
  if (arr = nil) then
    raise Exception.Create('Unable to find TJSArray instance.');

  b := nil;
  case JS_TypeOfValue(cx, argv^) of
    JSTYPE_NUMBER:
      if (JSValIsDouble(argv^)) then
        b := TJSDouble.Create(JSValToDouble(cx, argv^), arr.Engine, '')
      else
        b := TJSInteger.Create(JSValToInt(argv^), arr.Engine, '');
    JSTYPE_STRING:
      b := TJSString.Create(GetParamName(cx, argv^), arr.Engine, '');
    JSTYPE_BOOLEAN:
      b := TJSBoolean.Create(JSValToBoolean(argv^), arr.Engine, '');
    JSTYPE_OBJECT:
      begin
        if (IsArray(cx, argv^, arr.Engine.ArrayClass)) then
        begin
          JS_ValueToObject(cx, argv^, jsobj);
          JS_GetArrayLength(cx, jsobj, len);
          for i := 0 to len-1 do
          begin
            JS_GetElement(cx, jsobj, i, @val);
            ArrayBridge_Push(cx, obj, 1, @val, nil);
          end;
        end
        else
        begin
          JS_ValueToObject(cx, argv^, jsobj);
          b := TJSObject.Create(jsobj, arr.Engine, '');
        end;
      end;
  end;
  if (b <> nil) then
    arr.Add(b);

  Result := JS_TRUE;
end;

function  ArrayBridge_Pop(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  arr := TJSArray(GetDelphiObject(cx,obj));
  if (arr = nil) then
    raise Exception.Create('Unable to find TJSArray instance.');

  if (arr.Len = 0) then
  begin
    {$IFDEF D6OR7}
    JS_ReportError(cx, 'Array has no elements to pop.');
    {$ENDIF}
    Result := JS_FALSE;
    exit;
  end;

  rval^ := arr[0].JScriptVal;
  arr.RemoveItemAt(0);

  Result := JS_TRUE;
end;

function  ArrayBridge_Splice(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
  delat: Integer;
  count: Integer;
begin
  if (argc <> 2) then
  begin
    {$IFDEF D6OR7}
    JS_ReportError(cx, 'Splice method requires two arguments.');
    {$ENDIF}
    Result := JS_FALSE;
    exit;
  end;

  arr := TJSArray(GetDelphiObject(cx,obj));
  if (arr = nil) then
    raise Exception.Create('Unable to find TJSArray instance.');

  delat := JSValToInt(argv^);
  Inc(argv);
  count := JSValToInt(argv^);

  while (count > 0) and (arr.Len > 0) do
  begin
    arr.RemoveItemAt(delat);
    Dec(count);
  end;

  Result := JS_TRUE;
end;

function  ArrayBridge_Reverse(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
begin
  arr := TJSArray(GetDelphiObject(cx,obj));
  if (arr = nil) then
    raise Exception.Create('Unable to find TJSArray instance.');
  arr.Reverse;
  Result := JS_TRUE;
end;

function  ArrayBridge_Join(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
var
  arr: TJSArray;
  str: TBridgeString;
begin
  arr := TJSArray(GetDelphiObject(cx,obj));
  if (arr = nil) then
    raise Exception.Create('Unable to find TJSArray instance.');
  str := arr.Join;
  {$IFNDEF JSUnicode}
  rval^ := JSStringToJSVal(JS_NewStringCopyN(cx, PBridgeChar(str), Length(str)));
  {$ELSE}
  rval^ := JSStringToJSVal(JS_NewUCStringCopyN(cx, PBridgeChar(str), Length(str)));
  {$ENDIF}
  Result := JS_TRUE;
end;

end.

