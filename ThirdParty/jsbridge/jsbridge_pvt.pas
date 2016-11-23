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

unit jsbridge_pvt;

{$I jsconfig.inc}
{$I delphi.inc}

interface

uses js15decl, SysUtils, jsintf;

function Bridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_ConvertOp(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
function Bridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_EnumerateOp(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
procedure Bridge_FinalizeOp(cx: PJSContext; obj: PJSObject); cdecl;
function Bridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_ResolveOp(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl;
function Bridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_MethodCall(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;

const
  bridge_class: JSClass = (name: 'CBridge'; flags: JSCLASS_HAS_PRIVATE; addProperty: Bridge_AddProperty;
    delProperty: Bridge_DeleteProperty; getProperty: Bridge_GetProperty; setProperty: Bridge_SetProperty;
    enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
    finalize: Bridge_FinalizeOp);

  bridge_method_class: JSClass = (name: 'CBridgeMethod'; flags: JSCLASS_HAS_PRIVATE; addProperty: JS_PropertyStub;
    delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_PropertyStub;
    enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
    finalize: JS_FinalizeStub; call: Bridge_MethodCall);

implementation

uses jsbridge;

function GetDelphiObject(cx: PJSContext; obj: PJSObject): TJSBridge;
var
  data: PBridgeData;
begin
  data := PBridgeData(JS_GetPrivate(cx, obj));
  Result := data^.data;
end;

function GetParamName(cx: PJSContext; id: jsval): TBridgeString;
begin
  Result := JS_GetStringChars(JS_ValueToString(cx, id));
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var pch: TBridgeString); overload;
begin
  pch := JS_GetStringChars(JS_ValueToString(cx, vp^));
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

function Bridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function Bridge_ConvertOp(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function Bridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
begin
 // Need to raise a JS error here
  Result := JS_FALSE;
end;

function Bridge_EnumerateOp(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

procedure Bridge_FinalizeOp(cx: PJSContext; obj: PJSObject); cdecl;
begin
end;

function Bridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  jsobj: TJSBridge;
begin
  (* Method calls also come here to get the jsval for object functions *)
  jsobj := GetDelphiObject(cx, obj);
  vp^ := jsobj._GetProperty(GetParamName(cx, id));

  Result := JS_TRUE;
end;

function Bridge_ResolveOp(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function Bridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  jsobj: TJSBridge;
  str: TBridgeString;
  paramName: TBridgeString;
begin
  jsobj := GetDelphiObject(cx, obj);
  paramName := GetParamName(cx, id);
  GetParamValue(cx, vp, str);
  jsobj._SetProperty(paramName, str);

  Result := JS_TRUE;
end;

function Bridge_MethodCall(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
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
  meth: Pointer;
  jsobj: TJSBridge;
  i: Integer;
  arglist: Array of Pointer;
  tmpv: pjsval;
  val: jsval;
	str: TBridgeString;
	jscls: PJSClass;
	parm: ^TParm;
	data: PBridgeData;
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
  needvar: Boolean;
  strlist : array of TBridgeString;
begin
	tmpv := argv;
	Dec(tmpv, 2); // access argv[-2] to get the |this| value
	bridge := PJSObject(tmpv^);

	if (GetParamName(cx, tmpv^) <> '[object CBridgeMethod]') then
	begin
	  // Raise JS error here -- we don't have the right |this|
		Result := JS_FALSE;
		exit;
	end;

  origargc := argc;
	data := JS_GetPrivate(cx, obj);
	jsobj := data^.data;

  data := JS_GetPrivate(cx, bridge);
	meth := data^.data;
  str := data^.name;

  needvar := false;
  hasdata := jsobj._HasMethodInfo(str);
  if (hasdata) then
  begin
    rt := jsobj._GetMethodResult(str);
    pc := jsobj._GetParamCount(str);
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
          jscls := JS_GetClass(Pointer(val));
          if (jscls^.flags and JSCLASS_HAS_PRIVATE = JSCLASS_HAS_PRIVATE) then
            parm^.obj := JS_GetPrivate(cx, Pointer(val));

          if (jscls^.flags and JSCLASS_HAS_PRIVATE = 0) or (parm^.obj = nil) then
          begin
            str := JS_GetStringChars(JS_ValueToString(cx, val));
            SetLength(strlist, Length(strlist) + 1);
            strlist[Length(strlist) - 1] := str;
            parm^.obj := PBridgeChar(strlist[Length(strlist) - 1]);
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
  end;

  SetLength(arglist, 0);
  SetLength(strlist, 0);
  Result := JS_TRUE;
end;

end.

