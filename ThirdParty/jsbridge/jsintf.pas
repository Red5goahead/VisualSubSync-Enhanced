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
 *  Please send comments for enhancements, fixes, etc, to whoelse@sterlingbates.com
 *
 *  Contents: Delphi-based class interface to Mozilla's javascript engine.
 *
 *   Revision history: (for latest revisions, please check the CVS logs at delphi.mozdev.org)
 *    July 14, 2003 - Initial release
 *    July 15, 2003 - Initializing standard class in TJSEngine.AddClass now; fixes shutdown AV.
 *                  - Numerous parameter fixes, which were also causing AVs and Privileged Instruction errors
 *                  - Fixed StringToJSString to properly pre-allocate room for PJSString
 *                  - GetProperty & SetProperty now work
 *                  - Added DefineMethods and DefineProperties for externally defined objects
 *                  - Finally implemented first working native function call
 *    July 18, 2003 - Faster TJSEngine.Declare(Integer) method
 *                  - Proper implementation of TJSEngine.Declare(Double)
 *   August 5, 2003 - Updated file notes
 *  August 13, 2003 - Implemented TEngine.Serialize and .Deserialize
 *                  - Wrote basic method-by-method documentation for TJSEngine
 *     Oct 22, 2003 - Added compatibility with object-based property accessors via MethodAccessors conditional define (later removed)
 *     Oct 25, 2003 - Removed unnecessary JS_InitStandardClasses call from TJSEngine.AddClass
 *     Oct 30, 2003 - Continued standardizing the class through the SpiderMonkey interface
 *      Nov 4, 2003 - Changed class flag from JSCLASS_NEW_RESOLVE to JSCLASS_HAS_PRIVATE
 *      Nov 7, 2003 - Returning a string from GetProperty requires a call to UniqueString or Delphi prematurely frees the string
 *     Nov 11, 2003 - Added |AddNativeObject| method, which allows any TObject descendent to interact with script.
 *                    All fields and methods required to work with scripting _must_ be published.
 *     Nov 20, 2003 - Eliminated dependency on global |engine| variable
 *                  - Fixed numerous memory leaks
 *     Nov 23, 2003 - Added quick'n'dirty error reporter which can be overridden by the developer by |SetErrorReporter| method
 *     Nov 24, 2003 - Added TJSInteger, TJSDouble, and TJSString; all very lo-tech, but interoperate with JS & Delphi
 *                  - TJSEngine.Declare(val: String) now returns a jsval, not a PJSString (changed, see next comment)
 *                  - Modified all functions to work with TJSBase-derived objects, rather than mysterious jsval's.
 *     Nov 26, 2003 - Added a prototype TJSArray class; extremely likely that it has bugs at this point.
 *                  - There are a few AnsiString leaks that should be sorted out later
 *     Nov 27, 2003 - Added TJSBoolean and TJSObject classes
 *                  - Copied most of TJSObject's methods and functionality from TJSEngine
 *     Nov 28, 2003 - Downsized TJSEngine
 *       Dec 2, 2003 - Added TJSFunction object, with Call method shortcuts
 *      Dec 6, 2003 - Added declaration of JSErrorReport to js15decl.  Will add TJSErrorReporter later as an exception catching object.
 *      Dec 7, 2003 - Added enumeration of javascript objects to TJSObject
 *      Dec 9, 2003 - Removed Serialize: TMemoryStream method from TJSEngine - it wasn't working; other Serialize method still works
 *                  - Modified TJSEngine.Serialize(Script: PJSScript): TMemoryStream to return nil if the serialization failed
 *     Dec 10, 2003 - Fixed up TJSObject.IsFunction and TJSObject.TypeOf to work properly
 *     Dec 13, 2003 - Started some work on GC protection; it'll have to wait.  In the meantime I don't think there's a real problem.
 *                  - Noticed a small issue with SpiderMonkey: apparently properties named "int" don't work well :)
 *                  - Fixed up a couple of crasher bugs
 *                  - Added TJSObject.IsInteger to determine whether a JSTYPE_NUMBER is int or floating point
 *     Dec 14, 2003 - Added TJSScript class
 *                  - Removed Deserialize and Serialize methods from TJSEngine, and moved Compile methods to private
 *                  - Win32 only (for now) calls JS_Shutdown once all TJSEngines are freed; fixes occassional property confusion bug
 *     Dec 16, 2003 - Linux has a JS_Shutdown call for now, but is not at all threadsafe
 *     Dec 17, 2003 - TJSObject.Declare(array) is finally done
 *                  - TJSArray's functionality is now pretty much final; class is not yet finalized, but
 *                    now implements several js-side methods: splice, join, push, pop, and reverse
 *                  - Added TJSObject.ClassType to determine whether a given property is a Date or Array class (added proc to samples)
 *                  - TJSEngine now exposes ArrayClass and DateClass properties -- not for general use!
 *                  - Added TJSBase.ToString method
 *     Dec 18, 2003 - Finished TJSObject.GetFunction
 *     Mar 31, 2004 - Miscellaneous clean up around garbage collection
 *     Oct 16, 2004 - Just a note to let people know things *are* getting fixed :)  CVS will have a log of fixes to date.
 *)

unit jsintf;

{$I jsconfig.inc}
{$I delphi.inc}

interface

uses
  Classes, ComCtrls, ptrarray, namedarray, jsintf_bridge, TypInfo, js15decl,
  SysUtils{$IFNDEF LINUX}, Windows{$ENDIF}{$IFDEF JSDEBUGGER}, jsdebugger_win{$ENDIF};

const
  global_class: JSClass = (name: 'global'; flags: JSCLASS_HAS_PRIVATE; addProperty: JS_PropertyStub;
                           delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_PropertyStub;
                           enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
                           finalize: JS_FinalizeStub);

type
  PWord = ^Word;
  PInteger = ^Integer;

  JSClassType = (ctDate, ctArray, ctString, ctNumber, ctBoolean, ctUnknownClass, ctUnknownType);
  TNativeParam = (npUnknown, npReal, npInteger, npString, npObject, npPointer, npArray);
  TResultType = (rtString, rtInteger, rtNumber, rtObject, rtBoolean, rtNone);

  PNativeMethod = ^TNativeMethod;
  TNativeMethod = record
    Name: TBridgeString;       // Method name
    Obj: TObject;              // Object containing method
    JS: PJSObject;             // Corresponding JS object
  end;

  PMethodData = ^TMethodData;
  TMethodData = record
    ParamCount: Integer;
    ResultType: TResultType;
  end;

  PBridgeData = ^TBridgeData;
  TBridgeData = record
    container: Pointer;
    data: Pointer;
    name: TBridgeString;
  end;
  PEngineData = ^TEngineData;
  TEngineData = TObject;

  TJSBase = class;
  TJSInteger = class;
  TJSDouble = class;
  TJSArray = class;
  TJSString = class;
  TJSBoolean = class;
  TJSObject = class;
  TJSFunction = class;
{$IFDEF JSDEBUGGER}
  TJSDebugger = class;
{$ENDIF}

  TStringArray = Array of TBridgeString;
  TJSValArray = Array of jsval;
  TJSBaseArray = Array of TJSBase;

  TJSEngine = class;
  TJSErrorNotifyEvent = procedure (eng : TJSEngine; cx: PJSContext;
    Msg: PChar; report: PJSErrorReport) of object;
          
  TJSEngine = class
  private
    FArrayClass: PJSClass;
    FBooleanClass: PJSClass;
    FClasses: Array of JSClass;
    fcx: PJSContext;
    FDateClass: PJSClass;
{$IFDEF JSDEBUGGER}
    FDebugger: TJSDebugger;
    FDebugging: Boolean;
{$ENDIF}
    FGlobal: PJSObject;
    FNativeGlobal: TJSObject;
    FNatives: TPtrArray;
    FNumberClass: PJSClass;
    FPtrs: Array of Pointer;
    frt: PJSRuntime;
    FStackSize: Cardinal;
    FStringClass: PJSClass;
    FVarCount: Integer;
    FVarList: Array of TJSBase;
    FUserData : Pointer;
    FOnJSError : TJSErrorNotifyEvent;

    procedure AddPtr(const Ptr: Pointer);
    procedure AddVar(Obj: TJSBase);
    function Compile(const Code: TBridgeString; const Filename : TBridgeString = '';
      const lineNo : Cardinal = 1): PJSScript;

    procedure DeleteClasses;
    procedure DeletePtrs;
    procedure DeleteVarList;
    function HasVar(Obj: TJSBase): Boolean;
    function InternalExecute(Script: PJSScript): Boolean; overload;
    function InternalExecute(Script: PJSScript; Scope: TJSObject): Boolean; overload;
    procedure GetStandardClasses;
    procedure GrowVarList;
    function InternalCall(const func: PJSFunction; obj: PJSObject; var args: Array of TJSBase; rval: pjsval): Boolean;
    function InternalCallName(const func: TBridgeString; obj: PJSObject; var args: Array of TJSBase; rval: pjsval): Boolean;
    function InternalGet(const name: TBridgeString; obj: PJSObject; var rval: jsval): Boolean;
    procedure NewMethodClass(const AName: TBridgeString);
    procedure RemoveVar(Obj: TJSBase);
    procedure ScanMethods(AClass: TClass; AParent: PJSObject; AList: TNamedArray; Container: TJSObject);
    procedure SetErrorReporter(proc: JSErrorReporter);
        
  public
    property ArrayClass: PJSClass read FArrayClass;
    property BooleanClass: PJSClass read FBooleanClass;
    property Context: PJSContext read fcx;
    property DateClass: PJSClass read FDateClass;
{$IFDEF JSDEBUGGER}
    property Debugger: TJSDebugger read FDebugger;
{$ENDIF}
    property Global: TJSObject read FNativeGlobal;
    property NumberClass: PJSClass read FNumberClass;
    property Runtime: PJSRuntime read frt;
    property StringClass: PJSClass read FStringClass;
    property UserData : Pointer read FUserData write FUserData;
    property OnJSError : TJSErrorNotifyEvent read FOnJSError write FOnJSError;

    constructor Create(MaxMemory: Cardinal; StackSize: Cardinal = 8192);
    destructor Destroy; override;

    procedure AddNativeObject(Obj: TObject; JSObj: PJSObject);
    function Declare(val: Integer; const name: TBridgeString = ''): TJSInteger; overload;
    function Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray; overload;
    function Declare(val: Double; const name: TBridgeString = ''): TJSDouble; overload;
    function Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString; overload;
    function Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean; overload;
    { // These are deprecated, do not use them.
    function Evaluate(const code: TBridgeString; scope: TJSObject): Boolean; overload;
    function Evaluate(const code: TBridgeString): Boolean; overload;
    }
    function Execute(script: PJSScript): Boolean;
    procedure GarbageCollect;
    function GetNativeObject(Obj: TObject): PJSObject;
    function IsExceptionRaised: Boolean;
    function IsValidCode(const code: TBridgeString): Boolean;
    function NewJSObject: TJSObject; overload;
    function NewJSObject(const name: TBridgeString): TJSObject; overload;
    function NewJSObject(const name: TBridgeString; parent: TJSObject): TJSObject; overload;
{$IFDEF JSDEBUGGER}
    procedure SetDebugger(dbg: TJSDebugger);
    procedure StartDebugger;
{$ENDIF}
  end;

  (*
   * These were initially set up to be ref-counted, but that may be more effort than its worth.
   * On the other hand, it may open up thread safety for a single TJSBase to be used within
   * multiple threads.  Need to do more reading to see if this is correct or not :)
   *)
  TJSBase = class
  protected
    FConnected: Boolean;
    FDestroying: Boolean;
    FEngine: TJSEngine;
    FJSVal: jsval;
    FName: TBridgeString;
    FRefCnt: Integer;
    FScope: TJSObject;

    procedure AddRef;
    function CanGoLive: Boolean; virtual;
    procedure InternalConnect; virtual;
    procedure DecRef;
    function IsLive: Boolean; virtual;
    procedure SetConnected;
    procedure SetEngine(const Value: TJSEngine);
    procedure SetName(const Value: TBridgeString);
    procedure SetScope(const Value: TJSObject);
  public
    property Connected: Boolean read FConnected;
    property Destroying: Boolean read FDestroying write FDestroying;
    property Engine: TJSEngine read FEngine write SetEngine;
    property JScriptVal: jsval read FJSVal;
    property JSName: TBridgeString read FName write SetName;
    property Parent: TJSObject read FScope write SetScope;

    constructor Create(AEngine: TJSEngine; AName: TBridgeString); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure Connect(AEngine: TJSEngine; AName: TBridgeString; AParent: TJSObject); overload;
    procedure Connect(AEngine: TJSEngine; AName: TBridgeString); overload;
    function ToString: TBridgeString;
  end;

  TJSInteger = class(TJSBase)
  protected
    FValue: Integer;

    procedure InternalConnect; override;
    procedure SetValue(const Value: Integer);
    function GetValue: Integer;
  public
    property Value: Integer read GetValue write SetValue;

    constructor Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
  end;

  TJSDouble = class(TJSBase)
  protected
    FValue: Double;
    FJSDouble: pjsdouble;

    procedure InternalConnect; override;
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  public
    property Value: Double read GetValue write SetValue;

    constructor Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;
  end;

  (*
   * This class is just ugly.  In order to properly track variable setting and getting in javascript, 
   * I have to mock up a semi-Array-style class.  The current definition below _will not_ be final
   * product, but I'm keeping it here while I get things rolling.
   *)
  TJSArray = class(TJSBase)
  private
    function FindJSVal(val: jsval): TJSBase;
    function GetLength: Integer;
  protected
    FJSObj: PJSObject;
    FValues: Array of TJSBase;

    procedure InternalAdd(Value: TJSBase; Idx: Integer);
    procedure InternalConnect; override;
    procedure ConvertValue(Idx: Integer; val: jsval);
    function GetValue(Idx: Integer): TJSBase;
    procedure SetValue(Idx: Integer; const Value: TJSBase);
  public
    property JSObj: PJSObject read FJSObj;
    property Len: Integer read GetLength;
    property Value[Idx: Integer]: TJSBase read GetValue write SetValue; default;

    function Add(Value: Integer): TJSInteger; overload;
    function Add(const Value: TBridgeString): TJSString; overload;
    function Add(Value: Double): TJSDouble; overload;
    procedure Add(Obj: TJSBase); overload;
    procedure AddJSValAt(Idx: Integer; val: jsval);
    function GetJSValAt(Idx: Integer): jsval;
    function Join: TBridgeString;
    procedure RemoveItemAt(Idx: Integer);
    procedure Reverse;
    procedure SetSize(Val: Integer);
  end;

  (*
   * To be completed when TJSArray is finalized.
   *)
  TJSNamedArray = class(TJSBase)
  protected
    FValues: Array of TJSBase;

    procedure InternalConnect; override;
    function GetValue(Key: TBridgeString): TJSBase;
    procedure SetValue(Key: TBridgeString; const Value: TJSBase);
  public
    property Value[Key: TBridgeString]: TJSBase read GetValue write SetValue; default;
  end;

  TJSString = class(TJSBase)
  protected
    FValue: TBridgeString;
    FJSString: PJSString;

    procedure InternalConnect; override;
    function GetValue: TBridgeString;
    procedure SetValue(const Value: TBridgeString);
  public
    property Value: TBridgeString read GetValue write SetValue;

    constructor Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;
  end;

  TJSBoolean = class(TJSBase)
  protected
    FValue: Boolean;

    procedure InternalConnect; override;
    function GetValue: Boolean;
    procedure SetValue(Value: Boolean);
  public
    property Value: Boolean read GetValue write SetValue;

    constructor Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
  end;

  TJSObject = class(TJSBase)
  protected
    FJSObj: PJSObject;
    FMethods: TNamedArray;
    FNatives: TPtrArray;

    procedure CheckConnection;
    procedure InternalConnect; override;
    function InternalEvaluate(const code: TBridgeString; var rval: jsval): Boolean;
    procedure Init;
  public
    property JSObject: PJSObject read FJSObj write FJSObj;

    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;

    function AddMethod(const name: TBridgeString; proc: JSNative; paramcount: Integer): TJSFunction; overload;
    function AddMethod(const name: TBridgeString; proc: Pointer; paramcount: Integer): TJSObject; overload;
    function AddMethods(var methods: TJSFunctionSpecArray): Boolean;
    function AddNativeObject(Obj: TObject; const InstanceName: TBridgeString): TJSObject;
    function AddObject(var cls: JSClass; const AName: TBridgeString): TJSObject;
    function AddProperties(var props: TJSPropertySpecArray): Boolean;
    function Call(const func: TBridgeString; params: Array of TJSBase; var str: TBridgeString): Boolean; overload;
    function Call(const func: TBridgeString; params: Array of TJSBase; var int: Integer): Boolean; overload;
    function Call(const func: TBridgeString; params: Array of TJSBase; var dbl: Double): Boolean; overload;
    function Call(const func: TBridgeString; params: Array of TJSBase; var res: TJSObject): Boolean; overload;
    function Call(const func: TBridgeString; params: Array of TJSBase; var bool: Boolean): Boolean; overload;
    function ClassType(const Name: TBridgeString): JSClassType;
    function Declare(val: Double; const name: TBridgeString = ''): TJSDouble; overload;
    function Declare(val: Integer; const name: TBridgeString = ''): TJSInteger; overload;
    function Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString; overload;
    function Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray; overload;
    function Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean; overload;
    function DeclareObject(const name: TBridgeString): TJSObject;
    function Enumerate: TStringArray;
    function Evaluate(const code: TBridgeString): Boolean; overload;
    function Evaluate(const code: TBridgeString; var str: TBridgeString): Boolean; overload;
    function Evaluate(const code: TBridgeString; var int: Integer): Boolean; overload;
    function Evaluate(const code: TBridgeString; var dbl: Double): Boolean; overload;
    function Evaluate(const code: TBridgeString; var res: TJSObject): Boolean; overload;
    function Evaluate(const code: TBridgeString; var bool: Boolean): Boolean; overload;
    function GetFunction(const name: TBridgeString): TJSFunction;
    function GetMethodResult(const name: TBridgeString): TResultType;
    function GetNativeProperty(Obj: TObject; AName: TBridgeString): jsval;
    function GetParamCount(const name: TBridgeString): Integer;
    function GetProperty(const name: TBridgeString; var dbl: Double): Boolean; overload;
    function GetProperty(const name: TBridgeString; var int: Integer): Boolean; overload;
    function GetProperty(const name: TBridgeString; var ret: TJSObject): Boolean; overload;
    function GetProperty(const name: TBridgeString; var str: TBridgeString): Boolean; overload;
    function GetProperty(const name: TBridgeString; var bool: Boolean): Boolean; overload;
    function HasMethodInfo(const name: TBridgeString): Boolean;
    function HasNativeProperty(Obj: TObject; const AName: TBridgeString; strict: Boolean): Boolean;
    function HasProperty(const name: TBridgeString): Boolean;
    function IsFunction(const name: TBridgeString): Boolean;
    function IsInteger(const name: TBridgeString): Boolean;
    procedure RemoveObject(Obj: TJSBase);
    procedure SetMethodInfo(const name: TBridgeString; ParamCount: Integer; ResultType: TResultType);
    function SetNativeProperty(Obj: TObject; AName: TBridgeString; AValue: Variant): JSBool;
    function SetProperty(const name: TBridgeString; val: TJSBase): Boolean;
    function TypeOf(const name: TBridgeString): JSType;
  end;

  TJSFunction = class(TJSBase)
  protected
    FArgCount: Integer;
    FCall: JSNative;
    FJSFun: PJSFunction;

    procedure InternalConnect; override;
  public
    property JSFunction: PJSFunction read FJSFun;

    constructor Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;

    function Call(params: Array of TJSBase; var str: TBridgeString): Boolean; overload;
    function Call(params: Array of TJSBase; var int: Integer): Boolean; overload;
    function Call(params: Array of TJSBase; var dbl: Double): Boolean; overload;
    function Call(params: Array of TJSBase; var res: TJSObject): Boolean; overload;
    function Call(params: Array of TJSBase; var bool: Boolean): Boolean; overload;

    function Call(var str: TBridgeString): Boolean; overload;
    function Call(var int: Integer): Boolean; overload;
    function Call(var dbl: Double): Boolean; overload;
    function Call(var res: TJSObject): Boolean; overload;
    function Call(var bool: Boolean): Boolean; overload;
  end;

{ Unused code
  TJSClass = class(TJSBase)
  private
    FArgCount: Integer;
    FClass: TClass;
    FJSClass: JSClass;
    FParamTypes: Array of TNativeParam;
  protected
    procedure InternalConnect; override;
  public
    constructor Create(AEngine: TJSEngine; const AName: TBridgeString; AClass: TClass); overload;
    constructor Create(AEngine: TJSEngine; const AName: TBridgeString; AClass: TClass; ArgCount: Integer); overload;
    constructor Create(AEngine: TJSEngine; const AName: TBridgeString; AClass: TClass; ArgCount: Integer; ParamTypes: Array of TNativeParam); overload;
    destructor Destroy; override;
  end;
}

  TJSScript = class
  private
    FCode: TBridgeString;
    FCompiled: Boolean;
    FScript: PJSScript;
  public
    property Code: TBridgeString read FCode write FCode;
    property Compiled: Boolean read FCompiled;
    property Script: PJSScript read FScript;

    constructor Create; overload;
    constructor Create(const ACode: TBridgeString); overload;
    constructor Create(const ACode: TBridgeString; AEngine: TJSEngine); overload;

    function Compile(AEngine: TJSEngine; const Filename: TBridgeString = '';
      const lineNo: Cardinal = 1) : Boolean;
    function Execute(AEngine: TJSEngine) : Boolean; overload;
    function Execute(AEngine: TJSEngine; AScope: TJSObject) : Boolean; overload;
    procedure LoadCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
    procedure LoadCompiledFromStream(AStream: TStream; AEngine: TJSEngine);
    procedure LoadRaw(const AFile: TBridgeString);
    procedure SaveCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
    procedure SaveCompiledToStream(AStream: TStream; AEngine: TJSEngine);
    procedure SaveRaw(const AFile: TBridgeString);
  end;

{$IFDEF JSDEBUGGER}
  TJSDebugger = class
  private
    FCode: String;
    FConnected: Boolean;
    FEnabled: Boolean;
    FEngine: TJSEngine;
    FIsError: Boolean;
    FIsTrap: Boolean;
    FScripts: Array of PJSScript;
    FStep: Boolean;

    procedure Connect(Engine: TJSEngine);
    function  DebugExecute(const Code: String; Frame: PJSStackFrame): String;
    procedure Disconnect;
    function  IsBusy: Boolean;
    procedure IterateProps(cx: PJSContext; Win: TDebugMain; Node: TTreeNode; obj: jsval);
    function  MakePropertyString(cx: PJSContext; id: jsid; val: jsval): String;
    function  OnEnteringFunction(cx: PJSContext; fp: PJSStackFrame): Pointer;
    function  OnExitingFunction(cx: PJSContext; fp: PJSStackFrame): Pointer;
    function  OnError(cx: PJSContext; Message: PChar; Report: PJSErrorReport): JSBool;
    procedure OnNewScript(script: PJSScript);
    procedure OnObject(cx: PJSContext; obj: PJSObject; isNew: JSBool);
    function  OnStartExecution(cx: PJSContext; fp: PJSStackFrame): Pointer;
    function  OnStopExecution(cx: PJSContext; fp: PJSStackFrame): Pointer;
    function  OnTrapHandler(cx: PJSContext; script: PJSScript; pc: pjsbytecode; rval: pjsval): Cardinal;
    function  ShowDebugWindow(cx: PJSContext; ErrMsg: PChar; Line: Integer; _Type: TDebugType): TDebugResult;
  public
    constructor Create; overload;
    constructor Create(Engine: TJSEngine); overload;
    destructor Destroy; override;

    procedure Disable;
    procedure Enable;
    procedure Initialize;
    function  SetBreakpoint(Line: Integer): Integer;
    procedure SetCode(const Code: String);
    procedure StartStepping;
    procedure StopStepping;
  end;
{$ENDIF}

var
  __JSEngines: Integer;

implementation

{$IFDEF JSDEBUGGER}
(* TJSDebugger hooks *)
function CallHook(cx: PJSContext; fp: PJSStackFrame; before: JSBool; ok: PJSBool; closure: Pointer): Pointer; cdecl;
var
  dbg: TJSDebugger;
begin
  dbg := closure;
  if (before = JS_TRUE) then
    Result := dbg.OnEnteringFunction(cx, fp)
  else
    Result := dbg.OnExitingFunction(cx, fp);
end;

function XDebugHook(cx: PJSContext; message: PChar; report: PJSErrorReport; closure: Pointer): JSBool; cdecl;
var
  eng: TJSEngine;
begin
  eng := TJSEngine(PEngineData(JS_GetContextPrivate(cx))^);
  Result := eng.Debugger.OnError(cx, message, report);
end;
{$ENDIF} // JSDEBUGGER

procedure ErrorTrap(cx: PJSContext; message: PChar; report: PJSErrorReport); cdecl;
var
  eng: TJSEngine;
  peng: PEngineData;
begin
  peng := JS_GetContextPrivate(cx);
  eng := TJSEngine(peng^);
{$IFDEF JSDEBUGGER}
  if (eng.Debugger <> nil) then
    eng.Debugger.OnError(cx, message, report);
{$ENDIF}
  if Assigned(eng.FOnJSError) then
    eng.FOnJSError(eng, cx, message, report);
end;

{$IFDEF JSDEBUGGER}

function ExecHook(cx: PJSContext; fp: PJSStackFrame; before: JSBool; ok: PJSBool; closure: Pointer): Pointer; cdecl;
var
  dbg: TJSDebugger;
begin
  dbg := closure;
  if (before = JS_TRUE) then
    Result := dbg.OnStartExecution(cx, fp)
  else
    Result := dbg.OnStopExecution(cx, fp);
end;

procedure NewScriptHook(cx: PJSContext; filename: PChar; lineno: uintN; script: PJSScript; fun: PJSFunction; callerdata: Pointer); cdecl;
var
  dbg: TJSDebugger;
begin
  dbg := callerdata;
  dbg.OnNewScript(script);
end;

procedure ObjectHook(cx: PJSContext; obj: PJSObject; isNew: JSBool; closure: Pointer); cdecl;
var
  dbg: TJSDebugger;
begin
  dbg := closure;
  dbg.OnObject(cx, obj, isNew);
end;

function TrapHandler(cx: PJSContext; script: PJSScript; pc: pjsbytecode; rval: pjsval; closure: Pointer): Cardinal; cdecl;
var
  dbg: TJSDebugger;
begin
  dbg := closure;
  Result := dbg.OnTrapHandler(cx, script, pc, rval);
end;
{$ENDIF} // JSDEBUGGER

{ TJSEngine }

procedure TJSEngine.AddNativeObject(Obj: TObject; JSObj: PJSObject);
begin
  FNatives[Obj] := JSObj;
end;

procedure TJSEngine.AddPtr(const Ptr: Pointer);
begin
  SetLength(FPtrs, Length(FPtrs)+1);
  FPtrs[Length(FPtrs)-1] := Ptr;
end;

procedure TJSEngine.AddVar(Obj: TJSBase);
begin
  if (HasVar(Obj)) then
    exit;
  if (FVarCount >= Length(FVarList)) then
    GrowVarList;
  FVarList[FVarCount] := Obj;
  Inc(FVarCount);
end;

function TJSEngine.Compile(const Code: TBridgeString; const Filename : TBridgeString = '';
  const lineNo : Cardinal = 1): PJSScript;
begin
  {$IFNDEF JSUnicode}
  Result := JS_CompileScript(fcx, FGlobal, PBridgeChar(Code), Length(Code), PBridgeChar(Filename), lineNo);
  {$ELSE}
  // TODO : Unicode filename support in spydermonkey ?
  Result := JS_CompileUCScript(fcx, FGlobal, PBridgeChar(Code), Length(Code),
    PChar(String(Filename)), lineNo);
  {$ENDIF}
end;

constructor TJSEngine.Create(MaxMemory: Cardinal; StackSize: Cardinal = 8192);
var
  PEng: PEngineData;
begin
  {$IFDEF LINUX}Inc(__JSEngines);{$ELSE}InterlockedIncrement(__JSEngines);{$ENDIF}
  FStackSize := StackSize;

  frt := JS_Init(MaxMemory);
  fcx := JS_NewContext(frt, FStackSize);

  New(PEng);
  AddPtr(PEng);
  PEng^ := self;
  JS_SetContextPrivate(fcx, PEng);

  FGlobal := JS_NewObject(fcx, @global_class, nil, nil);
  JS_InitStandardClasses(fcx, FGlobal);

  FNatives := TPtrArray.Create;
  FNatives.OwnsValues := false;

  FNativeGlobal := TJSObject.Create(FGlobal, self, '');

  GetStandardClasses;
  SetErrorReporter(ErrorTrap);
end;

function TJSEngine.Declare(val: Integer; const name: TBridgeString = ''): TJSInteger;
begin
  Result := TJSInteger.Create(val, self, name, FNativeGlobal);
  AddVar(Result);
end;

function TJSEngine.Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray;
begin
  Result := TJSArray.Create(self, name);
  AddVar(Result);
end;

function TJSEngine.Declare(val: Double; const name: TBridgeString = ''): TJSDouble;
begin
  Result := TJSDouble.Create(val, self, name, FNativeGlobal);
  AddVar(Result);
end;

function TJSEngine.Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString;
begin
  Result := TJSString.Create(val, self, name, FNativeGlobal);
  AddVar(Result);
end;

function TJSEngine.Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean;
begin
  Result := TJSBoolean.Create(val, self, name, FNativeGlobal);
  AddVar(Result);
end;

procedure TJSEngine.DeleteClasses;
begin
  SetLength(FClasses, 0);
end;

procedure TJSEngine.DeletePtrs;
var
  i: Integer;
begin
  for i := 0 to Length(FPtrs)-1 do
    Dispose(FPtrs[i]);
  SetLength(FPtrs, 0);
end;

procedure TJSEngine.DeleteVarList;
var
  i: Integer;
begin
  for i := 0 to Length(FVarList)-1 do
    if (FVarList[i] <> nil) then
    begin
      FVarList[i].Destroying := true;
      FVarList[i].Free;
    end;
  SetLength(FVarList, 0);
end;

destructor TJSEngine.Destroy;
begin
{$IFDEF JSDEBUGGER}
  if (FDebugging) then    // Must occur first
    FDebugger.Free;
{$ENDIF}

  JS_DestroyContext(fcx);
  JS_Finish(frt);

  FNatives.Free;
  FNativeGlobal.Free;

  DeletePtrs;
  DeleteClasses;
  DeleteVarList;

{$IFDEF LINUX}
  Dec(__JSEngines);
  if (__JSEngines = 0) then
    JS_Shutdown;
{$ELSE}
  if (InterlockedDecrement(__JSEngines) = 0) then
    JS_Shutdown;
{$ENDIF}

  inherited;
end;

function TJSEngine.Execute(Script: PJSScript): Boolean;
begin
  Result := InternalExecute(Script);
end;

procedure TJSEngine.GarbageCollect;
begin
  JS_GC(fcx);
end;

function TJSEngine.GetNativeObject(Obj: TObject): PJSObject;
begin
  Result := FNatives[Obj];
end;

procedure TJSEngine.GetStandardClasses;
var
  obj: PJSObject;

  function Eval(const str: TBridgeString): jsval;
  begin
    {$IFNDEF JSUnicode}
    JS_EvaluateScript(fcx, FGlobal, PBridgeChar(str), Length(str), nil, 0, @Result);
    {$ELSE}
    JS_EvaluateUCScript(fcx, FGlobal, PBridgeChar(str), Length(str), nil, 0, @Result);
    {$ENDIF}
  end;

begin
  JS_ValueToObject(fcx, Eval('Date.prototype'), obj);
  FDateClass := JS_GetClass(obj);

  JS_ValueToObject(fcx, Eval('Array.prototype'), obj);
  FArrayClass := JS_GetClass(obj);

  JS_ValueToObject(fcx, Eval('Boolean.prototype'), obj);
  FBooleanClass := JS_GetClass(obj);

  JS_ValueToObject(fcx, Eval('String.prototype'), obj);
  FStringClass := JS_GetClass(obj);

  JS_ValueToObject(fcx, Eval('Number.prototype'), obj);
  FNumberClass := JS_GetClass(obj);
end;

procedure TJSEngine.GrowVarList;
begin
  SetLength(FVarList, Length(FVarList)+16);
end;

function TJSEngine.HasVar(Obj: TJSBase): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Length(FVarList)-1 do
    if (FVarList[i] = Obj) then
    begin
      Result := true;
      exit;
    end;
end;

function TJSEngine.InternalCall(const func: PJSFunction; obj: PJSObject; var args: Array of TJSBase; rval: pjsval): Boolean;
var
  myargs: TJSValArray;
  i: Integer;
begin
  if (obj = nil) then
    obj := FGlobal;

  if (Length(args) = 0) then
    Result := (JS_CallFunction(fcx, obj, func, 0, nil, rval) = JS_TRUE)
  else
  begin
    SetLength(myargs, Length(args));
    for i := 0 to Length(args)-1 do
      if (args[i] <> nil) then
        myargs[i] := args[i].JScriptVal
      else
        myargs[i] := JSVAL_NULL;

    Result := (JS_CallFunction(fcx, obj, func, Length(myargs), @myargs[0], rval) = JS_TRUE);
    SetLength(myargs, 0);
  end;
end;

function TJSEngine.InternalCallName(const func: TBridgeString; obj: PJSObject; var args: array of TJSBase; rval: pjsval): Boolean;
var
  myargs: TJSValArray;
  i, jsres: Integer;
  myfunc : jsval;

begin
  if (obj = nil) then
    obj := FGlobal;

  Result := False;
  if (Length(args) = 0) then
  begin
    {$IFNDEF JSUnicode}
    Result := (JS_CallFunctionName(fcx, obj, PBridgeChar(func), 0, nil, rval) = JS_TRUE)
    {$ELSE}
    // TODO : test this (this is a try to avoid patching the dll)
    jsres := JS_GetUCProperty(fcx, obj, PBridgeChar(func), Length(func), @myfunc);
    if (jsres = JS_TRUE) and (myfunc <> 0) then
    begin
      Result := (JS_CallFunctionValue(fcx, obj, myfunc, 0, nil, rval) = JS_TRUE);
    end;

    //Result := (JS_CallUCFunctionName(fcx, obj, PBridgeChar(func), Length(func), 0, nil, rval) = JS_TRUE)
    {$ENDIF}
  end
  else
  begin
    SetLength(myargs, Length(args));
    for i := 0 to Length(args)-1 do
      myargs[i] := args[i].JScriptVal;

    {$IFNDEF JSUnicode}
    Result := (JS_CallFunctionName(fcx, obj, PBridgeChar(func), Length(myargs), @myargs[0], rval) = JS_TRUE);
    {$ELSE}
    // TODO : test this (this is a try to avoid patching the dll)
    jsres := JS_GetUCProperty(fcx, obj, PBridgeChar(func), Length(func), @myfunc);
    if (jsres = JS_TRUE) and (myfunc <> 0) then
    begin
      Result := (JS_CallFunctionValue(fcx, obj, myfunc, Length(myargs), @myargs[0], rval) = JS_TRUE);
    end;
    
    //Result := (JS_CallUCFunctionName(fcx, obj, PBridgeChar(func), Length(func), Length(myargs), @myargs[0], rval) = JS_TRUE);
    {$ENDIF}
    SetLength(myargs, 0);
  end;


end;

function TJSEngine.InternalExecute(Script: PJSScript): Boolean;
begin
  Result := InternalExecute(Script, FNativeGlobal);
end;

function TJSEngine.InternalExecute(Script: PJSScript; Scope: TJSObject): Boolean;
var
  rval: jsval;
begin
  if (Scope = nil) then
    Scope := FGlobal;
  Result := (JS_ExecuteScript(fcx, Scope.JSObject, Script, @rval) = JS_TRUE);
end;

function TJSEngine.InternalGet(const name: TBridgeString; obj: PJSObject; var rval: jsval): Boolean;
begin
  if (obj = nil) then
    obj := FGlobal;
  {$IFNDEF JSUnicode}
  Result := (JS_GetProperty(fcx, obj, PBridgeChar(name), @rval) = JS_TRUE);
  {$ELSE}
  Result := (JS_GetUCProperty(fcx, obj, PBridgeChar(name), Length(name), @rval) = JS_TRUE);
  {$ENDIF}
end;

function TJSEngine.IsExceptionRaised: Boolean;
begin
  Result := (JS_IsExceptionPending(fcx) = JS_TRUE);
end;

function TJSEngine.IsValidCode(const code: TBridgeString): Boolean;
begin
  {$IFNDEF JSUnicode}
  Result := (JS_BufferIsCompilableUnit(fcx, FGlobal, PBridgeChar(code), Length(code)) = JS_TRUE);
  {$ELSE}
  Result := (JS_UCBufferIsCompilableUnit(fcx, FGlobal, PBridgeChar(code), Length(code)) = JS_TRUE);
  {$ENDIF}
end;

function TJSEngine.NewJSObject: TJSObject;
begin
  Result := TJSObject.Create(nil, self, '');
end;

function TJSEngine.NewJSObject(const name: TBridgeString): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name);
end;

function TJSEngine.NewJSObject(const name: TBridgeString; parent: TJSObject): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name, parent);
end;

procedure TJSEngine.NewMethodClass(const AName: TBridgeString);
var
  len: Integer;
begin
  len := Length(FClasses);
  SetLength(FClasses, len+1);

  with FClasses[len] do
  begin
    name := CreateAnsiString(AName);
    flags := JSCLASS_HAS_PRIVATE;
    addProperty := JS_PropertyStub;
    delProperty := JS_PropertyStub;
    getProperty := JS_PropertyStub;
    setProperty := JS_PropertyStub;
    enumerate := JS_EnumerateStub;
    resolve := JS_ResolveStub;
    convert := JS_ConvertStub;
    finalize := JS_FinalizeStub;
    call := IntfBridge_MethodCall;
  end;
end;

procedure TJSEngine.RemoveVar(Obj: TJSBase);
var
  i: Integer;
begin
  for i := 0 to Length(FVarList)-1 do
    if (FVarList[i] = Obj) then
    begin
      FVarList[i] := nil;
      exit;
    end;
end;

procedure TJSEngine.ScanMethods(AClass: TClass; AParent: PJSObject; AList: TNamedArray; Container: TJSObject);
type
  TMethodTableEntry = packed record
    len: Word;
    addr: Pointer;
    name: ShortString;
  end;
var
  pp: ^Pointer;
  pMethodTable: Pointer;
  pMethodEntry: ^TMethodTableEntry;
  i, numEntries: Word;
  str: String;
  jsobj: PJSObject;
  data: PBridgeData;
begin
  if (AClass = nil) then exit;

  pp := Pointer(Integer(AClass) +vmtMethodTable);
  pMethodTable := pp^;

  if (pMethodTable <> nil) then
  begin
    numEntries := PWord(pMethodTable)^;
    pMethodEntry := Pointer(Integer(pMethodTable) +2);
    for i := 1 to numEntries do
    begin
      str := pMethodEntry^.name;
      NewMethodClass(str);

      {$IFNDEF JSUnicode}
      jsobj := JS_DefineObject(fcx, AParent, CreateAnsiString(str), @intf_bridge_method_class, nil, JSPROP_ENUMERATE);
      {$ELSE}
      jsobj := JS_NewObject(fcx, @intf_bridge_method_class, nil, AParent);
      JS_DefineUCProperty(fcx, AParent, CreateWideString(str), Length(str), JSObjectToJSVal(jsobj), nil, nil, JSPROP_ENUMERATE);
      {$ENDIF}

      New(data);
      data^.container := Container;
      data^.data := pMethodEntry^.addr;
      data^.name := str;
      JS_SetPrivate(fcx, jsobj, data);
      AddPtr(data);

      AList[str] := jsobj;

      pMethodEntry := Pointer(Integer(pMethodEntry) +pMethodEntry^.len);
    end;
  end;
  ScanMethods(AClass.ClassParent, AParent, AList, Container);
end;

{$IFDEF JSDEBUGGER}
procedure TJSEngine.SetDebugger(dbg: TJSDebugger);
begin
  FDebugger := dbg;
  FDebugging := (dbg <> nil);
  if (FDebugging) then
    FDebugger.Enable;
end;
{$ENDIF}

procedure TJSEngine.SetErrorReporter(proc: JSErrorReporter);
begin
  JS_SetErrorReporter(fcx, proc);
end;

{$IFDEF JSDEBUGGER}
procedure TJSEngine.StartDebugger;
begin
  SetDebugger(TJSDebugger.Create(self));
end;
{$ENDIF}

{ TJSBase }

procedure TJSBase.AddRef;
begin
  Inc(FRefCnt);
end;

function TJSBase.CanGoLive: Boolean;
begin
  Result := (FName <> '') and (FScope <> nil) and (FScope.Connected);
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: TBridgeString; AParent: TJSObject);
begin
  Engine := AEngine;
  Parent := AParent;
  JSName := AName;
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: TBridgeString);
begin
  Engine := AEngine;
  Parent := AEngine.Global;
  JSName := AName;
end;

constructor TJSBase.Create(AEngine: TJSEngine; AName: TBridgeString);
begin
  Engine := AEngine;
  JSName := AName;
  Parent := FEngine.Global;
end;

constructor TJSBase.Create;
begin
  FEngine := nil;
  FScope := nil;
end;

procedure TJSBase.DecRef;
begin
  Dec(FRefCnt);
  if (FRefCnt = 0) then
    Free;
end;

destructor TJSBase.Destroy;
{$IFDEF JSUnicode}
var
  rval: jsval;
{$ENDIF}
begin
  if (FEngine <> nil) and (not FDestroying) then
  try
    FEngine.RemoveVar(self);  // only for ref-counting, and we'll probably move to something else soon
    if (FName <> '') and (FScope <> nil) then
      {$IFNDEF JSUnicode}
      JS_DeleteProperty(FEngine.Context, FScope.JSObject, PBridgeChar(FName));
      {$ELSE}
      JS_DeleteUCProperty2(FEngine.Context, FScope.JSObject, PBridgeChar(FName), Length(FName), @rval);
      {$ENDIF}
  except
  end;
end;

procedure TJSBase.InternalConnect;
begin
end;

function TJSBase.IsLive: Boolean;
begin
  (*
   * This may not be the fastest way to determine whether the property already exists in FScope.
   *)
  //Result := (FScope <> nil) and FScope.HasProperty(FName);
  Result := false;
end;

procedure TJSBase.SetConnected;
begin
  FConnected := (FEngine <> nil);
  if (FConnected) then
  begin
    FEngine.AddVar(Self);
    InternalConnect;
  end;
end;

procedure TJSBase.SetEngine(const Value: TJSEngine);
begin
  FEngine := Value;
  SetConnected;
end;

procedure TJSBase.SetName(const Value: TBridgeString);
begin
  FName := Value;
  SetConnected;
end;

procedure TJSBase.SetScope(const Value: TJSObject);
begin
  if (FEngine <> nil) and (Value = nil) then
    FScope := FEngine.Global
  else
    FScope := Value;
  SetConnected;
end;

function TJSBase.ToString: TBridgeString;
begin
  Result := GetParamName(FEngine.Context, FJSVal);
end;

{ TJSInteger }

procedure TJSInteger.InternalConnect;
begin
  inherited;
  SetValue(FValue);
end;

constructor TJSInteger.Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSInteger.Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

function TJSInteger.GetValue: Integer;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSInteger.SetValue(const Value: Integer);
begin
  FValue := Value;
  if (FConnected) then
  begin
    FJSVal := IntToJSVal(FValue);
    if (not IsLive) and (CanGoLive) then
      FScope.SetProperty(FName, self);
  end;
end;

{ TJSDouble }

procedure TJSDouble.InternalConnect;
begin
  inherited;
  SetValue(FValue);
end;

constructor TJSDouble.Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSDouble.Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

destructor TJSDouble.Destroy;
begin
  inherited;
end;

function TJSDouble.GetValue: Double;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSDouble.SetValue(const Value: Double);
begin
  FValue := Value;
  if (FConnected) then
  begin
    FJSVal := DoubleToJSVal(FEngine.Context, FValue);
    if (not IsLive) and (CanGoLive) then
    begin
      FScope.SetProperty(FName, self);
    end;
  end;
end;

{ TJSArray }

function TJSArray.Add(Value: Integer): TJSInteger;
begin
  Result := FEngine.Declare(Value);
  InternalAdd(Result, Length(FValues));
end;

function TJSArray.Add(const Value: TBridgeString): TJSString;
begin
  Result := FEngine.Declare(Value);
  InternalAdd(Result, Length(FValues));
end;

function TJSArray.Add(Value: Double): TJSDouble;
begin
  Result := FEngine.Declare(Value);
  InternalAdd(Result, Length(FValues));
end;

procedure TJSArray.Add(Obj: TJSBase);
begin
  InternalAdd(Obj, Length(FValues));
end;

procedure TJSArray.AddJSValAt(Idx: Integer; val: jsval);
var
  isnew: Boolean;
  obj: TJSObject;
  pobj: PJSObject;
begin
  isnew := (Idx >= 0) and (Idx >= Length(FValues));

  if (isnew) then
    case JS_TypeOfValue(FEngine.Context, val) of
      JSTYPE_STRING: InternalAdd(FEngine.Declare(JSStringToString(JS_ValueToString(FEngine.Context, val))), Idx);
      JSTYPE_NUMBER:
        if (JSValIsInt(val)) then
          InternalAdd(FEngine.Declare(JSValToInt(val)), Idx)
        else
          InternalAdd(FEngine.Declare(JSValToDouble(FEngine.Context, val)), Idx);
      JSTYPE_OBJECT:
        begin
          obj := FEngine.NewJSObject;
          JS_ValueToObject(FEngine.Context, val, pobj);
          obj.JSObject := pobj;
          InternalAdd(obj, Idx);
        end;
      JSTYPE_BOOLEAN:
        InternalAdd(FEngine.Declare(JSValToBoolean(val)), Idx);
    end
  else
    ConvertValue(Idx, val);
end;

procedure TJSArray.ConvertValue(Idx: Integer; val: jsval);
var
  b: TJSBase;
  obj: TJSObject;
  pobj : PJSObject;
begin
  b := FValues[Idx];

  case JS_TypeOfValue(FEngine.Context, val) of
    JSTYPE_NUMBER:
      if (JSValIsInt(val)) then
      begin
        if (b is TJSInteger) then
          TJSInteger(b).Value := JSValToInt(val)
        else
          InternalAdd(FEngine.Declare(JSValToInt(val)), Idx);
      end
      else if (JSValIsDouble(val)) then
      begin
        if (b is TJSDouble) then
          TJSDouble(b).Value := JSValToDouble(FEngine.Context, val)
        else
          InternalAdd(FEngine.Declare(JSValToDouble(FEngine.Context, val)), Idx);
      end;
    JSTYPE_STRING:
      if (b is TJSString) then
        TJSString(b).Value := JSStringToString(JS_ValueToString(FEngine.Context, val))
      else
        InternalAdd(FEngine.Declare(JSStringToString(JS_ValueToString(FEngine.Context, val))), Idx);
    JSTYPE_BOOLEAN:
      if (b is TJSBoolean) then
        TJSBoolean(b).Value := JSValToBoolean(val)
      else
        InternalAdd(FEngine.Declare(JSValToBoolean(val)), Idx);
    JSTYPE_OBJECT:
      if (b is TJSObject) then
      begin
        JS_ValueToObject(FEngine.Context, val, pobj);
        TJSObject(b).JSObject := pobj;
      end
      else
      begin
        obj := FEngine.NewJSObject;
        JS_ValueToObject(FEngine.Context, val, pobj);
        obj.JSObject := pobj;
        InternalAdd(obj, Idx);
      end;
  end;
end;

function TJSArray.FindJSVal(val: jsval): TJSBase;
var
  i: Integer;
begin
  for i := 0 to Length(FValues)-1 do
  begin
    if (val = FValues[i].JScriptVal) then
    begin
      Result := FValues[i];
      exit;
    end;
  end;
  Result := nil;
end;

function TJSArray.GetJSValAt(Idx: Integer): jsval;
begin
  if (Idx >= 0) and (Idx < Length(FValues)) then
  begin
    if (FValues[Idx] <> nil) then
      Result := FValues[Idx].JScriptVal
    else
      Result := JSVAL_NULL;
  end
  else
    Result := JSVAL_VOID;
end;

function TJSArray.GetLength: Integer;
begin
  Result := Length(FValues);
end;

(* Deprecated *)
function TJSArray.GetValue(Idx: Integer): TJSBase;
var
  rval: jsval;
begin
  if (FConnected) and (FName <> '') then
  begin
    JS_GetElement(FEngine.Context, FJSObj, Idx, @rval);
    Result := FindJSVal(rval);
  end
  else
    Result := FValues[Idx];
end;

procedure TJSArray.InternalAdd(Value: TJSBase; Idx: Integer);
begin
  if (Idx >= 0) and (Idx < Length(FValues)) then
    FValues[Idx] := Value
  else
  begin
    SetLength(FValues, Idx+1);
    FValues[Idx] := Value;
  end;
end;

procedure TJSArray.InternalConnect;
var
  data: PBridgeData;
begin
  FJSObj := JS_NewObject(FEngine.Context, @base_bridge_array, nil, nil);
  FJSVal := jsval(FJSObj) or JSVAL_OBJECT;

  New(data);
  data^.container := FEngine;
  data^.data := self;
  FEngine.AddPtr(data);
  JS_SetPrivate(FEngine.Context, FJSObj, data);

  JS_DefineFunctions(FEngine.Context, FJSObj, @base_bridge_array_methods);
  {$IFNDEF JSUnicode}
  JS_DefineProperty(FEngine.Context, FJSObj, 'length', IntToJSVal(Len), ArrayBridge_GetLength, ArrayBridge_SetLength, JSPROP_PERMANENT);
  {$ELSE}
  JS_DefineUCProperty(FEngine.Context, FJSObj, 'length', Length('length'), IntToJSVal(Len), ArrayBridge_GetLength, ArrayBridge_SetLength, JSPROP_PERMANENT);
  {$ENDIF}

  if (not IsLive) and (CanGoLive) then
    FScope.SetProperty(FName, self);
end;

function TJSArray.Join: TBridgeString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(FValues)-1 do
  begin
    if (FValues[i] <> nil) then
      Result := Result +FValues[i].ToString
    else
      Result := Result +'null';
    if (i < Length(FValues)-1) then
      Result := Result +', ';
  end;
end;

procedure TJSArray.RemoveItemAt(Idx: Integer);
var
  i: Integer;
begin
  // This for loop can be shrunk to a move command which would speed things up
  for i := Idx+1 to Length(FValues)-1 do
    FValues[i-1] := FValues[i];
  SetLength(FValues, Length(FValues)-1);
end;

procedure TJSArray.Reverse;
var
  tmp: TJSBase;
  i: Integer;
  j: Integer;
  max: Integer;
  len: Integer;
begin
  len := Length(FValues);
  i := 0;
  j := len-1;
  max := len div 2;

  while i < max do
  begin
    tmp := FValues[i];
    FValues[i] := FValues[j];
    FValues[j] := tmp;
    Inc(i);
    Dec(j);
  end;
end;

procedure TJSArray.SetSize(Val: Integer);
begin
  SetLength(FValues, Val);
end;

procedure TJSArray.SetValue(Idx: Integer; const Value: TJSBase);
begin
  if (Idx >= Length(FValues)) then
    SetLength(FValues, Idx+1);
  FValues[Idx] := Value;
end;

{ TJSNamedArray }

function TJSNamedArray.GetValue(Key: TBridgeString): TJSBase;
begin
  Result := nil;
end;

procedure TJSNamedArray.InternalConnect;
begin
 //
end;

procedure TJSNamedArray.SetValue(Key: TBridgeString; const Value: TJSBase);
begin

end;

{ TJSString }

constructor TJSString.Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSString.Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

destructor TJSString.Destroy;
begin
  inherited;
end;

function TJSString.GetValue: TBridgeString;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSString.InternalConnect;
begin
  inherited;
  SetValue(FValue);
end;

procedure TJSString.SetValue(const Value: TBridgeString);
begin
  FValue := Value;
  if (FConnected) then
  begin
    {$IFNDEF JSUnicode}
    FJSString := JS_NewStringCopyN(FEngine.Context, PBridgeChar(FValue), Length(FValue));
    {$ELSE}
    FJSString := JS_NewUCStringCopyN(FEngine.Context, PBridgeChar(FValue), Length(FValue));
    {$ENDIF}
    FJSVal := jsval(FJSString) or JSVAL_STRING;

    if (not IsLive) and (CanGoLive) then
    begin
      JS_RemoveRoot(FEngine.Context, @FJSVal);
      FScope.SetProperty(FName, self);
    end
    else
      JS_AddRoot(FEngine.Context, @FJSVal);
  end;
end;

{ TJSBoolean }

procedure TJSBoolean.InternalConnect;
begin
  SetValue(FValue);
end;

constructor TJSBoolean.Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSBoolean.Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

function TJSBoolean.GetValue: Boolean;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSBoolean.SetValue(Value: Boolean);
begin
  FValue := Value;
  if (FConnected) then
  begin
    FJSVal := BoolToJSVal(FValue);
    if (not IsLive) and (CanGoLive) then
      FScope.SetProperty(FName, self);
  end;
end;

{ TJSObject }

function TJSObject.AddMethod(const name: TBridgeString; proc: JSNative; paramcount: Integer): TJSFunction;
var
  p: PJSFunction;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  p := JS_DefineFunction(FEngine.Context, FJSObj, CreateAnsiString(name), proc, paramcount, JSPROP_ENUMERATE);
  {$ELSE}
  p := JS_DefineUCFunction(FEngine.Context, FJSObj, CreateWideString(name), Length(name), proc, paramcount, JSPROP_ENUMERATE);
  {$ENDIF}
  if (p <> nil) then
    Result := TJSFunction.Create(p, FEngine, name, self)
  else
    Result := nil;
end;

function TJSObject.AddMethod(const name: TBridgeString; proc: Pointer; paramcount: Integer): TJSObject;
var
  p: PJSObject;
begin
  CheckConnection;
  p := JS_NewObject(FEngine.Context, @intf_bridge_method_class, nil, FJSObj);
  {$IFNDEF JSUnicode}
  JS_DefineProperty(FEngine.Context, FJSObj, CreateAnsiString(name), JSObjectToJSVal(p), nil, nil, JSPROP_ENUMERATE);
  {$ELSE}
  JS_DefineUCProperty(FEngine.Context, FJSObj, CreateWideString(name), Length(name), JSObjectToJSVal(p), nil, nil, JSPROP_ENUMERATE);
  {$ENDIF}
  if (p <> nil) then
    Result := TJSObject.Create(p, FEngine, name, self)
  else
    Result := nil;
end;

function TJSObject.AddMethods(var methods: TJSFunctionSpecArray): Boolean;
var
  len: Integer;
begin
  CheckConnection;
  (* The last element of |methods| must be blank *)
  len := Length(methods);
  SetLength(methods, len+1);
  FillChar(methods[len], SizeOf(JSFunctionSpec), #0);

  Result := (JS_DefineFunctions(FEngine.Context, FJSObj, @methods[0]) = JS_TRUE);
end;

function TJSObject.AddNativeObject(Obj: TObject; const InstanceName: TBridgeString): TJSObject;
var
  tinfo: PTypeInfo;
  ptypes: TTypeKinds;
  plist: PPropList;
  i: Integer;
  count: Integer;
  LName: String;
  methods: TNamedArray;
  data: PBridgeData;
  js: PJSObject;
  cx: PJSContext;
  cls: TClass;
begin
  CheckConnection;
  Result := nil;

  if (FNatives[Obj] = nil) then
  begin
    cx := FEngine.Context;
    js := JS_NewObject(cx, @intf_bridge_class, nil, FJSObj);
    Result := TJSObject.Create(js, FEngine, InstanceName, self);

    New(data);
    FEngine.AddPtr(data);

    data^.container := self;
    data^.data := Obj;
    JS_SetPrivate(cx, js, data);

    cls := Obj.ClassType;
    tinfo := PTypeInfo(cls.ClassInfo);

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
        JS_DefineProperty(cx, js, CreateAnsiString(LName), JSVAL_NULL, nil, nil, JSPROP_ENUMERATE or JSPROP_PERMANENT);
        {$ELSE}
        JS_DefineUCProperty(cx, js, CreateWideString(LName), Length(LName), JSVAL_NULL, nil, nil, JSPROP_ENUMERATE or JSPROP_PERMANENT);
        {$ENDIF}
      end;
    FreeMem(plist);

    methods := TNamedArray.Create;
    methods.OwnsValues := false;
    FEngine.ScanMethods(Obj.ClassType, js, methods, Result);
    FNatives[Obj] := methods;
    FEngine.AddNativeObject(Obj, js);

    FEngine.AddVar(Result);
  end;
end;

function TJSObject.AddObject(var cls: JSClass; const AName: TBridgeString): TJSObject;
var
  jsobj: PJSObject;
begin
  CheckConnection;
  jsobj := JS_NewObject(FEngine.Context, @cls, nil, FJSObj);
  Result := TJSObject.Create(jsobj, FEngine, AName, self);
end;

function TJSObject.AddProperties(var props: TJSPropertySpecArray): Boolean;
var
  len: Integer;
begin
  CheckConnection;
  (* The last element of |props| must be blank *)
  len := Length(props);
  SetLength(props, len+1);
  FillChar(props[len], SizeOf(JSPropertySpec), #0);

  Result := (JS_DefineProperties(FEngine.Context, FJSObj, @props[0]) = JS_TRUE);
end;

function TJSObject.Call(const func: TBridgeString; params: Array of TJSBase; var str: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := FEngine.InternalCallName(func, FJSObj, params, @rval);
  if (not Result) then
  begin
    str := '';
    exit;
  end;

  str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
  {$IFNDEF JSUnicode}
  UniqueString(str);
  {$ENDIF}
end;

function TJSObject.Call(const func: TBridgeString; params: Array of TJSBase; var dbl: Double): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := FEngine.InternalCallName(func, FJSObj, params, @rval);
  if (not Result) then
  begin
    dbl := JSVAL_NULL;
    exit;
  end;

  if (not JSValIsNull(rval)) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl);
end;

function TJSObject.Call(const func: TBridgeString; params: Array of TJSBase; var int: Integer): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := FEngine.InternalCallName(func, FJSObj, params, @rval);
  if (not Result) then
  begin
    int := JSVAL_NULL;
    exit;
  end;

  int := JSValToInt(rval);
end;

function TJSObject.Call(const func: TBridgeString; params: Array of TJSBase; var bool: Boolean): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := FEngine.InternalCallName(func, FJSObj, params, @rval);
  if (not Result) then
  begin
    bool := false;
    exit;
  end;

  bool := JSValToBoolean(rval);
end;

function TJSObject.Call(const func: TBridgeString; params: Array of TJSBase; var res: TJSObject): Boolean;
var
  rval: jsval;
  p: PJSObject;
  t: Pointer;
begin
  CheckConnection;

  Result := FEngine.InternalCallName(func, FJSObj, params, @rval);
  if (not Result) then
  begin
    res := nil;
    exit;
  end;

  (* !!!
   * This isn't complete yet.  We need to query the object's parent and name to make this work.
   *)
  JS_ValueToObject(FEngine.Context, rval, p);
  t := JS_GetPrivate(FEngine.Context, p);
  if (t <> nil) then
    res := TJSObject(t)
  else
    res := TJSObject.Create(p, FEngine, '');
end;

procedure TJSObject.CheckConnection;
begin
  if (not FConnected) then
    raise Exception.Create('Connection to TJSEngine instance expected.  Assign Engine property of TJSObject instance.');
end;

function TJSObject.ClassType(const Name: TBridgeString): JSClassType;
var
  rval: jsval;
  cls: PJSClass;
  pobj : PJSObject;
begin
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(Name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(Name), Length(Name), @rval);
  {$ENDIF}
  if (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_OBJECT) then
  begin
    JS_ValueToObject(FEngine.Context, rval, pobj);
    cls := JS_GetClass(pobj);
    Result := ctUnknownClass;
  end
  else
  begin
    cls := nil;
    Result := ctUnknownType;
  end;

  if (cls = FEngine.FArrayClass) then
    Result := ctArray
  else if (cls = FEngine.FDateClass) then
    Result := ctDate
  else if (cls = FEngine.FBooleanClass) then
    Result := ctBoolean
  else if (cls = FEngine.FNumberClass) then
    Result := ctNumber
  else if (cls = FEngine.FStringClass) then
    Result := ctString
  else
    case JS_TypeOfValue(FEngine.Context, rval) of
      JSTYPE_STRING: Result := ctString;
      JSTYPE_BOOLEAN: Result := ctBoolean;
      JSTYPE_NUMBER: Result := ctNumber;
    end;
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FJSObj := AValue;
  FJSVal := JSObjectToJSVal(FJSObj);

  Engine := AEngine;
  if (AEngine <> nil) then
    Parent := FEngine.Global; // Set this before we
  JSName := AName;
  Init;
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FJSObj := AValue;
  FJSVal := JSObjectToJSVal(FJSObj);

  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
  Init;
end;

function TJSObject.Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString;
begin
  CheckConnection;
  Result := TJSString.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(val: Integer; const name: TBridgeString = ''): TJSInteger;
begin
  CheckConnection;
  Result := TJSInteger.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(val: Double; const name: TBridgeString = ''): TJSDouble;
begin
  CheckConnection;
  Result := TJSDouble.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.DeclareObject(const name: TBridgeString): TJSObject;
begin
  CheckConnection;
  Result := TJSObject.Create(nil, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean;
begin
  CheckConnection;
  Result := TJSBoolean.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray;
var
  i: Integer;
begin
  CheckConnection;
  Result := TJSArray.Create(FEngine, name);
  Result.Parent := self;
  for i := 0 to Length(val)-1 do
    Result.Add(val[i]);
  FEngine.AddVar(Result);
end;

destructor TJSObject.Destroy;
begin
  inherited;
  FNatives.Free;
  FMethods.Free;
end;

function TJSObject.Enumerate: TStringArray;
var
  list: PJSIdArray;
  curid: pjsid;
  val: jsval;
  i: Integer;
begin
  CheckConnection;
  list := JS_Enumerate(FEngine.Context, FJSObj);
  curid := @list^.vector;

  SetLength(Result, list^.length);
  for i := 0 to list^.length-1 do
  begin
    JS_IdToValue(FEngine.Context, curid^, @val);
    Result[i] := TBridgeString(JS_GetStringChars(JS_ValueToString(FEngine.Context, val)));
    Inc(curid);
  end;
end;

function TJSObject.Evaluate(const code: TBridgeString): Boolean;
var
  rval: jsval;
begin
  Result := InternalEvaluate(code, rval);
end;

function TJSObject.Evaluate(const code: TBridgeString; var int: Integer): Boolean;
var
  rval: jsval;
begin
  Result := InternalEvaluate(code, rval);
  if (Result) then
    JS_ValueToInt32(FEngine.Context, rval, @int);
end;

function TJSObject.Evaluate(const code: TBridgeString; var str: TBridgeString): Boolean;
var
  rval: jsval;
begin
  Result := InternalEvaluate(code, rval);
  if (Result) then
    str := TBridgeString(JS_GetStringChars(JS_ValueToString(FEngine.Context, rval)));
end;

function TJSObject.Evaluate(const code: TBridgeString; var dbl: Double): Boolean;
var
  rval: jsval;
begin
  Result := InternalEvaluate(code, rval);
  if (Result) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl);
end;

function TJSObject.Evaluate(const code: TBridgeString; var bool: Boolean): Boolean;
var
  rval: jsval;
begin
  Result := InternalEvaluate(code, rval);
  if (Result) then
    bool := (rval = JSVAL_TRUE);
end;

function TJSObject.Evaluate(const code: TBridgeString; var res: TJSObject): Boolean;
var
  rval: jsval;
  t: Pointer;
  p: PJSObject;
begin
  Result := InternalEvaluate(code, rval);
  if (Result) then
  begin
    JS_ValueToObject(FEngine.Context, rval, p);
    t := JS_GetPrivate(FEngine.Context, p);
    if (t <> nil) then
      res := TJSObject(t)
    else
      res := TJSObject.Create(p, FEngine, '');
  end;
end;

function TJSObject.GetFunction(const name: TBridgeString): TJSFunction;
var
  rval: jsval;
  jsfun: PJSFunction;
begin
  Result := nil;

  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval);
  {$ENDIF}
  if (rval = JSVAL_VOID) or (rval = JSVAL_NULL) then
    exit;

  jsfun := JS_ValueToFunction(FEngine.Context, rval);
  if (jsfun <> nil) then
    Result := TJSFunction.Create(jsfun, FEngine, name, self);
end;

function TJSObject.GetMethodResult(const name: TBridgeString): TResultType;
var
  meth: PMethodData;
begin
  meth := FMethods[name];
  if (meth <> nil) then
    Result := meth^.ResultType
  else
    Result := rtNone;
end;

function TJSObject.GetNativeProperty(Obj: TObject; AName: TBridgeString): jsval;
var
  ts: TBridgeString;
  PInfo: PPropInfo;
  methods: TNamedArray;
  jsobj: PJSObject;
begin
  CheckConnection;
  (* Method calls also come here to get the jsval for object functions *)
  PInfo := GetPropInfo(Obj.ClassInfo, AName);
  if (PInfo <> nil) then
    case PInfo^.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
      tkString,
      tkLString,
      tkWString:
        begin
          {$IFNDEF JSUnicode}
          ts := TBridgeString(GetStrProp(Obj, AName));
          Result := jsval(JS_NewStringCopyN(FEngine.Context, PBridgeChar(ts), Length(ts))) or JSVAL_STRING;
          {$ELSE}
            {$IFNDEF D6OR7}
            // Delphi 5 encapsulates the GetWideStrProp inside of GetStrProp
            ts := TBridgeString(GetStrProp(Obj, AName));
            {$ELSE}
            ts := TBridgeString(GetWideStrProp(Obj, AName));
            {$ENDIF}
            Result := jsval(JS_NewUCStringCopyN(FEngine.Context, PBridgeChar(ts), Length(ts))) or JSVAL_STRING;
          {$ENDIF}
        end;
      tkInteger, tkChar, tkWChar, tkClass, tkEnumeration:
        Result := IntToJSVal(GetOrdProp(Obj, AName));
    else
      Result := JSVAL_NULL;
    end
  else
  begin
    methods := TNamedArray(FNatives[Obj]);
    jsobj := methods[AName];
    if (jsobj = nil) then
    begin
      {$IFDEF D6OR7}JS_ReportError(FEngine.Context, PChar(String(AName +' is not a function.')));{$ENDIF}
      Result := JS_FALSE;
    end
    else
      Result := jsval(jsobj);
  end;
end;

function TJSObject.GetParamCount(const name: TBridgeString): Integer;
var
  meth: PMethodData;
begin
  meth := FMethods[name];
  if (meth <> nil) then
    Result := meth^.ParamCount
  else
    Result := -1;
end;

function TJSObject.GetProperty(const name: TBridgeString; var int: Integer): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    JS_ValueToInt32(FEngine.Context, rval, @int)
  else
    int := JSVAL_NULL;
end;

function TJSObject.GetProperty(const name: TBridgeString; var dbl: Double): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl)
  else
    dbl := JSVAL_NULL;
end;

function TJSObject.GetProperty(const name: TBridgeString; var ret: TJSObject): Boolean;
var
  rval: jsval;
  p: PJSObject;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
  begin
    JS_ValueToObject(FEngine.Context, rval, p);
    (* !!!
     * This is wasteful.  We need to figure out how to find existing wrappers
     * for instance |p|.
     *)
    ret := TJSObject.Create(p, FEngine, name, self);
  end
  else
    ret := nil;
end;

function TJSObject.GetProperty(const name: TBridgeString; var bool: Boolean): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    bool := JSValToBoolean(rval)
  else
    bool := false;
end;

function TJSObject.GetProperty(const name: TBridgeString; var str: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
  begin
    {$IFNDEF JSUnicode}
    str := JS_GetStringBytes(JS_ValueToString(FEngine.Context, rval));
    {$ELSE}
    str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
    {$ENDIF}
    {$IFDEF D6OR7}
    UniqueString(str);
    {$ENDIF}
  end
  else
    str := '';
end;

function TJSObject.HasMethodInfo(const name: TBridgeString): Boolean;
begin
  Result := (FMethods[name] <> nil);
end;

function TJSObject.HasNativeProperty(Obj: TObject; const AName: TBridgeString; strict: Boolean): Boolean;
var
  PInfo: PPropInfo;
  methods: TNamedArray;
  jsobj: PJSObject;
begin
  CheckConnection;
  PInfo := GetPropInfo(Obj.ClassInfo, AName);
  Result := (PInfo <> nil);

  if (not Result) and (not strict) then // if (not strict) we can check for methods as well
  begin
    // Check method table
    methods := TNamedArray(FNatives[Obj]);
    jsobj := methods[AName];
    if (jsobj <> nil) then
      Result := true;
  end;
end;

function TJSObject.HasProperty(const name: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval);
  {$ENDIF}
  Result := (rval <> JSVAL_VOID);
end;

procedure TJSObject.Init;
begin
  FNatives := TPtrArray.Create;
  FNatives.OwnsValues := true;
  FMethods := TNamedArray.Create;
  FMethods.OwnsValues := true;
end;

procedure TJSObject.InternalConnect;
begin
  if (FJSObj = nil) then
  begin
    FJSObj := JS_NewObject(FEngine.Context, @intf_general_class, nil, nil);
    FJSVal := JSObjectToJSVal(FJSObj);
    FScope := FEngine.Global;
  end;

  if (not IsLive) and (CanGoLive) then
  begin
    JS_RemoveRoot(FEngine.Context, @FJSVal);
    FScope.SetProperty(FName, self);
  end;
end;

function TJSObject.InternalEvaluate(const code: TBridgeString; var rval: jsval): Boolean;
begin
  CheckConnection;

  {$IFNDEF JSUnicode}
  Result := (JS_EvaluateScript(FEngine.Context, FJSObj, PBridgeChar(code), Length(code), nil, 0, @rval) = JS_TRUE);
  {$ELSE}
  Result := (JS_EvaluateUCScript(FEngine.Context, FJSObj, PBridgeChar(code), Length(code), nil, 0, @rval) = JS_TRUE);
  {$ENDIF}
end;

function TJSObject.IsFunction(const name: TBridgeString): Boolean;
var
  rval : jsval;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval);
  {$ENDIF}
  if (rval <> JSVAL_VOID) then
    Result := (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_FUNCTION)
  else
    Result := false;
end;

function TJSObject.IsInteger(const name: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval);
  {$ENDIF}
  if (rval <> JSVAL_VOID) then
    Result := JSValIsInt(rval)
  else
    Result := false;
end;

procedure TJSObject.RemoveObject(Obj: TJSBase);
var
  parent: PJSObject;
  {$IFDEF JSUnicode}
  rval: jsval;
  {$ENDIF}
begin
  CheckConnection;
  parent := Obj.Parent.JSObject;
  {$IFNDEF JSUnicode}
  JS_DeleteProperty(FEngine.Context, parent, PBridgeChar(Obj.JSName));
  {$ELSE}
  JS_DeleteUCProperty2(FEngine.Context, parent, PBridgeChar(Obj.JSName), Length(Obj.JSName), @rval);
  {$ENDIF}
  Obj.Free;
end;

procedure TJSObject.SetMethodInfo(const name: TBridgeString; ParamCount: Integer; ResultType: TResultType);
var
  meth: PMethodData;
begin
  New(meth);
  meth^.ParamCount := ParamCount;
  meth^.ResultType := ResultType;
  
  FMethods.Add(name, meth);
end;

function TJSObject.SetNativeProperty(Obj: TObject; AName: TBridgeString; AValue: Variant): JSBool;
var
  PInfo: PPropInfo;
begin
  CheckConnection;
  PInfo := GetPropInfo(Obj.ClassInfo, AName);
  if (PInfo <> nil) then
  begin
    {$IFNDEF FPC}
    if (PInfo^.PropType^^.Kind <> tkClass) then
    {$ELSE}
    if (PInfo^.PropType^.Kind <> tkClass) then
    {$ENDIF}
      SetPropValue(Obj, AName, AValue);
    Result := JS_TRUE;
  end
  else
  begin
    // Set a JS error here
    Result := JS_FALSE;
  end;
end;

function TJSObject.SetProperty(const name: TBridgeString; val: TJSBase): Boolean;
begin
  CheckConnection;
  if (HasProperty(name)) then
    {$IFNDEF JSUnicode}
    Result := (JS_SetProperty(FEngine.Context, FJSObj, CreateAnsiString(name), @val.JScriptVal) = JS_TRUE)
    {$ELSE}
    Result := (JS_SetUCProperty(FEngine.Context, FJSObj, CreateWideString(name), Length(name), @val.JScriptVal) = JS_TRUE)
    {$ENDIF}
  else
    {$IFNDEF JSUnicode}
    Result := (JS_DefineProperty(FEngine.Context, FJSObj, CreateAnsiString(name), val.JScriptVal, nil, nil, JSPROP_ENUMERATE) = JS_TRUE);
    {$ELSE}
    Result := (JS_DefineUCProperty(FEngine.Context, FJSObj, CreateWideString(name), Length(name), val.JScriptVal, nil, nil, JSPROP_ENUMERATE) = JS_TRUE);
    {$ENDIF}
end;

function TJSObject.TypeOf(const name: TBridgeString): JSType;
var
  rval: jsval;
begin
  CheckConnection;
  if (FEngine.InternalGet(name, FJSObj, rval)) then
    Result := JS_TypeOfValue(FEngine.Context, rval)
  else
    Result := JSTYPE_VOID;
end;

{ TJSFunction }

function TJSFunction.Call(params: Array of TJSBase; var int: Integer): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    int := JSVAL_NULL;
    exit;
  end;

  int := JSValToInt(rval);
end;

function TJSFunction.Call(params: Array of TJSBase; var str: TBridgeString): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    str := '';
    exit;
  end;

  // TODO : check real return type before converting
  // it will crash in some case if the js function return an integer instead of a string

  str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
  {$IFDEF D6OR7}
  UniqueString(str);
  {$ENDIF}
end;

function TJSFunction.Call(params: Array of TJSBase; var dbl: Double): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    dbl := JSVAL_NULL;
    exit;
  end;

  if (not JSValIsNull(rval)) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl);
end;

function TJSFunction.Call(params: Array of TJSBase; var bool: Boolean): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    bool := false;
    exit;
  end;

  bool := JSValToBoolean(rval);
end;

function TJSFunction.Call(params: Array of TJSBase; var res: TJSObject): Boolean;
var
  rval: jsval;
  p: PJSObject;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    res := nil;
    exit;
  end;

  (*
   * This isn't complete yet.  We need to query the object's parent and name to make this work.
   *)
  JS_ValueToObject(FEngine.Context, rval, p);
  res := TJSObject.Create(p, FEngine, '');
end;

function TJSFunction.Call(var int: Integer): Boolean;
begin
  Result := Call([], int);
end;

function TJSFunction.Call(var str: TBridgeString): Boolean;
begin
  Result := Call([], str);
end;

function TJSFunction.Call(var dbl: Double): Boolean;
begin
  Result := Call([], dbl);
end;

function TJSFunction.Call(var bool: Boolean): Boolean;
begin
  Result := Call([], bool);
end;

function TJSFunction.Call(var res: TJSObject): Boolean;
begin
  Result := Call([], res);
end;

constructor TJSFunction.Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FJSFun := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSFunction.Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FJSFun := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

destructor TJSFunction.Destroy;
begin
  inherited;
end;

procedure TJSFunction.InternalConnect;
begin
  if (FJSFun = nil) then
    {$IFNDEF JSUnicode}
    FJSFun := JS_DefineFunction(FEngine.Context, FScope.JSObject, PBridgeChar(FName), FCall, FArgCount, JSPROP_ENUMERATE);
    {$ELSE}
    FJSFun := JS_DefineUCFunction(FEngine.Context, FScope.JSObject, PBridgeChar(FName), Length(FName), FCall, FArgCount, JSPROP_ENUMERATE);
    {$ENDIF}
  // not sure yet :)
end;

{ TJSScript }

function TJSScript.Compile(AEngine: TJSEngine; const Filename: TBridgeString = '';
  const lineNo: Cardinal = 1) : Boolean;
begin
  FScript := AEngine.Compile(FCode, Filename, lineNo);
  FCompiled := (FScript <> nil);
  Result := FCompiled;
end;

constructor TJSScript.Create;
begin
  FCode := '';
  FScript := nil;
end;

constructor TJSScript.Create(const ACode: TBridgeString);
begin
  FCode := ACode;
end;

constructor TJSScript.Create(const ACode: TBridgeString; AEngine: TJSEngine);
begin
  FCode := ACode;
  Compile(AEngine);
end;

function TJSScript.Execute(AEngine: TJSEngine) : Boolean;
begin
  Result := Execute(AEngine, AEngine.Global);
end;

function TJSScript.Execute(AEngine: TJSEngine; AScope: TJSObject) : Boolean;
begin
  Result := False;
  if (not FCompiled) then
    Compile(AEngine);
  if (not FCompiled) then
    Exit;
  Result := AEngine.InternalExecute(FScript, AScope);
end;

procedure TJSScript.LoadCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmOpenRead);
  try
    LoadCompiledFromStream(fs, AEngine);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.LoadCompiledFromStream(AStream: TStream; AEngine: TJSEngine);
var
  ms: TMemoryStream;
  xdr: PJSXDRState;
  data: PBridgeChar;
  len: size_t;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromStream(AStream);

    ms.Position := 0;
    data := ms.Memory;
    len := ms.Size;

    xdr := JS_XDRNewMem(AEngine.Context, JSXDR_DECODE);
    if (xdr <> nil) then
    begin
      JS_XDRMemSetData(xdr, data, len);
      JS_XDRScript(xdr, FScript);
    end;

    FCompiled := true;
    FCode := '';
  finally
    ms.Free;
  end;
end;

procedure TJSScript.LoadRaw(const AFile: TBridgeString);
var
  fs: TFileStream;
  BOM : array[0..3] of BYTE; // http://www.unicode.org/faq/utf_bom.html#25
  CodeString : string;
  CodeUCS4String : UCS4String;
  DataSize : Cardinal;
const
  UTF8BOM : array[0..2] of BYTE = ($EF, $BB, $BF);
  UTF16LEBOM : array[0..1] of BYTE = ($FF, $FE);
  UTF16BEBOM : array[0..1] of BYTE = ($FE, $FF);
  UTF32LEBOM : array[0..3] of BYTE = ($FF, $FE, $00, $00);
  UTF32BEBOM : array[0..3] of BYTE = ($00, $00, $FE, $FF);
begin
  fs := TFileStream.Create(AFile, fmOpenRead);
  {$IFDEF JSUnicode}
  ZeroMemory(@BOM[0], Length(BOM));
  fs.Read(BOM, 4);
  if CompareMem(@BOM[0], @UTF8BOM[0], Length(UTF8BOM)) then
  begin
    fs.Seek(Length(UTF8BOM), soFromBeginning);
    DataSize := fs.Size - Length(UTF8BOM);
    SetLength(CodeString, DataSize);
    fs.Read(CodeString[1], DataSize);
    FCode := UTF8Decode(CodeString);
  end
  else if CompareMem(@BOM[0], @UTF32LEBOM[0], Length(UTF32LEBOM)) then
  begin
    fs.Seek(Length(UTF32LEBOM), soFromBeginning);
    DataSize := fs.Size - Length(UTF32LEBOM);
    SetLength(CodeUCS4String, DataSize div SizeOf(UCS4Char));
    fs.Read(CodeUCS4String[1], DataSize);
    FCode := UCS4StringToWideString(CodeUCS4String);
  end
  else if CompareMem(@BOM[0], @UTF16LEBOM[0], Length(UTF16LEBOM)) then
  begin
    fs.Seek(Length(UTF16LEBOM), soFromBeginning);
    DataSize := fs.Size - Length(UTF16LEBOM);
    SetLength(FCode, DataSize div SizeOf(WideChar));
    fs.Read(FCode[1], DataSize);
  end
  else
  begin
    // TODO : do UTF16BE and UTF32BE reading
    fs.Seek(0, soFromBeginning);
    SetLength(CodeString, fs.Size);
    fs.Read(CodeString[1], fs.Size);
    FCode := CodeString;
  end;
  {$ELSE}
  SetLength(FCode, fs.Size);
  fs.Read(FCode[1], fs.Size);
  {$ENDIF}
  fs.Free;

  FCompiled := false;
  FScript := nil;
end;

procedure TJSScript.SaveCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmCreate);
  try
    SaveCompiledToStream(fs, AEngine);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.SaveCompiledToStream(AStream: TStream; AEngine: TJSEngine);
var
  xdr: PJSXDRState;
  data: Pointer;
  len: size_t;
begin
  if (not FCompiled) then
    Compile(AEngine);

  xdr := JS_XDRNewMem(AEngine.Context, JSXDR_ENCODE);
  if (xdr <> nil) and (JS_XDRScript(xdr, FScript) = JS_TRUE) then
  begin
    data := JS_XDRMemGetData(xdr, @len);
    AStream.Write(data^, len);
  end
  else
    raise Exception.Create('The compiled script code may be corrupted; unable to save it to disk.');
end;

procedure TJSScript.SaveRaw(const AFile: TBridgeString);
var
  S: TStringList;
begin
  // TODO : Fix unicode
  S := TStringList.Create;
  try
    S.Text := FCode;
    S.SaveToFile(AFile);
  finally
    S.Free;
  end;
end;

{$IFDEF JSDEBUGGER}

{ TJSDebugger }

procedure TJSDebugger.Connect(Engine: TJSEngine);
begin
  FEngine := Engine;
  FEngine.SetDebugger(Self);
  FConnected := true;

  JS_SetNewScriptHookProc(FEngine.Runtime, NewScriptHook, Self);
  JS_SetDebugErrorHook(FEngine.Runtime, XDebugHook, Self);
  JS_SetCallHook(FEngine.Runtime, CallHook, Self);
  JS_SetDebuggerHandler(FEngine.Runtime, TrapHandler, Self);  // "debugger" keyword encountered
  JS_SetExecuteHook(FEngine.Runtime, ExecHook, Self);
  JS_SetObjectHook(FEngine.Runtime, ObjectHook, Self);
  JS_SetThrowHook(FEngine.Runtime, TrapHandler, Self);
end;

constructor TJSDebugger.Create;
begin
  FStep := false;
  FEnabled := false;
end;

constructor TJSDebugger.Create(Engine: TJSEngine);
begin
  Connect(Engine);
end;

function TJSDebugger.DebugExecute(const Code: String; Frame: PJSStackFrame): String;
var
  rval: jsval;
  //ex: jsval;
  cx: PJSContext;
begin
  Result := '';

  cx := FEngine.Context;
  // May not need to save exception state
  //JS_GetPendingException(cx, @ex);
  //JS_ClearPendingException(cx);

  JS_EvaluateInStackFrame(cx, Frame, PChar(Code), Length(Code), '', 0, @rval);
  Result := JS_GetStringBytes(JS_ValueToString(cx, rval));

  //JS_SetPendingException(cx, ex);
end;

destructor TJSDebugger.Destroy;
begin
  Disconnect;
end;

procedure TJSDebugger.Disable;
begin
  FEnabled := false;
end;

procedure TJSDebugger.Disconnect;
begin
  JS_ClearAllWatchPoints(FEngine.Context);
  JS_ClearAllTraps(FEngine.Context);
end;

procedure TJSDebugger.Enable;
begin
  FEnabled := true;
end;

procedure TJSDebugger.Initialize;
begin
end;

function TJSDebugger.IsBusy: Boolean;
begin
  Result := (FIsTrap) or (FIsError);
end;

procedure TJSDebugger.IterateProps(cx: PJSContext; Win: TDebugMain; Node: TTreeNode; obj: jsval);
var
  list: PJSIdArray;
  curid: pjsid;
  val: jsval;
  prop: jsval;
  i: Integer;
  vname: String;
  N: TTreeNode;
  pobj: PJSObject;
begin
  JS_ValueToObject(cx, obj, pobj);

  if (pobj <> nil) then
    list := JS_Enumerate(cx, pobj)
  else
    list := nil;

  if (list = nil) or (list^.length = 0) then
    Win.AddSubProperty(Node, '(empty)')
  else
  begin
    curid := @list^.vector;

    for i := 0 to list^.length-1 do
    begin
      JS_IdToValue(cx, curid^, @val);
      JS_GetProperty(cx, pobj, JS_GetStringBytes(JS_ValueToString(cx, val)), @prop);
      vname := MakePropertyString(cx, val, prop);

      N := Win.AddSubProperty(Node, vname);

      if (JS_TypeOfValue(cx, prop) = JSTYPE_OBJECT) then
        IterateProps(cx, Win, N, prop);

      Inc(curid);
    end;
  end;
end;

function TJSDebugger.MakePropertyString(cx: PJSContext; id: jsid; val: jsval): String;
begin
  Result := JS_GetStringBytes(JS_ValueToString(cx, id));

  if (Result <> 'undefined') then
    case JS_TypeOfValue(cx, val) of
      JSTYPE_VOID: Result := Result +' = undefined';
      JSTYPE_OBJECT: Result := Result +' = object';
      JSTYPE_FUNCTION: Result := Result +' = function';
      JSTYPE_STRING: Result := Result +' = "' +JS_GetStringBytes(JS_ValueToString(cx, val)) +'"';
      JSTYPE_NUMBER,
      JSTYPE_BOOLEAN: Result := Result +' = ' +JS_GetStringBytes(JS_ValueToString(cx, val));
    end;
end;

function TJSDebugger.OnEnteringFunction(cx: PJSContext; fp: PJSStackFrame): Pointer;
var
  line: Integer;
  script: PJSScript;
begin
  Result := nil;
  if (not FEnabled) or (not FStep) then exit;

  line := -1;
  script := JS_GetFrameScript(cx, fp);
  if (script <> nil) then
    line := JS_PCToLineNumber(cx, script, JS_GetFramePC(cx, fp));
  ShowDebugWindow(cx, '', line, dtTrace);
  Result := Self;   // Forces a call back when exiting the function
end;

function TJSDebugger.OnError(cx: PJSContext; Message: PChar; Report: PJSErrorReport): JSBool;
begin
  (* Return JS_TRUE to pass the error on to the default error handler *)
  Result := JS_TRUE;
  if (not FEnabled) or (IsBusy) then exit;

  ShowDebugWindow(cx, Message, Report^.lineno, dtError);
  JS_ClearPendingException(cx);
  Result := JS_TRUE;
end;

function TJSDebugger.OnExitingFunction(cx: PJSContext; fp: PJSStackFrame): Pointer;
(*
var
  line: Integer;
  script: PJSScript;
*)
begin
  Result := nil;
  (* An error occurs below for some odd reason

  if (not FEnabled) or (not FStep) then exit;

  line := -1;
  script := JS_GetFrameScript(cx, fp);
  if (script <> nil) then
    line := JS_PCToLineNumber(cx, script, JS_GetFramePC(cx, fp));
  ShowDebugWindow(cx, '', line, dtTrace);
  *)
end;

procedure TJSDebugger.OnNewScript(script: PJSScript);
var
  Len: Integer;
begin
  Len := Length(FScripts);
  SetLength(FScripts, Len+1);
  FScripts[Len] := script;
end;

procedure TJSDebugger.OnObject(cx: PJSContext; obj: PJSObject; isNew: JSBool);
begin
  (* Todo
   * isNew = JS_TRUE when the object is being created, otherwise is JS_FALSE
   * obj may or may not have a name.  Calls via JS_NewObject (and the engine's
   * internal equivalent) initialize anonymous objects.
   * This event can be used to detect the creation of new functions as well.
   *)
end;

function TJSDebugger.OnStartExecution(cx: PJSContext; fp: PJSStackFrame): Pointer;
var
  line: Integer;
  script: PJSScript;
begin
  Result := nil;
  if (not FEnabled) or (not FStep) then exit;

  line := -1;
  script := JS_GetFrameScript(cx, fp);
  if (script <> nil) then
    line := JS_PCToLineNumber(cx, script, JS_GetFramePC(cx, fp));
  ShowDebugWindow(cx, '', line, dtTrace);
  Result := Self;   // Forces a call back when exiting the function
end;

function TJSDebugger.OnStopExecution(cx: PJSContext; fp: PJSStackFrame): Pointer;
var
  line: Integer;
  script: PJSScript;
begin
  Result := nil;
  if (not FEnabled) or (not FStep) then exit;

  line := -1;
  script := JS_GetFrameScript(cx, fp);
  if (script <> nil) then
    line := JS_PCToLineNumber(cx, script, JS_GetFramePC(cx, fp));
  ShowDebugWindow(cx, '', line, dtTrace);
end;

function TJSDebugger.OnTrapHandler(cx: PJSContext; script: PJSScript; pc: pjsbytecode; rval: pjsval): Cardinal;
var
  line: Integer;
begin
  Result := JSTRAP_CONTINUE;
  if (not FEnabled) or (IsBusy) then exit;

  line := JS_PCToLineNumber(cx, script, pc);
  case (ShowDebugWindow(cx, '', line-1, dtBreakpoint)) of
    dtContinue: Result := JSTRAP_CONTINUE;
    dtStepInto: begin end;  // not yet implemented
    dtStepOver: begin end;  // not sure yet
    dtAbort: Result := JSTRAP_ERROR;
  end;

  (*
   * The step into and step over values should be reasonably easy to
   * implement with the FStep variable in conjunction with
   * OnEnterFunction and OnExitFunction methods.
   *)
end;

function TJSDebugger.SetBreakpoint(Line: Integer): Integer;
var
  pc: pjsbytecode;
  i: Integer;
begin
  for i := 0 to Length(FScripts)-1 do
  begin
    pc := JS_LineNumberToPC(FEngine.Context, FScripts[i], Line);
    Result := JS_PCToLineNumber(FEngine.Context, FScripts[i], pc);
    if (Result = Line) then
    begin
      JS_SetTrap(FEngine.Context, FScripts[i], pc, TrapHandler, Self);
      exit;
    end;
  end;

  Result := -1;
end;

procedure TJSDebugger.SetCode(const Code: String);
begin
  FCode := Code;
end;

function TJSDebugger.ShowDebugWindow(cx: PJSContext; ErrMsg: PChar; Line: Integer; _Type: TDebugType): TDebugResult;
var
  fp: PJSStackFrame;
  //obj: PJSObject;
  fun: PJSFunction;
  pp: PJSScopeProperty;
  pd: PJSPropertyDesc;
  fname: String;
  //oname: String;
  win: TDebugMain;
  tline: Integer;
  script: PJSScript;
  code: String;
  N: TTreeNode;
  vname: String;
begin
  win := TDebugMain.Create(nil);

  win.SetType(_Type);
  win.SetMessage(ErrMsg);
  win.SetCode(FCode);
  win.GotoLine(Line);
  win.ExecuteProc := DebugExecute;

  (* Scan the call stack *)
  fp := nil;
  repeat
    fp := JS_FrameIterator(cx, @fp);
    if (fp <> nil) then
    begin
      script := JS_GetFrameScript(cx, fp);
      if (script <> nil) then
      begin
        (* JS_DecompileScript() throws an exception here, so ignore for now *)
        (*
         * One problem with decompiling the given script is that each function
         * has it's own script.  To properly piece together all of the code
         * that is currently executing, use JS_GetScriptFilename() to identify
         * each individual file that may be running, then
         * JS_GetScriptBaseLineNumber() to figure out the order of each script
         * within its file.
         *)

        code := '';

        tline := JS_PCToLineNumber(cx, script, JS_GetFramePC(cx, fp));
        fun := JS_GetFrameFunction(cx, fp);

        if (fun <> nil) then
        begin
          fname := JS_GetStringBytes(JS_GetFunctionId(fun));

          (* Use this code to determine the function caller
          obj := JS_GetFrameCallObject(cx, fp);
          if (obj <> nil) then
            oname := JS_GetStringBytes(JS_ValueToString(cx, JSObjectToJSVal(obj)))
          else
            oname := '[global]';
          *)

          win.AddCallStack(fname, code, fp, tline);
        end;
      end;
    end;
  until (fp = nil);

  (* Iterate through all live objects *)
  pp := nil;
  repeat
    pp := JS_PropertyIterator(FEngine.Global.JSObject, @pp);
    if (pp <> nil) then
    begin
      New(pd);
      try
        JS_GetPropertyDesc(cx, FEngine.Global.JSObject, pp, pd);
        vname := MakePropertyString(cx, pd^.id, pd^.value);
        N := win.AddProperty(vname);

        if (JS_TypeOfValue(cx, pd^.value) = JSTYPE_OBJECT) then
          IterateProps(cx, win, N, pd^.value);
      finally
        Dispose(pd);
      end;
    end;
  until (pp = nil);

  try
    win.ShowModal;
    Result := Win.Result;
  finally
    win.Free;
  end;
end;

procedure TJSDebugger.StartStepping;
begin
  FStep := true;
end;

procedure TJSDebugger.StopStepping;
begin
  FStep := false;
end;

{$ENDIF} // JSDEBUGGER

{ TJSClass }

{ Unused code
constructor TJSClass.Create(AEngine: TJSEngine; const AName: TBridgeString; AClass: TClass);
begin

end;

constructor TJSClass.Create(AEngine: TJSEngine; const AName: TBridgeString; AClass: TClass; ArgCount: Integer);
begin

end;

constructor TJSClass.Create(AEngine: TJSEngine; const AName: TBridgeString; AClass: TClass; ArgCount: Integer; ParamTypes: array of TNativeParam);
begin

end;

destructor TJSClass.Destroy;
begin

  inherited;
end;

procedure TJSClass.InternalConnect;
begin
  inherited;

end;
}

end.

