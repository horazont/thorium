(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thorium.pas
** Last update: 2010-03-14
This file is part of the Thorium Scripting Language Project.

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms
of the GNU General Public license (the  "GPL License"), in which case the
provisions of GPL License are applicable instead of those
above.

For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
j.wielicki@sotecware.net

For a Changelog see the CHANGELOG.txt.

Special thanks go to (alphabetical order):
* Delphic
  For his great tutorial about scripting languages on delphigl.
  <http://wiki.delphigl.com/index.php/Tutorial_Scriptsprachen_Teil_1>
  <http://wiki.delphigl.com/index.php/Tutorial_Scriptsprachen_Teil_2>
* those in #delphigl on the euirc network
  For not kicking me when I jumped in and asked weird questions about asm stuff
  and design concepts. And of course for the answers ;)
* delphigl.com
  A great german Pascal and OpenGL community.
* Kevin Niehage
  For a lot of information about calling conventions on 32bit platforms.
* littleDave
  For surviving my attack with a lot of private messages concerning how to
  understand and implement a scripting language.
*******************************************************************************)
unit Thorium;

{$mode objfpc}{$H+}
{$asmmode att}
{$C-}

// Used to measure the time needed by each instruction. Produces a lot of
// overhead and console output. Only for internal purposes. And wont work on
// linux... So I could kick it out actually.
{.$define Timecheck}

// Used for internal debug purposes. Produces a lot of overhead
{$define DebugToConsole}
{$ifdef DebugToConsole}
  // Produce a Thorium stackdump after each instruction.
  {$define Stackdump}
  // Output the name of each instruction before it gets executed.
  {$define InstructionDump}
  // Produce a VM statedump after each instruction
  {$define StateDump}
  // Catch SIGUSR1 and print current state information
  {.$define HookSIGUSR1}
{$endif}

// Hard debugging makes it easier for debugging tools to trace the code,
// however, it highly degrades performance. Do use only if really needed ;)
{$define HardDebug}
{$ifdef HardDebug}
  // Do not apply optimizations when debugging is enabled to not confuse GDB…
  {$OPTIMIZATION OFF}
  // Do not perform inlining to stop confusing debugging software
  {$inline off}
{$else}
  // Make sure the compiler applies some optimizations, e.g. jumptables for cases
  // (NEEDED in TThoriumVirtualMachine.ExecuteInstruction).
  {$define BUG0011900}
  {$ifdef BUG0011900}
  {$OPTIMIZATION ON}
  {$else}
  {$OPTIMIZATION LEVEL3}
  {$endif}
  {$OPTIMIZATION REGVAR}
  // Enable inlining for better performance
  {$inline on}
{$endif}

// This switch controls whether to use the zlib to allow compression of modules.
// It is quite efficient since the Thorium instructions are optimized for speed,
// not for size and thus they contain a lot of unused space the zlib algorithm
// is able to eliminate.
{$define AllowCompression}
{$ifdef AllowCompression}
  // When this switch is enabled, the "compress" switch of modules is by default
  // set to true. Recommended when compression is enabled.
  {$define CompressByDefault}
{$endif}

// Check for endianess of target platform. See the compiler directives below for
// more infos.
{$ifndef ENDIAN_LITTLE}
{$warning Could not detect little endian platform for sure.}
{$ifdef ENDIAN_BIG}
{$error BigEndian plattform *will* cause problems with Thorium NativeCall. So fasten your seatbelts, get popcorn and watch the show.}
{$warning You may disable the line above BUT THE USAGE OF THORIUM IS ON YOUR OWN RISK THEN. YOU ARE USING A COMPLETELY UNTESTED AND UNSUPORRTED FEATURE!}
{$hint And you damn freak, who has bigendian nowadays AND wants to compile thorium?}
{$note The problem is that I am too lazy to implement the neccessary target location change code in the Precompile function. You will need to modifiy it to make it get the correct offsets of integer. And you need to pray that there won't be any problems with pointers and floats. There I cannot help you.}
{$endif}
{$endif}

interface

uses
  Classes, SysUtils, thorium_globals, thorium_utils, typinfo, Variants, md5,
  contnrs, fgl, Math
  {$ifdef HookSIGUSR1}
    , BaseUnix
  {$endif}
  {$ifdef AllowCompression}
    {$ifdef FPC}
      , zstream
    {$else}
      , zlib
    {$endif}
  {$endif}
  {$ifdef Timecheck}, Windows{$endif};

type
(*
   Region: Forward declarations
   Description: This region just contains some forward declarations of classes.
                                                                              *)
{%REGION 'Forward declarations' /fold}
  TThoriumPersistent = class;
  TThoriumParameters = class;
  TThoriumType = class;
  TThoriumIdentifierTable = class;
  TThoriumPublicValue = class;
  TThoriumFunction = class;
  TThoriumVariable = class;
  TThoriumIntList = class;
  TThoriumIntStack = class;
  TThoriumJumpList = class;
  TThoriumHostParameters = class;
  TThoriumHostCallableBase = class;
  TThoriumHostFunctionBase = class;
  TThoriumHostMethodBase = class;
  TThoriumHostObjectType = class;
  TThoriumRTTIObjectType = class;
  TThoriumInstructions = class;
  TThoriumModule = class;
  TThoriumLibraryConstant = class;
  TThoriumLibraryProperty = class;
  TThoriumLibrary = class;
  TThoriumRuntimeModule = class;
  TThoriumRuntimeFunction = class;
  TThoriumVirtualMachine = class;
  TThoriumDebuggingVirtualMachine = class;
  TThorium = class;
{%ENDREGION}

(*
   Region: Thorium exceptions
   Declarations: EThoriumException, EThoriumCompilerException,
                 EThoriumVerificationException, EThoriumDependencyException,
                 EThoriumHashException, EThoriumRuntimeException
   Description: Exceptions thrown by Thorium.
                                                                              *)
{%REGION 'Thorium exceptions' /fold}

  (* Generic thorium exception, which is only rarely raised directly. Mostly the
     other types will be used. *)
  EThoriumException = class (Exception);
    (* An exception thrown by the compiler. Should occur rarely and if, please
       notifiy the author! *)
    EThoriumCompilerException = class (EThoriumException);
      (* Thrown when a unrecoverable compiler error occurs (i.e. syntax error
         or something *)
      EThoriumCompilerError = class (EThoriumCompilerException);
    (* This exception occurs during the verification phase of a loaded or
       compiled-for-dependency module. *)
    EThoriumVerificationException = class (EThoriumException);
      (* This exception will be raised when a dependency needed to load a module
         or a library is not present. *)
      EThoriumDependencyException = class (EThoriumVerificationException);
      (* This excpetion will be raised when a hash check for a module, library,
         function, property or class type failes. *)
      EThoriumHashException = class (EThoriumVerificationException);
    (* This exception is raised by the virtual machine or any other Thorium
       runtime environment components when something goes wrong. *)
    EThoriumRuntimeException = class (EThoriumException);
      (* This exception is raised when an instruction raises an exception. You
         may obtain more information about the original exception using the
         respective property. *)
      EThoriumRuntimeExecutionException = class (EThoriumRuntimeException)
      public
        constructor Create(Module: TThoriumRuntimeModule;
          InstructionAddr: TThoriumInstructionAddress; Instruction: PThoriumInstruction;
          OriginalException: Exception);
        destructor Destroy; override;
      private
        FOriginalException: Exception;
      public
        property OriginalException: Exception read FOriginalException;
      end;
    EThoriumDebuggerException = class (EThoriumException);

    (* Raised e.g. if a user tries to initalize a direct property with a
       variable of a type different to the type supplied. *)
    EThoriumConsistencyError = class (EThoriumException);

{%ENDREGION}

(*
   Region: Various external type & function stuff
   Declarations: TThoriumRTTIStaticMethods, TThoriumRTTIMethods,
                 TThoriumExternalFunctionVarType, TThoriumSimpleVarargs,
                 TThoriumHostRecordField, TThoriumHostRecordFields
   Description: These types are used in callbacks concerning host class types
                or in method / function declarations.
                                                                              *)
{%REGION 'Various external type & function stuff' /fold}

  (* An array containing references to host environment functions. *)
  TThoriumRTTIStaticMethods = array of TThoriumHostFunctionBase;

  (* An array containing references to host environment methods. *)
  TThoriumRTTIMethods = array of TThoriumHostMethodBase;

  (* This record completely defines a type of the host environment, including
     host class types. *)
  TThoriumHostType = record
    HostType: PTypeInfo;

    (* If storing is true, the script will call EnableHostControl when the
       parameter is passed to the function. *)
    Storing: Boolean;
  end;

  (* A pointer to a record which completely defines a type of the host
     environment. *)
  PThoriumHostType = ^TThoriumHostType;

  (* This record defines a field of an host environmental record. *)
  TThoriumHostRecordField = record
    FieldType: TThoriumHostType;
    FieldName: String;
    Offset: Cardinal;
  end;

  (* A pointer to a record field definition. *)
  PThoriumHostRecordField = ^TThoriumHostRecordField;

  (* An array of record field definitions. *)
  TThoriumHostRecordFields = array of TThoriumHostRecordField;

  (* A record which is used in simple function and method calls to represent
     an array of values. *)
  TThoriumSimpleVarargs = record
    Count: SizeUInt;
    Data: Pointer;
  end;

  (* A pointer to a record which represents an array of values. *)
  PThoriumSimpleVarargs = ^TThoriumSimpleVarargs;

  TThoriumNativeData = record
    ForType: PTypeInfo;
    Data: Pointer;
  end;

{%ENDREGION}

(*
   Region: Thorium values
                                                                              *)
{%REGION 'Thorium values' /fold}
  PThoriumValue = ^TThoriumValue;
  PThoriumArray = ^TThoriumArray;
  PThoriumStaticArray = ^TThoriumStaticArray;
  PThoriumStruct = ^TThoriumStruct;

  TThoriumValue = record
    // Should be available all the time for all types which need special
    // handling (e.g. custom free functions etc.)
    RTTI: TThoriumType;
    References: LongInt;
    NativeData: TThoriumNativeData;
  case Byte of
    0: (Int: TThoriumInteger);
    1: (Float: TThoriumFloat);
    2: (Str: PThoriumString);
    3: (Func: TThoriumFunction);
    4: (HostFunc: TThoriumHostFunctionBase);
    5: (HostMethod: TThoriumHostMethodBase);
    6: (Struct: PThoriumStruct);
    7: (HostObject: Pointer);
    8: (DynamicArray: PThoriumArray);
   11: (StaticArray: PThoriumStaticArray);
    9: (RuntimeFunc: TThoriumRuntimeFunction);
   10: (Ptr: PThoriumValue);
  end;

  TThoriumArray = array of TThoriumValue;
  TThoriumStaticArray = PThoriumValue;
  TThoriumStruct = PThoriumValue;

(*  TThoriumCompileTimeValue = record
    // Keep this as reference counter
    CTTI: TThoriumType;
    Value: TThoriumValue;
  end;*)

{%ENDREGION}

(*
   Region: Events & callbacks
   Declarations: TThoriumOnPropertyGet, TThoriumOnPropertySet,
                 TThoriumOnPropertySetCallback, TThoriumOnRequireModule,
                 TThoriumOnCompilerOutput, TThoriumOnOpenModule,
                 TThoriumRTTIMethodsCallback, TThoriumRTTIStaticMethodsCallback,
                 TThoriumSimpleMethod, TThoriumClassMethod,
                 TThoriumInstructionFunc1R
   Description: These callbacks are used all over Thorium in various situations.
                                                                              *)
{%REGION 'Events & callbacks' /fold}
  TThoriumOnPropertyGet = procedure (Sender: TThoriumLibraryProperty;
    var AThoriumValue: TThoriumValue)  of object;
  TThoriumOnPropertySet = procedure (Sender: TThoriumLibraryProperty;
    const AThoriumValue: TThoriumValue) of object;
  TThoriumOnPropertySetCallback = procedure (Sender: TThoriumLibraryProperty;
    const AThoriumValue: TThoriumValue; var AllowSet: Boolean) of object;
  TThoriumOnRequireModule = procedure (Sender: TThorium;
    const Name: String; var ANewModule: TThoriumModule) of object;
  TThoriumOnCompilerOutput = procedure (Sender: TThorium;
    const Module: TThoriumModule; const Msg: String) of object;
  TThoriumOnOpenModule = procedure (Sender: TThorium; const ModuleName: String;
    var Stream: TStream) of object;
  TThoriumRTTIMethodsCallback = procedure (Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods) of object;
  TThoriumRTTIStaticMethodsCallback = procedure (Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIStaticMethods) of object;
  TThoriumSimpleMethod = procedure (const Input: array of Pointer;
    Result: PThoriumValue) of object;
  TThoriumClassMethod = procedure (const Input: array of Pointer;
    Result: PThoriumValue) of object;
  TThoriumDebugCallbackObject = procedure (Sender: TThoriumDebuggingVirtualMachine;
    Kind: TThoriumDebugEvent; Obj: TObject) of object;
  TThoriumDebugCallbackValue = procedure (Sender: TThoriumDebuggingVirtualMachine;
    Kind: TThoriumDebugEvent; Value: LongInt) of object;
  TThoriumVMResolveModulePtr = function (const AModuleRef: Pointer): TThoriumRuntimeModule of object;
  TThoriumVMResolveFunctionPtr = function (const AFunctionRef: Pointer): TThoriumRuntimeFunction of object;
  TThoriumHandleOffset = procedure (Sender: TObject; const Origin, Count: Integer; var Changed: Boolean) of object;


  (* Only internally used. *)
  TThoriumInstructionFunc1R = function (RI: Word): TThoriumInstruction;
{%ENDREGION}

(*
   Region: Thorium persistent baseclass
   Declarations: IThoriumPersistent, TThoriumPersistent
   Description: This section deals with the declaration of the base classes and
                interfaces used to publish host environment classes to the
                Thorium script.
                                                                              *)
{%REGION 'Thorium persistent baseclass' /fold}

  (* This interface is used by the Thorium engine to notify an object about
     references on the stack of the Thorium engine. It is a requirement to
     publish a class to Thorium (based on the simple RTTI interface). *)
  IThoriumPersistent = interface ['{AA693BE1-456A-448E-A290-E75A780AD57B}']
    procedure EnableHostControl;
    procedure DisableHostControl;
    procedure FreeReference;
    function GetReference: TObject;
  end;

  { TThoriumReferenceImplementation }

  TThoriumReferenceImplementation = class (TObject, IUnknown, IThoriumPersistent)
  public
    constructor Create(ATarget: TObject);
  private
    FHostControlled: Boolean;
    FReferences: LongInt;
    FTarget: TObject;
  protected // IUnknown
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    function QueryInterface(const IID: TGuid; out Obj): LongInt; stdcall;
  protected // IThoriumPersistent
    procedure EnableHostControl;
    procedure DisableHostControl;
    procedure FreeReference;
    function GetReference: TObject;
  end;

  { TThoriumPersistent }

  {$M+}
  (* This class is the easiest way to publish a class to Thorium. Any class
     derived from this one can be published using a TThoriumLibrary via the
     host RTTI object type. *)
  TThoriumPersistent = class (TPersistent, IUnknown, IThoriumPersistent)
  public
    {$ifdef DebugToConsole}
    procedure BeforeDestruction; override;
    {$endif}
    constructor Create;
    destructor Destroy; override;
  private
    FReferenceImplementation: TThoriumReferenceImplementation;
    FReference: IUnknown;
    FReference2: IThoriumPersistent;
  protected
    property Reference: IUnknown read FReference implements IUnknown;
  protected
    class procedure GetStaticMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIStaticMethods); virtual;
    class procedure GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods); virtual;
  public
    property ThoriumReference: IThoriumPersistent read FReference2 implements IThoriumPersistent;
  end;
  {$M-}
  TThoriumPersistentClass = class of TThoriumPersistent;
  PThoriumPersistent = ^TThoriumPersistent;

{%ENDREGION}

(*
   Region: Base classes
   Declarations: TThoriumHashableObject, TThoriumIntList, TThoriumIntStack,
                 TThoriumJumpList
   Description: This classes are used spread over the entire library.
                                                                              *)
{%REGION 'Base classes' /fold}

  { TThoriumHashableObject }

  (* This is the base class of all classes in Thorium which may be hashed to
     allow verification when loading a module. *)
  TThoriumHashableObject = class (TObject)
  public
    constructor Create;
  private
    FCalculatingHash: Boolean;
    FFrozen: Boolean;
    FHashGenerated: Boolean;
  protected
    FHash: TThoriumHash;
  protected
    procedure CalcHash; virtual; abstract;
    procedure ForceFrozen;
    procedure ForceUnfrozen;
    procedure HardUnfreeze;
    procedure InvalidateHash;
  public
    function GetHash: TThoriumHash;
    function GetHashCode: PtrInt; override;
  public
    procedure Freeze;
  end;

  (* Only internally used. *)

  { TThoriumIntList }

  TThoriumIntList = class (TObject)
    constructor Create;
    destructor Destroy; override;
  private
    FList: PInteger;
    FCapacity, FCount: Integer;

    function GetItem(Index: Integer): Integer;
    procedure Expand;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure SetItem(Index: Integer; Value: Integer);
  public
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
    property Count: Integer read FCount write SetCount;
    property Capacity: Integer read FCapacity write SetCapacity;

    function AddEntry(Value: Integer): Integer;
    function FindValue(AValue: Integer): Integer;
    procedure DeleteEntry(AIndex: Integer);
    procedure Clear;
  end;

  (* Only internally used. *)
  TThoriumIntStack = class (TThoriumIntList)
  public
    procedure Push(Value: Integer);
    function Pop: Integer;
  end;

  (* Only internally used. *)
  TThoriumJumpList = class (TThoriumIntList)
  public
    procedure FillAddresses(DownToCount: Integer; Address: TThoriumInstructionAddress;
      Instructions: TThoriumInstructions);
    procedure ChangeAddresses(Offset: Integer; AfterAddress: TThoriumInstructionAddress;
      Instructions: TThoriumInstructions);
  end;

{%ENDREGION}

(*
   Region: Thorium types
                                                                              *)
{%REGION 'Thorium types' /fold}
  TThoriumTypeKind = (tkSimple, tkFunction, tkHostFunction, tkHostMethod,
    tkHostType, tkStruct, tkArray, tkString);

  TThoriumOperationDescriptionKind = (okCast, okOperation, okAssignment,
    okCreation, okCustom);

  TThoriumCastDescription = record
    Needed: Boolean;
    Instruction: TThoriumInstructionCAST;
    TargetType: TThoriumType;
  end;

  TThoriumOperationInstructionDescription = record
    Value1RIOffset: Integer;
    Value2RIOffset: Integer;
    TargetRIOffset: Integer;
    Instruction: TThoriumInstructionREG;
  end;

  TThoriumOperationDescription = record
    Operation: TThoriumOperation;
    ResultType: TThoriumType;
    Casts: array [0..1] of TThoriumCastDescription;
    OperationInstruction: TThoriumOperationInstructionDescription;
  end;

  TThoriumAssignmentDescription = record
    Casting: Boolean;
    Cast: TThoriumCastDescription;
  end;

  TThoriumInitialData = record
  case Byte of
    0: (Int: Int64);
    1: (Flt: Double);
    2: (Bin: array [0..7] of Byte);
  end;

  TThoriumCreateInstructionDescription = record
    TargetRegisterOffset: Integer;
    Instruction: TThoriumInstructionREG;
  end;

  TThoriumCustomOperation = record
    Instructions: TThoriumInstructionArray;
  end;

  TThoriumGenericOperationRegister = (gorTarget, gorValue1, gorValue2);
  TThoriumGenericOperationRegisters = set of TThoriumGenericOperationRegister;

  TThoriumGenericOperation = record
    Kind: TThoriumOperationDescriptionKind;
    Cast: TThoriumCastDescription;
    Operation: TThoriumOperationDescription;
    Assignment: TThoriumAssignmentDescription;
    Creation: TThoriumCreateInstructionDescription;
    Custom: TThoriumCustomOperation;

    TargetRI, Value1RI, Value2RI: TThoriumRegisterID;
    ClearRegisters: TThoriumGenericOperationRegisters;
  end;

  TThoriumOperationArray = array of TThoriumGenericOperation;

  TThoriumNativeCallSpecification = record
    Offset: Integer;
    Mask: Integer;
    ValueMode: TThoriumNativeCallValueMode;
    FloatMode: TThoriumNativeCallFloatMode;
    RefMode: TThoriumNativeCallRefMode;
  end;

  {IThoriumNativeCallCompatible = interface ['{02E24A87-41F3-4D0A-98D7-F889E3FE2394}']
    function GetNativeCallSpecification(const HostType: PTypeInfo; out Spec: TThoriumNativeCallSpecification): Boolean;
    function GetParameterHostType(const I: Integer): PTypeInfo;
  end;}

  IThoriumCallable = interface ['{C9317686-61BE-4D72-AF95-9E0FF25C752E}']
    function GetParameters: TThoriumParameters;
    function GetReturnType: TThoriumType;
  end;

  IThoriumHostCallable = interface ['{14B6F0DA-D255-4A73-87E8-FC1AC5CA83A1}']
    function GetHostParameters: TThoriumHostParameters;
    function GetHostReturnType: TThoriumHostType;
    function NeedsNativeData: Boolean;
  end;

  TThoriumStructFieldDefinition = record
    Name: String;
    Offset: ptruint;
    ValueType: TThoriumType;
  end;

  TThoriumAccess = record
    Allowed: Boolean;
    ValueRIOffset: Integer;
    TargetRIOffset: Integer;
    IndexRIOffset: Integer;
    ExtendedRIOffset: Integer;
    Instruction: TThoriumInstructionREG;
  end;

  TThoriumAccessDefinition = record
    ReadAccess: TThoriumAccess;
    WriteAccess: TThoriumAccess;
  end;

  { TThoriumType }

  TThoriumType = class (TThoriumHashableObject, IUnknown)
  public
    constructor Create(AThorium: TThorium);
  private
    FName: String;
//    FLazy: Boolean;
    FReferences: LongInt;
  protected
    FThorium: TThorium;
  protected // IUnknown
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
  protected
    function GetNoneInitialData(out InitialData: TThoriumInitialData): Boolean; virtual;
    function GetName: String;
    function GetTypeKind: TThoriumTypeKind; virtual; abstract;
    procedure RaiseMissingTheObject;
  public // IUnknown
    function QueryInterface(const iid : tguid; out obj) : longint;stdcall;
  public
    property Name: String read GetName;
    property TypeKind: TThoriumTypeKind read GetTypeKind;
  public
    function CanAssignTo(var Assignment: TThoriumAssignmentDescription; const AnotherType: TThoriumType = nil): Boolean; virtual;
    function CanCall: Boolean; virtual;
    function CanCreate(const InitialData: TThoriumInitialData; const ToRegister: Boolean; out Instruction: TThoriumCreateInstructionDescription): Boolean; virtual;
    function CanCreateNone(const ToRegister: Boolean; out Instruction: TThoriumCreateInstructionDescription): Boolean; virtual;
    function CanPerformOperation(var Operation: TThoriumOperationDescription; const TheObject: TThoriumType = nil; const ExName: String = ''; const ExType: PTypeInfo = nil): Boolean; virtual;
    function CreateValueFromPtr(const Ptr: Pointer): TThoriumValue; virtual; abstract;
    function DuplicateValue(const Input: TThoriumValue): TThoriumValue; virtual;
    function Equals(Obj: TObject): boolean; override;
    function HasFieldAccess: Boolean; virtual;
    function HasIndexedAccess: Boolean; virtual;
    (*function GetClassType: TClass;*)
    (*function GetInstance: TThoriumType;*)
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; virtual; abstract;
    function NeedsClear: Boolean; virtual;
    function DoAddition(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoBitAnd(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoBitNot(const AValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoBitOr(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoBitShl(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoBitShr(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoBitXor(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoCast(const AValue: TThoriumValue; const TargetType: TThoriumType): TThoriumValue; virtual; abstract;
    procedure DoCastlessAssign(const ASource: Pointer; var ADest: TThoriumValue); virtual; abstract;
    function DoClone(const AValue: TThoriumValue): TThoriumValue; virtual;
    function DoCreate(const InitialData: TThoriumInitialData): TThoriumValue; virtual; abstract;
    function DoCreateNone: TThoriumValue; virtual; abstract;
    function DoCmpEqual(const AValue, BValue: TThoriumValue): Boolean; virtual; abstract;
    function DoCmpGreater(const AValue, BValue: TThoriumValue): Boolean; virtual; abstract;
    function DoCmpGreaterOrEqual(const AValue, BValue: TThoriumValue): Boolean; virtual;
    function DoCmpLess(const AValue, BValue: TThoriumValue): Boolean; virtual; abstract;
    function DoCmpLessOrEqual(const AValue, BValue: TThoriumValue): Boolean; virtual;
    function DoCmpNotEqual(const AValue, BValue: TThoriumValue): Boolean; virtual; abstract;
    procedure DoDecrement(var ASubject: TThoriumValue); virtual; abstract;
    function DoDeref(const AValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoDivision(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    procedure DoDevolatile(const ASubject: TThoriumValue); virtual; abstract;
    procedure DoEnableHostControl(const AValue: TThoriumValue); virtual;
    function DoEvaluate(const AValue: TThoriumValue): Boolean; virtual; abstract;
    procedure DoFree(var AValue: TThoriumValue); virtual; abstract;
    procedure DoFreeNative(var AValue: TThoriumValue); virtual; abstract;
    procedure DoFromNative(var AValue: TThoriumValue); virtual; abstract;
    function DoGetField(const AValue: TThoriumValue; const AFieldID: QWord): TThoriumValue; virtual; abstract;
    function DoGetIndexed(const AValue: TThoriumValue; const AIndex: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoGetLength(const AValue: TThoriumValue): Integer; virtual; abstract;
    function DoGetStaticField(const AFieldID: QWord): TThoriumValue; virtual; abstract;
    procedure DoIncrement(var ASubject: TThoriumValue); virtual; abstract;
    function DoIntegerDivision(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoLogicalAnd(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoLogicalOr(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    procedure DoLogicalNot(var ASubject: TThoriumValue); virtual; abstract;
    function DoLogicalXor(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    procedure DoNegate(var AValue: TThoriumValue); virtual; abstract;
    function DoModulus(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    function DoMultiplication(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    procedure DoSetField(const AValue: TThoriumValue; const AFieldID: QWord; const NewValue: TThoriumValue); virtual; abstract;
    procedure DoSetIndexed(const AValue: TThoriumValue; const AIndex: TThoriumValue; const NewValue: TThoriumValue); virtual; abstract;
    procedure DoSetStaticField(const AFieldID: QWord; const NewValue: TThoriumValue); virtual; abstract;
    function DoSubtraction(const AValue, BValue: TThoriumValue): TThoriumValue; virtual; abstract;
    procedure DoToNative(var AValue: TThoriumValue; const AType: PTypeInfo); virtual; abstract;
    function DoToString(const AValue: TThoriumValue): String; virtual;
    class function PerformOperation(const AValue: TThoriumValue; const Operation: TThoriumOperationDescription; const BValue: PThoriumValue = nil): TThoriumValue;
    class function PerformCmpOperation(const AValue: TThoriumValue; const Operation: TThoriumOperationDescription; const BValue: PThoriumValue = nil): Boolean;
    function ToString: ansistring; override;
    function UsesType(const AnotherType: TThoriumType; MayRecurse: Boolean = True): Boolean; virtual;
  end;
  TThoriumTypes = specialize TFPGList<TThoriumType>;

  { TThoriumTypeSimple }

  TThoriumTypeSimple = class (TThoriumType)
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    procedure DoFree(var AValue: TThoriumValue); override;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
  end;

  { TThoriumTypeInteger }

  TThoriumTypeInteger = class (TThoriumTypeSimple)
  protected
    function GetNoneInitialData(out InitialData: TThoriumInitialData
       ): Boolean; override;
  public
    function CreateFromInt(const Int: TThoriumInteger): TThoriumValue;
  public
    function CanAssignTo(var Assignment: TThoriumAssignmentDescription;
       const AnotherType: TThoriumType=nil): Boolean; override;
    function CanCreate(const InitialData: TThoriumInitialData;
       const ToRegister: Boolean; out
       Instruction: TThoriumCreateInstructionDescription): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function CreateValueFromPtr(const Ptr: Pointer): TThoriumValue; override;

    function DoAddition(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitAnd(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitOr(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitXor(const AValue, BValue: TThoriumValue): TThoriumValue;
         override;
    function DoBitShr(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitShl(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitNot(const AValue: TThoriumValue): TThoriumValue;
       override;
    function DoCast(const AValue: TThoriumValue; const TargetType: TThoriumType
       ): TThoriumValue; override;
    function DoCmpEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreater(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreaterOrEqual(const AValue, BValue: TThoriumValue
       ): Boolean; override;
    function DoCmpLess(const AValue, BValue: TThoriumValue): Boolean; override;
    function DoCmpLessOrEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpNotEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCreate(const InitialData: TThoriumInitialData): TThoriumValue;
       override;
    procedure DoDecrement(var ASubject: TThoriumValue); override;
    function DoDivision(const AValue, BValue: TThoriumValue): TThoriumValue;
         override;
    function DoEvaluate(const AValue: TThoriumValue): Boolean; override;
    procedure DoFreeNative(var AValue: TThoriumValue); override;
    procedure DoFromNative(var AValue: TThoriumValue); override;
    procedure DoIncrement(var ASubject: TThoriumValue); override;
    function DoIntegerDivision(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    function DoModulus(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoMultiplication(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    procedure DoNegate(var AValue: TThoriumValue); override;
    function DoSubtraction(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    procedure DoToNative(var AValue: TThoriumValue; const AType: PTypeInfo);
       override;
    function DoToString(const AValue: TThoriumValue): String; override;
  end;

  { TThoriumTypeFloat }

  TThoriumTypeFloat = class (TThoriumTypeSimple)
  protected
    function GetNoneInitialData(out InitialData: TThoriumInitialData
       ): Boolean; override;
  public
    function CanCreate(const InitialData: TThoriumInitialData;
       const ToRegister: Boolean; out
       Instruction: TThoriumCreateInstructionDescription): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function CreateValueFromPtr(const Ptr: Pointer): TThoriumValue; override;

    function DoAddition(const AValue, BValue: TThoriumValue): TThoriumValue;
         override;
    function DoCmpEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreater(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreaterOrEqual(const AValue, BValue: TThoriumValue
       ): Boolean; override;
    function DoCmpLess(const AValue, BValue: TThoriumValue): Boolean; override;
    function DoCmpLessOrEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpNotEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCreate(const InitialData: TThoriumInitialData
         ): TThoriumValue; override;
    procedure DoDecrement(var ASubject: TThoriumValue); override;
    function DoDivision(const AValue, BValue: TThoriumValue): TThoriumValue;
        override;
    procedure DoFreeNative(var AValue: TThoriumValue); override;
    procedure DoFromNative(var AValue: TThoriumValue); override;
    procedure DoIncrement(var ASubject: TThoriumValue); override;
    procedure DoNegate(var AValue: TThoriumValue); override;
    function DoMultiplication(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    function DoSubtraction(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    procedure DoToNative(var AValue: TThoriumValue; const AType: PTypeInfo); override;
    function DoToString(const AValue: TThoriumValue): String; override;
  end;

  { TThoriumTypeString }

  TThoriumTypeString = class (TThoriumTypeSimple)
  protected
    function GetNoneInitialData(out InitialData: TThoriumInitialData
       ): Boolean; override;
    function GetTypeKind: TThoriumTypeKind; override;
  public
    function CreateFromString(const Str: String): TThoriumValue;
  public
    function CanCreate(const InitialData: TThoriumInitialData;
       const ToRegister: Boolean; out
       Instruction: TThoriumCreateInstructionDescription): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String='';
       const ExType: PTypeInfo = nil): Boolean; override;
    function CreateValueFromPtr(const Ptr: Pointer): TThoriumValue; override;
    function HasIndexedAccess: Boolean; override;
    function HasFieldAccess: Boolean; override;
    function NeedsClear: Boolean; override;

    function DoAddition(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoCmpEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreater(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreaterOrEqual(const AValue, BValue: TThoriumValue
       ): Boolean; override;
    function DoCmpLess(const AValue, BValue: TThoriumValue): Boolean; override;
    function DoCmpLessOrEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpNotEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    procedure DoFree(var AValue: TThoriumValue); override;
    procedure DoFreeNative(var AValue: TThoriumValue); override;
    procedure DoFromNative(var AValue: TThoriumValue); override;
    function DoGetField(const AValue: TThoriumValue; const AFieldID: QWord
       ): TThoriumValue; override;
    function DoGetIndexed(const AValue: TThoriumValue; const AIndex: TThoriumValue
       ): TThoriumValue; override;
    procedure DoToNative(var AValue: TThoriumValue; const AType: PTypeInfo);
       override;
    function DoToString(const AValue: TThoriumValue): String; override;
  end;

  { TThoriumTypeFunction }

  TThoriumTypeFunction = class (TThoriumType, IThoriumCallable)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FParameters: TThoriumParameters;
    FReturnType: TThoriumType;
    function GetHasReturnValue: Boolean;
    function GetHasReturnValueInt: Integer;
  protected
    procedure Assign(ASource: TThoriumTypeFunction);
    function GetTypeKind: TThoriumTypeKind; override;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function GetParameters: TThoriumParameters;
    function GetReturnType: TThoriumType;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
  published
    property HasReturnValue: Boolean read GetHasReturnValue;
    property HasReturnValueInt: Integer read GetHasReturnValueInt;
    property Parameters: TThoriumParameters read FParameters;
    property ReturnType: TThoriumType read FReturnType write FReturnType;
  end;

  { TThoriumTypeHostFunction }

  TThoriumTypeHostFunction = class (TThoriumType, IThoriumCallable, IThoriumHostCallable)
  public
    constructor Create(const AThorium: TThorium; const AHostFunction: TThoriumHostCallableBase);
    destructor Destroy; override;
  private
    FHostFunction: TThoriumHostCallableBase;
    FParameters: TThoriumParameters;
    FReturnType: TThoriumType;
    function GetHasReturnValue: Boolean;
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function GetParameters: TThoriumParameters;
    function GetHostParameters: TThoriumHostParameters;
    function GetReturnType: TThoriumType;
    function GetHostReturnType: TThoriumHostType;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
    function NeedsNativeData: Boolean;
  published
    property HasReturnValue: Boolean read GetHasReturnValue;
    property Parameters: TThoriumParameters read FParameters;
    property ReturnType: TThoriumType read FReturnType;
    property HostFunction: TThoriumHostCallableBase read FHostFunction;
  end;

  { TThoriumTypeHostType }

  TThoriumTypeHostType = class (TThoriumType)
  public
    constructor Create(const AThorium: TThorium; const AHostType: TThoriumHostObjectType);
  private
    FHostType: TThoriumHostObjectType;
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    property HostType: TThoriumHostObjectType read FHostType;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
  end;

  { TThoriumTypeStruct }

  TThoriumTypeStruct = class (TThoriumType)
  public
    constructor Create(AThorium: TThorium);
    destructor Destroy; override;
  private
    FCount: Integer;
    FFields: array of TThoriumStructFieldDefinition;

    procedure Expand;
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    function Add(const AName: String;
      const ValueType: TThoriumType): Integer;
    function Add(const AReference: TThoriumStructFieldDefinition): Integer;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    procedure Delete(const AIndex: Integer);
    function IndexOf(const AName: String): Integer;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
    function NeedsClear: Boolean; override;
    function UsesType(const AnotherType: TThoriumType; MayRecurse: Boolean=True
       ): Boolean; override;
  end;

  { TThoriumTypeArray }

  TThoriumTypeArray = class (TThoriumType)
  public
    constructor Create(const AThorium: TThorium; const AValueType: TThoriumType);
  private
    FValueType: TThoriumType;
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    property ValueType: TThoriumType read FValueType;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType = nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
    function HasIndexedAccess: Boolean; override;
    function NeedsClear: Boolean; override;
  end;

  { TThoriumTypeDynamicArray }

  TThoriumTypeDynamicArray = class (TThoriumTypeArray)
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType = nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;

    function DoGetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue): TThoriumValue; override;
    function DoGetLength(const AValue: TThoriumValue): Integer; override;
    procedure DoSetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue; const NewValue: TThoriumValue); override;
  end;

  { TThoriumTypeStaticArray }

  TThoriumTypeStaticArray = class (TThoriumTypeArray)
  public
    constructor Create(const AThorium: TThorium; const AValueType: TThoriumType;
       Count: Integer);
  private
    FCount: Integer;
  public
    property Count: Integer read FCount;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType = nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;

    function DoGetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue): TThoriumValue; override;
    function DoGetLength(const AValue: TThoriumValue): Integer; override;
    procedure DoSetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue; const NewValue: TThoriumValue); override;
  end;

    (* A set of values processable by Thorium representing the complete register
     set of a Thorium virtual machine. *)
  TThoriumRegisters = array [0..THORIUM_REGISTER_COUNT-1] of TThoriumValue;

{%ENDREGION}

(*
   Region: Identifier qualification and relocation
                                                                              *)
{%REGION 'Identifier qualification and relocation' /fold}
  (* An array of TThoriumHostObjectType mainly used to notify the compiler about
     (possibly relocation needing) uses of an external type. *)
  TThoriumHostObjectTypeArray = array of TThoriumHostObjectType;

  (* An array of TThoriumLibraryProperty mainly used to notify the compiler
     about (possibly relocation needing) uses of a library property. *)
  TThoriumLibraryPropertyArray = array of TThoriumLibraryProperty;

  TThoriumValueState = (vsDynamic, vsAccessable, vsStatic);
  // vsDynamic = Result of a calculating operation
  // vsAccessable = Read from register or stack, just a "clone", not a copy
  // vsStatic = Constant value which may be used for CTE (Compile time evaluation)

  (* A record containing information about a fully qualified identifier, which
     means that all brackets, dots and square brackets of the identifier
     expression are parsed. It contains also information about used host types
     and library properties and how to read or write the value of the
     identifier. *)
  TThoriumQualifiedIdentifier = record
    FullStr: String;
    Kind: TThoriumQualifiedIdentifierKind;
    State: TThoriumValueState;
    FinalType: TThoriumType;
    Value: TThoriumValue;
    Writable: Boolean;

    GetJumpMarks: TThoriumIntArray;
    GetCode: TThoriumOperationArray;
    SetJumpMarks: TThoriumIntArray;
    SetCode: TThoriumOperationArray;
    UsedExtendedTypes: array of TThoriumHostObjectType;
    UsedLibraryProps: array of TThoriumLibraryProperty;
  end;
  PThoriumQualifiedIdentifier = ^TThoriumQualifiedIdentifier;

  (* This record contains information about a relocation. *)
  TThoriumRelocation = record
    ByteOffset: Cardinal;
    ObjectIndex: Cardinal;
  end;

  (* A pointer to a record containing information about a relocation. *)
  PThoriumRelocation = ^TThoriumRelocation;

  TThoriumQualifiedIdentifierList = specialize TFPGList<PThoriumQualifiedIdentifier>;
{%ENDREGION}

(*
   Region: Stack and identifier table entries
                                                                              *)
{%REGION 'Stack and identifier table entries' /fold}

  (* Defines which data a stack entry contains. *)
  TThoriumStackEntryType = (stValue, stStackFrame, stVarargs, stNull);

  TThoriumStackFrame = record
    PreviousStackFrame: Integer;
    ReturnAddress: TThoriumInstructionAddress;
    ReturnModule: TThoriumRuntimeModule;
    ParameterCount: Cardinal;
    HasReturnValue: Boolean;
    RegisterDumpRange: TThoriumRegisterID;
    RegisterDumpData: Pointer;
    DropResult: Boolean;
  end;

  TThoriumVarargsBuffer = record
    Data: Pointer;
    DataOrigin: Pointer;
    Buffer: Pointer;
    BufferOrigin: Pointer;
    ToFree: PThoriumValue;
    ToFreeOrigin: PThoriumValue;
  end;

  (* This record represents one entry on the Thorium stack. It may either
     contain a value, a stack frame or a set of varargs for a function call. *)
  TThoriumStackEntry = record
  case _Type: TThoriumStackEntryType of
    stValue:
    (
      Value: TThoriumValue
    );
    stStackFrame:
    (
      StackFrame: TThoriumStackFrame;
    );
    stVarargs:
    (
      VarargsBuffer: TThoriumVarargsBuffer;
    );
    stNull:
    (

    );
  end;

  (* Pointer to an entry on the Thorium stack. *)
  PThoriumStackEntry = ^TThoriumStackEntry;

  (* An entry in an identifier table used by the Thorium compiler which defines
     of which kind an identifier is and how it might be found. *)
  TThoriumTableEntry = record
    Name: PString;
    _Type: TThoriumTableEntryType;
    Offset: Integer; // This is the register of a register variable and the index of a library constant.
    TypeSpec: TThoriumType;
    Writable: Boolean;
    ValidValue: Boolean;
    Value: TThoriumValue;
    Ptr: Pointer;
  end;

  PThoriumTableEntry = ^TThoriumTableEntry;

  TThoriumTableEntryResult = record
    Entry: TThoriumTableEntry;
    SourceModule: TThoriumModule;
    SourceLibrary: TThoriumLibrary;
  end;
  PThoriumTableEntryResult = ^TThoriumTableEntryResult;
  TThoriumTableEntryResultList = specialize TFPGList<PThoriumTableEntryResult>;
  TThoriumTableEntryResults = array of TThoriumTableEntryResult;

{%ENDREGION}

(*
   Region: Thorium functions and variables
   Declarations: TThoriumParameters, TThoriumPublicValue, TThoriumFunction,
                 TThoriumFunctionCallbackCapsule, TThoriumVariable
   Description: The classes in this region are used by the compiler and to
                publish Thorium script functions and variables to the host
                environment.
                                                                              *)
{%REGION 'Thorium functions and variables' /fold}

  { TThoriumParameters }

  (* This class manages a set of Thorium types used as parameter or return type
     specification. *)
  TThoriumParameters = class (TObject)
    constructor Create;
    destructor Destroy; override;
  private
    FList: TThoriumTypes;

    function GetCount: Integer;
    function GetTypeSpec(Index: Integer): TThoriumType;
  protected
    procedure Assign(ASource: TThoriumParameters);
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Remove(AType: TThoriumType);
  public
    property Count: Integer read GetCount;
    property TypeSpec[Index: Integer]: TThoriumType read GetTypeSpec; default;
  public
    procedure Add(AType: TThoriumType);
    function Duplicate: TThoriumParameters;
    procedure GetParameterSpec(const Index: Integer; out ParamSpec: TThoriumType); deprecated;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  { TThoriumPublicValue }

  (* A base class for any symbol published by a module. *)
  TThoriumPublicValue = class (TObject)
    constructor Create(AModule: TThoriumModule; AName: String); virtual;
  private
    FModule: TThoriumModule;
    FName: String;
  public
    property Module: TThoriumModule read FModule;
    property Name: String read FName;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
  end;

  { TThoriumFunction }

  (* This class represents a function published by a module and is also used as
     a temporary object by the compiler to store information about the current
     function. *)
  TThoriumFunction = class (TThoriumPublicValue)
    constructor Create(AModule: TThoriumModule; AName: String); override;
    destructor Destroy; override;
  private
    FEntryPoint: Integer;
    FEventCapsules: TFPHashList;
    FNestingLevel: Integer;
    FPrototype: TThoriumTypeFunction;
    FPrototyped: Boolean;
    FPrototypedCalls: TThoriumJumpList;
    FVisibilityLevel: TThoriumVisibilityLevel;
  public
    property EntryPoint: Integer read FEntryPoint;
    property NestingLevel: Integer read FNestingLevel;
    property Prototype: TThoriumTypeFunction read FPrototype;
    property Prototyped: Boolean read FPrototyped;
    property VisibilityLevel: TThoriumVisibilityLevel read FVisibilityLevel;

    function Duplicate: TThoriumFunction;
    (*function AsEvent(AParameters: array of TThoriumHostType;
      ReturnType: TThoriumHostType): TThoriumFunctionCallbackCapsule; overload;
    function AsEvent(AParameters: array of TThoriumHostType;
      ReturnType: TThoriumHostType;
      ExtParameters: array of TThoriumHostObjectType;
      ExtReturnType: TThoriumHostObjectType = nil): TThoriumFunctionCallbackCapsule; overload;*)
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;
  TThoriumFunctions = specialize TFPGList<TThoriumFunction>;

  { TThoriumVariable }

  (* A variable published by a module. *)
  TThoriumVariable = class (TThoriumPublicValue)
    constructor Create(AModule: TThoriumModule; AName: String); override;
  private
    FIsStatic: Boolean;
    FStackPosition: Integer;
    FTypeSpec: TThoriumType;
  public
    property IsStatic: Boolean read FIsStatic;
    property StackPosition: Integer read FStackPosition;
    property TypeSpec: TThoriumType read FTypeSpec;

    procedure AssignFromTableEntry(const ATableEntry: TThoriumTableEntry);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;
  TThoriumVariables = specialize TFPGList<TThoriumVariable>;

{%ENDREGION}

(*
   Region: Host functions & methods
   Declarations: TThoriumHostCallableBase, TThoriumHostFunctionBase,
                 TThoriumHostFunctionSimpleMethod,
                 TThoriumHostFunctinNativeCall,
                 TThoriumHostMethodAsFunctionNativeCall, TThoriumHostMethodBase,
                 TThoriumHostMethodSimple, TThoriumHostMethodNativeCall
   Description: These classes are used to publish functions and methods of the
                host enironment to Thorium script. The creation is usually done
                by helper functions of the affected classes, so do not create
                them directly.
                                                                              *)
{%REGION 'Host functions & methods' /fold}

  { TThoriumHostParameters }

  (* This class represents a set of host environment types used as a parameter
     specification for host functions and methods. *)
  TThoriumHostParameters = class (TThoriumHashableObject)
    constructor Create;
    destructor Destroy; override;
  private
    FCapacity: Integer;
    FCount: Integer;
    FParams: PThoriumHostType;
    function GetParameterInfo(Index: Integer): PThoriumHostType;
    function GetParameter(Index: Integer): PTypeInfo;
    function GetStoring(Index: Integer): Boolean;
    procedure SetParameter(Index: Integer; const AValue: PTypeInfo);
    procedure SetStoring(Index: Integer; const AValue: Boolean);
  protected
    procedure Expand;
    procedure SetCapacity(AValue: Integer);
  public
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Parameters[Index: Integer]: PTypeInfo read GetParameter write SetParameter; default;
    property Storing[Index: Integer]: Boolean read GetStoring write SetStoring;
  public
    function Add(const AType: PTypeInfo; const IsStoring: Boolean = False): Integer;
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    function IndexOf(const AType: PTypeInfo; const Nth: Integer): Integer;
    procedure Insert(const AIndex: Integer; const AType: PTypeInfo;
      const IsStoring: Boolean = False);
  end;

  { TThoriumHostCallableBase }

  (* A base class for any callable object published by the host environment. *)
  TThoriumHostCallableBase = class (TThoriumHashableObject)
  public
    constructor Create(ALibrary: TThoriumLibrary); virtual;
    destructor Destroy; override;
  protected
    FLibrary: TThoriumLibrary;
    FName: String;
    FParameters: TThoriumHostParameters;
    FReturnType: TThoriumHostType;
    FPrototype: TThoriumTypeHostFunction;
    function GetPrototype: TThoriumTypeHostFunction;
  protected
    procedure CalcHash; override;
  public
    procedure CreatePrototype; virtual;
    function NeedsNativeData: Boolean; virtual;
  public
    property Parameters: TThoriumHostParameters read FParameters;
    property Prototype: TThoriumTypeHostFunction read GetPrototype;
    property ReturnType: TThoriumHostType read FReturnType write FReturnType;
    property ReturnTypeStoring: Boolean read FReturnType.Storing write FReturnType.Storing;
    property Name: String read FName write FName;
  end;

  TThoriumHostCallableList = specialize TFPGList<TThoriumHostCallableBase>;

  { TThoriumHostFunctionBase }

  (* The base class for a function published by the host environment. *)
  TThoriumHostFunctionBase = class (TThoriumHostCallableBase)
  protected
    procedure CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine = nil); virtual; abstract;
  end;
  TThoriumHostFunctionBaseClass = class of TThoriumHostFunctionBase;

  { TThoriumHostFunctionSimpleMethod }

  (* This host function class is based on a specific callback which will be
     called when the virtual machine calls the function. *)
  TThoriumHostFunctionSimpleMethod = class (TThoriumHostFunctionBase)
    constructor Create(ALibrary: TThoriumLibrary); override;
  private
    FMethod: TThoriumSimpleMethod;
  protected
    procedure CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine = nil); override;
  public
    property Method: TThoriumSimpleMethod read FMethod write FMethod;
  end;

  { TThoriumHostFunctionNativeCall }

  (* This host function class is able to call any function of the host
     environment directly without the need for a wrapper. *)
  TThoriumHostFunctionNativeCall = class (TThoriumHostFunctionBase)
  public
    constructor Create(ALibrary: TThoriumLibrary); override;
    destructor Destroy; override;
  private
    FCallingConvention: TThoriumNativeCallingConvention;
    FCodePointer: Pointer;
    FInstructions: Pointer;
    FVAOffset: Integer;
  protected
    procedure CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine=nil); override;
  public
    procedure CreatePrototype; override;
    function NeedsNativeData: Boolean; override;
    procedure Precompile; virtual;
  public
    property CallingConvention: TThoriumNativeCallingConvention read FCallingConvention write FCallingConvention;
    property CodePointer: Pointer read FCodePointer write FCodePointer;
  end;

  { TThoriumHostMethodAsFunctionNativeCall }

  (* This host function class is able to call any method of the host
     environment directly with a constant first parameter of the type of a
     pointer without the need for a wrapper. *)
  TThoriumHostMethodAsFunctionNativeCall = class (TThoriumHostFunctionNativeCall)
  public
    constructor Create(ALibrary: TThoriumLibrary); override;
  private
    FDataPointer: Pointer;
  protected
    procedure CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine=nil); override;
  public
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    procedure Precompile; override;
  end;

  { TThoriumHostMethodBase }

  (* The base class for any method published by the host environment. *)
  TThoriumHostMethodBase = class (TThoriumHostCallableBase)
  public
    constructor Create(ALibrary: TThoriumLibrary); override;
  protected
    FHostObjectType: TThoriumHostObjectType;
  protected
    procedure CallFromVirtualMachine(OfObject: TObject; AVirtualMachine: TThoriumVirtualMachine=nil); virtual; abstract;
  end;

  { TThoriumHostMethodSimple }

  (* This host method class is based on a specific callback which will be called
     when the virtual machine calls the method. *)
  TThoriumHostMethodSimple = class (TThoriumHostMethodBase)
  public
    constructor Create(ALibrary: TThoriumLibrary); override;
  private
    FClassMethod: TThoriumClassMethod;
  protected
    procedure CallFromVirtualMachine(OfObject: TObject; AVirtualMachine: TThoriumVirtualMachine=nil); override;
  public
    property ClassMethod: TThoriumClassMethod read FClassMethod write FClassMethod;
  end;

  { TThoriumHostMethodNativeCall }

  (* This host method class is able to call any host environment method without
     the need for a wrapper. *)
  TThoriumHostMethodNativeCall = class (TThoriumHostMethodBase)
  public
    constructor Create(ALibrary: TThoriumLibrary); override;
    destructor Destroy; override;
  private
    FCallingConvention: TThoriumNativeCallingConvention;
    FCodePointer: Pointer;
    FInstructions: Pointer;
    FVAOffset: Integer;
  protected
    procedure CallFromVirtualMachine(OfObject: TObject; AVirtualMachine: TThoriumVirtualMachine=nil); override;
  public
    property CallingConvention: TThoriumNativeCallingConvention read FCallingConvention write FCallingConvention;
    property CodePointer: Pointer read FCodePointer write FCodePointer;
  public
    function NeedsNativeData: Boolean; override;
    procedure Precompile;
  end;

{%ENDREGION}

(*
   Region: Host class types
   Declarations: TThoriumHostObjectType, TThoriumRTTIObjectType
   Description: The classes in this region provide the interface between the
                host classes (usually based on TThoriumPersistent) and Thorium
                script. Creation of this classes is usually done by helper
                functions in TThoriumLibrary, so do not instanciate them
                directly.
                                                                              *)
{%REGION 'Host class types' /fold}

  { TThoriumHostObjectType }

  (* This is the base class for any class-alike type published by the host
     environment. You will need to override the methods to make it represent
     a type you want. *)
  TThoriumHostObjectType = class (TThoriumType)
    constructor Create(ALibrary: TThoriumLibrary); virtual;
    destructor Destroy; override;
  protected
    FLibrary: TThoriumLibrary;
  public
    procedure ApplyStoring(var AValue: Pointer; MayDecreaseReference: Boolean = True); virtual; abstract;
    procedure DisposeValue(var AValue: Pointer); virtual; abstract;
    function DuplicateInstance(const AValue: Pointer): Pointer; virtual; abstract;
    function FindMethod(const AMethodName: String): TThoriumHostMethodBase; virtual;
    function GetFieldID(const FieldIdent: String; out ID: QWord): Boolean; virtual;
    function GetFieldStoring(const AFieldID: QWord): Boolean; virtual; abstract;
    procedure GetFieldType(const AFieldID: QWord; out TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition); virtual; abstract;
    function GetIndexType(const IndexType: TThoriumType; out TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition): Boolean; virtual;
    function GetNewInstance: Pointer; virtual; abstract;
    function GetStaticFieldID(const FieldIdent: String; out ID: QWord): Boolean; virtual;
    function GetStaticFieldStoring(const AFieldID: QWord): Boolean; virtual; abstract;
    procedure GetStaticFieldType(const AFieldID: QWord; out TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition); virtual; abstract;
  end;
  TThoriumHostObjectTypeClass = class of TThoriumHostObjectType;

  { TThoriumRTTIObjectType }

  (* This is a derivate of TThoriumHostObjectType which uses the RTTI
     information of a class implementing IThoriumPersistent and some callbacks
     to publish a class to Thorium. Some optimizations have been made to
     reduce the overhead of RTTI as much as possible. *)
  TThoriumRTTIObjectType = class (TThoriumHostObjectType)
    constructor Create(ALibrary: TThoriumLibrary); override;
    constructor Create(ALibrary: TThoriumLibrary;
      ABaseClass: TThoriumPersistentClass; AbstractClass: Boolean = False);
    constructor Create(ALibrary: TThoriumLibrary; ABaseClass: TClass;
      MethodCallback: TThoriumRTTIMethodsCallback;
      StaticMethodCallback: TThoriumRTTIStaticMethodsCallback;
      AbstractClass: Boolean = False);
    destructor Destroy; override;
  private
    FBaseClass: TClass;
    FCanUsePersistent: Boolean;
    FPropCount: SmallInt;
    FPropList: PPropList;
    FStaticMethods: array of TThoriumHostFunctionBase;
    FMethods: array of TThoriumHostMethodBase;
    FStoringProperties: TStringList;
  protected
    procedure CalcHash; override;
  public
    procedure ApplyStoring(var AValue: Pointer; MayDecreaseReference: Boolean=
       True); override;
    procedure DisposeValue(var AValue: Pointer); override;
    function DuplicateInstance(const AValue: Pointer): Pointer; override;
    function FindMethod(const AMethodName: String): TThoriumHostMethodBase;
       override;
    function GetFieldID(const FieldIdent: String; out ID: QWord): Boolean;
       override;
    function GetFieldStoring(const AFieldID: QWord): Boolean; override;
    procedure GetFieldType(const AFieldID: QWord; out TypeSpec: TThoriumType;
       out Access: TThoriumAccessDefinition); override;
    function GetNewInstance: Pointer; override;
    function GetPropertyStoring(const PropInfo: PPropInfo): Boolean;
    function GetPropertyStoring(const PropertyName: String): Boolean;
    function GetStaticFieldID(const FieldIdent: String; out ID: QWord
       ): Boolean; override;
    procedure GetStaticFieldType(const AFieldID: QWord; out
       TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition); override;
    procedure DoEnableHostControl(const AValue: TThoriumValue); override;
    function DoEvaluate(const AValue: TThoriumValue): Boolean; override;
    function DoGetField(const AValue: TThoriumValue; const AFieldID: QWord
       ): TThoriumValue; override;
    procedure DoSetField(const AValue: TThoriumValue;
              const AFieldID: QWord; const NewValue: TThoriumValue); override;
    procedure SetPropertyStoring(const PropInfo: PPropInfo; const Storing: Boolean);
    procedure SetPropertyStoring(const PropertyName: String; const Storing: Boolean);
  public
    property BaseClass: TClass read FBaseClass;

    function NewNativeCallMethod(const AName: String;
      const ACodePointer: Pointer;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo = nil;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostMethodNativeCall;
    function NewNativeCallStaticMethod(const AName: String;
      const ACodePointer: Pointer; const ADataPointer: Pointer;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo = nil;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostMethodAsFunctionNativeCall;
    function NewNativeCallStaticFunction(const AName: String;
      const ACodePointer: Pointer;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo = nil;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostFunctionNativeCall;
  end;

  { TThoriumHostRecordType }

  generic TThoriumHostRecordType<RecordType> = class (TThoriumHostObjectType)
  public
    constructor Create(ALibrary: TThoriumLibrary); override;
    constructor Create(ALibrary: TThoriumLibrary;
      AFields: array of TThoriumHostRecordField);
    destructor Destroy; override;
  private
    FFields: TThoriumHostRecordFields;
  private
    function IndexOfFieldDefinition(const AFieldName: String): Integer;
  protected
    procedure CalcHash; override;
  public
    function CanAssignTo(var Assignment: TThoriumAssignmentDescription;
       const AnotherType: TThoriumType=nil): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil): Boolean; override;
    procedure DisposeValue(var AValue: Pointer); override;
    function DuplicateInstance(const AValue: Pointer): Pointer; override;
    function GetFieldID(const FieldIdent: String; out ID: QWord): Boolean;
       override;
    function GetFieldStoring(const AFieldID: QWord): Boolean; override;
    procedure GetFieldType(const AFieldID: QWord; out
      TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
      override;
    function GetNewInstance: Pointer; override;
    function OpGetField(const AInstance: Pointer; const AFieldID: QWord
       ): TThoriumValue; override;
    procedure OpSetField(const AInstance: Pointer; const AFieldID: QWord;
       const NewValue: TThoriumValue); override;
  public
  end;
  TThoriumHostRecordClass = class of TThoriumHostRecordType;

{%ENDREGION}

(*
   Region: Host libraries
   Declarations: TThoriumLibraryConstant, TThoriumLibraryProperty,
                 TThoriumLibraryPropertyDirect,
                 TThoriumLibraryPropertyDirectSetCallback,
                 TThoriumLibraryPropertyCallback, TThoriumLibrary
   Description: The TThoriumLibrary class group is used to publish a group of
                symbols (e.g. functions, classes and variables) to Thorium
                script. For an example see the customlib example.
                                                                              *)
{%REGION 'Host libraries' /fold}

  (* A constant exported by a library. *)
  TThoriumLibraryConstant = class (TObject)
  private
    FName: String;
    FValue: TThoriumValue;
  public
    property Name: String read FName;
    property Value: TThoriumValue read FValue;
  end;

  TThoriumLibraryConstantList = specialize TFPGList<TThoriumLibraryConstant>;

  { TThoriumLibraryProperty }

  (* An abstract property exportet by a library. *)
  TThoriumLibraryProperty = class (TThoriumHashableObject)
  public
    constructor Create; virtual;
  private
    FName: String;
  public
    procedure GetValue(var AThoriumValue: TThoriumValue); virtual; abstract;
    function GetStatic: Boolean; virtual; abstract;
    function GetType: TThoriumType; virtual; abstract;
    procedure SetValue(const AThoriumValue: TThoriumValue); virtual; abstract;
  public
    property Name: String read FName;
  end;
  TThoriumLibraryPropertyClass = class of TThoriumLibraryProperty;
  TThoriumLibraryPropertyList = specialize TFPGList<TThoriumLibraryProperty>;

  { TThoriumLibraryPropertyDirect }

  (* A property whose value is saved directly in this object exported by a
     library. *)
  TThoriumLibraryPropertyDirect = class (TThoriumLibraryProperty)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FStatic: Boolean;
    FTypeSpec: TThoriumType;
    FValue: TThoriumValue;
  protected
    procedure CalcHash; override;
  public
    procedure GetValue(var AThoriumValue: TThoriumValue); override;
    function GetStatic: Boolean; override;
    function GetType: TThoriumType; override;
    procedure SetValue(const AThoriumValue: TThoriumValue); override;
  public
    // Direct access for the host
    property Value: TThoriumValue read FValue write FValue;
  end;

  { TThoriumLibraryPropertyDirectSetCallback }

  (* A property whose value is saved directly in this object exported by a
     library. When the value is changed, a callback will be called to allow
     a detailed control about the value. *)
  TThoriumLibraryPropertyDirectSetCallback = class (TThoriumLibraryPropertyDirect)
  public
    constructor Create; override;
  private
    FOnPropertySet: TThoriumOnPropertySetCallback;
  public
    procedure SetValue(const AThoriumValue: TThoriumValue); override;
  public
    property OnPropertySet: TThoriumOnPropertySetCallback read FOnPropertySet write FOnPropertySet;
  end;

  { TThoriumLibraryPropertyCallback }

  (* A virtual property which only works with callbacks. *)
  TThoriumLibraryPropertyCallback = class (TThoriumLibraryProperty)
  public
    constructor Create; override;
  private
    FOnPropertyGet: TThoriumOnPropertyGet;
    FOnPropertySet: TThoriumOnPropertySet;
    FStatic: Boolean;
    FTypeSpec: TThoriumType;
  protected
    procedure CalcHash; override;
  public
    procedure GetValue(var AThoriumValue: TThoriumValue); override;
    function GetStatic: Boolean; override;
    function GetType: TThoriumType; override;
    procedure SetValue(const AThoriumValue: TThoriumValue); override;
  end;

  TThoriumTypesMap = specialize TFPGMap<PTypeInfo, TThoriumType>;

  TThoriumLibraryList = specialize TFPGList<TThoriumLibrary>;

  { TThoriumLibrary }

  TThoriumLibrary = class (TThoriumHashableObject)
  public
    constructor Create(AThorium: TThorium);
    destructor Destroy; override;
  private
    FConstants: TThoriumLibraryConstantList;
    FName: String;
    FHostCallables: TThoriumHostCallableList;
    FProperties: TThoriumLibraryPropertyList;
    FRequiredLibraries: TThoriumLibraryList;
    FRTTITypes: TThoriumTypes;
    FThorium: TThorium;
    FTypes: TThoriumTypes;
    FTypeMap: TThoriumTypesMap;
  private
    procedure Clear;
  protected
    procedure AddDependency(const ALibName: String);
    procedure AddDependency(const ALib: TThoriumLibrary); virtual;
    procedure CreatePrototypes;
    procedure InitializeLibrary; virtual;
    procedure RaiseRTTIRequired(const AStr: String);
    procedure SetupPropertyDirect(const AInstance: TThoriumLibraryPropertyDirect;
      const AName: String;
      const ATypeSpec: TThoriumType; const AStatic: Boolean;
      const AInitialValue: PThoriumValue = nil);
  protected // Helper functions
    procedure CheckExisting(const AName: String; const AType: TThoriumType);

    function RegisterConstant(const AName: String;
      const AValue: TThoriumValue): PThoriumValue;

    function RegisterConstant(const AName: String;
      const AValue; const AType: PTypeInfo): PThoriumValue;

    function RegisterConstant(const AName: String;
      const AValue: TThoriumInteger): PThoriumValue;

    function RegisterConstant(const AName: String;
      const AValue: TThoriumFloat): PThoriumValue;

    function RegisterConstant(const AName: String;
      const AValue: TThoriumString): PThoriumValue;

    function RegisterFinishedObjectType(const AName: String;
      const AInstance: TThoriumHostObjectType;
      const ATypeInfo: PTypeInfo): TThoriumHostObjectType;

    function RegisterNativeCallFunction(const AName: String;
      const ACodePointer: Pointer; const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo;
      const ACallingConvention: TThoriumNativeCallingConvention): TThoriumHostFunctionNativeCall;

    function RegisterNativeCallMethodAsFunction(const AName: String;
      const ACodePointer: Pointer; const ADataPointer: Pointer;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo;
      const ACallingConvention: TThoriumNativeCallingConvention): TThoriumHostMethodAsFunctionNativeCall;

    function RegisterPropertyCallback(const AName: String;
      const ATypeSpec: TThoriumType; AStatic: Boolean;
      const AGetCallback: TThoriumOnPropertyGet;
      const ASetCallback: TThoriumOnPropertySet): TThoriumLibraryPropertyCallback;

    function RegisterPropertyCustom(const AName: String;
      const AInstance: TThoriumLibraryProperty): TThoriumLibraryProperty;

    function RegisterPropertyDirect(const AName: String;
      const ATypeSpec: TThoriumType;
      const AStatic: Boolean; const AInitialValue: PThoriumValue = nil): TThoriumLibraryPropertyDirect;

    function RegisterPropertyDirectCallback(const AName: String;
      const ATypeSpec: TThoriumType; const AStatic: Boolean;
      const ACallback: TThoriumOnPropertySetCallback;
      const AInitialValue: PThoriumValue = nil): TThoriumLibraryPropertyDirectSetCallback;

    function RegisterRTTIType(const AClass: TThoriumPersistentClass;
      AbstractClass: Boolean = False): TThoriumRTTIObjectType;

    function RegisterRTTIType(const AClass: TClass;
      AMethodsCallback: TThoriumRTTIMethodsCallback;
      AStaticMethodsCallback: TThoriumRTTIStaticMethodsCallback;
      AbstractClass: Boolean = False): TThoriumRTTIObjectType;

    function RegisterSimpleMethod(const AName: String;
      const AFunction: TThoriumSimpleMethod;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo): TThoriumHostFunctionSimpleMethod;

    function RegisterType(const AName: String; const AInstance: TThoriumType;
      const ATypes: array of PTypeInfo): TThoriumType;
  public
    function DeepFindType(const AName: String): TThoriumType;
    function DeepFindTypeForHostType(const AType: PTypeInfo): TThoriumType;
    procedure DeepLookupIdentifier(const AName: String; var Entries: TThoriumTableEntryResults);
    function FindType(const AName: String): TThoriumType;
    function FindTypeForHostType(const AType: PTypeInfo): TThoriumType;
    class function GetName: String; virtual; abstract;
    procedure LookupIdentifier(const AName: String; var Entries: TThoriumTableEntryResults);
  end;

  TThoriumLibraryClass = class of TThoriumLibrary;

{%ENDREGION}

(*
   Region: Compiler utilities
   Declarations: TThoriumIdentifierTable, TThoriumScanner, TThoriumInstructions
   Description: These classes are used by the compiler to translate the Thorium
                script source to the virtual machine instructions which are then
                executed by the Thorium virtual machine.
                                                                              *)
{%REGION 'Compiler utilities' /fold}

  { TThoriumIdentifierTable }

  (* This table contains information about declared identifiers and is used
     internally by the compiler. *)
  TThoriumIdentifierTable = class (TObject)
    constructor Create;
    destructor Destroy; override;
  private
    FCapacity, FCount: Integer;
    FIdentifiers: PThoriumTableEntry;

    procedure Expand;
    procedure ForceCapacity(NewCapacity: Integer);
    function NewEntry: PThoriumTableEntry;
    procedure SetCapacity(NewCapacity: Integer);
  protected
    procedure FreeEntry(var AEntry: TThoriumTableEntry);
  public
    property Count: Integer read FCount;
    function AddFunctionIdentifier(Name: String; Func: TThoriumFunction): PThoriumTableEntry;
    function AddParameterIdentifier(Name: String; Offset: Integer; TypeSpec: TThoriumType): PThoriumTableEntry;
    function AddRegisterVariableIdentifier(Name: String; RegisterID: TThoriumRegisterID; TypeSpec: TThoriumType): PThoriumTableEntry;
    function AddVariableIdentifier(const AName: String; Offset: Integer; TypeSpec: TThoriumType; Global, Writable: Boolean; AValue: PThoriumValue = nil): PThoriumTableEntry;
    procedure ClearTable;
    function ClearTableTo(NewCount: Integer): Integer;
    function FindIdentifier(Name: String; out Ident: TThoriumTableEntry): Boolean;
    procedure ReadIdentifier(Index: Integer; out Ident: TThoriumTableEntry);
  end;

  { TThoriumInstructions }

  (* This class holds a list of instructions which may be executed by the
     virtual machine. *)
  TThoriumInstructions = class (TObject)
    constructor Create;
    destructor Destroy; override;
  private
    FInstructions: PThoriumInstruction;
    FCapacity: LongInt;
    FCount: LongInt;
    FInserted: LongInt;
    FSetPosition: TThoriumInstructionAddress;
    FPosition: TThoriumInstructionAddress;
    FAddressLists: TFPList;
    FAddressPointers: TFPList;

    procedure Expand;
    function GetInstruction(Index: Integer): PThoriumInstruction;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetPosition(NewPosition: TThoriumInstructionAddress);
  protected
    procedure FixupAdresses(const MinTarget: TThoriumInstructionAddress; Offset: LongInt);
  public
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Instruction[Index: Integer]: PThoriumInstruction read GetInstruction; default;
    property Position: TThoriumInstructionAddress read FPosition write SetPosition;

    function AppendCode(AInstruction: TThoriumInstruction): Integer;
    function AppendCode(Code: TThoriumInstructionArray): Integer;
    procedure DeleteInstructions(AIndex, ACount: Integer);
    procedure Finish;
    procedure ClearCode;
    
    procedure RegisterAddressList(AList: TThoriumIntList);
    procedure UnRegisterAddressList(AList: TThoriumIntList);
    
    procedure AddInstructionPointer(APointer: PThoriumInstructionAddress);
    procedure RemoveInstructionPointer(APointer: PThoriumInstructionAddress);
    
    procedure DumpCodeBin(DestStream: TStream);
    function DumpCodeStr(ColorfulOutput: Boolean = False): String;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Assign(ASource: TThoriumInstructions);
  end;

{%ENDREGION}

(*
   Region: Optimizer
                                                                              *)
{%REGION 'Optimizer' /fold}
  { TThoriumCustomOptimizerPattern }

  TThoriumCustomOptimizerPattern = class (TObject)
  public
    constructor Create; virtual;
  public
    function Handle(const CurrInstruction: PThoriumInstruction;
      const Remaining: Integer; var Offset: Integer): Boolean; virtual; abstract;
  end;
  TThoriumOptimizerPatternClass = class of TThoriumCustomOptimizerPattern;
  TThoriumOptimizerPatterns = specialize TFPGList<TThoriumCustomOptimizerPattern>;

  { TThoriumOptimizer }

  TThoriumOptimizer = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FInstructions: TThoriumInstructions;
    FJumpList: TThoriumJumpList;
    FOffsetCallback: TThoriumHandleOffset;
    FPatterns: TThoriumOptimizerPatterns;
  protected
    function DoHandleOffset(const Origin, Count: Integer): Boolean;
  public
    function AddPattern(const AClass: TThoriumOptimizerPatternClass): TThoriumCustomOptimizerPattern;
    function Optimize(AInstructions: TThoriumInstructions; OffsetCallback: TThoriumHandleOffset): Integer;
  end;
{%ENDREGION}

(*
   Region: Compiler
   Declarations: TThoriumCustomCompiler, TThoriumDefaultCompiler
   Description: These classes are responsible to compile the script source code
                into thorium bytecode.
                                                                              *)
{%REGION 'Compiler' /fold}

  TThoriumModules = specialize TFPGList<TThoriumModule>;
  TThoriumLibraries = specialize TFPGList<TThoriumLibrary>;

  TThoriumCompilerBreakContext = class (TObject)
  private
    FJumpList: TThoriumIntList;
    FTableTarget: Integer;
  public
    property JumpList: TThoriumIntList read FJumpList write FJumpList;
    property TableTarget: Integer read FTableTarget write FTableTarget;
  end;
  TThoriumCompilerBreakContextList = specialize TFPGList<TThoriumCompilerBreakContext>;
  TThoriumIntListList = specialize TFPGList<TThoriumIntList>;

  { TThoriumCustomCompiler }

  TThoriumCustomCompiler = class (TObject)
  public
    constructor Create(ATarget: TThoriumModule); virtual;
    destructor Destroy; override;
  private
    FStoredTypes: TInterfaceList;
  private
    function GetTypeFloat: TThoriumTypeFloat;
    function GetTypeInteger: TThoriumTypeInteger;
    function GetTypeString: TThoriumTypeString;
    function IsRegisterInUse(AID: TThoriumRegisterID): Boolean;
    procedure SetRegisterInUse(AID: TThoriumRegisterID; AInUse: Boolean);
  protected
    FBreakContexts: TThoriumCompilerBreakContextList;
    FCodeHook: Boolean;
    FCodeHook1: PThoriumInstructionArray;
    FCodeHook2: PThoriumInstructionArray;
    FError: Boolean;
    FGlobalValueCount: PInteger;
    FHostFuncUsage: TFPList;
    FHostFuncRelocations: TFPList;
    FHostTypeUsage: TFPList;
    FHostTypeRelocations: TFPList;
    FInstructions: TThoriumInstructions;
    FLastError: String;
    FLibPropUsage: TFPList;
    FLibPropRelocations: TFPList;
    FJumps: TThoriumIntList;
    FModule: TThoriumModule;
    FOptimizedInstructions: LongInt;
    FPrivateFunctions: TThoriumFunctions;
    FPublicFunctions: TThoriumFunctions;
    FPublicVariables: TThoriumVariables;
    FRegisterUsage: TThoriumRegisterMask;
    FRequiredModules: TThoriumModules;
    FRequiredLibraries: TThoriumLibraries;
    FSourceHash: TThoriumHash;
    FSourceLength: Cardinal;
    FStringLibrary: TStringList;
    FTable: TThoriumIdentifierTable;
    FTableSizes: TThoriumIntStack;
    FThorium: TThorium;
    FTypeTable: TFPObjectHashTable;
  protected
    property TypeInteger: TThoriumTypeInteger read GetTypeInteger;
    property TypeFloat: TThoriumTypeFloat read GetTypeFloat;
    property TypeString: TThoriumTypeString read GetTypeString;
  protected
    function AddLibraryPropertyUsage(const AProp: TThoriumLibraryProperty): Integer;
    function AddLibraryPropertyUsageEx(var TargetArray: TThoriumLibraryPropertyArray;
        AItem: TThoriumLibraryProperty): Integer;
    procedure AddLibraryPropertyUsages(const AProp: TThoriumLibraryPropertyArray);
    function AddLibraryPropertyUsageToRelocate(const AProp: TThoriumLibraryProperty; const AOffset: ptruint): Integer;
    function AddLibraryString(const AStr: String): Integer;
    function AddHostTypeUsage(const AType: TThoriumHostObjectType): Integer;
    function AddHostTypeUsageEx(var TargetArray: TThoriumHostObjectTypeArray;
        AItem: TThoriumHostObjectType): Integer;
    procedure AddHostTypeUsages(const AUsageArray: TThoriumHostObjectTypeArray);
    function AddHostTypeUsageToRelocate(const AType: TThoriumHostObjectType; const AOffset: ptruint): Integer;
    function AddHostFunctionUsage(const AFunc: TThoriumHostCallableBase): Integer;
    function AddHostFunctionUsageToRelocate(const AFunc: TThoriumHostCallableBase; const AOffset: ptruint): Integer;
    function AddPublicVariable(AName: String): TThoriumVariable;
    function AppendCode(ACodeArray: TThoriumInstructionArray): Integer;
    function AppendCodeEx(const ASource: TThoriumInstructionArray; var ADest: TThoriumInstructionArray): Integer;
    function AppendCodeToOperation(const ASource: TThoriumInstructionArray; var AOperations: TThoriumOperationArray): Integer;
    function AppendOperations(AOperations: TThoriumOperationArray): Integer;
    procedure AppendOperation(var AOperations: TThoriumOperationArray; AOperation: TThoriumGenericOperation);
    procedure CompilerError(const Msg: String); virtual;
    procedure CompilerError(const Msg: String; X, Y: Integer); virtual;
    procedure ClaimRegister(const ARegID: TThoriumRegisterID);
    procedure PopAndClearByTable(const TargetTableSize: Integer);
    procedure DumpState; virtual;
    procedure EmbedHint(const S: String);
    procedure EmbedMetadata(const S: String);
(*    function FindTableEntry(const Ident: String; out Entry: TThoriumTableEntry;
      out Module: TThoriumModule; RaiseError: Boolean = True; AllowFar: Boolean = True): Boolean; inline;*)
    procedure FindTableEntries(const Ident: String;
      var Entries: TThoriumTableEntryResults);
    procedure ForceNewCustomOperation(var OperationArray: TThoriumOperationArray);
    function GenBreak: Integer;
    function GenCode(AInstruction: TThoriumInstruction): Integer; virtual; abstract;
    function GenCode(AInstruction: TThoriumInstruction; ACodeLine: Cardinal): Integer;
    function GenCodeEx(var TargetArray: TThoriumInstructionArray;
        AInstruction: TThoriumInstruction): Integer; virtual; abstract;
    function GenCodeEx(var TargetArray: TThoriumInstructionArray;
        AInstruction: TThoriumInstruction; CodeLine: Integer): Integer;
    function GenCodeToOperation(var OperationArray: TThoriumOperationArray;
        AInstruction: TThoriumInstruction): Integer;
    function GenCreation(AOperation: TThoriumCreateInstructionDescription;
      const ATargetRI: Word = THORIUM_REGISTER_INVALID): Integer;
    function GenOperation(AOperation: TThoriumOperationDescription;
      const ATargetRI: Word = THORIUM_REGISTER_INVALID;
      const AValue1RI: Word = THORIUM_REGISTER_INVALID;
      const AValue2RI: Word = THORIUM_REGISTER_INVALID): Integer;
    function GetBreakContext: TThoriumCompilerBreakContext;
    function GetCurrentTableStackPos: Integer;
    function GetFreeRegister(Kind: TThoriumRegisterKind; out RegisterID: TThoriumRegisterID; ThrowError: Boolean = True): Boolean;
    function GetHighestRegisterInUse: TThoriumRegisterID;
    function GetHookedInstructionPointerA(AIndex: Integer): PThoriumInstruction;
    function GetHookedInstructionPointerB(AIndex: Integer): PThoriumInstruction;
    function GetInstruction(Address: TThoriumInstructionAddress): PThoriumInstruction;
    function GetNextInstructionAddress: TThoriumInstructionAddress;
    function GetTableEntriesTo(StackPos: Integer): Integer;
    procedure FindRelocationTargets;
    function HasError: Boolean;
    procedure LoadLibrary(const LibName: String);
    procedure LoadModule(const ModName: String);
    procedure OptimizeCode;
    function PopBreakContext: TThoriumIntList;
    procedure PushBreakContext;
    procedure ReleaseRegister(ID: TThoriumRegisterID);
    procedure ResetState;
    procedure RestoreTable(var Offset: Integer; GenerateCode: Boolean = True);
    procedure SaveTable;
    procedure SetupFunction(AFunction: TThoriumFunction;
      const AEntryPoint: Integer; const AName: String);
  public
    function CompileFromStream(SourceStream: TStream; Flags: TThoriumCompilerFlags = [cfOptimize]): Boolean; virtual; abstract;
  end;
  TThoriumCompilerClass = class of TThoriumCustomCompiler;

{%ENDREGION}

(*
   Region: Module
   Declarations: TThoriumModule
   Description: This class represents one Thorium module. It is capable of
                compiling a module from source, loading it from or saving it
                to a binary.
                                                                              *)
{%REGION 'Module' /fold}

  { TThoriumModule }

  (* This class represents one Thorium module. It is capable of compiling a
     module from source, loading it from or saving it to a binary. *)
  TThoriumModule = class (TThoriumHashableObject)
    constructor Create(AThorium: TThorium); virtual;
    constructor Create(AThorium: TThorium; AName: String);
    destructor Destroy; override;
  private
    FCompiled: Boolean;
    FCompress: Boolean;
    FGlobalValueCount: Integer;
    FHostFuncUsage: TFPList;
    FHostFuncRelocations: TFPList;
    FHostTypeUsage: TFPList;
    FHostTypeRelocations: TFPList;
    FInstructions: TThoriumInstructions;
    FLastCompilerError: String;
    FLibPropUsage: TFPList;
    FLibPropRelocations: TFPList;
    FName: String;
    FOptimizedInstructions: LongInt;
    FPrivateFunctions: TThoriumFunctions;
    FPublicFunctions: TThoriumFunctions;
    FPublicVariables: TThoriumVariables;
    FRequiredModules: TThoriumModules;
    FRequiredLibraries: TThoriumLibraries;
    FSourceFile: String;
    FSourceHash: TThoriumHash;
    FSourceLength: Cardinal;
    FStringLibrary: TStringList;
    FThorium: TThorium;
  private
    function AddLibraryPropertyUsage(const AProp: TThoriumLibraryProperty): Integer;
    function AddLibraryPropertyUsageToRelocate(const AProp: TThoriumLibraryProperty; const AOffset: ptruint): Integer;
    function AddLibraryString(const AStr: String): Integer;
    function AddHostTypeUsage(const AType: TThoriumHostObjectType): Integer;
    function AddHostTypeUsageToRelocate(const AType: TThoriumHostObjectType; const AOffset: ptruint): Integer;
    function AddHostFunctionUsage(const AFunc: TThoriumHostCallableBase): Integer;
    function AddHostFunctionUsageToRelocate(const AFunc: TThoriumHostCallableBase; const AOffset: ptruint): Integer;
    procedure ClearAll;
    function GetLibraryString(Index: Integer): String;
    function GetLibraryStringCount: Integer;
    function GetInstructionCount: Integer;
    function GetPublicFunction(AIndex: Integer): TThoriumFunction;
    function GetPublicFunctionCount: Integer;
    function GetPublicVariable(AIndex: Integer): TThoriumVariable;
    function GetPublicVariableCount: Integer;
  protected
    procedure CalcHash; override;
    procedure FillHeader(out Header: TThoriumModuleHeader); virtual;
    procedure InternalLoadFromStream(Stream: TStream;
      const Header: TThoriumModuleHeader); virtual;
    procedure InternalSaveToStream(Stream: TStream;
      const Header: TThoriumModuleHeader); virtual;
  public
    property Compiled: Boolean read FCompiled;
    property Compress: Boolean read FCompress write FCompress;
    property InstructionCount: Integer read GetInstructionCount;
    property LastCompilerError: String read FLastCompilerError;
    property LibraryString[Index: Integer]: String read GetLibraryString;
    property LibraryStringCount: Integer read GetLibraryStringCount;
    property Name: String read FName;
    property OptimizedInstructions: LongInt read FOptimizedInstructions;
    property PublicFunction[Index: Integer]: TThoriumFunction read GetPublicFunction;
    property PublicFunctionCount: Integer read GetPublicFunctionCount;
    property PublicVariable[Index: Integer]: TThoriumVariable read GetPublicVariable;
    property PublicVariableCount: Integer read GetPublicVariableCount;
    property Thorium: TThorium read FThorium;
  public
    function CompileFromStream(SourceStream: TStream; ACompiler: TThoriumCompilerClass;
      Flags: TThoriumCompilerFlags = [cfOptimize]): Boolean;
    procedure Dump(ColorfulOutput: Boolean = False);
    function DumpCodeStr(ColorfulOutput: Boolean = False): String;
    function DumpLibStr: String;
    procedure ExecuteMain;
    function EncloseHostValue(const AValue: Pointer; const AType: PTypeInfo): TThoriumValue;
    function FindPublicFunction(const AName: String): TThoriumFunction;
    function IndexOfPublicFunction(const AName: String): Integer;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

{%ENDREGION}

(*
   Region: Virtual machine & utilities
   Declarations: TThoriumStack, TThoriumVirtualMachine
   Description: Combined these classes represent the virtual machine which
                executes the compiled Thorium script on demand.
                                                                              *)
{%REGION 'Virtual machine & utilities' /fold}

  { TThoriumStack }

  (* This class represents the internal Thorium stack which is used by the
     virtual machine and by called functions. *)
  TThoriumStack = class (TObject)
    constructor Create;
    destructor Destroy; override;
  private
    FEntries: PThoriumStackEntry;
    FTop: PThoriumStackEntry;
    FCount: Integer;
    FCapacity: Integer;

    procedure Expand;
    function GetStackEntry(ScopeRoot, Index: Integer): PThoriumStackEntry;
    procedure SetCapacity(NewCapacity: Integer);
  public
    property StackEntry[ScopeRoot, Index: Integer]: PThoriumStackEntry read GetStackEntry;
    property EntryCount: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;

    function FastGetStackEntry(ScopeRoot, Index: Integer): PThoriumStackEntry; inline;
    function GetTopStackEntry: PThoriumStackEntry; inline;

    function GetTop(Offset: Integer = 0): PThoriumStackEntry; inline;
    function Prealloc: PThoriumStackEntry; inline;
    function Push: PThoriumStackEntry; inline;
    procedure Push(AEntry: PThoriumStackEntry); inline;
    procedure Pop(Amount: Integer; FreeValues: Boolean = True); inline;
    function PopTop: PThoriumStackEntry; inline;
    procedure ClearStack;
  end;

  { TThoriumRuntimeFunction }

  TThoriumRuntimeFunction = class (TObject)
  public
    constructor Create(const ACompiledFunction: TThoriumFunction);
  private
    FParameterCount: Integer;
    FPrototype: TThoriumTypeFunction;
    FHasReturnValue: Boolean;
    FEntryPoint: Integer;
    FModule: TThoriumRuntimeModule;
  protected
    procedure FillStackFrame(var StackFrame: TThoriumStackFrame); inline;
  end;

  TThoriumRuntimeFunctions = specialize TFPGList<TThoriumRuntimeFunction>;
  // Need to use ptrint instead of TThoriumFunctin as the map uses greater and
  // less operators, which are not supported by classes -.-
  TThoriumRuntimeFunctionMap = specialize TFPGMap<ptrint, TThoriumRuntimeFunction>;

  { TThoriumRuntimeModule }

  TThoriumRuntimeModule = class (TObject)
  public
    constructor Create(const ACompiledModule: TThoriumModule;
      const ResolveModulePtrCallback: TThoriumVMResolveModulePtr;
      const ResolveFunctionPtrCallback: TThoriumVMResolveFunctionPtr);
    destructor Destroy; override;
  private
    FCompiledModule: TThoriumModule;
    FGlobalValues: array of TThoriumValue;
    FInstructions: TThoriumInstructions;
    FName: String;
    FResolveFunctionPtrCallback: TThoriumVMResolveFunctionPtr;
    FResolveModulePtrCallback: TThoriumVMResolveModulePtr;
    FStringLibrary: TStringList;
  protected
    function DoResolveFunctionPtr(const AFunctionRef: Pointer): TThoriumRuntimeFunction;
    function DoResolveModulePtr(const AModuleRef: Pointer): TThoriumRuntimeModule;
    function FilterInstruction(var AInstruction: TThoriumInstruction): Boolean; virtual;
    procedure Init;
  end;

  TThoriumRuntimeModules = specialize TFPGList<TThoriumRuntimeModule>;
  // Need to use ptrint instead of TThoriumModule as the map uses greater and
  // less operators, which are not supported by classes -.-
  TThoriumRuntimeModuleMap = specialize TFPGMap<ptrint, TThoriumRuntimeModule>;

  (* This class executes previously generated Thorium bytecode. *)

  { TThoriumVirtualMachine }

  TThoriumVirtualMachine = class (TObject)
    constructor Create(AThorium: TThorium);
    destructor Destroy; override;
  private
    FThorium: TThorium;
    FTypeString: TThoriumTypeString;
    FTypeInteger: TThoriumTypeInteger;
    FTypeFloat: TThoriumTypeFloat;
    FModules: TThoriumRuntimeModules;
    FModuleMap: TThoriumRuntimeModuleMap;
    FFunctions: TThoriumRuntimeFunctions;
    FFunctionMap: TThoriumRuntimeFunctionMap;

    FRegisters: TThoriumRegisters;
    FStateRegister: Word;

    FCurrentModule: TThoriumRuntimeModule;
    FCurrentInstructionIdx: Integer;
    FCurrentInstruction: PThoriumInstruction;
    FCurrentStackFrame: Integer;

    procedure ExecuteInstruction; inline;
  protected
    FStack: TThoriumStack;
  protected
    function ImportFunction(AFunction: TThoriumFunction): TThoriumRuntimeFunction;
    procedure ImportModules;
    function ImportModule(AModule: TThoriumModule): TThoriumRuntimeModule;
    function ResolveFunctionPtr(const AFunctionRef: Pointer): TThoriumRuntimeFunction;
    function ResolveModulePtr(const AModuleRef: Pointer): TThoriumRuntimeModule;
  public
    procedure DumpStack;
    procedure DumpState;
    function GetRuntimeFunction(const AFunction: TThoriumFunction): TThoriumRuntimeFunction;
    function GetRuntimeModule(const ACompiledModule: TThoriumModule): TThoriumRuntimeModule;
    function GetStack: TThoriumStack;
    procedure Execute(StartModule: TThoriumRuntimeModule; StartInstruction: Integer; CreateDefaultStackFrame: Boolean = True); virtual;
    function CallFunction(AFunction: TThoriumRuntimeFunction; AParameters: array of const): TThoriumValue;
    function QuickCall(AFunction: TThoriumRuntimeFunction; AParameters: array of TThoriumValue): TThoriumValue;
//    function SafeCall(AParameters: array of const): TThoriumValue;
  end;

  { TThoriumDebuggingVirtualMachine }

  TThoriumDebuggingVirtualMachine = class (TThoriumVirtualMachine)
    constructor Create(AThorium: TThorium);
    destructor Destroy; override;
  private
    FBreakpointInstructions: TThoriumIntList;
    FBreakpointLines: TThoriumIntList;
    FOnDebugObject: TThoriumDebugCallbackObject;
    FOnDebugValue: TThoriumDebugCallbackValue;
    FStepMode: TThoriumDebuggerStepMode;
    FStepping: Boolean;
  private
    function GetRegister(ARegID: TThoriumRegisterID): PThoriumValue;
  public
    property BreakpointInstructions: TThoriumIntList read FBreakpointInstructions;
    property BreakpointLines: TThoriumIntList read FBreakpointLines;
    property Registers[ARegID: TThoriumRegisterID]: PThoriumValue read GetRegister;
    property Stack: TThoriumStack read FStack;
    property StepMode: TThoriumDebuggerStepMode read FStepMode write FStepMode;
  public
    procedure Execute(StartModule: TThoriumRuntimeModule; StartInstruction: Integer;
       CreateDefaultStackFrame: Boolean=False); override;
    procedure StepInto;
    procedure StepOver;
  end;

{%ENDREGION}

(*
   Region: Thorium core engine
   Declarations: TThorium
   Description: This class manages all modules, libraries and the virtual
                machine of one Thorium context.
                                                                              *)
{%REGION 'Thorium core engine' /fold}

  { TThoriumLibCore }

  TThoriumLibCore = class (TThoriumLibrary)
  protected
    procedure InitializeLibrary; override;
  public
    class function GetName: String; override;
  end;

  (* This class manages all modules, libraries and the virtual machine thus
     representing one Thorium context. This class is not a singleton. Several
     instances may exist. *)
  TThorium = class (TObject)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FAnonymousID: Cardinal;
    FHostLibraries: TThoriumLibraries;
    FModules: TThoriumModules;
    FOnCompilerOutput: TThoriumOnCompilerOutput;
    FOnRequireModule: TThoriumOnRequireModule;
    FOnOpenModule: TThoriumOnOpenModule;
    FVirtualMachine: TThoriumVirtualMachine;
    FTypeFloat: TThoriumTypeFloat;
    FTypeInteger: TThoriumTypeInteger;
    FTypeString: TThoriumTypeString;
  private
    function GetLibrary(Index: Integer): TThoriumLibrary;
    function GetLibraryCount: Integer;
    function GetLocked: Boolean;
    function GetModule(Index: Integer): TThoriumModule;
    function GetModuleCount: Integer;
  protected
    procedure DoCompilerOutput(const Module: TThoriumModule; const Msg: String); virtual;
    function DoRequireModule(const Name: String; ACompiler: TThoriumCompilerClass; NeededHash: PThoriumHash = nil): TThoriumModule; virtual;
    function DoOpenModule(const ModuleName: String): TStream; virtual;
  public
    procedure ClearLibraries;
    procedure ClearModules;
    function FindLibrary(const Name: String): TThoriumLibrary;
    function FindModule(const Name: String; AllowLoad: Boolean = True; ACompiler: TThoriumCompilerClass = nil): TThoriumModule;
    function IndexOfModule(const AModule: TThoriumModule): Integer;
    procedure InitializeVirtualMachine;
    function LoadLibrary(const ALibrary: TThoriumLibraryClass): TThoriumLibrary;
    function LoadModuleFromFile(AModuleName: String; ACompiler: TThoriumCompilerClass; NeededHash: PThoriumHash = nil): TThoriumModule;
    function LoadModuleFromStream(AStream: TStream; ACompiler: TThoriumCompilerClass; AName: String = ''; NeededHash: PThoriumHash = nil): TThoriumModule;
    function NewModule(AName: String = ''): TThoriumModule;
    procedure ReleaseVirtualMachine;
  public
    property HostLibrary[Index: Integer]: TThoriumLibrary read GetLibrary;
    property HostLibraryCount: Integer read GetLibraryCount;
    property Locked: Boolean read GetLocked;
    property Module[Index: Integer]: TThoriumModule read GetModule;
    property ModuleCount: Integer read GetModuleCount;
    property OnCompilerOutput: TThoriumOnCompilerOutput read FOnCompilerOutput write FOnCompilerOutput;
    property OnOpenModule: TThoriumOnOpenModule read FOnOpenModule write FOnOpenModule;
    property OnRequireModule: TThoriumOnRequireModule read FOnRequireModule write FOnRequireModule;
    property TypeFloat: TThoriumTypeFloat read FTypeFloat;
    property TypeInteger: TThoriumTypeInteger read FTypeInteger;
    property TypeString: TThoriumTypeString read FTypeString;
    property VirtualMachine: TThoriumVirtualMachine read FVirtualMachine;
  end;

{%ENDREGION}

function ThoriumValueToStr(const Value: TThoriumValue): String;
function ThoriumMakeOOPEvent(ACode: Pointer; Userdata: Pointer): TMethod;
function ThoriumRegisterToStr(ARegisterID: TThoriumRegisterID): String;
function ThoriumInstructionToStr(AInstruction: TThoriumInstruction): String;
procedure ThoriumFreeValue(var AValue: TThoriumValue); inline;
procedure ThoriumReleaseValue(var AValue: TThoriumValue); inline;
function ThoriumDuplicateValue(const AValue: TThoriumValue): TThoriumValue; inline;
function ThoriumIncRef(const AValue: TThoriumValue): TThoriumValue; inline;

function HostRecordField(const AType: TThoriumHostType;
  const AName: String; const AOffset: Cardinal): TThoriumHostRecordField;
{
function HostVarType(const AHostType: TThoriumHostType;
  const AExtended: TThoriumHostObjectType = nil; const AStoring: Boolean = False): TThoriumHostTypeSpec;
}


operator := (Input: TThoriumInstructionArray): TThoriumGenericOperation;

operator := (Input: TThoriumValue): TThoriumInitialData;

function ThoriumEncapsulateOperation(const AOperation: TThoriumOperationDescription;
  const TargetRI: TThoriumRegisterID = THORIUM_REGISTER_INVALID;
  const Value1RI: TThoriumRegisterID = THORIUM_REGISTER_INVALID;
  const Value2RI: TThoriumRegisterID = THORIUM_REGISTER_INVALID;
  const ClearRegisters: TThoriumGenericOperationRegisters = []): TThoriumGenericOperation;

function GetDebugMode: Boolean;
property DebugMode: Boolean read GetDebugMode;

implementation

(*uses
  Thorium_DefaultCompiler;*)

var FDebugMode: Boolean = {$ifdef DebugToConsole}True{$else}False{$endif};

{$ifdef HookSIGUSR1}
var
  SigCurrCompiler: TThoriumCustomCompiler;
  SigCurrModule: TThoriumModule;

procedure HandleSigUSR1(signal: longint); cdecl;
begin
  WriteLn;
  WriteLn(':: signal sigusr1 received ::');
  WriteLn('Current module: ', SigCurrModule.Name);
  WriteLn('Current instruction set: ');
  SigCurrCompiler.FInstructions.DumpCodeStr;
  WriteLn;
  WriteLn('Current compiler state:');
  SigCurrCompiler.DumpState;
  WriteLn(':: end sigusr1 output ::');
end;

{$endif}

{$I Thorium_InstructionConstructors.inc}

{%REGION 'Native call helpers' /fold}
var
  // These offsets are needed by GenericPrecompile. Don't ask me why there is no
  // possibility to let the compiler do the work.
  STACKENTRY_TYPE_OFFSET: SizeUInt;
  STACKENTRY_DATA_OFFSET: SizeUInt;
  STACKENTRY_VADATA_OFFSET: SizeUInt;


{ The following two types, one constant and two functions were taken from
  astrings.inc of the FreePascal RunTimeLibrary and have been modified by the
  author of Thorium to fit the special needs of the direct call feature. }

type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = Packed Record
    Ref,
    Len   : SizeInt;
    First : Char;
  end;

const
  FirstOff   = SizeOf(TAnsiRec)-1;

procedure ExtractedAnsiStrIncrRef(S: Pointer);
{Original function name: fpc_ansistr_incr_ref}
begin
  If S=Nil then
    exit;
  if IsMultiThread then
    {$ifdef CPU64}
    InterLockedIncrement64(PAnsiRec(S-FirstOff)^.Ref)
    {$else}
    InterLockedIncrement(PAnsiRec(S-FirstOff)^.Ref)
    {$endif}
  else
    Inc(PAnsiRec(S-FirstOff)^.Ref);
end;

procedure ExtractedAnsiStrDecrRef(var S: Pointer);
{Original function name: fpc_ansistr_decr_ref}
Var
  l : pSizeInt;
Begin
  If S=Nil then exit;
  l:=@PAnsiRec(S-FirstOff)^.Ref;
  if IsMultiThread then
  begin
    {$ifdef CPU64}
    If InterLockedDecrement64(l^) = 0 then
    {$else}
    If InterLockedDecrement(l^) = 0 then
    {$endif}
    begin
      Dec(S, FirstOff);
      FreeMem(S);
      S := nil;
    end;
  end
  else
  begin
    Dec(l^);
    if l^ = 0 then
    begin
      Dec(S, FirstOff);
      FreeMem(S);
      S := nil;
    end;
  end;
end;

(* Generates NativeCall subscript code which can later be used to call a native
function without knowing its signature at (program) compile time. *)
(*procedure GenericPrecompile(var AInstructions: Pointer; var AVAOffset: Integer; Parameters: TThoriumHostParameters; ReturnType: TThoriumHostType; HasData: Boolean; CallingConvention: TThoriumNativeCallingConvention);
var
  Instructions: array of TThoriumNativeCallInstruction;
  Capacity: Integer;
  Count: Integer;
  ParamCount: Integer;
  VAOffset: Integer; // Used for varargs

  function GetNextBlock: PThoriumNativeCallInstruction;
  begin
    if Count = Capacity then
    begin
      Inc(Capacity, 16);
      SetLength(Instructions, Capacity);
    end;
    Result := @Instructions[Count];
    Inc(Count);
  end;

  function EntryBaseOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := AIndex * SizeOf(TThoriumStackEntry);
  end;

  function TypeOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := EntryBaseOffset(AIndex) - STACKENTRY_TYPE_OFFSET;
  end;

  function IntOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := EntryBaseOffset(AIndex) - STACKENTRY_DATA_OFFSET;
  end;

  function VADataOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := EntryBaseOffset(AIndex) - STACKENTRY_VADATA_OFFSET;
  end;

var
  I: Integer;
  HostType, CleanHostType: TThoriumHostType;
  PreCalcVAOffset: Integer;
begin
  Capacity := 0;
  Count := 0;
  PreCalcVAOffset := 0;
  {$ifndef CPU64}
  if CallingConvention <> ncRegister then
    with GetNextBlock()^ do
    begin
      Instruction := ccSkipRegister;
      Offset := 0;
      Mask := 0;
    end;
  {$endif}

  ParamCount := Parameters.Count;
  for I := 0 to ParamCount - 1 do
    if Parameters.Types[I] and htArray = htArray then
      Inc(PreCalcVAOffset);
  AVAOffset := PreCalcVAOffset;
  if HasData then
  begin
    with GetNextBlock()^ do
    begin
      Instruction := ccData;
      Offset := 0;
      Mask := 0;
    end;
  end;
  {$ifndef CPU64}
  if CallingConvention = ncRegister then
  begin
  {$endif}
    VAOffset := PreCalcVAOffset;
    for I := 0 to ParamCount - 1 do
    begin
      HostType := Parameters.Types[I];
      CleanHostType := HostType and (not htFlagSection);
      with GetNextBlock()^ do
      begin
        if HostType and htArray = htArray then
        begin
          Instruction := ccVA;
          Mask := VADataOffset((ParamCount-(I+1))+VAOffset); // address of the array
          Dec(VAOffset);
          Offset := IntOffset((ParamCount-(I+1))+VAOffset); // address of the count
        end
        else
        begin
          case CleanHostType of
            htIntS8, htIntS16, htIntS32, htIntS64,
            htIntU8, htIntU16, htIntU32, htIntU64:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt32:
            begin
              Instruction := ccSingle;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt64:
            begin
              Instruction := ccDouble;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt80:
            begin
              Instruction := ccExtended;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htString:
            begin
              Instruction := ccPtrDeref;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htExt:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
          end;
          if Parameters.Types[I] and htByRef = htByRef then
            Inc(Instruction, DEREF_OFFSET);
        end;
      end;
    end;
    with GetNextBlock()^ do
    begin
      Mask := 0;
      case ReturnType.HostType and (not htFlagSection) of
        htNone:
        begin
          Instruction := ccCall;
          Offset := 0;
        end;
        htIntS8, htIntS16, htIntS32, htIntS64,
        htIntU8, htIntU16, htIntU32, htIntU64:
        begin
          Instruction := ccCallRetInt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt32:
        begin
          Instruction := ccCallRetSingle;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt64:
        begin
          Instruction := ccCallRetDouble;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt80:
        begin
          Instruction := ccCallRetExtended;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htString:
        begin
          Instruction := ccCallRetString;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htExt:
        begin
          Instruction := ccCallRetExt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
          Mask := SizeInt(ReturnType.Extended);
        end;
      end;
    end;
  {$ifndef CPU64}
  end
  else
  begin
    VAOffset := 0;
    for I := ParamCount - 1 downto 0 do
    begin
      HostType := Parameters.Types[I];
      CleanHostType := HostType and (not htFlagSection);
      with GetNextBlock()^ do
      begin
        if HostType and htArray = htArray then
        begin
          Instruction := ccVARev;
          Offset := IntOffset((ParamCount-(I+1))+VAOffset); // address of the count
          Inc(VAOffset);
          Mask := VADataOffset((ParamCount-(I+1))+VAOffset); // address of the array
        end
        else
        begin
          case CleanHostType of
            htIntS8, htIntS16, htIntS32,
            htIntU8, htIntU16, htIntU32:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htIntS64, htIntU64:
            begin
              Instruction := ccInt64;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt32:
            begin
              Instruction := ccSingle;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt64:
            begin
              Instruction := ccDouble;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt80:
            begin
              Instruction := ccExtended;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htString:
            begin
              Instruction := ccPtrDeref;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htExt:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
          end;
          if HostType and htFlagSection = htByRef then
            Inc(Instruction, DEREF_OFFSET);
        end;
      end;
    end;
    with GetNextBlock()^ do
    begin
      Mask := 0;
      case ReturnType.HostType and (not htFlagSection) of
        htNone:
        begin
          Instruction := ccCall;
          Offset := 0;
        end;
        htIntS8, htIntS16, htIntS32,
        htIntU8, htIntU16, htIntU32:
        begin
          Instruction := ccCallRetInt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htIntS64, htIntU64:
        begin
          Instruction := ccCallRetInt64;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt32:
        begin
          Instruction := ccCallRetSingle;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt64:
        begin
          Instruction := ccCallRetDouble;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt80:
        begin
          Instruction := ccCallRetExtended;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htString:
        begin
          Instruction := ccCallRetString;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htExt:
        begin
          Instruction := ccCallRetExt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
          Mask := SizeInt(ReturnType.Extended);
        end;
      end;
    end;
  end;
  {$endif}
  VAOffset := PreCalcVAOffset;
  for I := 0 to ParamCount - 1 do
  begin
    HostType := Parameters.Types[I];
    if HostType and htArray = htArray then
      Dec(VAOffset)
    else if HostType = htFlt32 then
    begin
      with GetNextBlock()^ do
      begin
        Instruction := ccRevSingle;
        Offset := IntOffset((ParamCount-(I+1))+VAOffset);
        Mask := 0;
      end;
    end
    else if HostType = htFlt80 then
    begin
      with GetNextBlock()^ do
      begin
        Instruction := ccRevExtended;
        Offset := IntOffset((ParamCount-(I+1))+VAOffset);
        Mask := 0;
      end;
    end;
  end;
  with GetNextBlock()^ do
  begin
    Instruction := ccExit;
    Offset := 0;
    Mask := 0;
  end;
  if AInstructions <> nil then
    FreeMem(AInstructions);
  AInstructions := GetMem(Count * SizeOf(TThoriumNativeCallInstruction));
  Move(Instructions[0], AInstructions^, Count * SizeOf(TThoriumNativeCallInstruction));
end;    *)

procedure GenericPrecompile(var AInstructions: Pointer;
    AParameters: TThoriumHostParameters; AThParameters: TThoriumParameters;
    AReturnType: TThoriumHostType; AThReturnType: TThoriumType; HasData: Boolean;
    CallingConvention: TThoriumNativeCallingConvention);
begin

end;

{
procedure GenericCallbackPrecompile(var Instructions: Pointer; Parmeters: array of TThoriumHostType; ReturnType: TThoriumHostType);
begin

end;
}

{$ifdef CPU32}
label
  irEAX, irECX, irEDX, irPush,
  ivEAX, ivECX, ivEDX, ivPush,
  ivrEAX, ivrECX, ivrEDX, ivrPush,
  iSkipRegister, iIncStrRef, iPtrDeref, iDecStrRef, iData, iVA, iVARev, iVA2, iVA2Rev, iVAEmpty, iSingle, iDouble, iExtended, iInt, iInt64, iIntRef,
  iSingleRef, iDoubleRef, iExtendedRef, iCall, iCallRetInt, iCallRetInt64, iCallRetSingle, iCallRetDouble, iCallRetExtended,
  iCallRetString, iCallRetExt,
  iDecStack, iRevSingle, iRevExtended,
  lLoop, lExit, iNullPointer;
const
  IntReg : array [0..3] of Pointer = (
    @irEAX, @irEDX, @irECX, @irPush
  );
  IntVAReg : array [0..3] of Pointer = (
    @ivEAX, @ivEDX, @ivECX, @ivPush
  );
  IntVARevReg : array [0..3] of Pointer = (
    @ivrEAX, @ivrEDX, @ivrECX, @ivrPush
  );
  InstrMap: array [TThoriumNativeCallInstructionCode] of Pointer = (
    @iSkipRegister, @iIncStrRef, @iVARev, @iPtrDeref, @iDecStrRef, @iData, @iVA, @iInt, @iInt64, @iSingle, @iDouble, @iExtended,
    @iIntRef, @iIntRef, @iSingleRef, @iDoubleRef, @iExtendedRef, @iCall, @iCallRetInt, @iCallRetInt64,
    @iCallRetSingle, @iCallRetDouble, @iCallRetExtended, @iCallRetString, @iCallRetExt,
    @iDecStack, @iRevSingle, @iRevExtended, @lExit
  );
var
  IntRegPtr: Pointer = @IntReg;
  IntVARegPtr: Pointer = @IntVAReg;
  IntVARevRegPtr: Pointer = @IntVARevReg;

procedure ExecuteSubscript(Instructions: Pointer; StackTop: Pointer; Method: Pointer; Data: Pointer = nil); stdcall;
// up to 3 ints in eax ecx edx
var
  EAX, ECX, EDX, SEBX, SECX, SEDX, SESI, SESP, SEDI, SEAX1, SEBX1, SECX1, SEDX1, SESI1, SEDI1: DWord;
begin
  // eax : buffer
  // ebx : instructions ptr
  // ecx : int idx
  // edx : value ptr
  // esi : stack top
  // edi : instruction map ptr
  asm
    movl %eax, SEAX1
    movl %ebx, SEBX1
    movl %ecx, SECX1
    movl %edx, SEDX1
    movl %esi, SESI1
    movl %edi, SEDI1
    movl Instructions, %ebx
    movl $0, %ecx
    movl StackTop, %esi
    leal InstrMap, %edi
    movl %esp, SESP
    lLoop:
      movl $0, %eax
      movw (%ebx), %ax
      addl $2, %ebx

      movl %esi, %edx
      subl (%ebx), %edx

      addl $4, %ebx

      imull $4, %eax
      addl %edi, %eax
      jmp (%eax)
    iSkipRegister:
      movl $3, %ecx
      addl $4, %ebx
      jmp lLoop
    iIncStrRef:
      jmp lLoop
    iPtrDeref:
      movl (%edx), %edx
      cmp $0, %edx
      jz iNullPointer
      movl (%edx), %edx
    iNullPointer:
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      addl $4, %ebx
      jmp (%eax)
    iDecStrRef:
      jmp lLoop
    iData:
      movl Data, %edx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      addl $4, %ebx
      jmp (%eax)
    iVARev:
      movl (%edx), %edx
      movl %ecx, %eax
      imull $4, %eax
      addl IntVARevRegPtr, %eax
      jmp (%eax)
    iVA2Rev:
      cmp $0, %edx
      jz lLoop
      movl %esi, %edx
      subl (%ebx), %edx
      movl (%edx), %edx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      addl $4, %ebx
      jmp (%eax)
    iVA:
      movl (%edx), %edx
      cmp $0, %edx
      jz iVAEmpty
      movl %edx, SEDX
      movl %esi, %edx
      subl (%ebx), %edx
      movl (%edx), %edx
      movl %ecx, %eax
      imull $4, %eax
      addl IntVARegPtr, %eax
      jmp (%eax)
    iVA2:
      movl SEDX, %edx
    iVAEmpty:
      decl %edx
      addl $4, %ebx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      jmp (%eax)
    ivEAX:
      movl %edx, EAX
      incl %ecx
      jmp iVA2
    ivEDX:
      movl %edx, EDX
      incl %ecx
      jmp iVA2
    ivECX:
      movl %edx, ECX
      incl %ecx
      jmp iVA2
    ivPush:
      pushl %edx
      jmp iVA2
    ivrEAX:
      movl %edx, EAX
      incl %ecx
      jmp iVA2Rev
    ivrEDX:
      movl %edx, EDX
      incl %ecx
      jmp iVA2Rev
    ivrECX:
      movl %edx, ECX
      incl %ecx
      jmp iVA2Rev
    ivrPush:
      pushl %edx
      jmp iVA2Rev
    iSingle:
    	fldl (%edx)
      movl $0, 4(%edx)
    	fstps (%edx)
      pushl (%edx)
      addl $4, %ebx
      jmp lLoop
    iDouble:
      subl $8, %esp
      movl (%edx), %eax
      movl %eax, (%esp)
      movl 4(%edx), %eax
      movl %eax, 4(%esp)
      addl $4, %ebx
      jmp lLoop
    iExtended:
      fldl (%edx)
    	fstpt	(%edx)
      addl $4, %ebx
      movzwl 8(%edx), %eax
      pushl %eax
      pushl 4(%edx)
      pushl (%edx)
      jmp lLoop
    iInt:
      movl (%edx), %edx
      addl $4, %ebx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      jmp (%eax)
    iInt64:
      pushl 4(%edx)
      pushl (%edx)
      addl $4, %ebx
      jmp lLoop
    iIntRef:
      addl $4, %ebx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      jmp (%eax)
    iSingleRef:
    	fldl (%edx)
      movl $0, 4(%edx)
    	fstps (%edx)
      addl $4, %ebx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      jmp (%eax)
    iDoubleRef:
      addl $4, %ebx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      jmp (%eax)
    iExtendedRef:
      fldl (%edx)
    	fstpt	(%edx)
      addl $4, %ebx
      movl %ecx, %eax
      imull $4, %eax
      addl IntRegPtr, %eax
      jmp (%eax)
    iCall:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDI, %edi
      movl SESI, %esi
      movl SEDX, %edx
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      jmp lLoop
    iCallRetInt:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDX, %edx
      movl %eax, (%edx)
      movl $0, 4(%edx)
      movl SEDI, %edi
      movl SESI, %esi
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      jmp lLoop
    iCallRetInt64:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDX, %ebx
      movl %eax, (%ebx)
      movl %edx, 4(%ebx)
      movl %ebx, %edx
      movl SEDI, %edi
      movl SESI, %esi
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      jmp lLoop
    iCallRetSingle:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDI, %edi
      movl SESI, %esi
      movl SEDX, %edx
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      fstpl (%edx)
      jmp lLoop
    iCallRetDouble:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDI, %edi
      movl SESI, %esi
      movl SEDX, %edx
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      fstpl (%edx)
      jmp lLoop
    iCallRetExtended:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDI, %edi
      movl SESI, %esi
      movl SEDX, %edx
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      fstpl (%edx)
      jmp lLoop
    iCallRetString:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDX, %edx
      movl %eax, EAX
      movl (%edx), %edx
      movl %edx, SEDX
      movl %edx, %eax
      call ExtractedAnsiStrDecrRef
      movl SEDX, %edx
      movl EAX, %eax
      movl %eax, (%edx)
      movl SEDI, %edi
      movl SESI, %esi
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      jmp lLoop
    iCallRetExt:
      movl %ebx, SEBX
      movl %ecx, SECX
      movl %edx, SEDX
      movl %esi, SESI
      movl %edi, SEDI
      movl EAX, %eax
      movl ECX, %ecx
      movl EDX, %edx
      call Method
      movl SEDX, %edx
      movl %eax, (%edx)
      movl (%ebx), %eax
      movl %eax, 4(%edx) // copy the value from mask to the TypeClass field
      movl SEDI, %edi
      movl SESI, %esi
      movl SECX, %ecx
      movl SEBX, %ebx
      addl $4, %ebx
      jmp lLoop
    iDecStack:
      addl (%ebx), %esp
      addl $4, %ebx
      jmp lLoop
    iRevSingle:
    	flds (%edx)
      movl $0, 4(%edx)
    	fstpl (%edx)
      addl $4, %ebx
      jmp lLoop
    iRevExtended:
    	fldt (%edx)
      movl $0, 4(%edx)
    	fstpl (%edx)
      addl $4, %ebx
      jmp lLoop
    irEAX:
      movl %edx, EAX
      incl %ecx
      jmp lLoop
    irECX:
      movl %edx, ECX
      incl %ecx
      jmp lLoop
    irEDX:
      movl %edx, EDX
      incl %ecx
      jmp lLoop
    irPush:
      pushl %edx
      jmp lLoop
    lExit:
      movl SEAX1, %eax
      movl SEBX1, %ebx
      movl SECX1, %ecx
      movl SEDX1, %edx
      movl SESI1, %esi
      movl SEDI1, %edi
      movl SESP, %esp
  end;
end;
{$else}
label
  irRDI, irRSI, irRDX, irRCX, irR8, irR9, irPush,
  frXMM0, frXMM1, frXMM2, frXMM3, frXMM4, frXMM5, frXMM6, frXMM7, frPushSingle,
  drXMM0, drXMM1, drXMM2, drXMM3, drXMM4, drXMM5, drXMM6, drXMM7, frPushDouble,
  iSkipRegister, iIncStrRef, iPtrDeref, iDecStrRef, iData, iVA, iVA2, iVAEmpty, iSingle, iDouble, iExtended, iInt, iIntRef,
  iSingleRef, iDoubleRef, iExtendedRef, iCall, iCallRetInt, iCallRetSingle, iCallRetDouble, iCallRetExtended,
  iCallRetString, iCallRetExt,
  iDecStack, iRevSingle, iRevExtended,
  lLoop, lExit, iNullPointer;
const
  IntReg : array [0..6] of Pointer = (
    @irRDI, @irRSI, @irRDX, @irRCX, @irR8, @irR9, @irPush
  );
  SingleReg: array [0..8] of Pointer = (
    @frXMM0, @frXMM1, @frXMM2, @frXMM3, @frXMM4, @frXMM5, @frXMM6, @frXMM7, @frPushSingle
  );
  DoubleReg: array [0..8] of Pointer = (
    @drXMM0, @drXMM1, @drXMM2, @drXMM3, @drXMM4, @drXMM5, @drXMM6, @drXMM7, @frPushDouble
  );
  InstrMap: array [TThoriumNativeCallInstructionCode] of Pointer = (
    @iSkipRegister, @iIncStrRef, @iPtrDeref, @iDecStrRef, @iData, @iVA, @iInt, @iInt, @iSingle, @iDouble, @iExtended,
    @iIntRef, @iIntRef, @iSingleRef, @iDoubleRef, @iExtendedRef, @iCall, @iCallRetInt, @iCallRetInt,
    @iCallRetSingle, @iCallRetDouble, @iCallRetExtended, @iCallRetString, @iCallRetExt,
    @iDecStack, @iRevSingle, @iRevExtended, @lExit, nil, nil, nil, nil, nil, nil, nil
  );
var
  IntRegPtr: Pointer = @IntReg;
  SingleRegPtr: Pointer = @SingleReg;
  DoubleRegPtr: Pointer = @DoubleReg;

procedure ExecuteSubscript(Instructions: Pointer; StackTop: Pointer; Method: Pointer; Data: Pointer = nil);
// * up to 6 ints (or similar) in registers: rdi, rsi, rdx, rcx, r8, r9
// * up to 8 doubles (or singles) in registers: xmm0 through xmm7
// * extendeds are segmented and pushed onto the stack
var
  RDI, RAX, RBX, R10, R11, R12, R13, R14, R15: QWord;
begin
  // rax: buffer
  // rbx: back jump addr
  // rcx: param
  // rdx: param
  // rdi: param
  // rsi: param
  // r8 : param
  // r9 : param
  // r10: stack top
  // r11: int idx
  // r12: Instruction ptr
  // r13: Instruction map ptr
  // r14: value ptr
  // r15: flt idx
  asm
    movq %rax, RAX
    movq %rbx, RBX
    movq %r10, R10
    movq %r11, R11
    movq %r12, R12
    movq %r13, R13
    movq %r14, R14
    movq %r15, R15
    movq Instructions, %r12
    movq StackTop, %r10
    leaq InstrMap, %r13
    movq $0, %r11
    movq $0, %r15
    leaq lLoop, %rbx
    lLoop:
      xorq %rax, %rax
      movw (%r12), %ax
      addq $2, %r12

      movq %r10, %r14
      subq (%r12), %r14

      addq $8, %r12

      imulq $8, %rax
      addq %r13, %rax
      movq (%rax), %rax
      jmp %rax
    iSkipRegister:
      movq $6, %r11
      movq $8, %r15
      addq $8, %r12
      jmp lLoop
    iIncStrRef:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      movq %rdi, RDI
      movq (%r14), %rdi
      movq (%rdi), %rdi // double deref, needs String
      call ExtractedAnsiStrIncrRef
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      movq RDI, %rdi
      addq $8, %r12
      jmp lLoop
    iPtrDeref:
      movq (%r14), %r14
      cmp $0, %r14
      jz iNullPointer
      movq (%r14), %r14
    iNullPointer:
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iDecStrRef:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      movq %rdi, RDI
      movq (%r14), %rdi // no double deref here! needs pstring
      call ExtractedAnsiStrDecrRef
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      movq RDI, %rdi
      addq $8, %r12
      jmp lLoop
    iData:
      movq Data, %r14
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iVA:
      movq (%r14), %r14
      cmp $0, %r14
      jz iVAEmpty
      leaq iVA2, %rbx
      movq %r14, RDI
      movq %r10, %r14
      subq (%r12), %r14
      movq (%r14), %r14
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      jmp (%rax)
    iVA2:
      leaq lLoop, %rbx
      movq RDI, %r14
    iVAEmpty:
      decq %r14
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iInt:
      movq (%r14), %r14
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iSingle:
    	fldl (%r14)
      movq $0, (%r14)
    	fstps (%r14)
      movq %r15, %rax
      imulq $8, %rax
      addq SingleRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iDouble:
      movq %r15, %rax
      imulq $8, %rax
      addq DoubleRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iExtended:
      fldl (%r14)
      movq $0, (%r14)
      movq $0, 8(%r14)
    	fstpt	(%r14)
      addq $8, %r12
      subq $16, %rsp
      movq 8(%r14), %rax
      movq %rax, 8(%rsp)
      movq (%r14), %rax
      movq %rax, (%rsp)
      jmp lLoop
    iIntRef:
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iSingleRef:
    	fldl (%r14)
      movq $0, (%r14)
    	fstps (%r14)
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iDoubleRef:
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iExtendedRef:
      fldl (%r14)
      movq $0, (%r14)
      movq $0, 8(%r14)
    	fstpt	(%r14)
      movq %r11, %rax
      imulq $8, %rax
      addq IntRegPtr, %rax
      addq $8, %r12
      jmp (%rax)
    iCall:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      call Method
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      addq $8, %r12
      jmp lLoop
    iCallRetInt:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      call Method
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      addq $8, %r12
      movq %rax, (%r14)
      jmp lLoop
    iCallRetSingle:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      call Method
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      addq $8, %r12
      movlps %xmm0, (%r14)
      flds (%r14)
      movq $0, (%r14)
      fstpl (%r14)
      jmp lLoop
    iCallRetDouble:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      call Method
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      addq $8, %r12
      movlps %xmm0, (%r14)
      jmp lLoop
    iCallRetExtended:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      call Method
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      addq $8, %r12
      fstpl (%r14)
      jmp lLoop
    iCallRetString:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      call Method
      movq %rax, %r10
      //movq %r10, %rdi
      //call ExtractedAnsiStrIncrRef
      //movq %r10, %rdi
      //call ExtractedAnsiStrIncrRef
      //movq %r10, %rdi
      //call ExtractedAnsiStrIncrRef
      movq (%r14), %r14
      movq %r14, %rdi
      call ExtractedAnsiStrDecrRef
      movq %r10, (%r14)
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      addq $8, %r12
      jmp lLoop
    iCallRetExt:
      (*movq %r10, R10
      movq %r12, R12
      movq %r13, R13*)
      call Method
      (*movq R10, %r10
      movq R12, %r12
      movq R13, %r13*)
      movq %rax, (%r14)
      movq (%r12), %rax
      movq %rax, 8(%r14)
      addq $8, %r12
      jmp lLoop
    iDecStack:
      addq (%r12), %rsp
      addq $8, %r12
      jmp lLoop
    iRevSingle:
      flds (%r14)
      movq $0, (%r14)
      fstpl (%r14)
      addq $8, %r12
      jmp lLoop
    iRevExtended:
      fldt (%r14)
      movq $0, (%r14)
      fstpl (%r14)
      addq $8, %r12
      jmp lLoop
    irRDI:
      movq %r14, %rdi
      incq %r11
      jmp %rbx
    irRSI:
      movq %r14, %rsi
      incq %r11
      jmp %rbx
    irRDX:
      movq %r14, %rdx
      incq %r11
      jmp %rbx
    irRCX:
      movq %r14, %rcx
      incq %r11
      jmp %rbx
    irR8:
      movq %r14, %r8
      incq %r11
      jmp %rbx
    irR9:
      movq %r14, %r9
      incq %r11
      jmp %rbx
    irPush:
      pushq %r14
      jmp %rbx
    frXMM0:
      movlps (%r14), %xmm0
      incq %r15
      jmp lLoop
    frXMM1:
      movlps (%r14), %xmm1
      incq %r15
      jmp lLoop
    frXMM2:
      movlps (%r14), %xmm2
      incq %r15
      jmp lLoop
    frXMM3:
      movlps (%r14), %xmm3
      incq %r15
      jmp lLoop
    frXMM4:
      movlps (%r14), %xmm4
      incq %r15
      jmp lLoop
    frXMM5:
      movlps (%r14), %xmm5
      incq %r15
      jmp lLoop
    frXMM6:
      movlps (%r14), %xmm6
      incq %r15
      jmp lLoop
    frXMM7:
      movlps (%r14), %xmm7
      incq %r15
      jmp lLoop
    frPushSingle:
      pushq (%r14)
      jmp lLoop
    drXMM0:
      movlps (%r14), %xmm0
      incq %r15
      jmp lLoop
    drXMM1:
      movlps (%r14), %xmm1
      incq %r15
      jmp lLoop
    drXMM2:
      movlps (%r14), %xmm2
      incq %r15
      jmp lLoop
    drXMM3:
      movlps (%r14), %xmm3
      incq %r15
      jmp lLoop
    drXMM4:
      movlps (%r14), %xmm4
      incq %r15
      jmp lLoop
    drXMM5:
      movlps (%r14), %xmm5
      incq %r15
      jmp lLoop
    drXMM6:
      movlps (%r14), %xmm6
      incq %r15
      jmp lLoop
    drXMM7:
      movlps (%r14), %xmm7
      incq %r15
      jmp lLoop
    frPushDouble:
      pushq (%r14)
      jmp lLoop
    lExit:
      movq RAX, %rax
      movq RBX, %rbx
      movq R10, %r10
      movq R11, %r11
      movq R12, %r12
      movq R13, %r13
      movq R14, %r14
      movq R15, %r15
  end;
end;
{$endif}

procedure DirtyCallback;
begin
  // instance of TThoriumFunctionCallbackCapsule in 1st parameter slot
end;
{%ENDREGION}

{%REGION 'Timecheck' /fold}
{$ifdef Timecheck}
var
  Time1, Time2: Int64;
  Time: Cardinal;
  Freq: Int64;
  Perf: Boolean;
  Diff: Double;

procedure BeginTimecheck; inline;
begin
  Perf := QueryPerformanceFrequency(Freq);
  Perf := Perf and (Freq > 0);
  if Perf then
    QueryPerformanceCounter(Time1)
  else
    Time := GetTickCount;
end;

procedure EndTimecheck(Name: String); inline;
begin
  if Perf then
  begin
    QueryPerformanceCounter(Time2);
    Diff := ((Time2 - Time1) / Freq) * 1000;
  end
  else
    Diff := Time - GetTickCount;
  WriteLn(Format('[Timecheck ''%s'': %.8f ms]', [Name, Diff]));
end;
{$endif}
{%ENDREGION}

{%REGION 'Thorium helper funtions' /fold}

function HostRecordField(const AType: TThoriumHostType;
  const AName: String; const AOffset: Cardinal): TThoriumHostRecordField;
begin
  Result.FieldType := AType;
  Result.FieldName := AName;
  Result.Offset := AOffset;
end;

{
function HostVarType(const AHostType: TThoriumHostType;
  const AExtended: TThoriumHostObjectType = nil; const AStoring: Boolean = False): TThoriumHostType;
begin
  raise EThoriumException.Create('needs to be reimplemented.');
(*  Result.HostType := AHostType;
  Result.Extended := AExtended;
  if Result.HostType and (htTypeSection or htSizeSection) <> htExt then
  begin
    Result.Extended := nil;
    Result.Storing := False;
  end
  else
    Result.Storing := AStoring;*)
end;
}

operator:=(Input: TThoriumInstructionArray): TThoriumGenericOperation;
begin
  Result.Kind := okCustom;
  Result.Custom.Instructions := Input;
  Result.TargetRI := THORIUM_REGISTER_INVALID;
  Result.Value1RI := THORIUM_REGISTER_INVALID;
  Result.Value2RI := THORIUM_REGISTER_INVALID;
  Result.ClearRegisters := [];
end;

operator := (Input: TThoriumValue): TThoriumInitialData;
begin
  Result.Int := Input.Int;
end;

function ThoriumEncapsulateOperation(
  const AOperation: TThoriumOperationDescription;
  const TargetRI: TThoriumRegisterID; const Value1RI: TThoriumRegisterID;
  const Value2RI: TThoriumRegisterID;
  const ClearRegisters: TThoriumGenericOperationRegisters): TThoriumGenericOperation;
begin
  Result.Kind := okOperation;
  Result.Operation := AOperation;
  Result.TargetRI := TargetRI;
  Result.Value1RI := Value1RI;
  Result.Value2RI := Value2RI;
  Result.ClearRegisters := ClearRegisters;
end;

function GetDebugMode: Boolean;
begin
  Result := FDebugMode;
end;

procedure ThoriumDumpInstructions(const AInstructions: TThoriumInstructionArray);
var
  I: Integer;
begin
  WriteLn('-- Instruction array dump');
  for I := 0 to Length(AInstructions) - 1 do
  begin
    WriteLn(ThoriumInstructionToStr(AInstructions[I]));
  end;
  WriteLn('-- END Instruction array dump');
end;

procedure WriteHash(const AHash: TThoriumHash);
type
  THash = array [0..15] of Byte;
var
  I: Integer;
begin
  for I := 0 to 15 do
    Write(IntToHex(THash(AHash)[I], 2));
end;

function OperationInstructionDescription(const AInstruction: TThoriumInstruction;
  Op1: Integer = 0; Op2: Integer = 1; Target: Integer = 2): TThoriumOperationInstructionDescription;
begin
  Result.Instruction := TThoriumInstructionREG(AInstruction);
  Result.Value1RIOffset := Op1;
  Result.Value2RIOffset := Op2;
  Result.TargetRIOffset := Target;
end;

function AccessDescription(const AInstruction: TThoriumInstruction;
  AValueRegister: Integer = -1; ATargetRegister: Integer = -1;
  AExtendedRegister: Integer = -1; AIndexRegister: Integer = -1): TThoriumAccess;
begin
  Result.Allowed := True;
  Result.Instruction := TThoriumInstructionREG(AInstruction);
  Result.IndexRIOffset := AIndexRegister;
  Result.ValueRIOffset := AValueRegister;
  Result.ExtendedRIOffset := AExtendedRegister;
  Result.TargetRIOffset := ATargetRegister;
end;

function CreationDescription(const AInstruction: TThoriumInstruction;
  ATargetRegister: Integer = -1): TThoriumCreateInstructionDescription;
begin
  Result.Instruction := TThoriumInstructionREG(AInstruction);
  Result.TargetRegisterOffset := ATargetRegister;
end;

function ThoriumValueToStr(const Value: TThoriumValue): String;
begin
  Result := 'Cannot convert yet.';
end;

function ThoriumMakeOOPEvent(ACode: Pointer; Userdata: Pointer): TMethod;
begin
  Result.Data := Userdata;
  Result.Code := ACode;
end;

function ThoriumRegisterToStr(ARegisterID: TThoriumRegisterID): String;
begin
  case ARegisterID of
    THORIUM_REGISTER_C_MIN..THORIUM_REGISTER_C_MAX: Result := 'c'+IntToStr(ARegisterID - THORIUM_REGISTER_C_MIN + 1);
    THORIUM_REGISTER_EXP_MIN..THORIUM_REGISTER_EXP_MAX: Result := 'exp'+IntToStr(ARegisterID - THORIUM_REGISTER_EXP_MIN + 1);
    THORIUM_REGISTER_INVALID: Result := 'inv';
  else
    Result := 'ERR';
  end;
end;

function ThoriumInstructionToStr(AInstruction: TThoriumInstruction): String;
begin
  Result := THORIUM_INSTRUCTION_CODE_NAME[AInstruction.Instruction] + ' ';
  case AInstruction.Instruction of
    tiINT_S: with TThoriumInstructionINT_S(AInstruction) do Result := Result + Format('$0x%.16x', [Value]);
    tiINT: with TThoriumInstructionINT(AInstruction) do Result := Result + Format('$0x%.16x %%%s', [Value, ThoriumRegisterToStr(TRI)]);
    tiINTB: with TThoriumInstructionINTB(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);

    tiFLT_S: with TThoriumInstructionFLT_S(AInstruction) do Result := Result + Format('$%.8ff', [Value]);
    tiFLT: with TThoriumInstructionFLT(AInstruction) do Result := Result + Format('$%.8ff %%%s', [Value, ThoriumRegisterToStr(TRI)]);

    tiSTR_S: with TThoriumInstructionSTR_S(AInstruction) do Result := Result + Format('', []);
    tiSTRL_S: with TThoriumInstructionSTRL_S(AInstruction) do Result := Result + Format('[$0x%.8x]', [Index]);
    tiSTR: with TThoriumInstructionSTR(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);
    tiSTRL: with TThoriumInstructionSTRL(AInstruction) do Result := Result + Format('[$0x%.8x] %%%s', [Index, ThoriumRegisterToStr(TRI)]);

    tiEXT_S: with TThoriumInstructionEXT_S(AInstruction) do Result := Result + Format('[$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x]', [ptrint(ExtendedType)]);
    tiEXT: with TThoriumInstructionEXT(AInstruction) do Result := Result + Format('[$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x] %%%s', [ptrint(ExtendedType), ThoriumRegisterToStr(TRI)]);

    tiFNC: with TThoriumInstructionFNC(AInstruction) do Result := Result + Format('[$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x] %%%s', [ptrint(FunctionRef), ThoriumRegisterToStr(TRI)]);
    tiXFNC: with TThoriumInstructionXFNC(AInstruction) do Result := Result + Format('[$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x] %%%s', [ptrint(FunctionRef), ThoriumRegisterToStr(TRI)]);

    tiMOVER_G,
    tiCOPYR_G: with TThoriumInstructionMOVER_G(AInstruction) do Result := Result + Format('%%%s global($0x%8.8x)', [ThoriumRegisterToStr(SRI), Offset]);
    tiMOVER_L,
    tiCOPYR_L: with TThoriumInstructionMOVER_L(AInstruction) do Result := Result + Format('%%%s local($0x%8.8x)', [ThoriumRegisterToStr(SRI), Offset]);
    tiMOVER_P,
    tiCOPYR_P: with TThoriumInstructionMOVER_P(AInstruction) do Result := Result + Format('%%%s param($0x%8.8x)', [ThoriumRegisterToStr(SRI), -(Offset+1)]);
    tiMOVEG,
    tiCOPYG: with TThoriumInstructionMOVEG(AInstruction) do Result := Result + Format('global($0x%8.8x) %%%s', [Offset, ThoriumRegisterToStr(TRI)]);
    tiMOVEL,
    tiCOPYL: with TThoriumInstructionMOVEL(AInstruction) do Result := Result + Format('local($0x%8.8x) %%%s', [Offset, ThoriumRegisterToStr(TRI)]);
    tiMOVEP,
    tiCOPYP: with TThoriumInstructionMOVEP(AInstruction) do Result := Result + Format('param($0x%8.8x) %%%s', [-(Offset+1), ThoriumRegisterToStr(TRI)]);

    tiMOVER_FG,
    tiCOPYR_FG: with TThoriumInstructionMOVER_FG(AInstruction) do Result := Result + Format('%%%s @%s($0x%8.8x)', [ThoriumRegisterToStr(SRI), Offset, TThoriumModule(ModuleRef).Name]);
    tiMOVEFG,
    tiCOPYFG: with TThoriumInstructionMOVEFG(AInstruction) do Result := Result + Format('@%s($0x%8.8x) %%%s', [TThoriumModule(ModuleRef).Name, Offset, ThoriumRegisterToStr(TRI)]);

    tiCOPYR_ST: with TThoriumInstructionCOPYR_ST(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(SRI)]);
    tiCOPYR: with TThoriumInstructionCOPYR(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);

    tiMOVER_ST: with TThoriumInstructionMOVER_ST(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(SRI)]);
    tiMOVER: with TThoriumInstructionMOVER(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);
    tiMOVEST: with TThoriumInstructionMOVEST(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);

    tiPOP_S: with TThoriumInstructionPOP_S(AInstruction) do Result := Result + Format('$0x%.8x', [Amount]);
    tiSTACKHINT: with TThoriumInstructionSTACKHINT(AInstruction) do Result := Result + Format('$0x%.8x', [Amount]);

    tiCLR: with TThoriumInstructionCLR(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);

    tiCASTIF: with TThoriumInstructionCASTIF(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);
    tiCASTIE: with TThoriumInstructionCASTIE(AInstruction) do Result := Result + Format('%%%s %%%s [$0x%.'+IntToStr(SizeOf(ptrint)*2)+'x]', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI), ExtendedType]);
    tiCASTFE: with TThoriumInstructionCASTFE(AInstruction) do Result := Result + Format('%%%s %%%s [$0x%.'+IntToStr(SizeOf(ptrint)*2)+'x]', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI), ExtendedType]);
    tiCASTSE: with TThoriumInstructionCASTSE(AInstruction) do Result := Result + Format('%%%s %%%s [$0x%.'+IntToStr(SizeOf(ptrint)*2)+'x]', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI), ExtendedType]);
    tiCASTEI: with TThoriumInstructionCASTEI(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);
    tiCASTEF: with TThoriumInstructionCASTEF(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);
    tiCASTES: with TThoriumInstructionCASTES(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);
    tiCASTE: with TThoriumInstructionCASTE(AInstruction) do Result := Result + Format('%%%s %%%s [$0x%.'+IntToStr(SizeOf(ptrint)*2)+'x]', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI), ExtendedType]);

    tiCMPI, tiCMPIF, tiCMPIE, tiCMPF, tiCMPFI, tiCMPFE, tiCMPS, tiCMPSE, tiCMPE, tiCMPEI, tiCMPEF, tiCMPES:
      with TThoriumInstructionCMPI(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2)]);

    tiADDI: with TThoriumInstructionADDI(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);
    tiADDF: with TThoriumInstructionADDF(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);
    tiADDS: with TThoriumInstructionADDS(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiSUBI: with TThoriumInstructionSUBI(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);
    tiSUBF: with TThoriumInstructionSUBF(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiMULI: with TThoriumInstructionMULI(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);
    tiMULF: with TThoriumInstructionMULF(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiDIVI: with TThoriumInstructionDIVI(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);
    tiDIVF: with TThoriumInstructionDIVF(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiNEGI: with TThoriumInstructionNEGI(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(Op1)]);
    tiNEGF: with TThoriumInstructionNEGF(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(Op1)]);

    tiNOT: with TThoriumInstructionNOT(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(Op1)]);
    tiBNOT: with TThoriumInstructionBNOT(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(Op1)]);

    tiMOD: with TThoriumInstructionMOD(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiAND: with TThoriumInstructionAND(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiOR: with TThoriumInstructionOR(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiXOR: with TThoriumInstructionXOR(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiSHL: with TThoriumInstructionSHL(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiSHR: with TThoriumInstructionSHR(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(Op1), ThoriumRegisterToStr(Op2), ThoriumRegisterToStr(TRI)]);

    tiINCI: with TThoriumInstructionINCI(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);
    tiINCF: with TThoriumInstructionINCF(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);

    tiDECI: with TThoriumInstructionDECI(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);
    tiDECF: with TThoriumInstructionDECF(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);

    tiXFGET: with TThoriumInstructionXFGET(AInstruction) do Result := Result + Format('$0x%.16x %%%s %%%s', [ID, ThoriumRegisterToStr(ERI), ThoriumRegisterToStr(TRI)]);
    tiXFSET: with TThoriumInstructionXFSET(AInstruction) do Result := Result + Format('%%%s $0x%.16x %%%s', [ThoriumRegisterToStr(VRI), ID, ThoriumRegisterToStr(ERI)]);

    tiXIGET: with TThoriumInstructionXIGET(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(IRI), ThoriumRegisterToStr(ERI), ThoriumRegisterToStr(TRI)]);
    tiXISET: with TThoriumInstructionXISET(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(VRI), ThoriumRegisterToStr(IRI), ThoriumRegisterToStr(ERI)]);

    tiXCT: with TThoriumInstructionXCT(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(ERI)]);

    tiVASTART: with TThoriumInstructionVASTART(AInstruction) do Result := Result + Format('$0x%.8x $%d', [Length, Pointers]);
    tiVASTART_T: with TThoriumInstructionVASTART_T(AInstruction) do Result := Result + Format('$0x%0.8x $0x%.8x', [Length, Floats]);

    tiVA_I8, tiVA_I8S, tiVA_I16, tiVA_I16S, tiVA_I32, tiVA_I32S, tiVA_I64, tiVA_I64S,
    tiVA_F32, tiVA_F64, tiVA_F80, tiVA_S, tiVA_X, tiVAT_I, tiVAT_F, tiVAT_S,
    tiVAT_X:
      with TThoriumInstructionVA_I8(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(SRI)]);

    tiVAFINISH: ;

    tiJMP: with TThoriumInstructionJMP(AInstruction) do Result := Result + Format('$0x%.8x', [NewAddress]);

    tiJE: with TThoriumInstructionJE(AInstruction) do Result := Result + Format('$0x%.8x', [NewAddress]);
    tiJNE: with TThoriumInstructionJNE(AInstruction) do Result := Result + Format('$0x%.8x', [NewAddress]);
    tiJGT: with TThoriumInstructionJGT(AInstruction) do Result := Result + Format('$0x%.8x', [NewAddress]);
    tiJGE: with TThoriumInstructionJGE(AInstruction) do Result := Result + Format('$0x%.8x', [NewAddress]);
    tiJLT: with TThoriumInstructionJLT(AInstruction) do Result := Result + Format('$0x%.8x', [NewAddress]);
    tiJLE: with TThoriumInstructionJLE(AInstruction) do Result := Result + Format('$0x%.8x', [NewAddress]);

    tiCALL: with TThoriumInstructionCALL(AInstruction) do Result := Result + Format('%%%s {%%%s}', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(HRI)]);

    tiCALL_D: with TThoriumInstructionCALL_D(AInstruction) do Result := Result + Format('$0x%.8x $0x%.8x', [EntryPoint, Pops]);

    tiFCALL: with TThoriumInstructionFCALL(AInstruction) do Result := Result + Format('%%%s {%%%s}', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(HRI)]);

    tiXCALL: with TThoriumInstructionXCALL(AInstruction) do Result := Result + Format('%%%s {%%%s}', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(HRI)]);
    tiXCALL_M: with TThoriumInstructionXCALL_M(AInstruction) do Result := Result + Format('%%%s->%%%s {%%%s}', [ThoriumRegisterToStr(ERI), ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(HRI)]);

    tiRET:;

    tiNOOP: with TThoriumInstructionNOOP(AInstruction) do Result := Result + Format('0x%.16x 0x%.16x 0x%.16x', [Parameter1, Parameter2, Parameter3], THORIUM_NUMBER_FORMAT);

    tiEmbeddedHint: with TThoriumInstructionEmbeddedHint(AInstruction) do Result := '.'+StrPas(@Data[0]);
  else
    Result := 'error';
  end;
end;

(*procedure ThoriumVarTypeToTypeSpec(VarType: TThoriumHostType; var TypeSpec: TThoriumType);
begin
  case VarType and (htSizeSection or htTypeSection) of
    htIntS8, htIntS16, htIntS32, htIntS64,
    htIntU8, htIntU16, htIntU32, htIntU64:
    begin
      TypeSpec := TThoriumTypeInteger.Create();
    end;
    htFlt32, htFlt64, htFlt80:
    begin
      TypeSpec := TThoriumTypeFloat.Create();
    end;
    htStr:
    begin
      TypeSpec := TThoriumTypeString.Create();
    end;
    htExt:
    begin
      raise EThoriumException.Create('Cannot convert htExt types to internal type using ThoriumVarTypeToTypeSpec.');
    end;
  else
    raise EThoriumException.CreateFmt('Invalid value for VarType (%d) in ThoriumVarTypeToTypeSpec.', [VarType]);
  end;
end;*)

procedure ThoriumFreeValue(var AValue: TThoriumValue);
begin
  if AValue.RTTI = nil then
    Exit;
  AValue.RTTI.DoFree(AValue);
end;

procedure ThoriumReleaseValue(var AValue: TThoriumValue); inline;
begin
  Dec(AValue.References);
  if AValue.References = 0 then
    ThoriumFreeValue(AValue);
end;

function ThoriumDuplicateValue(const AValue: TThoriumValue): TThoriumValue;
begin
  if AValue.RTTI = nil then
    Result := AValue
  else
    Result := AValue.RTTI.DuplicateValue(AValue);
end;

function ThoriumIncRef(const AValue: TThoriumValue): TThoriumValue; inline;
begin
  Result := AValue;
  Inc(Result.References);
end;

function InitialData(const Int: TThoriumInteger): TThoriumInitialData;
begin
  Result.Int := Int;
end;

procedure AppendTableEntry(var Entries: TThoriumTableEntryResults;
  Match: TThoriumTableEntryResult);
var
  L: Integer;
begin
  L := Length(Entries);
  SetLength(Entries, L+1);
  FillByte(Entries[L], SizeOf(TThoriumTableEntryResult), 0);
  Entries[L] := Match;
end;

{%ENDREGION}

{%REGION 'Thorium exceptions' /fold}

{ EThoriumRuntimeExecutionException }

constructor EThoriumRuntimeExecutionException.Create(Module: TThoriumRuntimeModule;
  InstructionAddr: TThoriumInstructionAddress; Instruction: PThoriumInstruction;
  OriginalException: Exception);
begin
  if Instruction <> nil then
  begin
    if OriginalException <> nil then
    begin
      if Module <> nil then
      begin
        CreateFmt('Instruction at %8.8x ''%s'' raised exception %s (''%s'') in module %s at code line %d.', [InstructionAddr, ThoriumInstructionToStr(Instruction^), OriginalException.ClassName, OriginalException.Message, Module.FName, Instruction^.CodeLine]);
      end
      else
      begin
        CreateFmt('Instruction at %8.8x ''%s'' raised exception %s (''%s'') in unknown module at code line %d.', [InstructionAddr, ThoriumInstructionToStr(Instruction^), OriginalException.ClassName, OriginalException.Message, Instruction^.CodeLine]);
      end;
    end
    else
    begin
      if Module <> nil then
      begin
        CreateFmt('Instruction at %8.8x ''%s'' raised unknown exception in module %s at code line %d.', [InstructionAddr, ThoriumInstructionToStr(Instruction^), Module.FName, Instruction^.CodeLine]);
      end
      else
      begin
        CreateFmt('Instruction at %8.8x ''%s'' raised unknown exception in unknown module at code line %d.', [InstructionAddr, ThoriumInstructionToStr(Instruction^), Instruction^.CodeLine]);
      end;
    end;
  end
  else
  begin
    if OriginalException <> nil then
    begin
      if Module <> nil then
      begin
        CreateFmt('Unknown instruction at %8.8x raised exception %s (''%s'') in module %s at unknown code line.', [InstructionAddr, OriginalException.ClassName, OriginalException.Message, Module.FName]);
      end
      else
      begin
        CreateFmt('Unknown instruction at %8.8x raised exception %s (''%s'') in unknown module at unknown code line.', [InstructionAddr, OriginalException.ClassName, OriginalException.Message]);
      end;
    end
    else
    begin
      if Module <> nil then
      begin
        CreateFmt('Unknown instruction at %8.8x raised unknown exception in module %s at unknown code line.', [InstructionAddr, Module.FName]);
      end
      else
      begin
        CreateFmt('Unknown instruction at %8.8x raised unknown exception in unknown module at unknown code line.', [InstructionAddr]);
      end;
    end;
  end;
end;

destructor EThoriumRuntimeExecutionException.Destroy;
begin
  FreeAndNil(FOriginalException);
  inherited Destroy;
end;

{%ENDREGION}

{%REGION 'Thorium persistent baseclass' /fold}

{ TThoriumReferenceImplementation }

constructor TThoriumReferenceImplementation.Create(ATarget: TObject);
begin
  FTarget := ATarget;
  FReferences := 0;
  FHostControlled := True;
end;

function TThoriumReferenceImplementation._AddRef: LongInt; stdcall;
begin
  Result := InterLockedIncrement(FReferences);
end;

function TThoriumReferenceImplementation._Release: LongInt; stdcall;
begin
  Result := InterLockedDecrement(FReferences);
  if (Result = 0) and (not FHostControlled) then
    Free;
end;

function TThoriumReferenceImplementation.QueryInterface(const IID: TGuid; out
  Obj): LongInt; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := LongInt(E_NOINTERFACE);
end;

procedure TThoriumReferenceImplementation.EnableHostControl;
begin
  FHostControlled := True;
end;

procedure TThoriumReferenceImplementation.DisableHostControl;
begin
  FHostControlled := False;
  if FReferences = 0 then
    Free;
end;

procedure TThoriumReferenceImplementation.FreeReference;
begin
  if (InterLockedDecrement(FReferences) = 0) and not FHostControlled then
    Free;
end;

function TThoriumReferenceImplementation.GetReference: TObject;
begin
  InterLockedIncrement(FReferences);
  Result := FTarget;
end;

{ TThoriumPersistent }

{$ifdef DebugToConsole}
procedure TThoriumPersistent.BeforeDestruction;
begin
  WriteLn(ClassName, ': Free called at ', FReferenceImplementation.FReferences, ' references');
end;
{$endif}

constructor TThoriumPersistent.Create;
begin
  inherited Create;
  FReferenceImplementation := TThoriumReferenceImplementation.Create(Self);
  // These two must not count as references, so we need to decrease the refcount
  // manually ;)
  FReference := FReferenceImplementation;
  FReference2 := FReferenceImplementation;
  FReference2.FreeReference;
  FReference._Release;
end;

destructor TThoriumPersistent.Destroy;
begin
  FReferenceImplementation.Free;
  inherited Destroy;
end;

class procedure TThoriumPersistent.GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
begin

end;

class procedure TThoriumPersistent.GetStaticMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIStaticMethods);
begin

end;

{%ENDREGION}

{%REGION 'Thorium base classes' /fold}

{ TThoriumHashableObject }

constructor TThoriumHashableObject.Create;
begin
  FHashGenerated := False;
  FFrozen := False;
end;

procedure TThoriumHashableObject.ForceFrozen;
begin
  if not FFrozen then
    raise EThoriumException.CreateFmt('%s must be frozen to perform this operation.', [ClassName]);
end;

procedure TThoriumHashableObject.ForceUnfrozen;
begin
  if FFrozen then
    raise EThoriumException.CreateFmt('%s must be unfrozen to perform this operation.', [ClassName]);
end;

procedure TThoriumHashableObject.HardUnfreeze;
begin
  // Hard unfreeze (you know, somewhat like to unfreeze Han Solo with a
  // flamethrower ...)
  FFrozen := False;
end;

procedure TThoriumHashableObject.InvalidateHash;
begin
  FHashGenerated := False;
end;

function TThoriumHashableObject.GetHash: TThoriumHash;
begin
  ForceFrozen;
  if FCalculatingHash then
  begin
    // Recursive hash request.
    FillByte(Result, SizeOf(TThoriumHash), 0);
    Exit;
  end;
  if not FHashGenerated then
  begin
    FCalculatingHash := True;
    CalcHash;
    FCalculatingHash := False;
    FHashGenerated := True;
  end;
  Result := FHash;
end;

function TThoriumHashableObject.GetHashCode: PtrInt;
begin
  raise EThoriumException.Create('GetHashCode is not supported.');
end;

procedure TThoriumHashableObject.Freeze;
begin
  FFrozen := True;
end;

{ TThoriumIntList }

constructor TThoriumIntList.Create;
begin
  inherited Create;
  FList := nil;
  FCapacity := 0;
  FCount := 0;
end;

destructor TThoriumIntList.Destroy;
begin
  if FList <> nil then
    FreeMem(FList, SizeOf(Integer) * FCapacity);
  inherited Destroy;
end;

function TThoriumIntList.GetItem(Index: Integer): Integer;
// Returns the value of a list item at the offset Index.
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [Index]);
  Result := FList[Index];
end;

procedure TThoriumIntList.Expand;
// Expands the list capacity by a special value.
begin
  if FCapacity = 0 then
    SetCapacity(8)
  else if FCapacity <= 256 then
    SetCapacity(FCapacity shl 2)
  else
    SetCapacity(FCapacity + 128);
end;

procedure TThoriumIntList.SetCapacity(NewCapacity: Integer);
// Set the capacity to a given value. This does not allow to decrease the amount
// of items currently stored in the list.
begin
  if (NewCapacity < FCount) or (NewCapacity = FCapacity) then
    Exit;
  ReAllocMem(FList, SizeOf(Integer)*NewCapacity);
  FCapacity := NewCapacity;
end;

procedure TThoriumIntList.SetCount(NewCount: Integer);
// Set the count of the item set to a given value. This does not allow to
// enlarge the list.
begin
  if NewCount >= FCount then
    Exit;
  ReAllocMem(FList, SizeOf(Integer)*NewCount);
  FCount := NewCount;
  FCapacity := FCount;
end;

procedure TThoriumIntList.SetItem(Index: Integer; Value: Integer);
// Sets the value of the item at Index to Value.
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [Index]);
  FList[Index] := Value;
end;

function TThoriumIntList.AddEntry(Value: Integer): Integer;
// Adds a new entry to the list and returns its index.
begin
  if FCapacity < FCount + 1 then
    Expand;
  FList[FCount] := Value;
  Result := FCount;
  Inc(FCount);
end;

function TThoriumIntList.FindValue(AValue: Integer): Integer;
// Searches for a value and returns its index. If it is not found, -1 is
// returned.
var
  I: Integer;
  LocalList: PInteger;
begin
  LocalList := FList;
  for I := 0 to FCount - 1 do
  begin
    if LocalList^ = AValue then
    begin
      Result := I;
      Exit;
    end;
    Inc(LocalList);
  end;
  Result := -1;
end;

procedure TThoriumIntList.DeleteEntry(AIndex: Integer);
// Delete the entry at AIndex.
begin
  if (AIndex >= FCount) or (AIndex < 0) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [AIndex]);
  if (AIndex = FCount - 1) then
  begin
    Dec(FCount);
    Exit;
  end;
  Move(PInteger(ptruint(FList)+SizeOf(Integer)*Cardinal(AIndex+1))^,
    PInteger(ptruint(FList)+SizeOf(Integer)*Cardinal(AIndex))^,
    (FCount - (AIndex + 1))*SizeOf(Integer)
    );
    //(AIndex - (FCount - 1)));
  Dec(FCount);
end;

procedure TThoriumIntList.Clear;
begin
  SetCount(0);
  SetCapacity(FCapacity div 2);
end;

{ TThoriumIntStack }

procedure TThoriumIntStack.Push(Value: Integer);
begin
  AddEntry(Value);
end;

function TThoriumIntStack.Pop: Integer;
begin
  if FCount > 0 then
  begin
    Result := FList[FCount - 1];
    DeleteEntry(FCount-1);
  end
  else
    Result := 0;
end;

{ TThoriumJumpList }

procedure TThoriumJumpList.FillAddresses(DownToCount: Integer; Address: TThoriumInstructionAddress;
  Instructions: TThoriumInstructions);
// Handle the values of the list as addresses of JMP, JMP.F or JMP.T
// instructions in Instructions and set their addresses to Address. This is done
// for all items from the top down to the item with Index DownToCount. These
// items are removed from the list afterwards.
var
  I: Integer;
  LocalList: PInteger;
begin
  LocalList := FList;
  for I := FCount-1 downto DownToCount do
  begin
    TThoriumInstructionJMP(Instructions.Instruction[LocalList^]^).NewAddress := Address;
    Inc(LocalList);
  end;
  Count := DownToCount;
end;

procedure TThoriumJumpList.ChangeAddresses(Offset: Integer; AfterAddress: TThoriumInstructionAddress;
  Instructions: TThoriumInstructions);
var
  I: Integer;
  LocalList: PInteger;
begin
  LocalList := FList;
  for I := 0 to FCount - 1 do
  begin
    if LocalList^ >= AfterAddress then
      LocalList^ := LocalList^ + Offset;
    Inc(LocalList);
  end;
  LocalList := FList;
  for I := 0 to FCount - 1 do
  begin
    if TThoriumInstructionJMP(Instructions.Instruction[LocalList^]^).NewAddress >= AfterAddress then
      TThoriumInstructionJMP(Instructions.Instruction[LocalList^]^).NewAddress := TThoriumInstructionJMP(Instructions.Instruction[LocalList^]^).NewAddress + Offset;
    Inc(LocalList);
  end;
end;

{%ENDREGION}

{%REGION 'Thorium types' /fold}

{ TThoriumType }

constructor TThoriumType.Create(AThorium: TThorium);
begin
//  FName := AName;
  FName := 'lambda:type';
  FReferences := 0;
  FThorium := AThorium;
end;

function TThoriumType._AddRef: longint; stdcall;
begin
  //Result := InterLockedIncrement(FReferences);
  Result := 1;
end;

function TThoriumType._Release: longint; stdcall;
begin
(*  Result := InterLockedDecrement(FReferences);
  if Result = 0 then
    Free;       *)
  Result := 1;
end;

function TThoriumType.GetNoneInitialData(out InitialData: TThoriumInitialData
  ): Boolean;
begin
  InitialData.Int := 0;
  Result := False;
end;

function TThoriumType.GetName: String;
begin
  // Well, THATs quite an ugly hack.
  if Self = nil then
     Exit('void');
  Result := FName;
end;

procedure TThoriumType.RaiseMissingTheObject;
begin
  raise EThoriumException.Create('Missing a valid TheObject parameter.');
end;

function TThoriumType.QueryInterface(const iid: tguid; out obj): longint;
  stdcall;
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TThoriumType.CanAssignTo(
  var Assignment: TThoriumAssignmentDescription; const AnotherType: TThoriumType
  ): Boolean;
begin
  Result := IsEqualTo(AnotherType);
  if Result then
  begin
    Assignment.Cast.Needed := False;
  end;
end;

function TThoriumType.CanCall: Boolean;
var
  Dummy: TThoriumOperationDescription;
begin
  Dummy.Operation := opCall;
  Result := CanPerformOperation(Dummy);
end;

function TThoriumType.CanCreate(const InitialData: TThoriumInitialData;
  const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  Result := False;
end;

function TThoriumType.CanCreateNone(const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
var
  Initial: TThoriumInitialData;
begin
  Result := GetNoneInitialData(Initial);
  if Result then
    Result := CanCreate(Initial, ToRegister, Instruction);
end;

function TThoriumType.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  Result := False;
end;

function TThoriumType.DuplicateValue(const Input: TThoriumValue
  ): TThoriumValue;
begin
  Result := Input;
end;

function TThoriumType.Equals(Obj: TObject): boolean;
begin
  if Obj is TThoriumType then
    Result := IsEqualTo(TThoriumType(Obj))
  else
    Result := False;
end;


function TThoriumType.HasFieldAccess: Boolean;
begin
  Result := False;
end;

function TThoriumType.HasIndexedAccess: Boolean;
begin
  Result := False;
end;

function TThoriumType.NeedsClear: Boolean;
begin
  Result := False;
end;

function TThoriumType.DoClone(const AValue: TThoriumValue): TThoriumValue;
begin
  Result := AValue;
  Result.References := 1;
end;

function TThoriumType.DoCmpGreaterOrEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := DoCmpEqual(AValue, BValue) or DoCmpGreater(AValue, BValue);
end;

function TThoriumType.DoCmpLessOrEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := DoCmpEqual(AValue, BValue) or DoCmpLess(AValue, BValue);
end;

procedure TThoriumType.DoEnableHostControl(const AValue: TThoriumValue);
begin

end;

function TThoriumType.DoToString(const AValue: TThoriumValue): String;
begin
  Result := '(unsupported type: '+GetName+')';
end;

class function TThoriumType.PerformOperation(const AValue: TThoriumValue;
  const Operation: TThoriumOperationDescription; const BValue: PThoriumValue
  ): TThoriumValue;
var
  A, B: TThoriumValue;
begin
  if Operation.Casts[0].Needed then
    A := AValue.RTTI.DoCast(AValue, Operation.Casts[0].TargetType)
  else
    A := AValue;
  if BValue <> nil then
  begin
    if Operation.Casts[1].Needed then
      B := BValue^.RTTI.DoCast(BValue^, Operation.Casts[1].TargetType)
    else
      B := BValue^;
  end;
  case Operation.Operation of
    opIncrement:
    begin
      Result := A;
      AValue.RTTI.DoIncrement(Result);
    end;
    opDecrement:
    begin
      Result := B;
      AValue.RTTI.DoDecrement(Result);
    end;
    (*opCmpEqual: Result := DoCmpEqual(A, B);
    opCmpNotEqual: Result := DoCmpNotEqual(A, B);
    opCmpGreater: Result := DoCmpGreater(A, B);
    opCmpGreaterOrEqual: Result := DoCmpGreaterOrEqual(A, B);
    opCmpLess: Result := DoCmpLess(A, B);
    opCmpLessOrEqual: Result := DoCmpLessOrEqual(A, B);*)

    opAddition: Result := AValue.RTTI.DoAddition(A, B);
    opSubtraction: Result := AValue.RTTI.DoSubtraction(A, B);
    opMultiplication: Result := AValue.RTTI.DoMultiplication(A, B);
    opDivision: Result := AValue.RTTI.DoDivision(A, B);
    opIntegerDivision: Result := AValue.RTTI.DoIntegerDivision(A, B);
    opModulus: Result := AValue.RTTI.DoModulus(A, B);

    opBitAnd: Result := AValue.RTTI.DoBitAnd(A, B);
    opBitOr: Result := AValue.RTTI.DoBitOr(A, B);
    opBitXor: Result := AValue.RTTI.DoBitXor(A, B);
    opBitShr: Result := AValue.RTTI.DoBitShr(A, B);
    opBitShl: Result := AValue.RTTI.DoBitShl(A, B);
    opBitNot:
    begin
      Result := A;
      AValue.RTTI.DoBitNot(Result);
    end;

    opLen: Result := AValue.RTTI.FThorium.FTypeInteger.CreateFromInt(AValue.RTTI.DoGetLength(A));
    opString: Result := AValue.RTTI.FThorium.FTypeString.CreateFromString(AValue.RTTI.DoToString(A));
    opClone: Result := AValue.RTTI.DoClone(A);

    opLogicalAnd: Result := AValue.RTTI.DoLogicalAnd(A, B);
    opLogicalOr: Result := AValue.RTTI.DoLogicalOr(A, B);
    opLogicalXor: Result := AValue.RTTI.DoLogicalXor(A, B);
    opLogicalNot:
    begin
      Result := A;
      AValue.RTTI.DoLogicalNot(Result);
    end;
  else
    raise EThoriumRuntimeException.CreateFmt('Invalid generic operation: %s.', [GetEnumName(TypeInfo(TThoriumOperation), Ord(Operation.Operation))]);
  end;
end;

class function TThoriumType.PerformCmpOperation(const AValue: TThoriumValue;
  const Operation: TThoriumOperationDescription; const BValue: PThoriumValue
  ): Boolean;
var
  A, B: TThoriumValue;
begin
  if Operation.Casts[0].Needed then
    A := AValue.RTTI.DoCast(AValue, Operation.Casts[0].TargetType)
  else
    A := AValue;
  if BValue <> nil then
  begin
    if Operation.Casts[1].Needed then
      B := BValue^.RTTI.DoCast(BValue^, Operation.Casts[1].TargetType)
    else
      B := BValue^;
  end;
  case Operation.Operation of
    opCmpEqual: Result := AValue.RTTI.DoCmpEqual(A, B);
    opCmpNotEqual: Result := AValue.RTTI.DoCmpNotEqual(A, B);
    opCmpGreater: Result := AValue.RTTI.DoCmpGreater(A, B);
    opCmpGreaterOrEqual: Result := AValue.RTTI.DoCmpGreaterOrEqual(A, B);
    opCmpLess: Result := AValue.RTTI.DoCmpLess(A, B);
    opCmpLessOrEqual: Result := AValue.RTTI.DoCmpLessOrEqual(A, B);
    opEvaluate: Result := AValue.RTTI.DoEvaluate(AValue);
  else
    raise EThoriumRuntimeException.CreateFmt('Invalid comparision operation: %s.', [GetEnumName(TypeInfo(TThoriumOperation), Ord(Operation.Operation))]);
  end;
end;

function TThoriumType.ToString: ansistring;
begin
  Result := 'ThoriumType '''+GetName+'''';
end;

function TThoriumType.UsesType(const AnotherType: TThoriumType;
  MayRecurse: Boolean): Boolean;
begin
  Result := False;
end;

{ TThoriumTypeSimple }

function TThoriumTypeSimple.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkSimple;
end;

procedure TThoriumTypeSimple.DoFree(var AValue: TThoriumValue);
begin
  AValue.RTTI := nil;
end;

function TThoriumTypeSimple.IsEqualTo(const AnotherType: TThoriumType): Boolean;
begin
  Result := AnotherType.ClassType = ClassType;
end;

{ TThoriumTypeInteger }

function TThoriumTypeInteger.GetNoneInitialData(out
  InitialData: TThoriumInitialData): Boolean;
begin
  Result := True;
  InitialData.Int := 0;
end;

function TThoriumTypeInteger.CreateFromInt(const Int: TThoriumInteger
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := Int;
end;

function TThoriumTypeInteger.CanAssignTo(
  var Assignment: TThoriumAssignmentDescription; const AnotherType: TThoriumType
  ): Boolean;
begin
  if AnotherType is TThoriumTypeFloat then
  begin
    if not Assignment.Casting then
      Result := inherited;
    Assignment.Cast.Needed := True;
    Assignment.Cast.Instruction := TThoriumInstructionCAST(castif(0, 0));
    Assignment.Cast.TargetType := AnotherType;
  end
  else
    Result := inherited;
end;

function TThoriumTypeInteger.CanCreate(const InitialData: TThoriumInitialData;
  const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  if ToRegister then
    Instruction := CreationDescription(int(InitialData.Int, 0), 4)
  else
    Instruction := CreationDescription(int_s(InitialData.Int));
  Result := True;
end;

function TThoriumTypeInteger.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;

  function IntIntOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeInteger then
    begin
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
      Result := True;
    end
    else
      Result := False;
  end;

  function IntFltOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeFloat then
    begin
      Operation.ResultType := TheObject;
      Operation.Casts[0].Needed := True;
      Operation.Casts[0].Instruction := TThoriumInstructionCAST(castif(0, 0));
      Operation.Casts[0].TargetType := Operation.ResultType;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
      Result := True;
    end
    else
      Result := False;
  end;

  procedure IntOp(Instruction: TThoriumInstruction);
  begin
    Operation.ResultType := Self;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    Operation.OperationInstruction := OperationInstructionDescription(Instruction, -1, -1, 0);
  end;

begin
  Result := True;
  if Operation.Operation in opReflexive then
  begin
    Operation.ResultType := nil;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    case Operation.Operation of
      opIncrement:
        IntOp(inci(0));
      opDecrement:
        IntOp(deci(0));
      opBitNot:
        IntOp(_not(0));
      opNegate:
        IntOp(negi(0));
      opToNative:
      begin
        Assert(ExType <> nil);
        if ExType^.Kind in [tkInteger,tkChar,tkEnumeration,tkWChar,tkSet,tkInt64,tkQWord] then
          Operation.OperationInstruction := OperationInstructionDescription(x2n(0, ExType), 0, -1, -1)
        else
          Exit(False);
        Exit;
      end;
      opFromNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(n2x(0), 0, -1, -1);
      end;
      opFreeNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(clrn(0), 0, -1, -1);
      end;
      opEvaluate:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(evali(0), 0, -1, -1);
        Exit;
      end;
    else
      Result := inherited;
    end;
    Exit;
  end
  else if TheObject = nil then
    RaiseMissingTheObject
  else
  begin
    case Operation.Operation of
      opAddition:
      begin
        if IntIntOp(addi(0, 0, 0)) then
          Exit
        else if IntFltOp(addf(0, 0, 0)) then
          Exit;
      end;

      opSubtraction:
      begin
        if IntIntOp(subi(0, 0, 0)) then
          Exit
        else if IntFltOp(subf(0, 0, 0)) then
          Exit;
      end;

      opMultiplication:
      begin
        if IntIntOp(muli(0, 0, 0)) then
          Exit
        else if IntFltOp(mulf(0, 0, 0)) then
          Exit;
      end;

      opDivision:
      begin
        if TheObject is TThoriumTypeInteger then
        begin
          Operation.ResultType := FThorium.FTypeFloat;
          Operation.Casts[0].Needed := True;
          Operation.Casts[0].Instruction := TThoriumInstructionCAST(castif(0, 0));
          Operation.Casts[0].TargetType := Operation.ResultType;
          Operation.Casts[1].Needed := True;
          Operation.Casts[1].Instruction := TThoriumInstructionCAST(castif(0, 0));
          Operation.Casts[1].TargetType := Operation.ResultType;
          Operation.OperationInstruction := OperationInstructionDescription(divf(0, 0, 0));
          Exit;
        end
        else if IntFltOp(divf(0, 0, 0)) then
          Exit;
      end;

      opIntegerDivision:
        if IntIntOp(divi(0, 0, 0)) then
          Exit;

      opModulus:
        if IntIntOp(_mod(0, 0, 0)) then
          Exit;

      opBitOr:
        if IntIntOp(_or(0, 0, 0)) then
          Exit;

      opBitXor:
        if IntIntOp(_xor(0, 0, 0)) then
          Exit;

      opBitAnd:
        if IntIntOp(_and(0, 0, 0)) then
          Exit;

      opBitShl:
        if IntIntOp(_shl(0, 0, 0)) then
          Exit;

      opBitShr:
        if IntIntOp(_shr(0, 0, 0)) then
          Exit;

      opCmpLessOrEqual, opCmpGreaterOrEqual, opCmpGreater, opCmpLess,
      opCmpNotEqual, opCmpEqual:
      begin
        Operation.ResultType := nil;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        if TheObject is TThoriumTypeInteger then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpi(0, 0), 0, 1, -1);
          Exit;
        end
        else if TheObject is TThoriumTypeFloat then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpif(0, 0), 0, 1, -1);
          Exit;
        end;
      end;

      opString:
      begin
        Operation.ResultType := FThorium.FTypeString;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(tostr_i(0, 0), 0, -1, 1);
        Exit(True);
      end;
    end;
  end;
  Result := inherited;
end;

function TThoriumTypeInteger.CreateValueFromPtr(const Ptr: Pointer
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := PThoriumInteger(Ptr)^;
end;

function TThoriumTypeInteger.DoAddition(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int + BValue.Int;
end;

function TThoriumTypeInteger.DoBitAnd(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int and BValue.Int;
end;

function TThoriumTypeInteger.DoBitOr(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int or BValue.Int;
end;

function TThoriumTypeInteger.DoBitXor(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int xor BValue.Int;
end;

function TThoriumTypeInteger.DoBitShr(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int shr BValue.Int;
end;

function TThoriumTypeInteger.DoBitShl(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int shl BValue.Int;
end;

function TThoriumTypeInteger.DoBitNot(const AValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := not AValue.Int;
end;

function TThoriumTypeInteger.DoCast(const AValue: TThoriumValue;
  const TargetType: TThoriumType): TThoriumValue;
begin
  if TargetType is TThoriumTypeFloat then
  begin
    Result.RTTI := TargetType;
    Result.Float := AValue.Int;
  end
  else
    raise EThoriumRuntimeException.CreateFmt('Cannot cast %s to %s.', [Name, TargetType.Name]);
end;

function TThoriumTypeInteger.DoCmpEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int = BValue.Float
  else
    Result := AValue.Int = BValue.Int;
end;

function TThoriumTypeInteger.DoCmpGreater(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int > BValue.Float
  else
    Result := AValue.Int > BValue.Int;
end;

function TThoriumTypeInteger.DoCmpGreaterOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int >= BValue.Float
  else
    Result := AValue.Int >= BValue.Int;
end;

function TThoriumTypeInteger.DoCmpLess(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int < BValue.Float
  else
    Result := AValue.Int < BValue.Int;
end;

function TThoriumTypeInteger.DoCmpLessOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int <= BValue.Float
  else
    Result := AValue.Int <= BValue.Int;
end;

function TThoriumTypeInteger.DoCmpNotEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int <> BValue.Float
  else
    Result := AValue.Int <> BValue.Int;
end;

procedure TThoriumTypeInteger.DoDecrement(var ASubject: TThoriumValue);
begin
  ASubject.Int -= 1;
end;

function TThoriumTypeInteger.DoDivision(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := FThorium.FTypeFloat;
  Result.Float := AValue.Int / BValue.Int;
end;

function TThoriumTypeInteger.DoEvaluate(const AValue: TThoriumValue): Boolean;
begin
  Result := AValue.Int <> 0;
end;

procedure TThoriumTypeInteger.DoFreeNative(var AValue: TThoriumValue);
begin
  AValue.NativeData.Data := nil;
end;

procedure TThoriumTypeInteger.DoFromNative(var AValue: TThoriumValue);
begin
  AValue.NativeData.Data := nil;
end;

function TThoriumTypeInteger.DoCreate(const InitialData: TThoriumInitialData
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := InitialData.Int;
end;

procedure TThoriumTypeInteger.DoIncrement(var ASubject: TThoriumValue);
begin
  ASubject.Int += 1;
end;

function TThoriumTypeInteger.DoIntegerDivision(const AValue,
  BValue: TThoriumValue): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int div BValue.Int;
end;

function TThoriumTypeInteger.DoModulus(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int mod BValue.Int;
end;

function TThoriumTypeInteger.DoMultiplication(const AValue,
  BValue: TThoriumValue): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int * BValue.Int;
end;

procedure TThoriumTypeInteger.DoNegate(var AValue: TThoriumValue);
begin
  AValue.Int := -AValue.Int;
end;

function TThoriumTypeInteger.DoSubtraction(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int - BValue.Int;
end;

procedure TThoriumTypeInteger.DoToNative(var AValue: TThoriumValue;
  const AType: PTypeInfo);
begin
  AValue.NativeData.ForType := nil;
  AValue.NativeData.Data := @AValue.Int;
end;

function TThoriumTypeInteger.DoToString(const AValue: TThoriumValue): String;
begin
  Result := IntToStr(AValue.Int);
end;

{ TThoriumTypeFloat }

function TThoriumTypeFloat.GetNoneInitialData(out
  InitialData: TThoriumInitialData): Boolean;
begin
  Result := True;
  InitialData.Flt := 0.0;
end;

function TThoriumTypeFloat.CanCreate(const InitialData: TThoriumInitialData;
  const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  if ToRegister then
    Instruction := CreationDescription(flt(InitialData.Flt, 0), 4)
  else
    Instruction := CreationDescription(flt_s(InitialData.Flt));
  Result := True;
end;

function TThoriumTypeFloat.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;

  function FltFltOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeFloat then
    begin
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
    end
    else
      Result := False;
  end;

  function FltIntOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeInteger then
    begin
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := True;
      Operation.Casts[1].Instruction := TThoriumInstructionCAST(castif(0, 0));
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
    end
    else
      Result := False;
  end;

  procedure FltOp(Instruction: TThoriumInstruction);
  begin
    Operation.ResultType := Self;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    Operation.OperationInstruction := OperationInstructionDescription(Instruction, 0, -1, -1);
  end;

begin
  Result := True;
  if Operation.Operation in opReflexive then
  begin
    Operation.ResultType := nil;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    case Operation.Operation of
      opNegate:
        FltOp(negf(0));
      opIncrement:
        FltOp(incf(0));
      opDecrement:
        FltOp(decf(0));
      opToNative:
      begin
        if ExType^.Kind in [tkFloat] then
          Operation.OperationInstruction := OperationInstructionDescription(x2n(0, ExType), 0, -1, -1)
        else
          Exit(False);
        Exit;
      end;
      opFromNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(n2x(0), 0, -1, -1);
      end;
      opFreeNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(clrn(0), 0, -1, -1);
      end;
    else
      Result := inherited;
    end;
    Exit;
  end
  else if TheObject = nil then
    RaiseMissingTheObject
  else
  begin
    case Operation.Operation of
      opAddition:
        if FltFltOp(addf(0, 0, 0)) then
          Exit
        else if FltIntOp(addf(0, 0, 0)) then
          Exit;
      opSubtraction:
        if FltFltOp(subf(0, 0, 0)) then
          Exit
        else if FltIntOp(subf(0, 0, 0)) then
          Exit;
      opMultiplication:
        if FltFltOp(mulf(0, 0, 0)) then
          Exit
        else if FltIntOp(mulf(0, 0, 0)) then
          Exit;
      opDivision:
        if FltFltOp(divf(0, 0, 0)) then
          Exit
        else if FltIntOp(divf(0, 0, 0)) then
          Exit;

      opCmpLessOrEqual, opCmpGreaterOrEqual, opCmpGreater, opCmpLess,
      opCmpNotEqual, opCmpEqual:
      begin
        Operation.ResultType := nil;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        if TheObject is TThoriumTypeInteger then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpfi(0, 0), 0, 1, -1);
          Exit;
        end
        else if TheObject is TThoriumTypeFloat then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpf(0, 0), 0, 1, -1);
          Exit;
        end;
      end;
    end;
  end;
  Result := inherited;
end;

function TThoriumTypeFloat.CreateValueFromPtr(const Ptr: Pointer
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := PThoriumFloat(Ptr)^;
end;

function TThoriumTypeFloat.DoAddition(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float + BValue.Float;
end;

function TThoriumTypeFloat.DoCmpEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float = BValue.Int
  else
    Result := AValue.Float = BValue.Float;
end;

function TThoriumTypeFloat.DoCmpGreater(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float > BValue.Int
  else
    Result := AValue.Float > BValue.Float;
end;

function TThoriumTypeFloat.DoCmpGreaterOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float >= BValue.Int
  else
    Result := AValue.Float >= BValue.Float;
end;

function TThoriumTypeFloat.DoCmpLess(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float < BValue.Int
  else
    Result := AValue.Float < BValue.Float;
end;

function TThoriumTypeFloat.DoCmpLessOrEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float <= BValue.Int
  else
    Result := AValue.Float <= BValue.Float;
end;

function TThoriumTypeFloat.DoCmpNotEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float <> BValue.Int
  else
    Result := AValue.Float <> BValue.Float;
end;

function TThoriumTypeFloat.DoCreate(const InitialData: TThoriumInitialData
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := InitialData.Int;
end;

procedure TThoriumTypeFloat.DoDecrement(var ASubject: TThoriumValue);
begin
  ASubject.Float -= 1;
end;

function TThoriumTypeFloat.DoDivision(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float / BValue.Float;
end;

procedure TThoriumTypeFloat.DoFreeNative(var AValue: TThoriumValue);
begin
  if AValue.NativeData.ForType <> nil then
    FreeMem(AValue.NativeData.Data);
  AValue.NativeData.Data := nil;
end;

procedure TThoriumTypeFloat.DoFromNative(var AValue: TThoriumValue);
begin
  if AValue.NativeData.ForType = nil then
  begin
    AValue.NativeData.Data := nil;
    Exit;
  end;
  case GetTypeData(AValue.NativeData.ForType)^.FloatType of
    ftSingle:
    begin
      AValue.Float := PSingle(AValue.NativeData.Data)^;
    end;
    ftExtended:
    begin
      AValue.Float := PExtended(AValue.NativeData.Data)^;
    end;
    ftComp:
    begin
      AValue.Float := PComp(AValue.NativeData.Data)^;
    end;
    ftCurr:
    begin
      AValue.Float := PCurrency(AValue.NativeData.Data)^;
    end;
  end;
  FreeMem(AValue.NativeData.Data);
  AValue.NativeData.Data := nil;
  AValue.NativeData.ForType := nil;
end;

procedure TThoriumTypeFloat.DoIncrement(var ASubject: TThoriumValue);
begin
  ASubject.Float += 1;
end;

procedure TThoriumTypeFloat.DoNegate(var AValue: TThoriumValue);
begin
  AValue.Float := -AValue.Float;
end;

function TThoriumTypeFloat.DoMultiplication(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float * BValue.Float;
end;

function TThoriumTypeFloat.DoSubtraction(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float - BValue.Float;
end;

procedure TThoriumTypeFloat.DoToNative(var AValue: TThoriumValue;
  const AType: PTypeInfo);
begin
  AValue.NativeData.ForType := AType;
  case GetTypeData(AType)^.FloatType of
    ftSingle:
    begin
      AValue.NativeData.Data := GetMem(SizeOf(Single));
      PSingle(AValue.NativeData.Data)^ := AValue.Float;
    end;
    ftDouble:
    begin
      AValue.NativeData.ForType := nil;
      AValue.NativeData.Data := @AValue.Float;
    end;
    ftExtended:
    begin
      AValue.NativeData.Data := GetMem(SizeOf(Extended));
      PExtended(AValue.NativeData.Data)^ := AValue.Float;
    end;
    ftComp:
    begin
      AValue.NativeData.Data := GetMem(SizeOf(Comp));
      PComp(AValue.NativeData.Data)^ := AValue.Float;
    end;
    ftCurr:
    begin
      AValue.NativeData.Data := GetMem(SizeOf(Currency));
      PCurrency(AValue.NativeData.Data)^ := AValue.Float;
    end;
  end;
end;

function TThoriumTypeFloat.DoToString(const AValue: TThoriumValue): String;
begin
  Result := FloatToStr(AValue.Float, THORIUM_NUMBER_FORMAT);
end;

{ TThoriumTypeString }

function TThoriumTypeString.GetNoneInitialData(out
  InitialData: TThoriumInitialData): Boolean;
begin
  Result := True;
  InitialData.Int := -1;
end;

function TThoriumTypeString.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkString;
end;

function TThoriumTypeString.CreateFromString(const Str: String): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := Str;
end;

function TThoriumTypeString.CanCreate(const InitialData: TThoriumInitialData;
  const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  if ToRegister then
  begin
    if InitialData.Int < 0 then
      Instruction := CreationDescription(str(0), 0)
    else
      Instruction := CreationDescription(strl(InitialData.Int, 0), 2);
  end
  else
  begin
    if InitialData.Int < 0 then
      Instruction := CreationDescription(str_s())
    else
      Instruction := CreationDescription(strl_s(InitialData.Int));
  end;
  Result := True;
end;

function TThoriumTypeString.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;

  function StrStrOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeString then
    begin
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
    end
    else
      Result := False;
  end;

  function StrIndexOp(Instruction: TThoriumInstruction; AOp1, AOp2, ATarget: Integer): Boolean;
  begin
    if TheObject is TThoriumTypeInteger then
    begin
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction, AOp1, AOp2, ATarget);
    end
    else
      Result := False;
  end;

begin
  Result := True;
  if Operation.Operation in opReflexive then
  begin
    Operation.ResultType := nil;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    case Operation.Operation of
      opString:
      begin
        NewNonCastOperation(Operation);
        Operation.ResultType := Self;
        Operation.OperationInstruction := OperationInstructionDescription(copyr(0, 0), 0, -1, 1);
        Exit(True);
      end;
      opLen:
      begin
        NewNonCastOperation(Operation);
        Operation.ResultType := FThorium.FTypeInteger;
        Operation.OperationInstruction := OperationInstructionDescription(len_s(0, 0), 0, -1, 1);
        Exit(True);
      end;
      opToNative:
      begin
        if ExType^.Kind in [tkAString] then
          Operation.OperationInstruction := OperationInstructionDescription(x2n(0, ExType), 0, -1, -1)
        else
          Exit(False);
        Exit;
      end;
      opFromNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(n2x(0), 0, -1, -1);
      end;
      opFreeNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(clrn(0), 0, -1, -1);
      end;
    else
      Exit(inherited CanPerformOperation(Operation, TheObject, ExName));
    end;
  end
  else if TheObject = nil then
    RaiseMissingTheObject
  else
  begin
    case Operation.Operation of
      opAddition:
        if StrStrOp(adds(0, 0, 0)) then
          Exit;
      opIndexedRead:
        if StrIndexOp(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0), -1, -1, -1) then
          Exit;

      opCmpEqual, opCmpGreater, opCmpGreaterOrEqual, opCmpLess,
      opCmpLessOrEqual, opCmpNotEqual:
      begin
        if (TheObject is TThoriumTypeString) then
        begin
          Operation.ResultType := nil;
          Operation.Casts[0].Needed := False;
          Operation.Casts[1].Needed := False;
          Operation.OperationInstruction := OperationInstructionDescription(cmps(0, 0), 0, 1, -1);
          Exit;
        end;
      end;
    end;
  end;
  Result := inherited;
end;

function TThoriumTypeString.CreateValueFromPtr(const Ptr: Pointer
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := PThoriumString(Ptr)^;
end;

function TThoriumTypeString.HasIndexedAccess: Boolean;
begin
  Result := True;
end;

function TThoriumTypeString.HasFieldAccess: Boolean;
begin
  Result := True;
end;

function TThoriumTypeString.NeedsClear: Boolean;
begin
  Result := True;
end;

function TThoriumTypeString.DoAddition(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := AValue.Str^ + BValue.Str^;
end;

function TThoriumTypeString.DoCmpEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ = BValue.Str^;
end;

function TThoriumTypeString.DoCmpGreater(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ > BValue.Str^;
end;

function TThoriumTypeString.DoCmpGreaterOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  Result := AValue.Str^ >= BValue.Str^;
end;

function TThoriumTypeString.DoCmpLess(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ < BValue.Str^;
end;

function TThoriumTypeString.DoCmpLessOrEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ <= BValue.Str^;
end;

function TThoriumTypeString.DoCmpNotEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ <> BValue.Str^;
end;

procedure TThoriumTypeString.DoFree(var AValue: TThoriumValue);
begin
  Dispose(AValue.Str);
  AValue.RTTI := nil;
end;

procedure TThoriumTypeString.DoFreeNative(var AValue: TThoriumValue);
begin

end;

procedure TThoriumTypeString.DoFromNative(var AValue: TThoriumValue);
begin

end;

function TThoriumTypeString.DoGetField(const AValue: TThoriumValue;
  const AFieldID: QWord): TThoriumValue;
begin
  {$ifdef DebugToConsole}
  case AFieldID of
    0:
    begin
      Result.RTTI := FThorium.FTypeInteger;
      Result.Int := Length(AValue.Str^);
    end;
  end;
  {$else}
  case AFieldID of
    0: Result.Int := Length(AValue.Str^);
  end;
  {$endif}
end;

function TThoriumTypeString.DoGetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := AValue.Str^[AIndex.Int];
end;

procedure TThoriumTypeString.DoToNative(var AValue: TThoriumValue;
  const AType: PTypeInfo);
begin
  AValue.NativeData.Data := AValue.Str;
end;

function TThoriumTypeString.DoToString(const AValue: TThoriumValue): String;
begin
  Result:=AValue.Str^;
end;

{ TThoriumTypeFunction }

constructor TThoriumTypeFunction.Create;
begin
  FParameters := TThoriumParameters.Create;
  FReturnType := nil;
end;

destructor TThoriumTypeFunction.Destroy;
begin
  FReturnType := nil;
  FParameters.Free;
  inherited Destroy;
end;

function TThoriumTypeFunction.GetHasReturnValue: Boolean;
begin
  Result := FReturnType <> nil;
end;

function TThoriumTypeFunction.GetHasReturnValueInt: Integer;
begin
  if HasReturnValue then
    Result := 1
  else
    Result := 0;
end;

procedure TThoriumTypeFunction.Assign(ASource: TThoriumTypeFunction);
begin
  FParameters.Assign(ASource.FParameters);
  FReturnType := ASource.ReturnType;
end;

function TThoriumTypeFunction.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkFunction;
end;

function TThoriumTypeFunction.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  if Operation.Operation = opCall then
  begin
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    Operation.OperationInstruction := OperationInstructionDescription(call(0, 0), 1, -1, 0);
    Operation.ResultType := FReturnType;
    Result := True;
  end
  else
    Result := inherited;
end;

function TThoriumTypeFunction.GetParameters: TThoriumParameters;
begin
  Result := FParameters;
end;

function TThoriumTypeFunction.GetReturnType: TThoriumType;
begin
  Result := FReturnType;
end;

function TThoriumTypeFunction.IsEqualTo(const AnotherType: TThoriumType
  ): Boolean;
begin
  raise EThoriumException.Create('TThoriumTypeFunction.IsEqualTo not implemented yet.');
end;

{ TThoriumTypeHostFunction }

constructor TThoriumTypeHostFunction.Create(const AThorium: TThorium;
  const AHostFunction: TThoriumHostCallableBase);
var
  I: Integer;
  TypeSpec: TThoriumType;
  Op: TThoriumOperationDescription;
begin
  inherited Create(AThorium);
  FHostFunction := AHostFunction;
  FParameters := TThoriumParameters.Create;
  for I := 0 to AHostFunction.FParameters.Count - 1 do
  begin
    TypeSpec := AHostFunction.FLibrary.DeepFindTypeForHostType(AHostFunction.FParameters.GetParameter(I));
    if TypeSpec = nil then
      raise EThoriumException.CreateFmt('Cannot find respective thorium type for %s.', [AHostFunction.FParameters.GetParameter(I)^.Name]);
    if AHostFunction.NeedsNativeData then
    begin;
      Op.Operation := opToNative;
      if not TypeSpec.CanPerformOperation(Op, nil, '', AHostFunction.FParameters.Parameters[I]) then
        raise EThoriumException.CreateFmt('''%s'' cannot be passed to this native call function (opToNative not supported for ''%s'').', [TypeSpec.Name, AHostFunction.FParameters.GetParameter(I)^.Name]);
      Op.Operation := opFromNative;
      if not TypeSpec.CanPerformOperation(Op) then
        raise EThoriumException.CreateFmt('''%s'' cannot be passed to a native call function (opFromNative not supported).', [TypeSpec.Name]);
    end;
    FParameters.Add(TypeSpec);
  end;
  if AHostFunction.ReturnType.HostType <> nil then
  begin
    TypeSpec := AHostFunction.FLibrary.DeepFindTypeForHostType(AHostFunction.FReturnType.HostType);
    if TypeSpec = nil then
      raise EThoriumException.CreateFmt('Cannot find respective thorium type for %s.', [AHostFunction.ReturnType.HostType^.Name]);
    if AHostFunction.NeedsNativeData then
    begin;
      Op.Operation := opToNative;
      if not TypeSpec.CanPerformOperation(Op, nil, '', AHostFunction.ReturnType.HostType) then
        raise EThoriumException.CreateFmt('''%s'' cannot be return type of this native call function (opToNative not supported for ''%s'').', [TypeSpec.Name, AHostFunction.ReturnType.HostType^.Name]);
      Op.Operation := opFromNative;
      if not TypeSpec.CanPerformOperation(Op) then
        raise EThoriumException.CreateFmt('''%s'' cannot be return type of a native call function (opFromNative not supported).', [TypeSpec.Name]);
    end;
    FReturnType := TypeSpec;
  end
  else
    FReturnType := nil;
end;

destructor TThoriumTypeHostFunction.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

function TThoriumTypeHostFunction.GetHasReturnValue: Boolean;
begin
  Result := FReturnType <> nil;
end;

function TThoriumTypeHostFunction.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkHostFunction;
end;

function TThoriumTypeHostFunction.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  Result := False;
end;

function TThoriumTypeHostFunction.GetParameters: TThoriumParameters;
begin
  Result := FParameters;
end;

function TThoriumTypeHostFunction.GetHostParameters: TThoriumHostParameters;
begin
  Result := HostFunction.Parameters;
end;

function TThoriumTypeHostFunction.GetReturnType: TThoriumType;
begin
  Result := FReturnType;
end;

function TThoriumTypeHostFunction.GetHostReturnType: TThoriumHostType;
begin
  Result := HostFunction.ReturnType;
end;

function TThoriumTypeHostFunction.IsEqualTo(const AnotherType: TThoriumType
  ): Boolean;
begin
  raise EThoriumException.Create('TThoriumTypeHostFunction.IsEqualTo not implemented yet.');
end;

function TThoriumTypeHostFunction.NeedsNativeData: Boolean;
begin
  Exit(FHostFunction.NeedsNativeData);
end;

{ TThoriumTypeHostType }

constructor TThoriumTypeHostType.Create(const AThorium: TThorium;
  const AHostType: TThoriumHostObjectType);
begin
  inherited Create(AThorium);
  FHostType := AHostType;
end;

function TThoriumTypeHostType.GetTypeKind: TThoriumTypeKind;
begin
  Result:=tkHostType;
end;

function TThoriumTypeHostType.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  raise EThoriumException.Create('Not implemented yet.');
end;

function TThoriumTypeHostType.IsEqualTo(const AnotherType: TThoriumType
  ): Boolean;
begin
  raise EThoriumException.Create('Not implemented yet.');
end;

{ TThoriumTypeStruct }

constructor TThoriumTypeStruct.Create(AThorium: TThorium);
begin
  inherited Create(AThorium);
  FCount := 0;
end;

destructor TThoriumTypeStruct.Destroy;
begin
  inherited Destroy;
end;

procedure TThoriumTypeStruct.Expand;
begin
  SetLength(FFields, Length(FFields) + 8);
end;

function TThoriumTypeStruct.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkStruct;
end;

function TThoriumTypeStruct.Add(const AName: String; const ValueType: TThoriumType): Integer;
begin
  if ValueType.UsesType(Self) then
    raise EThoriumException.Create('Cannot cross-refer types in structs.');
  if FCount = Length(FFields) then
    Expand;
  FFields[FCount].Name := AName;
  FFields[FCount].ValueType := ValueType;
  FFields[FCount].Offset := SizeOf(TThoriumValue) * FCount;
  Result := FCount;
  Inc(FCount);
end;

function TThoriumTypeStruct.Add(const AReference: TThoriumStructFieldDefinition
  ): Integer;
begin
  Result := Add(AReference.Name, AReference.ValueType);
end;

function TThoriumTypeStruct.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  Result := inherited CanPerformOperation(Operation, TheObject);
end;

procedure TThoriumTypeStruct.Delete(const AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('List index (%d) out of bounds (0…%d)', [AIndex, 0, FCount - 1]);
  FFields[AIndex].Name := '';
  FFields[AIndex].ValueType := nil;
  for I := AIndex + 1 to FCount - 1 do
  begin
    FFields[I-1].Name := FFields[I].Name;
    FFields[I-1].ValueType := FFields[I].ValueType;
  end;
  FFields[FCount-1].Name := '';
  FFields[FCount-1].ValueType := nil;
  Dec(FCount);
end;

function TThoriumTypeStruct.IndexOf(const AName: String): Integer;
begin
  for Result := 0 to FCount - 1 do
    if FFields[Result].Name = AName then
      Exit;
  Result := -1;
end;

function TThoriumTypeStruct.IsEqualTo(const AnotherType: TThoriumType): Boolean;
var
  OtherInstance: TThoriumTypeStruct;
  I: Integer;
begin
  if not (AnotherType is TThoriumTypeStruct) then
    Exit(False);
  OtherInstance := TThoriumTypeStruct(AnotherType);
  if OtherInstance.FCount <> FCount then
    Exit(False);
  for I := 0 to FCount - 1 do
  begin
    if OtherInstance.FFields[I].Name <> Self.FFields[I].Name then
      Exit(False);
    if OtherInstance.FFields[I].ValueType <> Self.FFields[I].ValueType then
      Exit(False);
    if OtherInstance.FFields[I].Offset <> Self.FFields[I].Offset then
      Exit(False);
  end;
  Result := True;
end;

function TThoriumTypeStruct.NeedsClear: Boolean;
begin
  Result := True;
end;

function TThoriumTypeStruct.UsesType(const AnotherType: TThoriumType;
  MayRecurse: Boolean): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if FFields[I].ValueType.IsEqualTo(AnotherType) then
      Exit(True)
    else if MayRecurse and FFields[I].ValueType.UsesType(AnotherType) then
      Exit(True);
  end;
  Result := False;
end;

{ TThoriumTypeArray }

constructor TThoriumTypeArray.Create(const AThorium: TThorium;
  const AValueType: TThoriumType);
begin
  inherited Create(AThorium);
  FValueType := AValueType;
end;

function TThoriumTypeArray.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkArray;
end;

function TThoriumTypeArray.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  if Operation.Operation in opReflexive then
  begin
    case Operation.Operation of
      opLen:
      begin
        Operation.ResultType := FThorium.FTypeInteger;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(len_a(0, 0), 0, -1, 1);
        Exit(True);
      end;
      opDevolatile:
      begin
        Operation.ResultType := nil;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(update_x(0), -1, -1, 0);
        Exit(True);
      end;
    end;
  end
  else if (Operation.Operation in opIndexedAccess) and (TheObject is TThoriumTypeInteger) then
  begin
    case Operation.Operation of
      opIndexedRead:
      begin
        Operation.ResultType := FValueType;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(xiget(0, 0, 0), 1, 0, 2);
        Exit(True);
      end;
      opIndexedWrite:
      begin
        Operation.ResultType := FValueType;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(xiset(0, 0, 0), 1, 0, 2);
        Exit(True);
      end;
    end;
  end
  else if TheObject.IsEqualTo(Self) then
  begin
    case Operation.Operation of
      opAddition:
      begin
        Operation.ResultType := Self;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
      opMultiplication:
      begin
        Operation.ResultType := Self;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
      opCmpEqual:
      begin
        Operation.ResultType := FThorium.FTypeInteger;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
      opCmpNotEqual:
      begin
        Operation.ResultType := FThorium.FTypeInteger;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
    else
      Exit(False);
    end;
  end;
  Result := False;
end;

function TThoriumTypeArray.IsEqualTo(const AnotherType: TThoriumType): Boolean;
begin
  Result := (AnotherType is TThoriumTypeArray) and (TThoriumTypeArray(AnotherType).FValueType.IsEqualTo(FValueType));
end;

function TThoriumTypeArray.HasIndexedAccess: Boolean;
begin
  Result := True;
end;

function TThoriumTypeArray.NeedsClear: Boolean;
begin
  Result := True;
end;

{ TThoriumTypeDynamicArray }

function TThoriumTypeDynamicArray.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  case Operation.Operation of
    opAppend:
    begin
      if not TheObject.IsEqualTo(FValueType) then
        Exit(inherited CanPerformOperation(Operation, TheObject, ExName));
      NewNonCastOperation(Operation);
      Operation.ResultType := nil;
      Operation.OperationInstruction := OperationInstructionDescription(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0), -1, -1, -1);
      Exit(True);
    end;
  else
    Exit(inherited CanPerformOperation(Operation, TheObject, ExName));
  end;
end;

function TThoriumTypeDynamicArray.DoGetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue): TThoriumValue;
begin
  if (AIndex.Int < 0) or (AIndex.Int > High(AValue.DynamicArray^)) then
    raise EThoriumRuntimeException.CreateFmt('Dynamic array index %d out of bounds (%d..%d)', [AIndex.Int, 0, High(AValue.DynamicArray^)]);
  Result := AValue.DynamicArray^[AIndex.Int];
end;

function TThoriumTypeDynamicArray.DoGetLength(const AValue: TThoriumValue): Integer;
begin
  Result := Length(AValue.DynamicArray^);
end;

procedure TThoriumTypeDynamicArray.DoSetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue; const NewValue: TThoriumValue);
begin
  if (AIndex.Int < 0) or (AIndex.Int > High(AValue.DynamicArray^)) then
    raise EThoriumRuntimeException.CreateFmt('Dynamic array index %d out of bounds (%d..%d)', [AIndex.Int, 0, High(AValue.DynamicArray^)]);
  ThoriumFreeValue(AValue.DynamicArray^[AIndex.Int]);
  AValue.DynamicArray^[AIndex.Int] := ThoriumDuplicateValue(NewValue);
end;

{ TThoriumTypeStaticArray }

constructor TThoriumTypeStaticArray.Create(const AThorium: TThorium;
  const AValueType: TThoriumType; Count: Integer);
begin
  if Count <= 0 then
    raise EThoriumException.CreateFmt('Cannot create a static array type with a length of %d.', [Count]);
  inherited Create(AThorium, AValueType);
  FCount := Count;
end;

function TThoriumTypeStaticArray.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  Result := inherited;
  if Operation.Operation in [opAddition, opMultiplication] then
    Exit(False);
  if Operation.Operation = opLen then
    Operation.OperationInstruction := OperationInstructionDescription(int(FCount, 0), -1, -1, 4);
end;

function TThoriumTypeStaticArray.IsEqualTo(const AnotherType: TThoriumType
  ): Boolean;
begin
  Result := (AnotherType is TThoriumTypeStaticArray) and
    (TThoriumTypeStaticArray(AnotherType).FCount = FCount) and
    (TThoriumTypeStaticArray(AnotherType).FValueType.IsEqualTo(FValueType));
end;

function TThoriumTypeStaticArray.DoGetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue): TThoriumValue;
begin
  if (AIndex.Int < 0) or (AIndex.Int >= FCount) then
    raise EThoriumRuntimeException.CreateFmt('Static array index %d out of bounds (%d..%d)', [AIndex.Int, 0, FCount-1]);
  Result := AValue.StaticArray^[AIndex.Int];
end;

function TThoriumTypeStaticArray.DoGetLength(const AValue: TThoriumValue
  ): Integer;
begin
  Result := FCount;
end;

procedure TThoriumTypeStaticArray.DoSetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue; const NewValue: TThoriumValue);
begin
  if (AIndex.Int < 0) or (AIndex.Int >= FCount) then
    raise EThoriumRuntimeException.CreateFmt('Static array index %d out of bounds (%d..%d)', [AIndex.Int, 0, FCount-1]);
  ThoriumFreeValue(AValue.StaticArray^[AIndex.Int]);
  AValue.StaticArray^[AIndex.Int] := ThoriumDuplicateValue(NewValue);
end;

{%ENDREGION}

{%REGION 'Thorium functions and variables' /fold}

{ TThoriumParameters }

constructor TThoriumParameters.Create;
begin
  inherited Create;
  FList := TThoriumTypes.Create;
end;

destructor TThoriumParameters.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TThoriumParameters.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TThoriumParameters.GetTypeSpec(Index: Integer): TThoriumType;
begin
  Result := FList[Index];
end;

procedure TThoriumParameters.Assign(ASource: TThoriumParameters);
var
  I: Integer;
begin
  Clear;
  for I := 0 to ASource.FList.Count - 1 do
    Add(ASource.FList[I]);
end;

procedure TThoriumParameters.Add(AType: TThoriumType);
begin
  FList.Add(AType);
end;

procedure TThoriumParameters.Clear;
begin
  FList.Clear;
end;

procedure TThoriumParameters.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

procedure TThoriumParameters.Remove(AType: TThoriumType);
begin
  FList.Remove(AType);
end;

function TThoriumParameters.Duplicate: TThoriumParameters;
var
  I: Integer;
begin
  Result := TThoriumParameters.Create;
  for I := 0 to FList.Count - 1 do
  begin
    Result.FList.Add(FList[I]);
  end;
end;

procedure TThoriumParameters.GetParameterSpec(const Index: Integer; out
  ParamSpec: TThoriumType);
begin
  ParamSpec := FList[Index];
end;

procedure TThoriumParameters.LoadFromStream(Stream: TStream);
begin
  raise EThoriumException.Create('Not reimplemented yet.');
  (*Clear;
  Stream.Read(ACount, SizeOf(LongInt));
  FList.Capacity := ACount;
  for I := 0 to ACount - 1 do
  begin
    TypeSpec := GetMem(SizeOf(TThoriumType));
    Stream.Read(TypeSpec^, SizeOf(TThoriumType));
    FList.Add(TypeSpec);
  end;*)
end;

procedure TThoriumParameters.SaveToStream(Stream: TStream);
(*var
  I: Integer;
  ACount: LongInt;*)
begin
  raise EThoriumException.Create('Not reimplemented yet.');
  (*ACount := FList.Count;
  Stream.Write(ACount, SizeOf(LongInt));
  for I := 0 to Count - 1 do
    Stream.Write(PThoriumType(FList[I])^, SizeOf(TThoriumType));*)
end;

{ TThoriumPublicValue }

constructor TThoriumPublicValue.Create(AModule: TThoriumModule; AName: String);
begin
  inherited Create;
  FModule := AModule;
  FName := AName;
end;

procedure TThoriumPublicValue.LoadFromStream(Stream: TStream);
begin
  FName := Stream.ReadAnsiString;
end;

procedure TThoriumPublicValue.SaveToStream(Stream: TStream);
begin
  Stream.WriteAnsiString(FName);
end;

{ TThoriumFunction }

constructor TThoriumFunction.Create(AModule: TThoriumModule; AName: String);
begin
  inherited Create(AModule, AName);
  FEntryPoint := -1;
  //FEventCapsules := TFPObjectHashTable.CreateWith(50, @RSHash);
  FNestingLevel := -1;
  FPrototype := TThoriumTypeFunction.Create;
  FPrototyped := False;
  FVisibilityLevel := vsPrivate;
  FPrototypedCalls := TThoriumJumpList.Create;
end;

destructor TThoriumFunction.Destroy;
begin
  FPrototype.Free;
  FPrototypedCalls.Free;
  inherited Destroy;
end;

function TThoriumFunction.Duplicate: TThoriumFunction;
begin
  Result := TThoriumFunction.Create(FModule, FName);
  Result.FEntryPoint := FEntryPoint;
  Result.FNestingLevel := FNestingLevel;
  Result.FPrototyped := False;
  Result.FVisibilityLevel := FVisibilityLevel;
  Result.FPrototype.Assign(FPrototype);
end;

(*function TThoriumFunction.AsEvent(AParameters: array of TThoriumHostType;
  ReturnType: TThoriumHostType): TThoriumFunctionCallbackCapsule;
begin
  //Result := AsEvent(Parameters, ReturnType, [], nil);
end;

function TThoriumFunction.AsEvent(AParameters: array of TThoriumHostType;
  ReturnType: TThoriumHostType; ExtParameters: array of TThoriumHostObjectType;
  ExtReturnType: TThoriumHostObjectType): TThoriumFunctionCallbackCapsule;
begin

end;*)

procedure TThoriumFunction.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FEntryPoint, SizeOf(TThoriumInstructionAddress));
  Stream.Read(FVisibilityLevel, SizeOf(TThoriumVisibilityLevel));
  raise Exception.Create('Not re-implemented yet.');
end;

procedure TThoriumFunction.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FEntryPoint, SizeOf(TThoriumInstructionAddress));
  Stream.Write(FVisibilityLevel, SizeOf(TThoriumVisibilityLevel));
  raise Exception.Create('Reimplement TThoriumFunction.SaveToStream');
  //FParameters.SaveToStream(Stream);
  //FReturnValues.SaveToStream(Stream);
end;

{ TThoriumVariable }

constructor TThoriumVariable.Create(AModule: TThoriumModule; AName: String);
begin
  inherited Create(AModule, AName);
  FIsStatic := False;
  FStackPosition := 0;
  FillByte(FTypeSpec, SizeOf(TThoriumType), 0);
end;

procedure TThoriumVariable.AssignFromTableEntry(
  const ATableEntry: TThoriumTableEntry);
begin
  if not (ATableEntry._Type in [ttGlobal]) then
    raise EThoriumCompilerError.Create('Cannot assign a non-global to a TThoriumVariable instance.');
  FStackPosition := ATableEntry.Offset;
  FTypeSpec := ATableEntry.TypeSpec;
  FIsStatic := not ATableEntry.Writable;
end;

procedure TThoriumVariable.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FIsStatic, SizeOf(Boolean));
  Stream.Read(FStackPosition, SizeOf(Integer));
  Stream.Read(FTypeSpec, SizeOf(TThoriumType))
end;

procedure TThoriumVariable.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FIsStatic, SizeOf(Boolean));
  Stream.Write(FStackPosition, SizeOf(Integer));
  Stream.Write(FTypeSpec, SizeOf(TThoriumType));
end;
{%ENDREGION}

{%REGION 'Host class types' /fold}

{ TThoriumHostObjectType }

constructor TThoriumHostObjectType.Create(ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary.FThorium);
  FLibrary := ALibrary;
  FHashGenerated := False;
end;

destructor TThoriumHostObjectType.Destroy;
begin
  inherited Destroy;
end;

function TThoriumHostObjectType.FindMethod(const AMethodName: String
  ): TThoriumHostMethodBase;
begin
  Result := nil;
end;

function TThoriumHostObjectType.GetFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
begin
  Result := False;
end;

function TThoriumHostObjectType.GetIndexType(const IndexType: TThoriumType; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition): Boolean;
begin
  Result := False;
end;

function TThoriumHostObjectType.GetStaticFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
begin
  Result := False;
end;

{ TThoriumRTTIObjectType }

constructor TThoriumRTTIObjectType.Create(ALibrary: TThoriumLibrary);
begin
  raise EThoriumException.Create('TThoriumRTTIObjectType must not be created without BaseClass.');
end;

constructor TThoriumRTTIObjectType.Create(ALibrary: TThoriumLibrary;
  ABaseClass: TThoriumPersistentClass; AbstractClass: Boolean = False);
begin
  if ABaseClass = nil then
    raise EThoriumException.Create('TThoriumRTTIObjectType must not be created without BaseClass.');
  Create(ALibrary, ABaseClass, @ABaseClass.GetMethodList, @ABaseClass.GetStaticMethodList, AbstractClass);
  FCanUsePersistent := True;
end;

constructor TThoriumRTTIObjectType.Create(ALibrary: TThoriumLibrary;
  ABaseClass: TClass; MethodCallback: TThoriumRTTIMethodsCallback;
  StaticMethodCallback: TThoriumRTTIStaticMethodsCallback;
  AbstractClass: Boolean = False);
var
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  if not AbstractClass and not Supports(ABaseClass, IThoriumPersistent) then
    raise EThoriumException.CreateFmt('%s does not support IThoriumPersistent.', [ABaseClass.ClassName]);
  inherited Create(ALibrary);
  FCanUsePersistent := False;
  FBaseClass := ABaseClass;
  TypeInfo := FBaseClass.ClassInfo;
  if TypeInfo <> nil then
  begin
    FPropCount := GetTypeData(TypeInfo)^.PropCount;
    GetMem(FPropList, FPropCount * SizeOf(Pointer));
    GetPropInfos(TypeInfo, FPropList);
  end
  else
  begin
    FPropCount := 0;
    FPropList := nil;
  end;
  if MethodCallback <> nil then
  begin
    MethodCallback(Self, FMethods);
    for I := 0 to High(FMethods) do
    begin
      with FMethods[I] do
      begin
        FName := ThoriumCase(FMethods[I].FName);
        FHostObjectType := Self;
      end;
      if FMethods[I] is TThoriumHostMethodNativeCall then
        TThoriumHostMethodNativeCall(FMethods[I]).Precompile;
    end;
  end;
  if StaticMethodCallback <> nil then
  begin
    StaticMethodCallback(Self, FStaticMethods);
    for I := 0 to High(FStaticMethods) do
    begin
      with FStaticMethods[I] do
        FName := ThoriumCase(FStaticMethods[I].FName);
      if FStaticMethods[I] is TThoriumHostFunctionNativeCall then
        TThoriumHostFunctionNativeCall(FStaticMethods[I]).Precompile;
    end;
  end;
  FStoringProperties := TStringList.Create;
end;

destructor TThoriumRTTIObjectType.Destroy;
var
  I: Integer;
begin
  Freemem(FPropList);
  for I := 0 to High(FMethods) do
    FMethods[I].Free;
  for I := 0 to High(FStaticMethods) do
    FStaticMethods[I].Free;
  FStoringProperties.Free;
  inherited Destroy;
end;

procedure TThoriumRTTIObjectType.ApplyStoring(var AValue: Pointer;
  MayDecreaseReference: Boolean);
var
  Intf: IThoriumPersistent;
begin
  if FCanUsePersistent then
  begin
    TThoriumPersistent(AValue).FReferenceImplementation.EnableHostControl;
    if MayDecreaseReference then
      TThoriumPersistent(AValue).FReferenceImplementation.FreeReference;
  end
  else
  begin
    TObject(AValue).GetInterface(IThoriumPersistent, Intf);
    Intf.EnableHostControl;
    if MayDecreaseReference then
      Intf.FreeReference;
    Intf := nil;
  end;
end;

procedure TThoriumRTTIObjectType.DisposeValue(var AValue: Pointer);
var
  Intf: IThoriumPersistent;
begin
  if FCanUsePersistent then
    TThoriumPersistent(AValue).FReferenceImplementation.FreeReference
  else
  begin
    TObject(AValue).GetInterface(IThoriumPersistent, Intf);
    Intf.FreeReference;
    Intf := nil;
  end;
  AValue := nil;
end;

function TThoriumRTTIObjectType.DuplicateInstance(const AValue: Pointer
  ): Pointer;
var
  Intf: IThoriumPersistent;
begin
  // Then assign the pointer from the source value. We can use the GetReference
  // method to automatically increase the stuff...

  if FCanUsePersistent then
    TThoriumPersistent(Result) := TThoriumPersistent(TThoriumPersistent(AValue).FReferenceImplementation.GetReference)
  else
  begin
    TObject(AValue).GetInterface(IThoriumPersistent, Intf);
    TObject(Result) := Intf.GetReference;
    Intf := nil;
  end;
  //TThoriumPersistent(Result.Extended.Value) := TThoriumPersistent(AValue.Value).GetReference;
end;

function TThoriumRTTIObjectType.FindMethod(const AMethodName: String): TThoriumHostMethodBase;
var
  I: Integer;
begin
  for I := 0 to High(FMethods) do
  begin
    Result := FMethods[I];
    if Result.Name = AMethodName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumRTTIObjectType.GetFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
var
  FieldName: ShortString;
  I: Integer;
begin
  if Length(FieldIdent) > 255 then
  begin
    Result := False;
    Exit;
  end;
  FieldName := FieldIdent;
  for I := 0 to FPropCount - 1 do
  begin
    if (ShortCompareText(FieldName, UpCase(FPropList^[I]^.Name)) = 0) then
    begin
      ID := QWord(I);
      Result := True;
      Exit;
    end;
  end;
  for I := 0 to High(FMethods) do
  begin
    if FieldIdent = FMethods[I].Name then
    begin
      ID := QWord(I) or THORIUM_RTTI_METHOD_BIT;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TThoriumRTTIObjectType.GetFieldStoring(const AFieldID: QWord
  ): Boolean;
var
  PropInfo: PPropInfo;
begin
  if (AFieldID and THORIUM_RTTI_METHOD_BIT <> 0) then
  begin
    Result := False;
    Exit;
  end;
  PropInfo := FPropList^[AFieldID];
  Result := GetPropertyStoring(PropInfo);
end;

procedure TThoriumRTTIObjectType.GetFieldType(const AFieldID: QWord; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
var
  Info: PPropInfo;
begin
  if (AFieldID and THORIUM_RTTI_METHOD_BIT = THORIUM_RTTI_METHOD_BIT) then
  begin
    TypeSpec := FMethods[AFieldID xor THORIUM_RTTI_METHOD_BIT].Prototype;
    Access.ReadAccess.Allowed := True;
    Access.ReadAccess := AccessDescription(xmeth(FMethods[AFieldID xor THORIUM_RTTI_METHOD_BIT], 0, 0), -1, 4, 5);
    Access.WriteAccess.Allowed := False;
  end
  else
  begin
    Info := FPropList^[AFieldID];//PPropInfo(ptruint(AFieldID));
    Access.ReadAccess := AccessDescription(xfget(AFieldID, 0, 0), -1, 5, 4);
    if Info^.SetProc = nil then
    begin
      Access.WriteAccess.Allowed := False;
    end
    else
    begin
      Access.WriteAccess := AccessDescription(xfset(AFieldID, 0, 0), 5, -1, 4);
    end;
(*    case Info^.PropType^.Kind of
      tkInteger, tkEnumeration, tkSet, tkInt64, tkQWord, tkBool:
        TypeSpec := TThoriumTypeInteger.Create;
      tkFloat:
        TypeSpec := TThoriumTypeFloat.Create;
      tkSString, tkLString, tkAString, tkWString:
        TypeSpec := TThoriumTypeString.Create;
      tkClass:
      begin
        TypeData := GetTypeData(Info^.PropType);
        ObjClass := TypeData^.ClassType;
        ExtType := FLibrary.DeepFindHostTypeForType(ObjClass.ClassInfo);
        if ExtType <> nil then
          TypeSpec := ExtType;
      end;
    end;*)
    TypeSpec := FLibrary.DeepFindTypeForHostType(Info^.PropType);
  end;
end;

function TThoriumRTTIObjectType.GetNewInstance: Pointer;
begin
  Result := nil;
end;

function TThoriumRTTIObjectType.GetPropertyStoring(const PropInfo: PPropInfo
  ): Boolean;
begin
  Result := FStoringProperties.IndexOfObject(TObject(PropInfo)) >= 0;
end;

function TThoriumRTTIObjectType.GetPropertyStoring(const PropertyName: String
  ): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(FBaseClass, PropertyName);
  if PropInfo = nil then
    raise EPropertyError.CreateFmt('Property ''%s'' not found.', [PropertyName]);
  Result := GetPropertyStoring(PropInfo);
end;

function TThoriumRTTIObjectType.GetStaticFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FStaticMethods) do
  begin
    if FStaticMethods[I].Name = FieldIdent then
    begin
      ID := QWord(I) or THORIUM_RTTI_METHOD_BIT;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TThoriumRTTIObjectType.GetStaticFieldType(const AFieldID: QWord; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
begin
  if AFieldID and THORIUM_RTTI_METHOD_BIT = 0 then
  begin
    raise EThoriumException.Create('Cannot do that.')
  end
  else
  begin
    Access.ReadAccess.Allowed := False;
    Access.WriteAccess.Allowed := False;
  end;
end;

procedure TThoriumRTTIObjectType.DoEnableHostControl(const AValue: TThoriumValue
  );
var
  Intf: IThoriumPersistent;
begin
  if FCanUsePersistent then
    TThoriumPersistent(AValue.HostObject).FReferenceImplementation.EnableHostControl
  else
  begin
    TObject(AValue.HostObject).GetInterface(IThoriumPersistent, Intf);
    Intf.EnableHostControl;
    Intf := nil;
  end;
end;

function TThoriumRTTIObjectType.DoEvaluate(const AValue: TThoriumValue): Boolean;
begin
  if AValue.HostObject <> nil then
    Exit(True);
  Result := False;
end;

function TThoriumRTTIObjectType.DoGetField(const AValue: TThoriumValue;
  const AFieldID: QWord): TThoriumValue;
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

procedure TThoriumRTTIObjectType.DoSetField(const AValue: TThoriumValue;
  const AFieldID: QWord; const NewValue: TThoriumValue);
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

procedure TThoriumRTTIObjectType.SetPropertyStoring(const PropInfo: PPropInfo;
  const Storing: Boolean);
var
  Idx: Integer;
begin
  Idx := FStoringProperties.IndexOfObject(TObject(PropInfo));
  if Storing then
  begin
    if Idx < 0 then
      FStoringProperties.AddObject(PropInfo^.Name, TObject(PropInfo));
  end
  else
  begin
    if Idx >= 0 then
      FStoringProperties.Delete(Idx);
  end;
end;

procedure TThoriumRTTIObjectType.SetPropertyStoring(const PropertyName: String;
  const Storing: Boolean);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(FBaseClass, PropertyName);
  if PropInfo = nil then
    raise EPropertyError.CreateFmt('Property ''%s'' not found.', [PropertyName]);
  SetPropertyStoring(PropInfo, Storing);
end;

procedure TThoriumRTTIObjectType.CalcHash;
var
  Signature: String;
  SigLength: Integer;
  Offset: Integer;
  I: Integer;
begin
  SigLength := 12;
  for I := 0 to FPropCount - 1 do
    Inc(SigLength, SizeOf(TTypeKind) + Length(FPropList^[I]^.Name));
  for I := 0 to High(FMethods) do
    Inc(SigLength, 16);
  for I := 0 to High(FStaticMethods) do
    Inc(SigLength, 16);
  SetLength(Signature, SigLength);
  Move(FPropCount, Signature[1], SizeOf(Integer));
  Move(High(FMethods), Signature[5], SizeOf(Integer));
  Move(High(FStaticMethods), Signature[9], SizeOf(Integer));
  Offset := 13;
  for I := 0 to FPropCount - 1 do
  begin
    Move(FPropList^[I]^.PropType^.Kind, Signature[Offset], SizeOf(TTypeKind));
    Inc(Offset, SizeOf(TTypeKind));
    Move(FPropList^[I]^.Name[1], Signature[Offset], Length(FPropList^[I]^.Name));
    Inc(Offset, Length(FPropList^[I]^.Name));
  end;
  for I := 0 to High(FMethods) do
  begin
    Move(FMethods[I].GetHash, Signature[Offset], 16);
    Inc(Offset, 16);
  end;
  for I := 0 to High(FStaticMethods) do
  begin
    Move(FStaticMethods[I].GetHash, Signature[Offset], 16);
    Inc(Offset, 16);
  end;
  FHash := TThoriumHash(MD5Buffer(Signature[1], Length(Signature)));
end;

function TThoriumRTTIObjectType.NewNativeCallMethod(const AName: String;
  const ACodePointer: Pointer; const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodNativeCall;
var
  I: Integer;
begin
  Result := TThoriumHostMethodNativeCall.Create(Self.FLibrary);
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    for I := 0 to High(AParameters) do
      Parameters.Add(AParameters[I]);
    FReturnType.HostType := AReturnType;
    CallingConvention := ACallingConvention;
  end;
end;

function TThoriumRTTIObjectType.NewNativeCallStaticMethod(
  const AName: String; const ACodePointer: Pointer; const ADataPointer: Pointer;
  const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodAsFunctionNativeCall;
var
  I: Integer;
begin
  Result := TThoriumHostMethodAsFunctionNativeCall.Create(Self.FLibrary);
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    DataPointer := ADataPointer;
    for I := 0 to High(AParameters) do
      Parameters.Add(AParameters[I]);
    FReturnType.HostType := AReturnType;
    CallingConvention := ACallingConvention;
  end;
end;

function TThoriumRTTIObjectType.NewNativeCallStaticFunction(
  const AName: String; const ACodePointer: Pointer;
  const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostFunctionNativeCall;
var
  I: Integer;
begin
  Result := TThoriumHostFunctionNativeCall.Create(Self.FLibrary);
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    for I := 0 to High(AParameters) do
      Parameters.Add(AParameters[I]);
    FReturnType.HostType := AReturnType;
    CallingConvention := ACallingConvention;
  end;
end;

{ TThoriumHostRecordType }

constructor TThoriumHostRecordType.Create(ALibrary: TThoriumLibrary);
begin
  raise EThoriumException.Create('TThoriumHostRecordType must not be created without field information.');
end;

constructor TThoriumHostRecordType.Create(ALibrary: TThoriumLibrary;
  AFields: array of TThoriumHostRecordField);
var
  I: Integer;
begin
  inherited Create(ALibrary);
  SetLength(FFields, High(AFields) + 1);
  for I := 0 to High(AFields) do
  begin
    FFields[I] := AFields[I];
    FFields[I].FieldName := ThoriumCase(FFields[I].FieldName);
  end;
end;

destructor TThoriumHostRecordType.Destroy;
begin
  inherited Destroy;
end;

function TThoriumHostRecordType.IndexOfFieldDefinition(const AFieldName: String
  ): Integer;
begin
  for Result := 0 to Length(FFields) - 1 do
  begin
    if FFields[Result].FieldName = AFieldName then
      Exit;
  end;
  Result := -1;
end;

procedure TThoriumHostRecordType.CalcHash;
var
  I: Integer;
  Buffer: PByte;
  BufferSize: Ptruint;
  FieldHash: TThoriumHash;
begin
  (*BufferSize := EntrySize * Length(FFields);
  Buffer := GetMem(BufferSize);
  try
    for I := 0 to Length(FFields) - 1 do
    begin
      if FFields[I].FieldType.HostType and (htTypeSection or htSizeSection) = htExt then
        FieldHash := FFields[I].FieldType.Extended.GetHash
      else
        FillByte(FieldHash, SizeOf(TThoriumHash), 0);
      Buffer[I*EntrySize] := FFields[I].FieldType.HostType;
      Move(FieldHash[0], Buffer[I*EntrySize + 1], SizeOf(TThoriumHash));
    end;
    FHash := TThoriumHash(MD5Buffer(Buffer, BufferSize));
  finally
    FreeMem(Buffer);
  end;*)
  raise Exception.Create('Needs to be reimplemented');
end;

function TThoriumHostRecordType.CanAssignTo(
  var Assignment: TThoriumAssignmentDescription; const AnotherType: TThoriumType
  ): Boolean;
begin
  Result := IsEqualTo(AnotherType);
  if not Result then
  begin
    Result := inherited;
    Exit;
  end;
  Assignment.Cast.Needed := False;
end;

function TThoriumHostRecordType.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType
  ): Boolean;
begin
  Result := False;
end;

procedure TThoriumHostRecordType.DisposeValue(var AValue: Pointer);
type
  PRecordType = ^RecordType;
begin
  Dispose(PRecordType(AValue));
  AValue := nil;
end;

function TThoriumHostRecordType.DuplicateInstance(const AValue: Pointer
  ): Pointer;
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

function TThoriumHostRecordType.GetFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
var
  FieldDefinitionIdx: Integer;
  FieldDefinition: PThoriumHostRecordField;
  Mask: Cardinal;
begin
(*  FieldDefinitionIdx := IndexOfFieldDefinition(FieldIdent);
  Result := FieldDefinitionIdx >= 0;
  if not Result then
    Exit;
  FieldDefinition := @FFields[FieldDefinitionIdx];
  case FieldDefinition^.FieldType.HostType of
    htByte, htShortInt: Mask := $FF;
    htWord, htSmallInt: Mask := $FFFF;
    htDWord, htLongInt: Mask := $FFFFFFFF;
  else
    Mask := 0;
  end;
  ID := Mask or (QWord(FieldDefinitionIdx) shl 32);*)
  raise EThoriumException.Create('Not reimplemented yet.');
end;

function TThoriumHostRecordType.GetFieldStoring(const AFieldID: QWord
  ): Boolean;
var
  FieldDefinition: PThoriumHostRecordField;
begin
  FieldDefinition := @FFields[Integer((AFieldID shr 32) and $FFFFFFFF)];
  Result := FieldDefinition^.FieldType.Storing;
end;

procedure TThoriumHostRecordType.GetFieldType(const AFieldID: QWord; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

function TThoriumHostRecordType.GetNewInstance: Pointer;
type
  PRecordType = ^RecordType;
begin
  New(PRecordType(Result));
end;

function TThoriumHostRecordType.OpGetField(const AInstance: Pointer;
  const AFieldID: QWord): TThoriumValue;
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

procedure TThoriumHostRecordType.OpSetField(const AInstance: Pointer;
  const AFieldID: QWord; const NewValue: TThoriumValue);
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

{%ENDREGION}

{%REGION 'Host libraries' /fold}
{ TThoriumLibraryProperty }

constructor TThoriumLibraryProperty.Create;
begin

end;

{ TThoriumLibraryPropertyDirect }

constructor TThoriumLibraryPropertyDirect.Create;
begin
  inherited Create;
  FStatic := False;
  raise Exception.Create('Reimplement property');
  (*FTypeSpec.ValueType := vtBuiltIn;
  FTypeSpec.BuiltInType := btNil;*)
end;

destructor TThoriumLibraryPropertyDirect.Destroy;
begin
  raise Exception.Create('Reimplement property');
  //ThoriumFreeValue(FValue);
  inherited Destroy;
end;

procedure TThoriumLibraryPropertyDirect.CalcHash;
var
  Signature: String;
  SigLength: Integer;
  Offset: Integer;
begin
  raise Exception.Create('Reimplement property');
  (*SigLength := SizeOf(TThoriumValueType) + 1;
  if FTypeSpec.ValueType = vtBuiltIn then
    Inc(SigLength, SizeOf(TThoriumBuiltInType))
  else
    Inc(SigLength, SizeOf(TThoriumHash));
  SetLength(Signature, SigLength);
  if FStatic then
    Signature[1] := #1
  else
    Signature[1] := #0;
  Move(FTypeSpec.ValueType, Signature[2], SizeOf(TThoriumValueType));
  Offset := 1 + SizeOf(TThoriumValueType);
  if FTypeSpec.ValueType = vtBuiltIn then
    Move(FTypeSpec.BuiltInType, Signature[Offset], SizeOf(TThoriumBuiltInType))
  else
    Move(FTypeSpec.Extended.GetHash, Signature[Offset], SizeOf(TThoriumHash));
  FHash := TThoriumHash(MD5Buffer(Signature[1], Length(Signature)));*)
end;

procedure TThoriumLibraryPropertyDirect.GetValue(
  var AThoriumValue: TThoriumValue);
begin
  raise Exception.Create('Reimplement property');
end;

function TThoriumLibraryPropertyDirect.GetStatic: Boolean;
begin
  Result := FStatic;
end;

function TThoriumLibraryPropertyDirect.GetType: TThoriumType;
begin
  Result := FTypeSpec;
end;

procedure TThoriumLibraryPropertyDirect.SetValue(
  const AThoriumValue: TThoriumValue);
begin
  raise Exception.Create('Reimplement property');
end;

{ TThoriumLibraryPropertyDirectSetCallback }

constructor TThoriumLibraryPropertyDirectSetCallback.Create;
begin
  inherited Create;
  FOnPropertySet := nil;
end;

procedure TThoriumLibraryPropertyDirectSetCallback.SetValue(
  const AThoriumValue: TThoriumValue);
var
  Allow: Boolean;
begin
  Allow := True;
  FOnPropertySet(Self, AThoriumValue, Allow);
  if Allow then
    inherited SetValue(AThoriumValue);
end;

{ TThoriumLibraryPropertyCallback }

constructor TThoriumLibraryPropertyCallback.Create;
begin
  inherited Create;
  FStatic := False;
  raise Exception.Create('Reimplement property');
  (*FTypeSpec.ValueType := vtBuiltIn;
  FTypeSpec.BuiltInType := btNil;
  FOnPropertyGet := nil;
  FOnPropertySet := nil;*)
end;

procedure TThoriumLibraryPropertyCallback.CalcHash;
var
  Signature: String;
  SigLength: Integer;
  Offset: Integer;
begin
  raise Exception.Create('Reimplement property');
  (*SigLength := SizeOf(TThoriumValueType) + 1;
  if FTypeSpec.ValueType = vtBuiltIn then
    Inc(SigLength, SizeOf(TThoriumBuiltInType))
  else
    Inc(SigLength, SizeOf(TThoriumHash));
  SetLength(Signature, SigLength);
  if FStatic then
    Signature[1] := #1
  else
    Signature[1] := #0;
  Move(FTypeSpec.ValueType, Signature[2], SizeOf(TThoriumValueType));
  Offset := 1 + SizeOf(TThoriumValueType);
  if FTypeSpec.ValueType = vtBuiltIn then
    Move(FTypeSpec.BuiltInType, Signature[Offset], SizeOf(TThoriumBuiltInType))
  else
    Move(FTypeSpec.Extended.GetHash, Signature[Offset], SizeOf(TThoriumHash));
  FHash := TThoriumHash(MD5Buffer(Signature[1], Length(Signature)));*)
end;

procedure TThoriumLibraryPropertyCallback.GetValue(
  var AThoriumValue: TThoriumValue);
begin
  FOnPropertyGet(Self, AThoriumValue);
end;

function TThoriumLibraryPropertyCallback.GetStatic: Boolean;
begin
  Result := FStatic;
end;

function TThoriumLibraryPropertyCallback.GetType: TThoriumType;
begin
  Result := FTypeSpec;
end;

procedure TThoriumLibraryPropertyCallback.SetValue(
  const AThoriumValue: TThoriumValue);
begin
  FOnPropertySet(Self, AThoriumValue);
end;

{ TThoriumLibrary }

constructor TThoriumLibrary.Create(AThorium: TThorium);
begin
  inherited Create;
  FConstants := TThoriumLibraryConstantList.Create;
  FName := GetName;
  FHostCallables := TThoriumHostCallableList.Create;
  FProperties := TThoriumLibraryPropertyList.Create;
  FRequiredLibraries := TThoriumLibraryList.Create;
  FRTTITypes := TThoriumTypes.Create;
  FThorium := AThorium;
  FTypes := TThoriumTypes.Create;
  FTypeMap := TThoriumTypesMap.Create;
  InitializeLibrary;
end;

destructor TThoriumLibrary.Destroy;
begin
  HardUnfreeze;
  Clear;
  FTypes.Free;
  FRequiredLibraries.Free;
  FProperties.Free;
  FHostCallables.Free;
  FConstants.Free;
  FTypeMap.Free;
  FRTTITypes.Free;
  inherited Destroy;
end;

procedure TThoriumLibrary.Clear;
var
  I: Integer;
begin
  ForceUnfrozen;
  for I := 0 to FProperties.Count - 1 do
    FProperties[I].Free;
  FProperties.Clear;

  for I := 0 to FConstants.Count - 1 do
    FConstants[I].Free;
  FConstants.Clear;

  for I := 0 to FHostCallables.Count - 1 do
    FHostCallables[I].Free;
  FHostCallables.Clear;

  for I := 0 to FTypes.Count - 1 do
  begin
    FTypes[I]._Release;
    FTypes[I].Free;
  end;
  FTypes.Clear;
  FRTTITypes.Clear;
end;

procedure TThoriumLibrary.AddDependency(const ALibName: String);
var
  Lib: TThoriumLibrary;
begin
  Lib := FThorium.FindLibrary(ALibName);
  if Lib = nil then
    raise EThoriumDependencyException.CreateFmt('''%s'' cannot import required library ''%s''.', [GetName, ALibName]);
  AddDependency(Lib);
end;

procedure TThoriumLibrary.AddDependency(const ALib: TThoriumLibrary);
begin
  if FRequiredLibraries.IndexOf(ALib) >= 0 then
    Exit;
  FRequiredLibraries.Add(ALib);
end;

procedure TThoriumLibrary.CreatePrototypes;
var
  I: Integer;
begin
  for I := 0 to FHostCallables.Count - 1 do
    FHostCallables[I].CreatePrototype;
end;

procedure TThoriumLibrary.InitializeLibrary;
begin
  CreatePrototypes;
  Freeze;
end;

procedure TThoriumLibrary.RaiseRTTIRequired(const AStr: String);
begin
  raise EThoriumException.CreateFmt('RTTI required%s.', [AStr])
end;

procedure TThoriumLibrary.SetupPropertyDirect(
  const AInstance: TThoriumLibraryPropertyDirect; const AName: String;
  const ATypeSpec: TThoriumType; const AStatic: Boolean;
  const AInitialValue: PThoriumValue);
var
  Dummy: TThoriumCreateInstructionDescription;
begin
  with AInstance do
  begin
    FTypeSpec := ATypeSpec;
    FStatic := AStatic;
    if AInitialValue = nil then
    begin
      if not FTypeSpec.CanCreateNone(False, Dummy) then
        raise EThoriumException.CreateFmt('Direct library property: Cannot create ''none'' value for %s and no initial value supplied.', [FTypeSpec.GetName]);
      FValue := FTypeSpec.DoCreateNone;
    end
    else
    begin
      if AInitialValue^.RTTI <> nil then
        if not FTypeSpec.IsEqualTo(AInitialValue^.RTTI) then
          raise EThoriumConsistencyError.CreateFmt('Direct library property: %s (supplied type) is not equal to %s (supplied initial value).', [FTypeSpec.GetName, AInitialValue^.RTTI.GetName]);
      FValue := FTypeSpec.DuplicateValue(FValue);
    end;
  end;
  RegisterPropertyCustom(AName, AInstance);
end;

procedure TThoriumLibrary.CheckExisting(const AName: String;
  const AType: TThoriumType);
var
  CasedName: String;
  I: Integer;
begin
  CasedName := ThoriumCase(AName);
  for I := 0 to FConstants.Count - 1 do
  begin
    if (FConstants[I].Name = CasedName) and (FConstants[I].FValue.RTTI.IsEqualTo(AType)) then
      raise EThoriumConsistencyError.CreateFmt('Library ''%s'' already contains a symbol ''%s'' (constant) with type ''%s''.', [FName, AName, AType.GetName]);
  end;

  for I := 0 to FProperties.Count - 1 do
  begin
    if (FProperties[I].FName = CasedName) and (FProperties[I].GetType.IsEqualTo(AType)) then
      raise EThoriumConsistencyError.CreateFmt('Library ''%s'' already contains a symbol ''%s'' (property) with type ''%s''.', [FName, AName, AType.GetName]);
  end;

  for I := 0 to FTypes.Count - 1 do
  begin
    if (FTypes[I].GetName = CasedName) and (FTypes[I].IsEqualTo(AType)) then
      raise EThoriumConsistencyError.CreateFmt('Library ''%s'' already contains a symbol ''%s'' (type) with type ''%s''.', [FName, AName, AType.GetName]);
  end;
end;

function TThoriumLibrary.RegisterConstant(const AName: String;
  const AValue: TThoriumValue): PThoriumValue;
var
  CasedName: String;
  AConst: TThoriumLibraryConstant;
begin
  ForceUnfrozen;
  if AValue.RTTI = nil then
    RaiseRTTIRequired(' for library constants.');
  CasedName := ThoriumCase(AName);
  CheckExisting(CasedName, AValue.RTTI);

  AConst := TThoriumLibraryConstant.Create;
  AConst.FName := AName;
  AConst.FValue := AValue.RTTI.DuplicateValue(AValue);
  Result := @AConst.FValue;
  FConstants.Add(AConst);
end;

function TThoriumLibrary.RegisterConstant(const AName: String; const AValue;
  const AType: PTypeInfo): PThoriumValue;
var
  Value: TThoriumValue;
  TypeSpec: TThoriumType;
begin
  TypeSpec := DeepFindTypeForHostType(AType);
  if TypeSpec = nil then
    raise EThoriumException.Create('Could not find type declaration for given PTypeInfo.');
  Value := TypeSpec.CreateValueFromPtr(@AValue);
  Value.RTTI := TypeSpec;
  Result := RegisterConstant(AName, Value);
  ThoriumFreeValue(Value);
end;

function TThoriumLibrary.RegisterConstant(const AName: String;
  const AValue: TThoriumInteger): PThoriumValue;
begin
  Result := RegisterConstant(AName, AValue, TypeInfo(TThoriumInteger));
end;

function TThoriumLibrary.RegisterConstant(const AName: String;
  const AValue: TThoriumFloat): PThoriumValue;
begin
  Result := RegisterConstant(AName, AValue, TypeInfo(TThoriumFloat));
end;

function TThoriumLibrary.RegisterConstant(const AName: String;
  const AValue: TThoriumString): PThoriumValue;
begin
  Result := RegisterConstant(AName, AValue, TypeInfo(TThoriumString));
end;

function TThoriumLibrary.RegisterFinishedObjectType(const AName: String;
  const AInstance: TThoriumHostObjectType; const ATypeInfo: PTypeInfo
  ): TThoriumHostObjectType;
begin
  Result := TThoriumHostObjectType(RegisterType(AName, AInstance, ATypeInfo));
end;

function TThoriumLibrary.RegisterNativeCallFunction(const AName: String;
  const ACodePointer: Pointer; const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostFunctionNativeCall;
var
  CasedName: String;
  I: Integer;
begin
  ForceUnfrozen;
  CasedName := ThoriumCase(AName);
  Result := TThoriumHostFunctionNativeCall.Create(Self);
  try
    with Result do
    begin
      FName := CasedName;
      FCodePointer := ACodePointer;
      for I := 0 to High(AParameters) do
        FParameters.Add(AParameters[I]);
      FReturnType.HostType := AReturnType;
      FCallingConvention := ACallingConvention;
    end;
    // We cannot check for existing here, as the prototype is not known yet.
    // It will be known after precompilation, but then it's too late to mock
    // about anything wrong. So the user is responsible for taking care that no
    // duplicates get into this.
  except
    FreeAndNil(Result);
    raise;
  end;
  FHostCallables.Add(Result);
end;

function TThoriumLibrary.RegisterNativeCallMethodAsFunction(
  const AName: String; const ACodePointer: Pointer;
  const ADataPointer: Pointer; const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodAsFunctionNativeCall;
var
  CasedName: String;
  I: Integer;
begin
  ForceUnfrozen;
  CasedName := ThoriumCase(AName);
  Result := TThoriumHostMethodAsFunctionNativeCall.Create(Self);
  try
    with Result do
    begin
      FName := CasedName;
      FCodePointer := ACodePointer;
      FDataPointer := ADataPointer;
      for I := 0 to High(AParameters) do
        FParameters.Add(AParameters[I]);
      FReturnType.HostType := AReturnType;
      FCallingConvention := ACallingConvention;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
  FHostCallables.Add(Result);
end;

function TThoriumLibrary.RegisterPropertyCallback(const AName: String;
  const ATypeSpec: TThoriumType; AStatic: Boolean;
  const AGetCallback: TThoriumOnPropertyGet;
  const ASetCallback: TThoriumOnPropertySet): TThoriumLibraryPropertyCallback;
begin
  ForceUnfrozen;
  Result := TThoriumLibraryPropertyCallback.Create;
  try
    with Result do
    begin
      FTypeSpec := ATypeSpec;
      FStatic := AStatic;
      FOnPropertyGet := AGetCallback;
      FOnPropertySet := ASetCallback;
    end;
    RegisterPropertyCustom(AName, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TThoriumLibrary.RegisterPropertyCustom(const AName: String;
  const AInstance: TThoriumLibraryProperty): TThoriumLibraryProperty;
var
  CasedName: String;
begin
  ForceUnfrozen;
  Result := AInstance;
  CasedName := ThoriumCase(AName);
  CheckExisting(CasedName, Result.GetType);
  Result.FName := AName;
  FProperties.Add(Result);
end;

function TThoriumLibrary.RegisterPropertyDirect(const AName: String;
  const ATypeSpec: TThoriumType; const AStatic: Boolean;
  const AInitialValue: PThoriumValue): TThoriumLibraryPropertyDirect;
begin
  ForceUnfrozen;
  Result := TThoriumLibraryPropertyDirect.Create;
  try
    SetupPropertyDirect(Result, AName, ATypeSpec, AStatic, AInitialValue);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TThoriumLibrary.RegisterPropertyDirectCallback(const AName: String;
  const ATypeSpec: TThoriumType; const AStatic: Boolean;
  const ACallback: TThoriumOnPropertySetCallback;
  const AInitialValue: PThoriumValue): TThoriumLibraryPropertyDirectSetCallback;
begin
  ForceUnfrozen;
  Result := TThoriumLibraryPropertyDirectSetCallback.Create;
  Result.FOnPropertySet := ACallback;
  try
    SetupPropertyDirect(Result, AName, ATypeSpec, AStatic, AInitialValue);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TThoriumLibrary.RegisterRTTIType(
  const AClass: TThoriumPersistentClass; AbstractClass: Boolean
  ): TThoriumRTTIObjectType;
var
  CasedName: String;
begin
  ForceUnfrozen;
  CasedName := ThoriumCase(AClass.ClassName);
  Result := TThoriumRTTIObjectType.Create(Self, AClass, AbstractClass);
  try
    Result._AddRef;
    CheckExisting(CasedName, Result);
    Result.FName := CasedName;
  except
    Result._Release;
    FreeAndNil(Result);
    raise;
  end;
  FTypes.Add(Result);
  FRTTITypes.Add(Result);
  FTypeMap.Add(AClass.ClassInfo, Result);
end;

function TThoriumLibrary.RegisterRTTIType(const AClass: TClass;
  AMethodsCallback: TThoriumRTTIMethodsCallback;
  AStaticMethodsCallback: TThoriumRTTIStaticMethodsCallback;
  AbstractClass: Boolean): TThoriumRTTIObjectType;
var
  CasedName: String;
begin
  ForceUnfrozen;
  CasedName := ThoriumCase(AClass.ClassName);
  Result := TThoriumRTTIObjectType.Create(Self, AClass, AMethodsCallback, AStaticMethodsCallback, AbstractClass);
  try
    Result._AddRef;
    CheckExisting(CasedName, Result);
    Result.FName := CasedName;
  except
    Result._Release;
    FreeAndNil(Result);
    raise;
  end;
  FTypes.Add(Result);
  FRTTITypes.Add(Result);
  FTypeMap.Add(AClass.ClassInfo, Result);
end;

function TThoriumLibrary.RegisterSimpleMethod(const AName: String;
  const AFunction: TThoriumSimpleMethod;
  const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo): TThoriumHostFunctionSimpleMethod;
var
  CasedName: String;
  I: Integer;
begin
  ForceUnfrozen;
  CasedName := ThoriumCase(AName);
  Result := TThoriumHostFunctionSimpleMethod.Create(Self);
  try
    with Result do
    begin
      FName := CasedName;
      FMethod := AFunction;
      for I := 0 to High(AParameters) do
        FParameters.Add(AParameters[I]);
      FReturnType.HostType := AReturnType;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
  FHostCallables.Add(Result);
end;

function TThoriumLibrary.RegisterType(const AName: String;
  const AInstance: TThoriumType; const ATypes: array of PTypeInfo
  ): TThoriumType;
var
  CasedName: String;
  I: Integer;
begin
  ForceUnfrozen;
  CasedName := ThoriumCase(AName);
  // nil is used for void type.
  Result := AInstance;
  if AInstance <> nil then
  begin
    CheckExisting(CasedName, AInstance);
    Result.FName := CasedName;
    // Make sure it persists.
    Result._AddRef;
    for I := 0 to High(ATypes) do
      FTypeMap.Add(ATypes[I], Result);
  end;
  FTypes.Add(Result);
end;

function TThoriumLibrary.DeepFindType(const AName: String): TThoriumType;
var
  I: Integer;
begin
  Result := FindType(AName);
  if Result <> nil then
    Exit;
  for I := 0 to FRequiredLibraries.Count - 1 do
  begin
    Result := FRequiredLibraries[I].DeepFindType(AName);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumLibrary.DeepFindTypeForHostType(const AType: PTypeInfo
  ): TThoriumType;
var
  I: Integer;
begin
  Result := FindTypeForHostType(AType);
  if Result <> nil then
    Exit;
  for I := 0 to FRequiredLibraries.Count - 1 do
  begin
    Result := FRequiredLibraries[I].DeepFindTypeForHostType(AType);
    if Result <> nil then
      Exit;
  end;
end;

procedure TThoriumLibrary.DeepLookupIdentifier(const AName: String;
  var Entries: TThoriumTableEntryResults);
var
  I: Integer;
begin
  LookupIdentifier(AName, Entries);
  for I := 0 to FRequiredLibraries.Count - 1 do
    FRequiredLibraries[I].DeepLookupIdentifier(AName, Entries);
end;

function TThoriumLibrary.FindType(const AName: String): TThoriumType;
var
  I: Integer;
  CasedName: String;
begin
  CasedName := ThoriumCase(AName);
  for I := 0 to FTypes.Count - 1 do
  begin
    Result := FTypes[I];
    if Result.Name = CasedName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumLibrary.FindTypeForHostType(const AType: PTypeInfo
  ): TThoriumType;
var
  Idx: Integer;
begin
  Idx := -1;
  if not FTypeMap.Find(AType, Idx) then
    Exit(nil)
  else
    Exit(FTypeMap.Data[Idx]);
end;

procedure TThoriumLibrary.LookupIdentifier(const AName: String;
  var Entries: TThoriumTableEntryResults);
var
  I: Integer;
  CasedName: String;
  Match: TThoriumTableEntryResult;
begin
  ForceFrozen;
  Match.SourceLibrary := Self;
  Match.SourceModule := nil;
  CasedName := ThoriumCase(AName);
  for I := 0 to FConstants.Count - 1 do
  begin
    if FConstants[I].Name = CasedName then
    begin
      FillByte(Match.Entry, SizeOf(TThoriumTableEntry), 0);
      Match.Entry.Ptr := FConstants[I];
      Match.Entry._Type := ttLibraryConstant;
      Match.Entry.TypeSpec := FConstants[I].FValue.RTTI;
      Match.Entry.Value := FConstants[I].FValue;
      AppendTableEntry(Entries, Match);
    end;
  end;

  for I := 0 to FProperties.Count - 1 do
  begin
    if FProperties[I].Name = CasedName then
    begin
      FillByte(Match.Entry, SizeOf(TThoriumTableEntry), 0);
      Match.Entry.Ptr := FProperties[I];
      Match.Entry._Type := ttLibraryProperty;
      Match.Entry.TypeSpec := FProperties[I].GetType;
      AppendTableEntry(Entries, Match);
    end;
  end;

  for I := 0 to FHostCallables.Count - 1 do
  begin
    if FHostCallables[I].Name = CasedName then
    begin
      FillByte(Match.Entry, SizeOf(TThoriumTableEntry), 0);
      Match.Entry.Ptr := FHostCallables[I];
      Match.Entry.TypeSpec := FHostCallables[I].Prototype;
      Match.Entry._Type := ttHostCallable;
      AppendTableEntry(Entries, Match);
    end;
  end;

  for I := 0 to FTypes.Count - 1 do
  begin
    if FTypes[I].Name = CasedName then
    begin
      FillByte(Match.Entry, SizeOf(TThoriumTableEntry), 0);
      Match.Entry.Ptr := FTypes[I];
      Match.Entry.TypeSpec := FTypes[I];
      Match.Entry._Type := ttType;
      AppendTableEntry(Entries, Match);
    end;
  end;
end;

{%ENDREGION}

{%REGION 'Host functions & methods' /fold}
{ TThoriumHostParameters }

constructor TThoriumHostParameters.Create;
begin
  inherited Create;
  FCapacity := 0;
  FCount := 0;
  FParams := nil;
  Expand;
end;

destructor TThoriumHostParameters.Destroy;
begin
  if FParams <> nil then
    FreeMem(FParams);
  inherited Destroy;
end;

function TThoriumHostParameters.GetParameterInfo(Index: Integer
  ): PThoriumHostType;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt('Parameter index out of bounds (%d must be in [0..%d])', [Index, FCount-1]);
  Result := @FParams[Index];
end;

function TThoriumHostParameters.GetParameter(Index: Integer
  ): PTypeInfo;
begin
  Result := GetParameterInfo(Index)^.HostType;
end;

function TThoriumHostParameters.GetStoring(Index: Integer): Boolean;
begin
  Result := GetParameterInfo(Index)^.Storing;
end;

procedure TThoriumHostParameters.SetParameter(Index: Integer;
  const AValue: PTypeInfo);
begin
  ForceUnfrozen;
  if AValue = nil then
    raise EThoriumException.Create('Cannot set parameter type to nil.');
  GetParameterInfo(Index)^.HostType := AValue;
end;

procedure TThoriumHostParameters.SetStoring(Index: Integer;
  const AValue: Boolean);
begin
  ForceUnfrozen;
  GetParameterInfo(Index)^.Storing := AValue;
end;

procedure TThoriumHostParameters.Expand;
begin
  if (FCapacity = 0) then
    SetCapacity(8)
  else if FCapacity <= 256 then
    SetCapacity(FCapacity shl 2)
  else
    SetCapacity(FCapacity + 128);
end;

procedure TThoriumHostParameters.SetCapacity(AValue: Integer);
begin
  if AValue < FCapacity then
    Exit;
  FCapacity := AValue;
  ReAllocMem(FParams, FCapacity * SizeOf(TThoriumHostType));
end;

function TThoriumHostParameters.Add(const AType: PTypeInfo;
  const IsStoring: Boolean): Integer;
begin
  ForceUnfrozen;
  Result := FCount;
  if FCount = FCapacity then
    Expand;
  with FParams[FCount] do
  begin
    HostType := AType;
    Storing := IsStoring;
  end;
  Inc(FCount);
end;

procedure TThoriumHostParameters.Clear;
begin
  ForceUnfrozen;
  FCount := 0;
  if FCapacity > 16 then
    SetCapacity(16);
end;

procedure TThoriumHostParameters.Delete(const AIndex: Integer);
begin
  ForceUnfrozen;
  if AIndex = FCount - 1 then
  begin
    Dec(FCount);
    Exit;
  end;
  GetParameterInfo(AIndex);
  Move(FParams[AIndex+1], FParams[AIndex], SizeOf(TThoriumHostType) * (FCount - (AIndex+1)));
end;

function TThoriumHostParameters.IndexOf(const AType: PTypeInfo;
  const Nth: Integer): Integer;
var
  I: Integer;
  N: Integer;
begin
  N := -1;
  for I := 0 to FCount - 1 do
  begin
    if FParams[I].HostType = AType then
    begin
      Inc(N);
      if N = Nth then
        Exit(I);
    end;
  end;
  Result := -1;
end;

procedure TThoriumHostParameters.Insert(const AIndex: Integer;
  const AType: PTypeInfo; const IsStoring: Boolean);
var
  Info: PThoriumHostType;
begin
  ForceUnfrozen;
  if AIndex = FCount then
  begin
    Add(AType, IsStoring);
    Exit;
  end;
  if FCount = FCapacity then
    Expand;
  Info := GetParameterInfo(AIndex);
  Move(FParams[AIndex], FParams[AIndex+1], SizeOf(TThoriumHostType) * (FCount - AIndex));
  with Info^ do
  begin
    HostType := AType;
    Storing := IsStoring;
  end;
end;

{ TThoriumHostCallableBase }

function TThoriumHostCallableBase.GetPrototype: TThoriumTypeHostFunction;
begin
  if FPrototype = nil then
    raise Exception.Create('Prototype not built yet.');
  Result := FPrototype;
end;

constructor TThoriumHostCallableBase.Create(ALibrary: TThoriumLibrary);
begin
  FLibrary := ALibrary;
  FName := '';
  FParameters := TThoriumHostParameters.Create;
  FReturnType.HostType := nil;
end;

destructor TThoriumHostCallableBase.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

procedure TThoriumHostCallableBase.CalcHash;
var
  Signature: String;
  SigLength: Integer;
  Offset: Integer;
  I: Integer;
begin
  (*SigLength := 8 + Length(FName);
  for I := 0 to FParameters.Count - 1 do
  begin
    if FParameters.GetParamType(I) and (htTypeSection or htSizeSection) = htExt then
      Inc(SigLength, 17)
    else
      Inc(SigLength, 1);
  end;
  if FReturnType.HostType and (htTypeSection or htSizeSection) = htExt then
    Inc(SigLength, 17)
  else
    Inc(SigLength, 1);
  SetLength(Signature, SigLength);
  I := Length(FName);
  Move(I, Signature[1], SizeOf(Integer));
  Move(FParameters.Count, Signature[5], SizeOf(Integer));
  Offset := 9;
  for I := 0 to FParameters.Count - 1 do
  begin
    HostType := FParameters.GetParamType(I);
    Signature[Offset] := Chr(HostType);
    Inc(Offset);
    if HostType and (htTypeSection or htSizeSection) = htExt then
    begin
      Move(FParameters.GetExtendedType(I).GetHash, Signature[Offset], SizeOf(TThoriumHash));
      Inc(Offset, 16);
    end;
  end;
  Signature[Offset] := Chr(FReturnType.HostType);
  Inc(Offset);
  if FReturnType.HostType and (htTypeSection or htSizeSection) = htExt then
  begin
    Move(FReturnType.Extended.GetHash, Signature[Offset], SizeOf(TThoriumHash));
    Inc(Offset, 16);
  end;
  Move(FName[1], Signature[Offset], Length(FName));
  FHash := TThoriumHash(MD5Buffer(Signature[1], SigLength));*)
  raise EThoriumException.Create('Not reimplemented yet.');
end;

procedure TThoriumHostCallableBase.CreatePrototype;
begin
  FPrototype := TThoriumTypeHostFunction.Create(FLibrary.FThorium, Self);
end;

function TThoriumHostCallableBase.NeedsNativeData: Boolean;
begin
  Result := False;
end;

{ TThoriumExternalFunctionSimple }

constructor TThoriumHostFunctionSimpleMethod.Create(ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary);
  FMethod := nil;
end;

procedure TThoriumHostFunctionSimpleMethod.CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine = nil);
(*var
  VM: TThoriumVirtualMachine;
  Stack: TThoriumStack;
  I: Integer;
  PC: Integer;
  Params: array of Pointer;
  VAData: TThoriumSimpleVarargs;
  VAType: TThoriumHostType;
  ReturnVal: TThoriumValue;
  ST: PThoriumStackEntry;
  ParamType: TThoriumHostType;
  VAOffset: Integer;*)
begin
  raise Exception.Create('Reimplement');
  (*// Check for the virtual machine
  VM := AVirtualMachine;
  Stack := VM.GetStack;
  // Check if the method is valid
  if FMethod = nil then
    raise EThoriumRuntimeException.Create('Tried to call a nil method.');

  VAData.Count := 0;
  VAOffset := 0;
  VAType := htNone;
  // Store the parameter count to a local variable
  PC := FParameters.FCount;
  // Set the length of the variant array
  SetLength(Params, PC);
  for I := 0 to PC-1 do
  begin
    // Curr stack offset = I + VAOffset + 1
    // Parameter array index = PC-(I+1)
    ParamType := FParameters.GetParamType(PC-(I+1));
    if ParamType and htArray = htArray then
    begin
      if VAType <> htNone then
        raise EThoriumRuntimeException.Create('Cannot handle more than one vararg.');
      // Get the current stack entry
      ST := Stack.GetTop(I+VAOffset);
      // Check if it is an integer
      if (ST^.Value._Type <> vtBuiltIn) or (ST^.Value.BuiltIn._Type <> btInteger) then
        raise EThoriumRuntimeException.Create('Compiler mistake: No count indicator for varargs');
      // Determine the type for the varargs
      VAType := FParameters.GetParamType(PC-(I+1)) and not (htFlagSection);
      // Get the count from the stack
      VAData.Count := ST^.Value.BuiltIn.Int;
      if VAData.Count > 0 then
      begin
        Inc(VAOffset);
        VAData.Data := Stack.GetTop(I+VAOffset)^.VADataOrigin;
      end
      else
        VAData.Data := nil;
      // Assign the varargs array to the current parameter slot
      Params[PC-(I+1)] := @VAData;
    end
    else
    begin
      case ParamType of
        htIntU8, htIntS8, htIntU16, htIntS16, htIntU32, htIntS32, htIntU64,
        htIntS64:
        begin
          ST := Stack.GetTop(I+VAOffset);//VM.GetStack.GetStackEntry(THORIUM_STACK_SCOPE_FROMTOP, I+1);
          Params[PC-(I+1)] := @ST^.Value.BuiltIn.Int;
        end;
        htFlt32, htFlt64, htFlt80:
        begin
          ST := Stack.GetTop(I+VAOffset);
          Params[PC-(I+1)] := @ST^.Value.BuiltIn.Float;
        end;
        htString:
        begin
          Params[PC-(I+1)] := Stack.GetTop(I+VAOffset)^.Value.BuiltIn.Str;
        end;
        htExt:
        begin
          Params[PC-(I+1)] := Stack.GetTop(I+VAOffset)^.Value.Extended.Value;
        end;
      else
        raise EThoriumRuntimeException.Create('Invalid parameter type.');
      end;
    end;
  end;
  // Call the method with the given parameters
  if (FReturnType.HostType <> htNone) then
  begin
    // If a return value is given
    FMethod(Params, @ReturnVal);
    if VAType <> htNone then
    begin
      VM.FStack.Pop(2, True);
      Dec(PC, 2);
    end;
    VM.FStack.Pop(PC, False);
    ST := Stack.GetTopStackEntry;
    ST^._Type := stValue;
    ST^.Value := ReturnVal;
  end
  else
  begin
    FMethod(Params, nil);
    if VAType <> htNone then
    begin
      VM.FStack.Pop(2, True);
      Dec(PC, 2);
    end;
    VM.FStack.Pop(PC, False);
  end;  *)
end;

{ TThoriumHostFunctionNativeCall }

constructor TThoriumHostFunctionNativeCall.Create(ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary);
  FCallingConvention := ncRegister;
  FInstructions := nil;
  FCodePointer := nil;
end;

destructor TThoriumHostFunctionNativeCall.Destroy;
begin
  if FInstructions <> nil then
    FreeMem(FInstructions);
  inherited Destroy;
end;

procedure TThoriumHostFunctionNativeCall.CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine);
var
  Stack: TThoriumStack;
begin
  Stack := AVirtualMachine.GetStack;
  ExecuteSubscript(FInstructions, Stack.GetTopStackEntry, FCodePointer, nil);
  if FVAOffset > 0 then
    Stack.Pop(2, True);
  Stack.Pop(Parameters.Count - 1, False);
end;

procedure TThoriumHostFunctionNativeCall.CreatePrototype;
begin
  inherited;
  Precompile;
end;

function TThoriumHostFunctionNativeCall.NeedsNativeData: Boolean;
begin
  Result := True;
end;

procedure TThoriumHostFunctionNativeCall.Precompile;
begin
  GenericPrecompile(FInstructions, FParameters, FPrototype.FParameters, FReturnType, FPrototype.FReturnType, False, FCallingConvention);
end;

{ TThoriumHostMethodAsFunctionNativeCall }

constructor TThoriumHostMethodAsFunctionNativeCall.Create(
  ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary);
  FDataPointer := nil;
end;

procedure TThoriumHostMethodAsFunctionNativeCall.CallFromVirtualMachine(
  AVirtualMachine: TThoriumVirtualMachine);
var
  Stack: TThoriumStack;
begin
  Stack := AVirtualMachine.GetStack;
  ExecuteSubscript(FInstructions, Stack.GetTopStackEntry, FCodePointer, FDataPointer);
  if FVAOffset > 0 then
    Stack.Pop(2, True);
  Stack.Pop(FParameters.Count - 1, False);
end;

procedure TThoriumHostMethodAsFunctionNativeCall.Precompile;
begin
  GenericPrecompile(FInstructions, FParameters, FPrototype.FParameters, FReturnType, FPrototype.FReturnType, True, FCallingConvention);
end;

{ TThoriumHostMethodBase }

constructor TThoriumHostMethodBase.Create(ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary);
  FHostObjectType := nil;
end;

{ TThoriumHostMethodSimple }

constructor TThoriumHostMethodSimple.Create(ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary);
  FClassMethod := nil;
end;

procedure TThoriumHostMethodSimple.CallFromVirtualMachine(OfObject: TObject;
  AVirtualMachine: TThoriumVirtualMachine);
(*var
  VM: TThoriumVirtualMachine;
  Stack: TThoriumStack;
  I: Integer;
  PC: Integer;
  Params: array of Pointer;
  VAData: TThoriumSimpleVarargs;
  VAType: TThoriumHostType;
  VAOffset: Integer;
  ReturnVal: TThoriumValue;
  ST: PThoriumStackEntry;
  ParamType: TThoriumHostType;*)
begin
  raise Exception.Create('Reimplement');

  (*// Check for the virtual machine
  VM := AVirtualMachine;
  Stack := VM.GetStack;
  // Check if the method is valid
  if FClassMethod = nil then
    raise EThoriumRuntimeException.Create('Tried to call a nil method.');

  VAData.Count := 0;
  VAOffset := 0;
  VAType := htNone;
  // Store the parameter count to a local variable
  PC := FParameters.FCount;
  // Set the length of the variant array
  SetLength(Params, PC);
  for I := 0 to PC-1 do
  begin
    // Curr stack offset = I + VAOffset + 1
    // Parameter array index = PC-(I+1)
    ParamType := FParameters.GetParamType(PC-(I+1));
    if ParamType and htArray = htArray then
    begin
      if VAType <> htNone then
        raise EThoriumRuntimeException.Create('Cannot handle more than one vararg.');
      // Get the current stack entry
      ST := Stack.GetTop(I+VAOffset);
      // Check if it is an integer
      if (ST^.Value._Type <> vtBuiltIn) or (ST^.Value.BuiltIn._Type <> btInteger) then
        raise EThoriumRuntimeException.Create('Compiler mistake: No count indicator for varargs');
      // Determine the type for the varargs
      VAType := FParameters.GetParamType(PC-(I+1)) and not (htFlagSection);
      // Get the count from the stack
      VAData.Count := ST^.Value.BuiltIn.Int;
      if VAData.Count > 0 then
      begin
        Inc(VAOffset);
        VAData.Data := Stack.GetTop(I+VAOffset)^.VADataOrigin;
      end
      else
        VAData.Data := nil;
      // Assign the varargs array to the current parameter slot
      Params[PC-(I+1)] := @VAData;
    end
    else
    begin
      case ParamType of
        htIntU8, htIntS8, htIntU16, htIntS16, htIntU32, htIntS32, htIntU64,
        htIntS64:
        begin
          ST := Stack.GetTop(I+VAOffset);//VM.GetStack.GetStackEntry(THORIUM_STACK_SCOPE_FROMTOP, I+1);
          Params[PC-(I+1)] := @ST^.Value.BuiltIn.Int;
        end;
        htFlt32, htFlt64, htFlt80:
        begin
          ST := Stack.GetTop(I+VAOffset);
          Params[PC-(I+1)] := @ST^.Value.BuiltIn.Float;
        end;
        htString:
        begin
          Params[PC-(I+1)] := Stack.GetTop(I+VAOffset)^.Value.BuiltIn.Str;
        end;
        htExt:
        begin
          Params[PC-(I+1)] := Stack.GetTop(I+VAOffset)^.Value.Extended.Value;
        end;
      else
        raise EThoriumRuntimeException.Create('Invalid parameter type.');
      end;
    end;
  end;
  // Modify the method information
  TMethod(FClassMethod).Data := OfObject;
  // Call the method with the given parameters
  if (FReturnType.HostType <> htNone) then
  begin
    // If a return value is given,
    FClassMethod(Params, @ReturnVal);
    if VAType <> htNone then
    begin
      VM.FStack.Pop(2, True);
      Dec(PC, 2);
    end;
    VM.FStack.Pop(PC);
    ST := VM.FStack.GetStackEntry(THORIUM_STACK_SCOPE_FROMTOP, 1);
    ST^._Type := stValue;
    ST^.Value := ReturnVal;
  end
  else
  begin
    FClassMethod(Params, nil);
    if VAType <> htNone then
    begin
      VM.FStack.Pop(2, True);
      Dec(PC, 2);
    end;
    VM.FStack.Pop(PC);
  end;   *)
end;

{ TThoriumHostMethodNativeCall }

constructor TThoriumHostMethodNativeCall.Create(ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary);
  FCallingConvention := ncRegister;
  FCodePointer := nil;
  FInstructions := nil;
end;

destructor TThoriumHostMethodNativeCall.Destroy;
begin
  if FInstructions <> nil then
    FreeMem(FInstructions);
  inherited Destroy;
end;

procedure TThoriumHostMethodNativeCall.CallFromVirtualMachine(OfObject: TObject;
  AVirtualMachine: TThoriumVirtualMachine);
var
  Stack: TThoriumStack;
begin
  Stack := AVirtualMachine.GetStack;
  ExecuteSubscript(FInstructions, Stack.GetTopStackEntry, FCodePointer, OfObject);
  if FVAOffset > 0 then
    Stack.Pop(2, True);
  Stack.Pop(Parameters.Count - 1, False);
end;

function TThoriumHostMethodNativeCall.NeedsNativeData: Boolean;
begin
  Result := True;
end;

procedure TThoriumHostMethodNativeCall.Precompile;
begin
  GenericPrecompile(FInstructions, FParameters, FPrototype.FParameters, FReturnType, FPrototype.FReturnType, True, FCallingConvention);
end;
{%ENDREGION}

{%REGION 'Compiler utilities' /fold}

{ TThoriumIdentifierTable }

constructor TThoriumIdentifierTable.Create;
begin
  inherited Create;
  FIdentifiers := nil;
  FCapacity := 0;
  FCount := 0;
end;

destructor TThoriumIdentifierTable.Destroy;
begin
  ClearTable;
  inherited Destroy;
end;

procedure TThoriumIdentifierTable.Expand;
// Expands the list to a new size depending on the current size (to have a bit
// more intelligent expansion)
begin
  if FCapacity = 0 then
    SetCapacity(8)
  else if FCapacity <= 256 then
    SetCapacity(FCapacity shl 2)
  else
    SetCapacity(FCapacity + 128);
end;

procedure TThoriumIdentifierTable.ForceCapacity(NewCapacity: Integer);
// Forces the capacity to a specific value. It is even allowed to delete some
// entries with this function (in contrary to the SetCapacity method)
begin
  if FCount > NewCapacity then
  begin
    ClearTableTo(NewCapacity);
    // ClearTableTo calls ForceCapacity on its own
    Exit;
  end;
  ReAllocMem(FIdentifiers, NewCapacity * SizeOf(TThoriumTableEntry));
  FCapacity := NewCapacity;
end;

function TThoriumIdentifierTable.NewEntry: PThoriumTableEntry;
// Gets the next free entry in the list.
begin
  if FCapacity < FCount + 1 then
    Expand;
  Result := PThoriumTableEntry(ptruint(FIdentifiers) + Cardinal(FCount * SizeOf(TThoriumTableEntry)));
  Initialize(Result);
  Inc(FCount);
end;

procedure TThoriumIdentifierTable.SetCapacity(NewCapacity: Integer);
// Sets the capacity, but limits it to be at least equal to the current count
// (this means you cannot delete entries with this function)
begin
  if (NewCapacity < FCount) or (FCapacity = NewCapacity) then
    Exit;
  ReAllocMem(FIdentifiers, NewCapacity * SizeOf(TThoriumTableEntry));
  FCapacity := NewCapacity;
end;

procedure TThoriumIdentifierTable.FreeEntry(var AEntry: TThoriumTableEntry);
begin
  ThoriumFreeValue(AEntry.Value);
  Dispose(AEntry.Name);
  Finalize(AEntry);
end;

function TThoriumIdentifierTable.AddRegisterVariableIdentifier(Name: String;
  RegisterID: TThoriumRegisterID; TypeSpec: TThoriumType): PThoriumTableEntry;
// Adds an identifier declared as variable
begin
  Result := NewEntry;
  New(Result^.Name);
  Result^.Name^ := Name;
  Result^._Type := ttLocalRegisterVariable;
  Result^.Offset := RegisterID;
  Result^.TypeSpec := TypeSpec;
  Result^.ValidValue := False;
  FillByte(Result^.Value, SizeOf(TThoriumValue), 0);
end;

function TThoriumIdentifierTable.AddVariableIdentifier(const AName: String;
  Offset: Integer; TypeSpec: TThoriumType; Global, Writable: Boolean;
  AValue: PThoriumValue): PThoriumTableEntry;
begin
  Result := NewEntry;
  New(Result^.Name);
  Result^.Name^ := AName;
  if Global then
    Result^._Type := ttGlobal
  else
    Result^._Type := ttLocal;
  Result^.Offset := Offset;
  Result^.TypeSpec := TypeSpec;
  Result^.Writable := Writable;
  if (not Writable) and (AValue <> nil) then
  begin
    Result^.ValidValue := True;
    Result^.Value := ThoriumDuplicateValue(AValue^);
  end
  else
    Result^.ValidValue := False
end;

function TThoriumIdentifierTable.AddFunctionIdentifier(Name: String; Func: TThoriumFunction): PThoriumTableEntry;
// Adds an identifier declared as function
begin
  Result := NewEntry;
  New(Result^.Name);
  Result^.Name^ := Name;
  Result^._Type := ttGlobalCallable;
  Result^.Offset := 0;
  Result^.TypeSpec := Func.FPrototype;
  Result^.Ptr := Func;
  Result^.ValidValue := False;
  FillByte(Result^.Value, SizeOf(TThoriumValue), 0);
end;

function TThoriumIdentifierTable.AddParameterIdentifier(Name: String;
  Offset: Integer; TypeSpec: TThoriumType): PThoriumTableEntry;
var
  Val: TThoriumValue;
begin
  Result := NewEntry;
  New(Result^.Name);
  Result^.Name^ := Name;
  Result^._Type := ttParameter;
  Result^.Offset := Offset;
  Result^.TypeSpec := TypeSpec;
  Result^.Writable := False;
  FillByte(Result^.Value, SizeOf(TThoriumValue), 0);
end;

procedure TThoriumIdentifierTable.ClearTable;
// Deletes all identifiers from the table
var
  I: Integer;
  Entry: PThoriumTableEntry;
begin
  Entry := FIdentifiers;
  for I := 0 to FCount - 1 do
  begin
    FreeEntry(Entry^);
    Inc(Entry);
  end;
  FreeMem(FIdentifiers);
  FIdentifiers := nil;
  FCapacity := 0;
  FCount := 0;
end;

function TThoriumIdentifierTable.ClearTableTo(NewCount: Integer): Integer;
// Deletes all entries down to count entries (this should not be negative...
// could lead to some weird errors...)
var
  I: Integer;
  Entry: PThoriumTableEntry;
begin
  Result := 0;
  Entry := @FIdentifiers[NewCount];
  for I := NewCount to FCount - 1 do
  begin
    FreeEntry(Entry^);
    Inc(Entry);
  end;
  FCount := NewCount;
  ForceCapacity(NewCount);
end;

function TThoriumIdentifierTable.FindIdentifier(Name: String; out Ident: TThoriumTableEntry): Boolean;
// Traverses the list and tries to find an identifier with the given name. If so
// the declaration is written to Ident and true is returned. Otherwise, Ident is
// not changed and false is returned. Ident.Name will be nil, even if an entry
// is found.
var
  Rec: PThoriumTableEntry;
  I: Integer;
begin
  Rec := PThoriumTableEntry(ptruint(FIdentifiers) + Cardinal((FCount-1) * SizeOf(TThoriumTableEntry)));
  for I := FCount - 1 downto 0 do
  begin
    if Rec^.Name^ = Name then
    begin
      Move(Rec^, Ident, SizeOf(TThoriumTableEntry));
      Ident.Name := nil;
      Result := True;
      Exit;
    end;
    Dec(Rec);
  end;
  Result := False;
end;

procedure TThoriumIdentifierTable.ReadIdentifier(Index: Integer; out
  Ident: TThoriumTableEntry);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt('Table index (%d) out of bounds (%d..%d).', [Index, 0, FCount-1]);
  Ident := FIdentifiers[Index];
end;

{ TThoriumInstructions }

constructor TThoriumInstructions.Create;
begin
  inherited Create;
  FInstructions := nil;
  FCapacity := 0;
  FCount := 0;
  FPosition := 0;
  FInserted := 0;
  FSetPosition := 0;
  FAddressLists := TFPList.Create;
  FAddressPointers := TFPList.Create;
end;

destructor TThoriumInstructions.Destroy;
begin
  FreeMem(FInstructions);
  FAddressLists.Free;
  FAddressPointers.Free;
  inherited Destroy;
end;

procedure TThoriumInstructions.Expand;
// Expands the list to a bigger size.
begin
  if FCapacity = 0 then
    SetCapacity(8)
  else if FCapacity <= 256 then
    SetCapacity(FCapacity shl 2)
  else
    SetCapacity(FCapacity + 128);
end;

function TThoriumInstructions.GetInstruction(Index: Integer): PThoriumInstruction;
// Gets the pointer to the instruction at index Index or nil, if Index is out of
// bounds.
begin
  if (Index < 0) or (Index > FCount - 1) then
    Result := nil
  else
    Result := PThoriumInstruction(ptruint(FInstructions) + Cardinal(Index) * SizeOf(TThoriumInstruction));
end;

procedure TThoriumInstructions.SetCapacity(NewCapacity: Integer);
// Sets the new capacity for the instruction list. If NewCapacity is smaller
// than the current count, this method does nothing.
begin
  if (NewCapacity < FCount) then
    Exit;
  ReAllocMem(FInstructions, NewCapacity * SizeOf(TThoriumInstruction));
  FCapacity := NewCapacity;
end;

procedure TThoriumInstructions.SetPosition(NewPosition: TThoriumInstructionAddress);
// Sets the position of the next instruction. Is useful for switch-case-state-
// ments. When moving around, make sure that any jump commands are corrected
// accordingly
var
  I, List: Integer;
  CurrList: TThoriumIntList;
  Instr: PThoriumInstruction;
begin
  WriteLn('NewPosition > FCount: ', NewPosition, ' > ', FCount, ' => ', NewPosition > FCount);
  if NewPosition > FCount then
    Exit;
  if FPosition <> FCount then
  begin
    FixupAdresses(FSetPosition, FInserted);
    FInserted := 0;
  end;
  FPosition := NewPosition;
  FSetPosition := NewPosition;
end;

procedure TThoriumInstructions.FixupAdresses(
  const MinTarget: TThoriumInstructionAddress; Offset: LongInt);
var
  Instr: PThoriumInstruction;
  List, I: Integer;
  CurrList: TThoriumIntList;
begin
  Instr := FInstructions;
  for I := 0 to FCount - 1 do
  begin
    if Instr^.Instruction in THORIUM_JMP_INSTRUCTIONS then
    begin
      if TThoriumInstructionJMP(Instr^).NewAddress >= MinTarget then
        TThoriumInstructionJMP(Instr^).NewAddress := TThoriumInstructionJMP(Instr^).NewAddress + Offset;

    end;
    Inc(Instr);
  end;
  for List := 0 to FAddressLists.Count - 1 do
  begin
    CurrList := TThoriumIntList(FAddressLists[List]);
    for I := 0 to CurrList.Count - 1 do
    begin
      if CurrList.Items[I] >= MinTarget then
        CurrList.Items[I] := CurrList.Items[I] + Offset;
    end;
  end;
  for I := 0 to FAddressPointers.Count - 1 do
  begin
    if PThoriumInstructionAddress(FAddressPointers[I])^ >= MinTarget then
      PThoriumInstructionAddress(FAddressPointers[I])^ += Offset;
  end;
end;

(*function TThoriumInstructions.GenCode(InstructionCode: TThoriumInstructionCode;
  Param1, Param2, Param3: Int64; CodeLine: Cardinal): Integer;
// Adds a new instruction with the instruction code and the three parameters
// given.
var
  InstructionRec: PThoriumInstruction;
begin
  if FCapacity < FCount + 1 then
    Expand;
  Assert(FPosition <= FCount, 'FPosition <= FCount in TThoriumInstructions.GenCode');
  if FPosition = FCount then
  begin
    Inc(FCount);
    InstructionRec := GetInstruction(FPosition);
    InstructionRec^.Instruction := InstructionCode;
    InstructionRec^.Parameter1 := Param1;
    InstructionRec^.Parameter2 := Param2;
    InstructionRec^.Parameter3 := Param3;
    InstructionRec^.CodeLine := CodeLine;
    Result := FPosition;
    Inc(FPosition);
  end
  else
  begin
    Inc(FCount);
    InstructionRec := PThoriumInstruction(ptrint(FInstructions)+FPosition*SizeOf(TThoriumInstruction));
    Move(InstructionRec^,
      PThoriumInstruction(ptrint(InstructionRec)+SizeOf(TThoriumInstruction))^,
      SizeOf(TThoriumInstruction)*(FCount - FPosition));
    InstructionRec^.Instruction := InstructionCode;
    InstructionRec^.Parameter1 := Param1;
    InstructionRec^.Parameter2 := Param2;
    InstructionRec^.Parameter3 := Param3;
    InstructionRec^.CodeLine := CodeLine;
    Result := FPosition;
    Inc(FPosition);
  end;
end; *)

function TThoriumInstructions.AppendCode(AInstruction: TThoriumInstruction): Integer;
// Adds a new instruction with the instruction code and the three parameters
// given.
var
  InstructionRec: PThoriumInstruction;
begin
  if FCapacity < FCount + 1 then
    Expand;
  Assert(FPosition <= FCount, 'FPosition <= FCount in TThoriumInstructions.GenCode');
  if FPosition = FCount then
  begin
    Inc(FCount);
    InstructionRec := GetInstruction(FPosition);
    Move(AInstruction, InstructionRec^, SizeOf(TThoriumInstruction));
    Result := FPosition;
    Inc(FPosition);
  end
  else
  begin
    InstructionRec := PThoriumInstruction(ptruint(FInstructions)+Cardinal(FPosition*SizeOf(TThoriumInstruction)));
    Move(InstructionRec^,
      PThoriumInstruction(ptruint(InstructionRec)+SizeOf(TThoriumInstruction))^,
      SizeOf(TThoriumInstruction)*Cardinal(FCount - FPosition));
    Move(AInstruction, InstructionRec^, SizeOf(TThoriumInstruction));
    Result := FPosition;
    Inc(FCount);
    Inc(FPosition);
    Inc(FInserted);
  end;
end;

function TThoriumInstructions.AppendCode(Code: TThoriumInstructionArray): Integer;
// Adds multiple instructions at once.
var
  I: Integer;
begin
  for I := Low(Code) to High(Code) do
  begin
    //with Code[I] do
    //  GenCode(Instruction, Parameter1, Parameter2, Parameter3, CodeLine);
    if I = Low(Code) then
      Result := AppendCode(Code[I])
    else
      Result := AppendCode(Code[I]);

  end;
end;

procedure TThoriumInstructions.DeleteInstructions(AIndex, ACount: Integer);
begin
  Move(FInstructions[AIndex+ACount], FInstructions[AIndex], SizeOf(TThoriumInstruction)*(FCount-(AIndex+ACount)));
  FCount := FCount - ACount;
  FixupAdresses(AIndex+ACount, -ACount)
end;

(*function TThoriumInstructions.InsertCodeAt(APosition: Integer;
  InstructionCode: TThoriumInstructionCode; Param1, Param2, Param3: Int64;
  CodeLine: Cardinal = 0): Integer;
// Inserts a new instruction at a given position. The position is treated in a
// way which allows you to use it with the Position field as parameter and get
// the same result as if you would use GenCode directly.
var
  InstructionRec: PThoriumInstruction;
begin
  if APosition = FPosition then
  begin
    Result := GenCode(InstructionCode, Param1, Param2, Param3, CodeLine);
    Exit;
  end;
  if FCapacity < FCount + 1 then
    Expand;
  Inc(FCount);
  InstructionRec := PThoriumInstruction(ptrint(FInstructions)+APosition*SizeOf(TThoriumInstruction));
  Move(InstructionRec^,
    PThoriumInstruction(ptrint(InstructionRec)+SizeOf(TThoriumInstruction))^,
    SizeOf(TThoriumInstruction)*(FCount - APosition));
  InstructionRec^.Instruction := InstructionCode;
  InstructionRec^.Parameter1 := Param1;
  InstructionRec^.Parameter2 := Param2;
  InstructionRec^.Parameter3 := Param3;
  InstructionRec^.CodeLine := CodeLine;
  Result := APosition;
  Inc(FPosition);
end;     *)

procedure TThoriumInstructions.Finish;
// If there is more memory reserved than instructions were added, the reserved
// memory is reduced to the needed amount. This should be called after the
// compilation process.
// After that, iterate through the instructions and parse the special NOOP
// codes
var
  I: Integer;
  Instr: PThoriumInstructionNOOP;
//  Func: TThoriumFunction;
begin
  // Now we will iterate through the instructions to process the NOOP codes.
  I := 0;
  Instr := PThoriumInstructionNOOP(FInstructions);
  while I < FCount do
  begin
    if Instr^.Instruction = tiNOOP then
    begin
      case Instr^.Kind of
        THORIUM_NOOPMARK_PLACEHOLDER:
        begin
          DeleteInstructions(I, 1);
          Continue;
        end;
      end;
    end;
    Inc(Instr);
    Inc(I);
  end;

  if FCount < FCapacity then
  begin
    ReAllocMem(FInstructions, FCount * SizeOf(TThoriumInstruction));
    FCapacity := FCount;
  end;
end;

procedure TThoriumInstructions.ClearCode;
// Clears the whole code.
begin
  FreeMem(FInstructions);
  FInstructions := nil;
  FCapacity := 0;
  FCount := 0;
  FPosition := 0;
  FAddressLists.Clear;
  FAddressPointers.Clear;
end;

procedure TThoriumInstructions.RegisterAddressList(AList: TThoriumIntList);
begin
  if FAddressLists.IndexOf(AList) < 0 then
    FAddressLists.Add(AList);
end;

procedure TThoriumInstructions.UnRegisterAddressList(AList: TThoriumIntList);
var
  I: Integer;
begin
  I := FAddressLists.IndexOf(AList);
  if I >= 0 then
    FAddressLists.Delete(I);
end;

procedure TThoriumInstructions.AddInstructionPointer(APointer: PThoriumInstructionAddress);
begin
  if FAddressPointers.IndexOf(APointer) < 0 then
    FAddressPointers.Add(APointer);
end;

procedure TThoriumInstructions.RemoveInstructionPointer(APointer: PThoriumInstructionAddress);
var
  I: Integer;
begin
  I := FAddressPointers.IndexOf(APointer);
  if I >= 0 then
    FAddressPointers.Delete(I);
end;

procedure TThoriumInstructions.DumpCodeBin(DestStream: TStream);
// Writes the binary code to the stream
begin
  if FInstructions = nil then
    Exit;
  DestStream.Write(FInstructions^, FCount * SizeOf(TThoriumInstruction));
end;

function TThoriumInstructions.DumpCodeStr(ColorfulOutput: Boolean): String;
// Writes a half human readable code to the string

  function GetColorCode(A: Integer; B: Integer = -1; C: Integer = -1): String;
  begin
    if not ColorfulOutput then
      Exit('')
    else
      Exit(ColorCmd(A, B, C));
  end;

var
  I: Integer;
  CurrentInstruction: PThoriumInstruction;
  ColAddr, ColInstruction, ColInstructionHint, ColLine, CurInstruction, CurInstructionCol: String;
begin
  ColAddr := GetColorCode(1, 34);
  ColInstruction := GetColorCode(1, 37);
  ColInstructionHint := GetColorCode(0, 37);
  ColLine := GetColorCode(1, 33);
  Result := Format('%s %-9.9s %s  %-55.55s %s %9.9s'+GetColorCode(0), [ColAddr, 'Address', ColInstruction, 'Instruction', ColLine, 'LineNo.']) + LineEnding;
  CurrentInstruction := FInstructions;
  for I := 0 to FCount - 1 do
  begin
    if (I <> 0) then
      Result := Result + LineEnding;
    CurInstruction := ThoriumInstructionToStr(CurrentInstruction^);
    if CurInstruction[1] = '.' then
      CurInstructionCol := ColInstructionHint
    else
      CurInstructionCol := ColInstruction;
    Result := Result + Format('%s0x%.8x %s %-56.56s %s%10d%s', [
      ColAddr,
      I,
      CurInstructionCol,
      CurInstruction
      ,
      ColLine,
      CurrentInstruction^.CodeLine,
      GetColorCode(0)
    ]);
    Inc(CurrentInstruction);
  end;
end;

procedure TThoriumInstructions.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(Integer));
  SetCapacity(FCount);
  Stream.Read(FInstructions^, SizeOf(TThoriumInstruction) * FCount);
end;

procedure TThoriumInstructions.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(Integer));
  Stream.Write(FInstructions^, SizeOf(TThoriumInstruction) * FCount);
end;

procedure TThoriumInstructions.Assign(ASource: TThoriumInstructions);
begin
  ClearCode;
  SetCapacity(ASource.FCount);
  Move(ASource.FInstructions^, FInstructions^, SizeOf(TThoriumInstruction) * ASource.FCount);
  FCount := ASource.FCount;
  FPosition := ASource.FPosition;
  FInserted := ASource.FInserted;
  FSetPosition := ASource.FSetPosition;
end;

{%ENDREGION}

{%REGION 'Optimizer' /fold}

{ TThoriumCustomOptimizerPattern }

constructor TThoriumCustomOptimizerPattern.Create;
begin

end;

{ TThoriumOptimizer }

constructor TThoriumOptimizer.Create;
begin
  FPatterns := TThoriumOptimizerPatterns.Create;
end;

destructor TThoriumOptimizer.Destroy;
begin
  FPatterns.Free;
  inherited Destroy;
end;

function TThoriumOptimizer.DoHandleOffset(const Origin, Count: Integer): Boolean;
begin
  Result := False;
  if Count = 0 then
    Exit;
  if FOffsetCallback <> nil then
    FOffsetCallback(Self, Origin, Count, Result);
  if FJumpList <> nil then
  begin
    Result := True;
    FJumpList.ChangeAddresses(Count, Origin+1, FInstructions);
  end;
end;

function TThoriumOptimizer.AddPattern(
  const AClass: TThoriumOptimizerPatternClass): TThoriumCustomOptimizerPattern;
begin
  Result := AClass.Create;
  FPatterns.Add(Result);
end;

function TThoriumOptimizer.Optimize(AInstructions: TThoriumInstructions;
  OffsetCallback: TThoriumHandleOffset): Integer;
var
  Jumps: TThoriumJumpList;
  RootInstruction, Instruction: PThoriumInstruction;
  Remaining, Count: Integer;
  DoneSomething: Boolean;
  I, J: Integer;
  Offset: Integer;
begin
  Result := 0;
  Jumps := TThoriumJumpList.Create;
  try
    RootInstruction := AInstructions.FInstructions;
    Count := AInstructions.Count;
    Instruction := RootInstruction;
    for I := 0 to Count - 1 do
    begin
      if Instruction^.Instruction in [tiJMP, tiJE, tiJNE, tiJGE, tiJGT, tiJLE, tiJLT] then
        Jumps.AddEntry(I);
      Inc(Instruction);
    end;

    DoneSomething := True;
    while DoneSomething do
    begin
      DoneSomething := False;
      Instruction := RootInstruction;
      Remaining := Count;
      for I := 0 to Count - 1 do
      begin
        for J := 0 to FPatterns.Count - 1 do
        begin
          DoneSomething := FPatterns[J].Handle(Instruction, Remaining, Offset);
          if DoneSomething then
          begin
            Result += Offset;
            DoHandleOffset(I, Offset);
            Break;
          end;
        end;
        if DoneSomething then
          Break;
        Inc(Instruction);
        Dec(Remaining);
      end;
    end;
  finally
    Jumps.Free;
  end;
end;

{%ENDREGION}

{%REGION 'Compiler' /fold}

{ TThoriumCustomCompiler }

constructor TThoriumCustomCompiler.Create(ATarget: TThoriumModule);
begin
  FError:= False;
  FGlobalValueCount := @ATarget.FGlobalValueCount;
  FHostFuncUsage := ATarget.FHostFuncUsage;
  FHostFuncRelocations := ATarget.FHostFuncRelocations;
  FHostTypeUsage := ATarget.FHostTypeUsage;
  FHostTypeRelocations := ATarget.FHostTypeRelocations;
  FInstructions := ATarget.FInstructions;
  FLastError := '';
  FLibPropUsage := ATarget.FLibPropUsage;
  FLibPropRelocations := ATarget.FLibPropRelocations;
  FModule := ATarget;
  FOptimizedInstructions := ATarget.FOptimizedInstructions;
  FPrivateFunctions := ATarget.FPrivateFunctions;
  FPublicFunctions := ATarget.FPublicFunctions;
  FPublicVariables := ATarget.FPublicVariables;
  FRequiredModules := ATarget.FRequiredModules;
  FRequiredLibraries := ATarget.FRequiredLibraries;
  FStringLibrary := ATarget.FStringLibrary;
  FTable := TThoriumIdentifierTable.Create;
  FTableSizes := TThoriumIntStack.Create;
  FThorium := ATarget.FThorium;
  ResetState;
  {$ifdef HookSIGUSR1}
  SigCurrModule := FModule;
  SigCurrCompiler := Self;
  {$endif}
  FStoredTypes := TInterfaceList.Create;
  FBreakContexts := TThoriumCompilerBreakContextList.Create;
  FJumps := TThoriumIntList.Create;
  FInstructions.RegisterAddressList(FJumps);
end;

destructor TThoriumCustomCompiler.Destroy;
begin
  FInstructions.UnRegisterAddressList(FJumps);
  FJumps.Free;
  FBreakContexts.Free;
  FTableSizes.Free;
  FTable.Free;
  FStoredTypes.Clear;
  FStoredTypes.Free;
  inherited Destroy;
end;

function TThoriumCustomCompiler.GetTypeFloat: TThoriumTypeFloat;
begin
  Result := FThorium.FTypeFloat;
end;

function TThoriumCustomCompiler.GetTypeInteger: TThoriumTypeInteger;
begin
  Result := FThorium.FTypeInteger;
end;

function TThoriumCustomCompiler.GetTypeString: TThoriumTypeString;
begin
  Result := FThorium.FTypeString;
end;

function TThoriumCustomCompiler.IsRegisterInUse(AID: TThoriumRegisterID
  ): Boolean;
begin
  Result := (FRegisterUsage[AID div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (AID shl (AID mod THORIUM_REGISTER_MASK_BLOCK_SIZE)) <> 0);
end;

procedure TThoriumCustomCompiler.SetRegisterInUse(AID: TThoriumRegisterID;
  AInUse: Boolean);
var
  Item: Word;
begin
  Item := FRegisterUsage[AID div THORIUM_REGISTER_MASK_BLOCK_SIZE];
  if AInUse then
    Item := Item or ((1 shl (AID mod THORIUM_REGISTER_MASK_BLOCK_SIZE)))
  else
    Item := Item and not (1 shl (AID mod THORIUM_REGISTER_MASK_BLOCK_SIZE));
  FRegisterUsage[AID div THORIUM_REGISTER_MASK_BLOCK_SIZE] := Item;
end;

function TThoriumCustomCompiler.AddLibraryPropertyUsage(
  const AProp: TThoriumLibraryProperty): Integer;
begin
  Result := FLibPropUsage.IndexOf(AProp);
  if Result < 0 then
    Result := FLibPropUsage.Add(AProp);
end;

function TThoriumCustomCompiler.AddLibraryPropertyUsageEx(
  var TargetArray: TThoriumLibraryPropertyArray; AItem: TThoriumLibraryProperty
  ): Integer;
begin
  Result := Length(TargetArray);
  SetLength(TargetArray, Result+1);
  TargetArray[Result] := AItem;
end;

procedure TThoriumCustomCompiler.AddLibraryPropertyUsages(
  const AProp: TThoriumLibraryPropertyArray);
var
  I: Integer;
begin
  for I := 0 to High(AProp) do
    AddLibraryPropertyUsage(AProp[I]);
end;

function TThoriumCustomCompiler.AddLibraryPropertyUsageToRelocate(
  const AProp: TThoriumLibraryProperty; const AOffset: ptruint): Integer;
var
  Info: PThoriumRelocation;
begin
  Info := GetMem(SizeOf(TThoriumRelocation));
  Info^.ObjectIndex := AddLibraryPropertyUsage(AProp);
  Info^.ByteOffset := AOffset;
  Result := FLibPropRelocations.Add(Info);
end;

function TThoriumCustomCompiler.AddLibraryString(const AStr: String): Integer;
// Adds a string to the library, if it is not already in, otherwise return its
// index.
begin
  Result := FStringLibrary.IndexOf(AStr);
  if Result > -1 then
    Exit;
  Result := FStringLibrary.Add(AStr);
end;

function TThoriumCustomCompiler.AddHostTypeUsage(
  const AType: TThoriumHostObjectType): Integer;
begin
  Result := FHostTypeUsage.IndexOf(AType);
  if Result < 0 then
    Result := FHostTypeUsage.Add(AType);
end;

function TThoriumCustomCompiler.AddHostTypeUsageEx(
  var TargetArray: TThoriumHostObjectTypeArray; AItem: TThoriumHostObjectType
  ): Integer;
begin
  Result := Length(TargetArray);
  SetLength(TargetArray, Result+1);
  TargetArray[Result] := AItem;
end;

procedure TThoriumCustomCompiler.AddHostTypeUsages(
  const AUsageArray: TThoriumHostObjectTypeArray);
var
  I: Integer;
begin
  for I := 0 to High(AUsageArray) do
    AddHostTypeUsage(AUsageArray[I]);
end;

function TThoriumCustomCompiler.AddHostTypeUsageToRelocate(
  const AType: TThoriumHostObjectType; const AOffset: ptruint): Integer;
var
  Info: PThoriumRelocation;
begin
  Info := GetMem(SizeOf(TThoriumRelocation));
  Info^.ObjectIndex := AddHostTypeUsage(AType);
  Info^.ByteOffset := AOffset;
  Result := FHostTypeRelocations.Add(Info);
end;

function TThoriumCustomCompiler.AddHostFunctionUsage(
  const AFunc: TThoriumHostCallableBase): Integer;
begin
  Result := FHostFuncUsage.IndexOf(AFunc);
  if Result < 0 then
    Result := FHostFuncUsage.Add(AFunc);
end;

function TThoriumCustomCompiler.AddHostFunctionUsageToRelocate(
  const AFunc: TThoriumHostCallableBase; const AOffset: ptruint): Integer;
var
  Info: PThoriumRelocation;
begin
  Info := GetMem(SizeOf(TThoriumRelocation));
  Info^.ObjectIndex := AddHostFunctionUsage(AFunc);
  Info^.ByteOffset := AOffset;
  Result := FHostFuncRelocations.Add(Info);
end;

function TThoriumCustomCompiler.AddPublicVariable(AName: String): TThoriumVariable;
begin
  Result := TThoriumVariable.Create(FModule, AName);
  FPublicVariables.Add(Result);
end;

function TThoriumCustomCompiler.AppendCode(
  ACodeArray: TThoriumInstructionArray): Integer;
begin
  if FCodeHook then
  begin
    Result := Length(FCodeHook1^);
    SetLength(FCodeHook1^, Result + Length(ACodeArray));
    Move(ACodeArray[0], FCodeHook1^[Result], Length(ACodeArray)*SizeOf(TThoriumInstruction));
    if FCodeHook2 <> nil then
    begin
      Result := Length(FCodeHook2^);
      SetLength(FCodeHook2^, Result+Length(ACodeArray));
      Move(ACodeArray[0], FCodeHook2^[Result], Length(ACodeArray)*SizeOf(TThoriumInstruction));
    end;
    Result := -1;
  end
  else
    Result := FInstructions.AppendCode(ACodeArray);
end;

function TThoriumCustomCompiler.AppendCodeEx(
  const ASource: TThoriumInstructionArray; var ADest: TThoriumInstructionArray
  ): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(ASource) do
    Result := GenCodeEx(ADest, ASource[I]);
end;

function TThoriumCustomCompiler.AppendCodeToOperation(
  const ASource: TThoriumInstructionArray;
  var AOperations: TThoriumOperationArray): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(ASource) do
    Result := GenCodeToOperation(AOperations, ASource[I]);
end;

function TThoriumCustomCompiler.AppendOperations(
  AOperations: TThoriumOperationArray): Integer;
var
  I: Integer;
begin
  for I := 0 to High(AOperations) do
  begin
    case AOperations[I].Kind of
      okOperation:
        Result := GenOperation(AOperations[I].Operation, AOperations[I].TargetRI, AOperations[I].Value1RI, AOperations[I].Value2RI);
      okCustom:
        Result := AppendCode(AOperations[I].Custom.Instructions);
      okCreation:
        Result := GenCreation(AOperations[I].Creation, AOperations[I].TargetRI);
      //okAssignment:
      //  Result := genass
      //okCast:
      //  Result := gen
    else
      raise EThoriumCompilerException.CreateFmt('Cannot handle this kind of generic operation in AppendOperations: %s.', [GetEnumName(TypeInfo(TThoriumOperationDescriptionKind), Ord(AOperations[I].Kind))]);
    end;
    if (gorTarget in AOperations[I].ClearRegisters) then
      GenCode(clr(AOperations[I].TargetRI));
    if (gorValue1 in AOperations[I].ClearRegisters) then
      GenCode(clr(AOperations[I].Value1RI));
    if (gorValue2 in AOperations[I].ClearRegisters) then
      GenCode(clr(AOperations[I].Value2RI));
  end;
end;

procedure TThoriumCustomCompiler.AppendOperation(
  var AOperations: TThoriumOperationArray; AOperation: TThoriumGenericOperation
  );
var
  I: Integer;
begin
  I := Length(AOperations);
  SetLength(AOperations, I+1);
  AOperations[I] := AOperation;
end;

procedure TThoriumCustomCompiler.CompilerError(const Msg: String);
begin
  CompilerError(Msg, 0, 0);
end;

procedure TThoriumCustomCompiler.CompilerError(const Msg: String; X, Y: Integer);
begin
  if (X >= 0) and (Y >= 0) then
    FLastError := Format('%s(%d|%d): %s', [FModule.FName, Y, X, Msg])
  else
    FLastError := Format('%s: %s', [FModule.FName, Msg]);
  FError := True;
end;

procedure TThoriumCustomCompiler.ClaimRegister(const ARegID: TThoriumRegisterID
  );
begin
  if (ARegID >= THORIUM_REGISTER_RUNTIME_MIN)
     and (ARegID <= THORIUM_REGISTER_RUNTIME_MAX) then
  begin
    CompilerError('Thou shall not acuqire a runtime register.');
    Exit;
  end;
  if IsRegisterInUse(ARegID) then
  begin
    CompilerError('Manually claimed register already in use: '+ThoriumRegisterToStr(ARegID));
    Exit;
  end;
  SetRegisterInUse(ARegID, True);
end;

procedure TThoriumCustomCompiler.PopAndClearByTable(
  const TargetTableSize: Integer);
var
  StackCount, I: Integer;
  Ident: TThoriumTableEntry;
begin
  StackCount := 0;
  for I := FTable.Count - 1 downto TargetTableSize do
  begin
    FTable.ReadIdentifier(I, Ident);
    case Ident._Type of
      ttLocal, ttGlobal, ttParameter:
      begin
        Inc(StackCount);
      end;

      ttLocalRegisterVariable:
      begin
        if Ident.TypeSpec.NeedsClear then
          GenCode(clr(Ident.Offset));
      end;
    end;
  end;
  if StackCount > 0 then
    GenCode(pop_s(StackCount));
end;

procedure TThoriumCustomCompiler.DumpState;
begin

end;

procedure TThoriumCustomCompiler.EmbedHint(const S: String);
begin
  try
    GenCode(EmbeddedHint(S));
  except
    on E: EThoriumCompilerException do
    begin
      CompilerError(E.Message);
      Exit;
    end
    else
      raise;
  end;
end;

procedure TThoriumCustomCompiler.EmbedMetadata(const S: String);
var
  BlockCount: Integer;
  Block: String;
  I: Integer;
  Remain: Integer;
begin
  SetLength(Block, 20);
  DivMod(Length(S), 20, BlockCount, Remain);
  for I := 0 to BlockCount - 1 do
  begin
    Move(S[1 + I * 20], Block[1], 20);
    EmbedHint('$ '+Block);
  end;
  if Remain > 0 then
  begin
    FillByte(Block[1], 20, 0);
    Move(S[1 + BlockCount * 20], Block[1], Remain);
    EmbedHint('$ '+Block);
  end;
end;

(*function TThoriumCustomCompiler.FindTableEntry(const Ident: String; out
  Entry: TThoriumTableEntry; out Module: TThoriumModule; RaiseError: Boolean;
  AllowFar: Boolean): Boolean; inline;
// Searches all accessible tables for an value with the given identifier
var
  I: Integer;
  VarI: Integer;
  CurrVar: TThoriumVariable;
  CurrFunc: TThoriumFunction;
  CurrExternalFunc: TThoriumHostFunctionBase;
  CurrConst: TThoriumLibraryConstant;
  CurrProp: TThoriumLibraryProperty;
  List: PPointerList;
begin
  Module := nil;
  // First check the own module
  Result := FTable.FindIdentifier(Ident, Entry);
  // If we could not find anything in here and far access is allowed, we scan
  // for the included modules.
  if (not Result) and (AllowFar) and (FThorium <> nil) then
  begin
    for I := FRequiredModules.Count - 1 downto 0 do
    begin
      // First get the module index.
      Module := FRequiredModules.Items[I];
      // Check all function identifiers in the given module
      List := PPointerList(Module.FPublicFunctions.List);
      for VarI := Module.FPublicFunctions.Count - 1 downto 0 do
      begin
        // Get the function pointer
        CurrFunc := TThoriumFunction(List^[VarI]);
        // Check if it matches
        if CurrFunc.FName = Ident then
        begin
          // And if so fill the entry record and return.
          Entry.Name := nil;
          Entry.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
          Entry.Offset := -1;
          Entry._Type := ttGlobalCallable;
          Entry.TypeSpec := CurrFunc.PrototypeIntf;
          Entry.Value.Func := CurrFunc;
          Result := True;
          Exit;
        end;
      end;
      // Now check all variables of the module
      List := PPointerList(Module.FPublicVariables.List);
      for VarI := Module.FPublicVariables.Count - 1 downto 0 do
      begin
        // Get the variable pointer
        CurrVar := TThoriumVariable(List^[VarI]);
        // Check if the name matches
        if CurrVar.FName = Ident then
        begin
          // And if so fill the entry record and return.
          Entry.Name := nil;
          Entry.Scope := THORIUM_STACK_SCOPE_MODULEROOT;
          Entry.Offset := CurrVar.FStackPosition;
          Entry.TypeSpec := CurrVar.FTypeSpec;
          // The static field needs a special handling
          if CurrVar.FIsStatic then
            Entry._Type := etStatic
          else
            Entry._Type := etVariable;
          Result := True;
          Exit;
        end;
      end;
    end;
    // Now... No included module could serve us contents, so we look in the
    // external functions
    // Let it find us the function :P
    CurrExternalFunc := FModule.FindHostFunction(Ident); // FThorium.FExternalFunctionRegistry.FindFunctionDirect(Ident);
    if (CurrExternalFunc <> nil) then
    begin
      Entry.Name := nil;
      Entry.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
      Entry.Offset := -1;
      Entry._Type := ttHostCallable;
      Entry.TypeSpec := CurrExternalFunc.PrototypeIntf;
      Entry.Value.HostFunc := CurrExternalFunc;
      Result := True;
      Exit;
    end;
    CurrProp := FModule.FindLibraryProperty(Ident);
    if CurrProp <> nil then
    begin
      Entry.Name := nil;
      Entry.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
      Entry.Offset := -1;
      Entry._Type := ttLibraryProperty;
      Entry.TypeSpec := CurrProp.GetType;
      Entry.Ptr := CurrProp;
      Result := True;
      Exit;
    end;
    // Well, no function, no variable in a module, nothing, so it can only
    // be a constant then.
    CurrConst := FModule.FindLibraryConstant(Ident);
    if CurrConst <> nil then
    begin
      Entry.Name := nil;
      Entry.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
      Entry.Offset := -1;
      Entry._Type := ttLibraryConstant;
      Entry.Value := CurrConst.FValue;
      // Reimplement this
      raise Exception.Create('Re-implement ExtractTypeSpec or replace with appropriate surrogate.');
      //Entry.TypeSpec := ThoriumExtractTypeSpec(Entry.Value);
      Entry.Ptr := CurrConst;
      Result := True;
      Exit;
    end;
  end;
  // If we were not able to find anything and we are allowed to raise an error
  // do so.
  if (not Result) and (RaiseError) then
    CompilerError('Undeclared identifier: '+Ident);
end;         *)

procedure TThoriumCustomCompiler.FindTableEntries(const Ident: String;
  var Entries: TThoriumTableEntryResults);

  procedure ScanOwnSymbolTable;
  // Scan order:
  // Backwards, so that closer symbols are recognized before further away
  // symbols
  var
    Match: TThoriumTableEntryResult;
    I: Integer;
  begin
    Match.SourceLibrary := nil;
    Match.SourceModule := FModule;
    for I := FTable.Count - 1 downto 0 do
    begin
      FTable.ReadIdentifier(I, Match.Entry);
      if (Match.Entry.Name <> nil) and (Match.Entry.Name^ = Ident) then
        AppendTableEntry(Entries, Match);
    end;
  end;

  procedure ScanIncludedModule(const AModule: TThoriumModule);
  // Scan order:
  // * Public var table
  // * Public func table
  // * Public type table (TODO)
  var
    I: Integer;
    VarTable: TThoriumVariables;
    VarEntry: TThoriumVariable;
    FuncTable: TThoriumFunctions;
    FuncEntry: TThoriumFunction;
    Match: TThoriumTableEntryResult;
  begin
    Match.SourceLibrary := nil;
    Match.SourceModule := AModule;
    VarTable := AModule.FPublicVariables;
    for I := 0 to VarTable.Count - 1 do
    begin
      VarEntry := VarTable[I];
      if VarEntry.FName = Ident then
      begin
        with Match.Entry do
        begin
          Name := nil;
          Offset := VarEntry.FStackPosition;
          TypeSpec := VarEntry.FTypeSpec;
          _Type := ttGlobal;
        end;
        AppendTableEntry(Entries, Match);
        Break; // We can break here, since two variables with the same name in
               // the same module are not allowed.
      end;
    end;

    FuncTable := AModule.FPublicFunctions;
    for I := 0 to FuncTable.Count - 1 do
    begin
      FuncEntry := FuncTable[I];
      if FuncEntry.FName = Ident then
      begin
        with Match.Entry do
        begin
          _Type := ttGlobalCallable;
          Name := nil;
          TypeSpec := FuncEntry.FPrototype;
          Value.Func := FuncEntry;
        end;
        AppendTableEntry(Entries, Match);
      end;
    end;

    WriteLn('Public type table is not implemented yet, thus no identifiers are searched');
  end;

  procedure ScanIncludedLibrary(const ALibrary: TThoriumLibrary);
  begin
    ALibrary.LookupIdentifier(Ident, Entries);
  end;

// Search all accessible modules and contexts for entries matching the
// identifier Ident.
// Scan order:
// * Own module symbol table
// * Included module symbol tables
// * Included library symbol tables
// * Global symbol tables
var
  I: Integer;
begin
  ScanOwnSymbolTable;
  for I := 0 to FRequiredModules.Count - 1 do
    ScanIncludedModule(FRequiredModules[I]);
  for I := 0 to FRequiredLibraries.Count - 1 do
    ScanIncludedLibrary(FRequiredLibraries[I]);
end;

procedure TThoriumCustomCompiler.ForceNewCustomOperation(
  var OperationArray: TThoriumOperationArray);
var
  I: Integer;
begin
  if Length(OperationArray) = 0 then
    Exit;
  I := High(OperationArray);
  if OperationArray[I].Kind = okCustom then
  begin
    SetLength(OperationArray, I+2);
    with OperationArray[I+1] do
    begin
      Kind := okCustom;
      TargetRI := THORIUM_REGISTER_INVALID;
      Value1RI := THORIUM_REGISTER_INVALID;
      Value2RI := THORIUM_REGISTER_INVALID;
    end;
  end;
end;

function TThoriumCustomCompiler.GenBreak: Integer;
var
  StackCount, I: Integer;
  Ctx: TThoriumCompilerBreakContext;
  Ident: TThoriumTableEntry;
begin
  Ctx := GetBreakContext;
  if Ctx = nil then
  begin
    CompilerError('Nothing to break here.');
    Exit(THORIUM_JMP_INVALID);
  end;

  PopAndClearByTable(Ctx.FTableTarget);
  Result := GenCode(jmp(THORIUM_JMP_INVALID));
  Ctx.JumpList.AddEntry(Result);
end;

function TThoriumCustomCompiler.GenCode(AInstruction: TThoriumInstruction; ACodeLine: Cardinal): Integer;
begin
  if FCodeHook then
  begin
    Result := Length(FCodeHook1^);
    SetLength(FCodeHook1^, Result+1);
    Move(AInstruction, FCodeHook1^[Result], SizeOf(TThoriumInstruction));
    FCodeHook1^[Result].CodeLine := ACodeLine;
    if FCodeHook2 <> nil then
    begin
      Result := Length(FCodeHook2^);
      SetLength(FCodeHook2^, Result+1);
      FCodeHook2^[Result] := FCodeHook1^[Result];
    end;
  end
  else
  begin
    AInstruction.CodeLine := ACodeLine;
    Result := FInstructions.AppendCode(AInstruction);
    if AInstruction.Instruction in THORIUM_JMP_INSTRUCTIONS then
      FJumps.AddEntry(Result);
  end;
end;

function TThoriumCustomCompiler.GenCodeEx(
  var TargetArray: TThoriumInstructionArray; AInstruction: TThoriumInstruction;
  CodeLine: Integer): Integer;
begin
  Result := Length(TargetArray);
  SetLength(TargetArray, Result+1);
  TargetArray[Result] := AInstruction;
  TargetArray[Result].CodeLine := CodeLine;
end;

function TThoriumCustomCompiler.GenCodeToOperation(
  var OperationArray: TThoriumOperationArray; AInstruction: TThoriumInstruction
  ): Integer;
var
  I: Integer;
begin
  I := High(OperationArray);
  if (I < 0) or (OperationArray[I].Kind <> okCustom) then
  begin
    Inc(I);
    SetLength(OperationArray, I+1);
    with OperationArray[I] do
    begin
      Kind := okCustom;
      TargetRI := THORIUM_REGISTER_INVALID;
      Value1RI := THORIUM_REGISTER_INVALID;
      Value2RI := THORIUM_REGISTER_INVALID;
    end;
  end;
  Result := GenCodeEx(OperationArray[I].Custom.Instructions, AInstruction);
end;

function TThoriumCustomCompiler.GenCreation(
  AOperation: TThoriumCreateInstructionDescription; const ATargetRI: Word
  ): Integer;
var
  Instruction: TThoriumInstructionREG;
begin
  Instruction := AOperation.Instruction;
  if AOperation.TargetRegisterOffset >= 0 then
    Instruction.Reserved[AOperation.TargetRegisterOffset] := ATargetRI;
  Result := GenCode(TThoriumInstruction(Instruction));
end;

function TThoriumCustomCompiler.GenOperation(
  AOperation: TThoriumOperationDescription; const ATargetRI: Word;
  const AValue1RI: Word; const AValue2RI: Word): Integer;
var
  Instruction: TThoriumOperationInstructionDescription;
  Cast: TThoriumInstructionCAST;
begin
  ClaimRegister(THORIUM_REGISTER_C1);
  ClaimRegister(THORIUM_REGISTER_C2);
  Instruction := AOperation.OperationInstruction;
  if Instruction.Value1RIOffset >= 0 then
    Instruction.Instruction.Reserved[Instruction.Value1RIOffset] := AValue1RI;
  if Instruction.Value2RIOffset >= 0 then
    Instruction.Instruction.Reserved[Instruction.Value2RIOffset] := AValue2RI;
  if Instruction.TargetRIOffset >= 0 then
    Instruction.Instruction.Reserved[Instruction.TargetRIOffset] := ATargetRI;
  if AOperation.Casts[0].Needed then
  begin
    Cast := AOperation.Casts[0].Instruction;
    Cast.SRI := AValue1RI;
    Cast.TRI := THORIUM_REGISTER_C1;
    GenCode(TThoriumInstruction(Cast));
    Instruction.Instruction.Reserved[Instruction.Value1RIOffset] := THORIUM_REGISTER_C1;
  end;
  if AOperation.Casts[1].Needed then
  begin
    Cast := AOperation.Casts[1].Instruction;
    Cast.SRI := AValue2RI;
    Cast.TRI := THORIUM_REGISTER_C2;
    GenCode(TThoriumInstruction(Cast));
    Instruction.Instruction.Reserved[Instruction.Value2RIOffset] := THORIUM_REGISTER_C2;
  end;
  Result := GenCode(TThoriumInstruction(Instruction.Instruction));
  ReleaseRegister(THORIUM_REGISTER_C1);
  ReleaseRegister(THORIUM_REGISTER_C2);
end;

function TThoriumCustomCompiler.GetBreakContext: TThoriumCompilerBreakContext;
begin
  if FBreakContexts.Count = 0 then
    Exit(nil)
  else
    Exit(FBreakContexts[FBreakContexts.Count - 1]);
end;

function TThoriumCustomCompiler.GetCurrentTableStackPos: Integer;
begin
  Result := FTableSizes.Count;
end;

function TThoriumCustomCompiler.GetFreeRegister(Kind: TThoriumRegisterKind; out
  RegisterID: TThoriumRegisterID; ThrowError: Boolean): Boolean;
var
  I: Word;
  Min, Max: Word;
begin
  Result := False;
  case Kind of
    trC:
    begin
      Min := THORIUM_REGISTER_C_MIN;
      Max := THORIUM_REGISTER_C_MAX;
    end;
    trEXP:
    begin
      Min := THORIUM_REGISTER_EXP_MIN;
      Max := THORIUM_REGISTER_EXP_MAX;
    end;
  else
    raise EThoriumCompilerException.CreateFmt('Invalid register kind: %d', [Ord(Kind)]);
  end;
  for I := Min to Max do
  begin
    if not (FRegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE)) <> 0) then
    begin
      Result := True;
      RegisterID := I;
      FRegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] := FRegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] xor ((FRegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE))) xor (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE)));
      Exit;
    end;
  end;
  if ThrowError then
  begin
    case Kind of
      trC: CompilerError('Internal error: Compiler dynamically uses CACHE-registers.');
      trEXP: CompilerError('Need more expression registers (currently '+IntToStr(THORIUM_REGISTER_EXP_COUNT)+'). Recompile compiler with more registers or try to crop down your expressions.');
    end;
  end;
end;

function TThoriumCustomCompiler.GetHighestRegisterInUse: TThoriumRegisterID;
var
  I: TThoriumRegisterID;
begin
  Result := THORIUM_REGISTER_C_MAX;
  for I := THORIUM_REGISTER_EXP_MIN to THORIUM_REGISTER_EXP_MAX do
  begin
    if (FRegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE)) <> 0) then
      Result := I;
  end;
end;

function TThoriumCustomCompiler.GetHookedInstructionPointerA(AIndex: Integer
  ): PThoriumInstruction;
begin
  Result := @FCodeHook1^[AIndex];
end;

function TThoriumCustomCompiler.GetHookedInstructionPointerB(AIndex: Integer
  ): PThoriumInstruction;
begin
  Result := @FCodeHook2^[AIndex];
end;

function TThoriumCustomCompiler.GetInstruction(
  Address: TThoriumInstructionAddress): PThoriumInstruction;
begin
  if FCodeHook then
  begin
    if FCodeHook2 <> nil then
    begin
      CompilerError('Direct access to instructions in dual code hook mode not allowed.');
      Exit(nil);
    end;
    Exit(@FCodeHook1^[Address]);
  end
  else
    Exit(FInstructions.Instruction[Address]);
end;

function TThoriumCustomCompiler.GetNextInstructionAddress: TThoriumInstructionAddress;
begin
  if FCodeHook then
    Result := Length(FCodeHook1^)
  else
    Result := FInstructions.Position;
end;

function TThoriumCustomCompiler.GetTableEntriesTo(StackPos: Integer): Integer;
begin
  Result := FTable.Count - FTableSizes.Items[StackPos-1];
end;

procedure TThoriumCustomCompiler.FindRelocationTargets;
var
  Instruction: PThoriumInstruction;
  I: Integer;
begin
  Instruction := FInstructions.Instruction[0];
  for I := 0 to FInstructions.Count - 1 do
  begin
    case Instruction^.Instruction of
      tiEXT, tiEXT_S:
      begin
        AddHostTypeUsageToRelocate(TThoriumHostObjectType(TThoriumInstructionEXT(Instruction^).ExtendedType), I * SizeOf(TThoriumInstruction) + SizeOf(TThoriumInstructionCode));
      end;
      tiXFNC:
      begin
        AddHostFunctionUsageToRelocate(TThoriumHostFunctionBase(TThoriumInstructionXFNC(Instruction^).FunctionRef), I * SizeOf(TThoriumInstruction) + SizeOf(TThoriumInstructionCode));
      end;
      tiXMETH:
      begin
        AddHostFunctionUsageToRelocate(TThoriumHostFunctionBase(TThoriumInstructionXMETH(Instruction^).MethodRef), I * SizeOf(TThoriumInstruction) + SizeOf(TThoriumInstructionCode));
      end;
      tiXPGET:
      begin
        AddLibraryPropertyUsageToRelocate(TThoriumLibraryProperty(TThoriumInstructionXPGET(Instruction^).Prop), I * SizeOf(TThoriumInstruction) + SizeOf(TThoriumInstructionCode));
      end;
      tiXPSET:
      begin
        AddLibraryPropertyUsageToRelocate(TThoriumLibraryProperty(TThoriumInstructionXPSET(Instruction^).Prop), I * SizeOf(TThoriumInstruction) + SizeOf(TThoriumInstructionCode));
      end;
    end;
    Inc(Instruction);
  end;
end;

function TThoriumCustomCompiler.HasError: Boolean;
begin
  Result := FError;
end;

procedure TThoriumCustomCompiler.LoadLibrary(const LibName: String);
var
  Lib: TThoriumLibrary;
begin
  if FThorium <> nil then
  begin
    Lib := FThorium.FindLibrary(LibName);
    if Lib = nil then
    begin
      CompilerError('Could not load library "'+LibName+'". Library not found.');
      Exit;
    end;
    if FRequiredLibraries.IndexOf(Lib) < 0 then
      FRequiredLibraries.Add(Lib);
  end
  else
    CompilerError('Could not load library "'+LibName+'". No Thorium context available.');
end;

procedure TThoriumCustomCompiler.LoadModule(const ModName: String);
var
  Module: TThoriumModule;
begin
  if FThorium <> nil then
  begin
    Module := FThorium.FindModule(ModName);
    if Module = nil then
    begin
      CompilerError('Could not load module "'+ModName+'". Module not found.');
      Exit;
    end;
    if not Module.Compiled then
    begin
      CompilerError('Could not load module "'+ModName+'". Circular module reference (or threading problem).');
      Exit;
    end;
    if FRequiredModules.IndexOf(Module) < 0 then
      FRequiredModules.Add(Module);
  end
  else
    CompilerError('Could not load module "'+ModName+'". No Thorium context available.');
end;

procedure TThoriumCustomCompiler.OptimizeCode;

var
  Instruction: PThoriumInstruction;
  Jumps: TThoriumJumpList;
  I: Integer;
  Remaining: Integer;
  DoneSomething: Boolean;

  (*function Opt_Expression_1_4: Integer;
  // Optimizes following this pattern:
  //   %iE %rA %**G
  //   %iF %rB %**H
  //   "mov" %rA %rC
  //   "mov" %rB %rD
  //
  //   %E %C %G
  //   %F %D %H
  type
    TThoriumInstructionSet1 = record
      InstructionCode: TThoriumInstructionCode;
      RegID: Word;
      Other: array [0..10] of Word;
      CodeLine: Cardinal;
    end;
    TThoriumInstructionSet2 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..2] of Word;
      RegID: Word;
      Other2: array [0..7] of Word;
      CodeLine: Cardinal;
    end;
    TThoriumInstructionSet3 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..1] of Word;
      RegID: Word;
      Other2: array [0..8] of Word;
      CodeLine: Cardinal;
    end;
  const
    CompatibleInstructions = [tiREG_I, tiREG_E, tiREG_F, tiREG_L, tiMUL_R, tiADD_R, tiCMP_R, tiEXT_R, tiLIB_S];
    Set1 = [tiREG_I, tiREG_E, tiREG_F];
    Set2 = [tiMUL_R, tiADD_R, tiCMP_R, tiEXT_R, tiREG_L];
    Set3 = [tiLIB_S];
  var
    Ass1, Ass2, Mov1, Mov2: PThoriumInstruction;
    Reg1, Reg2: TThoriumRegisterID;
  begin
    Result := 0;
    Ass1 := @Instruction[0];
    Ass2 := @Instruction[1];
    Mov1 := @Instruction[2];
    Mov2 := @Instruction[3];
    if not ((Ass1^.Instruction in CompatibleInstructions) and
      (Ass2^.Instruction in CompatibleInstructions) and
      (Mov1^.Instruction = tiMOV) and
      (Mov2^.Instruction = tiMOV)) then
      Exit;
    if Ass1^.Instruction in Set1 then
      Reg1 := TThoriumInstructionSet1(Ass1^).RegID
    else if Ass1^.Instruction in Set2 then
      Reg1 := TThoriumInstructionSet2(Ass1^).RegID
    else
      Reg1 := TThoriumInstructionSet3(Ass1^).RegID;
    if Ass2^.Instruction in Set1 then
      Reg2 := TThoriumInstructionSet1(Ass2^).RegID
    else if Ass2^.Instruction in Set2 then
      Reg2 := TThoriumInstructionSet2(Ass2^).RegID
    else
      Reg2 := TThoriumInstructionSet3(Ass2^).RegID;
    if ((Reg1 = TThoriumInstructionMOV(Mov1^).SRI) and (Reg2 = TThoriumInstructionMOV(Mov2^).SRI)) then
    begin
      Result := -2;
      DoneSomething := True;
      if Ass1^.Instruction in Set1 then
        TThoriumInstructionSet1(Ass1^).RegID := TThoriumInstructionMOV(Mov1^).TRI
      else if Ass1^.Instruction in Set2 then
        TThoriumInstructionSet2(Ass1^).RegID := TThoriumInstructionMOV(Mov1^).TRI
      else
        TThoriumInstructionSet3(Ass1^).RegID := TThoriumInstructionMOV(Mov1^).TRI;
      if Ass2^.Instruction in Set1 then
        TThoriumInstructionSet1(Ass2^).RegID := TThoriumInstructionMOV(Mov2^).TRI
      else if Ass2^.Instruction in Set2 then
        TThoriumInstructionSet2(Ass2^).RegID := TThoriumInstructionMOV(Mov2^).TRI
      else
        TThoriumInstructionSet3(Ass2^).RegID := TThoriumInstructionMOV(Mov2^).TRI;
      FInstructions.DeleteInstructions(I + 2, 2);
    end;
  end;

  function Opt_Expression_2_3: Integer;
  // Optimizes following this pattern:
  //   %iF %rB %**H
  //   "mov" %rA %rC
  //   "mov" %rB %rD
  //
  //   %F %D %H
  //   "mov" %A %C
  type
    TThoriumInstructionSet1 = record
      InstructionCode: TThoriumInstructionCode;
      RegID: Word;
      Other: array [0..10] of Word;
      CodeLine: Cardinal;
    end;
    TThoriumInstructionSet2 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..2] of Word;
      RegID: Word;
      Other2: array [0..7] of Word;
      CodeLine: Cardinal;
    end;
    TThoriumInstructionSet3 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..1] of Word;
      RegID: Word;
      Other2: array [0..8] of Word;
      CodeLine: Cardinal;
    end;
  const
    CompatibleInstructions = [tiREG_I, tiREG_E, tiREG_F, tiREG_L, tiMUL_R, tiADD_R, tiCMP_R, tiEXT_R, tiLIB_S];
    Set1 = [tiREG_I, tiREG_E, tiREG_F];
    Set2 = [tiMUL_R, tiADD_R, tiCMP_R, tiEXT_R, tiREG_L];
    Set3 = [tiLIB_S];
  var
    Ass1, Mov1, Mov2: PThoriumInstruction;
    Reg1: TThoriumRegisterID;
  begin
    Result := 0;
    Ass1 := @Instruction[0];
    Mov1 := @Instruction[1];
    Mov2 := @Instruction[2];
    if not ((Ass1^.Instruction in CompatibleInstructions) and
      (Mov1^.Instruction = tiMOV) and
      (Mov2^.Instruction = tiMOV)) then
      Exit;
    if Ass1^.Instruction in Set1 then
      Reg1 := TThoriumInstructionSet1(Ass1^).RegID
    else if Ass1^.Instruction in Set2 then
      Reg1 := TThoriumInstructionSet2(Ass1^).RegID
    else
      Reg1 := TThoriumInstructionSet3(Ass1^).RegID;
    if (Reg1 = TThoriumInstructionMOV(Mov2^).SRI) then
    begin
      Result := -1;
      DoneSomething := True;
      if Ass1^.Instruction in Set1 then
        TThoriumInstructionSet1(Ass1^).RegID := TThoriumInstructionMOV(Mov2^).TRI
      else if Ass1^.Instruction in Set2 then
        TThoriumInstructionSet2(Ass1^).RegID := TThoriumInstructionMOV(Mov2^).TRI
      else
        TThoriumInstructionSet3(Ass1^).RegID := TThoriumInstructionMOV(Mov2^).TRI;
      FInstructions.DeleteInstructions(I + 2, 1);
    end;
  end;   *)

  function UsesRegister(const AInstruction: PThoriumInstruction; ARegister: Word): Boolean;
  type
    {$PACKRECORDS 2}
    TThoriumInstructionSet1 = record
      InstructionCode: TThoriumInstructionCode;
      RegID: Word;
      Other1: array [0..10] of Word;
      CodeLine: Cardinal;
    end;

    TThoriumInstructionSet2 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: Word;
      RegID: Word;
      Other2: array [0..9] of Word;
      CodeLine: Cardinal;
    end;

    TThoriumInstructionSet3 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..1] of Word;
      RegID: Word;
      Other2: array [0..8] of Word;
      CodeLine: Cardinal;
    end;

    TThoriumInstructionSet4 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..2] of Word;
      RegID: Word;
      Other2: array [0..7] of Word;
      CodeLine: Cardinal;
    end;

    TThoriumInstructionSet5 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..3] of Word;
      RegID: Word;
      Other2: array [0..6] of Word;
      CodeLine: Cardinal;
    end;

    TThoriumInstructionSet6 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..4] of Word;
      RegID: Word;
      Other2: array [0..5] of Word;
      CodeLine: Cardinal;
    end;
    {$PACKRECORDS DEFAULT}
  const
    InstructionSet1 = [tiINTB, tiSTR, tiCOPYR_ST,
      tiCOPYR, tiMOVER_ST, tiMOVEST, tiCLR, tiCMPI, tiCMPIF,
      tiCMPIE, tiCMPF, tiCMPFI, tiCMPFE, tiCMPS, tiCMPSE, tiCMPE, tiCMPEI,
      tiCMPEF, tiCMPES, tiADDI, tiADDF, tiADDS, tiSUBI, tiSUBF, tiMULI,
      tiMULF, tiDIVI, tiDIVF, tiMOD, tiAND, tiOR, tiXOR, tiSHL, tiSHR,
      tiXIGET, tiXCT];
    InstructionSet2 = [tiCOPYR, tiMOVER, tiCMPI, tiCMPIF,
      tiCMPIE, tiCMPF, tiCMPFI, tiCMPFE, tiCMPS, tiCMPSE, tiCMPE, tiCMPEI,
      tiCMPEF, tiCMPES, tiADDI, tiADDF, tiADDS, tiSUBI, tiSUBF, tiMULI,
      tiMULF, tiDIVI, tiDIVF, tiMOD, tiAND, tiOR, tiXOR, tiSHL, tiSHR];
    InstructionSet3 = [tiSTRL, tiADDI, tiADDF, tiADDS, tiSUBI, tiSUBF, tiMULI,
      tiMULF, tiDIVI, tiDIVF, tiMOD, tiAND, tiOR, tiXOR, tiSHL, tiSHR];
    InstructionSet4 = [];
    InstructionSet5 = [tiINT, tiFLT, tiEXT, tiFNC, tiXFNC, tiXFGET,
      tiXFSET];
    InstructionSet6 = [tiXFSET, tiXFGET];
  begin
    Result := (AInstruction^.Instruction in InstructionSet1)
      and (TThoriumInstructionSet1(AInstruction^).RegID = ARegister);
    if Result then
      Exit;
    Result := (AInstruction^.Instruction in InstructionSet2)
      and (TThoriumInstructionSet2(AInstruction^).RegID = ARegister);
    if Result then
      Exit;
    Result := (AInstruction^.Instruction in InstructionSet3)
      and (TThoriumInstructionSet3(AInstruction^).RegID = ARegister);
    if Result then
      Exit;
    Result := (AInstruction^.Instruction in InstructionSet4)
      and (TThoriumInstructionSet4(AInstruction^).RegID = ARegister);
    if Result then
      Exit;
    Result := (AInstruction^.Instruction in InstructionSet5)
      and (TThoriumInstructionSet5(AInstruction^).RegID = ARegister);
    if Result then
      Exit;
    Result := (AInstruction^.Instruction in InstructionSet6)
      and (TThoriumInstructionSet6(AInstruction^).RegID = ARegister);
  end;

  function Opt_Expression_1_x: Integer;
  // Looks for a cast on a value which has been created just before and converts
  // it hard coded to the correct type.
  var
    V1, Cast1: PThoriumInstruction;
    Reg: Word;
    Offset: Integer;
  begin
    Result := 0;
    V1 := Instruction;
    if V1^.Instruction <> tiINT then
      Exit;
    Reg := TThoriumInstructionINT(V1^).TRI;
    Offset := 1;
    while (Offset < Remaining) do
    begin
      Cast1 := @Instruction[Offset];
      if Cast1^.Instruction = tiCASTIF then
      begin
        TThoriumInstructionINT(V1^).TRI := TThoriumInstructionCASTIF(Cast1^).TRI;
        TThoriumInstructionINT(V1^).Instruction := tiFLT;
        TThoriumInstructionFLT(V1^).Value := TThoriumInstructionINT(V1^).Value;
        FInstructions.DeleteInstructions(I + Offset, 1);
        Result := -1;
        Break;
      end
      else if UsesRegister(Cast1, Reg) then
        Break;
      Inc(Offset);
    end;
  end;

  function Opt_Expression_2_2: Integer;
  // When a variable is created to a register and pushed to stack top after
  // this, this gets optimized by this.
  var
    V, Push: PThoriumInstruction;
  begin
    Result := 0;
    V := Instruction;
    if not (V^.Instruction in [tiSTRL, tiINT, tiSTR, tiFLT]) then
      Exit;
    Push := @Instruction[1];
    if Push^.Instruction <> tiMOVER_ST then
      Exit;
    if (TThoriumInstructionINT(V^).TRI <> TThoriumInstructionMOVER_ST(Push^).SRI) then
      Exit;
    case V^.Instruction of
      tiSTRL:
        V^.Instruction := tiSTRL_S;
      tiSTR:
        V^.Instruction := tiSTR_S;
      tiINT:
        V^.Instruction := tiINT_S;
      tiFLT:
        V^.Instruction := tiFLT_S;
    end;
    FInstructions.DeleteInstructions(I+1, 1);
    Result -= 1;
  end;

  function Opt_Expression_4_2: Integer;
  // When a value is moved to a register from stack and pushed to stack top
  // immediately after this, this gets erased by this function.
  var
    Copy, Move: PThoriumInstruction;
  begin
    Result := 0;
    Copy := Instruction;
    Move := @Instruction[1];
    if (Copy^.Instruction <> tiMOVEST) or (Move^.Instruction <> tiMOVER_ST) then
      Exit;
    if (TThoriumInstructionMOVEST(Copy^).TRI = TThoriumInstructionMOVER_ST(Move^).SRI) then
    begin
      FInstructions.DeleteInstructions(I, 2);
      Result -= 2;
    end;
  end;

  function Opt_RegVar_1_2: Integer;
  // The compiler is at some places not really prepared to use register
  // variables. So these functions are made to fix it ;)
  var
    Copy, Compare: PThoriumInstruction;
  begin
    Result := 0;
    Copy := Instruction;
    Compare := @Instruction[1];
    if (Copy^.Instruction <> tiCOPYR) or not (Compare^.Instruction in [tiCMPE, tiCMPEF, tiCMPEI, tiCMPES, tiCMPF, tiCMPFE, tiCMPFI, tiCMPI, tiCMPIE, tiCMPIF, tiCMPS, tiCMPSE]) then
      Exit;
    if (TThoriumInstructionCOPYR(Copy^).TRI = TThoriumInstructionCMPI(Compare^).Op1) then
    begin
      TThoriumInstructionCMPI(Compare^).Op1 := TThoriumInstructionCOPYR(Copy^).SRI;
      FInstructions.DeleteInstructions(I, 1);
      Result -= 1;
    end
    else if (TThoriumInstructionCOPYR(Copy^).TRI = TThoriumInstructionCMPI(Compare^).Op2) then
    begin
      TThoriumInstructionCMPI(Compare^).Op2 := TThoriumInstructionCOPYR(Copy^).SRI;
      FInstructions.DeleteInstructions(I, 1);
      Result -= 1;
    end;
  end;

  function Opt_RegVar_2_x: Integer;
  // The compiler is at some places not really prepared to use register
  // variables. So these functions are made to fix it ;)
  var
    Copy, Op: PThoriumInstruction;
    Reg: Word;
    Offset: Integer;
  begin
    Result := 0;
    Copy := Instruction;
    if Copy^.Instruction <> tiCOPYR then
      Exit;
    Reg := TThoriumInstructionCOPYR(Copy^).TRI;
    Offset := 1;
    while (Offset < Remaining) do
    begin
      Op := @Instruction[Offset];
      if Op^.Instruction in [tiCMPI, tiCMPIF, tiCMPIE, tiCMPF, tiCMPFI,
        tiCMPFE, tiCMPS, tiCMPSE, tiCMPE, tiCMPEI, tiCMPEF, tiCMPES, tiADDI,
        tiADDF, tiADDS, tiSUBI, tiSUBF, tiMULI, tiMULF, tiDIVI, tiDIVF, tiMOD,
        tiAND, tiOR, tiXOR, tiSHL, tiSHR] then
      begin
        if (TThoriumInstructionOperator(Op^).Op1 = Reg) then
        begin
          TThoriumInstructionOperator(Op^).Op1 := TThoriumInstructionCOPYR(Copy^).SRI;
          FInstructions.DeleteInstructions(I, 1);
          Result -= 1;
          Exit;
        end
        else if (TThoriumInstructionOperator(Op^).Op2 = Reg) then
        begin
          TThoriumInstructionOperator(Op^).Op2 := TThoriumInstructionCOPYR(Copy^).SRI;
          FInstructions.DeleteInstructions(I, 1);
          Result -= 1;
          Exit
        end;
      end
      else if UsesRegister(Op, Reg) then
        Break;
      Inc(Offset);
    end;
  end;

  (*function Opt_Expression_1_2: Integer;
  // Optimizes the creation of variables which are casted to another type
  // immediately after this.
  type
    TThoriumInstructionSet1 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..3] of Word;
      RegID: Word;
      Other2: array [0..6] of Word;
      CodeLine: Cardinal;
    end;

    TThoriumInstructionCAST = record
      Instruction: TThoriumInstructionCode;
      SRI: Word;
      TRI: Word;
      Reserved: array [0..9] of Word;
      // Debug infos
      CodeLine: Cardinal;
    end;
  var
    V1, Cast: PThoriumInstruction;
    Reg: Word;
    Match: Boolean;
  begin
    Result := 0;
    V1 := @Instruction[0];
    Cast := @Instruction[1];
    if not (V1^.Instruction in [tiINT]) then
      Exit;

    if (Cast^.Instruction in [tiCASTIF]) and (TThoriumInstructionCASTIF(Cast^).SRI = TThoriumInstructionSet1(V1^).RegID) then
    begin
      V1^.Instruction := tiFLT;
      TThoriumInstructionFLT(V1^).Value := TThoriumInstructionINT(V1^).Value;
      TThoriumInstructionSet1(V1^).RegID := TThoriumInstructionCAST(Cast^).TRI;
      Result := -1;
      FInstructions.DeleteInstructions(I+1, 1);
    end;
  end;

  function Opt_Expression_1_4: Integer;
  // Optimizes the creation of variables which are casted to another type
  // immediately after this.
  type
    TThoriumInstructionSet1 = record
      InstructionCode: TThoriumInstructionCode;
      Other1: array [0..3] of Word;
      RegID: Word;
      Other2: array [0..6] of Word;
      CodeLine: Cardinal;
    end;

    TThoriumInstructionCAST = record
      Instruction: TThoriumInstructionCode;
      SRI: Word;
      TRI: Word;
      Reserved: array [0..9] of Word;
      // Debug infos
      CodeLine: Cardinal;
    end;
  var
    V1, Cast1, V2, Cast2: PThoriumInstruction;
    Reg: Word;
    Match: Boolean;
  begin
    Result := 0;
    V1 := @Instruction[0];
    V2 := @Instruction[1];
    Cast1 := @Instruction[2];
    Cast2 := @Instruction[3];
    if not (V1^.Instruction in [tiINT]) and not (V2^.Instruction in [tiINT]) then
      Exit;

    if (V1^.Instruction in [tiINT]) and (Cast1^.Instruction in [tiCASTIF]) and (TThoriumInstructionCASTIF(Cast1^).SRI = TThoriumInstructionSet1(V1^).RegID) then
    begin
      V1^.Instruction := tiFLT;
      TThoriumInstructionFLT(V1^).Value := TThoriumInstructionINT(V1^).Value;
      TThoriumInstructionSet1(V1^).RegID := TThoriumInstructionCAST(Cast1^).TRI;
      FInstructions.DeleteInstructions(I+1, 1);
      Result += -1;
    end;

    if (V2^.Instruction in [tiINT]) and (Cast2^.Instruction in [tiCASTIF]) and (TThoriumInstructionCASTIF(Cast2^).SRI = TThoriumInstructionSet1(V2^).RegID) then
    begin
      V2^.Instruction := tiFLT;
      TThoriumInstructionFLT(V2^).Value := TThoriumInstructionINT(V2^).Value;
      TThoriumInstructionSet1(V2^).RegID := TThoriumInstructionCAST(Cast2^).TRI;
      FInstructions.DeleteInstructions(I+1+Result, 1);
      Result += -1;
    end;
  end;      *)

  function Opt_Jump_1: Integer;
  // Looks for jumps which lead directly to the next instruction
  var
    Jmp: PThoriumInstruction;
  begin
    Result := 0;
    Jmp := @Instruction[0];
    if not (Jmp^.Instruction in [tiJMP, tiJE, tiJGE, tiJLE, tiJLT, tiJGT, tiJNE]) then
      Exit;
    if (TThoriumInstructionJMP(Jmp^).NewAddress = I+1) then
    begin
      FInstructions.DeleteInstructions(I, 1);
      Jumps.DeleteEntry(Jumps.FindValue(I));
      Result -= 1;
    end;
  end;

  function Opt_Jump_2: Integer;
  // Look for a jump which leads to a jump.
  var
    Jmp, Jmp2: PThoriumInstruction;
  begin
    Result := 0;
    Jmp := @Instruction[0];
    if not (Jmp^.Instruction in [tiJMP, tiJE, tiJGE, tiJLE, tiJLT, tiJGT, tiJNE]) then
      Exit;
    if (LongInt(TThoriumInstructionJMP(Jmp^).NewAddress) < 0) then
      Exit;
    Jmp2 := @Instruction[TThoriumInstructionJMP(Jmp^).NewAddress];
    if Jmp2^.Instruction = tiJMP then
      TThoriumInstructionJMP(Jmp^).NewAddress := TThoriumInstructionJMP(Jmp2^).NewAddress;
  end;

  function Opt_Jump_3: Integer;
  // Look for a nonconditional jump which leads to a ret.
  var
    Jmp, Ret: PThoriumInstruction;
  begin
    Result := 0;
    Jmp := @Instruction[0];
    if not (Jmp^.Instruction in [tiJMP]) then
      Exit;
    if (LongInt(TThoriumInstructionJMP(Jmp^).NewAddress) < 0) then
      Exit;
    Ret := @Instruction[TThoriumInstructionJMP(Jmp^).NewAddress];
    if Ret^.Instruction = tiRET then
    begin
      Jmp^.Instruction := Ret^.Instruction;
      Jmp^.Parameter1 := Ret^.Parameter1;
      Jmp^.Parameter2 := Ret^.Parameter2;
      Jmp^.Parameter3 := Ret^.Parameter3;
    end;
  end;

  function Opt_call_d_1_2x: Integer;
  // Writes call.d instructions where it is possible.
  var
    Call, Pop, Ret: PThoriumInstruction;
  begin
    Result := 0;
    Call := Instruction;
    Pop := @Instruction[1];
    if Pop^.Instruction = tiRET then
    begin
      Ret := @Instruction[1];
      Pop := nil;
    end
    else
    begin
      if Remaining < 3 then
        Exit;
      Ret := @Instruction[2];
    end;
    if (Call^.Instruction <> tiCALL) or (Ret^.Instruction <> tiRET) or ((Pop <> nil) and (Pop^.Instruction <> tiPOP_S)) then
      Exit;
    Call^.Instruction := tiCALL_D;
    TThoriumInstructionCALL_D(Call^).KeepResult := 0;
    if Pop <> nil then
    begin
      TThoriumInstructionCALL_D(Call^).Pops := TThoriumInstructionPOP_S(Pop^).Amount;
      FInstructions.DeleteInstructions(I+1, 2);
      Result -= 2;
    end
    else
    begin
      TThoriumInstructionCALL_D(Call^).Pops := 0;
      FInstructions.DeleteInstructions(I+1, 1);
      Result -= 1;
    end;
  end;

  function HandleOffset(Offset: Integer): Boolean;
  var
    I2: Integer;
    Func: TThoriumFunction;
  begin
    FOptimizedInstructions -= Offset;
    Result := False;
    if Offset = 0 then
      Exit;
    Result := True;
    DoneSomething := True;
    Jumps.ChangeAddresses(Offset, I+1, FInstructions);
    for I2 := 0 to FPublicFunctions.Count - 1 do
    begin
      Func := FPublicFunctions[I2];
      if Func.FEntryPoint >= I+1 then
        Func.FEntryPoint := Func.FEntryPoint + Offset;
    end;
  end;

begin
  FOptimizedInstructions := 0;
  Jumps := TThoriumJumpList.Create;
  try
    Instruction := FInstructions.FInstructions;
    for I := 0 to FInstructions.Count - 1 do
    begin
      //if Instruction^.Instruction in [tiJMP, tiJMP_F, tiJMP_T, tiCALL, tiCALL_F] then
      if Instruction^.Instruction in [tiJMP, tiJE, tiJNE, tiJGE, tiJGT, tiJLE, tiJLT, tiCALL] then
        Jumps.AddEntry(I);
      Inc(Instruction);
    end;

    DoneSomething := True;
    while DoneSomething do
    begin
      DoneSomething := False;
      Instruction := FInstructions.FInstructions;
      Remaining := FInstructions.Count;
      for I := 0 to FInstructions.Count - 1 do
      begin
        if (Remaining >= 2) then
          HandleOffset(Opt_Expression_1_x);
        if DoneSomething then Break;
        if (Remaining >= 2) then
          HandleOffset(Opt_Expression_2_2);
        if DoneSomething then Break;
        if (Remaining >= 2) then
          HandleOffset(Opt_Expression_4_2);
        if DoneSomething then Break;
        if (Remaining >= 1) then
          HandleOffset(Opt_Jump_1);
        if DoneSomething then Break;
        if (Remaining >= 1) then
          HandleOffset(Opt_Jump_2);
        if DoneSomething then Break;
        if (Remaining >= 1) then
          HandleOffset(Opt_Jump_3);
        if DoneSomething then Break;
        if (Remaining >= 2) then
          HandleOffset(Opt_RegVar_1_2);
        if DoneSomething then Break;
        if (Remaining >= 2) then
          HandleOffset(Opt_RegVar_2_x);
        if DoneSomething then Break;
        //if (Remaining >= 2) then
        //  HandleOffset(Opt_call_d_1_2x);
        //if DoneSomething then Break;
        (*if (Remaining >= 4) then
        begin
          HandleOffset(Opt_Expression_1_4);
        end;
        if (Remaining >= 3) then
        begin
          HandleOffset(Opt_Expression_2_3);
        end;*)
        Inc(Instruction);
        Dec(Remaining);
      end;
    end;
  finally
    Jumps.Free;
    FModule.FOptimizedInstructions := FOptimizedInstructions;

  end;
end;

function TThoriumCustomCompiler.PopBreakContext: TThoriumIntList;
var
  I: Integer;
  Ctx: TThoriumCompilerBreakContext;
begin
  I := FBreakContexts.Count;
  if I = 0 then
  begin
    CompilerError('Break context stack underflow.');
    Exit(nil);
  end
  else
  begin
    Dec(I);
    Ctx := FBreakContexts[I];
    Result := Ctx.JumpList;
    Ctx.Free;
    FBreakContexts.Delete(I);
  end;
end;

procedure TThoriumCustomCompiler.PushBreakContext;
var
  Context: TThoriumCompilerBreakContext;
begin
  Context := TThoriumCompilerBreakContext.Create;
  Context.JumpList := TThoriumIntList.Create;
  Context.TableTarget := FTable.Count;
  FBreakContexts.Add(Context);
end;

procedure TThoriumCustomCompiler.ReleaseRegister(ID: TThoriumRegisterID);
begin
  FRegisterUsage[ID div THORIUM_REGISTER_MASK_BLOCK_SIZE] := FRegisterUsage[ID div THORIUM_REGISTER_MASK_BLOCK_SIZE] xor (FRegisterUsage[ID div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (ID mod THORIUM_REGISTER_MASK_BLOCK_SIZE)));
end;

procedure TThoriumCustomCompiler.ResetState;
begin
  FillByte(FRegisterUsage, SizeOf(TThoriumRegisterMask), 0);
end;

procedure TThoriumCustomCompiler.RestoreTable(var Offset: Integer;
  GenerateCode: Boolean);
begin
  // Pop the entry off the table stack height
  Offset := FTableSizes.Pop;
  // Generate the code to free the values referred to by the table stack entries
  if GenerateCode then
    PopAndClearByTable(Offset);
  FTable.ClearTableTo(Offset);
end;

procedure TThoriumCustomCompiler.SaveTable;
begin
  FTableSizes.Push(FTable.Count);
end;

procedure TThoriumCustomCompiler.SetupFunction(AFunction: TThoriumFunction;
  const AEntryPoint: Integer; const AName: String);
begin
  AFunction.FEntryPoint := AEntryPoint;
  AFunction.FName := AName;
end;

{%ENDREGION}

{%REGION 'Module' /fold}
{ TThoriumModule }

constructor TThoriumModule.Create(AThorium: TThorium);
begin
  inherited Create;
  FCompiled := False;
  FCompress := {$ifdef CompressByDefault}True{$else}False{$endif};
  FillByte(FHash, SizeOf(TThoriumHash), 0);
  FHashGenerated := False;
  FHostFuncUsage := TFPList.Create;
  FHostFuncRelocations := TFPList.Create;
  FHostTypeUsage := TFPList.Create;
  FHostTypeRelocations := TFPList.Create;
  FInstructions := TThoriumInstructions.Create;
  FLastCompilerError := '';
  FLibPropUsage := TFPList.Create;
  FLibPropRelocations := TFPList.Create;
  FName := '';
  FOptimizedInstructions := 0;
  FPrivateFunctions := TThoriumFunctions.Create;
  FPublicFunctions := TThoriumFunctions.Create;
  FPublicVariables := TThoriumVariables.Create;
  FRequiredModules := TThoriumModules.Create;
  FRequiredLibraries := TThoriumLibraries.Create;
  FSourceFile := '';
  FillByte(FSourceHash, SizeOf(TThoriumHash), 0);
  FSourceLength := 0;
  FStringLibrary := TStringList.Create;
  FThorium := AThorium;
  FGlobalValueCount := 0;
end;

constructor TThoriumModule.Create(AThorium: TThorium; AName: String);
begin
  Create(AThorium);
  FName := AName;
end;

destructor TThoriumModule.Destroy;
begin
  ClearAll;
  FRequiredModules.Free;
  FRequiredLibraries.Free;
  FPrivateFunctions.Free;
  FPublicFunctions.Free;
  FPublicVariables.Free;
  FInstructions.Free;
  FStringLibrary.Free;
  FHostTypeUsage.Free;
  FHostTypeRelocations.Free;
  FHostFuncUsage.Free;
  FHostFuncRelocations.Free;
  FLibPropUsage.Free;
  FLibPropRelocations.Free;
  inherited Destroy;
end;

function TThoriumModule.AddLibraryPropertyUsage(
  const AProp: TThoriumLibraryProperty): Integer;
begin
  raise EThoriumException.Create('TThorumModule.AddLibraryPropetyUsage is not re-implemented yet.');
end;

function TThoriumModule.AddLibraryPropertyUsageToRelocate(
  const AProp: TThoriumLibraryProperty; const AOffset: ptruint): Integer;
var
  Info: PThoriumRelocation;
begin
  Info := GetMem(SizeOf(TThoriumRelocation));
  Info^.ObjectIndex := AddLibraryPropertyUsage(AProp);
  Info^.ByteOffset := AOffset;
  Result := FLibPropRelocations.Add(Info);
end;

function TThoriumModule.AddLibraryString(const AStr: String): Integer;
// Adds a string to the library, if it is not already in, otherwise return its
// index.
begin
  Result := FStringLibrary.IndexOf(AStr);
  if Result > -1 then
    Exit;
  Result := FStringLibrary.Add(AStr);
end;

function TThoriumModule.AddHostTypeUsage(const AType: TThoriumHostObjectType
  ): Integer;
begin
  Result := FHostTypeUsage.IndexOf(AType);
  if Result < 0 then
    Result := FHostTypeUsage.Add(AType);
end;

function TThoriumModule.AddHostTypeUsageToRelocate(const AType: TThoriumHostObjectType;
  const AOffset: ptruint): Integer;
var
  Info: PThoriumRelocation;
begin
  Info := GetMem(SizeOf(TThoriumRelocation));
  Info^.ObjectIndex := AddHostTypeUsage(AType);
  Info^.ByteOffset := AOffset;
  Result := FHostTypeRelocations.Add(Info);
end;

function TThoriumModule.AddHostFunctionUsage(const AFunc: TThoriumHostCallableBase): Integer;
begin
  Result := FHostFuncUsage.IndexOf(AFunc);
  if Result < 0 then
    Result := FHostFuncUsage.Add(AFunc);
end;

function TThoriumModule.AddHostFunctionUsageToRelocate(const AFunc: TThoriumHostCallableBase;
  const AOffset: ptruint): Integer;
var
  Info: PThoriumRelocation;
begin
  Info := GetMem(SizeOf(TThoriumRelocation));
  Info^.ObjectIndex := AddHostFunctionUsage(AFunc);
  Info^.ByteOffset := AOffset;
  Result := FHostFuncRelocations.Add(Info);
end;

procedure TThoriumModule.ClearAll;
// Clears all relevant informations.
var
  I: Integer;
begin
  FSourceFile := '';
  FInstructions.ClearCode;
  FStringLibrary.Clear;
  for I := 0 to FPublicVariables.Count - 1 do
    TThoriumVariable(FPublicVariables[I]).Free;
  FPublicVariables.Clear;
  for I := 0 to FPublicFunctions.Count - 1 do
    TThoriumFunction(FPublicFunctions[I]).Free;
  FPublicFunctions.Clear;
  for I := 0 to FPrivateFunctions.Count - 1 do
    TThoriumFunction(FPrivateFunctions[I]).Free;
  FPrivateFunctions.Clear;
  FHostTypeUsage.Clear;
  for I := 0 to FHostTypeRelocations.Count - 1 do
    FreeMem(FHostTypeRelocations[I]);
  FHostTypeRelocations.Clear;
  FHostFuncUsage.Clear;
  for I := 0 to FHostFuncRelocations.Count - 1 do
    FreeMem(FHostFuncRelocations[I]);
  FHostFuncRelocations.Clear;
  FLibPropUsage.Clear;
  for I := 0 to FLibPropRelocations.Count - 1 do
    FreeMem(FLibPropRelocations[I]);
  FLibPropRelocations.Clear;
  FRequiredModules.Count := 0;
  FRequiredLibraries.Count := 0;
  FCompiled := False;
  FHashGenerated := False;
end;

function TThoriumModule.GetLibraryString(Index: Integer): String;
// Gets the string from the library at Index.
begin
  Result := FStringLibrary[Index];
end;

function TThoriumModule.GetLibraryStringCount: Integer;
// Gets the count of strings saved in the library.
begin
  Result := FStringLibrary.Count;
end;

function TThoriumModule.GetInstructionCount: Integer;
// Returns the number of instructions the module has
begin
  Result := FInstructions.Count;
end;

function TThoriumModule.GetPublicFunction(AIndex: Integer): TThoriumFunction;
begin
  Result := TThoriumFunction(FPublicFunctions[AIndex]);
end;

function TThoriumModule.GetPublicFunctionCount: Integer;
begin
  Result := FPublicFunctions.Count;
end;

function TThoriumModule.GetPublicVariable(AIndex: Integer): TThoriumVariable;
begin
  Result := TThoriumVariable(FPublicVariables[AIndex]);
end;

function TThoriumModule.GetPublicVariableCount: Integer;
begin
  Result := FPublicVariables.Count;
end;


procedure TThoriumModule.CalcHash;
var
  Buffer: PThoriumModuleHashBase;
begin
  if not FCompiled then
    raise EThoriumException.Create('Cannot calculate hash of an uncompiled module.');
  Buffer := GetMem(SizeOf(TThoriumModuleHashBase));
  with Buffer^ do
  begin
    FileVersion := THORIUM_FILE_VERSION;
    RequireCount := FRequiredModules.Count;
    FunctionExportCount := FPublicFunctions.Count;
    VariableExportCount := FPublicVariables.Count;
    StringCount := FStringLibrary.Count;
    HostTypeDependencyCount := FHostTypeUsage.Count;
    HostTypeRelocationCount := FHostTypeRelocations.Count;
    HostFuncDependencyCount := FHostFuncUsage.Count;
    HostFuncRelocationCount := FHostFuncRelocations.Count;
    LibPropDependencyCount := FLibPropUsage.Count;
    LibPropRelocationCount := FLibPropRelocations.Count;
    InstructionCount := FInstructions.Count;
    RandomFactor := Random(High(Cardinal));
    InstructionHash := TThoriumHash(MD5Buffer(FInstructions.Instruction[0]^, SizeOf(TThoriumInstruction)*FInstructions.Count));
  end;
  FHash := TThoriumHash(MD5Buffer(Buffer^, SizeOf(TThoriumModuleHashBase)));
  FreeMem(Buffer);
end;

procedure TThoriumModule.FillHeader(out Header: TThoriumModuleHeader);
begin
  Header.ID := THORIUM_MODULE_HEADER_ID;
  Header.FileVersion := THORIUM_FILE_VERSION;
  Header.HeaderSize := SizeOf(TThoriumModuleHeader);
  Header.SourceFileNameLength := 0;
  Header.RequireCount := FRequiredModules.Count;
  Header.LibraryCount := FRequiredLibraries.Count;
  Header.FunctionExportCount := FPublicFunctions.Count;
  Header.VariableExportCount := FPublicVariables.Count;
  Header.StringCount := FStringLibrary.Count;
  Header.HostTypeDependencyCount := FHostTypeUsage.Count;
  Header.HostTypeRelocationCount := FHostTypeRelocations.Count;
  Header.HostFuncDependencyCount := FHostFuncUsage.Count;
  Header.HostFuncRelocationCount := FHostFuncRelocations.Count;
  Header.LibPropDependencyCount := FLibPropUsage.Count;
  Header.LibPropRelocationCount := FLibPropRelocations.Count;
  Header.Hash := GetHash;
  Header.SourceHash := FSourceHash;
  Header.SourceLength := FSourceLength;
  {$ifdef AllowCompression}
  if FCompress then
    Header.ZippedContent := 1
  else
    Header.ZippedContent := 0;
  {$else}
  Header.ZippedContent := 0;
  {$endif}
end;

procedure TThoriumModule.InternalLoadFromStream(Stream: TStream;
  const Header: TThoriumModuleHeader);
begin
  raise Exception.Create('Re-implement TThoriumModule.InternalLoadFromStream');
end;

procedure TThoriumModule.InternalSaveToStream(Stream: TStream;
  const Header: TThoriumModuleHeader);
begin
  raise Exception.Create('Re-implement TThoriumModule.InternalSaveToStream');
end;

function TThoriumModule.CompileFromStream(SourceStream: TStream;
  ACompiler: TThoriumCompilerClass; Flags: TThoriumCompilerFlags): Boolean;
var
  Compiler: TThoriumCustomCompiler;
begin
  Result := False;
  FCompiled := False;
  Compiler := ACompiler.Create(Self);
  try
    Result := Compiler.CompileFromStream(SourceStream, Flags);
    if not Result then
      FLastCompilerError := Compiler.FLastError
    else
      FCompiled := True;
  finally
    Compiler.Free;
    if not Result then
      ClearAll;
  end;
end;

procedure TThoriumModule.Dump(ColorfulOutput: Boolean);

  function GetColorCode(A: Integer; B: Integer = -1; C: Integer = -1): String;
  begin
    if not ColorfulOutput then
      Exit('')
    else
      Exit(ColorCmd(A, B, C));
  end;

var
  I, J: Integer;
  TypeSpec: TThoriumType;
  Callable: TThoriumHostCallableBase;
begin
  WriteLn('Module: ', GetColorCode(1), FName, GetColorCode(0));
  WriteLn('Code (', FOptimizedInstructions, ' removed by optimization): ');
  WriteLn(DumpCodeStr(ColorfulOutput));
  //WriteLn('Optimized instructions: ', FOptimizedInstructions);
  if FPublicFunctions.Count > 0 then
  begin;
    //WriteLn('Exported functions: ');
    WriteLn(GetColorCode(1), ' EntryAddr   Header', GetColorCode(0));
    for I := 0 to FPublicFunctions.Count - 1 do
    begin
      with TThoriumFunction(FPublicFunctions[I]) do
      begin
        if not Prototype.HasReturnValue then
          Write(GetColorCode(34), '0x', IntToHex(EntryPoint, 8), GetColorCode(0), '  void ')
        else
        begin
          TypeSpec := Prototype.ReturnType;
          Write(GetColorCode(34), '0x', IntToHex(EntryPoint, 8), GetColorCode(0), '  ', TypeSpec.Name, ' ');
        end;
        Write(Name, '(');
        for J := 0 to Prototype.Parameters.Count - 1 do
        begin
          if (J <> 0) then
            Write(', ');
          Prototype.Parameters.GetParameterSpec(J, TypeSpec);
          Write(TypeSpec.Name);
        end;
        WriteLn(')');
      end;
    end;
  end;
  if FPublicVariables.Count > 0 then
  begin
    //WriteLn('Exported variables:');
    WriteLn(GetColorCode(1), ' StackAddr   Flag    Name', GetColorCode(0));
    for I := 0 to FPublicVariables.Count - 1 do
    begin
      with TThoriumVariable(FPublicVariables[I]) do
      begin
        if IsStatic then
          WriteLn(GetColorCode(32), '0x', IntToHex(StackPosition, 8), GetColorCode(0), '  static  ', TypeSpec.Name, ' ', Name)
        else
          WriteLn(GetColorCode(32), '0x', IntToHex(StackPosition, 8), GetColorCode(0), '          ', TypeSpec.Name, ' ', Name);
      end;
    end;
  end;
  if FStringLibrary.Count > 0 then
  begin
    //WriteLn('Strings:');
    WriteLn(GetColorCode(1), ' Index       String', GetColorCode(0));
    for I := 0 to FStringLibrary.Count - 1 do
      WriteLn(GetColorCode(35), '0x', IntToHex(I, 8), GetColorCode(0), '  "', StringReplace(StringReplace(FStringLibrary[I], #10, '\n', [rfReplaceAll]), '"', '\"', [rfReplaceAll]), '"');
  end;
  if FRequiredLibraries.Count > 0 then
  begin
    WriteLn(GetColorCode(1), ' Index       Library identifier', GetColorCode(0));
    for I := 0 to FRequiredLibraries.Count - 1 do
    begin
      WriteLn(GetColorCode(2), '0x', IntToHex(I, 8), GetColorCode(0), '  ', FRequiredLibraries[I].GetName);
    end;
  end;
  if FHostTypeUsage.Count > 0 then
  begin
    //WriteLn('Extended type usage:');
    WriteLn(GetColorCode(1), ' Index       Flg   Type name', GetColorCode(0));
    for I := 0 to FHostTypeUsage.Count - 1 do
    begin
      if TThoriumHostObjectType(FHostTypeUsage[I]) is TThoriumRTTIObjectType then
        WriteLn(GetColorCode(2), '0x', IntToHex(I, 8), GetColorCode(0), '  RTTI  ', TThoriumRTTIObjectType(FHostTypeUsage[I]).BaseClass.ClassName)
      else
        WriteLn(GetColorCode(2), '0x', IntToHex(I, 8), GetColorCode(0), '        ', TThoriumHostObjectType(FHostTypeUsage[I]).ClassName);
    end;
    if FHostTypeRelocations.Count > 0 then
    begin
      WriteLn(GetColorCode(1), ' UsByteOff   Index');
      for I := 0 to FHostTypeRelocations.Count - 1 do
      begin
        with TThoriumRelocation(FHostTypeRelocations[I]^) do
          WriteLn(GetColorCode(31), '0x', IntToHex(ByteOffset, 8), GetColorCode(0), '  ', GetColorCode(2), '0x', IntToHex(ObjectIndex, 8), GetColorCode(0));
      end;
    end;
  end;
  if FHostFuncUsage.Count > 0 then
  begin
    WriteLn(GetColorCode(1), ' Index       Name', GetColorCode(0));
    for I := 0 to FHostFuncUsage.Count - 1 do
    begin
      Write(GetColorCode(2), '0x', IntToHex(I, 8), GetColorCode(0), '  ');
      Callable := TThoriumHostCallableBase(FHostFuncUsage[I]);
      if Callable is TThoriumHostMethodBase then
      begin
        if TThoriumHostMethodBase(Callable).FHostObjectType is TThoriumRTTIObjectType then
          WriteLn(TThoriumRTTIObjectType(TThoriumHostMethodBase(Callable).FHostObjectType).FBaseClass.ClassName, '.', Callable.Name)
        else
          WriteLn(TThoriumHostMethodBase(Callable).FHostObjectType.ClassName, '.', Callable.Name);
      end
      else
      begin
        WriteLn(Callable.Name);
      end;
    end;
    if FHostFuncRelocations.Count > 0 then
    begin
      WriteLn(GetColorCode(1), ' UsByteOff   Index', GetColorCode(0));
      for I := 0 to FHostFuncRelocations.Count - 1 do
        with PThoriumRelocation(FHostFuncRelocations[I])^ do
        begin
          WriteLn(GetColorCode(31), '0x', IntToHex(ByteOffset, 8), GetColorCode(0), '  ', GetColorCode(2), '0x', IntToHex(ObjectIndex, 8), GetColorCode(0));
        end;
    end;
  end;
  if FRequiredModules.Count > 0 then
  begin
    WriteLn(GetColorCode(1), ' Index       Module name', GetColorCode(0));
    for I := 0 to FRequiredModules.Count - 1 do
    begin
      WriteLn(GetColorCode(2), '0x', IntToHex(I, 8), GetColorCode(0), '  ', TThoriumModule(FRequiredModules.Items[I]).Name);
    end;
  end;
end;

function TThoriumModule.DumpCodeStr(ColorfulOutput: Boolean): String;
// Encapsulation for TThoriumInstructions.DumpCodeStr
begin
  Result := FInstructions.DumpCodeStr(ColorfulOutput);
end;

function TThoriumModule.DumpLibStr: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FStringLibrary.Count - 1 do
  begin
    Result := Result + '$'+IntToHex(I, SizeOf(Integer)*2)+': '+FStringLibrary[I]+LineEnding;
  end;
end;

procedure TThoriumModule.ExecuteMain;
// Executes the main function of the module.
begin

end;

function TThoriumModule.EncloseHostValue(const AValue: Pointer;
  const AType: PTypeInfo): TThoriumValue;
var
  I: Integer;
  TypeSpec: TThoriumType;
begin
  TypeSpec := nil;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    TypeSpec := FRequiredLibraries[I].FindTypeForHostType(AType);
    if TypeSpec <> nil then
      Break;
  end;
  if TypeSpec = nil then
    raise EThoriumRuntimeException.Create('Cannot enclose type.');
  Result := TypeSpec.CreateValueFromPtr(AValue);
end;

function TThoriumModule.FindPublicFunction(const AName: String
  ): TThoriumFunction;
var
  I: Integer;
begin
  for I := 0 to FPublicFunctions.Count - 1 do
  begin
    Result := FPublicFunctions[I];
    if Result.FName = AName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumModule.IndexOfPublicFunction(const AName: String): Integer;
var
  I: Integer;
begin
  for I := 0 to FPublicFunctions.Count - 1 do
    if FPublicFunctions[I].FName = AName then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TThoriumModule.LoadFromStream(Stream: TStream);
var
  Header: TThoriumModuleHeader;
  {$ifdef AllowCompression}
  Decompressor: Tdecompressionstream;
  {$endif}
begin
  FillByte(Header, SizeOf(TThoriumModuleHeader), 0);
  Stream.Read(Header, SizeOf(TThoriumModuleHeader));
  if Header.ID <> THORIUM_MODULE_HEADER_ID then
    raise EThoriumVerificationException.Create('This is not a compiled Thorium Scripting Language Module or the file is corrupted.');
  if Header.FileVersion <> THORIUM_FILE_VERSION then
  begin
    // wanted to have a try on recompiling, but I think this is not neccessary
    // the engine will handle this in the LoadFromFile thing.
    raise EThoriumVerificationException.Create('Incompatible file format version.');
  end;
  if Header.ZippedContent = 1 then
  begin
    {$ifndef AllowCompression}
    raise EThoriumVerificationException.Create('Compressed module but no compression support enabled ($define AllowCompression).');
    {$else}
    Decompressor := Tdecompressionstream.create(Stream);
    try
      ClearAll;
      InternalLoadFromStream(Decompressor, Header);
    finally
      Decompressor.Free;
    end;
    {$endif}
  end
  else
  begin
    ClearAll;
    InternalLoadFromStream(Stream, Header);
  end;
  FCompiled := True;
  FHashGenerated := True;
end;

procedure TThoriumModule.SaveToStream(Stream: TStream);
// Save the complete module to a stream
var
  Header: TThoriumModuleHeader;
  {$ifdef AllowCompression}
  Compressor: Tcompressionstream;
  {$endif}
begin
  if not Compiled then
    raise EThoriumException.Create('Cannot save a module which is not compiled.');
  FillHeader(Header);
  Stream.Write(Header, SizeOf(TThoriumModuleHeader));
  if Header.ZippedContent = 1 then
  begin
    {$ifndef AllowCompression}
    raise EThoriumVerificationException.Create('Module requested compression but no compression support enabled ($define AllowCompression).');
    {$else}
    Compressor := Tcompressionstream.create(clmax, Stream);
    try
      InternalSaveToStream(Compressor, Header);
    finally
      Compressor.Free;
    end;
    {$endif}
  end
  else
  begin
    InternalSaveToStream(Stream, Header);
  end;
end;
{%ENDREGION}

{%REGION 'Virtual machine & utilities' /fold}
{ TThoriumStack }

constructor TThoriumStack.Create;
begin
  inherited Create;
  FCapacity := 8;
  FCount := 0;
  GetMem(FEntries, SizeOf(TThoriumStackEntry) * 8);
  FTop := FEntries;
  Dec(FTop);
end;

destructor TThoriumStack.Destroy;
begin
  ClearStack;
  FreeMem(FEntries, SizeOf(TThoriumStackEntry) * FCapacity);
  inherited Destroy;
end;

procedure TThoriumStack.Expand;
// Expands the stack
begin
  //if FCapacity < 512 then
    SetCapacity(FCapacity shl 1)
  //else
  //  SetCapacity(FCapacity + 256);
end;

function TThoriumStack.GetStackEntry(ScopeRoot, Index: Integer): PThoriumStackEntry;
// Gets the stack entry at the given offset (calculated by adding ScopeRoot to
// Index). This does not perform any range check for performance issues.
begin
  if ScopeRoot = THORIUM_STACK_SCOPE_FROMTOP then
  begin
    Result := GetTop(Index+1);
    Exit;
  end;
  Result := PThoriumStackEntry(ptruint(FEntries) + Cardinal(ScopeRoot + Index)*SizeOf(TThoriumStackEntry));
end;

procedure TThoriumStack.SetCapacity(NewCapacity: Integer);
// Increases the size of the stack. You cannot decrease the stack by calling
// this, but you can decrease the amount of used memory (for empty stack slots).
begin
  if (NewCapacity < FCount) then
    Exit;
  ReAllocMem(FEntries, NewCapacity * SizeOf(TThoriumStackEntry));
  FTop := PThoriumStackEntry(ptruint(FEntries) + (FCount-1) * SizeOf(TThoriumStackEntry));
  FCapacity := NewCapacity;
end;

function TThoriumStack.FastGetStackEntry(ScopeRoot, Index: Integer): PThoriumStackEntry;
// Gets the stack entry at the given offset (calculated by adding ScopeRoot to
// Index). This does not perform any range check for performance issues.
begin
  Result := PThoriumStackEntry(ptruint(FEntries) + Cardinal(ScopeRoot + Index)*SizeOf(TThoriumStackEntry));
end;

function TThoriumStack.GetTopStackEntry: PThoriumStackEntry; inline;
// The fastest possible method to get the current top stack entry.
begin
  Result := FTop;
end;

function TThoriumStack.GetTop(Offset: Integer = 0): PThoriumStackEntry;
// Gets the item on the top or the item which is Offset items below it. This
// does no checks again.
begin
  Result := PThoriumStackEntry(ptruint(FTop) - Offset * SizeOf(TThoriumStackEntry));
end;

function TThoriumStack.Prealloc: PThoriumStackEntry;
// Just allocates a stack entry and initializes it as a nil value.
begin
  Result := GetMem(SizeOf(TThoriumStackEntry));
  (*FillByte(Result^, SizeOf(TThoriumStackEntry), 0);
  Result^._Type := stValue;
  Result^.Value._Type := vtBuiltIn;
  Result^.Value.BuiltIn._Type := btNil;*)
end;

function TThoriumStack.Push: PThoriumStackEntry;
// Gets a empty stack slot on the top (if there is not enough space, the stack
// will expand) and return the pointer to it (declared as nil)
begin
  if FCount = FCapacity then
    Expand;
  // DevNote: If this changes, change also the code of the other push function.
  Inc(FTop);
  Result := FTop;
  
  //FillDWord(Result^, SizeOf(TThoriumStackEntry), 0);
  //Result^._Type := stValue;
  //Result^.Value._Type := vtBuiltIn;
  //Result^.Value.BuiltIn._Type := btNil;
  Inc(FCount);
end;

procedure TThoriumStack.Push(AEntry: PThoriumStackEntry);
// This expects a pointer to an element allocated with Prealloc (it does not
// make a difference really if you allocate it yourself or not). It will JUST
// COPY the contents of the given structure to a new stack slot on the top and
// FREE the given structure afterwards.
begin
  if FCount = FCapacity then
    Expand;
  Inc(FTop);
  Move(AEntry^, FTop^, SizeOf(TThoriumStackEntry));
  Inc(FCount);
  FreeMem(AEntry, SizeOf(TThoriumStackEntry));
end;

procedure TThoriumStack.Pop(Amount: Integer; FreeValues: Boolean = True);
// Removes Amount items from the stack. If Amount is negative, the stack height
// is set to the absolute value.
// Any value removed will be freed.
var
  Idx: Integer;
  CurrEntry: PThoriumStackEntry;
begin
  if Amount > FCount then
  begin
    ClearStack;
    Exit;
  end;
  if Amount < 0 then
  begin
    if FreeValues then
    begin
      for Idx := FCount - 1 downto -Amount do
      begin
        if (FEntries[Idx]._Type = stValue) then
          ThoriumFreeValue(FEntries[Idx].Value);
      end;
    end;
    FCount := -Amount;
    FTop := PThoriumStackEntry(ptruint(FEntries) + (FCount-1) * SizeOf(TThoriumStackEntry));
    Exit;
  end;
  if FreeValues then
  begin
    CurrEntry := @FEntries[FCount - 1];
    for Idx := FCount - 1 downto FCount - Amount do
    begin
      case CurrEntry^._Type of
        stValue:
          ThoriumFreeValue(CurrEntry^.Value);
        stVarargs:
        begin
          FreeMem(CurrEntry^.VarargsBuffer.DataOrigin);
          if CurrEntry^.VarargsBuffer.BufferOrigin <> nil then
            FreeMem(CurrEntry^.VarargsBuffer.BufferOrigin);
          if CurrEntry^.VarargsBuffer.ToFreeOrigin <> nil then
          begin
            while CurrEntry^.VarargsBuffer.ToFree <> CurrEntry^.VarargsBuffer.ToFreeOrigin do
            begin
              Dec(CurrEntry^.VarargsBuffer.ToFree);
              ThoriumFreeValue(CurrEntry^.VarargsBuffer.ToFree[0]);
            end;
            FreeMem(CurrEntry^.VarargsBuffer.ToFreeOrigin);
          end;
        end;
      end;
      Dec(CurrEntry);
    end;
  end;
  Dec(FTop, Amount);
  Dec(FCount, Amount);
end;

function TThoriumStack.PopTop: PThoriumStackEntry; inline;
// This pops off the first element from the stack. This function is not capable
// of freeing the value since it returns it.
begin
  Result := FTop;
  Dec(FTop);
  Dec(FCount);
end;

procedure TThoriumStack.ClearStack;
// Removes and frees all values on the stack. Note: This does not free any
// memory reserved for the structures (=> does not decrease the "real" size of
// the stack).

// A NOTE ABOUT AN EXCEPTION WHICH MIGHT OCCUR HERE!
// If you experience an EAccessViolation at the ThoriumFreeValue call, you are
// probably calling something, which makes the stack clear itself, in a finally
// section or anything alike.
// The main problem might be that the Virtual Machine has thrown an exception
// about a crash in an instruction, which lead the Stack in an unstable state
// (there is currently no way to detect this).
// Try to use a proper debugger to fetch the exception thrown by the VM, since
// it contains a lot more information about the crash. Otherwise try to build
// a try-except-section around the call to the VM (e.g. via SafeCall of
// TThoriumFunction) and check for a EThoriumRuntimeExecutionException.
// A NOTE ABOUT AN EXCEPTION WHICH MIGHT OCCUR HERE!
var
  Idx: Integer;
begin
  for Idx := FCount - 1 downto 0 do
  begin
    if FEntries[Idx]._Type = stValue then
      ThoriumFreeValue(FEntries[Idx].Value);
      // IF YOU EXPERIENCE A CRASH HERE, HAVE A LOOK IN THE COMMENT ABOVE!
  end;
  FCount := 0;
  FTop := FEntries;
  Dec(FTop);
end;

{ TThoriumRuntimeFunction }

constructor TThoriumRuntimeFunction.Create(
  const ACompiledFunction: TThoriumFunction);
begin
  FEntryPoint := ACompiledFunction.FEntryPoint;
  FPrototype := ACompiledFunction.FPrototype;
  FHasReturnValue := FPrototype.FReturnType <> nil;
  FParameterCount := FPrototype.FParameters.Count;
end;

procedure TThoriumRuntimeFunction.FillStackFrame(
  var StackFrame: TThoriumStackFrame);
begin
  StackFrame.ParameterCount := FParameterCount;
  StackFrame.HasReturnValue := FHasReturnValue;
end;

{ TThoriumRuntimeModule }

constructor TThoriumRuntimeModule.Create(const ACompiledModule: TThoriumModule;
  const ResolveModulePtrCallback: TThoriumVMResolveModulePtr;
  const ResolveFunctionPtrCallback: TThoriumVMResolveFunctionPtr);
begin
  FCompiledModule := ACompiledModule;
  FName := ACompiledModule.FName;
  FResolveModulePtrCallback := ResolveModulePtrCallback;
  FResolveFunctionPtrCallback := ResolveFunctionPtrCallback;
  FInstructions := TThoriumInstructions.Create;
  FInstructions.Assign(ACompiledModule.FInstructions);
  FStringLibrary := TStringList.Create;
  FStringLibrary.Assign(ACompiledModule.FStringLibrary);
end;

destructor TThoriumRuntimeModule.Destroy;
var
  I: Integer;
begin
  FStringLibrary.Free;
  for I := 0 to High(FGlobalValues) do
    ThoriumFreeValue(FGlobalValues[I]);
  FInstructions.Free;
  inherited Destroy;
end;

function TThoriumRuntimeModule.DoResolveFunctionPtr(const AFunctionRef: Pointer
  ): TThoriumRuntimeFunction;
begin
  if FResolveFunctionPtrCallback = nil then
    raise EThoriumException.Create('No function ptr resolution callback assigned.');
  Result := FResolveFunctionPtrCallback(AFunctionRef);
end;

function TThoriumRuntimeModule.DoResolveModulePtr(const AModuleRef: Pointer
  ): TThoriumRuntimeModule;
begin
  if FResolveModulePtrCallback = nil then
    raise EThoriumException.Create('No module ptr resolution callback assigned.');
  Result := FResolveModulePtrCallback(AModuleRef);
end;

function TThoriumRuntimeModule.FilterInstruction(
  var AInstruction: TThoriumInstruction): Boolean;
begin
  Result := True;
  case AInstruction.Instruction of
    tiNOOP, tiEmbeddedHint:
      Exit(False);
    tiMOVEFG, tiCOPYFG, tiMOVER_FG, tiCOPYR_FG:
    begin
      TThoriumInstructionMOVEFG(AInstruction).ModuleRef := DoResolveModulePtr(TThoriumInstructionMOVEFG(AInstruction).ModuleRef);
    end;
    tiFNC:
    begin
      TThoriumInstructionFNC(AInstruction).FunctionRef := DoResolveFunctionPtr(TThoriumInstructionFNC(AInstruction).FunctionRef);
    end;
  end;
end;

procedure TThoriumRuntimeModule.Init;
var
  I: Integer;
  Func: TThoriumRuntimeFunction;
begin
  SetLength(FGlobalValues, FCompiledModule.FGlobalValueCount);
  if FCompiledModule.FGlobalValueCount > 0 then
    FillByte(FGlobalValues[0], SizeOf(TThoriumValue) * Length(FGlobalValues), 0);

  for I := 0 to FCompiledModule.FPrivateFunctions.Count - 1 do
  begin
    Func := DoResolveFunctionPtr(FCompiledModule.FPrivateFunctions[I]);
    FInstructions.AddInstructionPointer(@Func.FEntryPoint);
  end;
  for I := 0 to FCompiledModule.FPublicFunctions.Count - 1 do
  begin
    Func := DoResolveFunctionPtr(FCompiledModule.FPublicFunctions[I]);
    FInstructions.AddInstructionPointer(@Func.FEntryPoint);
  end;

  I := 0;
  while I < FInstructions.Count do
  begin
    if not FilterInstruction(FInstructions.GetInstruction(I)^) then
    begin
      FInstructions.DeleteInstructions(I, 1);
      Continue;
    end;
    Inc(I);
  end;
end;

{ TThoriumVirtualMachine }

constructor TThoriumVirtualMachine.Create(AThorium: TThorium);
var
  I: Integer;
begin
  inherited Create;
  FThorium := AThorium;
  FTypeString := FThorium.FTypeString;
  FTypeInteger := FThorium.FTypeInteger;
  FTypeFloat := FThorium.FTypeFloat;
  FStack := TThoriumStack.Create;
  FModules := TThoriumRuntimeModules.Create;
  FModuleMap := TThoriumRuntimeModuleMap.Create;
  FFunctions := TThoriumRuntimeFunctions.Create;
  FFunctionMap := TThoriumRuntimeFunctionMap.Create;
  ImportModules;
  FCurrentModule := nil;
  FCurrentInstructionIdx := -1;
  FCurrentStackFrame := -1;
  FillByte(FRegisters, SizeOf(TThoriumRegisters), 0);
  for I := 0 to FModules.Count - 1 do
  begin
    if FModules[I].FInstructions.Count > 0 then
      Execute(FModules[I], 0, False);
  end;
end;

destructor TThoriumVirtualMachine.Destroy;
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
    FModules[I].Free;
  FModules.Free;
  FStack.Free;
  inherited Destroy;
end;

procedure TThoriumVirtualMachine.ExecuteInstruction; inline;
var
  _NewEntry, _Operand1: PThoriumStackEntry;
  I, J: Integer;

  procedure int_s; inline;
  begin
    with TThoriumInstructionINT_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stValue;
      _NewEntry^.Value.RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      _NewEntry^.Value.Int := Value;
      {$ifdef Timecheck}EndTimecheck('int.s');{$endif}
    end;
  end;

  procedure int; inline;
  begin
    with TThoriumInstructionINT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := Value;
      {$ifdef Timecheck}EndTimecheck('int');{$endif}
    end;
  end;

  procedure intb; inline;
  begin
    with TThoriumInstructionINTB(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      case Kind of
        THORIUM_OP_EQUAL:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_EQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].Int := 1
          else
            FRegisters[TRI].Int := 0;
        end;
        THORIUM_OP_NOTEQUAL:
        begin
          if not (FStateRegister and THORIUM_STATE_ACCUM_EQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].Int := 1
          else
            FRegisters[TRI].Int := 0;
        end;
        THORIUM_OP_GREATER:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_GREATER = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].Int := 1
          else
            FRegisters[TRI].Int := 0;
        end;
        THORIUM_OP_LESS:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_LESS = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].Int := 1
          else
            FRegisters[TRI].Int := 0;
        end;
        THORIUM_OP_GREATEREQUAL:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_GREATEREQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].Int := 1
          else
            FRegisters[TRI].Int := 0;
        end;
        THORIUM_OP_LESSEQUAL:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_LESSEQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].Int := 1
          else
            FRegisters[TRI].Int := 0;
        end;
      else
        FRegisters[TRI].Int := 0;
      end;
      {$ifdef Timecheck}EndTimecheck('intb');{$endif}
    end;
  end;

  procedure flt_s; inline;
  begin
    with TThoriumInstructionFLT_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stValue;
      _NewEntry^.Value.RTTI := {$ifdef DebugToConsole}FTypeFloat{$else}nil{$endif};
      _NewEntry^.Value.Float := Value;
      {$ifdef Timecheck}EndTimecheck('flt.s');{$endif}
    end;
  end;

  procedure flt; inline;
  begin
    with TThoriumInstructionFLT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeFloat{$else}nil{$endif};;
      FRegisters[TRI].Float := Value;
      {$ifdef Timecheck}EndTimecheck('flt');{$endif}
    end;
  end;

  procedure str_s; inline;
  begin
    with TThoriumInstructionSTR_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stValue;
      _NewEntry^.Value.RTTI := FThorium.FTypeString;
      New(_NewEntry^.Value.Str);
      {$ifdef Timecheck}EndTimecheck('str.s');{$endif}
    end;
  end;

  procedure strl_s; inline;
  begin
    with TThoriumInstructionSTRL_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stValue;
      _NewEntry^.Value.RTTI := FThorium.FTypeString;
      New(_NewEntry^.Value.Str);
      _NewEntry^.Value.Str^ := FCurrentModule.FStringLibrary[Index];
      {$ifdef Timecheck}EndTimecheck('strl.s');{$endif}
    end;
  end;

  procedure str; inline;
  begin
    with TThoriumInstructionSTR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := FThorium.FTypeString;
      New(FRegisters[TRI].Str);
      {$ifdef Timecheck}EndTimecheck('str');{$endif}
    end;
  end;

  procedure strl; inline;
  begin
    with TThoriumInstructionSTRL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := FThorium.FTypeString;
      New(FRegisters[TRI].Str);
      FRegisters[TRI].Str^ := FCurrentModule.FStringLibrary[Index];
      {$ifdef Timecheck}EndTimecheck('strl');{$endif}
    end;
  end;

  procedure ext_s; inline;
  begin
    with TThoriumInstructionEXT_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^.Value := TThoriumType(ExtendedType).DoCreateNone;
      {$ifdef Timecheck}EndTimecheck('ext.s');{$endif}
    end;
  end;

  procedure ext; inline;
  begin
    with TThoriumInstructionEXT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := TThoriumType(ExtendedType).DoCreateNone;
      {$ifdef Timecheck}EndTimecheck('ext');{$endif}
    end;
  end;

  procedure fnc; inline;
  begin
    with TThoriumInstructionFNC(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].Func := TThoriumFunction(FunctionRef);
      FRegisters[TRI].RTTI := FRegisters[TRI].Func.Prototype;
      {$ifdef Timecheck}EndTimecheck('fnc');{$endif}
    end;
  end;

  procedure xfnc; inline;
  begin
    with TThoriumInstructionXFNC(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].HostFunc := TThoriumHostFunctionBase(FunctionRef);
      FRegisters[TRI].RTTI := FRegisters[TRI].HostFunc.Prototype;
      {$ifdef Timecheck}EndTimecheck('fnce');{$endif}
    end;
  end;

  procedure xmeth; inline;
  begin
    with TThoriumInstructionXMETH(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].HostMethod := TThoriumHostMethodBase(MethodRef);
      FRegisters[TRI].RTTI := FRegisters[TRI].HostMethod.Prototype;
      {$ifdef Timecheck}EndTimecheck('fnce');{$endif}
    end;
  end;

  procedure mover_g; inline;
  begin
    with TThoriumInstructionMOVER_G(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      ThoriumFreeValue(FCurrentModule.FGlobalValues[Offset]);
      FCurrentModule.FGlobalValues[Offset] := FRegisters[SRI];
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyr_g; inline;
  begin
    with TThoriumInstructionCOPYR_G(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      ThoriumFreeValue(FCurrentModule.FGlobalValues[Offset]);
      FCurrentModule.FGlobalValues[Offset] := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure moveg; inline;
  begin
    with TThoriumInstructionMOVEG(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := FCurrentModule.FGlobalValues[Offset];
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyg; inline;
  begin
    with TThoriumInstructionMOVEG(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := ThoriumDuplicateValue(FCurrentModule.FGlobalValues[Offset]);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure mover_fg; inline;
  begin
    with TThoriumInstructionMOVER_FG(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      ThoriumFreeValue(TThoriumRuntimeModule(ModuleRef).FGlobalValues[Offset]);
      TThoriumRuntimeModule(ModuleRef).FGlobalValues[Offset] := FRegisters[SRI];
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyr_fg; inline;
  begin
    with TThoriumInstructionCOPYR_FG(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      ThoriumFreeValue(TThoriumRuntimeModule(ModuleRef).FGlobalValues[Offset]);
      TThoriumRuntimeModule(ModuleRef).FGlobalValues[Offset] := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure movefg; inline;
  begin
    with TThoriumInstructionMOVEFG(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := TThoriumRuntimeModule(ModuleRef).FGlobalValues[Offset];
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyfg; inline;
  begin
    with TThoriumInstructionMOVEFG(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := ThoriumDuplicateValue(TThoriumRuntimeModule(ModuleRef).FGlobalValues[Offset]);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure mover_l; inline;
  begin
    with TThoriumInstructionMOVER_L(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FCurrentStackFrame, Offset+1);
      Assert(_Operand1^._Type = stValue);
      ThoriumFreeValue(_Operand1^.Value);
      _Operand1^.Value := FRegisters[SRI];
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyr_l; inline;
  begin
    with TThoriumInstructionCOPYR_L(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FCurrentStackFrame, Offset+1);
      Assert(_Operand1^._Type = stValue);
      ThoriumFreeValue(_Operand1^.Value);
      _Operand1^.Value := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure movel; inline;
  begin
    with TThoriumInstructionMOVEL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      Assert(FStack.FastGetStackEntry(FCurrentStackFrame, Offset+1)^._Type = stValue);
      FRegisters[TRI] := FStack.FastGetStackEntry(FCurrentStackFrame, Offset+1)^.Value;
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyl; inline;
  begin
    with TThoriumInstructionCOPYL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      Assert(FStack.FastGetStackEntry(FCurrentStackFrame, Offset+1)^._Type = stValue);
      FRegisters[TRI] := ThoriumDuplicateValue(FStack.FastGetStackEntry(FCurrentStackFrame, Offset+1)^.Value);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure mover_p; inline;
  begin
    with TThoriumInstructionMOVER_P(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FCurrentStackFrame, Offset);
      Assert(_Operand1^._Type = stValue);
      ThoriumFreeValue(_Operand1^.Value);
      _Operand1^.Value := FRegisters[SRI];
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyr_p; inline;
  begin
    with TThoriumInstructionCOPYR_P(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FCurrentStackFrame, Offset);
      Assert(_Operand1^._Type = stValue);
      ThoriumFreeValue(_Operand1^.Value);
      _Operand1^.Value := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure movep; inline;
  begin
    with TThoriumInstructionMOVEP(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      Assert(FStack.FastGetStackEntry(FCurrentStackFrame, Offset)^._Type = stValue);
      FRegisters[TRI] := FStack.FastGetStackEntry(FCurrentStackFrame, Offset)^.Value;
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyp; inline;
  begin
    with TThoriumInstructionCOPYP(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      Assert(FStack.FastGetStackEntry(FCurrentStackFrame, Offset)^._Type = stValue);
      FRegisters[TRI] := ThoriumDuplicateValue(FStack.FastGetStackEntry(FCurrentStackFrame, Offset)^.Value);
      {$ifdef Timecheck}EndTimecheck(THORIUM_INSTRUCTION_CODE_NAME[Instruction]);{$endif}
    end;
  end;

  procedure copyr_st; inline;
  begin
    with TThoriumInstructionCOPYR_ST(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stValue;
      _NewEntry^.Value := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('copyr.st');{$endif}
    end;
  end;

  procedure copyr; inline;
  begin
    with TThoriumInstructionCOPYR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('copyr');{$endif}
    end;
  end;

  procedure mover_st; inline;
  begin
    with TThoriumInstructionMOVER_ST(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stValue;
      Move(FRegisters[SRI], _NewEntry^.Value, SizeOf(TThoriumValue));
      {$ifdef Timecheck}EndTimecheck('mover.st');{$endif}
    end;
  end;

  procedure mover; inline;
  begin
    with TThoriumInstructionMOVER(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      Move(FRegisters[SRI], FRegisters[TRI], SizeOf(TThoriumValue));
      {$ifdef Timecheck}EndTimecheck('mover');{$endif}
    end;
  end;

  procedure movest; inline;
  begin
    with TThoriumInstructionMOVEST(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTop;
      Move(_Operand1^.Value, FRegisters[TRI], SizeOf(TThoriumValue));
      FStack.Pop(1, False);
      {$ifdef Timecheck}EndTimecheck('moves');{$endif}
    end;
  end;

  procedure pop_s; inline;
  begin
    with TThoriumInstructionPOP_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStack.Pop(Amount);
      {$ifdef Timecheck}EndTimecheck('pop.s');{$endif}
    end;
  end;

  procedure clr; inline;
  begin
    with TThoriumInstructionCLR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      ThoriumFreeValue(FRegisters[TRI]);
      {$ifdef Timecheck}EndTimecheck('clr');{$endif}
    end;
  end;

  procedure castif; inline;
  begin
    with TThoriumInstructionCASTIF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].Float := FRegisters[TRI].Int;
      {$ifdef Timecheck}EndTimecheck('castif');{$endif}
    end;
  end;

  procedure cmpi; inline;
  begin
    with TThoriumInstructionCMPI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      if FRegisters[Op1].Int > FRegisters[Op2].Int then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].Int < FRegisters[Op2].Int then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmpi');{$endif}
    end;
  end;

  procedure cmpif; inline;
  begin
    with TThoriumInstructionCMPIF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      if FRegisters[Op1].Int > FRegisters[Op2].Float then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].Int < FRegisters[Op2].Float then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmpif');{$endif}
    end;
  end;

  procedure cmpf; inline;
  begin
    with TThoriumInstructionCMPF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      if FRegisters[Op1].Float > FRegisters[Op2].Float then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].Float < FRegisters[Op2].Float then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmpf');{$endif}
    end;
  end;

  procedure cmpfi; inline;
  begin
    with TThoriumInstructionCMPFI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      if FRegisters[Op1].Float > FRegisters[Op2].Int then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].Float < FRegisters[Op2].Int then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmpfi');{$endif}
    end;
  end;

  procedure cmps; inline;
  begin
    with TThoriumInstructionCMPS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      I := CompareStr(FRegisters[Op1].Str^, FRegisters[Op2].Str^);
      if I < 0 then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else if I > 0 then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmps');{$endif}
    end;
  end;

  procedure addi; inline;
  begin
    with TThoriumInstructionADDI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int + FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('addi');{$endif}
    end;
  end;

  procedure addf; inline;
  begin
    with TThoriumInstructionADDF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeFloat{$else}nil{$endif};
      FRegisters[TRI].Float := FRegisters[Op1].Float + FRegisters[Op2].Float;
      {$ifdef Timecheck}EndTimecheck('addf');{$endif}
    end;
  end;

  procedure adds; inline;
  begin
    with TThoriumInstructionADDS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if TRI = Op1 then
      begin
        FRegisters[TRI].Str^ := FRegisters[Op1].Str^ + FRegisters[Op2].Str^;
      end
      else
      begin
        FRegisters[TRI].RTTI := FTypeString;
        New(FRegisters[TRI].Str);
        FRegisters[TRI].Str^ := FRegisters[Op1].Str^ + FRegisters[Op2].Str^;
      end;
      {$ifdef Timecheck}EndTimecheck('adds');{$endif}
    end;
  end;

  procedure subi; inline;
  begin
    with TThoriumInstructionSUBI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int -
        FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('subi');{$endif}
    end;
  end;

  procedure subf; inline;
  begin
    with TThoriumInstructionSUBF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeFloat{$else}nil{$endif};
      FRegisters[TRI].Float := FRegisters[Op1].Float - FRegisters[Op2].Float;
      {$ifdef Timecheck}EndTimecheck('subf');{$endif}
    end;
  end;

  procedure muli; inline;
  begin
    with TThoriumInstructionMULI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int * FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('muli');{$endif}
    end;
  end;

  procedure mulf; inline;
  begin
    with TThoriumInstructionMULF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeFloat{$else}nil{$endif};
      FRegisters[TRI].Float := FRegisters[Op1].Float * FRegisters[Op2].Float;
      {$ifdef Timecheck}EndTimecheck('mulf');{$endif}
    end;
  end;

  procedure divi; inline;
  begin
    with TThoriumInstructionDIVI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int div FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('divi');{$endif}
    end;
  end;

  procedure divf; inline;
  begin
    with TThoriumInstructionDIVF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeFloat{$else}nil{$endif};
      FRegisters[TRI].Float := FRegisters[Op1].Float / FRegisters[Op2].Float;
      {$ifdef Timecheck}EndTimecheck('divf');{$endif}
    end;
  end;

  procedure negi; inline;
  begin
    with TThoriumInstructionNEGI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[Op1].Int := -FRegisters[Op1].Int;
      {$ifdef Timecheck}EndTimecheck('negi');{$endif}
    end;
  end;

  procedure negf; inline;
  begin
    with TThoriumInstructionNEGF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[Op1].Float := -FRegisters[Op1].Float;
      {$ifdef Timecheck}EndTimecheck('negf');{$endif}
    end;
  end;

  procedure _not; inline;
  begin
    with TThoriumInstructionNOT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[Op1].Int := not FRegisters[Op1].Int;
      {$ifdef Timecheck}EndTimecheck('not');{$endif}
    end;
  end;

  procedure bnot; inline;
  begin
    with TThoriumInstructionNOT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if FRegisters[Op1].Int = 0 then
        FRegisters[Op1].Int := 1
      else
        FRegisters[Op1].Int := 0;
      {$ifdef Timecheck}EndTimecheck('bnot');{$endif}
    end;
  end;

  procedure _mod; inline;
  begin
    with TThoriumInstructionMOD(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int mod FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('mod');{$endif}
    end;
  end;

  procedure _and; inline;
  begin
    with TThoriumInstructionAND(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int and FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('and');{$endif}
    end;
  end;

  procedure _or; inline;
  begin
    with TThoriumInstructionOR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int or FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('or');{$endif}
    end;
  end;

  procedure _xor; inline;
  begin
    with TThoriumInstructionXOR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int xor FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('xor');{$endif}
    end;
  end;

  procedure _shl; inline;
  begin
    with TThoriumInstructionSHL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int shl FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('shl');{$endif}
    end;
  end;

  procedure _shr; inline;
  begin
    with TThoriumInstructionSHR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI := {$ifdef DebugToConsole}FTypeInteger{$else}nil{$endif};
      FRegisters[TRI].Int := FRegisters[Op1].Int shr FRegisters[Op2].Int;
      {$ifdef Timecheck}EndTimecheck('shr');{$endif}
    end;
  end;

  procedure inci; inline;
  begin
    with TThoriumInstructionINCI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].Int+=1;
      {$ifdef Timecheck}EndTimecheck('inci');{$endif}
    end;
  end;

  procedure incf; inline;
  begin
    with TThoriumInstructionINCF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].Float+=1;
      {$ifdef Timecheck}EndTimecheck('incf');{$endif}
    end;
  end;

  procedure deci; inline;
  begin
    with TThoriumInstructionDECI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].Int-=1;
      {$ifdef Timecheck}EndTimecheck('deci');{$endif}
    end;
  end;

  procedure decf; inline;
  begin
    with TThoriumInstructionDECF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].Float-=1;
      {$ifdef Timecheck}EndTimecheck('decf');{$endif}
    end;
  end;

  procedure xfget; inline;
  begin
    with TThoriumInstructionXFGET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[THORIUM_REGISTER_C5_RUNTIME] := FRegisters[ERI];
      FRegisters[ERI] := FRegisters[THORIUM_REGISTER_C5_RUNTIME].RTTI.DoGetField(FRegisters[ERI], ID);
      {$ifdef Timecheck}EndTimecheck('xfget');{$endif}
    end;
  end;

  procedure xfset; inline;
  begin
    with TThoriumInstructionXFSET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[ERI].RTTI.DoSetField(FRegisters[ERI], ID, FRegisters[VRI]);
      {$ifdef Timecheck}EndTimecheck('xfset');{$endif}
    end;
  end;

  procedure xiget; inline;
  begin
    with TThoriumInstructionXIGET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[THORIUM_REGISTER_C5_RUNTIME] := FRegisters[ERI].RTTI.DoGetIndexed(FRegisters[ERI], FRegisters[IRI]);
      FRegisters[TRI] := FRegisters[THORIUM_REGISTER_C5_RUNTIME];
      {$ifdef Timecheck}EndTimecheck('xiget');{$endif}
    end;
  end;

  procedure xct; inline;
  begin
    with TThoriumInstructionXCT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[ERI].RTTI.DoEnableHostControl(FRegisters[ERI]);
      {$ifdef Timecheck}EndTimecheck('xct');{$endif}
    end;
  end;

  procedure x2n; inline;
  begin
    with TThoriumInstructionX2N(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI.DoToNative(FRegisters[TRI], HostType);
      {$ifdef Timecheck}EndTimecheck('x2n');{$endif}
    end;
  end;

  procedure n2x; inline;
  begin
    with TThoriumInstructionN2X(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI.DoFromNative(FRegisters[TRI]);
      {$ifdef Timecheck}EndTimecheck('n2x');{$endif}
    end;
  end;

  procedure clrn; inline;
  begin
    with TThoriumInstructionCLRN(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].RTTI.DoFreeNative(FRegisters[TRI]);
      {$ifdef Timecheck}EndTimecheck('clrn');{$endif}
    end;
  end;

  procedure xiset; inline;
  begin
    with TThoriumInstructionXISET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[ERI].RTTI.DoSetIndexed(FRegisters[ERI], FRegisters[IRI], FRegisters[VRI]);
      {$ifdef Timecheck}EndTimecheck('xiset');{$endif}
    end;
  end;

  procedure xpget; inline;
  begin
    with TThoriumInstructionXPGET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      TThoriumLibraryProperty(Prop).GetValue(FRegisters[TRI]);
      {$ifdef Timecheck}EndTimecheck('xpset');{$endif}
    end;
  end;

  procedure xpset; inline;
  begin
    with TThoriumInstructionXPSET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      TThoriumLibraryProperty(Prop).SetValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('xpset');{$endif}
    end;
  end;

  procedure vastart; inline;
  begin
    with TThoriumInstructionVASTART(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stVarargs;
      if Pointers = 1 then
        _NewEntry^.VarargsBuffer.Data := GetMem(Length * SizeOf(ptruint))
      else
        _NewEntry^.VarargsBuffer.Data := GetMem(Length);
      _NewEntry^.VarargsBuffer.DataOrigin := _NewEntry^.VarargsBuffer.Data;
      _NewEntry^.VarargsBuffer.BufferOrigin := nil;
      _NewEntry^.VarargsBuffer.ToFreeOrigin := nil;

      {$ifdef Timecheck}EndTimecheck('vastart');{$endif}
    end;
  end;

  procedure va_i8; inline;
  begin
    with TThoriumInstructionVA_I8(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].Int >= $FF then
        PByte(_Operand1^.VarargsBuffer.Data)^ := $FF
      else if FRegisters[SRI].Int <= 0 then
        PByte(_Operand1^.VarargsBuffer.Data)^ := 0
      else
        PByte(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int;
      Inc(_Operand1^.VarargsBuffer.Data, 1);
      {$ifdef Timecheck}EndTimecheck('va.i8');{$endif}
    end;
  end;

  procedure va_i16; inline;
  begin
    with TThoriumInstructionVA_I16(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].Int >= $FFFF then
        PWord(_Operand1^.VarargsBuffer.Data)^ := $FFFF
      else if FRegisters[SRI].Int <= 0 then
        PWord(_Operand1^.VarargsBuffer.Data)^ := 0
      else
        PWord(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int;
      Inc(_Operand1^.VarargsBuffer.Data, 2);
      {$ifdef Timecheck}EndTimecheck('va.i16');{$endif}
    end;
  end;

  procedure va_i32; inline;
  begin
    with TThoriumInstructionVA_I32(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].Int >= $FFFFFFFF then
        PDWord(_Operand1^.VarargsBuffer.Data)^ := $FFFFFFFF
      else if FRegisters[SRI].Int <= 0 then
        PDWord(_Operand1^.VarargsBuffer.Data)^ := 0
      else
        PDWord(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int;
      Inc(_Operand1^.VarargsBuffer.Data, 4);
      {$ifdef Timecheck}EndTimecheck('va.i32');{$endif}
    end;
  end;

  procedure va_i64; inline;
  begin
    with TThoriumInstructionVA_I64(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PQWord(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int and $7FFFFFFFFFFFFFFF;
      Inc(_Operand1^.VarargsBuffer.Data, 8);
      {$ifdef Timecheck}EndTimecheck('va.i64');{$endif}
    end;
  end;

  procedure va_i8s; inline;
  begin
    with TThoriumInstructionVA_I8S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].Int >= 127 then
        PShortInt(_Operand1^.VarargsBuffer.Data)^ := 127
      else if FRegisters[SRI].Int <= -128 then
        PShortInt(_Operand1^.VarargsBuffer.Data)^ := -128
      else
        PShortInt(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int;
      Inc(_Operand1^.VarargsBuffer.Data, 1);
      {$ifdef Timecheck}EndTimecheck('va.i8s');{$endif}
    end;
  end;

  procedure va_i16s; inline;
  begin
    with TThoriumInstructionVA_I16S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].Int >= 32767 then
        PSmallInt(_Operand1^.VarargsBuffer.Data)^ := 32767
      else if FRegisters[SRI].Int <= -32768 then
        PSmallInt(_Operand1^.VarargsBuffer.Data)^ := -32768
      else
        PSmallInt(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int;
      Inc(_Operand1^.VarargsBuffer.Data, 2);
      {$ifdef Timecheck}EndTimecheck('va.i16s');{$endif}
    end;
  end;

  procedure va_i32s; inline;
  begin
    with TThoriumInstructionVA_I32S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].Int >= 2147483647 then
        PLongInt(_Operand1^.VarargsBuffer.Data)^ := 2147483647
      else if FRegisters[SRI].Int <= -2147483648 then
        PLongInt(_Operand1^.VarargsBuffer.Data)^ := -2147483648
      else
        PLongInt(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int;
      Inc(_Operand1^.VarargsBuffer.Data, 4);
      {$ifdef Timecheck}EndTimecheck('va.i32s');{$endif}
    end;
  end;

  procedure va_i64s; inline;
  begin
    with TThoriumInstructionVA_I64S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PInt64(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Int;
      Inc(_Operand1^.VarargsBuffer.Data, 8);
      {$ifdef Timecheck}EndTimecheck('va.i64s');{$endif}
    end;
  end;

  procedure va_f32; inline;
  begin
    with TThoriumInstructionVA_F32(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PSingle(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Float;
      Inc(_Operand1^.VarargsBuffer.Data, 4);
      {$ifdef Timecheck}EndTimecheck('va.f32');{$endif}
    end;
  end;

  procedure va_f64; inline;
  begin
    with TThoriumInstructionVA_F64(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PDouble(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Float;
      Inc(_Operand1^.VarargsBuffer.Data, 8);
      {$ifdef Timecheck}EndTimecheck('va.f64');{$endif}
    end;
  end;

  procedure va_f80; inline;
  begin
    with TThoriumInstructionVA_F80(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PExtended(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Float;
      Inc(_Operand1^.VarargsBuffer.Data, 10);
      {$ifdef Timecheck}EndTimecheck('va.f80');{$endif}
    end;
  end;

  procedure va_s; inline;
  begin
    with TThoriumInstructionVA_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      // Use move instead of assignment to avoid hassle with the stringmanager
      // okay, okay, use assignment instead of move to avoid hassle with the
      // stringmanager -.-
      _Operand1 := FStack.GetTopStackEntry;
      // Move(FRegisters[SRI].BuiltIn.Str^, PString(_Operand1^.VarargsBuffer.Data)^, SizeOf(String));
      PPointer(_Operand1^.VarargsBuffer.Data)^ := nil;
      PString(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].Str^;
      Inc(_Operand1^.VarargsBuffer.Data, SizeOf(Ptruint));
      {$ifdef Timecheck}EndTimecheck('va.s');{$endif}
    end;
  end;

  procedure va_x; inline;
  begin
    with TThoriumInstructionVA_X(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PPointer(_Operand1^.VarargsBuffer.Data)^ := FRegisters[SRI].HostObject;
      Inc(_Operand1^.VarargsBuffer.Data, SizeOf(ptruint));
      {$ifdef Timecheck}EndTimecheck('va.x');{$endif}
    end;
  end;

  procedure vastart_t; inline;
  begin
    with TThoriumInstructionVASTART_T(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stVarargs;
      _NewEntry^.VarargsBuffer.Data := GetMem(Length * SizeOf(TVarRec));
      _NewEntry^.VarargsBuffer.DataOrigin := _NewEntry^.VarargsBuffer.Data;
      _NewEntry^.VarargsBuffer.Buffer := GetMem(Floats * SizeOf(Extended) + (Length - (ToClear + Floats)) * SizeOf(TThoriumInteger));
      _NewEntry^.VarargsBuffer.BufferOrigin := _NewEntry^.VarargsBuffer.Buffer;
      _NewEntry^.VarargsBuffer.ToFree := GetMem(ToClear * SizeOf(TThoriumValue));
      _NewEntry^.VarargsBuffer.ToFreeOrigin := _NewEntry^.VarargsBuffer.ToFree;
      {$ifdef Timecheck}EndTimecheck('vastart.t');{$endif}
    end;
  end;

  procedure vat_f; inline;
  begin
    with TThoriumInstructionVAT_F(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PExtended(_Operand1^.VarargsBuffer.Buffer)^ := FRegisters[SRI].Float;
      with PVarRec(_Operand1^.VarargsBuffer.Data)^ do
      begin
        VType := vtExtended;
        VExtended := _Operand1^.VarargsBuffer.Buffer;
      end;
      Inc(_Operand1^.VarargsBuffer.Buffer, SizeOf(Extended));
      Inc(_Operand1^.VarargsBuffer.Data, SizeOf(TVarRec));
      {$ifdef Timecheck}EndTimecheck('vat.f');{$endif}
    end;
  end;

  procedure vat_i; inline;
  begin
    with TThoriumInstructionVAT_I(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PThoriumInteger(_Operand1^.VarargsBuffer.Buffer)^ := FRegisters[SRI].Int;
      with PVarRec(_Operand1^.VarargsBuffer.Data)^ do
      begin
        VType := vtInt64;
        VInt64 := _Operand1^.VarargsBuffer.Buffer;
      end;
      Inc(_Operand1^.VarargsBuffer.Buffer, SizeOf(TThoriumInteger));
      Inc(_Operand1^.VarargsBuffer.Data, SizeOf(TVarRec));
      {$ifdef Timecheck}EndTimecheck('vat.i');{$endif}
    end;
  end;

  procedure vat_s; inline;
  begin
    with TThoriumInstructionVAT_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      with PVarRec(_Operand1^.VarargsBuffer.Data)^ do
      begin
        VType := vtAnsiString;
        VAnsiString := PPointer(FRegisters[SRI].Str)^;
      end;
      //Move(FRegisters[SRI], _Operand1^.VarargsBuffer.ToFree[0], SizeOf(TThoriumValue));
      //Inc(_Operand1^.VarargsBuffer.ToFree);
      Inc(_Operand1^.VarargsBuffer.Data, SizeOf(TVarRec));
      {$ifdef Timecheck}EndTimecheck('vat.s');{$endif}
    end;
  end;

  procedure vat_x; inline;
  begin
    with TThoriumInstructionVAT_X(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      with PVarRec(_Operand1^.VarargsBuffer.Data)^ do
      begin
        VType := vtPointer;
        VPointer := FRegisters[SRI].HostObject;
      end;
      Move(FRegisters[SRI], _Operand1^.VarargsBuffer.ToFree[0], SizeOf(TThoriumValue));
      Inc(_Operand1^.VarargsBuffer.ToFree);
      Inc(_Operand1^.VarargsBuffer.Data, SizeOf(TVarRec));
      {$ifdef Timecheck}EndTimecheck('vat.x');{$endif}
    end;
  end;

  procedure vafinish; inline;
  begin
    with TThoriumInstructionVAFINISH(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      {$ifdef Timecheck}EndTimecheck('vafinish');{$endif}
    end;
  end;

  procedure jmp; inline;
  begin
    with TThoriumInstructionJMP(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      Inc(FCurrentInstruction, (NewAddress-1)-FCurrentInstructionIdx);
      //WriteLn('JMP by offset ', (NewAddress-1)-FCurrentInstructionIdx, ' to ', IntToHex(NewAddress-1, 4), ' from ', IntToHex(FCurrentInstructionIdx, 4));
      Inc(FCurrentInstructionIdx, (NewAddress-1)-FCurrentInstructionIdx);
      FCurrentInstructionIdx := NewAddress-1;
      {$ifdef Timecheck}EndTimecheck('jmp');{$endif}
    end;
  end;

  procedure je; inline;
  begin
    with TThoriumInstructionJE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if (FStateRegister and THORIUM_STATE_ACCUM_EQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
      begin
        Inc(FCurrentInstruction, (NewAddress-1) - FCurrentInstructionIdx);
        Inc(FCurrentInstructionIdx, (NewAddress-1) - FCurrentInstructionIdx);
        FCurrentInstructionIdx := NewAddress-1;
      end;
      {$ifdef Timecheck}EndTimecheck('je');{$endif}
    end;
  end;

  procedure jne; inline;
  begin
    with TThoriumInstructionJNE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if not (FStateRegister and THORIUM_STATE_ACCUM_EQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
      begin
        Inc(FCurrentInstruction, (NewAddress-1) - FCurrentInstructionIdx);
        Inc(FCurrentInstructionIdx, (NewAddress-1) - FCurrentInstructionIdx);
        FCurrentInstructionIdx := NewAddress-1;
      end;
      {$ifdef Timecheck}EndTimecheck('jne');{$endif}
    end;
  end;

  procedure jgt; inline;
  begin
    with TThoriumInstructionJGT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if (FStateRegister and THORIUM_STATE_ACCUM_GREATER = FStateRegister and THORIUM_STATE_COMPFLAGS) then
      begin
        Inc(FCurrentInstruction, (NewAddress-1) - FCurrentInstructionIdx);
        Inc(FCurrentInstructionIdx, (NewAddress-1) - FCurrentInstructionIdx);
        FCurrentInstructionIdx := NewAddress-1;
      end;
      {$ifdef Timecheck}EndTimecheck('jgt');{$endif}
    end;
  end;

  procedure jge; inline;
  begin
    with TThoriumInstructionJGE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if (FStateRegister and THORIUM_STATE_ACCUM_GREATEREQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
      begin
        Inc(FCurrentInstruction, (NewAddress-1) - FCurrentInstructionIdx);
        Inc(FCurrentInstructionIdx, (NewAddress-1) - FCurrentInstructionIdx);
        FCurrentInstructionIdx := NewAddress-1;
      end;
      {$ifdef Timecheck}EndTimecheck('jge');{$endif}
    end;
  end;

  procedure jlt; inline;
  begin
    with TThoriumInstructionJLT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if (FStateRegister and THORIUM_STATE_ACCUM_LESS = FStateRegister and THORIUM_STATE_COMPFLAGS) then
      begin
        Inc(FCurrentInstruction, (NewAddress-1) - FCurrentInstructionIdx);
        Inc(FCurrentInstructionIdx, (NewAddress-1) - FCurrentInstructionIdx);
        FCurrentInstructionIdx := NewAddress-1;
      end;
      {$ifdef Timecheck}EndTimecheck('jlt');{$endif}
    end;
  end;

  procedure jle; inline;
  begin
    with TThoriumInstructionJLE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if (FStateRegister and THORIUM_STATE_ACCUM_LESSEQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
      begin
        Inc(FCurrentInstruction, (NewAddress-1) - FCurrentInstructionIdx);
        Inc(FCurrentInstructionIdx, (NewAddress-1) - FCurrentInstructionIdx);
        FCurrentInstructionIdx := NewAddress-1;
      end;
      {$ifdef Timecheck}EndTimecheck('jle');{$endif}
    end;
  end;

  procedure call; inline;
  begin
    with TThoriumInstructionCALL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stStackFrame;
      FRegisters[SRI].RuntimeFunc.FillStackFrame(_NewEntry^.StackFrame);
      _NewEntry^.StackFrame.ReturnAddress := FCurrentInstructionIdx;
      _NewEntry^.StackFrame.ReturnModule := nil;
      _NewEntry^.StackFrame.PreviousStackFrame := FCurrentStackFrame;
      _NewEntry^.StackFrame.RegisterDumpRange := HRI;
      _NewEntry^.StackFrame.DropResult := False;
      GetMem(_NewEntry^.StackFrame.RegisterDumpData, SizeOf(TThoriumValue)*(HRI+1));
      Move(FRegisters[0], _NewEntry^.StackFrame.RegisterDumpData^, SizeOf(TThoriumValue)*(HRI+1));

      FCurrentStackFrame := FStack.FCount-1;
      I := FRegisters[SRI].Func.FEntryPoint;
      Inc(FCurrentInstruction, (I-1) - FCurrentInstructionIdx);
      Inc(FCurrentInstructionIdx, (I-1) - FCurrentInstructionIdx);
      {$ifdef Timecheck}EndTimecheck('call');{$endif}
    end;
  end;

  procedure call_d; inline;
  begin
    with TThoriumInstructionCALL_D(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise Exception.Create('call.d not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('call.d');{$endif}
    end;
  end;

  procedure fcall; inline;
  begin
    with TThoriumInstructionFCALL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := stStackFrame;
      FRegisters[SRI].RuntimeFunc.FillStackFrame(_NewEntry^.StackFrame);
      _NewEntry^.StackFrame.ReturnAddress := FCurrentInstructionIdx;
      _NewEntry^.StackFrame.PreviousStackFrame := FCurrentStackFrame;
      _NewEntry^.StackFrame.ReturnModule := FCurrentModule;
      _NewEntry^.StackFrame.RegisterDumpRange := HRI;
      _NewEntry^.StackFrame.DropResult := False;
      GetMem(_NewEntry^.StackFrame.RegisterDumpData, SizeOf(TThoriumValue)*HRI);
      Move(FRegisters[0], _NewEntry^.StackFrame.RegisterDumpData^, SizeOf(TThoriumValue)*HRI);

      FCurrentInstructionIdx := FRegisters[SRI].RuntimeFunc.FEntryPoint - 1;
      FCurrentStackFrame := FStack.FCount-1;
      FCurrentModule := FRegisters[SRI].RuntimeFunc.FModule;
      FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
      {$ifdef Timecheck}EndTimecheck('fcall');{$endif}
    end;
  end;

  procedure xcall; inline;
  begin
    with TThoriumInstructionXCALL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      { TODO : Honour HRI }
      FRegisters[SRI].HostFunc.CallFromVirtualMachine(Self);
      {$ifdef Timecheck}EndTimecheck('ecall');{$endif}
    end;
  end;

  procedure xcall_m; inline;
  begin
    with TThoriumInstructionXCALL_M(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      { TODO : Honour HRI }
      FRegisters[SRI].HostMethod.CallFromVirtualMachine(TObject(FRegisters[ERI].HostObject), Self);
      {$ifdef Timecheck}EndTimecheck('ecall');{$endif}
    end;
  end;

  procedure ret; inline;
  begin
    with TThoriumInstructionRET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(0, FCurrentStackFrame);
      FCurrentInstructionIdx := _Operand1^.StackFrame.ReturnAddress;
      if _Operand1^.StackFrame.ReturnModule <> nil then
      begin
        FCurrentModule := _Operand1^.StackFrame.ReturnModule;
      end;
      FCurrentStackFrame := _Operand1^.StackFrame.PreviousStackFrame;
      FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
      if _Operand1^.StackFrame.RegisterDumpData <> nil then
      begin
        Move(_Operand1^.StackFrame.RegisterDumpData^, FRegisters[0], SizeOf(TThoriumValue)*(_Operand1^.StackFrame.RegisterDumpRange+1));
        FreeMem(_Operand1^.StackFrame.RegisterDumpData);
      end;
      if (_Operand1^.StackFrame.HasReturnValue) then
      begin
        if (_Operand1^.StackFrame.DropResult) then
        begin
          // Pop the result away
          FStack.Pop(2+_Operand1^.StackFrame.ParameterCount, False);
        end
        else
        begin
          // Assign the value immediatedly above the stack frame to the slot
          // reserved for the return value.
          if _Operand1^.StackFrame.ParameterCount = 0 then
          begin
            // If there are no parameters, we can directly overwrite the stack
            // frame.
            _Operand1^ := _Operand1[1];
            // _Operand1[1]._Type := stNull; // To avoid freeing by the pop
            FStack.Pop(1, False);
          end
          else
          begin
            // Otherwise we just overwrite the slot of the last parameter
            // ThoriumFreeValue(_Operand1[-(_Operand1^.Params)].Value);
            {$ifdef R+}
            {$define wasr}
            {$endif}
            {$R-}
            _Operand1[-(_Operand1^.StackFrame.ParameterCount)] := _Operand1[1];
            {$ifdef wasr}
            {$R+}
            {$undef wasr}
            {$endif}
            // Again, avoid freeing by the pop
            // _Operand1[1]._Type := stNull;
            FStack.Pop(1+_Operand1^.StackFrame.ParameterCount, False);
          end;
        end;
      end
      else
        FStack.Pop(1+_Operand1^.StackFrame.ParameterCount, False);
      {$ifdef Timecheck}EndTimecheck('ret');{$endif}
    end;
  end;

  (*procedure Template; inline;
  begin
    with TThoriumInstruction(CurrInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      {$ifdef Timecheck}EndTimecheck('');{$endif}
    end;
  end;*)
begin
  case FCurrentInstruction^.Instruction of
    tiINT_S: int_s;
    tiINT: int;
    tiINTB: intb;
    tiFLT_S: flt_s;
    tiFLT: flt;
    tiSTR_S: str_s;
    tiSTRL_S: strl_s;
    tiSTR: str;
    tiSTRL: strl;
    tiEXT_S: ext_s;
    tiEXT: ext;
    tiFNC: fnc;
    tiXFNC: xfnc;
    tiXMETH: xmeth;
    tiMOVER_G: mover_g;
    tiCOPYR_G: copyr_g;
    tiMOVEG: moveg;
    tiCOPYG: copyg;
    tiMOVER_FG: mover_fg;
    tiCOPYR_FG: copyr_fg;
    tiMOVEFG: movefg;
    tiCOPYFG: copyfg;
    tiMOVER_L: mover_l;
    tiCOPYR_L: copyr_l;
    tiMOVEL: movel;
    tiCOPYL: copyl;
    tiMOVER_P: mover_p;
    tiCOPYR_P: copyr_p;
    tiMOVEP: movep;
    tiCOPYP: copyp;
    tiCOPYR_ST: copyr_st;
    tiCOPYR: copyr;
    tiMOVER_ST: mover_st;
    tiMOVER: mover;
    tiMOVEST: movest;
    tiPOP_S: pop_s;
    tiCLR: clr;
    tiCASTIF: castif;
    tiCMPI: cmpi;
    tiCMPIF: cmpif;
    tiCMPF: cmpf;
    tiCMPFI: cmpfi;
    tiCMPS: cmps;
    tiADDI: addi;
    tiADDF: addf;
    tiADDS: adds;
    tiSUBI: subi;
    tiSUBF: subf;
    tiMULI: muli;
    tiMULF: mulf;
    tiDIVI: divi;
    tiDIVF: divf;
    tiNEGI: negi;
    tiNEGF: negf;
    tiNOT: _not;
    tiBNOT: bnot;
    tiMOD: _mod;
    tiAND: _and;
    tiOR: _or;
    tiXOR: _xor;
    tiSHL: _shl;
    tiSHR: _shr;
    tiINCI: inci;
    tiINCF: incf;
    tiDECI: deci;
    tiDECF: decf;
    tiXFGET: xfget;
    tiXFSET: xfset;
    tiXIGET: xiget;
    tiXISET: xiset;
    tiXPGET: xpget;
    tiXPSET: xpset;
    tiXCT: xct;
    tiX2N: x2n;
    tiN2X: n2x;
    tiCLRN: clrn;
    tiVASTART: vastart;
    tiVASTART_T: vastart_t;
    tiVA_I8: va_i8;
    tiVA_I16: va_i16;
    tiVA_I32: va_i32;
    tiVA_I64: va_i64;
    tiVA_I8S: va_i8s;
    tiVA_I16S: va_i16s;
    tiVA_I32S: va_i32s;
    tiVA_I64S: va_i64s;
    tiVA_F32: va_f32;
    tiVA_F64: va_f64;
    tiVA_F80: va_f80;
    tiVA_S: va_s;
    tiVA_X: va_x;
    tiVAT_F: vat_f;
    tiVAT_I: vat_i;
    tiVAT_S: vat_s;
    tiVAT_X: vat_x;
    tiVAFINISH: vafinish;
    tiJMP: jmp;
    tiJE: je;
    tiJNE: jne;
    tiJGT: jgt;
    tiJGE: jge;
    tiJLT: jlt;
    tiJLE: jle;
    tiCALL: call;
    tiCALL_D: call_d;
    tiFCALL: fcall;
    tiXCALL: xcall;
    tiXCALL_M: xcall_m;
    tiRET: ret;
    tiEmbeddedHint: ;
  else
    //Assert(False, 'Unknown opcode');
    raise EThoriumRuntimeException.CreateFmt('Unknown opcode: %d (%s)', [Ord(FCurrentInstruction^.Instruction), GetEnumName(TypeInfo(TThoriumInstructionCode), Ord(FCurrentInstruction^.Instruction))]);
  end;
  Inc(FCurrentInstruction);
  Inc(FCurrentInstructionIdx);
  //System.Inc(FCurrentInstructionIdx);
end;

function TThoriumVirtualMachine.ImportFunction(AFunction: TThoriumFunction
  ): TThoriumRuntimeFunction;
var
  I: Integer;
begin
  if FFunctionMap.Find(ptrint(AFunction), I) then
    Exit(FFunctionMap.Data[I]);

  Result := TThoriumRuntimeFunction.Create(AFunction);
  FFunctionMap.Add(ptrint(AFunction), Result);
  Result.FModule := ResolveModulePtr(AFunction.FModule);
end;

procedure TThoriumVirtualMachine.ImportModules;
var
  I: Integer;
begin
  for I := 0 to FThorium.FModules.Count - 1 do
    ImportModule(FThorium.FModules[I]);
end;

function TThoriumVirtualMachine.ImportModule(AModule: TThoriumModule
  ): TThoriumRuntimeModule;
var
  I: Integer;
begin
  if FModuleMap.Find(ptrint(AModule), I) then
    Exit(FModuleMap.Data[I]);

  Result := TThoriumRuntimeModule.Create(AModule, @ResolveModulePtr, @ResolveFunctionPtr);
  FModuleMap.Add(ptrint(AModule), Result);
  Result.Init;
end;

function TThoriumVirtualMachine.ResolveFunctionPtr(const AFunctionRef: Pointer
  ): TThoriumRuntimeFunction;
begin
  Result := ImportFunction(TThoriumFunction(AFunctionRef));
end;

function TThoriumVirtualMachine.ResolveModulePtr(const AModuleRef: Pointer
  ): TThoriumRuntimeModule;
begin
  Result := ImportModule(TThoriumModule(AModuleRef));
end;

function TThoriumVirtualMachine.GetStack: TThoriumStack;
begin
  Result := FStack;
end;

procedure TThoriumVirtualMachine.Execute(StartModule: TThoriumRuntimeModule; StartInstruction: Integer; CreateDefaultStackFrame: Boolean = True);
var
  Frame: PThoriumStackEntry;
  {$ifdef DebugToConsole}
  OldIdx: Integer;
  {$endif}
begin
  FCurrentInstructionIdx := StartInstruction;
  FCurrentModule := StartModule;
  if (FCurrentInstructionIdx > FCurrentModule.FInstructions.Count - 1) or
    (FCurrentInstructionIdx < 0) then
    raise EThoriumRuntimeException.Create('Instruction index out of bounds.');
  if CreateDefaultStackFrame and (FCurrentStackFrame < 0) then
  begin
    FCurrentStackFrame := FStack.EntryCount;
    Frame := FStack.Push;
    Frame^._Type := stStackFrame;
    Frame^.StackFrame.PreviousStackFrame := -1;
    Frame^.StackFrame.ReturnModule := nil;
    Frame^.StackFrame.ReturnAddress := THORIUM_RETURN_EXIT;
    Frame^.StackFrame.RegisterDumpRange := 0;
    Frame^.StackFrame.RegisterDumpData := nil;
    Frame^.StackFrame.HasReturnValue := False;
    Frame^.StackFrame.ParameterCount := 0;
    Frame^.StackFrame.DropResult := False;
  end;
  try
    FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
    repeat
      if FCurrentInstruction^.Instruction = tiEmbeddedHint then
      begin
        Inc(FCurrentInstructionIdx);
        Inc(FCurrentInstruction);
        Continue;
      end;
      {$ifdef Stackdump or InstructionDump or StateDump}
      {$ifndef InstructionDump}WriteLn{$else}Write{$endif}('$', IntToHex(FCurrentInstructionIdx, 8), ': ');
      {$endif}
      {$ifdef InstructionDump}
      WriteLn(ThoriumInstructionToStr(FCurrentInstruction^));
      {$endif}
      ExecuteInstruction;
      {$ifdef Stackdump or StateDump}
      {$ifdef StateDump}
      DumpState;
      {$endif}
      {$ifdef Stackdump}
      DumpStack;
      {$endif}
      WriteLn;
      {$endif}
    until (FCurrentInstructionIdx < 0) or (FCurrentInstructionIdx >= FCurrentModule.FInstructions.Count);
  except
    on E: Exception do
    begin
      // Tell the exception handling system that we want to free the current
      // exception ourselves.
      AcquireExceptionObject;
      // Raise a thorium exception which gives more information about the crash.
      raise EThoriumRuntimeExecutionException.Create(FCurrentModule, FCurrentInstructionIdx, FCurrentInstruction, E);
    end
    else
    begin
      // The exception is not a Exception-based exception, so we cannot get
      // more information about it.
      raise EThoriumRuntimeExecutionException.Create(FCurrentModule, FCurrentInstructionIdx, FCurrentInstruction, nil);
    end;
  end;
end;

function TThoriumVirtualMachine.CallFunction(AFunction: TThoriumRuntimeFunction;
  AParameters: array of const): TThoriumValue;
var
  Values: array of TThoriumValue;
  I: Integer;
begin
  SetLength(Values, Length(AParameters));
  for I := 0 to High(AParameters) do
  begin
    if AParameters[0].VType in [vtExtended, vtVariant, vtString] then
      Values[I] := AFunction.FPrototype.Parameters.TypeSpec[I].CreateValueFromPtr(AParameters[0].VExtended)
    else
      Values[I] := AFunction.FPrototype.Parameters.TypeSpec[I].CreateValueFromPtr(@AParameters[0].VAnsiString);
  end;
  Result := QuickCall(AFunction, Values);
end;

function TThoriumVirtualMachine.QuickCall(AFunction: TThoriumRuntimeFunction;
  AParameters: array of TThoriumValue): TThoriumValue;

var
  I: Integer;
  Frame: PThoriumStackEntry;
begin
  for I := 0 to High(AParameters) do
  begin
    with (FStack.Push)^ do
    begin
      _Type := stValue;
      Move(AParameters[I], Value, SizeOf(TThoriumValue));
    end;
  end;
  Frame := FStack.Push;

  with Frame^ do
  begin
    _Type := stStackFrame;
    AFunction.FillStackFrame(StackFrame);
    with StackFrame do
    begin
      PreviousStackFrame := -1;
      ReturnAddress := THORIUM_RETURN_EXIT;
      ReturnModule := nil;
      RegisterDumpRange := 0;
      RegisterDumpData := nil;
      DropResult := False;
    end;
  end;
  FCurrentStackFrame := FStack.EntryCount-1;
  Execute(AFunction.FModule, AFunction.FEntryPoint, False);
  if AFunction.FPrototype.HasReturnValue then
  begin
    Move(FStack.GetTop(0)^.Value, Result, SizeOf(TThoriumValue));
    FStack.Pop(1, False);
  end
  else
    FillByte(Result, SizeOf(TThoriumValue), 0);
end;

procedure TThoriumVirtualMachine.DumpStack;
var
  I, J, K: Integer;
  RelPos: String;
  ThisEntry: PThoriumStackEntry;
  PrevStackFrame, NextStackFrame: Integer;
begin
  WriteLn('Stack [[ ');
  J := FStack.EntryCount - 1;
  PrevStackFrame := -1;
  NextStackFrame := -1;
  for K := 0 to FStack.EntryCount - 1 do
  begin
    if FStack.GetTop(K)^._Type = stStackFrame then
    begin
      NextStackFrame := FStack.EntryCount - (K+1);
      Break;
    end;
  end;
  for I := 0 to FStack.EntryCount - 1 do
  begin
    ThisEntry := FStack.GetTop(I);
    if (FCurrentStackFrame < J) and (NextStackFrame = FCurrentStackFrame) then
      RelPos := Format('local($0x%8.8x)', [(J-1) - FCurrentStackFrame])
    else if (FCurrentStackFrame > J) and (FStack.GetStackEntry(0, FCurrentStackFrame)^.StackFrame.ParameterCount >= PrevStackFrame - J ) and (PrevStackFrame = FCurrentStackFrame) then
      RelPos := Format('param($0x%8.8x)', [FCurrentStackFrame - J])
    else if (J = FCurrentStackFrame) then
      RelPos := '(current)'
    else
      RelPos := '';
    case ThisEntry^._Type of
      stValue: WriteLn(Format('  %4d %18s Value (%s) %s', [J, RelPos, ThisEntry^.Value.RTTI.Name, ThisEntry^.Value.RTTI.DoToString(ThisEntry^.Value)]));
      stStackFrame:
      begin
        if ThisEntry^.StackFrame.ReturnAddress = THORIUM_RETURN_EXIT then
          WriteLn(Format('  %4d %18s Stackframe (root)', [J, RelPos]))
        else if ThisEntry^.StackFrame.ReturnModule = nil then
          WriteLn(Format('  %4d %18s Stackframe (return to 0x%8.8x in %s)', [J, RelPos, ThisEntry^.StackFrame.ReturnAddress, FCurrentModule.FName]))
        else
          WriteLn(Format('  %4d %18s Stackframe (return to 0x%8.8x in %s)', [J, RelPos, ThisEntry^.StackFrame.ReturnAddress, ThisEntry^.StackFrame.ReturnModule.FName]));
        PrevStackFrame := J;
        NextStackFrame := -1;
        for K := I+1 to FStack.EntryCount - 1 do
        begin
          if FStack.GetTop(K)^._Type = stStackFrame then
          begin
            NextStackFrame := FStack.EntryCount - (K+1);
            Break;
          end;
        end;
      end;
      stNull: WriteLn(Format('  %4d %18s null', [J, RelPos]));
      stVarargs: WriteLn(Format('  %4d %18s Varargs (%d)', [J, RelPos, MemSize(ThisEntry^.VarargsBuffer.BufferOrigin)]));
    else
      WriteLn(Format('  %4d CORRUPTED', [J]));
    end;
    Dec(J);
  end;
  WriteLn(']]');
end;

procedure TThoriumVirtualMachine.DumpState;
var
  Frame: PThoriumStackEntry;
begin
  WriteLn('State [[');
  WriteLn('  State register [[');
  if FStateRegister and THORIUM_STATE_EQUAL = THORIUM_STATE_EQUAL then
    WriteLn('    equal');
  if FStateRegister and THORIUM_STATE_LESS = THORIUM_STATE_LESS then
    WriteLn('    less');
  if FStateRegister and THORIUM_STATE_GREATER = THORIUM_STATE_GREATER then
    WriteLn('    greater');
  if FStateRegister and THORIUM_STATE_NEG = THORIUM_STATE_NEG then
    WriteLn('    neg');
  if FStateRegister and THORIUM_STATE_ZERO = THORIUM_STATE_ZERO then
    WriteLn('    zero');
  WriteLn('  ]]');
  if FCurrentStackFrame <> -1 then
  begin
    WriteLn('  Current stack frame [[ ');
    WriteLn('    index (int) ', FCurrentStackFrame);
    Frame := FStack.GetStackEntry(0, FCurrentStackFrame);
    WriteLn('    hasReturnValue (bool) ', Frame^.StackFrame.HasReturnValue);
    WriteLn('    paramCount (int) ', Frame^.StackFrame.ParameterCount);
    WriteLn('  ]]');
  end
  else
    WriteLn('  Raw mode (no stack frame)');
  WriteLn('  Current module (string) ', FCurrentModule.FName);
  WriteLn(']]');
end;

function TThoriumVirtualMachine.GetRuntimeFunction(
  const AFunction: TThoriumFunction): TThoriumRuntimeFunction;
var
  Idx: Integer;
begin
  if not FFunctionMap.Find(ptrint(AFunction), Idx) then
    raise EThoriumRuntimeException.CreateFmt('Function ''%s'' was not loaded in the virtual machine.', [AFunction.Name]);
  Result := FFunctionMap.Data[Idx];
end;

function TThoriumVirtualMachine.GetRuntimeModule(
  const ACompiledModule: TThoriumModule): TThoriumRuntimeModule;
var
  Idx: Integer;
begin
  if not FModuleMap.Find(ptrint(ACompiledModule), Idx) then
    raise EThoriumRuntimeException.CreateFmt('Module ''%s'' was not loaded in the virtual machine.', [ACompiledModule.Name]);
  Result := FModuleMap.Data[Idx];
end;

{ TThoriumDebuggingVirtualMachine }

constructor TThoriumDebuggingVirtualMachine.Create(AThorium: TThorium);
begin
  inherited Create(AThorium);
  FBreakpointInstructions := TThoriumIntList.Create;
  FBreakpointLines := TThoriumIntList.Create;
  FOnDebugValue := nil;
  FOnDebugObject := nil;
  FStepping := False;
  FStepMode := smLine;
end;

destructor TThoriumDebuggingVirtualMachine.Destroy;
begin
  FBreakpointLines.Free;
  FBreakpointInstructions.Free;
  inherited Destroy;
end;

function TThoriumDebuggingVirtualMachine.GetRegister(ARegID: TThoriumRegisterID
  ): PThoriumValue;
begin
  Result := @FRegisters[ARegID];
end;

procedure TThoriumDebuggingVirtualMachine.Execute(StartModule: TThoriumRuntimeModule;
  StartInstruction: Integer; CreateDefaultStackFrame: Boolean);
var
  Frame: PThoriumStackEntry;
  {$ifdef DebugToConsole}
  OldIdx: Integer;
  {$endif}
  BreakpointLine: Integer;
  BreakpointLineActive: Boolean;
begin
  if (FOnDebugObject = nil) or (FOnDebugValue = nil) then
    raise EThoriumDebuggerException.Create('Debug callbacks are not defined.');
  FCurrentInstructionIdx := StartInstruction;
  FCurrentModule := StartModule;
  if (FCurrentInstructionIdx > FCurrentModule.FInstructions.Count - 1) or
    (FCurrentInstructionIdx < 0) then
    raise EThoriumRuntimeException.Create('Instruction index out of bounds.');
  if CreateDefaultStackFrame and (FCurrentStackFrame < 0) then
  begin
    FCurrentStackFrame := FStack.EntryCount;
    Frame := FStack.Push;
    Frame^._Type := stStackFrame;
    Frame^.StackFrame.PreviousStackFrame := -1;
    Frame^.StackFrame.ReturnModule := nil;
    Frame^.StackFrame.ReturnAddress := THORIUM_RETURN_EXIT;
    Frame^.StackFrame.RegisterDumpRange := 0;
    Frame^.StackFrame.RegisterDumpData := nil;
    Frame^.StackFrame.HasReturnValue := False;
    Frame^.StackFrame.ParameterCount := 0;
    Frame^.StackFrame.DropResult := False;
  end;
  try
    FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
    repeat
      {$ifdef Stackdump}
      Write('$', IntToHex(FCurrentInstructionIdx, 8), ': ');
      {$endif}
      {$ifdef InstructionDump}
      {$ifndef Stackdump}
      Write('$', IntToHex(FCurrentInstructionIdx, 8), ': '); WriteLn{$else}Write{$endif}(ThoriumInstructionToStr(FCurrentInstruction^), ' ');
      {$endif}
      ExecuteInstruction;
      {$ifdef Stackdump}
      DumpStack;
      WriteLn;
      //ReadLn;
      {$endif}
    until (FCurrentInstructionIdx < 0) or (FCurrentInstructionIdx >= FCurrentModule.FInstructions.Count);
  except
    on E: Exception do
    begin
      // Tell the exception handling system that we want to free the current
      // exception ourselves.
      AcquireExceptionObject;
      // Raise a thorium exception which gives more information about the crash.
      raise EThoriumRuntimeExecutionException.Create(FCurrentModule, FCurrentInstructionIdx, FCurrentInstruction, E);
    end
    else
    begin
      // The exception is not a Exception-based exception, so we cannot get
      // more information about it.
      raise EThoriumRuntimeExecutionException.Create(FCurrentModule, FCurrentInstructionIdx, FCurrentInstruction, nil);
    end;
  end;
end;

procedure TThoriumDebuggingVirtualMachine.StepInto;
begin

end;

procedure TThoriumDebuggingVirtualMachine.StepOver;
begin

end;

{%ENDREGION}

{%REGION 'Thorium core engine' /fold}

var
  Test: PString;

{ TThoriumLibCore }

procedure TThoriumLibCore.InitializeLibrary;
begin
  Test := @FName;
  RegisterType('int', TThoriumTypeInteger.Create(FThorium), [
    TypeInfo(Byte),
    TypeInfo(SmallInt),
    TypeInfo(Word),
    TypeInfo(ShortInt),
    TypeInfo(Cardinal),
    TypeInfo(LongInt),
    TypeInfo(QWord),
    TypeInfo(Int64){,
    TypeInfo(TThoriumInteger)}
  ]);
  RegisterType('string', TThoriumTypeString.Create(FThorium), [
    TypeInfo(UTF8String),
    TypeInfo(AnsiString){,
    TypeInfo(TThoriumString)}
  ]);
  RegisterType('float', TThoriumTypeFloat.Create(FThorium), [
    TypeInfo(Float),
    TypeInfo(Double),
    TypeInfo(Extended){,
    TypeInfo(TThoriumFloat)}
  ]);
  RegisterType('void', nil, []);
  inherited;
end;

class function TThoriumLibCore.GetName: String;
begin
  Result := 'thorium';
end;

{ TThorium }

constructor TThorium.Create;
begin
  inherited Create;
  FModules := TThoriumModules.Create;
  FHostLibraries := TThoriumLibraries.Create;
  //FVirtualMachines := TList.Create;
  FVirtualMachine := nil;
  FOnCompilerOutput := nil;
  FOnRequireModule := nil;
  FOnOpenModule := nil;
  FAnonymousID := 0;
  LoadLibrary(TThoriumLibCore);
  FTypeString := TThoriumTypeString(FHostLibraries[0].FindType('string'));
  FTypeInteger := TThoriumTypeInteger(FHostLibraries[0].FindType('int'));
  FTypeFloat := TThoriumTypeFloat(FHostLibraries[0].FindType('float'));
end;

destructor TThorium.Destroy;
begin
  if FVirtualMachine <> nil then
    FVirtualMachine.Free;
  ClearLibraries;
  FModules.Free;
  FHostLibraries.Free;
  inherited Destroy;
end;

function TThorium.GetLibrary(Index: Integer): TThoriumLibrary;
begin
  Result := TThoriumLibrary(FHostLibraries[Index]);
end;

function TThorium.GetLibraryCount: Integer;
begin
  Result := FHostLibraries.Count;
end;

function TThorium.GetLocked: Boolean;
begin
  Result := FVirtualMachine <> nil;
end;

function TThorium.GetModule(Index: Integer): TThoriumModule;
// Gets the module with index Index.
begin
  Result := TThoriumModule(FModules[Index]);
end;

function TThorium.GetModuleCount: Integer;
// Gets the count of modules.
begin
  Result := FModules.Count;
end;

procedure TThorium.DoCompilerOutput(const Module: TThoriumModule; const Msg: String);
// Process compiler output
begin
  if FOnCompilerOutput <> nil then
    FOnCompilerOutput(Self, Module, Msg);
end;

function TThorium.DoRequireModule(const Name: String; ACompiler: TThoriumCompilerClass; NeededHash: PThoriumHash = nil): TThoriumModule;
// Handle the OnRequireModule event.
var
  ModuleHash: TThoriumHash;
begin
  Result := nil;
  if FOnRequireModule <> nil then
    FOnRequireModule(Self, Name, Result);
  if (Result <> nil) and (NeededHash <> nil) then
  begin
    ModuleHash := Result.GetHash;
    if not CompareMem(@ModuleHash, NeededHash, SizeOf(TThoriumHash)) then
    begin
      raise EThoriumHashException.Create('Hash mismatch for module '''+Result.Name+'''.');
    end;
  end;
  if (Result = nil) then
    Result := LoadModuleFromFile(Name, ACompiler, NeededHash);
end;

function TThorium.DoOpenModule(const ModuleName: String): TStream;
begin
  if Assigned(FOnOpenModule) then
  begin
    Result := nil;
    FOnOpenModule(Self, ModuleName, Result);
  end
  else
  begin
    try
      if not FileExists(ModuleName) then
        Result := nil
      else
        Result := TFileStream.Create(ModuleName, fmOpenRead);
    except
      on E: EFOpenError do
      begin
        raise EFOpenError.CreateFmt('Cannot open module file ''%s''.', [ModuleName]);
      end
      else
        raise;
    end;
  end;
end;

procedure TThorium.ClearLibraries;
// To clear the libraries, it is neccessary to clear the modules too.
var
  I: Integer;
begin
  ClearModules;
  for I := 0 to FHostLibraries.Count - 1 do
    TThoriumLibrary(FHostLibraries[I]).Free;
  FHostLibraries.Clear;
end;

procedure TThorium.ClearModules;
// Deletes all modules.
var
  I: Integer;
begin
  if GetLocked then
    raise EThoriumRuntimeException.Create('Cannot clear modules while engine is in locked state.');
  for I := 0 to FModules.Count - 1 do
    TThoriumModule(FModules[I]).Free;
  FModules.Clear;
end;

function TThorium.FindLibrary(const Name: String): TThoriumLibrary;
var
  CasedName: String;
  I: Integer;
begin
  CasedName := ThoriumCase(Name);
  for I := 0 to FHostLibraries.Count - 1 do
  begin
    Result := FHostLibraries[I];
    if Result.FName = CasedName then
      Exit;
  end;
  Result := nil;
end;

function TThorium.FindModule(const Name: String; AllowLoad: Boolean = True; ACompiler: TThoriumCompilerClass = nil): TThoriumModule;
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    Result := FModules[I];
    if Result.FName = Name then
      Exit;
  end;
  if AllowLoad then
  begin
    Result := DoRequireModule(Name, ACompiler);
    Exit;
  end;
  Result := nil;
end;

function TThorium.IndexOfModule(const AModule: TThoriumModule): Integer;
begin
  Result := FModules.IndexOf(AModule);
end;

procedure TThorium.InitializeVirtualMachine;
begin
  if FVirtualMachine <> nil then
    Exit;
  FVirtualMachine := TThoriumVirtualMachine.Create(Self);
end;

function TThorium.LoadLibrary(const ALibrary: TThoriumLibraryClass): TThoriumLibrary;
begin
  if GetLocked then
    raise EThoriumRuntimeException.Create('Cannot load library while engine is in locked state.');
  if FindLibrary(ALibrary.GetName) <> nil then
    raise EThoriumException.CreateFmt('Host library ''%s'' already loaded.', [ALibrary.GetName]);
  Result := ALibrary.Create(Self);
  FHostLibraries.Add(Result);
end;

function TThorium.LoadModuleFromFile(AModuleName: String; ACompiler: TThoriumCompilerClass; NeededHash: PThoriumHash = nil): TThoriumModule;

  function StripExtension(const AFileName: String): String;
  var
    Ext: String;
  begin
    Ext := ExtractFileExt(AFileName);
    Result := AFileName;
    Delete(Result, Pos(Ext, AFileName), Length(Ext));
  end;

// Try to find a matching file for AModuleName
var
  FS: TStream;
  SourceFS: TStream;
  SourceHash: TThoriumHash;
  ChangedFileName, Buffer: String;
  Head: TThoriumModuleHeader;
  Success: Boolean;
begin
  if GetLocked then
    raise EThoriumRuntimeException.Create('Cannot load modules while engine is in locked state (i.e. while the virtual machine is loaded).');
  // Look for a compiled file
  ChangedFileName := AModuleName + '.tsb';//ChangeFileExt(AModuleName, '.tsb');
  // FileExists interferes with virtual file systems, which I originally wanted
  // to support. So we have to handle exceptions in DoOpenModule correctly...
  // Though. We do not have to. The normal open operation via file stream may
  // check seperately and external handlers should do too.
  Success := True;
  try
    FS := DoOpenModule(ChangedFileName);//TFileStream.Create(ChangedFileName, fmOpenRead);
    if FS = nil then
      Success := False
    else
    begin
      try
        FillByte(Head, SizeOf(TThoriumModuleHeader), 0);
        FS.Read(Head, SizeOf(TThoriumModuleHeader));
        FS.Position := FS.Position - SizeOf(TThoriumModuleHeader);
        if (Head.ID = THORIUM_MODULE_HEADER_ID) and
          (Head.FileVersion = THORIUM_FILE_VERSION) and (NeededHash = nil) then
        begin
          try
            SourceFS := DoOpenModule(AModuleName + '.tss');
            try
              SetLength(Buffer, SourceFS.Size);
              SourceFS.Read(Buffer[1], Length(Buffer));
              SourceHash := TThoriumHash(MD5String(Buffer));
              if (SourceFS.Size <> Head.SourceLength) or (not CompareMem(@SourceHash, @Head.SourceHash, SizeOf(TThoriumHash))) then
              begin
                // Seems that the source has changed... But we will only reload
                // if we can recompile without problems (i.e. NeededHash = nil).
                SourceFS.Free;
                SourceFS := TStringStream.Create(Buffer);
                Result := LoadModuleFromStream(SourceFS, ACompiler, StripExtension(ChangedFileName), nil);
                Result.FSourceFile := AModuleName + '.tss';
              end;
            finally
              SourceFS.Free;
            end;
          except

          end;
        end;
        Result := LoadModuleFromStream(FS, ACompiler, StripExtension(ChangedFileName), NeededHash);
      finally
        FS.Free;
      end;
    end;
  except
    on E: EThoriumException do
    begin
      ChangedFileName := AModuleName + '.tss';
      if FileExists(ChangedFileName) then
      begin
        FS := DoOpenModule(ChangedFileName);
        try
          Result := LoadModuleFromStream(FS, ACompiler, StripExtension(ChangedFileName), NeededHash);
          Result.FSourceFile := AModuleName + '.tss';
        finally
          FS.Free;
        end;
        Exit;
      end
      else
        raise;
    end;
    on E: EFOpenError do
    begin
      // Let the program continue and try with a .tss.
      Success := False;
    end
    else
      raise;
  end;
  if Success then
    Exit;
  // Look for a source file
  try
    if ExtractFileExt(AModuleName) <> '.tss' then
      ChangedFileName := AModuleName + '.tss'
    else
      ChangedFileName := AModuleName;
    FS := DoOpenModule(ChangedFileName);
    if FS <> nil then
    begin
      try
        Result := LoadModuleFromStream(FS, ACompiler, StripExtension(ChangedFileName), NeededHash);
        Result.FSourceFile := AModuleName + '.tss';
      finally
        FS.Free;
      end;
    end
    else
      raise EThoriumException.Create('Could not find module '''+AModuleName+'''.');
  except
    on E: EThoriumException do
    begin
      raise EThoriumException.Create('Could not load module '''+AModuleName+'''. '+E.Message);
    end;
    on E: EFOpenError do
    begin
      raise EThoriumException.Create('Could not find module '''+AModuleName+'''.');
    end;
    on E: Exception do
    begin
      raise EThoriumException.CreateFmt('Could not process module '''+AModuleName+''' (%s: %s).', [E.ClassName, E.Message]);
    end
    else
      raise;
  end;
end;

function TThorium.LoadModuleFromStream(AStream: TStream; ACompiler: TThoriumCompilerClass; AName: String = ''; NeededHash: PThoriumHash = nil): TThoriumModule;
// Create a module, find out which kind of stuff is in the stream and then
// load it.
var
  Start: QWord;
begin
  if GetLocked then
    raise EThoriumRuntimeException.Create('Cannot load a module while engine is in locked state.');
  Start := 0;
  AStream.Read(Start, SizeOf(QWord));
  AStream.Position := AStream.Position - SizeOf(QWord);
  // This is a compiled module!
  if (Start = THORIUM_MODULE_HEADER_ID) then
  begin
    Result := TThoriumModule.Create(Self, AName);
    try
      Result.LoadFromStream(AStream);
    except
      Result.Free;
      raise;
    end;
  end
  else
  begin
    if AName = '' then
    begin
      AName := 'anonymous module #'+IntToStr(FAnonymousID);
      Inc(FAnonymousID);
    end;
    Result := TThoriumModule.Create(Self, AName);
    if (not Result.CompileFromStream(AStream, ACompiler)) or (not Result.Compiled) then
    begin
      try
        raise EThoriumException.Create('Could not compile module '''+AName+''': "'+Result.LastCompilerError+'".');
      except
        Result.Free;
        raise;
      end;
    end;
  end;
  if (NeededHash <> nil) then
  begin
    if not Result.FHashGenerated then
      Result.GetHash;
    if not CompareMem(@Result.FHash, NeededHash, SizeOf(TThoriumHash)) then
    begin
      try
        raise EThoriumHashException.Create('Hash mismatch for module '''+Result.Name+'''.');
      except
        Result.Free;
        raise;
      end;
    end;
  end;
  FModules.Add(Result);
end;

function TThorium.NewModule(AName: String = ''): TThoriumModule;
// Create a new module and return it.
begin
  if GetLocked then
  begin
    Result := nil;
    Exit;
  end;
  if AName = '' then
  begin
    AName := 'anonymous module #'+IntToStr(FAnonymousID);
    Inc(FAnonymousID);
  end;
  Result := TThoriumModule.Create(Self, AName);
  FModules.Add(Result);
end;

procedure TThorium.ReleaseVirtualMachine;
begin
  if FVirtualMachine = nil then
    Exit;
  FVirtualMachine.Free;
  FVirtualMachine := nil;
end;
{%ENDREGION}

var
  // Used for the offset calculation below.
  TestEntry: TThoriumStackEntry;

initialization
// See the comment about the variables in the Native call helpers region.
STACKENTRY_TYPE_OFFSET := Offset(TestEntry, TestEntry.Value.RTTI);
STACKENTRY_DATA_OFFSET := Offset(TestEntry, TestEntry.Value.Int);
STACKENTRY_VADATA_OFFSET := Offset(TestEntry, TestEntry.VarargsBuffer.DataOrigin);

{$ifdef HookSIGUSR1}
  FpSignal(Baseunix.SIGUSR1, @HandleSigUSR1);
{$endif}

end.
