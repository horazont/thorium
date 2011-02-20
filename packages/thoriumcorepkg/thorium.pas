(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thorium.pas
** Last update: 2010-04-23
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
{.$define DebugToConsole}
{$ifdef DebugToConsole}
  // Produce a Thorium stackdump after each instruction.
  {.$define Stackdump}
  // Output the name of each instruction before it gets executed.
  {.$define InstructionDump}
  // Catch SIGUSR1 and print current state information
  {.$define HookSIGUSR1}
{$endif}

{.$define HardDebug}
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
{$hint You may disable the line above BUT THE USAGE OF THORIUM IS ON YOUR OWN RISK THEN. YOU ARE USING A COMPLETELY UNTESTED AND UNSUPORRTED FEATURE!}
{$hint And you damn freak, who has bigendian nowadays AND wants to compile thorium?}
{$note The problem is that I am too lazy to implement the neccessary target location change code in the Precompile function. You will need to modifiy it to make it get the correct offsets of integer. And you need to pray that there won't be any problems with pointers and floats. There I cannot help you.}
{$endif}
{$endif}

interface

uses
  Classes, SysUtils, thorium_globals, thorium_utils, typinfo, Variants, md5,
  contnrs, fgl
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
  TThoriumIdentifierTable = class;
  TThoriumPublicValue = class;
  TThoriumFunction = class;
  TThoriumFunctionCallbackCapsule = class;
  TThoriumVariable = class;
  TThoriumIntList = class;
  TThoriumIntStack = class;
  TThoriumJumpList = class;
  TThoriumHostFunctionBase = class;
  TThoriumHostMethodBase = class;
  TThoriumHostObjectType = class;
  TThoriumRTTIObjectType = class;
  TThoriumScanner = class;
  TThoriumInstructions = class;
  TThoriumModule = class;
  TThoriumLibraryConstant = class;
  TThoriumLibraryProperty = class;
  TThoriumLibrary = class;
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
        constructor Create(Module: TThoriumModule;
          InstructionAddr: TThoriumInstructionAddress; Instruction: PThoriumInstruction;
          OriginalException: Exception);
        destructor Destroy; override;
      private
        FOriginalException: Exception;
      public
        property OriginalException: Exception read FOriginalException;
      end;
    EThoriumDebuggerException = class (EThoriumException);

{%ENDREGION}

(*
   Region: Thorium values and types
   Declarations: TThoriumBuiltInValue, TThoriumHostObjectTypeValue,
                 TThoriumValue, TThoriumValues, TThoriumRegisters, TThoriumType
   Description: The records in here represent complete values and types of
                Thorium.
                                                                              *)
{%REGION 'Thorium values and types' /fold}

  (* This is the built-in part of a TThoriumValue. *)
  TThoriumBuiltInValue = record
  case _Type: TThoriumBuiltInType of
    btInteger:
    (
      Int: TThoriumInteger
    );
    btFloat:
    (
      Float: TThoriumFloat
    );
    btString:
    (
      Str: PThoriumString
    );
  end;

  (* This is the host object type part of a TThoriumValue. *)
  TThoriumHostObjectTypeValue = record
    Value: TThoriumHostObject;
    TypeClass: TThoriumHostObjectType;
//    Size: TThoriumSizeInt;
  end;

  (* Pointer to an host object part of a TThoriumValue. *)
  PThoriumHostObjectTypeValue = ^TThoriumHostObjectTypeValue;

  (* A TThoriumValue represents a value processable by the Thorium engine. It
     is used on the Thorium stack and in the registers of the virtual
     machine. *)
  TThoriumValue = record
  case _Type: TThoriumValueType of
    vtBuiltIn:
    (
      BuiltIn: TThoriumBuiltInValue
    );
    vtExtendedType:
    (
      Extended: TThoriumHostObjectTypeValue
    );
    vtFunction:
    (
      Func: TThoriumFunction
    );
  end;

  (* Pointer to a value processable by Thorium. *)
  PThoriumValue = ^TThoriumValue;

  (* An array of values processable by Thorium. *)
  TThoriumValues = array of TThoriumValue;

  (* A set of values processable by Thorium representing the complete register
     set of a Thorium virtual machine. *)
  TThoriumRegisters = array [0..THORIUM_REGISTER_COUNT-1] of TThoriumValue;

  (* A definition of a type processable by Thorium. *)
  TThoriumType = record
  case ValueType: TThoriumValueType of
    vtBuiltIn:
    (
      BuiltInType: TThoriumBuiltInType;
    );
    vtExtendedType:
    (
      Extended: TThoriumHostObjectType;
    );
    vtFunction:
    (
      Func: TThoriumFunction
    );
    vtHostFunction:
    (
      HostFunc: TThoriumHostFunctionBase;
    );
    vtHostMethod:
    (
      HostMethod: TThoriumHostMethodBase;
    );
  end;

  (* Pointer to a definition of a type processable by Thorium. *)
  PThoriumType = ^TThoriumType;

  (* Pointer to the pointer to a definition of a type processable by Thorium. *)
  PPThoriumType = ^PThoriumType;
{%ENDREGION}

(*
   Region: Stack and identifier table
   Declarations: TThoriumStackEntry, TThoriumTableEntry
   Description: These records are used by the stack and the identifier table.
                                                                              *)
{%REGION 'Stack and identifier table' /fold}

  (* Defines which data a stack entry contains. *)
  TThoriumStackEntryType = (etValue, etStackFrame, etVarargs, etNull);

  (* This record represents one entry on the Thorium stack. It may either
     contain a value, a stack frame or a set of varargs for a function call. *)
  TThoriumStackEntry = record
  case _Type: TThoriumStackEntryType of
    etValue:
    (
      Value: TThoriumValue
    );
    etStackFrame:
    (
      PreviousStackFrame: LongInt;
      ReturnAddress: TThoriumInstructionAddress;
      ReturnModule: LongInt;
      Params: LongInt;
      RetVals: Word;
      RegisterDumpRange: Word;
      RegisterDump: Pointer;
      DropResult: Boolean;
    );
    etVarargs:
    (
      VAData: Pointer;
      VADataOrigin: Pointer;
      VABuffer: Pointer;
      VABufferOrigin: Pointer;
      VAToFree: PThoriumValue;
      VAToFreeOrigin: PThoriumValue;
    );
  end;

  (* Pointer to an entry on the Thorium stack. *)
  PThoriumStackEntry = ^TThoriumStackEntry;

  (* An entry in an identifier table used by the Thorium compiler which defines
     of which kind an identifier is and how it might be found. *)
  TThoriumTableEntry = record
    Name: PString;
    Scope: Integer;
    _Type: TThoriumTableEntryType;
    Offset: Integer; // This is the register of a register variable and the index of a library constant.
    TypeSpec: TThoriumType;
    Value: TThoriumValue;
    Ptr: Pointer;
  end;

  (* A pointer to an entry in an identifier table.*)
  PThoriumTableEntry = ^TThoriumTableEntry;
{%ENDREGION}

(*
   Region: Identifier qualification & relocation
   Declarations: TThoriumHostObjectTypeArray, TThoriumLibraryPropertyArray,
                 TThoriumQualifiedIdentifier, TThoriumRelocation
   Description: These types are used to represent a qualified identifier and how
                to read / write its value. Also the relocation information
                types are declared here.
                                                                              *)
{%REGION 'Identifier qualification & relocation' /fold}

  (* An array of TThoriumHostObjectType mainly used to notify the compiler about
     (possibly relocation needing) uses of an external type. *)
  TThoriumHostObjectTypeArray = array of TThoriumHostObjectType;

  (* An array of TThoriumLibraryProperty mainly used to notify the compiler
     about (possibly relocation needing) uses of a library property. *)
  TThoriumLibraryPropertyArray = array of TThoriumLibraryProperty;

  (* A record containing information about a fully qualified identifier, which
     means that all brackets, dots and square brackets of the identifier
     expression are parsed. It contains also information about used host types
     and library properties and how to read or write the value of the
     identifier. *)
  TThoriumQualifiedIdentifier = record
    FullStr: String;
    Kind: TThoriumQualifiedIdentifierKind;
    IsStatic: Boolean;
    FinalType: TThoriumType;
    Value: TThoriumValue;

    GetJumpMarks: TThoriumIntArray;
    GetCode: TThoriumInstructionArray;
    SetJumpMarks: TThoriumIntArray;
    SetCode: TThoriumInstructionArray;
    UsedExtendedTypes: array of TThoriumHostObjectType;
    UsedLibraryProps: array of TThoriumLibraryProperty;
  end;

  (* This record contains information about a relocation. *)
  TThoriumRelocation = record
    ByteOffset: Cardinal;
    ObjectIndex: Cardinal;
  end;

  (* A pointer to a record containing information about a relocation. *)
  PThoriumRelocation = ^TThoriumRelocation;
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
  TThoriumExternalFunctionVarType = record
    HostType: TThoriumHostType;
    Extended: TThoriumHostObjectType;

    (* If storing is true, the script will call PassControlToHost on
       TThoriumRTTIObject types before the method is called. *)
    Storing: Boolean;
  end;

  (* A pointer to a record which completely defines a type of the host
     environment. *)
  PThoriumExternalFunctionVarType = ^TThoriumExternalFunctionVarType;

  (* This record defines a field of an host environmental record. *)
  TThoriumHostRecordField = record
    FieldType: TThoriumExternalFunctionVarType;
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
    FHashGenerated: Boolean;
  protected
    FHash: TThoriumHash;
  protected
    procedure CalcHash; virtual; abstract;
    procedure InvalidateHash;
  public
    function GetHash: TThoriumHash;
  end;

  (* Only internally used. *)
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
    FList: TFPList;

    function GetParameterCount: Integer;
  protected
    function AddParameter: PThoriumType;
    procedure Clear;
    procedure RemoveParameter(const Index: Integer);
  public
    property Count: Integer read GetParameterCount;
  public
    function Duplicate: TThoriumParameters;
    procedure GetParameterSpec(const Index: Integer; out ParamSpec: TThoriumType);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  { TThoriumPublicValue }

  (* A base class for any symbol published by a module. *)
  TThoriumPublicValue = class (TObject)
    constructor Create(AModule: TThoriumModule); virtual;
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
    constructor Create(AModule: TThoriumModule); override;
    destructor Destroy; override;
  private
    FEntryPoint: Integer;
    FEventCapsules: TFPHashList;
    FNestingLevel: Integer;
    FParameters: TThoriumParameters;
    FPrototyped: Boolean;
    FPrototypedCalls: TThoriumJumpList;
    FReturnValues: TThoriumParameters;
    FVisibilityLevel: TThoriumVisibilityLevel;
  public
    property EntryPoint: Integer read FEntryPoint;
    property NestingLevel: Integer read FNestingLevel;
    property Parameters: TThoriumParameters read FParameters;
    property Prototyped: Boolean read FPrototyped;
    property ReturnValues: TThoriumParameters read FReturnValues;
    property VisibilityLevel: TThoriumVisibilityLevel read FVisibilityLevel;

    function Call(AParameters: array of TThoriumValue): TThoriumValue;
    function Duplicate: TThoriumFunction;
    function AsEvent(AParameters: array of TThoriumHostType;
      ReturnType: TThoriumHostType): TThoriumFunctionCallbackCapsule; overload;
    function AsEvent(AParameters: array of TThoriumHostType;
      ReturnType: TThoriumHostType;
      ExtParameters: array of TThoriumHostObjectType;
      ExtReturnType: TThoriumHostObjectType = nil): TThoriumFunctionCallbackCapsule; overload;
    procedure LoadFromStream(Stream: TStream); override;
    function SafeCall(AParameters: array of TThoriumValue): TThoriumValue;
    procedure SaveToStream(Stream: TStream); override;
  end;

  { TThoriumFunctionCallbackCapsule }

  (* This is for later use. *)
  TThoriumFunctionCallbackCapsule = class (TObject)
  public
    constructor Create(AFunction: TThoriumFunction;
      Parameters: array of TThoriumHostType;
      ReturnType: TThoriumHostType;
      ExtParameters: array of TThoriumHostObjectType;
      ExtReturnType: TThoriumHostObjectType = nil);
  private
    FFunction: TThoriumFunction;
    FInstructions: Pointer;
  public
  end;

  { TThoriumVariable }

  (* A variable published by a module. *)
  TThoriumVariable = class (TThoriumPublicValue)
    constructor Create(AModule: TThoriumModule); override;
  private
    FIsStatic: Boolean;
    FStackPosition: Integer;
    FTypeSpec: TThoriumType;
  public
    property IsStatic: Boolean read FIsStatic;
    property StackPosition: Integer read FStackPosition;
    property TypeSpec: TThoriumType read FTypeSpec;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

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

  { TThoriumHostFunctionParameterSpec }

  (* This class represents a set of host environment types used as a parameter
     specification for host functions and methods. *)
  TThoriumHostFunctionParameterSpec = class (TObject)
    constructor Create;
    destructor Destroy; override;
  private
    FCapacity: Integer;
    FCount: Integer;
    FParams: PThoriumExternalFunctionVarType;
  protected
    procedure Expand;
    function GetCompleteType(AIndex: Integer): PThoriumExternalFunctionVarType;
    function GetExtendedType(AIndex: Integer): TThoriumHostObjectType;
    function GetParamType(AIndex: Integer): TThoriumHostType;
    procedure SetCapacity(AValue: Integer);
    procedure SetExtendedType(AIndex: Integer; AValue: TThoriumHostObjectType);
    procedure SetParamType(AIndex: Integer; AValue: TThoriumHostType);
  public
    property Types[Index: Integer]: TThoriumHostType read GetParamType write SetParamType;
    property ExtendedTypes[Index: Integer]: TThoriumHostObjectType read GetExtendedType write SetExtendedType;
    property CompleteTypes[Index: Integer]: PThoriumExternalFunctionVarType read GetCompleteType;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;

    function AddType(AType: TThoriumHostType): Integer;
    function AddExtendedType(AType: TThoriumHostObjectType): Integer;
    function AllTypes: PThoriumExternalFunctionVarType;
    function IndexOfType(AType: TThoriumHostType; Nth: Integer = 0): Integer;
    procedure InsertType(AType: TThoriumHostType; AIndex: Integer);
    procedure InsertExtendedType(AType: TThoriumHostObjectType; AIndex: Integer);
    procedure DeleteType(AIndex: Integer);
    procedure Clear;
  end;

  { TThoriumHostCallableBase }

  (* A base class for any callable object published by the host environment. *)
  TThoriumHostCallableBase = class (TThoriumHashableObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  protected
    FName: String;
    FParameters: TThoriumHostFunctionParameterSpec;
    FReturnType: TThoriumExternalFunctionVarType;
  protected
    procedure CalcHash; override;
  public
    property Parameters: TThoriumHostFunctionParameterSpec read FParameters;
    property ReturnType: TThoriumExternalFunctionVarType read FReturnType write FReturnType;
    property ReturnTypeExtended: TThoriumHostObjectType read FReturnType.Extended write FReturnType.Extended;
    property ReturnTypeStoring: Boolean read FReturnType.Storing write FReturnType.Storing;
    property Name: String read FName write FName;
  end;

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
    constructor Create; override;
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
    constructor Create; override;
    destructor Destroy; override;
  private
    FCallingConvention: TThoriumNativeCallingConvention;
    FCodePointer: Pointer;
    FInstructions: Pointer;
    FVAOffset: Integer;
  protected
    procedure CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine=nil); override;
  public
    property CallingConvention: TThoriumNativeCallingConvention read FCallingConvention write FCallingConvention;
    property CodePointer: Pointer read FCodePointer write FCodePointer;
    procedure Precompile; virtual;
  end;

  { TThoriumHostMethodAsFunctionNativeCall }

  (* This host function class is able to call any method of the host
     environment directly with a constant first parameter of the type of a
     pointer without the need for a wrapper. *)
  TThoriumHostMethodAsFunctionNativeCall = class (TThoriumHostFunctionNativeCall)
  public
    constructor Create; override;
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
    constructor Create; override;
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
    constructor Create; override;
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
    constructor Create; override;
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
  TThoriumHostObjectType = class (TThoriumHashableObject)
    constructor Create(ALibrary: TThoriumLibrary); virtual;
    destructor Destroy; override;
  private
    FName: String;
  protected
    FLibrary: TThoriumLibrary;
  protected
    function GetNewInstance: Pointer; virtual; abstract;
    function DuplicateValue(const AValue: TThoriumHostObjectTypeValue): TThoriumValue; virtual; abstract;
    function PerformOperation(const AValue1, AValue2: TThoriumValue; const Op: TThoriumOperator): TThoriumValue; virtual; abstract;
    function PerformEvaluation(const AValue: TThoriumHostObjectTypeValue): Integer; virtual; abstract;
    function PerformNegation(const AValue: TThoriumHostObjectTypeValue): TThoriumValue; virtual; abstract;
    function PerformNot(const AValue: TThoriumHostObjectTypeValue): TThoriumValue; virtual; abstract;
    procedure DisposeValue(var AValue: TThoriumHostObjectTypeValue); virtual; abstract;

    function IsTypeCompatible(const Value1, Value2: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; virtual; abstract;
    function IsTypeOperationAvailable(const Value: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; virtual; abstract;

    function HasFields: Boolean; virtual; abstract;
    function HasStaticFields: Boolean; virtual; abstract;
    function HasIndicies: Boolean; virtual; abstract;

    function FindMethod(const AMethodName: String): TThoriumHostMethodBase; virtual;
  public
    property Name: String read FName;
    property Owner: TThoriumLibrary read FLibrary;

    procedure AssignValue(const ASource: TThoriumValue; var ADest: TThoriumValue); virtual;
    procedure ApplyStoring(var AValue: TThoriumHostObjectTypeValue; MayDecreaseReference: Boolean = True); virtual; abstract;

    function FieldID(const FieldIdent: String; out ID: QWord): Boolean; virtual; abstract;
    function StaticFieldID(const FieldIdent: String; out ID: QWord): Boolean; virtual; abstract;
    function IndexType(const InputType: TThoriumType; out ResultType: TThoriumTableEntry): Boolean; virtual; abstract;
    function StaticFieldType(const AFieldID: QWord; out ResultType: TThoriumTableEntry): Boolean; virtual; abstract;
    function FieldType(const AFieldID: QWord; out ResultType: TThoriumTableEntry): Boolean; virtual; abstract;

    function GetIndex(const AInstance: TThoriumValue; const AIndex: TThoriumValue): TThoriumValue; virtual; abstract;
    procedure SetIndex(const AInstance: TThoriumValue; const AIndex: TThoriumValue; const NewValue: TThoriumValue); virtual; abstract;

    function GetField(const AInstance: TThoriumValue; const AFieldID: QWord): TThoriumValue; virtual; abstract;
    function GetStaticField(const AInstance: TThoriumValue; const AFieldID: QWord): TThoriumValue; virtual; abstract;
    procedure SetField(const AInstance: TThoriumValue; const AFieldID: QWord; const NewValue: TThoriumValue); virtual; abstract;
    procedure SetStaticField(const AInstance: TThoriumValue; const AFieldID: QWord; const NewValue: TThoriumValue); virtual; abstract;

    function GetPropertyStoring(const AFieldID: QWord): Boolean; virtual; abstract;
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
    function GetNewInstance: Pointer; override;
    function DuplicateValue(const AValue: TThoriumHostObjectTypeValue): TThoriumValue; override;
    procedure DisposeValue(var AValue: TThoriumHostObjectTypeValue); override;

    function IsTypeCompatible(const Value1, Value2: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; override;
    function IsTypeOperationAvailable(const Value: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; override;

    function HasFields: Boolean; override;
    function HasStaticFields: Boolean; override;
    function HasIndicies: Boolean; override;

    function FindMethod(const AMethodName: String): TThoriumHostMethodBase; override;

    procedure CalcHash; override;
  public
    property BaseClass: TClass read FBaseClass;

    procedure ApplyStoring(var AValue: TThoriumHostObjectTypeValue; MayDecreaseReference: Boolean = True); override;

    function FieldID(const FieldIdent: String; out ID: QWord): Boolean; override;
    function StaticFieldID(const FieldIdent: String; out ID: QWord): Boolean; override;
    function FieldType(const AFieldID: QWord; out ResultType: TThoriumTableEntry): Boolean; override;
    function StaticFieldType(const AFieldID: QWord; out ResultType: TThoriumTableEntry): Boolean; override;

    function GetField(const AInstance: TThoriumValue; const AFieldID: QWord): TThoriumValue; override;
    procedure SetField(const AInstance: TThoriumValue; const AFieldID: QWord; const NewValue: TThoriumValue); override;

    function GetPropertyStoring(const PropertyName: String): Boolean;
    function GetPropertyStoring(const PropInfo: PPropInfo): Boolean;
    function GetPropertyStoring(const AFieldID: QWord): Boolean; override;
    procedure SetPropertyStoring(const PropertyName: String; IsStoring: Boolean = True);
    procedure SetPropertyStoring(const PropInfo: PPropInfo; IsStoring: Boolean = True);

    class function NewNativeCallMethod(const AName: String;
      const ACodePointer: Pointer;
      const AParameters: array of TThoriumHostType;
      const AReturnType: TThoriumHostType = htNone;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostMethodNativeCall;
    class function NewNativeCallStaticMethod(const AName: String;
      const ACodePointer: Pointer; const ADataPointer: Pointer;
      const AParameters: array of TThoriumHostType;
      const AReturnType: TThoriumHostType = htNone;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostMethodAsFunctionNativeCall;
    class function NewNativeCallStaticFunction(const AName: String;
      const ACodePointer: Pointer;
      const AParameters: array of TThoriumHostType;
      const AReturnType: TThoriumHostType = htNone;
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
    function GetNewInstance: Pointer; override;
    function DuplicateValue(const AValue: TThoriumHostObjectTypeValue): TThoriumValue; override;
    procedure DisposeValue(var AValue: TThoriumHostObjectTypeValue); override;

    function IsTypeCompatible(const Value1, Value2: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; override;
    function IsTypeOperationAvailable(const Value: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; override;

    function HasFields: Boolean; override;
    function HasStaticFields: Boolean; override;
    function HasIndicies: Boolean; override;
  public
    function FieldID(const FieldIdent: String; out ID: QWord): Boolean;
       override;
    function FieldType(const AFieldID: QWord; out
       ResultType: TThoriumTableEntry): Boolean; override;

    function GetField(const AInstance: TThoriumValue; const AFieldID: QWord
       ): TThoriumValue; override;
    function GetPropertyStoring(const AFieldID: QWord): Boolean; override;
    procedure SetField(const AInstance: TThoriumValue; const AFieldID: QWord;
       const NewValue: TThoriumValue); override;
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
  end;

  { TThoriumLibraryProperty }

  (* An abstract property exportet by a library. *)
  TThoriumLibraryProperty = class (TThoriumHashableObject)
  public
    constructor Create; virtual;
  private
    FName: String;
  public
    procedure GetValue(const AThoriumValue: PThoriumValue); virtual; abstract;
    function GetStatic: Boolean; virtual; abstract;
    function GetType: TThoriumType; virtual; abstract;
    procedure SetValue(const AThoriumValue: PThoriumValue); virtual; abstract;
  end;
  TThoriumLibraryPropertyClass = class of TThoriumLibraryProperty;

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
    procedure GetValue(const AThoriumValue: PThoriumValue); override;
    function GetStatic: Boolean; override;
    function GetType: TThoriumType; override;
    procedure SetValue(const AThoriumValue: PThoriumValue); override;
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
    procedure SetValue(const AThoriumValue: PThoriumValue); override;
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
    procedure GetValue(const AThoriumValue: PThoriumValue); override;
    function GetStatic: Boolean; override;
    function GetType: TThoriumType; override;
    procedure SetValue(const AThoriumValue: PThoriumValue); override;
  end;

  TThoriumTypeMap = specialize TFPGMap<PTypeInfo, TThoriumHostObjectType>;

  { TThoriumLibrary }

  (* The base class for any library the host environment may want to publish
     to Thorium. To build a library, you need to override the GetName and the
     InitiaizeLibrary methods. For more informations see the thoriumlibpkg
     package and the customlib example. *)
  TThoriumLibrary = class (TObject)
  public
    constructor Create(AThorium: TThorium);
    destructor Destroy; override;
  protected
    FConstants: TFPList;
    FName: String;
    FHostFunctions: TFPList;
    FHostRTTITypes: TFPList;
    FHostTypes: TFPList;
    FHostTypeMap: TThoriumTypeMap;
    FProperties: TFPList;
    FRequiredLibraries: TFPList;
    FThorium: TThorium;
  protected
    function GetConstant(AIndex: Integer): TThoriumLibraryConstant;
    function GetConstantCount: Integer;
    function GetHostFunction(AIndex: Integer): TThoriumHostFunctionBase;
    function GetHostFunctionCount: Integer;
    function GetHostType(AIndex: Integer): TThoriumHostObjectType;
    function GetHostTypeCount: Integer;
    function GetLibraryProperty(AIndex: Integer): TThoriumLibraryProperty;
    function GetLibraryPropertyCount: Integer;
    function GetRTTIType(AIndex: Integer): TThoriumRTTIObjectType;
    function GetRTTITypeCount: Integer;
    procedure PrecompileFunctions;
  protected
    procedure AddDependency(const ALibName: String);
    procedure AddDependency(const ALib: TThoriumLibrary);
    procedure ClearAll;
    procedure ClearFunctions;
    procedure ClearTypes;
    procedure DeleteHostFunction(AIndex: Integer);
    procedure DeleteHostType(AIndex: Integer);
    class function GetName: String; virtual; abstract;
    procedure InitializeLibrary; virtual;
    function RegisterConstant(const AName: String;
      const AValue: TThoriumValue): PThoriumValue;
    function RegisterFinishedObjectType(const AName: String;
      const AInstance: TThoriumHostObjectType; const ATypeInfo: PTypeInfo): TThoriumHostObjectType;
    function RegisterNativeCallFunction(const AName: String;
      const ACodePointer: Pointer; const AParameters: array of TThoriumHostType;
      const AReturnType: TThoriumHostType;
      const ACallingConvention: TThoriumNativeCallingConvention): TThoriumHostFunctionNativeCall;
    function RegisterNativeCallMethodAsFunction(const AName: String;
      const ACodePointer: Pointer; const ADataPointer: Pointer;
      const AParameters: array of TThoriumHostType;
      const AReturnType: TThoriumHostType;
      const ACallingConvention: TThoriumNativeCallingConvention): TThoriumHostMethodAsFunctionNativeCall;
    function RegisterObjectType(const AName: String;
      const ATypeClass: TThoriumHostObjectTypeClass): TThoriumHostObjectType;
    function RegisterPropertyCallback(const AName: String;
      const ATypeSpec: TThoriumType; Static: Boolean;
      const AGetCallback: TThoriumOnPropertyGet;
      const ASetCallback: TThoriumOnPropertySet): TThoriumLibraryPropertyCallback;
    function RegisterPropertyCustom(const AName: String;
      const AClass: TThoriumLibraryPropertyClass): TThoriumLibraryProperty;
    function RegisterPropertyDirect(const AName: String;
      const ATypeSpec: TThoriumType;
      Static: Boolean): TThoriumLibraryPropertyDirect;
    function RegisterPropertyDirectCallback(const AName: String;
      const ATypeSpec: TThoriumType; Static: Boolean;
      Callback: TThoriumOnPropertySetCallback): TThoriumLibraryPropertyDirectSetCallback;
    function RegisterRTTIType(const AClass: TThoriumPersistentClass;
      AbstractClass: Boolean = False): TThoriumRTTIObjectType;
    function RegisterRTTIType(const AClass: TClass;
      AMethodsCallback: TThoriumRTTIMethodsCallback;
      AStaticMethodsCallback: TThoriumRTTIStaticMethodsCallback;
      AbstractClass: Boolean = False): TThoriumRTTIObjectType;
    function RegisterSimpleMethod(const AName: String;
      const AFunction: TThoriumSimpleMethod;
      const AParameters: array of TThoriumHostType;
      const AReturnType: TThoriumHostType): TThoriumHostFunctionSimpleMethod;
  public
    function DeepFindHostType(const AName: String): TThoriumHostObjectType;
    function DeepFindRTTIType(const AName: String): TThoriumRTTIObjectType;
    function DeepFindRTTITypeByClass(const AClass: TClass): TThoriumRTTIObjectType;
    function FindConstant(const AName: String): TThoriumLibraryConstant;
    function FindHostFunction(const AName: String): TThoriumHostFunctionBase;
    function FindHostType(const AName: String): TThoriumHostObjectType;
    function FindHostTypeForType(const AType: PTypeInfo): TThoriumHostObjectType;
    function FindProperty(const AName: String): TThoriumLibraryProperty;
    function FindRTTIType(const AName: String): TThoriumRTTIObjectType;
    function FindRTTITypeByClass(const AClass: TClass): TThoriumRTTIObjectType;
    function IndexOfConstant(const AName: String): Integer;
    function IndexOfHostFunction(const AName: String): Integer;
    function IndexOfHostType(const AName: String): Integer;
    function IndexOfProperty(const AName: String): Integer;
    function IndexOfRTTIType(const AName: String): Integer;
  public
    property Constant[AIndex: Integer]: TThoriumLibraryConstant read GetConstant;
    property ConstantCount: Integer read GetConstantCount;
    property HostFunction[AIndex: Integer]: TThoriumHostFunctionBase read GetHostFunction;
    property HostFunctionCount: Integer read GetHostFunctionCount;
    property HostType[AIndex: Integer]: TThoriumHostObjectType read GetHostType;
    property HostTypeCount: Integer read GetHostTypeCount;
    property LibraryProperty[AIndex: Integer]: TThoriumLibraryProperty read GetLibraryProperty;
    property LibraryPropertyCount: Integer read GetLibraryPropertyCount;
    property RTTIType[AIndex: Integer]: TThoriumRTTIObjectType read GetRTTIType;
    property RTTITypeCount: Integer read GetRTTITypeCount;
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
  public
    property Count: Integer read FCount;
    procedure AddConstantIdentifier(Name: String; Scope: Integer; Offset: Integer; TypeSpec: TThoriumType; Value: TThoriumValue);
    procedure AddVariableIdentifier(Name: String; Scope: Integer; Offset: Integer; TypeSpec: TThoriumType);
    procedure AddRegisterVariableIdentifier(Name: String; RegisterID: TThoriumRegisterID; TypeSpec: TThoriumType);
    procedure AddFunctionIdentifier(Name: String; Func: TThoriumFunction);
    procedure ClearTable;
    function ClearTableTo(NewCount: Integer): Integer;
    function FindIdentifier(Name: String; out Ident: TThoriumTableEntry): Boolean;
  end;

  { TThoriumScanner }

  (* This class tokenizes the source code given and is used internally by the
     compiler. *)
  TThoriumScanner = class (TObject)
    constructor Create(AInputString: String);
    constructor Create(AInputStream: TStream);
    destructor Destroy; override;
  private
    FInputPosition: LongInt;
    FInputSize: LongInt;
    FInputString: String;
    FInputHash: TThoriumHash;
    FCurrentSym: TThoriumSymbol;
    FCurrentStr: String;
    FCurrentChar: Char;
    FCurrentLine: Integer;
    FCurrentX: Integer;
    FIsLineBreak: Boolean;
    FNextSym: TThoriumSymbol;
    FEndOfStream: Boolean;

    procedure Read(var C: Char); inline;
  protected
    procedure ScanForSymbol(var Sym: TThoriumSymbol; var Str: String);
  public
    property CurrentSym: TThoriumSymbol read FCurrentSym;
    property CurrentStr: String read FCurrentStr;
    property CurrentLine: Integer read FCurrentLine;
    // Before enabling this, adapt the Proceed method to fill the FNextSym field
    // property NextSym: TThoriumSymbol read FNextSym;
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
    function DumpCodeStr: String;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

{%ENDREGION}

(*
   Region: Compiler
   Declarations: TThoriumCustomCompiler, TThoriumDefaultCompiler
   Description: These classes are responsible to compile the script source code
                into thorium bytecode.
                                                                              *)

{%REGION 'Compiler' /fold}

  { TThoriumCustomCompiler }

  TThoriumCustomCompiler = class (TObject)
  public
    constructor Create(ATarget: TThoriumModule); virtual;
    destructor Destroy; override;
  protected
    FError: Boolean;
    FHostFuncUsage: TFPList;
    FHostFuncRelocations: TFPList;
    FHostTypeUsage: TFPList;
    FHostTypeRelocations: TFPList;
    FInstructions: TThoriumInstructions;
    FLastError: String;
    FLibPropUsage: TFPList;
    FLibPropRelocations: TFPList;
    FModule: TThoriumModule;
    FOptimizedInstructions: LongInt;
    FPublicFunctions: TFPList;
    FPublicVariables: TFPList;
    FRequiredModules: TThoriumIntList;
    FRequiredLibraries: TThoriumIntList;
    FSourceHash: TThoriumHash;
    FSourceLength: Cardinal;
    FStringLibrary: TStringList;
    FThorium: TThorium;
  protected
    function AddLibraryPropertyUsage(const AProp: TThoriumLibraryProperty): Integer;
    function AddLibraryPropertyUsageToRelocate(const AProp: TThoriumLibraryProperty; const AOffset: ptruint): Integer;
    function AddLibraryString(const AStr: String): Integer;
    function AddHostTypeUsage(const AType: TThoriumHostObjectType): Integer;
    function AddHostTypeUsageToRelocate(const AType: TThoriumHostObjectType; const AOffset: ptruint): Integer;
    function AddHostFunctionUsage(const AFunc: TThoriumHostCallableBase): Integer;
    function AddHostFunctionUsageToRelocate(const AFunc: TThoriumHostCallableBase; const AOffset: ptruint): Integer;
    function AddPublicVariable: TThoriumVariable;
    procedure CompilerError(const Msg: String; X, Y: Integer);
    procedure DumpState; virtual;
    procedure FindRelocationTargets;
    function HasError: Boolean;
    function IsTypeCompatible(Value1, Value2: TThoriumType; Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean;
    function IsTypeOperationAvailable(Value: TThoriumType; Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean;
    procedure OptimizeCode;
  public
    function CompileFromStream(SourceStream: TStream; Flags: TThoriumCompilerFlags = [cfOptimize]): Boolean; virtual; abstract;
  end;

  { TThoriumDefaultCompiler }

  TThoriumDefaultCompiler = class (TThoriumCustomCompiler)
  private
    Scanner: TThoriumScanner;
  protected
    procedure DumpState; override;
  public
    function CompileFromStream(SourceStream: TStream;
       Flags: TThoriumCompilerFlags=[cfOptimize]): Boolean; override;
  end;

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
    FPublicFunctions: TFPList;
    FPublicVariables: TFPList;
    FRequiredModules: TThoriumIntList;
    FRequiredLibraries: TThoriumIntList;
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
    function AddPublicVariable: TThoriumVariable;
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
    function FindHostFunction(const AName: String): TThoriumHostFunctionBase;
    function FindHostObjectType(const AName: String): TThoriumHostObjectType;
    function FindHostRTTIType(const AName: String): TThoriumRTTIObjectType;
    function FindLibraryConstant(const AName: String): TThoriumLibraryConstant;
    function FindLibraryProperty(const AName: String): TThoriumLibraryProperty;
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
    function CompileFromStream(SourceStream: TStream;
      Flags: TThoriumCompilerFlags = [cfOptimize]): Boolean;
    procedure Dump;
    function DumpCodeStr: String;
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

  (* This class executes previously generated Thorium bytecode. *)
  TThoriumVirtualMachine = class (TObject)
    constructor Create(AThorium: TThorium);
    destructor Destroy; override;
  private
    FThorium: TThorium;
    FModuleStackIndicies: TThoriumIntList;

    FRegisters: TThoriumRegisters;
    FStateRegister: Word;

    FCurrentModuleIdx: Integer;
    FCurrentModule: TThoriumModule;
    FCurrentInstructionIdx: Integer;
    FCurrentInstruction: PThoriumInstruction;
    FCurrentStackFrame: Integer;

    function StackScopeToIndex(Scope: Int64): Integer;
    procedure ExecuteInstruction; inline;
  protected
    FStack: TThoriumStack;
  public
    procedure DumpStack;
    function GetStack: TThoriumStack;
    procedure Execute(StartModuleIndex: Integer; StartInstruction: Integer; CreateDefaultStackFrame: Boolean = True); virtual;
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
    procedure Execute(StartModuleIndex: Integer; StartInstruction: Integer;
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

  (* This class manages all modules, libraries and the virtual machine thus
     representing one Thorium context. This class is not a singleton. Several
     instances may exist. *)
  TThorium = class (TObject)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FAnonymousID: Cardinal;
    FHostLibraries: TFPList;
    FModules: TFPList;
    FOnCompilerOutput: TThoriumOnCompilerOutput;
    FOnRequireModule: TThoriumOnRequireModule;
    FOnOpenModule: TThoriumOnOpenModule;
    FVirtualMachine: TThoriumVirtualMachine;
  private
    function GetLibrary(Index: Integer): TThoriumLibrary;
    function GetLibraryCount: Integer;
    function GetLocked: Boolean;
    function GetModule(Index: Integer): TThoriumModule;
    function GetModuleCount: Integer;
  protected
    procedure DoCompilerOutput(const Module: TThoriumModule; const Msg: String); virtual;
    function DoRequireModule(const Name: String; NeededHash: PThoriumHash = nil): TThoriumModule; virtual;
    function DoOpenModule(const ModuleName: String): TStream; virtual;
  public
    procedure ClearLibraries;
    procedure ClearModules;
    function FindLibrary(const Name: String): TThoriumLibrary;
    function FindModule(const Name: String; AllowLoad: Boolean = True): TThoriumModule;
    procedure InitializeVirtualMachine;
    function LoadLibrary(const ALibrary: TThoriumLibraryClass): TThoriumLibrary;
    function LoadModuleFromFile(AModuleName: String; NeededHash: PThoriumHash = nil): TThoriumModule;
    function LoadModuleFromStream(AStream: TStream; AName: String = ''; NeededHash: PThoriumHash = nil): TThoriumModule;
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
    property VirtualMachine: TThoriumVirtualMachine read FVirtualMachine;
  end;

{%ENDREGION}

function ThoriumValueToStr(const Value: TThoriumValue): String;
function ThoriumMakeOOPEvent(ACode: Pointer; Userdata: Pointer): TMethod;
function ThoriumRegisterToStr(ARegisterID: TThoriumRegisterID): String;
function ThoriumInstructionToStr(AInstruction: TThoriumInstruction): String;
procedure ThoriumVarTypeToTypeSpec(VarType: TThoriumHostType; var TypeSpec: TThoriumType);
procedure ThoriumExternalVarTypeToTypeSpec(VarType: PThoriumExternalFunctionVarType; out TypeSpec: TThoriumType);
function ThoriumCreateIntegerValue(const Value: TThoriumInteger): TThoriumValue;
function ThoriumCreateStringValue(const Value: TThoriumString): TThoriumValue;
function ThoriumCreateFloatValue(const Value: TThoriumFloat): TThoriumValue;
function ThoriumCreateExtendedTypeValue(const TypeClass: TThoriumHostObjectType): TThoriumValue;
function ThoriumCreateValue(const ATypeSpec: TThoriumType): TThoriumValue;
function ThoriumCompareType(const Type1, Type2: TThoriumType): Boolean;
function ThoriumDuplicateValue(const Value: TThoriumValue): TThoriumValue;

function HostRecordField(const AType: TThoriumExternalFunctionVarType;
  const AName: String; const AOffset: Cardinal): TThoriumHostRecordField;
function HostVarType(const AHostType: TThoriumHostType;
  const AExtended: TThoriumHostObjectType = nil; const AStoring: Boolean = False): TThoriumExternalFunctionVarType;

implementation

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

{%REGION 'Instruction functions' /fold}
(* These functions are helpers to fill the instruction record for the
   instruction given.*)

function int_s(AValue: Int64): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINT_S(Result) do
  begin
    Instruction := tiINT_S;
    Value := AValue;
    CodeLine := 0;
  end;
end;

function int(AValue: Int64; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINT(Result) do
  begin
    Instruction := tiINT;
    Value := AValue;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function intb(ATRI: Word; AKind: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINTB(Result) do
  begin
    Instruction := tiINTB;
    TRI := ATRI;
    Kind := AKind;
    CodeLine := 0;
  end;
end;

function flt_s(AValue: Double): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFLT_S(Result) do
  begin
    Instruction := tiFLT_S;
    Value := AValue;
    CodeLine := 0;
  end;
end;

function flt(AValue: Double; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFLT(Result) do
  begin
    Instruction := tiFLT;
    Value := AValue;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function str_s(): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTR_S(Result) do
  begin
    Instruction := tiSTR_S;
    CodeLine := 0;
  end;
end;

function strl_s(AIndex: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTRL_S(Result) do
  begin
    Instruction := tiSTRL_S;
    Index := AIndex;
    CodeLine := 0;
  end;
end;

function str(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTR(Result) do
  begin
    Instruction := tiSTR;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function strl(AIndex: Cardinal; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTRL(Result) do
  begin
    Instruction := tiSTRL;
    Index := AIndex;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function ext_s(AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionEXT_S(Result) do
  begin
    Instruction := tiEXT_S;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function ext(AExtendedType: TThoriumHostObjectType; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionEXT(Result) do
  begin
    Instruction := tiEXT;
    ExtendedType := AExtendedType;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function fnc(AFunctionRef: TThoriumFunction; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFNC(Result) do
  begin
    Instruction := tiFNC;
    FunctionRef := AFunctionRef;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function fnce(AFunctionRef: TThoriumHostFunctionBase; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFNCE(Result) do
  begin
    Instruction := tiFNCE;
    FunctionRef := AFunctionRef;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function copyr_s(ASRI: Word; AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_S(Result) do
  begin
    Instruction := tiCOPYR_S;
    SRI := ASRI;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function copyr_st(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_ST(Result) do
  begin
    Instruction := tiCOPYR_ST;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function copyr_fs(ASRI: Word; AModuleIndex: LongInt; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_FS(Result) do
  begin
    Instruction := tiCOPYR_FS;
    SRI := ASRI;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function copys_st(AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYS_ST(Result) do
  begin
    Instruction := tiCOPYS_ST;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function copyfs(AModuleIndex: LongInt; AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYFS(Result) do
  begin
    Instruction := tiCOPYFS;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function copyr(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR(Result) do
  begin
    Instruction := tiCOPYR;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function copys(AScope: Word; AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYS(Result) do
  begin
    Instruction := tiCOPYS;
    Scope := AScope;
    Offset := AOffset;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function moves_s(AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVES_S(Result) do
  begin
    Instruction := tiMOVES_S;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function mover_s(ASRI: Word; AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_S(Result) do
  begin
    Instruction := tiMOVER_S;
    SRI := ASRI;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function mover_st(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_ST(Result) do
  begin
    Instruction := tiMOVER_ST;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function mover(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER(Result) do
  begin
    Instruction := tiMOVER;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function moves(AScope: Word; AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVES(Result) do
  begin
    Instruction := tiMOVES;
    Scope := AScope;
    Offset := AOffset;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function movest(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVEST(Result) do
  begin
    Instruction := tiMOVEST;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function mover_fs(ASRI: Word; AModuleIndex: LongInt; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_FS(Result) do
  begin
    Instruction := tiMOVER_FS;
    SRI := ASRI;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function movefs(AModuleIndex: LongInt; AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVEFS(Result) do
  begin
    Instruction := tiMOVEFS;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function moves_st(AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVES_ST(Result) do
  begin
    Instruction := tiMOVES_ST;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function pop_s(AAmount: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionPOP_S(Result) do
  begin
    Instruction := tiPOP_S;
    Amount := AAmount;
    CodeLine := 0;
  end;
end;

function clr(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCLR(Result) do
  begin
    Instruction := tiCLR;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castif(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTIF(Result) do
  begin
    Instruction := tiCASTIF;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castie(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTIE(Result) do
  begin
    Instruction := tiCASTIE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function castfe(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTFE(Result) do
  begin
    Instruction := tiCASTFE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function castse(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTSE(Result) do
  begin
    Instruction := tiCASTSE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function castei(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTEI(Result) do
  begin
    Instruction := tiCASTEI;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castef(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTEF(Result) do
  begin
    Instruction := tiCASTEF;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castes(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTES(Result) do
  begin
    Instruction := tiCASTES;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function caste(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTE(Result) do
  begin
    Instruction := tiCASTE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function _cast(ASRI: Word; ATRI: Word; ATypeA, ATypeB: TThoriumType): TThoriumInstruction;
const
  typeInteger = 0;
  typeFloat = 1;
  typeString = 2;
  typeExtended = 3;
var
  TypeA, TypeB: Word;
begin
  if ATypeA.ValueType = vtBuiltIn then
  begin
    if ATypeA.BuiltInType = btInteger then
      TypeA := typeInteger
    else if ATypeA.BuiltInType = btFloat then
      TypeA := typeFloat
    else if ATypeA.BuiltInType = btString then
      TypeA := typeString
    else
      Exit;
  end
  else if ATypeA.ValueType = vtExtendedType then
    TypeA := typeExtended
  else
    Exit;
  if ATypeB.ValueType = vtBuiltIn then
  begin
    if ATypeB.BuiltInType = btInteger then
      TypeB := typeInteger
    else if ATypeB.BuiltInType = btFloat then
      TypeB := typeFloat
    else if ATypeB.BuiltInType = btString then
      TypeB := typeString
    else
      Exit;
  end
  else if ATypeB.ValueType = vtExtendedType then
    TypeB := typeExtended
  else
    Exit;
    
  case TypeA of
    typeInteger: case TypeB of
      typeFloat: Result := castif(ASRI, ATRI);
      typeExtended: Result := castie(ASRI, ATRI, ATypeB.Extended);
    end;
    typeFloat: case TypeB of
      typeExtended: Result := castfe(ASRI, ATRI, ATypeB.Extended);
    end;
    typeString: case TypeB of
      typeExtended: Result := castse(ASRI, ATRI, ATypeB.Extended);
    end;
    typeExtended: case TypeB of
      typeInteger: Result := castei(ASRI, ATRI);
      typeFloat: Result := castef(ASRI, ATRI);
      typeString: Result := castes(ASRI, ATRI);
      typeExtended: Result := caste(ASRI, ATRI, ATypeB.Extended);
    end;
  end;
end;

function cmpi(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPI(Result) do
  begin
    Instruction := tiCMPI;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpif(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPIF(Result) do
  begin
    Instruction := tiCMPIF;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpie(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPIE(Result) do
  begin
    Instruction := tiCMPIE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpf(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPF(Result) do
  begin
    Instruction := tiCMPF;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpfi(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPFI(Result) do
  begin
    Instruction := tiCMPFI;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpfe(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPFE(Result) do
  begin
    Instruction := tiCMPFE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmps(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPS(Result) do
  begin
    Instruction := tiCMPS;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpse(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPSE(Result) do
  begin
    Instruction := tiCMPSE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpe(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPE(Result) do
  begin
    Instruction := tiCMPE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpei(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPEI(Result) do
  begin
    Instruction := tiCMPEI;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpef(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPEF(Result) do
  begin
    Instruction := tiCMPEF;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpes(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPES(Result) do
  begin
    Instruction := tiCMPES;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function addi(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionADDI(Result) do
  begin
    Instruction := tiADDI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function addf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionADDF(Result) do
  begin
    Instruction := tiADDF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function adds(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionADDS(Result) do
  begin
    Instruction := tiADDS;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function subi(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSUBI(Result) do
  begin
    Instruction := tiSUBI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function subf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSUBF(Result) do
  begin
    Instruction := tiSUBF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function muli(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMULI(Result) do
  begin
    Instruction := tiMULI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function mulf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMULF(Result) do
  begin
    Instruction := tiMULF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function divi(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDIVI(Result) do
  begin
    Instruction := tiDIVI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function divf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDIVF(Result) do
  begin
    Instruction := tiDIVF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function negi(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNEGI(Result) do
  begin
    Instruction := tiNEGI;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function negf(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNEGF(Result) do
  begin
    Instruction := tiNEGF;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function _not(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNOT(Result) do
  begin
    Instruction := tiNOT;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function bnot(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionBNOT(Result) do
  begin
    Instruction := tiBNOT;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function _mod(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOD(Result) do
  begin
    Instruction := tiMOD;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _and(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionAND(Result) do
  begin
    Instruction := tiAND;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _or(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionOR(Result) do
  begin
    Instruction := tiOR;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _xor(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXOR(Result) do
  begin
    Instruction := tiXOR;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _shl(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSHL(Result) do
  begin
    Instruction := tiSHL;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _shr(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSHR(Result) do
  begin
    Instruction := tiSHR;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _operator(AOp1: Word; AOp2: Word; ATRI: Word; AType: TThoriumType; AKind: TThoriumOperation): TThoriumInstruction;
begin
  case AKind of
    toAdd, toSubtract, toMultiply, toDivide, toDiv:
    begin
      if (AType.ValueType = vtBuiltIn) then
      begin
        if AType.BuiltInType = btInteger then
        begin
          case AKind of
            toAdd: Result := addi(AOp1, AOp2, ATRI);
            toSubtract: Result := subi(AOp1, AOp2, ATRI);
            toMultiply: Result := muli(AOp1, AOp2, ATRI);
            toDivide, toDiv: Result := divi(AOp1, AOp2, ATRI);
          end;
        end
        else if AType.BuiltInType = btFloat then
        begin
          case AKind of
            toAdd: Result := addf(AOp1, AOp2, ATRI);
            toSubtract: Result := subf(AOp1, AOp2, ATRI);
            toMultiply: Result := mulf(AOp1, AOp2, ATRI);
            toDivide: Result := divf(AOp1, AOp2, ATRI);
          end;
        end
        else if AType.BuiltInType = btString then
        begin
          if AKind = toAdd then
            Result := adds(AOp1, AOp2, ATRI);
        end;
      end;
    end;
    toMod, toAnd, toOr, toXor:
    begin
      if (AType.ValueType = vtBuiltIn) and (AType.BuiltInType = btInteger) then
      begin
        case AKind of
          toMod: Result := _mod(AOp1, AOp2, ATRI);
          toAnd: Result := _and(AOp1, AOp2, ATRI);
          toOr: Result := _or(AOp1, AOp2, ATRI);
          toXor: Result := _xor(AOp1, AOp2, ATRI);
          toShl: Result := _shl(AOp1, AOp2, ATRI);
          toShr: Result := _shr(AOp1, AOp2, ATRI);
        end;
      end;
    end;
    toNegate, toNot, toBoolNot:
    begin
      if AOp1 <> ATRI then
        raise EThoriumCompilerException.Create('Compiler mistake. Keywords: _operand, toNegate-toNot-toBoolNot, AOp1-ATRI');
      if (AType.ValueType = vtBuiltIn) and (AType.BuiltInType = btInteger) then
      begin
        case AKind of
          toNot: Result := _not(AOp1);
          toNegate: Result := negi(AOp1);
          toBoolNot: Result := bnot(AOp1);
        end;
      end;
      if (AType.ValueType = vtBuiltIn) and (AType.BuiltInType = btFloat) then
      begin
        case AKind of
          toNegate: Result := negf(AOp1);
        end;
      end;
    end;
  else
    raise EThoriumCompilerException.Create('Compile mistake. Keywords: _operand, AKind');
  end;
end;

function _cmpOperator(AOp1, AOp2: Word; ATypeA, ATypeB: TThoriumType): TThoriumInstruction;
const
  typeInteger = 0;
  typeFloat = 1;
  typeString = 2;
  typeExtended = 3;
var
  TypeA, TypeB: Word;
begin
  if ATypeA.ValueType = vtBuiltIn then
  begin
    if ATypeA.BuiltInType = btInteger then
      TypeA := typeInteger
    else if ATypeA.BuiltInType = btFloat then
      TypeA := typeFloat
    else if ATypeA.BuiltInType = btString then
      TypeA := typeString
    else
      Exit;
  end
  else if ATypeA.ValueType = vtExtendedType then
    TypeA := typeExtended
  else
    Exit;
  if ATypeB.ValueType = vtBuiltIn then
  begin
    if ATypeB.BuiltInType = btInteger then
      TypeB := typeInteger
    else if ATypeB.BuiltInType = btFloat then
      TypeB := typeFloat
    else if ATypeB.BuiltInType = btString then
      TypeB := typeString
    else
      Exit;
  end
  else if ATypeB.ValueType = vtExtendedType then
    TypeB := typeExtended
  else
    Exit;

  case TypeA of
    typeInteger: case TypeB of
      typeInteger: Result := cmpi(AOp1, AOp2);
      typeFloat: Result := cmpif(AOp1, AOp2);
      typeExtended: Result := cmpie(AOp1, AOp2);
    end;
    typeFloat: case TypeB of
      typeInteger: Result := cmpfi(AOp1, AOp2);
      typeFloat: Result := cmpf(AOp1, AOp2);
      typeExtended: Result := cmpfe(AOp1, AOp2);
    end;
    typeString: case TypeB of
      typeString: Result := cmps(AOp1, AOp2);
      typeExtended: Result := cmpse(AOp1, AOp2);
    end;
    typeExtended: case TypeB of
      typeInteger: Result := cmpei(AOp1, AOp2);
      typeFloat: Result := cmpef(AOp1, AOp2);
      typeString: Result := cmpes(AOp1, AOp2);
      typeExtended: Result := cmpe(AOp1, AOp2);
    end;
  end;

end;

function inci_s(AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCI_S(Result) do
  begin
    Instruction := tiINCI_S;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function incf_s(AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCF_S(Result) do
  begin
    Instruction := tiINCF_S;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function inci_fs(AModuleIndex: LongInt; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCF_FS(Result) do
  begin
    Instruction := tiINCF_FS;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function incf_fs(AModuleIndex: LongInt; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCF_FS(Result) do
  begin
    Instruction := tiINCF_FS;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function inci(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCI(Result) do
  begin
    Instruction := tiINCI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function incf(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCF(Result) do
  begin
    Instruction := tiINCF;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function deci_s(AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECI_S(Result) do
  begin
    Instruction := tiDECI_S;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function decf_s(AScope: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECF_S(Result) do
  begin
    Instruction := tiDECF_S;
    Scope := AScope;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function deci_fs(AModuleIndex: LongInt; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECF_FS(Result) do
  begin
    Instruction := tiDECF_FS;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function decf_fs(AModuleIndex: LongInt; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECF_FS(Result) do
  begin
    Instruction := tiDECF_FS;
    ModuleIndex := AModuleIndex;
    Offset := AOffset;
    CodeLine := 0;
  end;
end;

function deci(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECI(Result) do
  begin
    Instruction := tiDECI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function decf(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECF(Result) do
  begin
    Instruction := tiDECF;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xpget(AProp: TThoriumLibraryProperty; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXPGET(Result) do
  begin
    Instruction := tiXPGET;
    Prop := AProp;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xpset(AProp: TThoriumLibraryProperty; ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXPSET(Result) do
  begin
    Instruction := tiXPSET;
    Prop := AProp;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function xfget(AID: Int64; AERI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXFGET(Result) do
  begin
    Instruction := tiXFGET;
    ID := AID;
    ERI := AERI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xfset(AID: Int64; AERI: Word; AVRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXFSET(Result) do
  begin
    Instruction := tiXFSET;
    ID := AID;
    ERI := AERI;
    VRI := AVRI;
    CodeLine := 0;
  end;
end;

function xsfget(AID: Int64; AExtendedType: TThoriumHostObjectType; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXSFGET(Result) do
  begin
    Instruction := tiXSFGET;
    ID := AID;
    ExtendedType := AExtendedType;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xsfset(AID: Int64; AExtendedType: TThoriumHostObjectType; AVRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXSFSET(Result) do
  begin
    Instruction := tiXSFSET;
    ID := AID;
    ExtendedType := AExtendedType;
    VRI := AVRI;
    CodeLine := 0;
  end;
end;

function xiget(AIRI: Word; AERI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXIGET(Result) do
  begin
    Instruction := tiXIGET;
    IRI := AIRI;
    ERI := AERI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xiset(AIRI: Word; AVRI: Word; AERI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXISET(Result) do
  begin
    Instruction := tiXISET;
    IRI := AIRI;
    VRI := AVRI;
    ERI := AERI;
    CodeLine := 0;
  end;
end;

function xct(AERI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXCT(Result) do
  begin
    Instruction := tiXCT;
    ERI := AERI;
    CodeLine := 0;
  end;
end;

function vastart(ALength: Cardinal; AIsPointerBased: Boolean): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVASTART(Result) do
  begin
    Instruction := tiVASTART;
    Length := ALength;
    if AIsPointerBased then
      Pointers := 1
    else
      Pointers := 0;
    CodeLine := 0;
  end;
end;

function va_i8(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I8(Result) do
  begin
    Instruction := tiVA_I8;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i16(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I16(Result) do
  begin
    Instruction := tiVA_I16;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i32(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I32(Result) do
  begin
    Instruction := tiVA_I32;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i64(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I64(Result) do
  begin
    Instruction := tiVA_I64;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i8s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I8S(Result) do
  begin
    Instruction := tiVA_I8S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i16s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I16S(Result) do
  begin
    Instruction := tiVA_I16S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i32s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I32S(Result) do
  begin
    Instruction := tiVA_I32S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i64s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I64S(Result) do
  begin
    Instruction := tiVA_I64S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_f32(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_F32(Result) do
  begin
    Instruction := tiVA_F32;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_f64(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_F64(Result) do
  begin
    Instruction := tiVA_F64;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_f80(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_F80(Result) do
  begin
    Instruction := tiVA_F80;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_S(Result) do
  begin
    Instruction := tiVA_S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_x(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_X(Result) do
  begin
    Instruction := tiVA_X;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vastart_t(ALength: Cardinal; AFloatCount: Cardinal; AToClear: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVASTART_T(Result) do
  begin
    Instruction := tiVASTART_T;
    Length := ALength;
    Floats := AFloatCount;
    ToClear := AToClear;
    CodeLine := 0;
  end;
end;

function vat_f(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_F(Result) do
  begin
    Instruction := tiVAT_F;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vat_i(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_I(Result) do
  begin
    Instruction := tiVAT_I;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vat_s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_S(Result) do
  begin
    Instruction := tiVAT_S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vat_x(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_X(Result) do
  begin
    Instruction := tiVAT_X;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vafinish(): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAFINISH(Result) do
  begin
    Instruction := tiVAFINISH;
    CodeLine := 0;
  end;
end;

function jmp(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJMP(Result) do
  begin
    Instruction := tiJMP;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function je(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJE(Result) do
  begin
    Instruction := tiJE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jne(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJNE(Result) do
  begin
    Instruction := tiJNE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jgt(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJGT(Result) do
  begin
    Instruction := tiJGT;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jge(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJGE(Result) do
  begin
    Instruction := tiJGE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jlt(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJLT(Result) do
  begin
    Instruction := tiJLT;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jle(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJLE(Result) do
  begin
    Instruction := tiJLE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function call(AEntryPoint: TThoriumInstructionAddress; AHRI: Word; ARetVal: Word; AParameters: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCALL(Result) do
  begin
    Instruction := tiCALL;
    EntryPoint := AEntryPoint;
    HRI := AHRI;
    RetVal := ARetVal;
    Parameters := AParameters;
    CodeLine := 0;
  end;
end;

function fcall(AEntryPoint: TThoriumInstructionAddress; AModuleIndex: LongInt; AHRI: Word; ARetVal: Word; AParameters: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFCALL(Result) do
  begin
    Instruction := tiFCALL;
    ModuleIndex := AModuleIndex;
    EntryPoint := AEntryPoint;
    HRI := AHRI;
    RetVal := ARetVal;
    Parameters := AParameters;
    CodeLine := 0;
  end;
end;

function xcall(AFunctionRef: TThoriumHostFunctionBase): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXCALL(Result) do
  begin
    Instruction := tiXCALL;
    FunctionRef := AFunctionRef;
    CodeLine := 0;
  end;
end;

function xcall_m(AMethodRef: TThoriumHostMethodBase; ARTTIValueRegister: TThoriumRegisterID): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXCALL_M(Result) do
  begin
    Instruction := tiXCALL_M;
    MethodRef := AMethodRef;
    RTTIValueRegister := ARTTIValueRegister;
    CodeLine := 0;
  end;
end;

function ret(): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionRET(Result) do
  begin
    Instruction := tiRET;
    CodeLine := 0;
  end;
end;

function noop(AKind: Word; AParameter1, AParameter2: Int64; AParameter3: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNOOP(Result) do
  begin
    Instruction := tiNOOP;
    Kind := AKind;
    Parameter1 := AParameter1;
    Parameter2 := AParameter2;
    Parameter3 := AParameter3;
    CodeLine := 0;
  end;
end;
{%ENDREGION}

{%REGION 'Native call helpers' /fold}
var
  // These offsets are needed by GenericPrecompile. Don't ask me why there is no
  // possibility to let the compiler do the work.
  STACKENTRY_VALUE_OFFSET: SizeUInt;
  VALUE_BUILTIN_OFFSET: SizeUInt;
  BUILTIN_VALUE_OFFSET: SizeUInt;
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
procedure GenericPrecompile(var AInstructions: Pointer; var AVAOffset: Integer; Parameters: TThoriumHostFunctionParameterSpec; ReturnType: TThoriumExternalFunctionVarType; HasData: Boolean; CallingConvention: TThoriumNativeCallingConvention);
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

  function ValueBaseOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := EntryBaseOffset(AIndex) - STACKENTRY_VALUE_OFFSET;
  end;

  function BuiltInBaseOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := ValueBaseOffset(AIndex) - VALUE_BUILTIN_OFFSET;
  end;

  function IntOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := BuiltInBaseOffset(AIndex) - BUILTIN_VALUE_OFFSET;
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
              Offset := BuiltInBaseOffset((ParamCount-(I+1))+VAOffset);
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
          Offset := BuiltInBaseOffset(ParamCount+PreCalcVAOffset);
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
              Offset := BuiltInBaseOffset((ParamCount-(I+1))+VAOffset);
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
          Offset := BuiltInBaseOffset(ParamCount+PreCalcVAOffset);
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
end;

procedure GenericCallbackPrecompile(var Instructions: Pointer; Parmeters: array of TThoriumHostType; ReturnType: TThoriumHostType);
begin

end;

{$ifdef CPU32}

procedure ExecuteSubscript(Instructions: Pointer; StackTop: Pointer; Method: Pointer; Data: Pointer = nil); stdcall;
// up to 3 ints in eax ecx edx
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

procedure ExecuteSubscript(Instructions: Pointer; StackTop: Pointer; Method: Pointer; Data: Pointer = nil);
// * up to 6 ints (or similar) in registers: rdi, rsi, rdx, rcx, r8, r9
// * up to 8 doubles (or singles) in registers: xmm0 through xmm7
// * extendeds are segmented and pushed onto the stack
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
    @iDecStack, @iRevSingle, @iRevExtended, @lExit
  );
var
  IntRegPtr: Pointer = @IntReg;
  SingleRegPtr: Pointer = @SingleReg;
  DoubleRegPtr: Pointer = @DoubleReg;
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

function ThoriumDerefStr(S: PString): String; inline;
begin
  if S = nil then
    Result := ''
  else
    Result := S^;
end;

function ThoriumConcatStr(S1, S2: PString): String; inline;
begin
  Result := ThoriumDerefStr(S1) + ThoriumDerefStr(S2);
end;

function ThoriumCreateBuiltInValue(const BuiltInType: TThoriumBuiltInType): TThoriumValue;
// Creates a thorium variable with a built-in-type.
begin
  Result._Type := vtBuiltIn;
  Result.BuiltIn._Type := BuiltInType;
  with Result.BuiltIn do
  begin
    case _Type of
      btInteger: Int := 0;
      btFloat: Float := 0.0;
      btString: New(Str);
    end;
  end;
end;

procedure ThoriumExternalVarTypeToTypeSpec(
  VarType: PThoriumExternalFunctionVarType; out TypeSpec: TThoriumType);
begin
  if VarType^.HostType and (htSizeSection or htTypeSection) = htExt then
  begin
    TypeSpec.ValueType := vtExtendedType;
    TypeSpec.Extended := VarType^.Extended;
  end
  else
    ThoriumVarTypeToTypeSpec(VarType^.HostType, TypeSpec);
end;

function ThoriumCreateIntegerValue(const Value: TThoriumInteger): TThoriumValue;
// Creates an integer value
begin
  Result._Type := vtBuiltIn;
  Result.BuiltIn._Type := btInteger;
  Result.BuiltIn.Int := Value;
end;

function ThoriumCreateFloatValue(const Value: TThoriumFloat): TThoriumValue;
// Creates a float value
begin
  Result._Type := vtBuiltIn;
  Result.BuiltIn._Type := btFloat;
  Result.BuiltIn.Float := Value;
end;

function ThoriumCreateStringValue(const Value: TThoriumString): TThoriumValue;
// Creates a string value
begin
  Result._Type := vtBuiltIn;
  Result.BuiltIn._Type := btString;
  New(Result.BuiltIn.Str);
  Result.BuiltIn.Str^ := Value;
end;

function ThoriumCreateExtendedTypeValue(const TypeClass: TThoriumHostObjectType): TThoriumValue;
// Creates a thorium variable with a extended type.
begin
  Result._Type := vtExtendedType;
  Result.Extended.TypeClass := TypeClass;
  Result.Extended.Value := TypeClass.GetNewInstance;
end;

function ThoriumCreateValue(const ATypeSpec: TThoriumType): TThoriumValue;
begin
  Result._Type := ATypeSpec.ValueType;
  case Result._Type of
    vtBuiltIn:
    begin
      Result.BuiltIn._Type := ATypeSpec.BuiltInType;
      case Result.BuiltIn._Type of
        btInteger: Result.BuiltIn.Int := 0;
        btFloat: Result.BuiltIn.Float := 0.0;
        btString: New(Result.BuiltIn.Str);
      end;
    end;
    vtExtendedType:
    begin
      Result.Extended.TypeClass := ATypeSpec.Extended;
      //Result.Extended.Size := Result.Extended.TypeClass.GetNeededMemoryAmount;
      Result.Extended.Value := Result.Extended.TypeClass.GetNewInstance;
    end;
  end;
end;

function ThoriumDuplicateValue(const Value: TThoriumValue): TThoriumValue;
// Takes the value given and duplicates it's contents.
begin
  Result._Type := Value._Type;
  case Value._Type of
    vtBuiltIn:
    begin
      Result.BuiltIn._Type := Value.BuiltIn._Type;
      case Value.BuiltIn._Type of
        btInteger: Result.BuiltIn.Int := Value.BuiltIn.Int;
        btFloat: Result.BuiltIn.Float := Value.BuiltIn.Float;
        btString:
        begin
          New(Result.BuiltIn.Str);
          Result.BuiltIn.Str^ := Value.BuiltIn.Str^;
          (*if Value.BuiltIn.Str = nil then
            Result.BuiltIn.Str := nil
          else
            New(Result.BuiltIn.Str);*)
        end;
      end;
    end;
    vtExtendedType:
    begin
      Result := Value.Extended.TypeClass.DuplicateValue(Value.Extended);
    end;
    vtFunction:
      Result.Func := Value.Func;
  end;
end;

function ThoriumExtractTypeSpec(const Value: TThoriumValue): TThoriumType;
// Creates a TThoriumType-structure based on the type definition in the
// TThoriumValue record.
begin
  Result.ValueType := Value._Type;
  case Value._type of
    vtBuiltIn: Result.BuiltInType := Value.BuiltIn._Type;
    vtExtendedType: Result.Extended := Value.Extended.TypeClass;
  end;
end;

function ThoriumBuiltInTypeSpec(const Value: TThoriumBuiltInType): TThoriumType;
// Returns a TThoriumType which contains a BuiltIn type with the given type.
begin
  Result.ValueType := vtBuiltIn;
  Result.BuiltInType := Value;
end;

function ThoriumHostObjectTypeSpec(const Value: TThoriumHostObjectType): TThoriumType;
// Returns a TThoriumType which contains an extended type with the given type.
begin
  Result.ValueType := vtExtendedType;
  Result.Extended := Value;
end;

function ThoriumNilTypeSpec: TThoriumType;
// Returns a nil type spec (e.g. for void returns)
begin
  Result.ValueType := vtBuiltIn;
  Result.BuiltInType := btNil;
end;

function ThoriumCompareType(const Type1, Type2: TThoriumType): Boolean;
begin
  //Result := CompareMem(@Type1, @Type2, SizeOf(TThoriumType));
  if Type1.ValueType <> Type2.ValueType then
  begin
    Result := False;
    Exit;
  end;
  case Type1.ValueType of
    vtBuiltIn: Result := Type1.BuiltInType = Type2.BuiltInType;
    vtExtendedType: Result := Type1.Extended = Type2.Extended;
    vtFunction: Result := Type1.Func = Type2.Func;
    vtHostFunction: Result := Type1.HostFunc = Type2.HostFunc;
  end;
end;

function ThoriumCompareTypeEx(const AssignType1, ToType2: TThoriumType): Boolean;
begin
  if AssignType1.ValueType <> ToType2.ValueType then
  begin
    Result := False;
    Exit;
  end;
  case AssignType1.ValueType of
    vtBuiltIn: Result := AssignType1.BuiltInType = ToType2.BuiltInType;
    vtExtendedType:
    begin
      if (not (AssignType1.Extended is TThoriumRTTIObjectType)) or (not (ToType2.Extended is TThoriumRTTIObjectType)) then
        Result := AssignType1.Extended = ToType2.Extended
      else
      begin
        Result := (TThoriumRTTIObjectType(AssignType1.Extended).BaseClass = TThoriumRTTIObjectType(ToType2.Extended).BaseClass)
          or (TThoriumRTTIObjectType(AssignType1.Extended).BaseClass.InheritsFrom(TThoriumRTTIObjectType(ToType2.Extended).BaseClass));
      end;
    end;
    vtFunction: Result := AssignType1.Func = ToType2.Func;
    vtHostFunction: Result := AssignType1.HostFunc = ToType2.HostFunc;
  end;
end;

function ThoriumTypeName(const TypeSpec: TThoriumType): String; overload;
// Returns the type name of the given TThoriumType record.
begin
  case TypeSpec.ValueType of
    vtBuiltIn:
    begin
      case TypeSpec.BuiltInType of
        btUnknown: Result := 'Unknown';
        btNil: Result := 'nil';
        btInteger: Result := 'int';
        btFloat: Result := 'float';
        btString: Result := 'string';
      else
        Result := 'errornous builtin type';
      end;
    end;
    vtExtendedType:
    begin
      Result := TypeSpec.Extended.FName;
    end;
    vtFunction:
    begin
      Result := 'function';
    end;
  else
    Result := 'errornous type';
  end;
end;

function ThoriumTypeName(const Value: TThoriumValue): String; overload;
// Returns the type name of the value given.
begin
  Result := ThoriumTypeName(ThoriumExtractTypeSpec(Value));
end;

function ThoriumValueToStr(const Value: TThoriumValue): String;
begin
  case Value._Type of
    vtBuiltIn: case Value.BuiltIn._Type of
      btNil: Result := 'nil';
      btUnknown: Result := 'unknown';
      btInteger: Result := Format('0x%.16x', [Value.BuiltIn.Int]);
      btFloat: Result := Format('%.8ff', [Value.BuiltIn.Float]);
      btString:
      begin
        if Value.BuiltIn.Str = nil then
          Result := '""'
        else
          Result := Format('"%s"', [Value.BuiltIn.Str^]);
      end;
      //btArray: Result := Format('array(%d)', [Value.BuiltIn.ElementCount]);
    else
      Result := '(unknown builtin type)';
    end;
    vtExtendedType: Result := Format('extended(%s)', [Value.Extended.TypeClass.FName]);
    vtFunction: Result := 'function';
    vtHostFunction: Result := 'external function';
  else
    Result := '(unknown type)';
  end;
end;

function ThoriumSymbolToOperation(const Sym: TThoriumSymbol): TThoriumOperation;
begin
  case Sym of
    tsPlus, tsAdditiveAssign: Result := toAdd;
    tsMinus, tsSubtractiveAssign: Result := toSubtract;
    tsMultiply, tsMultiplicativeAssign: Result := toMultiply;
    tsDivide, tsDivideAssign: Result := toDivide;
    tsAnd: Result := toAnd;
    tsOr: Result := toOr;
    tsXor: Result := toXor;
    tsDiv: Result := toDiv;
    tsMod: Result := toMod;
    tsShl: Result := toShl;
    tsShr: Result := toShr;
    tsAssign: Result := toAssign;
    tsEqual, tsNotEqual, tsLesser, tsGreater, tsLesserEqual, tsGreaterEqual: Result := toCompare;
  end;
end;

function ThoriumSymbolToOperator(const Sym: TThoriumSymbol): TThoriumOperator;
begin
  case Sym of
    tsPlus, tsAdditiveAssign: Result := tpAdd;
    tsMinus, tsSubtractiveAssign: Result := tpSubtract;
    tsMultiply, tsMultiplicativeAssign: Result := tpMultiply;
    tsDivide, tsDivideAssign: Result := tpDivide;
    tsAnd: Result := tpAnd;
    tsOr: Result := tpOr;
    tsXor: Result := tpXor;
    tsDiv: Result := tpDiv;
    tsMod: Result := tpMod;
    tsEqual: Result := tpEqual;
    tsNotEqual: Result := tpNotEqual;
    tsLesser: Result := tpLess;
    tsGreater: Result := tpGreater;
    tsLesserEqual: Result := tpLessEqual;
    tsGreaterEqual: Result := tpGreaterEqual;
  end;
end;

function ThoriumValueToVariant(const AThoriumValue: TThoriumValue;
  out Value: Variant; const VarType: TThoriumHostType = htNone): Boolean;
begin
  case AThoriumValue._Type of
    vtBuiltIn: case AThoriumValue.BuiltIn._Type of
      btInteger:
      begin
        Result := (VarType in [htIntS8, htIntU8, htIntS16, htIntU16, htIntS32,
          htIntU32, htIntS64, htIntU64, htAny]);
        if Result then
          Value := AThoriumValue.BuiltIn.Int;
      end;
      btFloat:
      begin
        Result := (VarType in [htIntS8, htIntU8, htIntS16, htIntU16, htIntS32,
          htIntU32, htIntS64, htIntU64, htFlt32, htFlt64, htFlt80, htAny]);
        if Result then
          Value := Extended(AThoriumValue.BuiltIn.Float);
      end;
      btString:
      begin
        Result := (VarType in [htAny, htStr]);
        if Result then
        begin
          if AThoriumValue.BuiltIn.Str = nil then
            Value := ''
          else
            Value := AThoriumValue.BuiltIn.Str^;
        end;
      end;
    end;
    vtExtendedType:
    begin
      Result := (VarType in [htAny, htExt]);
      if Result then
        Value := ptruint(@AThoriumValue.Extended);
    end;
  end;
end;

function ThoriumVariantToValue(const AVariant: Variant;
  out AThoriumValue: TThoriumValue; const GivenType: TThoriumHostType = htAny): Boolean;
var
  VarType: TVarType;
  Intval: ptruint;
begin
  Result := False;
  VarType := TVarData(AVariant).vtype;
  (*if (GivenType <> varany) and (VarType <> GivenType) then
  begin
    WriteLn('Leave');
    Exit;
  end;*)
  if GivenType = htExt then
  begin
    if (not VarType in [{$ifdef CPU32}varlongword, varinteger{$else}varint64, varqword{$endif}, varunknown]) then
      Exit;
    Result := True;
    AThoriumValue._Type := vtExtendedType;
    Intval := AVariant;
    AThoriumValue.Extended := PThoriumHostObjectTypeValue(Intval)^;
    Exit;
  end;
  case VarType of
    varsmallint, varinteger, varshortint, varint64, varbyte, varword,
    varlongword, varqword:
    begin
      AThoriumValue._Type := vtBuiltIn;
      AThoriumValue.BuiltIn._Type := btInteger;
      AThoriumValue.BuiltIn.Int := AVariant;
      Result := True;
    end;
    varsingle, vardouble, varcurrency, vardecimal:
    begin
      AThoriumValue._Type := vtBuiltIn;
      AThoriumValue.BuiltIn._Type := btFloat;
      AThoriumValue.BuiltIn.Float := AVariant;
      Result := True;
    end;
    varstring, varolestr:
    begin
      AThoriumValue._Type := vtBuiltIn;
      AThoriumValue.BuiltIn._Type := btString;
      New(AThoriumValue.BuiltIn.Str);
      AThoriumValue.BuiltIn.Str^ := String(AVariant);
      Result := True;
    end;
  end;
end;

function ThoriumTypeNeedsClear(const AType: TThoriumType): Boolean;
begin
  Result := not (((AType.ValueType = vtBuiltIn) and (AType.BuiltInType <> btString)) or (AType.ValueType in [vtFunction, vtHostFunction]));
end;

procedure ThoriumFreeValue(var AValue: TThoriumValue); inline;
// Frees the given variable.
begin
  if (AValue._Type = vtBuiltIn) and (AValue.BuiltIn._Type <> btString) then
    Exit;
  case AValue._Type of
    vtBuiltIn:
    begin
      AValue.BuiltIn.Str^ := '';
      Dispose(AValue.BuiltIn.Str);
    end;
    vtExtendedType:
    begin
      AValue.Extended.TypeClass.DisposeValue(AValue.Extended);
    end;
  end;
end;

function HostRecordField(const AType: TThoriumExternalFunctionVarType;
  const AName: String; const AOffset: Cardinal): TThoriumHostRecordField;
begin
  Result.FieldType := AType;
  Result.FieldName := AName;
  Result.Offset := AOffset;
end;

function HostVarType(const AHostType: TThoriumHostType;
  const AExtended: TThoriumHostObjectType = nil; const AStoring: Boolean = False): TThoriumExternalFunctionVarType;
begin
  Result.HostType := AHostType;
  Result.Extended := AExtended;
  if Result.HostType and (htTypeSection or htSizeSection) <> htExt then
  begin
    Result.Extended := nil;
    Result.Storing := False;
  end
  else
    Result.Storing := AStoring;
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

function ThoriumEvaluateBuiltInOperation(const Value1, Value2: TThoriumValue; Op: TThoriumOperation; out ResultValue: TThoriumValue): Boolean;
begin
  Result := True;
  ResultValue := ThoriumCreateBuiltInValue(btNil);
  if Op in [toNot, toNegate, toBoolNot] then
  begin
    case Value1.BuiltIn._Type of
      btInteger:
      begin
        ResultValue.BuiltIn._Type := btInteger;
        case Op of
          toNot: ResultValue.BuiltIn.Int := not Value1.BuiltIn.Int;
          toNegate: ResultValue.BuiltIn.Int := -Value1.BuiltIn.Int;
          toBoolNot: if Value1.BuiltIn.Int = 0 then
            ResultValue.BuiltIn.Int := 1
          else
            ResultValue.BuiltIn.Int := 0;
        end;
      end;
      btFloat:
      begin
        ResultValue.BuiltIn._Type := btFloat;
        if Op = toNegate then
          ResultValue.BuiltIn.Float := -Value1.BuiltIn.Float
        else
          Result := False;
      end;
    else
      Result := False;
    end;
    Exit;
  end;
  case Value1.BuiltIn._Type of
    btInteger:
    begin
      case Value2.BuiltIn._Type of
        btInteger:
        begin
          ResultValue.BuiltIn._Type := btInteger;
          case Op of
            toAdd:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int + Value2.BuiltIn.Int;
              Exit;
            end;
            toSubtract:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int - Value2.BuiltIn.Int;
              Exit;
            end;
            toDivide:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Int / Value2.BuiltIn.Int;
              ResultValue.BuiltIn._Type := btFloat;
              Exit;
            end;
            toMultiply:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int * Value2.BuiltIn.Int;
              Exit;
            end;
            toDiv:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int div Value2.BuiltIn.Int;
              Exit;
            end;
            toAnd:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int and Value2.BuiltIn.Int;
              Exit;
            end;
            toMod:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int mod Value2.BuiltIn.Int;
              Exit;
            end;
            toOr:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int or Value2.BuiltIn.Int;
              Exit;
            end;
            toXor:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int xor Value2.BuiltIn.Int;
              Exit;
            end;
            toShl:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int shl Value2.BuiltIn.Int;
              Exit;
            end;
            toShr:
            begin
              ResultValue.BuiltIn.Int := Value1.BuiltIn.Int shr Value2.BuiltIn.Int;
            end;
          end;
        end;
        btFloat:
        begin
          ResultValue.BuiltIn._Type := btFloat;
          case Op of
            toAdd:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Int + Value2.BuiltIn.Float;
              Exit;
            end;
            toSubtract:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Int - Value2.BuiltIn.Float;
              Exit;
            end;
            toMultiply:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Int * Value2.BuiltIn.Float;
              Exit;
            end;
            toDivide:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Int / Value2.BuiltIn.Float;
              Exit;
            end;
          end;
        end;
      end;
    end;
    btFloat:
    begin
      case Value2.BuiltIn._Type of
        btInteger:
        begin
          ResultValue.BuiltIn._Type := btFloat;
          case Op of
            toAdd:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float + Value2.BuiltIn.Int;
              Exit;
            end;
            toSubtract:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float - Value2.BuiltIn.Int;
              Exit;
            end;
            toMultiply:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float * Value2.BuiltIn.Int;
              Exit;
            end;
            toDivide:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float / Value2.BuiltIn.Int;
              Exit;
            end;
          end;
        end;
        btFloat:
        begin
          ResultValue.BuiltIn._Type := btFloat;
          case Op of
            toAdd:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float + Value2.BuiltIn.Float;
              Exit;
            end;
            toSubtract:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float - Value2.BuiltIn.Float;
              Exit;
            end;
            toMultiply:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float * Value2.BuiltIn.Float;
              Exit;
            end;
            toDivide:
            begin
              ResultValue.BuiltIn.Float := Value1.BuiltIn.Float / Value2.BuiltIn.Float;
              Exit;
            end;
          end;
        end;
      end;
    end;
    btString:
    begin
      if (Value2.BuiltIn._Type = btString) and (Op = toAdd) then
      begin
        ResultValue.BuiltIn._Type := btString;
        New(ResultValue.BuiltIn.Str);
        ResultValue.BuiltIn.Str^ := Value1.BuiltIn.Str^ + Value2.BuiltIn.Str^;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function ThoriumCompareBuiltIn(const Value1, Value2: TThoriumValue; Sym: TThoriumSymbol; out ResultValue: TThoriumValue): Boolean;
  function BoolToInt(B: Boolean): Int64; inline;
  begin
    if B then
      Result := 1
    else
      Result := 0;
  end;
begin
  Result := True;
  ResultValue := ThoriumCreateBuiltInValue(btInteger);
  case Value1.BuiltIn._Type of
    btInteger:
    begin
      case Value2.BuiltIn._Type of
        btInteger:
        begin
          case Sym of
            tsEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int = Value2.BuiltIn.Int);
              Exit;
            end;
            tsNotEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int <> Value2.BuiltIn.Int);
              Exit
            end;
            tsLesser:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int < Value2.BuiltIn.Int);
              Exit
            end;
            tsLesserEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int <= Value2.BuiltIn.Int);
              Exit
            end;
            tsGreater:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int > Value2.BuiltIn.Int);
              Exit
            end;
            tsGreaterEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int >= Value2.BuiltIn.Int);
              Exit
            end;
          end;
        end;
        btFloat:
        begin
          case Sym of
            tsEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int = Value2.BuiltIn.Float);
              Exit;
            end;
            tsNotEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int <> Value2.BuiltIn.Float);
              Exit
            end;
            tsLesser:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int < Value2.BuiltIn.Float);
              Exit
            end;
            tsLesserEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int <= Value2.BuiltIn.Float);
              Exit
            end;
            tsGreater:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int > Value2.BuiltIn.Float);
              Exit
            end;
            tsGreaterEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Int >= Value2.BuiltIn.Float);
              Exit
            end;
          end;
        end;
      end;
    end;
    btFloat:
    begin
      case Value2.BuiltIn._Type of
        btInteger:
        begin
          case Sym of
            tsEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float = Value2.BuiltIn.Int);
              Exit;
            end;
            tsNotEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float <> Value2.BuiltIn.Int);
              Exit
            end;
            tsLesser:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float < Value2.BuiltIn.Int);
              Exit
            end;
            tsLesserEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float <= Value2.BuiltIn.Int);
              Exit
            end;
            tsGreater:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float > Value2.BuiltIn.Int);
              Exit
            end;
            tsGreaterEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float >= Value2.BuiltIn.Int);
              Exit
            end;
          end;
        end;
        btFloat:
        begin
          case Sym of
            tsEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float = Value2.BuiltIn.Float);
              Exit;
            end;
            tsNotEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float <> Value2.BuiltIn.Float);
              Exit
            end;
            tsLesser:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float < Value2.BuiltIn.Float);
              Exit
            end;
            tsLesserEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float <= Value2.BuiltIn.Float);
              Exit
            end;
            tsGreater:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float > Value2.BuiltIn.Float);
              Exit
            end;
            tsGreaterEqual:
            begin
              ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Float >= Value2.BuiltIn.Float);
              Exit
            end;
          end;
        end;
      end;
    end;
    btString:
    begin
      if (Value2.BuiltIn._Type = btString) then
      begin
        case Sym of
          tsEqual:
          begin
            ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Str^ = Value2.BuiltIn.Str^);
            Exit;
          end;
          tsNotEqual:
          begin
            ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Str^ <> Value2.BuiltIn.Str^);
            Exit
          end;
          tsLesser:
          begin
            ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Str^ < Value2.BuiltIn.Str^);
            Exit
          end;
          tsLesserEqual:
          begin
            ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Str^ <= Value2.BuiltIn.Str^);
            Exit
          end;
          tsGreater:
          begin
            ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Str^ > Value2.BuiltIn.Str^);
            Exit
          end;
          tsGreaterEqual:
          begin
            ResultValue.BuiltIn.Int := BoolToInt(Value1.BuiltIn.Str^ >= Value2.BuiltIn.Str^);
            Exit
          end;
        end;
      end;
    end;
  end;
  Result := False;
end;

function OrdBool(B: Boolean): Integer; inline;
begin
  if B then
    Result := 1
  else
    Result := 0;
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
    tiFNCE: with TThoriumInstructionFNCE(AInstruction) do Result := Result + Format('[$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x] %%%s', [ptrint(FunctionRef), ThoriumRegisterToStr(TRI)]);

    tiCOPYR_S: with TThoriumInstructionCOPYR_S(AInstruction) do Result := Result + Format('%%%s %%sp($0x%.4x, $0x%.8x)', [ThoriumRegisterToStr(SRI), Scope, Offset]);
    tiCOPYR_ST: with TThoriumInstructionCOPYR_ST(AInstruction) do Result := Result + Format('%%s', [ThoriumRegisterToStr(SRI)]);
    tiCOPYR_FS: with TThoriumInstructionCOPYR_FS(AInstruction) do Result := Result + Format('%%s $0x%.8x(, $0x%.8x)', [ThoriumRegisterToStr(SRI), ModuleIndex, Offset]);
    tiCOPYS_ST: with TThoriumInstructionCOPYS_ST(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x)', [Scope, Offset]);
    tiCOPYFS: with TThoriumInstructionCOPYFS(AInstruction) do Result := Result + Format('$0x%.8x(, $0x%.8x) %%%s', [ModuleIndex, Offset, ThoriumRegisterToStr(TRI)]);
    tiCOPYS: with TThoriumInstructionCOPYS(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x) %%%s', [Scope, Offset, ThoriumRegisterToStr(TRI)]);
    tiCOPYR: with TThoriumInstructionCOPYR(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);

    tiMOVES_S: with TThoriumInstructionMOVES_S(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x)', [Scope, Offset]);
    tiMOVER_S: with TThoriumInstructionMOVER_S(AInstruction) do Result := Result + Format('%%%s %%sp($0x%.4x, $0x%.8x)', [ThoriumRegisterToStr(SRI), Scope, Offset]);
    tiMOVER_ST: with TThoriumInstructionMOVER_ST(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(SRI)]);
    tiMOVER: with TThoriumInstructionMOVER(AInstruction) do Result := Result + Format('%%%s %%%s', [ThoriumRegisterToStr(SRI), ThoriumRegisterToStr(TRI)]);
    tiMOVES: with TThoriumInstructionMOVES(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x) %%%s', [Scope, Offset, ThoriumRegisterToStr(TRI)]);
    tiMOVEST: with TThoriumInstructionMOVEST(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);
    tiMOVES_ST: with TThoriumInstructionMOVES_ST(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x)', [Scope, Offset]);

    tiPOP_S: with TThoriumInstructionPOP_S(AInstruction) do Result := Result + Format('$0x%.8x', [Amount]);
    tiPOPP_S: with TThoriumInstructionPOPP_S(AInstruction) do Result := Result + Format('$0x%.8x', [Amount]);

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

    tiINCI_S: with TThoriumInstructionINCI_S(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x)', [Scope, Offset]);
    tiINCF_S: with TThoriumInstructionINCF_S(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x)', [Scope, Offset]);
    tiINCI_FS: with TThoriumInstructionINCI_FS(AInstruction) do Result := Result + Format('$0x%.8x(, $0x%.8x)', [ModuleIndex, Offset]);
    tiINCF_FS: with TThoriumInstructionINCF_FS(AInstruction) do Result := Result + Format('$0x%.8x(, $0x%.8x)', [ModuleIndex, Offset]);
    tiINCI: with TThoriumInstructionINCI(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);
    tiINCF: with TThoriumInstructionINCF(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);

    tiDECI_S: with TThoriumInstructionDECI_S(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x)', [Scope, Offset]);
    tiDECF_S: with TThoriumInstructionDECF_S(AInstruction) do Result := Result + Format('%%sp($0x%.4x, $0x%.8x)', [Scope, Offset]);
    tiDECI_FS: with TThoriumInstructionDECI_FS(AInstruction) do Result := Result + Format('$0x%.8x(, $0x%.8x)', [ModuleIndex, Offset]);
    tiDECF_FS: with TThoriumInstructionDECF_FS(AInstruction) do Result := Result + Format('$0x%.8x(, $0x%.8x)', [ModuleIndex, Offset]);
    tiDECI: with TThoriumInstructionDECI(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);
    tiDECF: with TThoriumInstructionDECF(AInstruction) do Result := Result + Format('%%%s', [ThoriumRegisterToStr(TRI)]);

    tiXFGET: with TThoriumInstructionXFGET(AInstruction) do Result := Result + Format('$0x%.16x %%%s %%%s', [ID, ThoriumRegisterToStr(ERI), ThoriumRegisterToStr(TRI)]);
    tiXFSET: with TThoriumInstructionXFSET(AInstruction) do Result := Result + Format('%%%s $0x%.16x %%%s', [ThoriumRegisterToStr(VRI), ID, ThoriumRegisterToStr(ERI)]);

    tiXIGET: with TThoriumInstructionXIGET(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(IRI), ThoriumRegisterToStr(ERI), ThoriumRegisterToStr(TRI)]);
    tiXISET: with TThoriumInstructionXISET(AInstruction) do Result := Result + Format('%%%s %%%s %%%s', [ThoriumRegisterToStr(VRI), ThoriumRegisterToStr(IRI), ThoriumRegisterToStr(ERI)]);

    tiXPGET: with TThoriumInstructionXPGET(AInstruction) do Result := Result + Format('[$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x] %%%s', [ptrint(Prop), ThoriumRegisterToStr(TRI)]);
    tiXPSET: with TThoriumInstructionXPSET(AInstruction) do Result := Result + Format('%%%s [$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x]', [ThoriumRegisterToStr(SRI), ptrint(Prop)]);

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

    tiCALL: with TThoriumInstructionCALL(AInstruction) do Result := Result + Format('$0x%.8x {%%%s}', [EntryPoint, ThoriumRegisterToStr(HRI)]);

    tiCALL_D: with TThoriumInstructionCALL_D(AInstruction) do Result := Result + Format('$0x%.8x $0x%.8x', [EntryPoint, Pops]);

    tiFCALL: with TThoriumInstructionFCALL(AInstruction) do Result := Result + Format('$0x%.8x($0x%.8x)', [ModuleIndex, EntryPoint]);

    tiXCALL: with TThoriumInstructionXCALL(AInstruction) do Result := Result + Format('[$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x]', [ptrint(FunctionRef)]);
    tiXCALL_M: with TThoriumInstructionXCALL_M(AInstruction) do Result := Result + Format('%%%s [$0x%.'+IntToStr(SizeOf(ptruint)*2)+'x]', [ThoriumRegisterToStr(RTTIValueRegister), ptrint(MethodRef)]);

    tiRET:;

    tiNOOP: with TThoriumInstructionNOOP(AInstruction) do Result := Result + Format('0x%.16x 0x%.16x 0x%.16x', [Parameter1, Parameter2, Parameter3], THORIUM_NUMBER_FORMAT);
  else
    Result := 'error';
  end;
end;

procedure ThoriumVarTypeToTypeSpec(VarType: TThoriumHostType; var TypeSpec: TThoriumType);
begin
  case VarType and (htSizeSection or htTypeSection) of
    htIntS8, htIntS16, htIntS32, htIntS64,
    htIntU8, htIntU16, htIntU32, htIntU64:
    begin
      TypeSpec.ValueType := vtBuiltIn;
      TypeSpec.BuiltInType := btInteger;
    end;
    htFlt32, htFlt64, htFlt80:
    begin
      TypeSpec.ValueType := vtBuiltIn;
      TypeSpec.BuiltInType := btFloat;
    end;
    htStr:
    begin
      TypeSpec.ValueType := vtBuiltIn;
      TypeSpec.BuiltInType := btString;
    end;
    htExt:
    begin
      raise EThoriumException.Create('Cannot convert htExt types to internal type using ThoriumVarTypeToTypeSpec.');
    end;
  else
    raise EThoriumException.CreateFmt('Invalid value for VarType (%d) in ThoriumVarTypeToTypeSpec.', [VarType]);
  end;
end;
{%ENDREGION}

{%REGION 'Thorium exceptions' /fold}

{ EThoriumRuntimeExecutionException }

constructor EThoriumRuntimeExecutionException.Create(Module: TThoriumModule;
  InstructionAddr: TThoriumInstructionAddress; Instruction: PThoriumInstruction;
  OriginalException: Exception);
begin
  if Instruction <> nil then
  begin
    if OriginalException <> nil then
    begin
      if Module <> nil then
      begin
        CreateFmt('Instruction at %8.8x ''%s'' raised exception %s (''%s'') in module %s at code line %d.', [InstructionAddr, ThoriumInstructionToStr(Instruction^), OriginalException.ClassName, OriginalException.Message, Module.Name, Instruction^.CodeLine]);
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
        CreateFmt('Instruction at %8.8x ''%s'' raised unknown exception in module %s at code line %d.', [InstructionAddr, ThoriumInstructionToStr(Instruction^), Module.Name, Instruction^.CodeLine]);
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
        CreateFmt('Unknown instruction at %8.8x raised exception %s (''%s'') in module %s at unknown code line.', [InstructionAddr, OriginalException.ClassName, OriginalException.Message, Module.Name]);
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
        CreateFmt('Unknown instruction at %8.8x raised unknown exception in module %s at unknown code line.', [InstructionAddr, Module.Name]);
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
    FTarget.Free;
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
    FTarget.Free;
end;

procedure TThoriumReferenceImplementation.FreeReference;
begin
  if (InterLockedDecrement(FReferences) = 0) and not FHostControlled then
    FTarget.Free;
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
end;

procedure TThoriumHashableObject.InvalidateHash;
begin
  FHashGenerated := False;
end;

function TThoriumHashableObject.GetHash: TThoriumHash;
begin
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
// Set the capacity to a given value. This does not allow to decrease the size
// of the list.
begin
  if NewCapacity <= FCapacity then
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

{%REGION 'Thorium functions and variables' /fold}

{ TThoriumParameters }

constructor TThoriumParameters.Create;
begin
  inherited Create;
  FList := TFPList.Create;
end;

destructor TThoriumParameters.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TThoriumParameters.GetParameterCount: Integer;
// Returns the current count of registered parameters
begin
  Result := FList.Count;
end;

function TThoriumParameters.AddParameter: PThoriumType;
// Reserves the memory for a new parameter and returns it's pointer as result
begin
  Result := nil;
  GetMem(Result, SizeOf(TThoriumType));
  FList.Add(Result);
end;

procedure TThoriumParameters.RemoveParameter(const Index: Integer);
// Deletes a parameter from list and frees its memory.
begin
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  FreeMem(FList.Items[Index]);
  FList.Delete(Index);
end;

procedure TThoriumParameters.Clear;
// Deletes all parameters and frees their memory
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FreeMem(FList.Items[I]);
  FList.Clear;
end;

procedure TThoriumParameters.GetParameterSpec(const Index: Integer; out ParamSpec: TThoriumType);
// Fills the ParamSpec-record with the data of the parameter registered at Index
begin
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  ParamSpec := PThoriumType(FList.Items[Index])^;
end;

function TThoriumParameters.Duplicate: TThoriumParameters;
var
  I: Integer;
  Entry: PThoriumType;
begin
  Result := TThoriumParameters.Create;
  for I := 0 to FList.Count - 1 do
  begin
    Entry := nil;
    GetMem(Entry, SizeOf(TThoriumType));
    Move(PThoriumType(FList[I])^, Entry^, SizeOf(TThoriumType));
    Result.FList.Add(Entry);
  end;
end;

procedure TThoriumParameters.LoadFromStream(Stream: TStream);
var
  I: Integer;
  ACount: LongInt;
  TypeSpec: PThoriumType;
begin
  Clear;
  Stream.Read(ACount, SizeOf(LongInt));
  FList.Capacity := ACount;
  for I := 0 to ACount - 1 do
  begin
    TypeSpec := GetMem(SizeOf(TThoriumType));
    Stream.Read(TypeSpec^, SizeOf(TThoriumType));
    FList.Add(TypeSpec);
  end;
end;

procedure TThoriumParameters.SaveToStream(Stream: TStream);
var
  I: Integer;
  ACount: LongInt;
begin
  ACount := FList.Count;
  Stream.Write(ACount, SizeOf(LongInt));
  for I := 0 to Count - 1 do
    Stream.Write(PThoriumType(FList[I])^, SizeOf(TThoriumType));
end;

{ TThoriumPublicValue }

constructor TThoriumPublicValue.Create(AModule: TThoriumModule);
begin
  inherited Create;
  FModule := AModule;
  FName := '';
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

constructor TThoriumFunction.Create(AModule: TThoriumModule);
begin
  inherited Create(AModule);
  FEntryPoint := -1;
  //FEventCapsules := TFPObjectHashTable.CreateWith(50, @RSHash);
  FNestingLevel := -1;
  FParameters := TThoriumParameters.Create;
  FPrototyped := False;
  FReturnValues := TThoriumParameters.Create;
  FVisibilityLevel := vsPrivate;
  FPrototypedCalls := TThoriumJumpList.Create;
end;

destructor TThoriumFunction.Destroy;
begin
  FPrototypedCalls.Free;
  FParameters.Free;
  FReturnValues.Free;
  inherited Destroy;
end;

function TThoriumFunction.Call(AParameters: array of TThoriumValue
  ): TThoriumValue;
var
  I: Integer;
  VM: TThoriumVirtualMachine;
  Stack: TThoriumStack;
  Frame: PThoriumStackEntry;
begin
  VM := FModule.FThorium.FVirtualMachine;
  Stack := VM.FStack;
  for I := 0 to FParameters.Count - 1 do
  begin
    with (Stack.Push)^ do
    begin
      _Type := etValue;
      Move(AParameters[I], Value, SizeOf(TThoriumValue));
    end;
  end;
  Frame := Stack.Push;
  with Frame^ do
  begin
    _Type := etStackFrame;
    PreviousStackFrame := -1;
    ReturnAddress := THORIUM_JMP_EXIT;
    ReturnModule := -1;
    Params := Length(AParameters);
    RetVals := ReturnValues.Count;
    RegisterDumpRange := 0;
    RegisterDump := nil;
    DropResult := False;
  end;
  VM.FCurrentStackFrame := Stack.EntryCount-1;
  VM.Execute(VM.FThorium.FModules.IndexOf(FModule), FEntryPoint, False);
  if ReturnValues.Count = 1 then
  begin
    Move(Stack.GetTop(0)^.Value, Result, SizeOf(TThoriumValue));
    Stack.Pop(1, False);
  end;
end;

function TThoriumFunction.Duplicate: TThoriumFunction;
begin
  Result := TThoriumFunction.Create(FModule);
  Result.FName := FName;
  Result.FEntryPoint := FEntryPoint;
  Result.FNestingLevel := FNestingLevel;
  Result.FParameters.Free;
  Result.FParameters := FParameters.Duplicate;
  Result.FPrototyped := False;
  Result.FReturnValues.Free;
  Result.FReturnValues := FReturnValues.Duplicate;
  Result.FVisibilityLevel := FVisibilityLevel;
end;

function TThoriumFunction.AsEvent(AParameters: array of TThoriumHostType;
  ReturnType: TThoriumHostType): TThoriumFunctionCallbackCapsule;
begin
  //Result := AsEvent(Parameters, ReturnType, [], nil);
end;

function TThoriumFunction.AsEvent(AParameters: array of TThoriumHostType;
  ReturnType: TThoriumHostType; ExtParameters: array of TThoriumHostObjectType;
  ExtReturnType: TThoriumHostObjectType): TThoriumFunctionCallbackCapsule;
begin

end;

procedure TThoriumFunction.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FEntryPoint, SizeOf(TThoriumInstructionAddress));
  Stream.Read(FVisibilityLevel, SizeOf(TThoriumVisibilityLevel));
  FParameters.LoadFromStream(Stream);
  FReturnValues.LoadFromStream(Stream);
end;

function TThoriumFunction.SafeCall(AParameters: array of TThoriumValue
  ): TThoriumValue;
var
  I: Integer;
  ParamSpec: TThoriumType;
begin
  if FModule.FThorium.FVirtualMachine = nil then
    raise EThoriumRuntimeException.Create('Virtual machine not initialized.');
  if (Length(AParameters) <> FParameters.Count) then
    raise EThoriumRuntimeException.CreateFmt('Invalid parameter count (got %d, expected %d).', [Length(AParameters), FParameters.Count]);
  for I := 0 to FParameters.Count - 1 do
  begin
    FParameters.GetParameterSpec(I, ParamSpec);
    if not ThoriumCompareTypeEx(ThoriumExtractTypeSpec(AParameters[I]), ParamSpec) then
      raise EThoriumRuntimeException.CreateFmt('Incompatible parameter #%d', [I]);
  end;
  Result._Type := vtBuiltIn;
  Result.BuiltIn._Type := btNil;
  Result := Call(AParameters);
end;

procedure TThoriumFunction.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FEntryPoint, SizeOf(TThoriumInstructionAddress));
  Stream.Write(FVisibilityLevel, SizeOf(TThoriumVisibilityLevel));
  FParameters.SaveToStream(Stream);
  FReturnValues.SaveToStream(Stream);
end;

{ TThoriumFunctionCallbackCapsule }

constructor TThoriumFunctionCallbackCapsule.Create(AFunction: TThoriumFunction;
  Parameters: array of TThoriumHostType; ReturnType: TThoriumHostType;
  ExtParameters: array of TThoriumHostObjectType;
  ExtReturnType: TThoriumHostObjectType);

  function HostObjectTypeSpec(AType: TThoriumHostObjectType): TThoriumType;
  begin
    Result.ValueType := vtExtendedType;
    Result.Extended := AType;
  end;

var
  I: Integer;
  Params: PPThoriumType;
  Dummy: TThoriumType;
begin
  FFunction := AFunction;
  if FFunction = nil then
    raise EThoriumException.Create('Thorium function callback is nil.');
  if FFunction.FParameters.Count <> Length(Parameters) then
    raise EThoriumException.CreateFmt('Thorium function callback parameter count does not match. Got %d, expected %d.', [Length(Parameters), FFunction.FParameters.Count]);
  Params := PPThoriumType(FFunction.FParameters.FList.List);
  for I := 0 to High(Parameters) do
  begin
    if Parameters[I] and htArray = htArray then
      raise EThoriumException.Create('No support for array parameters via Thorium callback capsule.');
    if Parameters[I] and htByRef = htByRef then
      raise EThoriumException.Create('No support for pass-by-reference via Thorium callback capsule (yet! it shall come sometimes).');
    case Params^^.ValueType of
      vtBuiltIn:
      begin
        case Params^^.BuiltInType of
          btInteger:
          begin
            if not ((Parameters[I] and htTypeSection) in [htByte, htSmallInt, htWord, htShortInt, htLongInt, htDWord, htInt64, htQWord]) then
              raise EThoriumException.CreateFmt('Incompatible parameter type (%d and %s, parameter %d).', [Parameters[I] and htTypeSection, ThoriumTypeName(Params^^), I]);
          end;
          btFloat:
          begin
            if not ((Parameters[I] and htTypeSection) in [htSingle, htDouble, htFlt80]) then
              raise EThoriumException.CreateFmt('Incompatible parameter type (%d and %s, parameter %d).', [Parameters[I] and htTypeSection, ThoriumTypeName(Params^^), I]);
          end;
          btString:
          begin
            if Parameters[I] and htTypeSection <> htString then
              raise EThoriumException.CreateFmt('Incompatible parameter type (%d and %s, parameter %d).', [Parameters[I] and htTypeSection, ThoriumTypeName(Params^^), I]);
          end;
        else
          raise EThoriumException.CreateFmt('Thorium function ''%s'' has invalid parameter (%d).', [FFunction.Name, I]);
        end;
      end;
      vtExtendedType:
      begin
        if Parameters[I] and htTypeSection <> htExt then
          raise EThoriumException.CreateFmt('Incompatible parameter type (%d and %s, parameter %d).', [Parameters[I] and htTypeSection, ThoriumTypeName(Params^^), I]);
        if High(ExtParameters) < I then
          raise EThoriumException.Create('Not enough extended parameter type information.');
        if not (Params^^.Extended.IsTypeCompatible(Params^^, HostObjectTypeSpec(ExtParameters[I]), toAssign, Dummy)) then
          raise EThoriumException.CreateFmt('Incompatible parameter type (%s and %s, parameter %d).', [ExtParameters[I].Name, Params^^.Extended.Name, I]);
      end;
      vtFunction, vtHostMethod, vtHostFunction:
      begin
        raise EThoriumException.Create('No support for passing functions and methods via a Thorium callback capsule.');
      end;
    else
      raise EThoriumException.CreateFmt('Thorium function ''%s'' has invalid parameter (%d).', [FFunction.FName, I]);
    end;
    Inc(Params);
  end;
  if FFunction.FReturnValues.FList.Count > 1 then
    raise EThoriumException.Create('Cannot use a function with multiple return values as callback.');
  if (ReturnType = htNone) and (FFunction.FReturnValues.FList.Count = 1) then
    raise EThoriumException.Create('Unexpected return type.');
  Params := PPThoriumType(FFunction.FReturnValues.FList.List);
  case Params^^.ValueType of
    vtBuiltIn:
    begin
      case Params^^.BuiltInType of
        btInteger:
        begin
          if not ((ReturnType and htTypeSection) in [htByte, htSmallInt, htWord, htShortInt, htLongInt, htDWord, htInt64, htQWord]) then
            raise EThoriumException.CreateFmt('Incompatible return type (%d and %s).', [ReturnType and htTypeSection, ThoriumTypeName(Params^^)]);
        end;
        btFloat:
        begin
          if not ((ReturnType and htTypeSection) in [htSingle, htDouble, htFlt80]) then
            raise EThoriumException.CreateFmt('Incompatible return type (%d and %s).', [ReturnType and htTypeSection, ThoriumTypeName(Params^^)]);
        end;
        btString:
        begin
          if ReturnType and htTypeSection <> htString then
            raise EThoriumException.CreateFmt('Incompatible return type (%d and %s).', [ReturnType and htTypeSection, ThoriumTypeName(Params^^)]);
        end;
      else
        raise EThoriumException.CreateFmt('Thorium function ''%s'' has invalid return type.', [FFunction.Name]);
      end;
    end;
    vtExtendedType:
    begin
      if ReturnType and htTypeSection <> htExt then
        raise EThoriumException.CreateFmt('Incompatible parameter type (%d and %s).', [ReturnType and htTypeSection, ThoriumTypeName(Params^^)]);
      if ExtReturnType = nil then
        raise EThoriumException.Create('No host object type given for return value.');
      if not (Params^^.Extended.IsTypeCompatible(Params^^, HostObjectTypeSpec(ExtParameters[I]), toAssign, Dummy)) then
        raise EThoriumException.CreateFmt('Incompatible parameter type (%s and %s).', [ExtParameters[I].Name, Params^^.Extended.Name]);
    end;
    vtFunction, vtHostMethod, vtHostFunction:
      raise EThoriumException.Create('No support for passing functions and methods via a Thorium callback capsule.');
  else
    raise EThoriumException.CreateFmt('Thorium function ''%s'' has invalid parameter.', [FFunction.FName]);
  end;
  GenericCallbackPrecompile(FInstructions, Parameters, ReturnType);
end;

{ TThoriumVariable }

constructor TThoriumVariable.Create(AModule: TThoriumModule);
begin
  inherited Create(AModule);
  FIsStatic := False;
  FStackPosition := 0;
  FillByte(FTypeSpec, SizeOf(TThoriumType), 0);
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
  inherited Create;
  FName := '';
  FLibrary := ALibrary;
  FHashGenerated := False;
end;

destructor TThoriumHostObjectType.Destroy;
begin
  inherited Destroy;
end;

procedure TThoriumHostObjectType.AssignValue(const ASource: TThoriumValue;
  var ADest: TThoriumValue);
begin
  ThoriumFreeValue(ADest);
  ADest := DuplicateValue(ASource.Extended);
end;

function TThoriumHostObjectType.FindMethod(const AMethodName: String): TThoriumHostMethodBase;
var
  ID: QWord;
  TypeSpec: TThoriumTableEntry;
begin
  if FieldID(AMethodName, ID) then
  begin
    if FieldType(ID, TypeSpec) then
    begin
      if TypeSpec.TypeSpec.ValueType = vtHostMethod then
        Result := TypeSpec.TypeSpec.HostMethod
      else
        Result := nil;
    end
    else
      Result := nil;
  end
  else
    Result := nil;
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
        FName := UpperCase(FMethods[I].FName);
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
        FName := UpperCase(FStaticMethods[I].FName);
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

function TThoriumRTTIObjectType.GetNewInstance: Pointer;
begin
  Result := nil;
end;

procedure TThoriumRTTIObjectType.ApplyStoring(
  var AValue: TThoriumHostObjectTypeValue; MayDecreaseReference: Boolean = True);
var
  Intf: IThoriumPersistent;
begin
  if FCanUsePersistent then
  begin
    TThoriumPersistent(AValue.Value).FReferenceImplementation.EnableHostControl;
    if MayDecreaseReference then
      TThoriumPersistent(AValue.Value).FReferenceImplementation.FreeReference;
  end
  else
  begin
    TObject(AValue.Value).GetInterface(IThoriumPersistent, Intf);
    Intf.EnableHostControl;
    if MayDecreaseReference then
      Intf.FreeReference;
    Intf := nil;
  end;
end;

function TThoriumRTTIObjectType.DuplicateValue(const AValue: TThoriumHostObjectTypeValue): TThoriumValue;
var
  Intf: IThoriumPersistent;
begin
  // ToDo: Raise an error if the value is not compatible to this class.

  // Duplicate a value. First create a new value and set it's type spec
  Result := ThoriumCreateExtendedTypeValue(Self);
  // Then assign the pointer from the source value. We can use the GetReference
  // method to automatically increase the stuff...

  if FCanUsePersistent then
    TThoriumPersistent(Result.Extended.Value) := TThoriumPersistent(TThoriumPersistent(AValue.Value).FReferenceImplementation.GetReference)
  else
  begin
    TObject(AValue.Value).GetInterface(IThoriumPersistent, Intf);
    TObject(Result.Extended.Value) := Intf.GetReference;
    Intf := nil;
  end;
  //TThoriumPersistent(Result.Extended.Value) := TThoriumPersistent(AValue.Value).GetReference;
end;

procedure TThoriumRTTIObjectType.DisposeValue(var AValue: TThoriumHostObjectTypeValue);
var
  Intf: IThoriumPersistent;
begin
  // Disposing a RTTI value only means to decrease the reference counter and to
  // set the value to nil.
  if FCanUsePersistent then
  begin
    if TThoriumPersistent(AValue.Value) <> nil then
      TThoriumPersistent(AValue.Value).FReferenceImplementation.FreeReference;
  end
  else
  begin
    if AValue.Value = nil then
      Exit;
    TObject(AValue.Value).GetInterface(IThoriumPersistent, Intf);
    Intf.FreeReference;
    Intf := nil;
  end;
  AValue.Value := nil;
end;

function TThoriumRTTIObjectType.IsTypeCompatible(const Value1, Value2: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean;
begin
  Result := False;
  // Check if the first value is of the own type, like specified
  if Value1.Extended <> Self then
    raise EThoriumCompilerException.Create('Parameter 1 of TThoriumRTTIType.IsTypeCompatible must be an extended type equal to this instance.');
  // Can only compare or assign to extended types
  if Value2.ValueType <> vtExtendedType then
    Exit;
  // Check if we have a RTTI type
  if Value2.Extended is TThoriumRTTIObjectType then
  begin
    // Check if the operation may be valid
    if not (Operation in [toAssign, toCompare]) then
      Exit;

    case Operation of
      toCompare:
      begin
        // Comparsion is always available
        Result := True;
      end;
      toAssign:
      begin
        // Assignment only, if the baseclass of the other extended type is the
        // same or inherits from it.
        Result := (TThoriumRTTIObjectType(Value2.Extended).FBaseClass = FBaseClass) or
          TThoriumRTTIObjectType(Value2.Extended).FBaseClass.InheritsFrom(FBaseClass);
      end;
    end;
  end
  else
    // Otherwise let the other value decide this.
    Result := Value2.Extended.IsTypeCompatible(Value2, Value1, Operation, ResultType);
end;

function TThoriumRTTIObjectType.IsTypeOperationAvailable(const Value: TThoriumType; const Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean;
begin
  Result := False;
  if Value.Extended <> Self then
    raise EThoriumCompilerException.Create('Parameter 1 of TThoriumRTTIType.IsTypeOperationAvailable must be an extended type equal to this instance.');
end;

function TThoriumRTTIObjectType.HasFields: Boolean;
begin
  Result := True;
end;

function TThoriumRTTIObjectType.HasStaticFields: Boolean;
begin
  Result := True;
end;

function TThoriumRTTIObjectType.HasIndicies: Boolean;
begin
  Result := False;
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

function TThoriumRTTIObjectType.FieldID(const FieldIdent: String; out ID: QWord): Boolean;
var
  FieldName: ShortString;
//  Info: PPropInfo;
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

function TThoriumRTTIObjectType.StaticFieldID(const FieldIdent: String; out
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

function TThoriumRTTIObjectType.FieldType(const AFieldID: QWord; out ResultType: TThoriumTableEntry): Boolean;
var
  Info: PPropInfo;
  TypeData: PTypeData;
  ObjClass: TClass;
  ExtType: TThoriumHostObjectType;
begin
  if (AFieldID and THORIUM_RTTI_METHOD_BIT = THORIUM_RTTI_METHOD_BIT) then
  begin
    Result := True;
    ResultType.Name := nil;
    ResultType.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
    ResultType._Type := etFunction;
    ResultType.TypeSpec.ValueType := vtHostMethod;
    ResultType.TypeSpec.HostMethod := FMethods[AFieldID xor THORIUM_RTTI_METHOD_BIT];
  end
  else
  begin
    Info := FPropList^[AFieldID];//PPropInfo(ptruint(AFieldID));
    Result := True;
    ResultType.Name := nil;
    ResultType.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
    if Info^.SetProc = nil then
      ResultType._Type := etStatic
    else
      ResultType._Type := etVariable;
    case Info^.PropType^.Kind of
      tkInteger, tkEnumeration, tkSet, tkInt64, tkQWord, tkBool:
        ResultType.TypeSpec := ThoriumBuiltInTypeSpec(btInteger);
      tkFloat:
        ResultType.TypeSpec := ThoriumBuiltInTypeSpec(btFloat);
      tkSString, tkLString, tkAString, tkWString:
        ResultType.TypeSpec := ThoriumBuiltInTypeSpec(btString);
      tkClass:
      begin
        TypeData := GetTypeData(Info^.PropType);
        ObjClass := TypeData^.ClassType;
        ExtType := FLibrary.DeepFindRTTITypeByClass(ObjClass);
        if ExtType <> nil then
          ResultType.TypeSpec := ThoriumHostObjectTypeSpec(ExtType)
        else
          Result := False;
      end;
    else
      Result := False;
    end;
  end;
end;

function TThoriumRTTIObjectType.StaticFieldType(const AFieldID: QWord; out
  ResultType: TThoriumTableEntry): Boolean;
begin
  if AFieldID and THORIUM_RTTI_METHOD_BIT = 0 then
  begin
    Result := False;
  end
  else
  begin
    ResultType.Name := nil;
    ResultType.Offset := 0;
    ResultType.Scope := 0;
    ResultType.TypeSpec.ValueType := vtHostFunction;
    ResultType.TypeSpec.HostFunc := FStaticMethods[AFieldID and not THORIUM_RTTI_METHOD_BIT];
    Result := True;
  end;
end;

function TThoriumRTTIObjectType.GetField(const AInstance: TThoriumValue; const AFieldID: QWord): TThoriumValue;
var
  Info: PPropInfo;
begin
  Info := FPropList^[AFieldID];
  case Info^.PropType^.Kind of
    tkInteger, tkEnumeration, tkSet, tkInt64, tkQWord, tkBool:
    begin
      Result._Type := vtBuiltIn;
      Result.BuiltIn._Type := btInteger;
      Result.BuiltIn.Int := GetOrdProp(TObject(AInstance.Extended.Value), Info);
    end;
    tkFloat:
    begin
      Result._Type := vtBuiltIn;
      Result.BuiltIn._Type := btFloat;
      Result.BuiltIn.Float := GetFloatProp(TObject(AInstance.Extended.Value), Info);
    end;
    tkSString, tkLString, tkAString, tkWString:
    begin
      Result._Type := vtBuiltIn;
      Result.BuiltIn._Type := btString;
      New(Result.BuiltIn.Str);
      Result.BuiltIn.Str^ := GetStrProp(TObject(AInstance.Extended.Value), Info);
    end;
    tkClass:
    begin
      Result._Type := vtExtendedType;
      Result.Extended.TypeClass := FLibrary.DeepFindRTTIType(GetTypeData(Info^.PropType)^.ClassType.ClassName);
      Result.Extended.Value := TThoriumPersistent(GetObjectProp(TObject(AInstance.Extended.Value), Info)).FReferenceImplementation.GetReference;
    end;
  end;
end;

procedure TThoriumRTTIObjectType.SetField(const AInstance: TThoriumValue; const AFieldID: QWord; const NewValue: TThoriumValue);
var
  Info: PPropInfo;
begin
  Info := FPropList^[AFieldID];
  case Info^.PropType^.Kind of
    tkInteger, tkEnumeration, tkSet, tkInt64, tkQWord, tkBool:
    begin
      SetOrdProp(TObject(AInstance.Extended.Value), Info, NewValue.BuiltIn.Int);
    end;
    tkFloat:
    begin
      SetFloatProp(TObject(AInstance.Extended.Value), Info, NewValue.BuiltIn.Float);
    end;
    tkSString, tkLString, tkAString, tkWString:
    begin
      SetStrProp(TObject(AInstance.Extended.Value), Info, NewValue.BuiltIn.Str^);
    end;
    tkClass:
    begin
      SetObjectProp(TObject(AInstance.Extended.Value), Info, TObject(NewValue.Extended.Value));
    end;
  end;
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

function TThoriumRTTIObjectType.GetPropertyStoring(const PropInfo: PPropInfo
  ): Boolean;
begin
  Result := FStoringProperties.IndexOfObject(TObject(PropInfo)) >= 0;
end;

function TThoriumRTTIObjectType.GetPropertyStoring(const AFieldID: QWord
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

procedure TThoriumRTTIObjectType.SetPropertyStoring(const PropertyName: String;
  IsStoring: Boolean);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(FBaseClass, PropertyName);
  if PropInfo = nil then
    raise EPropertyError.CreateFmt('Property ''%s'' not found.', [PropertyName]);
  SetPropertyStoring(PropInfo);
end;

procedure TThoriumRTTIObjectType.SetPropertyStoring(const PropInfo: PPropInfo;
  IsStoring: Boolean);
var
  Idx: Integer;
begin
  Idx := FStoringProperties.IndexOfObject(TObject(PropInfo));
  if IsStoring then
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

class function TThoriumRTTIObjectType.NewNativeCallMethod(const AName: String;
  const ACodePointer: Pointer; const AParameters: array of TThoriumHostType;
  const AReturnType: TThoriumHostType;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodNativeCall;
var
  I: Integer;
begin
  Result := TThoriumHostMethodNativeCall.Create;
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    for I := 0 to High(AParameters) do
      Parameters.AddType(AParameters[I]);
    FReturnType.HostType := AReturnType;
    CallingConvention := ACallingConvention;
  end;
end;

class function TThoriumRTTIObjectType.NewNativeCallStaticMethod(
  const AName: String; const ACodePointer: Pointer; const ADataPointer: Pointer;
  const AParameters: array of TThoriumHostType;
  const AReturnType: TThoriumHostType;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodAsFunctionNativeCall;
var
  I: Integer;
begin
  Result := TThoriumHostMethodAsFunctionNativeCall.Create;
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    DataPointer := ADataPointer;
    for I := 0 to High(AParameters) do
      Parameters.AddType(AParameters[I]);
    FReturnType.HostType := AReturnType;
    CallingConvention := ACallingConvention;
  end;
end;

class function TThoriumRTTIObjectType.NewNativeCallStaticFunction(
  const AName: String; const ACodePointer: Pointer;
  const AParameters: array of TThoriumHostType;
  const AReturnType: TThoriumHostType;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostFunctionNativeCall;
var
  I: Integer;
begin
  Result := TThoriumHostFunctionNativeCall.Create;
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    for I := 0 to High(AParameters) do
      Parameters.AddType(AParameters[I]);
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
    FFields[I].FieldName := UpperCase(FFields[I].FieldName);
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
const
  EntrySize = SizeOf(TThoriumHash) + SizeOf(TThoriumHostType);
var
  I: Integer;
  Buffer: PByte;
  BufferSize: Ptruint;
  FieldHash: TThoriumHash;
begin
  BufferSize := EntrySize * Length(FFields);
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
  end;
end;

function TThoriumHostRecordType.GetNewInstance: Pointer;
var
  Value: ^RecordType;
begin
  New(Value);
  Result := Value;
end;

function TThoriumHostRecordType.DuplicateValue(
  const AValue: TThoriumHostObjectTypeValue): TThoriumValue;
type
  PRecordType = ^RecordType;
var
  Val: PRecordType;
begin
  Val := PRecordType(GetNewInstance);
  Val^ := PRecordType(AValue.Value)^;
  Result.Extended.TypeClass := Self;
  Result.Extended.Value := Val;
end;

procedure TThoriumHostRecordType.DisposeValue(
  var AValue: TThoriumHostObjectTypeValue);
type
  PRecordType = ^RecordType;
var
  Value: PRecordType;
begin
  Value := PRecordType(AValue.Value);
  Dispose(Value);
  AValue.Value := nil;
end;

function TThoriumHostRecordType.IsTypeCompatible(const Value1,
  Value2: TThoriumType; const Operation: TThoriumOperation; out
  ResultType: TThoriumType): Boolean;
begin
  Result := ThoriumCompareType(Value1, Value2);
  if not Result then
    Exit;
  case Operation of
    toAssign:
    begin
      ResultType := Value1;
    end;
  else
    Result := False;
  end;
end;

function TThoriumHostRecordType.IsTypeOperationAvailable(
  const Value: TThoriumType; const Operation: TThoriumOperation; out
  ResultType: TThoriumType): Boolean;
begin
  Result := False;
end;

function TThoriumHostRecordType.HasFields: Boolean;
begin
  Result := True;
end;

function TThoriumHostRecordType.HasStaticFields: Boolean;
begin
  Result := False;
end;

function TThoriumHostRecordType.HasIndicies: Boolean;
begin
  Result := False;
end;

function TThoriumHostRecordType.FieldID(const FieldIdent: String; out ID: QWord
  ): Boolean;
var
  FieldDefinitionIdx: Integer;
  FieldDefinition: PThoriumHostRecordField;
  Mask: Cardinal;
begin
  FieldDefinitionIdx := IndexOfFieldDefinition(FieldIdent);
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
  ID := Mask or (QWord(FieldDefinitionIdx) shl 32);
end;

function TThoriumHostRecordType.FieldType(const AFieldID: QWord; out
  ResultType: TThoriumTableEntry): Boolean;
var
  FieldDefinition: PThoriumHostRecordField;
begin
  FieldDefinition := @FFields[Integer((AFieldID shr 32) and $FFFFFFFF)];
  ThoriumVarTypeToTypeSpec(FieldDefinition^.FieldType.HostType, ResultType.TypeSpec);
  ResultType.Scope := 0;
  ResultType.Ptr := nil;
  ResultType._Type := etVariable;
  ResultType.Offset := 0;
  Result := True;
end;

function TThoriumHostRecordType.GetField(const AInstance: TThoriumValue;
  const AFieldID: QWord): TThoriumValue;
var
  FieldDefinition: PThoriumHostRecordField;
  Mask: Cardinal;
begin
  FieldDefinition := @FFields[Integer((AFieldID shr 32) and $FFFFFFFF)];
  case FieldDefinition^.FieldType.HostType of
    htByte, htShortInt, htWord, htSmallInt, htDWord, htLongInt:
    begin
      Mask := AFieldID and $FFFFFFFF;
      Result._Type := vtBuiltIn;
      Result.BuiltIn._Type := btInteger;
      Result.BuiltIn.Int := PDWord(AInstance.Extended.Value + FieldDefinition^.Offset)^ and Mask;
    end;
    htQWord, htInt64:
    begin
      Result._Type := vtBuiltIn;
      Result.BuiltIn._Type := btInteger;
      Result.BuiltIn.Int := PQWord(AInstance.Extended.Value + FieldDefinition^.Offset)^;
    end;
    htString:
      Result := ThoriumCreateStringValue(PString(AInstance.Extended.Value + FieldDefinition^.Offset)^);
    htSingle:
      Result := ThoriumCreateFloatValue(PSingle(AInstance.Extended.Value + FieldDefinition^.Offset)^);
    htDouble:
      Result := ThoriumCreateFloatValue(PDouble(AInstance.Extended.Value + FieldDefinition^.Offset)^);
    htFlt80:
      Result := ThoriumCreateFloatValue(PExtended(AInstance.Extended.Value + FieldDefinition^.Offset)^);
    htExt or htByRef:
    begin
      Result._Type := vtExtendedType;
      Result.Extended.TypeClass := FieldDefinition^.FieldType.Extended;
      Result.Extended.Value := PPointer(AInstance.Extended.Value + FieldDefinition^.Offset)^;
    end;
    htExt:
    begin
      Result._Type := vtExtendedType;
      Result.Extended.TypeClass := FieldDefinition^.FieldType.Extended;
      Result.Extended.Value := Pointer(AInstance.Extended.Value + FieldDefinition^.Offset);
    end;
  else
    raise EThoriumRuntimeException.CreateFmt('Cannot get field value, invalid type (%8.8x).', [FieldDefinition^.FieldType.HostType]);
  end;
end;

function TThoriumHostRecordType.GetPropertyStoring(const AFieldID: QWord
  ): Boolean;
var
  FieldDefinition: PThoriumHostRecordField;
begin
  FieldDefinition := @FFields[Integer((AFieldID shr 32) and $FFFFFFFF)];
  Result := FieldDefinition^.FieldType.Storing;
end;

procedure TThoriumHostRecordType.SetField(const AInstance: TThoriumValue;
  const AFieldID: QWord; const NewValue: TThoriumValue);
var
  FieldDefinition: PThoriumHostRecordField;
  Mask: Cardinal;
  Tmp: PDWord;
  TmpVal: TThoriumValue;
begin
  FieldDefinition := @FFields[Integer((AFieldID shr 32) and $FFFFFFFF)];
  case FieldDefinition^.FieldType.HostType of
    (*htByte, htShortInt, htWord, htSmallInt, htDWord, htLongInt:
    begin
      Mask := AFieldID and $FFFFFFFF;
      Tmp := PDWord(AInstance.Extended.Value + FieldDefinition^.Offset);
      Tmp^ := (Tmp^ and (not Mask)) or (NewValue.BuiltIn.Int and Mask);
    end;*)
    htByte, htShortInt:
    begin
      PByte(AInstance.Extended.Value + FieldDefinition^.Offset)^ := Byte(NewValue.BuiltIn.Int);
    end;
    htWord, htSmallInt:
    begin
      PWord(AInstance.Extended.Value + FieldDefinition^.Offset)^ := Word(NewValue.BuiltIn.Int);
    end;
    htDWord, htLongInt:
    begin
      PDWord(AInstance.Extended.Value + FieldDefinition^.Offset)^ := DWord(NewValue.BuiltIn.Int);
    end;
    htQWord, htInt64:
    begin
      PQWord(AInstance.Extended.Value + FieldDefinition^.Offset)^ := NewValue.BuiltIn.Int;
    end;
    htString:
      PString(AInstance.Extended.Value + FieldDefinition^.Offset)^ := NewValue.BuiltIn.Str^;
    htSingle:
      PSingle(AInstance.Extended.Value + FieldDefinition^.Offset)^ := NewValue.BuiltIn.Float;
    htDouble:
      PDouble(AInstance.Extended.Value + FieldDefinition^.Offset)^ := NewValue.BuiltIn.Float;
    htFlt80:
      PExtended(AInstance.Extended.Value + FieldDefinition^.Offset)^ := NewValue.BuiltIn.Float;
    htExt or htByRef:
    begin
      TmpVal._Type := vtExtendedType;
      TmpVal.Extended.TypeClass := NewValue.Extended.TypeClass;
      TmpVal.Extended.Value := PPointer(AInstance.Extended.Value + FieldDefinition^.Offset)^;
      NewValue.Extended.TypeClass.AssignValue(NewValue, TmpVal);
      //if FieldDefinition^.FieldType.Storing then
      //  NewValue.Extended.TypeClass.ApplyStoring(TmpVal.Extended, True);
      PPointer(AInstance.Extended.Value + FieldDefinition^.Offset)^ := TmpVal.Extended.Value;
    end;
    htExt:
    begin
      TmpVal._Type := vtExtendedType;
      TmpVal.Extended.TypeClass := NewValue.Extended.TypeClass;
      TmpVal.Extended.Value := Pointer(AInstance.Extended.Value + FieldDefinition^.Offset);
      NewValue.Extended.TypeClass.AssignValue(NewValue, TmpVal);
      //if FieldDefinition^.FieldType.Storing then
      //  NewValue.Extended.TypeClass.ApplyStoring(TmpVal.Extended, True);
      Move(TmpVal.Extended.Value^, Pointer(AInstance.Extended.Value + FieldDefinition^.Offset)^, MemSize(TmpVal.Extended.Value));
    end;
  else
    raise EThoriumRuntimeException.CreateFmt('Cannot get field value, invalid type (%8.8x).', [FieldDefinition^.FieldType.HostType]);
  end;
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
  FTypeSpec.ValueType := vtBuiltIn;
  FTypeSpec.BuiltInType := btNil;
end;

destructor TThoriumLibraryPropertyDirect.Destroy;
begin
  ThoriumFreeValue(FValue);
  inherited Destroy;
end;

procedure TThoriumLibraryPropertyDirect.CalcHash;
var
  Signature: String;
  SigLength: Integer;
  Offset: Integer;
begin
  SigLength := SizeOf(TThoriumValueType) + 1;
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
  FHash := TThoriumHash(MD5Buffer(Signature[1], Length(Signature)));
end;

procedure TThoriumLibraryPropertyDirect.GetValue(
  const AThoriumValue: PThoriumValue);
begin
  // Do not duplicate! Getter are expected to be non-creative.
  AThoriumValue^ := FValue;
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
  const AThoriumValue: PThoriumValue);
begin
  ThoriumFreeValue(FValue);
  FValue := ThoriumDuplicateValue(AThoriumValue^);
end;

{ TThoriumLibraryPropertyDirectSetCallback }

constructor TThoriumLibraryPropertyDirectSetCallback.Create;
begin
  inherited Create;
  FOnPropertySet := nil;
end;

procedure TThoriumLibraryPropertyDirectSetCallback.SetValue(
  const AThoriumValue: PThoriumValue);
var
  Allow: Boolean;
begin
  Allow := True;
  FOnPropertySet(Self, AThoriumValue^, Allow);
  if Allow then
    inherited SetValue(AThoriumValue);
end;

{ TThoriumLibraryPropertyCallback }

constructor TThoriumLibraryPropertyCallback.Create;
begin
  inherited Create;
  FStatic := False;
  FTypeSpec.ValueType := vtBuiltIn;
  FTypeSpec.BuiltInType := btNil;
  FOnPropertyGet := nil;
  FOnPropertySet := nil;
end;

procedure TThoriumLibraryPropertyCallback.CalcHash;
var
  Signature: String;
  SigLength: Integer;
  Offset: Integer;
begin
  SigLength := SizeOf(TThoriumValueType) + 1;
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
  FHash := TThoriumHash(MD5Buffer(Signature[1], Length(Signature)));
end;

procedure TThoriumLibraryPropertyCallback.GetValue(
  const AThoriumValue: PThoriumValue);
begin
  ThoriumFreeValue(AThoriumValue^);
  FOnPropertyGet(Self, AThoriumValue^);
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
  const AThoriumValue: PThoriumValue);
begin
  FOnPropertySet(Self, AThoriumValue^);
end;

{ TThoriumLibrary }

constructor TThoriumLibrary.Create(AThorium: TThorium);
begin
  FConstants := TFPList.Create;
  FName := GetName;
  FHostFunctions := TFPList.Create;
  FHostTypes := TFPList.Create;
  FHostRTTITypes := TFPList.Create;
  FHostTypeMap := TThoriumTypeMap.Create;
  FProperties := TFPList.Create;
  FRequiredLibraries := TFPList.Create;
  FThorium := AThorium;
  InitializeLibrary;
  PrecompileFunctions;
end;

destructor TThoriumLibrary.Destroy;
begin
  ClearAll;
  FConstants.Free;
  FHostTypeMap.Free;
  FHostFunctions.Free;
  FHostTypes.Free;
  FHostRTTITypes.Free;
  FProperties.Free;
  FRequiredLibraries.Free;
  inherited Destroy;
end;

function TThoriumLibrary.GetConstant(AIndex: Integer): TThoriumLibraryConstant;
begin
  Result := TThoriumLibraryConstant(FConstants[AIndex]);
end;

function TThoriumLibrary.GetConstantCount: Integer;
begin
  Result := FConstants.Count;
end;

function TThoriumLibrary.GetHostFunction(AIndex: Integer
  ): TThoriumHostFunctionBase;
begin
  Result := TThoriumHostFunctionBase(FHostFunctions[AIndex]);
end;

function TThoriumLibrary.GetHostFunctionCount: Integer;
begin
  Result := FHostFunctions.Count;
end;

function TThoriumLibrary.GetHostType(AIndex: Integer): TThoriumHostObjectType;
begin
  Result := TThoriumHostObjectType(FHostTypes[AIndex]);
end;

function TThoriumLibrary.GetHostTypeCount: Integer;
begin
  Result := FHostTypes.Count;
end;

function TThoriumLibrary.GetLibraryProperty(AIndex: Integer
  ): TThoriumLibraryProperty;
begin
  Result := TThoriumLibraryProperty(FProperties[AIndex]);
end;

function TThoriumLibrary.GetLibraryPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TThoriumLibrary.GetRTTIType(AIndex: Integer): TThoriumRTTIObjectType;
begin
  Result := TThoriumRTTIObjectType(FHostRTTITypes[AIndex]);
end;

function TThoriumLibrary.GetRTTITypeCount: Integer;
begin
  Result := FHostRTTITypes.Count;
end;

procedure TThoriumLibrary.PrecompileFunctions;
var
  I: Integer;
  List: PPointerList;
begin
  List := FHostFunctions.List;
  for I := 0 to FHostFunctions.Count - 1 do
    if TThoriumHostFunctionBase(List^[I]) is TThoriumHostFunctionNativeCall then
      TThoriumHostFunctionNativeCall(List^[I]).Precompile;
end;

procedure TThoriumLibrary.AddDependency(const ALibName: String);
var
  Lib: TThoriumLibrary;
begin
  Lib := FThorium.FindLibrary(ALibName);
  if Lib = nil then
    raise EThoriumDependencyException.CreateFmt('Dependency ''%s'' of host library ''%s'' not solved.', [ALibName, FName]);
  FRequiredLibraries.Add(Lib);
end;

procedure TThoriumLibrary.AddDependency(const ALib: TThoriumLibrary);
begin
  FRequiredLibraries.Add(ALib);
end;

procedure TThoriumLibrary.ClearAll;
var
  I: Integer;
begin
  // Properties and constants may contain references to host types, so clear
  // them before the others.
  for I := 0 to FProperties.Count - 1 do
    TThoriumLibraryProperty(FProperties[I]).Free;
  FProperties.Clear;
  for I := 0 to FConstants.Count - 1 do
    TThoriumLibraryConstant(FConstants[I]).Free;
  FConstants.Clear;
  for I := 0 to FHostTypes.Count - 1 do
    TThoriumHostObjectType(FHostTypes[I]).Free;
  FHostTypes.Clear;
  FHostRTTITypes.Clear;
  for I := 0 to FHostFunctions.Count - 1 do
    TThoriumHostFunctionBase(FHostFunctions[I]).Free;
  FHostFunctions.Clear;
end;

procedure TThoriumLibrary.ClearFunctions;
var
  I: Integer;
begin
  for I := 0 to FHostFunctions.Count - 1 do
    TThoriumHostFunctionBase(FHostFunctions[I]).Free;
  FHostFunctions.Clear;
end;

procedure TThoriumLibrary.ClearTypes;
var
  I: Integer;
begin
  for I := 0 to FHostTypes.Count - 1 do
    TThoriumHostObjectType(FHostTypes[I]).Free;
  FHostTypes.Clear;
  FHostRTTITypes.Clear;
end;

procedure TThoriumLibrary.DeleteHostFunction(AIndex: Integer);
begin
  TThoriumHostFunctionBase(FHostFunctions[AIndex]).Free;
  FHostFunctions.Delete(AIndex);
end;

procedure TThoriumLibrary.DeleteHostType(AIndex: Integer);
begin
  TThoriumHostObjectType(FHostTypes[AIndex]).Free;
  FHostTypes.Delete(AIndex);
end;

procedure TThoriumLibrary.InitializeLibrary;
begin

end;

function TThoriumLibrary.RegisterConstant(const AName: String;
  const AValue: TThoriumValue): PThoriumValue;
var
  NewName: String;
  Obj: TThoriumLibraryConstant;
begin
  NewName := UpperCase(AName);
  if (FindConstant(NewName) <> nil) or (FindProperty(NewName) <> nil) then
    raise EThoriumException.CreateFmt('Library constant or property ''%s'' already defined in ''%s''.', [NewName, AName]);
  Obj := TThoriumLibraryConstant.Create;
  Obj.FName := NewName;
  Obj.FValue := AValue;
  Result := @Obj.FValue;
  FConstants.Add(Obj);
end;

function TThoriumLibrary.RegisterFinishedObjectType(const AName: String;
  const AInstance: TThoriumHostObjectType; const ATypeInfo: PTypeInfo): TThoriumHostObjectType;
var
  NewName: String;
begin
  NewName := UpperCase(AName);
  if FindHostType(NewName) <> nil then
    raise EThoriumException.CreateFmt('Host type ''%s'' already declared in library ''%s''.', [NewName, FName]);
  Result := AInstance;
  Result.FName := NewName;
  FHostTypes.Add(Result);
  FHostTypeMap.Add(ATypeInfo, Result);
end;

function TThoriumLibrary.RegisterNativeCallFunction(const AName: String;
  const ACodePointer: Pointer; const AParameters: array of TThoriumHostType;
  const AReturnType: TThoriumHostType;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostFunctionNativeCall;
var
  NewName: String;
  I: Integer;
begin
  NewName := UpperCase(AName);
  if FindHostFunction(NewName) <> nil then
    raise EThoriumException.CreateFmt('Host function ''%s'' already declared in library ''%s''.', [NewName, FName]);
  Result := TThoriumHostFunctionNativeCall(FHostFunctions[FHostFunctions.Add(TThoriumHostFunctionNativeCall.Create)]);
  with Result do
  begin
    Name := NewName;
    CodePointer := ACodePointer;
    for I := 0 to High(AParameters) do
      Parameters.AddType(AParameters[I]);
    FReturnType.HostType := AReturnType;
    CallingConvention := ACallingConvention;
  end;
end;

function TThoriumLibrary.RegisterNativeCallMethodAsFunction(
  const AName: String; const ACodePointer: Pointer;
  const ADataPointer: Pointer; const AParameters: array of TThoriumHostType;
  const AReturnType: TThoriumHostType;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodAsFunctionNativeCall;
var
  NewName: String;
  I: Integer;
begin
  NewName := UpperCase(AName);
  if FindHostFunction(NewName) <> nil then
    raise EThoriumException.CreateFmt('Host function ''%s'' already declared in library ''%s''.', [NewName, FName]);
  Result := TThoriumHostMethodAsFunctionNativeCall(FHostFunctions[FHostFunctions.Add(TThoriumHostMethodAsFunctionNativeCall.Create)]);
  with Result do
  begin
    Name := NewName;
    CodePointer := ACodePointer;
    DataPointer := ADataPointer;
    for I := 0 to High(AParameters) do
      Parameters.AddType(AParameters[I]);
    FReturnType.HostType := AReturnType;
    CallingConvention := ACallingConvention;
  end;
end;

function TThoriumLibrary.RegisterObjectType(const AName: String;
  const ATypeClass: TThoriumHostObjectTypeClass): TThoriumHostObjectType;
var
  NewName: String;
begin
  NewName := UpperCase(AName);
  if FindHostType(NewName) <> nil then
    raise EThoriumException.CreateFmt('Host type ''%s'' already declared in library ''%s''.', [NewName, FName]);
  Result := TThoriumHostObjectType(FHostTypes[FHostTypes.Add(ATypeClass.Create(Self))]);
  Result.FName := NewName;
end;

function TThoriumLibrary.RegisterPropertyCallback(const AName: String;
  const ATypeSpec: TThoriumType; Static: Boolean;
  const AGetCallback: TThoriumOnPropertyGet;
  const ASetCallback: TThoriumOnPropertySet): TThoriumLibraryPropertyCallback;
begin
  Result := TThoriumLibraryPropertyCallback(RegisterPropertyCustom(AName, TThoriumLibraryPropertyCallback));
  with Result do
  begin
    FTypeSpec := ATypeSpec;
    FStatic := Static;
    FOnPropertyGet := AGetCallback;
    FOnPropertySet := ASetCallback;
  end;
  // Adding to the list is done in RegisterPropertyCustom
end;

function TThoriumLibrary.RegisterPropertyCustom(const AName: String;
  const AClass: TThoriumLibraryPropertyClass): TThoriumLibraryProperty;
var
  NewName: String;
begin
  NewName := UpperCase(AName);
  if (FindConstant(NewName) <> nil) or (FindProperty(NewName) <> nil) then
    raise EThoriumException.CreateFmt('Library constant or property ''%s'' already defined in ''%s''.', [NewName, AName]);
  Result := AClass.Create;
  Result.FName := NewName;
  FProperties.Add(Result);
end;

function TThoriumLibrary.RegisterPropertyDirect(const AName: String;
  const ATypeSpec: TThoriumType; Static: Boolean
  ): TThoriumLibraryPropertyDirect;
begin
  Result := TThoriumLibraryPropertyDirect(RegisterPropertyCustom(AName, TThoriumLibraryPropertyDirect));
  with Result do
  begin
    FTypeSpec := ATypeSpec;
    FStatic := Static;
    FValue := ThoriumCreateValue(FTypeSpec);
  end;
  // Adding to the list is done in RegisterPropertyCustom
end;

function TThoriumLibrary.RegisterPropertyDirectCallback(const AName: String;
  const ATypeSpec: TThoriumType; Static: Boolean;
  Callback: TThoriumOnPropertySetCallback
  ): TThoriumLibraryPropertyDirectSetCallback;
begin
  Result := TThoriumLibraryPropertyDirectSetCallback(RegisterPropertyCustom(AName, TThoriumLibraryPropertyDirectSetCallback));
  with Result do
  begin
    FTypeSpec := ATypeSpec;
    FStatic := Static;
    FValue := ThoriumCreateValue(FTypeSpec);
    FOnPropertySet := Callback;
  end;
  // Adding to the list is done in RegisterPropertyCustom
end;

function TThoriumLibrary.RegisterRTTIType(
  const AClass: TThoriumPersistentClass; AbstractClass: Boolean
  ): TThoriumRTTIObjectType;
var
  NewName: String;
begin
  NewName := UpperCase(AClass.ClassName);
  if FindHostType(NewName) <> nil then
    raise EThoriumException.CreateFmt('Host RTTI type ''%s'' already declared in library ''%s''.', [NewName, FName]);
  Result := TThoriumRTTIObjectType(FHostTypes[FHostTypes.Add(TThoriumRTTIObjectType.Create(Self, AClass, AbstractClass))]);
  Result.FName := NewName;
  FHostRTTITypes.Add(Result);
  FHostTypeMap.Add(AClass.ClassInfo, Result);
end;

function TThoriumLibrary.RegisterRTTIType(const AClass: TClass;
  AMethodsCallback: TThoriumRTTIMethodsCallback;
  AStaticMethodsCallback: TThoriumRTTIStaticMethodsCallback;
  AbstractClass: Boolean): TThoriumRTTIObjectType;
var
  NewName: String;
begin
  NewName := UpperCase(AClass.ClassName);
  if FindHostType(NewName) <> nil then
    raise EThoriumException.CreateFmt('Host RTTI type ''%s'' already declared in library ''%s''.', [NewName, FName]);
  Result := TThoriumRTTIObjectType(FHostTypes[FHostTypes.Add(TThoriumRTTIObjectType.Create(Self, AClass, AMethodsCallback, AStaticMethodsCallback, AbstractClass))]);
  Result.FName := NewName;
  FHostRTTITypes.Add(Result);
  FHostTypeMap.Add(AClass.ClassInfo, Result);
end;

function TThoriumLibrary.RegisterSimpleMethod(const AName: String;
  const AFunction: TThoriumSimpleMethod;
  const AParameters: array of TThoriumHostType;
  const AReturnType: TThoriumHostType): TThoriumHostFunctionSimpleMethod;
var
  NewName: String;
  I: Integer;
begin
  NewName := UpperCase(AName);
  if FindHostFunction(NewName) <> nil then
    raise EThoriumException.CreateFmt('Host function ''%s'' already declared in library ''%s''.', [NewName, FName]);
  Result := TThoriumHostFunctionSimpleMethod(FHostFunctions[FHostFunctions.Add(TThoriumHostFunctionSimpleMethod.Create)]);
  with Result do
  begin
    Name := NewName;
    Method := AFunction;
    for I := 0 to High(AParameters) do
      Parameters.AddType(AParameters[I]);
    FReturnType.HostType := AReturnType;
  end;
end;

function TThoriumLibrary.DeepFindHostType(const AName: String
  ): TThoriumHostObjectType;
var
  I: Integer;
begin
  Result := FindHostType(AName);
  if Result <> nil then
    Exit;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FRequiredLibraries[I]).DeepFindHostType(AName);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumLibrary.DeepFindRTTIType(const AName: String
  ): TThoriumRTTIObjectType;
var
  I: Integer;
begin
  Result := FindRTTIType(AName);
  if Result <> nil then
    Exit;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FRequiredLibraries[I]).DeepFindRTTIType(AName);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumLibrary.DeepFindRTTITypeByClass(const AClass: TClass
  ): TThoriumRTTIObjectType;
var
  I: Integer;
begin
  Result := FindRTTITypeByClass(AClass);
  if Result <> nil then
    Exit;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FRequiredLibraries[I]).DeepFindRTTITypeByClass(AClass);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumLibrary.FindConstant(const AName: String
  ): TThoriumLibraryConstant;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FConstants.List;
  for I := 0 to FConstants.Count - 1 do
  begin
    Result := TThoriumLibraryConstant(List^[I]);
    if Result.FName = UpName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumLibrary.FindHostFunction(const AName: String
  ): TThoriumHostFunctionBase;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FHostFunctions.List;
  for I := 0 to FHostFunctions.Count - 1 do
  begin
    Result := TThoriumHostFunctionBase(List^[I]);
    if Result.FName = UpName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumLibrary.FindHostType(const AName: String): TThoriumHostObjectType;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FHostTypes.List;
  for I := 0 to FHostTypes.Count - 1 do
  begin
    Result := TThoriumHostObjectType(List^[I]);
    if Result.FName = UpName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumLibrary.FindHostTypeForType(const AType: PTypeInfo
  ): TThoriumHostObjectType;
var
  Idx: Integer;
begin
  if not FHostTypeMap.Find(AType, Idx) then
    Result := nil
  else
    Result := FHostTypeMap.GetData(Idx);
end;

function TThoriumLibrary.FindProperty(const AName: String
  ): TThoriumLibraryProperty;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FProperties.List;
  for I := 0 to FProperties.Count - 1 do
  begin
    Result := TThoriumLibraryProperty(List^[I]);
    if Result.FName = UpName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumLibrary.FindRTTIType(const AName: String
  ): TThoriumRTTIObjectType;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FHostRTTITypes.List;
  for I := 0 to FHostRTTITypes.Count - 1 do
  begin
    Result := TThoriumRTTIObjectType(List^[I]);
    if Result.FName = UpName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumLibrary.FindRTTITypeByClass(const AClass: TClass
  ): TThoriumRTTIObjectType;
var
  I: Integer;
  List: PPointerList;
begin
  List := FHostRTTITypes.List;
  for I := 0 to FHostRTTITypes.Count - 1 do
  begin
    Result := TThoriumRTTIObjectType(List^[I]);
    if (Result.BaseClass = AClass) or (Result.BaseClass.InheritsFrom(AClass)) then
      Exit;
  end;
  Result := nil;
end;

function TThoriumLibrary.IndexOfConstant(const AName: String): Integer;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FConstants.List;
  for I := 0 to FConstants.Count - 1 do
  begin
    if TThoriumLibraryConstant(List^[I]).FName = UpName then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TThoriumLibrary.IndexOfHostFunction(const AName: String): Integer;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FHostFunctions.List;
  for I := 0 to FHostFunctions.Count - 1 do
  begin
    if TThoriumHostFunctionBase(List^[I]).FName = UpName then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TThoriumLibrary.IndexOfHostType(const AName: String): Integer;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FHostTypes.List;
  for I := 0 to FHostTypes.Count - 1 do
  begin
    if TThoriumHostObjectType(List^[I]).FName = UpName then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TThoriumLibrary.IndexOfProperty(const AName: String): Integer;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FProperties.List;
  for I := 0 to FProperties.Count - 1 do
  begin
    if TThoriumLibraryProperty(List^[I]).FName = UpName then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TThoriumLibrary.IndexOfRTTIType(const AName: String): Integer;
var
  I: Integer;
  UpName: String;
  List: PPointerList;
begin
  UpName := UpperCase(AName);
  List := FHostRTTITypes.List;
  for I := 0 to FHostRTTITypes.Count - 1 do
  begin
    if TThoriumHostObjectType(List^[I]).FName = UpName then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;
{%ENDREGION}

{%REGION 'Host functions & methods' /fold}
{ TThoriumHostFunctionParameterSpec }

constructor TThoriumHostFunctionParameterSpec.Create;
begin
  inherited Create;
  FCapacity := 0;
  FCount := 0;
  FParams := nil;
  Expand;
end;

destructor TThoriumHostFunctionParameterSpec.Destroy;
begin
  if FParams <> nil then
    FreeMem(FParams);
  inherited Destroy;
end;

procedure TThoriumHostFunctionParameterSpec.Expand;
begin
  if (FCapacity = 0) then
    SetCapacity(8)
  else if FCapacity <= 256 then
    SetCapacity(FCapacity shl 2)
  else
    SetCapacity(FCapacity + 128);
end;

function TThoriumHostFunctionParameterSpec.GetCompleteType(AIndex: Integer
  ): PThoriumExternalFunctionVarType;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('Parameter type index out of bounds (%d must be in [0..%d])', [AIndex, FCount-1]);
  Result := @FParams[AIndex];
end;

function TThoriumHostFunctionParameterSpec.GetExtendedType(AIndex: Integer): TThoriumHostObjectType;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('Parameter type index out of bounds (%d must be in [0..%d])', [AIndex, FCount-1]);
  if FParams[AIndex].HostType = htExt then
    Result := FParams[AIndex].Extended
  else
    Result := nil;
end;

function TThoriumHostFunctionParameterSpec.GetParamType(AIndex: Integer): TThoriumHostType;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('Parameter type index out of bounds (%d must be in [0..%d])', [AIndex, FCount-1]);
  Result := FParams[AIndex].HostType;
end;

procedure TThoriumHostFunctionParameterSpec.SetCapacity(AValue: Integer);
begin
  if AValue < FCapacity then
    Exit;
  FCapacity := AValue;
  ReAllocMem(FParams, FCapacity * SizeOf(TThoriumExternalFunctionVarType));
end;

procedure TThoriumHostFunctionParameterSpec.SetExtendedType(
  AIndex: Integer; AValue: TThoriumHostObjectType);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('Parameter type index out of bounds (%d must be in [0..%d])', [AIndex, FCount-1]);
  if AValue = nil then
    raise EThoriumException.Create('Cannot set nil as extended type. Use delete or SetParamType (prop. Types) instead.');
  FParams[AIndex].HostType := htExt or (FParams[AIndex].HostType and htFlagSection);
  FParams[AIndex].Extended := AValue;
end;

procedure TThoriumHostFunctionParameterSpec.SetParamType(AIndex: Integer; AValue: TThoriumHostType);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('Parameter type index out of bounds (%d must be in [0..%d])', [AIndex, FCount-1]);
  FParams[AIndex].HostType := AValue;
end;

function TThoriumHostFunctionParameterSpec.AddType(AType: TThoriumHostType): Integer;
begin
  if FCount = FCapacity then
    Expand;
  Result := FCount;
  Inc(FCount);
  FParams[Result].HostType := AType;
  FParams[Result].Storing := False;
end;

function TThoriumHostFunctionParameterSpec.AddExtendedType(
  AType: TThoriumHostObjectType): Integer;
begin
  Result := AddType(htExt);
  FParams[Result].Extended := AType;
end;

function TThoriumHostFunctionParameterSpec.AllTypes: PThoriumExternalFunctionVarType;
begin
  Result := FParams;
end;

function TThoriumHostFunctionParameterSpec.IndexOfType(AType: TThoriumHostType; Nth: Integer = 0): Integer;
var
  C, I: Integer;
begin
  C := -1;
  for I := 0 to FCount - 1 do
  begin
    if FParams[I].HostType = AType then
    begin
      Inc(C);
      if C = Nth then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

procedure TThoriumHostFunctionParameterSpec.InsertType(AType: TThoriumHostType; AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex > FCount) then
    raise EListError.CreateFmt('Parameter type index out of bounds (%d must be in [0..%d])', [AIndex, FCount]);
  if AIndex = FCount then
  begin
    AddType(AType);
    Exit;
  end;
  if FCount = FCapacity then
    Expand;
  Move(PThoriumExternalFunctionVarType(FParams + AIndex)^, PThoriumExternalFunctionVarType(FParams + AIndex + 1)^, (FCount - AIndex) * SizeOf(TThoriumExternalFunctionVarType));
  FParams[AIndex].HostType := AType;
  FParams[AIndex].Storing := False;
end;

procedure TThoriumHostFunctionParameterSpec.InsertExtendedType(
  AType: TThoriumHostObjectType; AIndex: Integer);
begin
  InsertType(htExt, AIndex);
  FParams[AIndex].Extended := AType;
end;

procedure TThoriumHostFunctionParameterSpec.DeleteType(AIndex: Integer);
begin
  Dec(FCount);
  if FCount = 0 then
    Exit;
  Move(PThoriumExternalFunctionVarType(FParams + AIndex + 1)^, PThoriumExternalFunctionVarType(FParams + AIndex)^, (FCount - AIndex) * SizeOf(TThoriumExternalFunctionVarType));
end;

procedure TThoriumHostFunctionParameterSpec.Clear;
begin
  FCount := 0;
  if FCapacity > 256 then
    SetCapacity(256);
end;

{ TThoriumHostCallableBase }

constructor TThoriumHostCallableBase.Create;
begin
  FName := '';
  FParameters := TThoriumHostFunctionParameterSpec.Create;
  FReturnType.HostType := htNone;
  FReturnType.Extended := nil;
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
  HostType: TThoriumHostType;
  I: Integer;
begin
  SigLength := 8 + Length(FName);
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
  FHash := TThoriumHash(MD5Buffer(Signature[1], SigLength));
end;

{ TThoriumExternalFunctionSimple }

constructor TThoriumHostFunctionSimpleMethod.Create;
begin
  inherited Create;
  FMethod := nil;
end;

procedure TThoriumHostFunctionSimpleMethod.CallFromVirtualMachine(AVirtualMachine: TThoriumVirtualMachine = nil);
var
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
  VAOffset: Integer;
begin
  // Check for the virtual machine
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
    ST^._Type := etValue;
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
  end;
end;

{ TThoriumHostFunctionNativeCall }

constructor TThoriumHostFunctionNativeCall.Create;
begin
  inherited Create;
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
  begin
    Stack.Pop(2, True);
    Stack.Pop(Parameters.Count - 1, False);
  end
  else
    Stack.Pop(Parameters.Count, False);
end;

procedure TThoriumHostFunctionNativeCall.Precompile;
begin
  GenericPrecompile(FInstructions, FVAOffset, Parameters, ReturnType, False, FCallingConvention);
end;

{ TThoriumHostMethodAsFunctionNativeCall }

constructor TThoriumHostMethodAsFunctionNativeCall.Create;
begin
  inherited Create;
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
  begin
    Stack.Pop(2, True);
    Stack.Pop(Parameters.Count - 1, False);
  end
  else
    Stack.Pop(Parameters.Count, False);
end;

procedure TThoriumHostMethodAsFunctionNativeCall.Precompile;
begin
  GenericPrecompile(FInstructions, FVAOffset, Parameters, ReturnType, True, FCallingConvention);
end;

{ TThoriumHostMethodBase }

constructor TThoriumHostMethodBase.Create;
begin
  inherited Create;
  FHostObjectType := nil;
end;

{ TThoriumHostMethodSimple }

constructor TThoriumHostMethodSimple.Create;
begin
  inherited Create;
  FClassMethod := nil;
end;

procedure TThoriumHostMethodSimple.CallFromVirtualMachine(OfObject: TObject;
  AVirtualMachine: TThoriumVirtualMachine);
var
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
  ParamType: TThoriumHostType;
begin
  // Check for the virtual machine
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
    ST^._Type := etValue;
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
  end;
end;

{ TThoriumHostMethodNativeCall }

constructor TThoriumHostMethodNativeCall.Create;
begin
  inherited Create;
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
  begin
    Stack.Pop(2, True);
    Stack.Pop(Parameters.Count - 1, False);
  end
  else
    Stack.Pop(Parameters.Count, False);
end;

procedure TThoriumHostMethodNativeCall.Precompile;
begin
  GenericPrecompile(FInstructions, FVAOffset, Parameters, ReturnType, True, FCallingConvention);
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
var
  I: Cardinal;
  Entry: PThoriumTableEntry;
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

procedure TThoriumIdentifierTable.AddConstantIdentifier(Name: String; Scope: Integer; Offset: Integer; TypeSpec: TThoriumType; Value: TThoriumValue);
// Adds an identifier declared as constant
var
  Rec: PThoriumTableEntry;
begin
  Rec := NewEntry;
  New(Rec^.Name);
  Rec^.Name^ := Name;
  Rec^.Scope := Scope;
  Rec^._Type := etStatic;
  Rec^.Offset := Offset;
  Rec^.TypeSpec := TypeSpec;
  Rec^.Value := ThoriumDuplicateValue(Value);
end;

procedure TThoriumIdentifierTable.AddVariableIdentifier(Name: String; Scope: Integer; Offset: Integer; TypeSpec: TThoriumType);
// Adds an identifier declared as variable
var
  Rec: PThoriumTableEntry;
begin
  Rec := NewEntry;
  New(Rec^.Name);
  Rec^.Name^ := Name;
  Rec^.Scope := Scope;
  Rec^._Type := etVariable;
  Rec^.Offset := Offset;
  Rec^.TypeSpec := TypeSpec;
  Rec^.Value := ThoriumCreateBuiltInValue(btUnknown);
end;

procedure TThoriumIdentifierTable.AddRegisterVariableIdentifier(Name: String;
  RegisterID: TThoriumRegisterID; TypeSpec: TThoriumType);
// Adds an identifier declared as variable
var
  Rec: PThoriumTableEntry;
begin
  Rec := NewEntry;
  New(Rec^.Name);
  Rec^.Name^ := Name;
  Rec^._Type := etRegisterVariable;
  Rec^.Offset := RegisterID;
  Rec^.TypeSpec := TypeSpec;
  Rec^.Value := ThoriumCreateBuiltInValue(btUnknown);
end;

procedure TThoriumIdentifierTable.AddFunctionIdentifier(Name: String; Func: TThoriumFunction);
// Adds an identifier declared as function
var
  Rec: PThoriumTableEntry;
begin
  Rec := NewEntry;
  New(Rec^.Name);
  Rec^.Name^ := Name;
  Rec^.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
  Rec^._Type := etFunction;
  Rec^.Offset := 0;
  Rec^.TypeSpec.ValueType := vtFunction;
  Rec^.TypeSpec.Func := Func;
  Rec^.Value := ThoriumCreateBuiltInValue(btUnknown);
end;

procedure TThoriumIdentifierTable.ClearTable;
// Deletes all identifiers from the table
var
  I: Integer;
  Entry: PThoriumTableEntry;
begin
  for I := 0 to FCount - 1 do
  begin
    Entry := PThoriumTableEntry(ptruint(FIdentifiers) + Cardinal(I * SizeOf(TThoriumTableEntry)));
    ThoriumFreeValue(Entry^.Value);
    Entry^.Name^ := '';
    Dispose(Entry^.Name);
    if (Entry^._Type = etFunction) then
      Entry^.TypeSpec.Func.Free;
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
    ThoriumFreeValue(Entry^.Value);
    Entry^.Name^ := '';
    Dispose(Entry^.Name);
    if (Entry^._Type = etFunction) then
      Entry^.TypeSpec.Func.Free;
    if Entry^._Type <> etRegisterVariable then
      Inc(Result);
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

{ TThoriumScanner }

constructor TThoriumScanner.Create(AInputString: String);
begin
  inherited Create;
  FInputPosition := 0;
  FInputSize := Length(AInputString);
  FInputString := AInputString;
  FInputHash := TThoriumHash(MD5String(FInputString));
  FCurrentSym := tsNone;
  FCurrentStr := '';
  FCurrentChar := ' ';
  FCurrentLine := 1;
  FCurrentX := 0;
  FNextSym := tsUnknown;
  FEndOfStream := False;
end;

constructor TThoriumScanner.Create(AInputStream: TStream);
var
  Buffer: String;
begin
  SetLength(Buffer, AInputStream.Size - AInputStream.Position);
  AInputStream.Read(Buffer[1], Length(Buffer));
  Create(Buffer);
end;

destructor TThoriumScanner.Destroy;
begin
  inherited Destroy;
end;

procedure TThoriumScanner.Read(var C: Char); inline;
begin
  Inc(FInputPosition);
  C := FInputString[FInputPosition];
end;

procedure TThoriumScanner.ScanForSymbol(var Sym: TThoriumSymbol;
  var Str: String);
// Scans for the next symbol and sets Sym and Str. Sym will contain the kind of
// symbol and Str will contains is represence as a string.

  function EndOfStream: Boolean; inline;
  // Checks if FInputStream is already on it's end.
  begin
    Result := (FInputPosition = FInputSize);
  end;

  function GetChar: Boolean; inline;
  // Gets the next non-space and non-control-character char.
  begin
    Result := True;
    if not EndOfStream then
      Read(FCurrentChar)
    else
    begin
      FCurrentChar := ' ';
      Result := False;
      Exit;
    end;

    FIsLineBreak := False;
    if FCurrentChar = #13 then
    begin
      if not EndOfStream then
        Read(FCurrentChar)
      else
        FCurrentChar := #10;

      if FCurrentChar <> #10 then
        Dec(FInputPosition);
      Inc(FCurrentLine);
      FCurrentX := 0;
      FIsLineBreak := True;
    end
    else if FCurrentChar = #10 then
    begin
      Inc(FCurrentLine);
      FCurrentX := 0;
      FIsLineBreak := True;
    end;

    if Ord(FCurrentChar) < Ord(' ') then
      FCurrentChar := ' ';
    Inc(FCurrentX);
  end;

var
  I: TThoriumSymbol;
  InitialLine: Integer;
  S2: String;

begin
  Sym := tsNone;
  Str := '';

  while True do
  begin
    // FCurrentChar := ' ';
    Sym := tsNone;
    Str := '';

    while (FCurrentChar = ' ') do
      if not GetChar then
        Exit;

    //if EndOfStream then
    //  Exit;

    case FCurrentChar of
      'A'..'Z', 'a'..'z', '_': // tsIdentifer or keyword
      begin
        while FCurrentChar in (THORIUM_LETTER+THORIUM_DIGIT) do
        begin
          Str := Str + FCurrentChar;
          GetChar;
        end;
        Sym := tsIdentifier;
        Str := UpperCase(Str);

        for I := THORIUM_FIRST_KEYWORD to THORIUM_LAST_KEYWORD do
          if THORIUM_SYMBOL_CODE[I] = Str then
          begin
            Sym := I;
            Break;
          end;
        Exit;
      end; // end: tsIdentifier or keyword

      '(', ')', '{', '}', ';', ',', '.', '[', ']', '!': // Single char symbols
      begin
        Str := FCurrentChar;
        Sym := tsUnknown;
        for I := THORIUM_FIRST_SINGLECHAROP to THORIUM_LAST_SINGLECHAROP do
          if THORIUM_SYMBOL_CODE[I] = Str then
          begin
            Sym := I;
            Break;
          end;
        GetChar;
        Exit;
      end; // end: Single char symbols

      '/': // Commentary symbols
      begin
        Str := FCurrentChar;
        GetChar;
        if (Str = '/') and (FCurrentChar = '*') then
        begin
          while not EndOfStream do
          begin
            GetChar;
            if (FCurrentChar = '*') then
            begin
              GetChar;
              if (FCurrentChar = '/') then
              begin
                GetChar;
                Break;
              end;
            end;
          end;
        end
        else if (Str = '/') and (FCurrentChar = '/') then
        begin
          InitialLine := FCurrentLine;
          while (InitialLine = FCurrentLine) and not EndOfStream do
            GetChar;
        end
        else // A normal slash
        begin
          Sym := tsUnknown;
          //GetChar;
          if (FCurrentChar = '=') then
          begin
            Str := Str + FCurrentChar;
            Sym := tsDivideAssign;
            GetChar;
            Exit;
          end
          else
          begin
            Sym := tsDivide;
            Exit;
          end;
        end;

      end; // end: Commentary symbols*)

      '+', '-': // Hard coded multichar operators
      begin
        Sym := tsUnknown;
        Str := FCurrentChar;
        GetChar;
        if (FCurrentChar = '=') then
        begin
          Str := Str + FCurrentChar;
          if Str = '+=' then
            Sym := tsAdditiveAssign
          else
            Sym := tsSubtractiveAssign;
          GetChar;
          Exit;
        end
        else if (FCurrentChar = '+') and (Str = '+') then
        begin
          Str := '++';
          Sym := tsPlusPlus;
          GetChar;
          Exit;
        end
        else if (FCurrentChar = '-') and (Str = '-') then
        begin
          Str := '--';
          Sym := tsMinusMinus;
          GetChar;
          Exit;
        end
        else
        begin
          if Str = '+' then
          begin
            Sym := tsPlus;
            Exit;
          end
          else if Str = '-' then
          begin
            Sym := tsMinus;
            Exit;
          end;
        end;
      end; // end: Hard coded multichar operators

      '<', '>', '*', ':', '=': // Multichar operators followed by equal signs
      begin
        Sym := tsUnknown;
        Str := FCurrentChar;
        GetChar;
        if (FCurrentChar = '=') then
        begin
          Str := Str + FCurrentChar;
          for I := THORIUM_FIRST_MULTICHAROP to THORIUM_LAST_MULTICHAROP do
            if THORIUM_SYMBOL_CODE[I] = Str then
            begin
              Sym := I;
              Break;
            end;
          GetChar;
          Exit;
        end
        else
        begin
          for I := THORIUM_FIRST_MULTICHAROP to THORIUM_LAST_MULTICHAROP do
            if THORIUM_SYMBOL_CODE[I] = Str then
            begin
              Sym := I;
              Exit;
            end;
        end;
      end; // end: Multichar operators followed by equal signs

      (*'''': // String value
      begin
        Sym := tsStringValue;
        Str := '';
        while True do
        begin
          GetChar;
          if FCurrentChar = '''' then
          begin
            GetChar;
            if FCurrentChar = '''' then
              Str := Str + FCurrentChar
            else
              Break;
          end
          else
            Str := Str + FCurrentChar;
        end;
        Exit;
      end; // end: String value*)
      '"': // String value
      begin
        Sym := tsStringValue;
        Str := '';
        while True do
        begin
          GetChar;
          if FCurrentChar = '"' then
          begin
            GetChar;
            Break;
          end;
          if FCurrentChar = '\' then
          begin
            GetChar;
            case LowerCase(FCurrentChar) of
              'r': Str := Str + #13;
              'n': Str := Str + #10;
              'x':
              begin
                GetChar;
                S2 := '$';
                while FCurrentChar in THORIUM_HEXDIGIT do
                  S2 := S2 + FCurrentChar;
                Str := Str + UTF8Chr(Cardinal(StrToInt(S2)));
              end;
              '"': Str := Str + '"';
              '\': Str := Str + '\';
            else
              Str := Str + '\' + FCurrentChar;
            end;
          end
          else
            Str := Str + FCurrentChar;
        end;
        Exit;
      end; // end: String value

      '0'..'9', '#': // Numeric / Ordinal value
      begin
        Str := FCurrentChar;
        (*if Str = '0' then
        begin
          Sym := tsIntegerValue;
          GetChar;
          while FCurrentChar in THORIUM_HEXDIGIT do
          begin
            Str := Str + FCurrentChar;
            GetChar;
          end;
          Exit;
        end // end: Str = '$'
        else *)if Str = '#' then
        begin
          Sym := tsStringValue;
          Str := '';
          GetChar;
          if FCurrentChar = '$' then
          begin
            Str := Str + FCurrentChar;
            GetChar;
            while FCurrentChar in THORIUM_HEXDIGIT do
            begin
              Str := Str + FCurrentChar;
              GetChar;
            end;
            Str := UTF8Chr(Cardinal(StrToInt(Str)));
            Exit;
          end;

          while FCurrentChar in THORIUM_DIGIT do
          begin
            Str := Str + FCurrentChar;
            GetChar;
          end;
          Str := UTF8Chr(Cardinal(StrToInt(Str)));
          Exit;
        end // end: Str = '#'
        else
        begin
          Sym := tsIntegerValue;
          GetChar;
          if (FCurrentChar = 'x') then
          begin
            Str := '$';
            GetChar;
            while FCurrentChar in THORIUM_HEXDIGIT do
            begin
              Str := Str + FCurrentChar;
              GetChar;
            end;
            Exit;
          end;
          while FCurrentChar in THORIUM_DIGIT do
          begin
            Str := Str + FCurrentChar;
            GetChar;
          end;

          if FCurrentChar = '.' then
          begin
            Sym := tsFloatValue;
            Str := Str + FCurrentChar;
            GetChar;
            while FCurrentChar in THORIUM_DIGIT do
            begin
              Str := Str + FCurrentChar;
              GetChar;
            end;
          end;
          Exit;
        end; // end: normal integer / float value
      end; // end: Numeric / Ordinal value
    else // Case else
      Sym := tsError;
      if (Ord(FCurrentChar) in [33..128]) then
        Str := 'Unexpected char '''+FCurrentChar+''''
      else
        Str := 'Unexpected char #'+IntToStr(Ord(FCurrentChar));
      Exit;
    end;

    Assert(Sym <> tsUnknown, 'Unknown symbol');
  end;
end;

(*procedure TThoriumScanner.Proceed;
begin
  ScanForSymbol(FCurrentSym, FCurrentStr);
end;*)

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
// ments. When moving around, make shure that any jump commands are corrected
// accordingly
var
  I, List: Integer;
  CurrList: TThoriumIntList;
  Instr: PThoriumInstruction;
begin
  if NewPosition > FCount then
    Exit;
  if FPosition <> FCount then
  begin
    Instr := FInstructions;
    Inc(Instr, 0);
    for I := 0 to FCount - 1 do
    begin
      if Instr^.Instruction in [tiJMP, tiJE, tiJNE, tiJLT, tiJGT, tiJLE, tiJGE] then
      begin
        if TThoriumInstructionJMP(Instr^).NewAddress >= FSetPosition then
          TThoriumInstructionJMP(Instr^).NewAddress := TThoriumInstructionJMP(Instr^).NewAddress + FInserted;

      end;
      Inc(Instr);
    end;
    for List := 0 to FAddressLists.Count - 1 do
    begin
      CurrList := TThoriumIntList(FAddressLists[List]);
      for I := 0 to CurrList.Count - 1 do
      begin
        if CurrList.Items[I] >= FSetPosition then
          CurrList.Items[I] := CurrList.Items[I] + FInserted;
      end;
    end;
    for I := 0 to FAddressPointers.Count - 1 do
      if PThoriumInstructionAddress(FAddressPointers[I])^ >= FSetPosition then
        PThoriumInstructionAddress(FAddressPointers[I])^ += FInserted;
    FInserted := 0;
  end;
  FPosition := NewPosition;
  FSetPosition := NewPosition;
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
  Func: TThoriumFunction;
begin
  if FCount < FCapacity then
  begin
    ReAllocMem(FInstructions, FCount * SizeOf(TThoriumInstruction));
    FCapacity := FCount;
  end;
  // Now we will iterate through the instructions to process the NOOP codes.
  Instr := PThoriumInstructionNOOP(FInstructions);
  for I := 0 to FCount - 1 do
  begin
    // If we have a NOOP instruction
    if Instr^.Instruction = tiNOOP then
    begin
      // check for it's first parameter:
      case Instr^.Kind of
        THORIUM_NOOPMARK_CALL: // Here is a call to a previously only forward
                               // declarated function.
        begin
          Func := TThoriumFunction(ptrint(Instr^.Parameter1));
          if Func.FPrototyped then
            raise EThoriumCompilerException.Create('Forward declaration not solved: '''+Func.Name+'''.');
          if Instr^.Parameter2 = -1 then
            TThoriumInstruction(Instr^) := call(Func.FEntryPoint, Instr^.Parameter3, Func.FReturnValues.Count, Func.FParameters.Count)
          else
            TThoriumInstruction(Instr^) := fcall(Func.FEntryPoint, Instr^.Parameter2, Instr^.Parameter3, Func.FReturnValues.Count, Func.FParameters.Count);
        end;
      end;
    end;
    Inc(Instr);
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

function TThoriumInstructions.DumpCodeStr: String;
// Writes a half human readable code to the string
var
  I: Integer;
  CurrentInstruction: PThoriumInstruction;
begin
  Result := Format(' %-9.9s   %-55.55s  %9.9s', ['Address', 'Instruction', 'LineNo.']) + LineEnding;
  CurrentInstruction := FInstructions;
  for I := 0 to FCount - 1 do
  begin
    if (I <> 0) then
      Result := Result + LineEnding;
    Result := Result + Format('0x%.8x  %-56.56s %10d', [
      I,
      ThoriumInstructionToStr(CurrentInstruction^),
      CurrentInstruction^.CodeLine
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
{%ENDREGION}

{%REGION 'Compiler' /fold}

{ TThoriumCustomCompiler }

constructor TThoriumCustomCompiler.Create(ATarget: TThoriumModule);
begin
  FError:= False;
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
  FPublicFunctions := ATarget.FPublicFunctions;
  FPublicVariables := ATarget.FPublicVariables;
  FRequiredModules := ATarget.FRequiredModules;
  FRequiredLibraries := ATarget.FRequiredLibraries;
  FStringLibrary := ATarget.FStringLibrary;
  FThorium := ATarget.FThorium;
  {$ifdef HookSIGUSR1}
  SigCurrModule := FModule;
  SigCurrCompiler := Self;
  {$endif}
end;

destructor TThoriumCustomCompiler.Destroy;
begin
  inherited Destroy;
end;

function TThoriumCustomCompiler.AddLibraryPropertyUsage(
  const AProp: TThoriumLibraryProperty): Integer;
begin
  Result := FLibPropUsage.IndexOf(AProp);
  if Result < 0 then
    Result := FLibPropUsage.Add(AProp);
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

function TThoriumCustomCompiler.AddPublicVariable: TThoriumVariable;
begin
  Result := TThoriumVariable.Create(FModule);
  FPublicVariables.Add(Result);
end;

procedure TThoriumCustomCompiler.CompilerError(const Msg: String; X, Y: Integer);
begin
  if (X >= 0) and (Y >= 0) then
    FLastError := Format('%s(%d|%d): %s', [FModule.FName, Y, X, Msg])
  else
    FLastError := Format('%s: %s', [FModule.FName, Msg]);
  FError := True;
end;

procedure TThoriumCustomCompiler.DumpState;
begin

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
      tiXCALL:
      begin
        AddHostFunctionUsageToRelocate(TThoriumHostFunctionBase(TThoriumInstructionXCALL(Instruction^).FunctionRef), I * SizeOf(TThoriumInstruction) + SizeOf(TThoriumInstructionCode));
      end;
      tiXCALL_M:
      begin
        AddHostFunctionUsageToRelocate(TThoriumHostFunctionBase(TThoriumInstructionXCALL_M(Instruction^).MethodRef), I * SizeOf(TThoriumInstruction) + SizeOf(TThoriumInstructionCode));
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

function TThoriumCustomCompiler.IsTypeCompatible(Value1, Value2: TThoriumType;
  Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean;
// Checks if the type is compatible
begin
  case Value1.ValueType of
    vtBuiltIn:
    begin
      if Value2.ValueType <> vtBuiltIn then
      begin
        if Value2.ValueType = vtExtendedType then
          Result := Value2.Extended.IsTypeCompatible(Value2, Value1, Operation, ResultType)
        else
          Result := False;
        Exit;
      end;

      Result := Operation in THORIUM_TYPE_OPERATORS[Value1.BuiltInType, Value2.BuiltInType];
      if Result then
      begin
        ResultType := ThoriumBuiltInTypeSpec(THORIUM_TYPE_OPERATOR_RESULTS[Value1.BuiltInType, Value2.BuiltInType, Operation]);
        Assert(ResultType.BuiltInType > btNil, 'ResultType must be unequal to btUnkown and btNil');
      end;
    end;
    vtExtendedType:
    begin
      Result := Value1.Extended.IsTypeCompatible(Value1, Value2, Operation, ResultType);
    end;
    vtFunction: Result := False;
  end;
end;

function TThoriumCustomCompiler.IsTypeOperationAvailable(Value: TThoriumType;
  Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean;
// Check if the operation can be executed on a value of this kind
begin
  case Value.ValueType of
    vtBuiltIn:
    begin
      case Value.BuiltInType of
        btUnknown, btNil: Result := False;
        btInteger, btFloat, btString: Result := Operation in THORIUM_TYPE_OPERATORS[Value.BuiltInType, Value.BuiltInType];
      end;
      if Result then
        ResultType := ThoriumBuiltInTypeSpec(THORIUM_TYPE_OPERATOR_RESULTS[Value.BuiltInType, Value.BuiltInType, Operation]);
    end;
    vtExtendedType:
    begin
      Result := Value.Extended.IsTypeOperationAvailable(Value, Operation, ResultType);
    end;
    vtFunction: Result := False;
  end;
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
    InstructionSet1 = [tiINTB, tiSTR, tiCOPYR_S, tiCOPYR_ST, tiCOPYR_FS,
      tiCOPYR, tiMOVER_S, tiMOVER_ST, tiMOVEST, tiCLR, tiCMPI, tiCMPIF,
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
    InstructionSet4 = [tiCOPYS];
    InstructionSet5 = [tiINT, tiFLT, tiEXT, tiFNC, tiFNCE, tiCOPYFS, tiXFGET,
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

  function Opt_Expression_3_2: Integer;
  // When a value is copyied to a register from stack and pushed to stack top
  // immediately after this, this gets erased by this function.
  var
    Copy, Move: PThoriumInstruction;
  begin
    Result := 0;
    Copy := Instruction;
    Move := @Instruction[1];
    if (Copy^.Instruction <> tiCOPYS) or (Move^.Instruction <> tiMOVER_ST) then
      Exit;
    if (TThoriumInstructionCOPYS(Copy^).TRI = TThoriumInstructionMOVER_ST(Move^).SRI) then
    begin
      Copy^.Instruction := tiCOPYS_ST;
      FInstructions.DeleteInstructions(I+1, 1);
      Result -= 1;
    end;
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

  function Opt_Expression_5_2: Integer;
  // When a value is moved from a stack to a register and from that register to
  // the stack top, this function will convert this to one instruction
  var
    Move1, Move2: PThoriumInstruction;
  begin
    Result := 0;
    Move1 := Instruction;
    Move2 := @Instruction[1];
    if (Move1^.Instruction = tiMOVES) and (Move2^.Instruction = tiMOVER_ST)
      and (TThoriumInstructionMOVES(Move1^).TRI = TThoriumInstructionMOVER_ST(Move2^).SRI) then
    begin
      TThoriumInstruction(Move1^).Instruction := tiMOVES_ST;
      FInstructions.DeleteInstructions(I+1, 1);
      Result -= 1;
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
    List: PPointerList;
  begin
    FOptimizedInstructions -= Offset;
    Result := False;
    if Offset = 0 then
      Exit;
    Result := True;
    DoneSomething := True;
    Jumps.ChangeAddresses(Offset, I+1, FInstructions);
    List := FPublicFunctions.List;
    for I2 := 0 to FPublicFunctions.Count - 1 do
    begin
      Func := TThoriumFunction(List^[I2]);
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
          HandleOffset(Opt_Expression_3_2);
        if DoneSomething then Break;
        if (Remaining >= 2) then
          HandleOffset(Opt_Expression_4_2);
        if DoneSomething then Break;
        if (Remaining >= 2) then
          HandleOffset(Opt_Expression_5_2);
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

{ TThoriumDefaultCompiler }

procedure TThoriumDefaultCompiler.DumpState;
begin
  if Scanner <> nil then
  begin
    WriteLn(' Scanner at line ', Scanner.FCurrentLine, ' x = ', Scanner.FCurrentX);
    WriteLn(' Current sym id = ', Ord(Scanner.FCurrentSym), '; ', GetEnumName(TypeInfo(TThoriumSymbol), Ord(Scanner.FCurrentSym)));
    WriteLn(' Current str = ', Scanner.FCurrentStr);
  end;
end;

function TThoriumDefaultCompiler.CompileFromStream(SourceStream: TStream;
  Flags: TThoriumCompilerFlags): Boolean;
// Compiles the source code given with SourceStream to binary instruction code.
var
  Table: TThoriumIdentifierTable;

  CurrentSym: TThoriumSymbol;
  CurrentStr: String;

  BreakJumps: TThoriumJumpList;
  Jumps: TThoriumJumpList;
  //FuncReturns: TThoriumReturnList;
  TableSizes: TThoriumIntStack;

  CodeHook: Boolean;
  CodeHook1, CodeHook2: PThoriumInstructionArray;

  RegisterUsage: TThoriumRegisterMask;

  procedure Proceed; inline;
  begin
    Scanner.ScanForSymbol(CurrentSym, CurrentStr);
  end;

  function InlineProceedTrue: Boolean; inline;
  begin
    Scanner.ScanForSymbol(CurrentSym, CurrentStr);
    Result := True;
  end;

  function InlineProceedFalse: Boolean; inline;
  begin
    Scanner.ScanForSymbol(CurrentSym, CurrentStr);
    Result := False;
  end;

  procedure CompilerError(const Msg: String); inline;
  begin
    Self.CompilerError(Msg, Scanner.FCurrentX, Scanner.CurrentLine);
  end;

  function GenCode(AInstruction: TThoriumInstruction): Integer; inline;
  // Add an instruction to the module instructions or to the current code hook
  begin
    if CodeHook then
    begin
      Result := Length(CodeHook1^);
      SetLength(CodeHook1^, Result+1);
      (*CodeHook1^[Result].Instruction := InstructionCode;
      CodeHook1^[Result].Parameter1 := Param1;
      CodeHook1^[Result].Parameter2 := Param2;
      CodeHook1^[Result].Parameter3 := Param3;*)
      Move(AInstruction, CodeHook1^[Result], SizeOf(TThoriumInstruction));
      CodeHook1^[Result].CodeLine := Scanner.CurrentLine - (Integer(Scanner.FIsLinebreak));
      if CodeHook2 <> nil then
      begin
        Result := Length(CodeHook2^);
        SetLength(CodeHook2^, Result+1);
        CodeHook2^[Result] := CodeHook1^[Length(CodeHook1^)-1];
      end;
    end
    else
    begin
      //Result := FInstructions.GenCode(InstructionCode, Param1, Param2, Param3, Scanner.CurrentLine - (Integer(Scanner.FIsLinebreak)));
      AInstruction.CodeLine := Scanner.CurrentLine - (Integer(Scanner.FIsLinebreak));
      Result := FInstructions.AppendCode(AInstruction);
    end;
  end;

  function AppendCode(ACodeArray: TThoriumInstructionArray): Integer; inline;
  // Append a bunch of instructions to the module instructions or to the current
  // code hook
  begin
    (*if (not CodeHook) and (CodeHook1 <> nil) then
      WriteLn('Weird stuff happens here');*)
    if CodeHook then
    begin
      Result := Length(CodeHook1^);
      //WriteLn(Length(ACodeArray), ' ', Length(CodeHook1^), ' (', IntToHex(ptruint(CodeHook1), SizeOf(ptruint)*2), ')');
      SetLength(CodeHook1^, Result + Length(ACodeArray));
      Move(ACodeArray[0], CodeHook1^[Result], Length(ACodeArray)*SizeOf(TThoriumInstruction));
      if CodeHook2 <> nil then
      begin
        Result := Length(CodeHook2^);
        SetLength(CodeHook2^, Result+Length(ACodeArray));
        Move(ACodeArray[0], CodeHook2^[Result], Length(ACodeArray)*SizeOf(TThoriumInstruction));
      end;
      Result := -1;
    end
    else
    begin
      Result := FInstructions.AppendCode(ACodeArray);
    end;
  end;

  function GetHookedInstructionPointerA(AIndex: Integer): PThoriumInstruction; inline;
  begin
    Result := @CodeHook1^[AIndex];
  end;

  function GetHookedInstructionPointerB(AIndex: Integer): PThoriumInstruction; inline;
  begin
    Result := @CodeHook2^[AIndex];
  end;

  procedure AddHostTypeUsages(Usages: TThoriumHostObjectTypeArray); inline;
  var
    I: Integer;
  begin
    for I := 0 to Length(Usages) - 1 do
      AddHostTypeUsage(Usages[I])
  end;

  procedure AddLibraryPropertyUsages(Usages: TThoriumLibraryPropertyArray); inline;
  var
    I: Integer;
  begin
    for I := 0 to Length(Usages) - 1 do
      AddLibraryPropertyUsage(Usages[I]);
  end;

  function PushEmpty(AType: TThoriumType): Integer; inline;
  // Pushes an empty value of the given type on the stack.
  begin
    case AType.ValueType of
      vtBuiltIn: case AType.BuiltInType of
        btFloat: Result := GenCode(flt_s(0.0));
        btInteger: Result := GenCode(int_s(0));
        btString: Result := GenCode(str_s());
      end;
      vtExtendedType:
      begin
        GenCode(ext_s(AType.Extended));
        AddHostTypeUsage(AType.Extended);
      end;
      vtFunction: raise EThoriumCompilerException.Create('There is no support for functions on the stack.');
    end;
  end;

  function NeedTypeOperation(Value1, Value2: TThoriumType; Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; inline;
  // Checks for a type operation and throws a compiler error if it's not
  // available
  begin
    Result := IsTypeCompatible(Value1, Value2, Operation, ResultType);
    if not Result then
      CompilerError('Operation '+THORIUM_OPERATION_NAME[Operation]+' is not available for '+ThoriumTypeName(Value1)+' and '+ThoriumTypeName(Value2)+'.');
  end;

  function NeedTypeOperation(Value: TThoriumType; Operation: TThoriumOperation; out ResultType: TThoriumType): Boolean; inline;
  // Checks for a type operation and throws a compiler error if it's not
  // available
  begin
    Result := IsTypeOperationAvailable(Value, Operation, ResultType);
    if not Result then
      CompilerError('Operation '+THORIUM_OPERATION_NAME[Operation]+' is not available for '+ThoriumTypeName(Value)+'.');
  end;

  function ExpectSymbol(SymbolMask: TThoriumSymbols; ThrowError: Boolean = True): Boolean; inline;
  // Checks if the current symbol matches the symbol mask and returns true if
  // this is the case. If ThrowError is true and the check fails, a compiler
  // error is generated.
  var
    SymStr: String;
  begin
    Result := (CurrentSym in SymbolMask);
    if (not Result) and (ThrowError) then
    begin
      SymStr := THORIUM_SYMBOL_NAMES[CurrentSym];
      if (CurrentSym = tsUnknown) then
        SymStr := SymStr + '(''' + CurrentStr + ''')';
      if SymbolMask <> [tsNone] then
      begin
        case CurrentSym of
          tsIdentifier: CompilerError('Unexpected symbol: '+SymStr+' ('''+CurrentStr+''')');
        else
          CompilerError('Unexpected symbol: '+SymStr);
        end;
      end
      else
        CompilerError('Unexpected symbol: '+SymStr+' (expected end of stream).');
    end;
  end;

  function BuiltInType(Ident: String): TThoriumBuiltInType; inline;
  // Converts a type identifier to the built in type identifier.
  begin
    if Ident = 'INT' then
      Result := btInteger
    else if Ident = 'STRING' then
      Result := btString
    else if Ident = 'FLOAT' then
      Result := btFloat
    else if Ident = 'VOID' then
      Result := btNil
    else
      Result := btUnknown;
  end;

  function TypeSpecByName(Ident: String; var TypeSpec: TThoriumType; AllowExtended: Boolean): Boolean; inline;
  // Converts a type identifier to a complete type spec structure.
  begin
    Ident := UpperCase(Ident);
    TypeSpec.BuiltInType := BuiltInType(Ident);
    if (TypeSpec.BuiltInType <> btUnknown) then
    begin
      Result := True;
      TypeSpec.ValueType := vtBuiltIn;
      Exit;
    end;
    if (AllowExtended) and (FThorium <> nil) then
    begin
      TypeSpec.Extended := FModule.FindHostObjectType(Ident); //FThorium.FExtendedTypeRegistry.GetTypeByName(Ident);
      if TypeSpec.Extended <> nil then
      begin
        Result := True;
        TypeSpec.ValueType := vtExtendedType;
        Exit;
      end;
    end;
    Result := False;
  end;

  function FindTableEntry(const Ident: String; out Entry: TThoriumTableEntry; out Module: Integer; RaiseError: Boolean = True; AllowFar: Boolean = True): Boolean; inline;
  // Searches all accessible tables for an value with the given identifier
  var
    I: Integer;
    VarI: Integer;
    CurrVar: TThoriumVariable;
    CurrFunc: TThoriumFunction;
    CurrExternalFunc: TThoriumHostFunctionBase;
    CurrModule: TThoriumModule;
    CurrConst: TThoriumLibraryConstant;
    CurrProp: TThoriumLibraryProperty;
    List: PPointerList;
  begin
    Module := -1;
    // First check the own module
    Result := Table.FindIdentifier(Ident, Entry);
    // If we could not find anything in here and far access is allowed, we scan
    // for the included modules.
    if (not Result) and (AllowFar) and (FThorium <> nil) then
    begin
      for I := FRequiredModules.Count - 1 downto 0 do
      begin
        // First get the module index.
        Module := FRequiredModules.Items[I];
        // Then the module pointer
        CurrModule := FThorium.Module[Module];
        // Check all function identifiers in the given module
        List := CurrModule.FPublicFunctions.List;
        for VarI := CurrModule.FPublicFunctions.Count - 1 downto 0 do
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
            Entry._Type := etFunction;
            Entry.TypeSpec.ValueType := vtFunction;
            Entry.TypeSpec.Func := CurrFunc;
            Result := True;
            Exit;
          end;
        end;
        // Now check all variables of the module
        List := CurrModule.FPublicVariables.List;
        for VarI := CurrModule.FPublicVariables.Count - 1 downto 0 do
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
        Entry._Type := etFunction;
        Entry.TypeSpec.ValueType := vtHostFunction;
        Entry.TypeSpec.HostFunc := CurrExternalFunc;
        Result := True;
        Exit;
      end;
      CurrProp := FModule.FindLibraryProperty(Ident);
      if CurrProp <> nil then
      begin
        Entry.Name := nil;
        Entry.Scope := THORIUM_STACK_SCOPE_NOSCOPE;
        Entry.Offset := -1;
        Entry._Type := etProperty;
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
        Entry._Type := etLibraryConstant;
        Entry.Value := CurrConst.FValue;
        Entry.TypeSpec := ThoriumExtractTypeSpec(Entry.Value);
        Entry.Ptr := CurrConst;
        Result := True;
        Exit;
      end;
    end;
    // If we were not able to find anything and we are allowed to raise an error
    // do so.
    if (not Result) and (RaiseError) then
      CompilerError('Undeclared identifier: '+Ident);
  end;

  function CurrentTableStackPos: Integer; inline;
  // Get the current pos of the table stack. Can be used with RestoreTableTo
  // to restore multiple frames.
  begin
    Result := TableSizes.Count;
  end;

  function GetTableEntriesTo(StackPos: Integer): Integer; inline;
  begin
    Result := Table.Count - TableSizes.Items[StackPos-1];
  end;

  procedure SaveTable; inline;
  // Pushes the current table size to the TableSize-stack for later restoration.
  begin
    TableSizes.Push(Table.Count);
  end;

  (*procedure RestoreTableTo(var Offset: Integer; Count: Integer; GenerateCode: Boolean = True); inline;
  // Restore the table size until the size of TableSize-stack is the one given
  // with Count.
  var
    OldSize: Integer;
    Amount: Integer;
  begin
    Amount := 0;
    while TableSizes.Count > Count do
    begin
      // Fetch the old size from the TableSize-stack.
      OldSize := TableSizes.Pop;
      // Check if it differs...
      if OldSize < Table.Count then
      begin
        // add the offset to the amount
        Amount += (Table.Count - OldSize);
        // decrease the offset
        Dec(Offset, Table.Count - OldSize);
        // clear the table downto the size
        Table.ClearTableTo(OldSize);
      end;
    end;
    // if code generation is allowed, we write a pop instruction
    if GenerateCode then
      GenCode(POP_S(Amount));
  end;*)

  procedure RestoreTable(var Offset: Integer; GenerateCode: Boolean = True); inline;
  // Restore the table size of the last SaveTable-call.
  var
    OldSize: Integer;
    StackDiff: Integer;
  begin
    // Fetch the old size from the TableSize-stack.
    OldSize := TableSizes.Pop;
    // Check if it differs...
    if OldSize < Table.Count then
    begin
      // first get the stack difference when the table gets cleared so far
      StackDiff := Table.ClearTableTo(OldSize);
      // ... and if this is the case do: if allowed code generation...
      if GenerateCode and (StackDiff > 0) then
        GenCode(pop_s(StackDiff));
      // anyway the decreasement of the current identifier offset...
      Dec(Offset, StackDiff);
    end;
  end;

  function GetFreeRegister(Kind: TThoriumRegisterKind; out RegisterID: TThoriumRegisterID; ThrowError: Boolean = True): Boolean; inline;
  // Searches a free register of the specified kind and returns its ID. If no
  // free register can be found, THORIUM_INVALID_REGISTER will be returned.
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
      if not (RegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE)) <> 0) then
      begin
        Result := True;
        RegisterID := I;
        RegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] := RegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] xor ((RegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE))) xor (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE)));
        Exit;
      end;
    end;
    if ThrowError then
    begin
      case Kind of
        trC: CompilerError('Internal error: Compiler uses CACHE-registers.');//CompilerError('Need more C registers (currently '+IntToStr(THORIUM_REGISTER_C_COUNT)+'). Recompile compiler with more registers or try to crop down your expressions.');
        trEXP: CompilerError('Need more expression registers (currently '+IntToStr(THORIUM_REGISTER_EXP_COUNT)+'). Recompile compiler with more registers or try to crop down your expressions.');
      end;
    end;
  end;

  procedure ReleaseRegister(ID: TThoriumRegisterID);
  // Unsets the usage flag of the specified register.
  begin
    RegisterUsage[ID div THORIUM_REGISTER_MASK_BLOCK_SIZE] := RegisterUsage[ID div THORIUM_REGISTER_MASK_BLOCK_SIZE] xor (RegisterUsage[ID div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (ID mod THORIUM_REGISTER_MASK_BLOCK_SIZE)));
  end;

  function GetHighestRegisterInUse: TThoriumRegisterID;
  var
    I: TThoriumRegisterID;
  begin
    Result := THORIUM_REGISTER_C_MAX;
    for I := THORIUM_REGISTER_EXP_MIN to THORIUM_REGISTER_EXP_MAX do
    begin
      if (RegisterUsage[I div THORIUM_REGISTER_MASK_BLOCK_SIZE] and (1 shl (I mod THORIUM_REGISTER_MASK_BLOCK_SIZE)) <> 0) then
        Result := I;
    end;
  end;

  procedure Module;
  // Compile a module.
  var
    CurrentReturnType: TThoriumType;
    CurrentScope: Integer;
    //CurrentFunction: TThoriumFunction;
    CurrentFunctionTableStack: Integer;

    // We need a forward declaration of Expression for use in QualifyIdentifier.
    function Expression(TargetRegister: TThoriumRegisterID; NeedStatic: Boolean = False; IsStatic: PBoolean = nil; StaticValue: PThoriumValue = nil; IsExpressionBased: PBoolean = nil): TThoriumType; forward;

    function QualifyIdentifier(out Ident: TThoriumQualifiedIdentifier;
      AllowedKinds: TThoriumQualifiedIdentifierKinds;
      TargetRegister: TThoriumRegisterID): Boolean;
    // Parse a whole identifier and return its specs in Ident. This includes
    // also any instructions needed for getting or setting the value of the
    // variable or object the identfifier identifies, if it is not a type or
    // a function.
    // Currently, there are the following identifiers supported:
    //   - Variable or constant identifiers
    //   - Type identifiers
    //   - Function identifiers
    //   - Undeclared identifiers

      function GenCodeEx(var TargetArray: TThoriumInstructionArray;
        AInstruction: TThoriumInstruction): Integer;
      // This generates different code for the getter and setter codes.
      begin
        Result := Length(TargetArray);
        SetLength(TargetArray, Result+1);
        TargetArray[Result] := AInstruction;
        TargetArray[Result].CodeLine := Scanner.CurrentLine - (Integer(Scanner.FIsLinebreak));
        (*TargetArray[Result].Instruction := InstructionCode;
        TargetArray[Result].Parameter1 := Param1;
        TargetArray[Result].Parameter2 := Param2;
        TargetArray[Result].Parameter3 := Param3;
        TargetArray[Result].CodeLine := Scanner.CurrentLine;*)
      end;

      function AddExtendedTypeUsageEx(var TargetArray: TThoriumHostObjectTypeArray;
        AItem: TThoriumHostObjectType): Integer;
      begin
        Result := Length(TargetArray);
        SetLength(TargetArray, Result+1);
        TargetArray[Result] := AItem;
      end;

      function AddLibPropertyUsageEx(var TargetArray: TThoriumLibraryPropertyArray;
        AItem: TThoriumLibraryProperty): Integer;
      begin
        Result := Length(TargetArray);
        SetLength(TargetArray, Result+1);
        TargetArray[Result] := AItem;
      end;

    var
      CurrIdent: String;
      CurrEntry, NewEntry: TThoriumTableEntry;
      CurrType, ExprType: TThoriumType;
      CurrModule: Integer;
      Prop: TThoriumLibraryProperty;
      BufInstruction: PThoriumInstruction;

      procedure CallExternalFunction(AsMethod: Boolean = False; ExtendedTypeRegister: TThoriumRegisterID = 0);
      var
        I: Integer;
        Func: TThoriumHostFunctionBase;
        ExprType, OldExprType, ParamType: TThoriumType;
        ParamRegID: TThoriumRegisterID;
        VACount, FltCount, ItemSize, ToClearCount: Cardinal;
        Untyped: Boolean;
        ExternalParamType: PThoriumExternalFunctionVarType;
        HostParamType: TThoriumHostType;
        VAStartIdx: TThoriumInstructionAddress;
        InstructionFunc: TThoriumInstructionFunc1R;
        PointerBased: Boolean;
        StaticStringList: TThoriumIntList;
        IsExpressionBased: Boolean;
      begin
        // Retreive the function
        Func := CurrType.HostFunc;
        // Check for a return value
        StaticStringList := TThoriumIntList.Create;
        try
          if Func.ReturnType.HostType <> htNone then
          begin
            // Push an empty slot to the stack for this
            //ThoriumVarTypeToTypeSpec(Func.ReturnType, ParamType);
            ThoriumExternalVarTypeToTypeSpec(@Func.ReturnType, ParamType);
            CurrType := ParamType;
            PushEmpty(ParamType);
          end
          else
            CurrType := ThoriumBuiltInTypeSpec(btNil);
          // Allocate a register for caching parameters
          if not GetFreeRegister(trEXP, ParamRegID) then
            Exit;
          // Now go through all parameters and get them...
          VACount := 0;
          for I := 0 to Func.Parameters.Count - 1 do
          begin
            HostParamType := Func.Parameters.Types[I];
            if HostParamType and htArray = htArray then
            begin
              // For now, you can only use varargs as the last parameter. So raise
              // compiler error if this is not the case.
              if I < Func.Parameters.Count - 1 then
              begin
                CompilerError('Host environment mistake: Varargs are only allowed as last parameter.');
                Exit;
              end;

              if I > 0 then
              begin
                if not ExpectSymbol([tsComma], False) then
                begin
                  // No varargs, so just push a zero to the stack.
                  GenCode(vastart(0, False));
                  GenCode(int_s(0));
                  // We can break here since this is per definitionem the last
                  // parameter.
                  Break;
                end
                else
                  Proceed;
              end
              else
              begin
                if ExpectSymbol([tsCloseBracket], False) then
                begin
                  // No varargs either, so just push a zero to the stack.
                  GenCode(vastart(0, False));
                  GenCode(int_s(0));
                  Break;
                end;
              end;
              // Get the parameter type spec
              ExternalParamType := Func.Parameters.CompleteTypes[I];

              if HostParamType and (not htFlagSection) = htAny then
                Untyped := True
              else
              begin
                // Strip the vararray flag from the vartype, since that is not
                // supported by ThoriumExternalVarTypeToTypeSpec.
                ExternalParamType^.HostType := ExternalParamType^.HostType and (not htFlagSection);
                ThoriumExternalVarTypeToTypeSpec(ExternalParamType, ParamType);
                PointerBased := False;
                case ExternalParamType^.HostType of
                  htIntS8:
                  begin
                    InstructionFunc := @va_i8s;
                    ItemSize := 1;
                  end;
                  htIntU8:
                  begin
                    InstructionFunc := @va_i8;
                    ItemSize := 1;
                  end;
                  htIntS16:
                  begin
                    InstructionFunc := @va_i16s;
                    ItemSize := 2;
                  end;
                  htIntU16:
                  begin
                    InstructionFunc := @va_i16;
                    ItemSize := 2;
                  end;
                  htIntS32:
                  begin
                    InstructionFunc := @va_i32s;
                    ItemSize := 4;
                  end;
                  htIntU32:
                  begin
                    InstructionFunc := @va_i32;
                    ItemSize := 4;
                  end;
                  htIntS64:
                  begin
                    InstructionFunc := @va_i64s;
                    ItemSize := 8;
                  end;
                  htIntU64:
                  begin
                    InstructionFunc := @va_i64;
                    ItemSize := 8;
                  end;
                  htFlt32:
                  begin
                    InstructionFunc := @va_f32;
                    ItemSize := 4;
                  end;
                  htFlt64:
                  begin
                    InstructionFunc := @va_f64;
                    ItemSize := 8;
                  end;
                  htFlt80:
                  begin
                    InstructionFunc := @va_f80;
                    ItemSize := 10;
                  end;
                  htStr:
                  begin
                    InstructionFunc := @va_s;
                    PointerBased := True;
                  end;
                  htExt:
                  begin
                    InstructionFunc := @va_x;
                    PointerBased := True;
                  end
                else
                  CompilerError('Unknown host type.');
                  Exit;
                end;
                ExternalParamType^.HostType := ExternalParamType^.HostType or htFlagSection;
                Untyped := False;
              end;

              FltCount := 0;
              VACount := 0;
              ToClearCount := 0;
              if Untyped then
                VAStartIdx := GenCode(vastart_t(0, 0, 0))
              else
                VAStartIdx := GenCode(vastart(0, PointerBased));
              repeat
                // Parse the given expression
                ExprType := Expression(ParamRegID, False, nil, nil, @IsExpressionBased);
                if HasError then
                  Exit;
                // Check if the types are compatible.
                OldExprType := ExprType;
                if (not Untyped) and (not NeedTypeOperation(ParamType, ExprType, toAssign, ExprType)) then
                begin
                  CompilerError('Incompatible type for argument '+IntToStr(I+VACount)+' of '+Func.Name);
                  Exit;
                end;
                if not Untyped then
                begin
                  // Cast if the types are not exactly equal.
                  if not ThoriumCompareTypeEx(OldExprType, ParamType) then
                    GenCode(_cast(ParamRegID, ParamRegID, OldExprType, ParamType));
                  if (ExternalParamType^.Storing) and (ExternalParamType^.Extended is TThoriumRTTIObjectType) then
                    GenCode(xct(ParamRegID));
                  // Move the value to the varargs.
                  GenCode(InstructionFunc(ParamRegID));
                end
                else
                begin
                  case ExprType.ValueType of
                    vtBuiltIn:
                      case ExprType.BuiltInType of
                        btInteger:
                          GenCode(vat_i(ParamRegID));
                        btFloat:
                        begin
                          GenCode(vat_f(ParamRegID));
                          Inc(FltCount);
                        end;
                        btString:
                        begin
                          GenCode(vat_s(ParamRegID));
                          Inc(ToClearCount);
                        end;
                      else
                        CompilerError('Invalid type as argument for untyped varargs.');
                        Exit;
                      end;
                    vtExtendedType:
                    begin
                      if (ExternalParamType^.Storing) and (ExternalParamType^.Extended is TThoriumRTTIObjectType) then
                        GenCode(xct(ParamRegID));
                      GenCode(vat_x(ParamRegID));
                      Inc(ToClearCount);
                    end;
                  else
                    CompilerError('Invalid type as argument for untyped varargs.');
                    Exit;
                  end;
                end;
                // Push the value on the stack
                //GenCode(mover_st(ParamRegID));
                Inc(VACount);

                if IsExpressionBased then
                begin
                  if ThoriumTypeNeedsClear(ParamType) then
                  begin
                    StaticStringList.AddEntry(ParamRegID);
                    if not GetFreeRegister(trEXP, ParamRegID) then
                      Exit;
                  end;
                end;
              until (CurrentSym <> tsComma) or (InlineProceedFalse);
              GenCode(int_s(VACount));
              if Untyped then
              begin
                if CodeHook then
                begin
                  with TThoriumInstructionVASTART_T(GetHookedInstructionPointerA(VAStartIdx)^) do
                  begin
                    Length := VACount;
                    Floats := FltCount;
                    ToClear := ToClearCount;
                  end;
                  if CodeHook2 <> nil then
                    with TThoriumInstructionVASTART_T(GetHookedInstructionPointerB(VAStartIdx)^) do
                    begin
                      Length := VACount;
                      Floats := FltCount;
                      ToClear := ToClearCount;
                    end;
                end
                else
                begin
                  with TThoriumInstructionVASTART_T(FInstructions.Instruction[VAStartIdx]^) do
                  begin
                    Length := VACount;
                    Floats := FltCount;
                    ToClear := ToClearCount;
                  end;
                end;
              end
              else
              begin
                // If not pointer based, the amount of bytes is expected to be in
                // the length field. So we multiply the count of arguments with
                // the size of each.
                if not PointerBased then
                  VACount := VACount * ItemSize;
                if CodeHook then
                begin
                  TThoriumInstructionVASTART(GetHookedInstructionPointerA(VAStartIdx)^).Length := VACount;
                  if CodeHook2 <> nil then
                    TThoriumInstructionVASTART(GetHookedInstructionPointerB(VAStartIdx)^).Length := VACount;
                end
                else
                  TThoriumInstructionVASTART(FInstructions.Instruction[VAStartIdx]^).Length := VACount;
              end;
            end
            else
            begin
              // Check for the comma
              if I > 0 then
              begin
                if not ExpectSymbol([tsComma]) then
                begin
                  CompilerError('Too few parameters for '+Func.Name);
                  Exit;
                end;
                Proceed;
              end
              else
              begin
                if ExpectSymbol([tsCloseBracket], False) then
                begin
                  CompilerError('Too few parameters for '+Func.Name);
                  Exit;
                end;
              end;
              // Parse the given expression
              ExprType := Expression(ParamRegID, False, nil, nil, @IsExpressionBased);
              if HasError then
                Exit;
              // Get the parameter type spec
              ExternalParamType := Func.Parameters.CompleteTypes[I];
              ThoriumExternalVarTypeToTypeSpec(ExternalParamType, ParamType);
              // Check if the types are compatible.
              OldExprType := ExprType;
              if not NeedTypeOperation(ParamType, ExprType, toAssign, ExprType) then
              begin
                CompilerError('Incompatible type for argument '+IntToStr(I+VACount)+' of '+Func.Name);
                Exit;
              end;
              // Cast if neccessary
              if not ThoriumCompareTypeEx(OldExprType, ParamType) then
                GenCode(_cast(ParamRegID, ParamRegID, OldExprType, ParamType));
              if (ExternalParamType^.Storing) and (ExternalParamType^.Extended is TThoriumRTTIObjectType) then
                GenCode(xct(ParamRegID));
              // Push the value on the stack
              GenCode(mover_st(ParamRegID));
              if IsExpressionBased then
              begin
                if ThoriumTypeNeedsClear(ParamType) then
                begin
                  StaticStringList.AddEntry(ParamRegID);
                  if not GetFreeRegister(trEXP, ParamRegID) then
                    Exit;
                end;
              end;
            end;
          end;
          // Release the parameter register
          ReleaseRegister(ParamRegID);
          // Check for the closing bracket.
          if not ExpectSymbol([tsCloseBracket]) then
            Exit;
          // Perform the external function call
          if AsMethod then
            GenCode(xcall_m(TThoriumHostMethodBase(Func), ExtendedTypeRegister))
          else
            GenCode(xcall(Func));
          if VACount > 0 then
            GenCode(vafinish());
          if (Func.ReturnType.HostType <> htNone) then
          begin
            // Read the result from stack and write it to the target register
            GenCode(movest(TargetRegister));
          end;
          for I := 0 to StaticStringList.Count - 1 do
          begin
            ParamRegID := StaticStringList.Items[I];
            GenCode(clr(ParamRegID));
            ReleaseRegister(ParamRegID);
          end;
        finally
          StaticStringList.Free;
        end;
      end;

      procedure CallFunction; inline;
      // Handle an internal function call.
      var
        I: Integer;
        Func: TThoriumFunction;
        ExprType, ParamType, OldExprType: TThoriumType;
        ParamRegID: TThoriumRegisterID;
        IsExpressionBased: Boolean;
        ExpressionFreeList: TThoriumIntList;
      begin
        // Get the function
        Func := CurrType.Func;

        // Create a list to store the registers which are needed to free values
        // later.
        ExpressionFreeList := TThoriumIntList.Create;
        try
          // Check if there is a return value
          if Func.ReturnValues.Count > 0 then
          begin
            // And push an empty value on the stack if this is the case.
            Func.ReturnValues.GetParameterSpec(0, ParamType);
            CurrType := ParamType;
          end
          else
            CurrType := ThoriumBuiltInTypeSpec(btNil);
          // Allocate the register where the parameters will be cached.
          if not GetFreeRegister(trEXP, ParamRegID) then
            Exit;
          // Now iterate through the parameters and parse their expressions.
          for I := 0 to Func.Parameters.Count - 1 do
          begin
            // Parse the given expression
            ExprType := Expression(ParamRegID, False, nil, nil, @IsExpressionBased);
            OldExprType := ExprType;
            // Get the parameter type spec
            Func.Parameters.GetParameterSpec(I, ParamType);
            // Check if the types are compatible.
            if not NeedTypeOperation(ParamType, ExprType, toAssign, ExprType) then
            begin
              CompilerError('Incompatible type for argument '+IntToStr(I)+' of '+Func.Name);
              Exit;
            end;
            // Cast if neccessary
            if not ThoriumCompareTypeEx(OldExprType, ParamType) then
              GenCode(_cast(ParamRegID, ParamRegID, OldExprType, ParamType));
            // If this is not the last parameter, check for a separating comma.
            if I < Func.Parameters.Count - 1 then
            begin
              if not ExpectSymbol([tsComma]) then
                Exit;
              Proceed;
            end;
            // Push the value on the stack
            GenCode(mover_st(ParamRegID));

            if IsExpressionBased then
            begin
              if ThoriumTypeNeedsClear(ParamType) then
              begin
                ExpressionFreeList.AddEntry(ParamRegID);
                if not GetFreeRegister(trEXP, ParamRegID) then
                  Exit;
              end;
            end;
          end;
          // Release the buffer register
          ReleaseRegister(ParamRegID);
          // Check for the closing bracket.
          if not ExpectSymbol([tsCloseBracket]) then
            Exit;
          // If this is a prototyped function...
          if Func.FPrototyped then
          begin
            // Write a NOOPMARK for later parsing to fill in the entry point which
            // is not known at the moment. If this is not a module local function
            // we need to locate the module. Otherwise we can write -1.
            if Func.FModule <> FModule then
              GenCode(noop(THORIUM_NOOPMARK_CALL, ptrint(Func), FThorium.FModules.IndexOf(Func.FModule), GetHighestRegisterInUse()))
            else
              GenCode(noop(THORIUM_NOOPMARK_CALL, ptrint(Func), -1, GetHighestRegisterInUse()));
          end
          else
          begin
            // Write the calling code. If this is not a module local function, we
            // have to write a fcall instruction instead of CALL to tell the vm
            // to switch the whole context.
            if Func.FModule <> FModule then
              GenCode(fcall(Func.EntryPoint, FThorium.FModules.IndexOf(Func.FModule), GetHighestRegisterInUse(), Func.ReturnValues.Count, Func.Parameters.Count))
            else
              GenCode(call(Func.EntryPoint, GetHighestRegisterInUse(), Func.ReturnValues.Count, Func.Parameters.Count));
          end;
          if (Func.ReturnValues.Count > 0) then
          begin
            // Read the result from stack and write it to the target register
            GenCode(movest(TargetRegister));
          end;
          for I := 0 to ExpressionFreeList.Count - 1 do
          begin
            ParamRegID := ExpressionFreeList[I];
            GenCode(clr(ParamRegID));
            ReleaseRegister(ParamRegID);
          end;
        finally
          ExpressionFreeList.Free;
        end;
      end;

    var
      FieldID: QWord;
      RegID, ExprRegID: Word;
      //UseStack: Boolean;
      PreviousWasExtended: Boolean;
      OldHook: Boolean;
      OldHook1, OldHook2: PThoriumInstructionArray;
    begin
      //UseStack := TargetRegister = THORIUM_REGISTER_INVALID;
      Result := False;
      if not GetFreeRegister(trEXP, RegID) then
        Exit;
      OldHook := CodeHook;
      OldHook1 := CodeHook1;
      OldHook2 := CodeHook2;
      try
        SetLength(Ident.GetCode, 0);
        SetLength(Ident.SetCode, 0);
        CodeHook := True;
        CodeHook1 := @Ident.GetCode;
        CodeHook2 := @Ident.SetCode;
        // WriteLn('Declared hook: ', IntToHex(ptruint(CodeHook1), SizeOf(ptruint)*2));
        Result := False;
        if not ExpectSymbol([tsIdentifier]) then
          Exit;
        CurrIdent := CurrentStr;
        Proceed;

        Ident.IsStatic := False;
        Ident.Value := ThoriumCreateBuiltInValue(btUnknown);
        Prop := nil;
        Result := FindTableEntry(CurrIdent, CurrEntry, CurrModule, False, not (ikNoFar in AllowedKinds));
        if not Result then
        begin
          Result := TypeSpecByName(CurrIdent, CurrType, True);
          if not Result then
          begin
            // Okay, neither type nor variable, so we have something undeclared...
            if not (ikUndeclared in AllowedKinds) then
            begin
              // .. which is not allowed
              CompilerError('Undeclared identifier: '''+CurrIdent+'''.');
              Exit;
            end;
            Ident.Kind := ikUndeclared;
            Ident.FullStr := CurrIdent;
            Result := True;
            Exit;
          end;
          // This seems to be a type. First check if types are allowed here
          if not (ikType in AllowedKinds) then
          begin
            // Last chance: Check if this is an extended type with static values
            if CurrType.ValueType = vtExtendedType then
            begin
              if not CurrType.Extended.HasStaticFields then
              begin
                CompilerError('Types are not allowed here and the specified extended type does not have any static fields.');
                Result := False;
                Exit;
              end;
              CurrEntry._Type := etExtendedType;
              CurrEntry.TypeSpec := CurrType;
              CurrEntry.Offset := 0;
              CurrEntry.Scope := 0;
            end
            else
            begin
              // And if not, throw an error, set the result to false and quit.
              CompilerError('Types are not allowed here.');
              Result := False;
              Exit;
            end;
          end
          else
          begin
            // Set the result type
            if (CurrType.ValueType = vtExtendedType) and (ikComplex in AllowedKinds) then
            begin
              CurrEntry._Type := etExtendedType;
              CurrEntry.TypeSpec := CurrType;
              CurrEntry.Offset := 0;
              CurrEntry.Scope := 0;
            end
            else
            begin
              Ident.FinalType := CurrType;
              Ident.Kind := ikType;
              if CurrType.ValueType = vtExtendedType then
                 AddExtendedTypeUsageEx(Ident.UsedExtendedTypes, CurrType.Extended);
              Result := True;
              Exit;
            end;
          end;
          (*if (CurrentSym = tsOpenSquareBracket) then
          begin
            // Check if it is an array type.
            Proceed;
            if not ExpectSymbol([tsCloseSquareBracket, tsIntegerValue]) then
              Exit;
            if (CurrentSym = tsIntegerValue) then
            begin
              Ident.FinalType.ArrayKind := akStatic;
              Ident.FinalType.ArrayCount := StrToInt(CurrentStr);
              Proceed;
              if not ExpectSymbol([tsCloseSquareBracket]) then
                Exit;
            end
            else if (CurrentSym = tsCloseSquareBracket) then
              Ident.FinalType.ArrayKind := akDynamic;
            Proceed;
          end;*)
        end;
        Result := False;

        case CurrEntry._Type of
          etVariable, etRegisterVariable: Ident.Kind := ikVariable;
          etProperty:
          begin
            Prop := TThoriumLibraryProperty(CurrEntry.Ptr);
            Ident.Kind := ikComplex;
            Ident.IsStatic := Prop.GetStatic;
          end;
          etLibraryConstant:
          begin
            Ident.Kind := ikStatic;
            Ident.IsStatic := True;
            Ident.Value := ThoriumDuplicateValue(CurrEntry.Value);
          end;
          etStatic:
          begin
            Ident.Kind := ikStatic;
            Ident.IsStatic := True;
            Ident.Value := ThoriumDuplicateValue(CurrEntry.Value);
          end;
          etFunction:
          begin
            Ident.Kind := ikStatic;
            Ident.IsStatic := True;
          end;
          etExtendedType:
          begin
            Ident.Kind := ikType;
            Ident.IsStatic := True;
          end;
        end;

        if AllowedKinds = [ikUndeclared, ikPrototypedFunction] then
        begin
          // Special handling for this combination because of its usage in the
          // FunctionDeclaration function.
          if CurrEntry._Type = etFunction then
          begin
            if CurrEntry.TypeSpec.Func.FPrototyped then
            begin
              Ident.IsStatic := True;
              Ident.Kind := ikPrototypedFunction;
              Ident.FinalType := CurrEntry.TypeSpec;
              Result := True;
              Exit;
            end;
          end;
          CompilerError('Duplicate identifier: '''+CurrIdent+'''.');
          Result := False;
          Exit;
        end;

        CurrType := CurrEntry.TypeSpec;
        // If it's a variable or a constant, write the code for storing it in
        // the given register
        if (CurrEntry._Type <> etFunction) and (CurrEntry._Type <> etExtendedType) then
        begin
          case CurrEntry._Type of
            etRegisterVariable:
            begin
              GenCodeEx(Ident.GetCode, mover(CurrEntry.Offset, TargetRegister));
              GenCodeEx(Ident.SetCode, mover(TargetRegister, CurrEntry.Offset));
            end;
            etVariable, etStatic:
            begin
              if CurrModule <> -1 then
              begin
                // The value is in another module, so we have to perform the far
                // operations.
                GenCodeEx(Ident.GetCode, movefs(CurrModule, CurrEntry.Offset, TargetRegister));
                GenCodeEx(Ident.SetCode, mover_fs(TargetRegister, CurrModule, CurrEntry.Offset));
              end
              else
              begin
                // In this case, the value is in this module, so we have to perform
                // the local operations.
                GenCodeEx(Ident.GetCode, moves(CurrEntry.Scope, CurrEntry.Offset, TargetRegister));
                GenCodeEx(Ident.SetCode, mover_s(TargetRegister, CurrEntry.Scope, CurrEntry.Offset));
              end;
            end;
            etProperty:
            begin
              GenCodeEx(Ident.GetCode, xpget(Prop, TargetRegister));
              GenCodeEx(Ident.SetCode, xpset(Prop, TargetRegister));
            end;
          end;
        end;

        // Check if an extended type occurs and if so, add it to the list
        if CurrEntry.TypeSpec.ValueType = vtExtendedType then
          AddExtendedTypeUsageEx(Ident.UsedExtendedTypes, CurrEntry.TypeSpec.Extended);

        //RegID := TargetRegister;

        PreviousWasExtended := False;
        // Now, proceed with qualifiers.
        while CurrentSym in [tsDot, tsOpenBracket, tsOpenSquareBracket] do
        begin
          PreviousWasExtended := CurrType.ValueType = vtExtendedType;
          // Set the identifier kind to complex
          Ident.Kind := ikComplex;
          // If we use registers, we will have to change the target registers
          // for the operations, since we cannot use the final register target.
          if (Length(Ident.GetCode) > 0) then
          begin
            // Get the pointer to the buffer instruction
            BufInstruction := @Ident.GetCode[Length(Ident.GetCode)-1];
            case BufInstruction^.Instruction of
              tiMOVES: TThoriumInstructionMOVES(BufInstruction^).TRI := RegID;
              tiMOVER_S: TThoriumInstructionMOVER_S(BufInstruction^).SRI := RegID;
              tiMOVER_FS: TThoriumInstructionMOVER_FS(BufInstruction^).SRI := RegID;
              tiMOVEFS: TThoriumInstructionMOVEFS(BufInstruction^).TRI := RegID;
              tiXFGET: TThoriumInstructionXFGET(BufInstruction^).TRI := RegID;
              tiXIGET: TThoriumInstructionXIGET(BufInstruction^).TRI := RegID;
              tiXPGET: TThoriumInstructionXPGET(BufInstruction^).TRI := RegID;
            end;
          end;
          // Use the same getter and setter for the last operation when having
          // complex expressions
          if (Length(Ident.SetCode) > 0) then
          begin
            Ident.SetCode[Length(Ident.SetCode)-1] := Ident.GetCode[Length(Ident.GetCode)-1];
            if (Length(Ident.SetCode) > 1) then
            begin
              BufInstruction := @Ident.SetCode[Length(Ident.SetCode)-2];
              if BufInstruction^.Instruction = tiXCT then
              begin
                BufInstruction^ := Ident.SetCode[Length(Ident.SetCode)-1];
                SetLength(Ident.SetCode, Length(Ident.SetCode)-1);
              end;
            end;
          end;

          case CurrentSym of
            tsDot: // Access a member of an extended type
            begin
              // Check if we have an extended type.
              if CurrType.ValueType <> vtExtendedType then
              begin
                CompilerError('Extended type expected, but '+ThoriumTypeName(CurrType)+' found.');
                Result := False;
                Exit;
              end;
              // Check for and get the next identifier
              Proceed;
              if not ExpectSymbol([tsIdentifier]) then
                Exit;

              // Check if we are going to access a static member
              if CurrEntry._Type = etExtendedType then
              begin
                // Just set this to any other value, we do not need it anymore
                CurrEntry._Type := etVariable;
                // Check for the field and if existing then get the ID
                if not CurrType.Extended.StaticFieldID(CurrentStr, FieldID) then
                begin
                  // If there is no field with this name, raise compiler error and
                  // exit.
                  CompilerError(ThoriumTypeName(CurrType)+' has no field named '''+CurrentStr+'''.');
                  Result := False;
                  Exit;
                end;
                // Retrieve the field information
                if not CurrType.Extended.StaticFieldType(FieldID, NewEntry) then
                begin
                  // If there is no field with this ID, we have to raise a
                  // compiler error.
                  CompilerError(ThoriumTypeName(CurrType)+' returned an invalid field ID.');
                  Result := False;
                  Exit;
                end;


                if not (NewEntry.TypeSpec.ValueType in [vtFunction, vtHostFunction, vtHostMethod]) then
                begin
                  // Generate the code for accessing the value of the field
                  GenCodeEx(Ident.GetCode, xsfget(FieldID, CurrType.Extended, TargetRegister));
                  GenCodeEx(Ident.SetCode, xsfset(FieldID, CurrType.Extended, TargetRegister));
                end;
              end
              else
              begin
                // Check for the field and if existing then get the ID
                if not CurrType.Extended.FieldID(CurrentStr, FieldID) then
                begin
                  // If there is no field with this name, raise compiler error and
                  // exit.
                  CompilerError(ThoriumTypeName(CurrType)+' has no field named '''+CurrentStr+'''.');
                  Result := False;
                  Exit;
                end;
                // Retrieve the field information
                if not CurrType.Extended.FieldType(FieldID, NewEntry) then
                begin
                  // If there is no field with this ID, we have to raise a
                  // compiler error.
                  CompilerError(ThoriumTypeName(CurrType)+' returned an invalid field ID.');
                  Result := False;
                  Exit;
                end;


                if not (NewEntry.TypeSpec.ValueType in [vtFunction, vtHostFunction, vtHostMethod]) then
                begin
                  // Generate the code for accessing the value of the field
                  GenCodeEx(Ident.GetCode, xfget(FieldID, RegID, TargetRegister));
                  if CurrType.Extended.GetPropertyStoring(FieldID) then
                    GenCodeEx(Ident.SetCode, xct(TargetRegister));
                  GenCodeEx(Ident.SetCode, xfset(FieldID, RegID, TargetRegister));
                end;
              end;

              // Link through the type
              CurrType := NewEntry.TypeSpec;
              // Set the static flag
              Ident.IsStatic := NewEntry._Type <> etVariable;

              Proceed;
            end;
            tsOpenSquareBracket: // Access a field of an array or an extended type.
            begin
              // Check if we have an extended type
              // (later: add ORed check for array here)
              if CurrType.ValueType <> vtExtendedType then
              begin
                CompilerError('Extended type expected, but '''+ThoriumTypeName(CurrType)+''' found.');
                Result := False;
                Exit;
              end;
              // Check if the extended type has an index
              if not CurrType.Extended.HasIndicies then
              begin
                CompilerError('No indicies in '''+ThoriumTypeName(CurrType)+'''.');
                Result := False;
                Exit;
              end;
              Proceed;

              // Get the buffer register for the expression
              if not GetFreeRegister(trEXP, ExprRegID) then
                Exit;

              // Get the expression in the square brackets.
              ExprType := Expression(ExprRegID, False);
              if HasError then
                Exit;

              GenCodeEx(Ident.GetCode, xiget(ExprRegID, RegID, TargetRegister));
              GenCodeEx(Ident.SetCode, xiset(ExprRegID, TargetRegister, RegID));

              // Release register
              ReleaseRegister(ExprRegID);

              // Check if the given type is compatible to the indicies of the
              // extended type.
              if not CurrType.Extended.IndexType(ExprType, NewEntry) then
              begin
                CompilerError('No index for this type in '''+ThoriumTypeName(CurrType)+'''.');
                Result := False;
                Exit;
              end;

              // Check for trailing closing bracket
              if not ExpectSymbol([tsCloseSquareBracket]) then
                Exit;
              Proceed;


            end;
            tsOpenBracket: // Perform a function call
            begin
              if not (CurrType.ValueType in [vtFunction, vtHostFunction, vtHostMethod]) then
              begin
                CompilerError('Function expected, but '''+ThoriumTypeName(CurrType)+''' found.');
                Result := False;
                Exit;
              end;
              Proceed;

              case CurrType.ValueType of
                vtFunction: CallFunction;
                vtHostFunction: CallExternalFunction;
                vtHostMethod: CallExternalFunction(True, RegID);
              end;
              Proceed;
            end;
          end;
          // Check if an extended type occurs and if so, add it to the list
          if CurrType.ValueType = vtExtendedType then
            AddExtendedTypeUsageEx(Ident.UsedExtendedTypes, CurrType.Extended);
        end;

        (*if RegID <> TargetRegister then
        begin
          GenCodeEx(Ident.SetCode, Ident.SetCode[Length(Ident.SetCode)-1]);
          Ident.SetCode[Length(Ident.SetCode)-2] := mover(TargetRegister, RegID);
          Ident.SetCode[Length(Ident.SetCode)-2].CodeLine := Ident.SetCode[Length(Ident.SetCode)-1].CodeLine;
        end;*)

        if (Ident.Kind = ikType) and not (ikType in AllowedKinds) then
        begin
          CompilerError('Types are not allowed here.');
          Result := False;
          Exit;
        end;

        if PreviousWasExtended then
        begin
          //GenCodeEx(Ident.GetCode, clr(RegID));
          //GenCodeEx(Ident.SetCode, clr(RegID));
        end;

        Result := True;
        if not (Ident.Kind in AllowedKinds) and (ikUndeclared in AllowedKinds) then
        begin
          if (Ident.Kind in [ikStatic, ikVariable]) then
          begin
            if CurrEntry.Scope < CurrentScope then
            begin
              Ident.Kind := ikUndeclared;
              Exit;
            end;
            CompilerError('Identifier already declared: '''+Ident.FullStr+'''.');
            Result := False;
            Exit;
          end;
          CompilerError('Undeclared identifier expected but '''+Ident.FullStr+''' found.');
          Result := False;
          Exit;
        end;

        if not (Ident.Kind in AllowedKinds) then
        begin
          CompilerError(THORIUM_IDENTIFIER_KIND_NAMES[Ident.Kind]+'s not allowed here.');
          Result := False;
          Exit;
        end;
        if (Ident.IsStatic) and (not (ikStatic in AllowedKinds)) then
        begin
          CompilerError('Static identifier not allowed here.');
          Result := False;
          Exit;
        end;
        Ident.FinalType := CurrType;
      finally
        CodeHook := OldHook;
        CodeHook1 := OldHook1;
        CodeHook2 := OldHook2;
        ReleaseRegister(RegID);
      end;
    end;

    procedure ConstantDeclaration(TypeIdent, Ident: TThoriumQualifiedIdentifier;
      VisibilityLevel: TThoriumVisibilityLevel; var Offset: Integer);
    // This function processes a declaration of a static identifier.
    // On entering this function, the current symbol is the tsAssign symbol.
    var
      Static: Boolean;
      Value: TThoriumValue;
      TypeSpec: TThoriumType;
    begin
      // Check if the type specification is valid for a constant (this means,
      // must be a built-in value)
      if (TypeIdent.FinalType.ValueType <> vtBuiltIn) or (TypeIdent.FinalType.BuiltInType < btInteger) then
      begin
        CompilerError('Illegal type for static value.');
        Exit;
      end;
      // Check if the following symbol is the expected tsAssign
      if not ExpectSymbol([tsAssign]) then
        Exit;
      Proceed;
      TypeSpec := Expression(0, True, @Static, @Value);
      if not ThoriumCompareTypeEx(TypeSpec, TypeIdent.FinalType) then
      begin
        CompilerError('Incompatible type.');
        Exit;
      end;
      case TypeSpec.BuiltInType of
        btInteger:
        begin
          GenCode(int_s(Value.BuiltIn.Int));
        end;
        btFloat:
        begin
          GenCode(flt_s(Value.BuiltIn.Float));
        end;
        btString:
        begin
          if Value.BuiltIn.Str^ = '' then
            GenCode(str_s())
          else
            GenCode(strl_s(AddLibraryString(Value.BuiltIn.Str^)));
        end;
      end;
      // Write the identifier to the identifier table of the compiler
      Table.AddConstantIdentifier(Ident.FullStr, CurrentScope, Offset, TypeIdent.FinalType, Value);
      // If the static value is public...
      if VisibilityLevel > vsPrivate then
      begin
        // ... add it to the public variable register
        with AddPublicVariable do
        begin
          FName := Ident.FullStr;
          FIsStatic := True;
          FTypeSpec := TypeIdent.FinalType;
          FStackPosition := Offset;
        end;
      end;
      // Increase the current stack offset
      Inc(Offset);
      // Check for the trailing semicolon
      if not ExpectSymbol([tsSemicolon]) then
        Exit;
      Proceed;
    end;

    procedure VariableDeclaration(TypeIdent, Ident: TThoriumQualifiedIdentifier;
      VisibilityLevel: TThoriumVisibilityLevel; var Offset: Integer;
      RegisterVariable: TThoriumRegisterID = THORIUM_REGISTER_INVALID);
    // This function processes the declaration of a non-static identifier.
    // On entering this function, the current symbol is either the tsAssign or
    // the semicolon
    var
      ValueType: TThoriumType;
      RegID: TThoriumRegisterID;
    begin
      // Check if the following symbol for being tsAssign or tsSemicolon
      if not ExpectSymbol([tsAssign, tsSemicolon]) then
        Exit;
      // Initialize the helper value
      // Check if we have an initial value...
      if CurrentSym = tsAssign then
      begin
        Proceed;
        // If this is going to be a register variable, we do not need one more
        // intermediate register. Just reuse the target.
        if RegisterVariable <> THORIUM_REGISTER_INVALID then
          RegID := RegisterVariable
        else
          if not GetFreeRegister(trEXP, RegID) then
            Exit;
        // Parse the expression and if possible, compile time evaluate it.
        ValueType := Expression(RegID, False);
        // There is no need to care about static expressions. We would only do
        // the same with them as the default handling would do.
        if RegisterVariable = THORIUM_REGISTER_INVALID then
        begin
          // In this case we need to release the register reserved and to move
          // the variable to the stack.
          GenCode(mover_st(RegID));
          ReleaseRegister(RegID);
        end;
      end
      else
      begin
        // In case of a normal variable...
        if RegisterVariable = THORIUM_REGISTER_INVALID then
        begin
          // Check which type we have and write a corresponding instruction with
          // a default initial value.
          PushEmpty(TypeIdent.FinalType);
        end
        else
        begin
          // Register variables need special handling (gouuchy gouuchy).
          case TypeIdent.FinalType.ValueType of
            vtBuiltIn:
            begin
              case TypeIdent.FinalType.BuiltInType of
                btInteger:
                begin
                  GenCode(int(0, RegisterVariable));
                end;
                btFloat:
                begin
                  GenCode(flt(0, RegisterVariable));
                end;
                btString:
                begin
                  GenCode(str(RegisterVariable));
                end;
              end;
            end;
            vtExtendedType:
            begin
              GenCode(ext(TypeIdent.FinalType.Extended, RegisterVariable));
            end;
          else
            raise EThoriumCompilerException.Create('Invalid type for register variable.');
          end;
        end;
      end;

      if RegisterVariable <> THORIUM_REGISTER_INVALID then
      begin
        // Add the identifier to the identifier table of the compiler
        Table.AddRegisterVariableIdentifier(Ident.FullStr, RegisterVariable, TypeIdent.FinalType);
        if VisibilityLevel > vsPrivate then
          raise EThoriumCompilerException.Create('Register variables cannot be public!');
      end
      else
      begin
        // Add the identifier to the identifier table of the compiler.
        Table.AddVariableIdentifier(Ident.FullStr, CurrentScope, Offset, TypeIdent.FinalType);
        // If it's a public value...
        if VisibilityLevel > vsPrivate then
        begin
          // ... we register it at the public variables.
          with AddPublicVariable do
          begin
            FName := Ident.FullStr;
            FIsStatic := False;
            FStackPosition := Offset;
            FTypeSpec := TypeIdent.FinalType;
          end;
        end;
        // Increase the stack offset.
        Inc(Offset);
      end;
      // Check for the trailing semicolon
      if not ExpectSymbol([tsSemicolon]) then
        Exit;
      Proceed;
    end;

    function SimpleExpression(TargetRegister: TThoriumRegisterID; NeedStatic: Boolean = False; IsStatic: PBoolean = nil; StaticValue: PThoriumValue = nil; IsExpressionBased: PBoolean = nil): TThoriumType;
    // This pareses a simple expresion. A simple expression consists of
    // Terms which can be added.

      function Term(TargetRegister: TThoriumRegisterID; out IsStatic: Boolean; out StaticValue: TThoriumValue; out IsExpressionBased: Boolean): TThoriumType;
      // This parses a term. A term can consist of multiple factors which
      // might be combined using a multiplication.

        function Factor(TargetRegister: TThoriumRegisterID; out IsStatic: Boolean; out StaticValue: TThoriumValue; out IsExpressionBased: Boolean): TThoriumType;
        // This parses a factor. A factor can be:
        // - A string
        // - An integer
        // - A float
        // - An expression (enclosed in brackets)
        // - An identifier
        var
          HasPrecedingOperator: Boolean;
          Sym: TThoriumSymbol;
          IdentInfo: TThoriumQualifiedIdentifier;
          Dummy: TThoriumType;
          Int: Int64;
          Float: Double;
          Static: Boolean;
          Value: TThoriumValue;
        begin
          HasPrecedingOperator := CurrentSym in [tsPlusPlus, tsMinusMinus, tsNot, tsMinus, tsBoolNot];
          if HasPrecedingOperator then
          begin
            Sym := CurrentSym;
            Proceed;
            if Sym in [tsNot, tsMinus, tsBoolNot] then
            begin
              Result := Factor(TargetRegister, Static, Value, IsExpressionBased);
              IsExpressionBased := True;
              IsStatic := Static;
              case Sym of
                tsNot:
                begin
                  if Static then
                  begin
                    if not ThoriumEvaluateBuiltInOperation(Value, StaticValue, toNot, StaticValue) then
                    begin
                      CompilerError('Cannot apply NOT operator on this operand.');
                      Exit;
                    end;
                  end
                  else
                  begin
                    if not IsTypeOperationAvailable(Result, toNot, Result) then
                    begin
                      CompilerError('Cannot apply NOT operator on this operand.');
                      Exit;
                    end;
                    GenCode(_operator(TargetRegister, 0, TargetRegister, Result, toNot));
                  end;
                end;
                tsMinus:
                begin
                  if Static then
                  begin
                    if not ThoriumEvaluateBuiltInOperation(Value, StaticValue, toNegate, StaticValue) then
                    begin
                      CompilerError('Cannot apply NEGATE operator on this operand.');
                      Exit;
                    end;
                  end
                  else
                  begin
                    if not IsTypeOperationAvailable(Result, toNegate, Result) then
                    begin
                      CompilerError('Cannot apply NEGATE operator on this operand.');
                      Exit;
                    end;
                    GenCode(_operator(TargetRegister, 0, TargetRegister, Result, toNegate));
                  end;
                end;
                tsBoolNot:
                begin
                  if Static then
                  begin
                    if not ThoriumEvaluateBuiltInOperation(Value, StaticValue, toBoolNot, StaticValue) then
                    begin
                      CompilerError('Cannot apply BOOLEAN NOT operator on this operand.');
                      Exit;
                    end;
                  end
                  else
                  begin
                    if not IsTypeOperationAvailable(Result, toBoolNot, Result) then
                    begin
                      CompilerError('Cannot apply BOOLEAN NOT operator on this operand.');
                      Exit;
                    end;
                    GenCode(_operator(TargetRegister, 0, TargetRegister, Result, toBoolNot));
                  end;
                end;
              end;
              Exit;
            end;
          end;

          IsStatic := False;

          case CurrentSym of
            tsIntegerValue: // An integer value
            begin
              // Check if there is a preceding operator (which is not allowed
              // on this kind of factor).
              if HasPrecedingOperator then
              begin
                CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to static values.');
                Exit;
              end;

              // Read it out and write it to the target register.
              IsStatic := True;
              Result := ThoriumBuiltInTypeSpec(btInteger);
              Int := StrToInt64(CurrentStr);
              // GenCode(int(Int, TargetRegister));
              IsStatic := True;
              IsExpressionBased := True;
              StaticValue := ThoriumCreateBuiltInValue(btInteger);
              StaticValue.BuiltIn.Int := Int;
              Proceed;
            end;
            tsFloatValue: // A float value
            begin
              // Check if there is a preceding operator (which is not allowed
              // on this kind of factor).
              if HasPrecedingOperator then
              begin
                CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to static values.');
                Exit;
              end;

              // Read it out, convert it to int64 and write it to the target
              // register.
              Result := ThoriumBuiltInTypeSpec(btFloat);
              Float := StrToFloat(CurrentStr, THORIUM_NUMBER_FORMAT);
              // GenCode(flt(Float, TargetRegister));
              IsStatic := True;
              IsExpressionBased := True;
              StaticValue := ThoriumCreateBuiltInValue(btFloat);
              StaticValue.BuiltIn.Float := Float;
              Proceed;
            end;
            tsStringValue: // A string value
            begin
              // Check if there is a preceding operator (which is not allowed
              // on this kind of factor).
              if HasPrecedingOperator then
              begin
                CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to static values.');
                Exit;
              end;

              // Read the string, save it to the library and generate the
              // loading instruction.
              Result := ThoriumBuiltInTypeSpec(btString);
              (*if CurrentStr = '' then
                GenCode(str(TargetRegister))
              else
                GenCode(strl(AddLibraryString(CurrentStr), TargetRegister)); *)
              IsStatic := True;
              IsExpressionBased := True;
              StaticValue := ThoriumCreateBuiltInValue(btString);
              StaticValue.BuiltIn.Str^ := CurrentStr;
              Proceed;
            end;
            tsOpenBracket: // A nested expression
            begin
              // Check if there is a preceding operator (which is not allowed
              // on this kind of factor).
              if HasPrecedingOperator then
              begin
                CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to static values.');
                Exit;
              end;

              Proceed;
              // Parse the expression (we can just chain the register through)
              Result := Expression(TargetRegister, NeedStatic, @IsStatic, @StaticValue, @IsExpressionBased);
              if HasError then
                Exit;
              if not ExpectSymbol([tsCloseBracket]) then
                Exit;
              Proceed;
            end;
            tsTrue: // A simple true
            begin
              // Check if there is a preceding operator (which is not allowed
              // on this kind of factor).
              if HasPrecedingOperator then
              begin
                CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to static values.');
                Exit;
              end;

              // Write a true (= 1) to the target register
              // GenCode(int(1, TargetRegister));
              Result := ThoriumBuiltInTypeSpec(btInteger);
              IsStatic := True;
              IsExpressionBased := True;
              StaticValue := ThoriumCreateBuiltInValue(btInteger);
              StaticValue.BuiltIn.Int := 1;
              Proceed;
            end;
            tsFalse: // A simple false
            begin
              // Check if there is a preceding operator (which is not allowed
              // on this kind of factor).
              if HasPrecedingOperator then
              begin
                CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to static values.');
                Exit;
              end;

              // Write a false (= 0) to the target register.
              // GenCode(int(0, TargetRegister));
              Result := ThoriumBuiltInTypeSpec(btInteger);
              IsStatic := True;
              IsExpressionBased := True;
              StaticValue := ThoriumCreateBuiltInValue(btInteger);
              StaticValue.BuiltIn.Int := 0;
              Proceed;
            end;
            tsIdentifier: // An identifier
            begin
              // WriteLn('=== Ident: ', CurrentStr);
              // Parse the identifier
              if not QualifyIdentifier(IdentInfo, [ikVariable, ikStatic, ikComplex], TargetRegister) then
                Exit;
              if HasError then
                Exit;
              // Check if we have a preceding operator and something else than
              // a variable.
              if HasPrecedingOperator and (IdentInfo.Kind <> ikVariable) then
              begin
                CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to anything but variables.');
                Exit;
              end;
              // Check if we have a operator behind the identifier
              if (CurrentSym in [tsPlusPlus, tsMinusMinus]) then
              begin
                if (IdentInfo.Kind <> ikVariable) then
                begin
                  CompilerError('Cannot apply '+THORIUM_SYMBOL_NAMES[Sym]+' to anything but variables.');
                  Exit;
                end
                else if HasPrecedingOperator then
                begin
                  CompilerError('Cannot apply a plusplus/minusminus operator twice.');
                  Exit;
                end;
              end;

              // Process the preceding operator
              if HasPrecedingOperator then
              begin
                if (IdentInfo.FinalType.ValueType <> vtBuiltIn) then
                begin
                  CompilerError('Cannot apply ++/-- operator on extended values or functions.');
                  Exit;
                end
                else if (not IsTypeOperationAvailable(IdentInfo.FinalType, toINC_DEC, Dummy)) then
                begin
                  CompilerError('Cannot apply ++/-- operator on the given type.');
                  Exit;
                end;
                // Direct access ability is forced, so we can access it directly.
                if IdentInfo.GetCode[0].Instruction = tiCOPYS then
                begin
                  // Apply inc/dec depending on given type
                  case Sym of
                    tsPlusPlus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(inci_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(incf_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                    tsMinusMinus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(deci_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(decf_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                  end;
                end
                else if IdentInfo.GetCode[0].Instruction = tiCOPYFS then
                begin
                  case Sym of
                    tsPlusPlus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(inci_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(incf_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                    tsMinusMinus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(deci_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(decf_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                  end;
                end;
              end;

              if (IdentInfo.IsStatic) and (IdentInfo.Value._Type = vtBuiltIn) and (IdentInfo.Value.BuiltIn._Type <> btUnknown) then
              begin
                IsStatic := True;
                StaticValue := IdentInfo.Value;
              end
              else
              begin
                AppendCode(IdentInfo.GetCode);
                AddHostTypeUsages(IdentInfo.UsedExtendedTypes);
              end;

              // Process the trailing operator
              if CurrentSym in [tsPlusPlus, tsMinusMinus] then
              begin
                if (IdentInfo.FinalType.ValueType <> vtBuiltIn) then
                begin
                  CompilerError('Cannot apply ++/-- operator on extended values or functions.');
                  Exit;
                end
                else if (not IsTypeOperationAvailable(IdentInfo.FinalType, toINC_DEC, Dummy)) then
                begin
                  CompilerError('Cannot apply ++/-- operator on the given type.');
                  Exit;
                end;
                // Direct access ability is forced, so we can access it directly.
                if IdentInfo.GetCode[0].Instruction = tiCOPYS then
                begin
                  // Apply inc/dec depending on given type
                  case Sym of
                    tsPlusPlus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(inci_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(incf_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                    tsMinusMinus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(deci_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(decf_s(TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                  end;
                end
                else if IdentInfo.GetCode[0].Instruction = tiCOPYFS then
                begin
                  case Sym of
                    tsPlusPlus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(inci_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(incf_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                    tsMinusMinus:
                    begin
                      case IdentInfo.FinalType.BuiltInType of
                        btInteger: GenCode(deci_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                        btFloat: GenCode(decf_fs(TThoriumInstructionCOPYFS(IdentInfo.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                  end;
                end;
                Proceed;
              end;

              IsExpressionBased := False;
              Result := IdentInfo.FinalType;
            end;
          else
            CompilerError('Invalid operand.');
          end;
        end;

      var
        Sym: TThoriumSymbol;
        Op: TThoriumOperation;
        PrevType: TThoriumType;
        OperandType: TThoriumType;
        RegID2, RegBuf: TThoriumRegisterID;
        Static1, Static2: Boolean;
        Value1, Value2, ResultValue: TThoriumValue;
      begin
        // (Term)
        Static1 := False;
        Static2 := False;

        // Parse the factor
        Result := Factor(TargetRegister, Static1, Value1, IsExpressionBased);
        if HasError then
          Exit;
        // Allocate two more registers
        if not GetFreeRegister(trEXP, RegID2, True) then
          Exit;
        // Parse more operators and operands
        while CurrentSym in THORIUM_MULTIPLICATIVE_OPERATOR do
        begin
          // Save the symbol and operator
          Sym := CurrentSym;
          Op := ThoriumSymbolToOperation(Sym);
          Proceed;
          // Parse the next factor
          if Static2 then
            ThoriumFreeValue(Value2);
          OperandType := Factor(RegID2, Static2, Value2, IsExpressionBased);
          IsExpressionBased := True;
          if HasError then
            Exit;
          // Check if the specified operation is available for the given
          // types
          PrevType := Result;
          if not NeedTypeOperation(Result, OperandType, Op, Result) then
            Exit;

          // Check if both operands are static. If that is the case, we can
          // evaluate the expression in compile time which is much better.
          // We do not have to check for builtin type since Factor is only able
          // to give such values as static.
          if (Static1 and Static2) then
          begin
            if ThoriumEvaluateBuiltInOperation(Value1, Value2, Op, ResultValue) then
            begin
              ReleaseRegister(RegID2);
              ThoriumFreeValue(Value1);
              ThoriumFreeValue(Value2);
              Value1 := ResultValue;
              Continue;
            end;
          end;
          if Static1 then
          begin
            // Write the static value to the target register since it seems not
            // to be possible to evaluate the result at compile time.
            case Value1.BuiltIn._Type of
              btInteger: GenCode(int(Value1.BuiltIn.Int, TargetRegister));
              btFloat: GenCode(flt(Value1.BuiltIn.Float, TargetRegister));
              btString:
              begin
                if Value1.BuiltIn.Str^ = '' then
                  GenCode(str(TargetRegister))
                else
                  GenCode(strl(AddLibraryString(Value1.BuiltIn.Str^), TargetRegister));
              end;
            end;
            // Release the value and mark it as non-static
            ThoriumFreeValue(Value1);
            Static1 := False;
          end;
          if Static2 then
          begin
            // Write the static value to the register RegID2 since it seems to
            // be impossible to evaluate the result at compile time.
            case Value2.BuiltIn._Type of
              btInteger: GenCode(int(Value2.BuiltIn.Int, RegID2));
              btFloat: GenCode(flt(Value2.BuiltIn.Float, RegID2));
              btString:
              begin
                if Value2.BuiltIn.Str^ = '' then
                  GenCode(str(RegID2))
                else
                  GenCode(strl(AddLibraryString(Value2.BuiltIn.Str^), RegID2));
              end;
            end;
            // Free the value and mark it as non-static.
            ThoriumFreeValue(Value2);
            Static2 := False;
          end;

          if ThoriumTypeNeedsClear(OperandType) then
          begin
            if not ThoriumCompareTypeEx(OperandType, Result) then
            begin
              if not GetFreeRegister(trEXP, RegBuf) then
                Exit;
              GenCode(_cast(RegID2, RegBuf, OperandType, Result));
              GenCode(clr(RegID2));
              ReleaseRegister(RegID2);
              RegID2 := RegBuf;
            end;
          end
          else
            if not ThoriumCompareTypeEx(OperandType, Result) then
              GenCode(_cast(RegID2, RegID2, OperandType, Result));
          if ThoriumTypeNeedsClear(PrevType) and not ThoriumCompareTypeEx(PrevType, Result) then
          begin
            if not GetFreeRegister(trEXP, RegBuf) then
              Exit;
            GenCode(_cast(TargetRegister, RegBuf, PrevType, Result));
            GenCode(clr(TargetRegister));
            GenCode(_operator(RegBuf, RegID2, TargetRegister, Result, Op));
            if ThoriumTypeNeedsClear(Result) then
              GenCode(clr(RegBuf));
            ReleaseRegister(RegBuf);
          end
          else
          begin
            if not ThoriumCompareTypeEx(PrevType, Result) then
              GenCode(_cast(TargetRegister, TargetRegister, PrevType, Result));
            GenCode(_operator(TargetRegister, RegID2, TargetRegister, Result, Op));
          end;
        end;
        ReleaseRegister(RegID2);
        IsStatic := Static1;
        StaticValue := Value1;
      end;

    var
      Sym: TThoriumSymbol;
      Op: TThoriumOperation;
      PrevType: TThoriumType;
      OperandType: TThoriumType;
      RegBuf: TThoriumRegisterID;
      RegID2: TThoriumRegisterID;
      Static1, Static2: Boolean;
      ExpressionBased: Boolean;
      Value1, Value2, ResultValue: TThoriumValue;
    begin
      // (Simple Expression)
      Static1 := False;
      Static2 := False;

      // Parse the first term.
      Result := Term(TargetRegister, Static1, Value1, ExpressionBased);
      if HasError then
        Exit;
      // Allocate two more registers
      if not GetFreeRegister(trEXP, RegID2, True) then
        Exit;
      // Parse more operators and operands
      while CurrentSym in THORIUM_ADDITIVE_OPERATOR do
      begin
        // Save the symbol and operator
        Sym := CurrentSym;
        Op := ThoriumSymbolToOperation(Sym);
        Proceed;
        // Parse the term
        if Static2 then
          ThoriumFreeValue(Value2);
        OperandType := Term(RegID2, Static2, Value2, ExpressionBased);
        ExpressionBased := True;
        if HasError then
          Exit;
        // Check if the operation is available
        PrevType := Result;
        if not NeedTypeOperation(Result, OperandType, Op, Result) then
          Exit;

        // See the same code in Term for more info
        if Static1 and Static2 then
        begin
          if ThoriumEvaluateBuiltInOperation(Value1, Value2, Op, ResultValue) then
          begin
            ReleaseRegister(RegID2);
            ThoriumFreeValue(Value1);
            ThoriumFreeValue(Value2);
            Value1 := ResultValue;
            Continue;
          end;
        end;
        if Static1 then
        begin
          // Write the static value to the target register since it seems not
          // to be possible to evaluate the result at compile time.
          case Value1.BuiltIn._Type of
            btInteger: GenCode(int(Value1.BuiltIn.Int, TargetRegister));
            btFloat: GenCode(flt(Value1.BuiltIn.Float, TargetRegister));
            btString:
            begin
              if Value1.BuiltIn.Str^ = '' then
                GenCode(str(TargetRegister))
              else
                GenCode(strl(AddLibraryString(Value1.BuiltIn.Str^), TargetRegister));
            end;
          end;
          // Release the value and mark it as non-static
          ThoriumFreeValue(Value1);
          Static1 := False;
        end;
        if Static2 then
        begin
          // Write the static value to the register RegID2 since it seems to
          // be impossible to evaluate the result at compile time.
          case Value2.BuiltIn._Type of
            btInteger: GenCode(int(Value2.BuiltIn.Int, RegID2));
            btFloat: GenCode(flt(Value2.BuiltIn.Float, RegID2));
            btString:
            begin
              if Value2.BuiltIn.Str^ = '' then
                GenCode(str(RegID2))
              else
                GenCode(strl(AddLibraryString(Value2.BuiltIn.Str^), RegID2));
            end;
          end;
          // Free the value and mark it as non-static.
          ThoriumFreeValue(Value2);
          Static2 := False;
        end;

        // Finally generate the code for the specific operation
        if ThoriumTypeNeedsClear(OperandType) then
        begin
          if not ThoriumCompareTypeEx(OperandType, Result) then
          begin
            if not GetFreeRegister(trEXP, RegBuf) then
              Exit;
            GenCode(_cast(RegID2, RegBuf, OperandType, Result));
            GenCode(clr(RegID2));
            ReleaseRegister(RegID2);
            RegID2 := RegBuf;
          end;
        end
        else
          if not ThoriumCompareTypeEx(OperandType, Result) then
            GenCode(_cast(RegID2, RegID2, OperandType, Result));
        if ThoriumTypeNeedsClear(PrevType) and not ThoriumCompareTypeEx(PrevType, Result) then
        begin
          if not GetFreeRegister(trEXP, RegBuf) then
            Exit;
          GenCode(_cast(TargetRegister, RegBuf, PrevType, Result));
          GenCode(clr(TargetRegister));
          GenCode(_operator(RegBuf, RegID2, TargetRegister, Result, Op));
          if ThoriumTypeNeedsClear(Result) then
            GenCode(clr(RegBuf));
          ReleaseRegister(RegBuf);
        end
        else
        begin
          if not ThoriumCompareTypeEx(PrevType, Result) then
            GenCode(_cast(TargetRegister, TargetRegister, PrevType, Result));
          GenCode(_operator(TargetRegister, RegID2, TargetRegister, Result, Op));
        end;
        // Release the register
        ReleaseRegister(RegID2);
        (*case Op of
          toAdd: GenCode(ADD_R(THORIUM_OP_A_ADD, 0, TargetRegister));
          toSubtract: GenCode(ADD_R(THORIUM_OP_A_SUBTRACT, 0, TargetRegister));
          toOr: GenCode(ADD_R(THORIUM_OP_A_OR, 0, TargetRegister));
          toXor: GenCode(ADD_R(THORIUM_OP_A_XOR, 0, TargetRegister));
        else
          CompilerError('Invalid operation');
        end;*)
      end;
      ReleaseRegister(RegID2);
      if IsStatic <> nil then
      begin
        IsStatic^ := Static1;
        StaticValue^ := Value1;
      end
      else
      begin
        if Static1 then
        begin
          case Value1.BuiltIn._Type of
            btInteger: GenCode(int(Value1.BuiltIn.Int, TargetRegister));
            btFloat: GenCode(flt(Value1.BuiltIn.Float, TargetRegister));
            btString:
            begin
              if Value1.BuiltIn.Str^ = '' then
                GenCode(str(TargetRegister))
              else
                GenCode(strl(AddLibraryString(Value1.BuiltIn.Str^), TargetRegister));
            end;
          end;
          // Release the value and mark it as non-static
          ThoriumFreeValue(Value1);
          Static1 := False;
        end;
      end;
      if IsExpressionBased <> nil then
        IsExpressionBased^ := ExpressionBased;
    end;

    function Expression(TargetRegister: TThoriumRegisterID; NeedStatic: Boolean = False; IsStatic: PBoolean = nil; StaticValue: PThoriumValue = nil; IsExpressionBased: PBoolean = nil): TThoriumType; inline;
    // This function parses a given expression.

    var
      Sym: TThoriumSymbol;
      Op: TThoriumOperation;
      PrevType: TThoriumType;
      OperandType: TThoriumType;
      RegID2: TThoriumRegisterID;
      Static1, Static2: Boolean;
      ExpressionBased: Boolean;
      Value1, Value2, ResultValue: TThoriumValue;
    begin
      // (Expression)
      Static1 := False;
      Static2 := False;

      // First we check if it can be an expression at all by checking if the
      // given symbol can be the initiator of a factor.
      if not ExpectSymbol(THORIUM_FACTOR_INITIATOR) then
        Exit;
      // Then we get the first simple expression part.
      ExpressionBased := False;
      Result := SimpleExpression(TargetRegister, NeedStatic, @Static1, @Value1, @ExpressionBased);
      if HasError then
        Exit;

      // Parse more operators and operands.
      while CurrentSym in THORIUM_RELATIONAL_OPERATOR do
      begin
        // Allocate the needed registers.
        if not GetFreeRegister(trEXP, RegID2, True) then
          Exit;
        Sym := CurrentSym;
        Op := ThoriumSymbolToOperation(Sym);
        Proceed;
        ExpressionBased := True;
        OperandType := SimpleExpression(RegID2, NeedStatic, @Static2, @Value2);
        if HasError then
          Exit;
        // Check if the operation is available
        PrevType := Result;
        if not NeedTypeOperation(Result, OperandType, Op, Result) then
          Exit;

        if (Static1 and Static2) then
        begin
          if ThoriumCompareBuiltIn(Value1, Value2, Sym, ResultValue) then
          begin
            ReleaseRegister(RegID2);
            ThoriumFreeValue(Value1);
            ThoriumFreeValue(Value2);
            Value1 := ResultValue;
            Continue;
          end;
        end;
        if Static1 then
        begin
          case Value1.BuiltIn._Type of
            btInteger: GenCode(int(Value1.BuiltIn.Int, TargetRegister));
            btFloat: GenCode(flt(Value1.BuiltIn.Float, TargetRegister));
            btString:
            begin
              if Value1.BuiltIn.Str^ = '' then
                GenCode(str(TargetRegister))
              else
                GenCode(strl(AddLibraryString(Value1.BuiltIn.Str^), TargetRegister));
            end;
          end;
          // Release the value and mark it as non-static
          ThoriumFreeValue(Value1);
          Static1 := False;
        end;
        if Static2 then
        begin
          case Value2.BuiltIn._Type of
            btInteger: GenCode(int(Value2.BuiltIn.Int, RegID2));
            btFloat: GenCode(flt(Value2.BuiltIn.Float, RegID2));
            btString:
            begin
              if Value2.BuiltIn.Str^ = '' then
                GenCode(str(RegID2))
              else
                GenCode(strl(AddLibraryString(Value2.BuiltIn.Str^), RegID2));
            end;
          end;
          // Release the value and mark it as non-static
          ThoriumFreeValue(Value2);
          Static2 := False;
        end;

        // Finally generate the code for the specific operation
        GenCode(_cmpOperator(TargetRegister, RegID2, PrevType, OperandType));
        if ThoriumTypeNeedsClear(PrevType) then
          GenCode(clr(TargetRegister));
        if ThoriumTypeNeedsClear(OperandType) then
          GenCode(clr(RegID2));
        // Release the operand registers.
        ReleaseRegister(RegID2);
        case Sym of
          tsEqual: GenCode(intb(TargetRegister, THORIUM_OP_EQUAL));
          tsNotEqual: GenCode(intb(TargetRegister, THORIUM_OP_NOTEQUAL));
          tsLesser: GenCode(intb(TargetRegister, THORIUM_OP_LESS));
          tsLesserEqual: GenCode(intb(TargetRegister, THORIUM_OP_LESSEQUAL));
          tsGreater: GenCode(intb(TargetRegister, THORIUM_OP_GREATER));
          tsGreaterEqual: GenCode(intb(TargetRegister, THORIUM_OP_GREATEREQUAL));
        end;
        // Set the current output as first operand and set the result to integer
        Result := ThoriumBuiltInTypeSpec(btInteger);
      end;

      if (IsStatic <> nil) then
      begin
        IsStatic^ := Static1;
        StaticValue^ := Value1;
      end
      else
      begin
        if Static1 then
        begin
          case Value1.BuiltIn._Type of
            btInteger: GenCode(int(Value1.BuiltIn.Int, TargetRegister));
            btFloat: GenCode(flt(Value1.BuiltIn.Float, TargetRegister));
            btString:
            begin
              if Value1.BuiltIn.Str^ = '' then
                GenCode(str(TargetRegister))
              else
                GenCode(strl(AddLibraryString(Value1.BuiltIn.Str^), TargetRegister));
            end;
          end;
          // Release the value and mark it as non-static
          ThoriumFreeValue(Value1);
          Static1 := False;
        end;
      end;
      if IsExpressionBased <> nil then
        IsExpressionBased^ := ExpressionBased;
    end;

    function ConditionalExpressionRegisterCaching(TrueAddress: TThoriumInstructionAddress; FalseAddress: TThoriumInstructionAddress; CacheRegister: TThoriumRegisterID; CompareInstruction: PThoriumInstructionAddress = nil; IsStatic: PBoolean = nil; TrueJmp: PCardinal = nil; FalseJmp: PCardinal = nil): TThoriumType;
    // Parses a comparing expression. It tries to cache a constant member of
    // this expression in a register to avoid the writing of a value in a loop
    // for example.
    // It returns the type of the expression which is cached in the register to
    // allow the user to clear the register if neccessary.
    var
      Sym: TThoriumSymbol;
      OldHook: Boolean;
      OldHook1, OldHook2: PThoriumInstructionArray;
      MyHook1, MyHook2: TThoriumInstructionArray;
      RegID1, RegID2: TThoriumRegisterID;
      Static1, Static2: Boolean;
      Value1, Value2, ResultValue: TThoriumValue;
      Type1, Type2, ResultType: TThoriumType;
      Op: TThoriumOperator;
      I: Integer;
    begin
      // (ConditionalExpressionRegisterCaching)
      OldHook := CodeHook;
      OldHook1 := CodeHook1;
      OldHook2 := CodeHook2;
      try
        CodeHook := True;
        CodeHook1 := @MyHook1;
        CodeHook2 := nil;
        if not GetFreeRegister(trEXP, RegID1) then
          Exit;
        Type1 := SimpleExpression(RegID1, False, @Static1, @Value1);
        if HasError then Exit;
        if not ExpectSymbol(THORIUM_RELATIONAL_OPERATOR) then
          Exit;
        Sym := CurrentSym;
        Proceed;

        CodeHook1 := @MyHook2;
        if not GetFreeRegister(trEXP, RegID2) then
          Exit;
        Type2 := SimpleExpression(RegID2, False, @Static2, @Value2);
        if HasError then Exit;

        if not NeedTypeOperation(Type1, Type2, toCompare, ResultType) then
          Exit;
      finally
        CodeHook := OldHook;
        CodeHook1 := OldHook1;
        CodeHook2 := OldHook2;
      end;
      if Static1 and Static2 then
      begin
        if ThoriumCompareBuiltIn(Value1, Value2, Sym, ResultValue) then
        begin
          ReleaseRegister(RegID1);
          ReleaseRegister(RegID2);
          if IsStatic <> nil then
            IsStatic^ := True;
          if CompareInstruction <> nil then
            CompareInstruction^ := FInstructions.Position;
          if ResultValue.BuiltIn.Int = 1 then
          begin
            if TrueAddress <> THORIUM_JMP_INVALID then
              GenCode(jmp(TrueAddress));
          end
          else
          begin
            if FalseAddress <> THORIUM_JMP_INVALID then
              GenCode(jmp(FalseAddress));
          end;
          Result := ThoriumBuiltInTypeSpec(btUnknown);
          Exit;
        end
        else
          raise EThoriumCompilerException.Create('Compiler cries for help: "I dunno what to do now :("');
      end;
      Result := ThoriumBuiltInTypeSpec(btUnknown);
      if Static1 then
      begin
        case Value1.BuiltIn._Type of
          btInteger: GenCode(int(Value1.BuiltIn.Int, CacheRegister));
          btFloat: GenCode(flt(Value1.BuiltIn.Float, CacheRegister));
          btString:
          begin
            if Value1.BuiltIn.Str^ = '' then
              GenCode(str(CacheRegister))
            else
              GenCode(strl(AddLibraryString(Value1.BuiltIn.Str^), CacheRegister));
          end;
        end;
        // Release the value and mark it as non-static
        Result := ThoriumExtractTypeSpec(Value1);
        ThoriumFreeValue(Value1);
      end;
      if Static2 then
      begin
        case Value2.BuiltIn._Type of
          btInteger: GenCode(int(Value2.BuiltIn.Int, CacheRegister));
          btFloat: GenCode(flt(Value2.BuiltIn.Float, CacheRegister));
          btString:
          begin
            if Value2.BuiltIn.Str^ = '' then
              GenCode(str(CacheRegister))
            else
              GenCode(strl(AddLibraryString(Value2.BuiltIn.Str^), CacheRegister));
          end;
        end;
        // Release the value and mark it as non-static
        Result := ThoriumExtractTypeSpec(Value2);
        ThoriumFreeValue(Value2);
      end;
      if not (Static1 or Static2) then
      begin
        if MyHook1[0].Instruction = tiCOPYR then
        begin
          // This one is a register variable - fast access, no need to cache
          if MyHook2[0].Instruction <> tiCOPYR then
          begin
            // mkay... now, find out where the TRI value of the last instruction
            // lies...
            I := Length(MyHook2)-1;
            while MyHook2[I].Instruction = tiCLR do
              Dec(I);
            Static2 := True;
            case MyHook2[I].Instruction of
              tiADDF, tiADDI, tiADDS, tiSUBF, tiSUBI, tiMULF, tiMULI, tiDIVF,
              tiDIVI, tiMOD, tiAND, tiOR, tiXOR, tiSHL, tiSHR:
                TThoriumInstructionADDI(MyHook2[I]).TRI := CacheRegister;
              tiINT, tiFLT, tiEXT:
                TThoriumInstructionINT(MyHook2[I]).TRI := CacheRegister;
              tiSTR:
                TThoriumInstructionSTR(MyHook2[I]).TRI := CacheRegister;
              tiSTRL:
                TThoriumInstructionSTRL(MyHook2[I]).TRI := CacheRegister;
              tiCOPYS:
                TThoriumInstructionCOPYS(MyHook2[I]).TRI := CacheRegister;
              tiCOPYFS:
                TThoriumInstructionCOPYFS(MyHook2[I]).TRI := CacheRegister;
              tiMOVEST:
                TThoriumInstructionMOVEST(MyHook2[I]).TRI := CacheRegister;
            else
              // It was not possible to change it accordingly...
              Static2 := False;
            end;
            if Static2 then
              AppendCode(MyHook2);
          end;
        end
        else if MyHook2[0].Instruction = tiCOPYR then
        begin
          // This one is a register variable - fast access, no need to cache
          if MyHook1[0].Instruction <> tiCOPYR then
          begin
            // mkay... now, find out where the TRI value of the last instruction
            // lies...
            I := Length(MyHook1)-1;
            while MyHook1[I].Instruction = tiCLR do
              Dec(I);
            Static1 := True;
            case MyHook1[I].Instruction of
              tiADDF, tiADDI, tiADDS, tiSUBF, tiSUBI, tiMULF, tiMULI, tiDIVF,
              tiDIVI, tiMOD, tiAND, tiOR, tiXOR, tiSHL, tiSHR:
                TThoriumInstructionADDI(MyHook1[I]).TRI := CacheRegister;
              tiINT, tiFLT, tiEXT:
                TThoriumInstructionINT(MyHook1[I]).TRI := CacheRegister;
              tiSTR:
                TThoriumInstructionSTR(MyHook1[I]).TRI := CacheRegister;
              tiSTRL:
                TThoriumInstructionSTRL(MyHook1[I]).TRI := CacheRegister;
              tiCOPYS:
                TThoriumInstructionCOPYS(MyHook1[I]).TRI := CacheRegister;
              tiCOPYFS:
                TThoriumInstructionCOPYFS(MyHook1[I]).TRI := CacheRegister;
              tiMOVEST:
                TThoriumInstructionMOVEST(MyHook1[I]).TRI := CacheRegister;
            else
              // It was not possible to change it accordingly...
              Static1 := False;
            end;
            if Static1 then
              AppendCode(MyHook1);
          end;
        end;
      end;
      if CompareInstruction <> nil then
        CompareInstruction^ := FInstructions.Position;

      if Static1 then
      begin
        AppendCode(MyHook2);
        GenCode(_cmpOperator(CacheRegister, RegID2, Type1, Type2));
      end
      else if Static2 then
      begin
        AppendCode(MyHook1);
        GenCode(_cmpOperator(RegID1, CacheRegister, Type1, Type2))
      end
      else
      begin
        AppendCode(MyHook1);
        AppendCode(MyHook2);
        GenCode(_cmpOperator(RegID1, RegID2, Type1, Type2));
      end;
      if not Static1 and ThoriumTypeNeedsClear(Type1) then
        GenCode(clr(RegID1));
      if not Static2 and ThoriumTypeNeedsClear(Type2) then
        GenCode(clr(RegID2));
      if HasError then
        Exit;
      Op := ThoriumSymbolToOperator(Sym);
      if TrueAddress = THORIUM_JMP_INVALID then
      begin
        // No true jump is expected, so we reverse the whole condition and
        // perform those jumps to the false address.
        if FalseAddress = THORIUM_JMP_INVALID then
        begin
          // Yeah, someone did a joke to us -.-. No jumps wanted... Just the
          // expression parsed... Weird, but ok. We will send a note output.
          raise EThoriumCompilerException.Create('Compiler Debug Exception: Compiler called ExpressionConditionalJump without any target jump address. It is recommended to change this to ExpressionComparing.');
        end;
        // Write the FalseJmp position
        if FalseJmp <> nil then
          FalseJmp^ := FInstructions.Position;
        // Generate the code for the inverted operation.
        case Op of
          tpNotEqual: GenCode(je(FalseAddress));
          tpGreater: GenCode(jle(FalseAddress));
          tpGreaterEqual: GenCode(jlt(FalseAddress));
          tpEqual: GenCode(jne(FalseAddress));
          tpLessEqual: GenCode(jgt(FalseAddress));
          tpLess: GenCode(jge(FalseAddress));
        end;
      end
      else
      begin
        // Generate code for the different comparsions
        if TrueJmp <> nil then
          TrueJmp^ := FInstructions.Position;
        case Op of
          tpNotEqual: GenCode(jne(TrueAddress));
          tpGreater: GenCode(jgt(TrueAddress));
          tpGreaterEqual: GenCode(jge(TrueAddress));
          tpEqual: GenCode(je(TrueAddress));
          tpLessEqual: GenCode(jle(TrueAddress));
          tpLess: GenCode(jlt(TrueAddress));
        end;
        // Genereate code for "false"-jump
        if (FalseAddress <> THORIUM_JMP_INVALID) then
        begin
          if FalseJmp <> nil then
            FalseJmp^ := FInstructions.Position;
          GenCode(jmp(FalseAddress));
        end;
      end;
      // Release the temporary registers
      ReleaseRegister(RegID1);
      ReleaseRegister(RegID2);
    end;

    function ExpressionComparing(NeedStatic: Boolean = False): TThoriumOperator; inline;
    // This function parses a given expression.

    var
      Sym: TThoriumSymbol;
      Op: TThoriumOperation;
      PrevType, Dummy: TThoriumType;
      OperandType: TThoriumType;
      RegID1, RegID2: TThoriumRegisterID;
    begin
      // (Expression)
      // First we check if it can be an expression at all by checking if the
      // given symbol can be the initiator of a factor.
      if not ExpectSymbol(THORIUM_FACTOR_INITIATOR) then
        Exit;
      // Get a free register for the first operand
      if not GetFreeRegister(trEXP, RegID1) then
        Exit;
      // Then we get the first simple expression part.
      PrevType := SimpleExpression(RegID1, NeedStatic);
      if HasError then
      begin
        ReleaseRegister(RegID1);
        Exit;
      end;

      if not ExpectSymbol(THORIUM_RELATIONAL_OPERATOR, False) then
      begin
        if (PrevType.ValueType = vtBuiltIn) and (Prevtype.BuiltInType = btInteger) then
        begin
          if not GetFreeRegister(trEXP, RegID2, True) then
          begin
            ReleaseRegister(RegID1);
            Exit;
          end;
          GenCode(int(0, RegID2));
          Result := tpNotEqual;
          GenCode(_cmpOperator(RegID1, RegID2, PrevType, PrevType));
          ReleaseRegister(RegID1);
          ReleaseRegister(RegID2);
          Exit;
        end
        else
        begin
          CompilerError('Incompatible types: Expected boolean/int, got '+ThoriumTypeName(PrevType));
          Exit;
        end;
        ReleaseRegister(RegID1);
        Exit;
      end
      else
      begin
        // Get the register for the second operand
        if not GetFreeRegister(trEXP, RegID2, True) then
        begin
          ReleaseRegister(RegID1);
          Exit;
        end;
        Sym := CurrentSym;
        Op := ThoriumSymbolToOperation(Sym);
        Proceed;
        OperandType := SimpleExpression(RegID2, NeedStatic);
        if HasError then
          Exit;
        // Check if the operation is available
        if not NeedTypeOperation(PrevType, OperandType, Op, Dummy) then
          Exit;

        // Perform comparsion
        GenCode(_cmpOperator(RegID1, RegID2, PrevType, OperandType));
        if ThoriumTypeNeedsClear(PrevType) then
          GenCode(clr(RegID1));
        if ThoriumTypeNeedsClear(OperandType) then
          GenCode(clr(RegID2));
        // Release the temporary registers
        ReleaseRegister(RegID1);
        ReleaseRegister(RegID2);
        Result := ThoriumSymbolToOperator(Sym);
      end;
    end;

    function ExpressionConditionalJump(TrueAddress: TThoriumInstructionAddress; FalseAddress: TThoriumInstructionAddress; ForceBrackets: Boolean = False; TrueJmp: PCardinal = nil; FalseJmp: PCardinal = nil): Boolean;
    var
      Op: TThoriumOperator;
    begin
      Result := False;
      if ForceBrackets then
      begin
        if (not ExpectSymbol([tsOpenBracket])) then
          Exit;
        Proceed;
      end;
      // Evaluate a comparing expression
      Op := ExpressionComparing;
      if HasError then
        Exit;
      if TrueAddress = THORIUM_JMP_INVALID then
      begin
        // No true jump is expected, so we reverse the whole condition and
        // perform those jumps to the false address.
        if FalseAddress = THORIUM_JMP_INVALID then
        begin
          // Yeah, someone did a joke to us -.-. No jumps wanted... Just the
          // expression parsed... Weird, but ok. We will send a note output.
          raise EThoriumCompilerException.Create('Compiler Debug Exception: Compiler called ExpressionConditionalJump without any target jump address. It is recommended to change this to ExpressionComparing.');
          if ForceBrackets then
          begin
            if (not ExpectSymbol([tsCloseBracket])) then
              Exit;
            Proceed;
          end;
          Result := True;
          Exit;
        end;
        // Write the FalseJmp position
        if FalseJmp <> nil then
          FalseJmp^ := FInstructions.Position;
        // Generate the code for the inverted operation.
        case Op of
          tpNotEqual: GenCode(je(FalseAddress));
          tpGreater: GenCode(jle(FalseAddress));
          tpGreaterEqual: GenCode(jlt(FalseAddress));
          tpEqual: GenCode(jne(FalseAddress));
          tpLessEqual: GenCode(jgt(FalseAddress));
          tpLess: GenCode(jge(FalseAddress));
        end;
      end
      else
      begin
        // Generate code for the different comparsions
        if TrueJmp <> nil then
          TrueJmp^ := FInstructions.Position;
        case Op of
          tpNotEqual: GenCode(jne(TrueAddress));
          tpGreater: GenCode(jgt(TrueAddress));
          tpGreaterEqual: GenCode(jge(TrueAddress));
          tpEqual: GenCode(je(TrueAddress));
          tpLessEqual: GenCode(jle(TrueAddress));
          tpLess: GenCode(jlt(TrueAddress));
        end;
        // Genereate code for "false"-jump
        if (FalseAddress <> THORIUM_JMP_INVALID) then
        begin
          if FalseJmp <> nil then
            FalseJmp^ := FInstructions.Position;
          GenCode(jmp(FalseAddress));
        end;
      end;
      if ForceBrackets then
      begin
        if (not ExpectSymbol([tsCloseBracket])) then
          Exit;
        Proceed;
      end;
      Result := True;
    end;

    procedure Statement(var Offset: Integer; AllowedStatements: TThoriumStatementKinds = THORIUM_ALL_STATEMENTS; ExpectSemicolon: Boolean = True);
    // This function compiles all statements which can occur.
    var
      ExpressionType: TThoriumType;

      (*function ConditionalExpression(TargetRegister: TThoriumRegisterID): Boolean; inline;
      // Just a helper function which checks for a expression in brackets
      // and treats it like a condition. This means, if the result on the
      // stack is not an integer, we convert it using the OP.C-instruction.
      var
        ExpressionType: TThoriumType;
      begin
        Result := False;
        if not ExpectSymbol([tsOpenBracket]) then
          Exit;
        Proceed;
        ExpressionType := Expression(TargetRegister);
        if HasError then Exit;
        if not ExpectSymbol([tsCloseBracket]) then
          Exit;
        Proceed;
        Result := True;
      end;*)

      procedure IfStatement;
      // Compiles an if statement with any elseif or else constructs.
      var
        LastJump: TThoriumInstructionAddress;
        JumpSize: Integer;
      begin
        JumpSize := Jumps.Count;
        Proceed;
        // Evaluate the condition and save the position of the to-the-end jump
        if not ExpressionConditionalJump(THORIUM_JMP_INVALID, 0, True, nil, @LastJump) then
          Exit;
        // Evaluate the statments
        Statement(Offset);
        if HasError then Exit;
        // And add the jump-to-end JMP
        Jumps.AddEntry(GenCode(jmp(0)));

        while CurrentSym = tsElseIf do
        begin
          // Fill in the last JMP.F-instruction with the position of the next
          // instruction
          TThoriumInstructionJMP(FInstructions.Instruction[LastJump]^).NewAddress := FInstructions.Position;

          Proceed;
          // Evaluate the condition and save the position of the to-the-end jump
          if not ExpressionConditionalJump(THORIUM_JMP_INVALID, 0, True, nil, @LastJump) then
            Exit;
          // And evaluate the statements
          Statement(Offset);
          if HasError then Exit;
          // Now add the jump-to-end JMP
          Jumps.AddEntry(GenCode(jmp(0)));
        end;

        // Set the last jump position to the next instruction.
        TThoriumInstructionJMP(FInstructions.Instruction[LastJump]^).NewAddress := FInstructions.Position;

        // If there is a else-branch...
        if CurrentSym = tsElse then
        begin
          Proceed;
          // ... generate the code for the statement in there.
          Statement(Offset);
          if HasError then Exit;
        end;

        Jumps.FillAddresses(JumpSize, FInstructions.Count, FInstructions);
      end;

      procedure WhileStatement(Reverse: Boolean);
      // Compiles a while statement which can also be reversed (do..while).
      var
        JumpPos, Backref: TThoriumInstructionAddress;
        BreakCount: Integer;
      begin
        Proceed;
        // Save the position of the next instruction. It does not matter, if we
        // have a reversed while-loop or a normal one. The position will be
        // needed later.
        if not Reverse then
        begin
          Backref := FInstructions.Position;
          if not ExpressionConditionalJump(THORIUM_JMP_INVALID, 0, True, nil, @JumpPos) then
            Exit;
        end
        else
          JumpPos := FInstructions.Position;

        BreakCount := BreakJumps.Count;
        // Generate code for the statements.
        SaveTable;
        Statement(Offset);
        RestoreTable(Offset);

        if not Reverse then
        begin
          // For the normal version, we set the jXX-destination to the
          // following instruction
          GenCode(jmp(Backref));
          TThoriumInstructionJMP(FInstructions.Instruction[JumpPos]^).NewAddress := FInstructions.Position;
        end
        else
        begin
          if not ExpectSymbol([tsWhile]) then
            Exit;
          Proceed;
          // Evaluate the expression and jump back to the beginning if the test
          // returns true.
          if not ExpressionConditionalJump(JumpPos, THORIUM_JMP_INVALID, True, nil, nil) then
            Exit;
        end;
        BreakJumps.FillAddresses(BreakCount, FInstructions.Position, FInstructions);
      end;

      procedure ForStatement;
      // Compiles a for statement.
      var
        StartPos, EndJump: TThoriumInstructionAddress;
        OldHook: Boolean;
        OldHook1, OldHook2: PThoriumInstructionArray;
        MyHook1: TThoriumInstructionArray;
        TypeIdent, Ident: TThoriumQualifiedIdentifier;
        Dummy: Integer;
        RegID, CacheReg: TThoriumRegisterID;
        Static: Boolean;
        NeedClear: Boolean;
      begin
        Dummy := 0;
        Proceed;
        // The opening bracket of the For clause.
        if not ExpectSymbol([tsOpenBracket]) then
          Exit;
        Proceed;
        SaveTable;
        if not ExpectSymbol([tsIdentifier]) then
          Exit;
        // Check if we find a type...
        if not QualifyIdentifier(TypeIdent, [ikType], 0) then
          Exit;
        // And a name after that
        if not QualifyIdentifier(Ident, [ikUndeclared, ikNoFar], 0) then
          Exit;
        if not GetFreeRegister(trEXP, RegID) then
          Exit;
        VariableDeclaration(TypeIdent, Ident, vsPrivate, Dummy, RegID);
        if HasError then
          Exit;
        //Statement(Offset, [tskAssignment, tskDeclaration]);
        // Save the position of the comparsion code.
        // Generate code for the comparsion and save the position of the false
        // jump.
        if not GetFreeRegister(trEXP, CacheReg) then
        begin
          NeedClear := False;
          StartPos := FInstructions.Position;
          if not ExpressionConditionalJump(THORIUM_JMP_INVALID, 0, False, nil, @EndJump) then
            Exit;
          CacheReg := THORIUM_REGISTER_INVALID;
        end
        else
        begin
          NeedClear := ThoriumTypeNeedsClear(ConditionalExpressionRegisterCaching(THORIUM_JMP_INVALID, 0, CacheReg, @StartPos, @Static, nil, @EndJump));
        end;
        //if not ExpressionConditionalJump(THORIUM_JMP_INVALID, 0, False, nil, @EndJump) then
        //  Exit;
        // We need a extra semicolon because an expression does not have a
        // own semicolon.
        if not ExpectSymbol([tsSemicolon]) then
          Exit;
        Proceed;
        // Save the position where the code for the end of one loop lies.
        OldHook := CodeHook;
        OldHook1 := CodeHook1;
        OldHook2 := CodeHook2;
        try
          CodeHook := True;
          CodeHook1 := @MyHook1;
          CodeHook2 := nil;
          Statement(Offset, [tskAssignment], False);
        finally
          CodeHook := OldHook;
          CodeHook1 := OldHook1;
          CodeHook2 := OldHook2;
        end;
        // And now the closing bracket.
        if not ExpectSymbol([tsCloseBracket]) then
          Exit;
        Proceed;
        // Parse the statement in the loop
        Statement(Offset);
        // Insert now the code which will be executed at the end of the loop
        AppendCode(MyHook1);
        // After the end-of-loop-code jump to the beginning again.
        GenCode(jmp(StartPos));
        // Enter the address of the code behind the for-loop.
        TThoriumInstructionJMP(FInstructions.Instruction[EndJump]^).NewAddress := FInstructions.Position;

        if CacheReg <> THORIUM_REGISTER_INVALID then
        begin
          if NeedClear then
            GenCode(clr(CacheReg));
          ReleaseRegister(CacheReg);
        end;
        ReleaseRegister(RegID);
        RestoreTable(Offset);
      end;

      procedure SwitchStatement;
      // Compiles a switch statement.
      var
        SwitchPos: TThoriumInstructionAddress;
        JMPTPos: TThoriumInstructionAddress;
        SwitchExpressionType, CaseExpressionType: TThoriumType;
        HasDefault: Boolean;
        DefaultPos: TThoriumInstructionAddress;
        BreakCount: Integer;
        ExpressionRegister, ValueRegister, ResultRegister: TThoriumRegisterID;
      begin
        BreakCount := BreakJumps.Count;
        Proceed;
        if not ExpectSymbol([tsOpenBracket]) then
          Exit;
        Proceed;
        // Allocate the register in which the result of the expression
        // will be stored and another one for the result of the comparsion.
        if (not GetFreeRegister(trEXP, ExpressionRegister)) or
          (not GetFreeRegister(trEXP, ResultRegister)) or
          (not GetFreeRegister(trEXP, ValueRegister)) then
          Exit;
        // Get the expression in the brackets of the switch and store it's type
        // in a variable
        SwitchExpressionType := SimpleExpression(ExpressionRegister);
        if HasError then Exit;
        // Save the current instruction position to write the jump instructions
        // there later
        SwitchPos := FInstructions.Position;
        if not ExpectSymbol([tsCloseBracket]) then
          Exit;
        Proceed;
        if not ExpectSymbol([tsOpenCurlyBracket]) then
          Exit;
        Proceed;
        // By default, there is no default directive.
        HasDefault := False;
        while CurrentSym in [tsCase, tsDefault] do
        begin
          if CurrentSym = tsDefault then
          begin
            if HasDefault then
            begin
              CompilerError('Default statement already defined.');
              Exit;
            end;
            Proceed;
            if not ExpectSymbol([tsColon]) then
              Exit;
            // Set the default flag and set the position of the default block.
            HasDefault := True;
            DefaultPos := FInstructions.Position;
            // Register the default address in the instructions to make it
            // change automatically when something is added before the default
            // block
            FInstructions.AddInstructionPointer(@DefaultPos);
            Proceed;
            // Get the instructions for the statement in the default block.
            SaveTable;
            Statement(Offset);
            RestoreTable(Offset);
          end
          else
          begin
            Proceed;
            // Go back to the switch-instructions
            FInstructions.Position := SwitchPos;
            // Evaluate the expression
            CaseExpressionType := Expression(ValueRegister);
            if HasError then Exit;
            // Check if a comparsion between the case expression and the switch
            // expression is possible.
            if not NeedTypeOperation(SwitchExpressionType, CaseExpressionType, toCompare, CaseExpressionType) then
              Exit;
            // Write the comparsion code
            GenCode(_cmpOperator(ExpressionRegister, ValueRegister, SwitchExpressionType, CaseExpressionType));
            // Write the jump instruction and save it's position. Let it remove
            // the top element on the stack.
            JMPTPos := GenCode(je(0));
            // Set the switch pos to the end of this switch instruction block
            SwitchPos := FInstructions.Position;
            // Go back to the end of the instructions
            FInstructions.Position := FInstructions.Count;
            // Set the jump address of the current switch case check to this
            // position
            TThoriumInstructionJMP(FInstructions.Instruction[JMPTPos]^).NewAddress := FInstructions.Position;
            // And finally generate the code for the case statement.
            if not ExpectSymbol([tsColon]) then
              Exit;
            Proceed;
            SaveTable;
            Statement(Offset);
            RestoreTable(Offset);
          end;
        end;
        // If we have a default...
        if HasDefault then
        begin
          // ... add a final jump instruction to the switch block
          FInstructions.Position := SwitchPos;
          GenCode(JMP(DefaultPos));
          FInstructions.Position := FInstructions.Count;
          FInstructions.RemoveInstructionPointer(@DefaultPos);
        end;
        if not ExpectSymbol([tsCloseCurlyBracket]) then
          Exit;
        BreakJumps.FillAddresses(BreakCount, FInstructions.Position, FInstructions);
        if ThoriumTypeNeedsClear(ExpressionType) then
          GenCode(clr(ExpressionRegister));
        ReleaseRegister(ExpressionRegister);
        ReleaseRegister(ResultRegister);
        ReleaseRegister(ValueRegister);
        // In the end we pop the result of the expression of the switch block
        // from the stack.
        Proceed;
      end;

    var
      Identifier1: String;
      TableSize: Integer;
      Entry1: TThoriumTableEntry;
      Module1: Integer;
      IdentInfo1, IdentInfo2: TThoriumQualifiedIdentifier;
      Dummy: TThoriumType;
      Sym: TThoriumSymbol;
      NeedSemicolon: Boolean;
      RegID1: TThoriumRegisterID;
    begin
      NeedSemicolon := False;
      case CurrentSym of
        tsOpenCurlyBracket:
        begin
          if not (tskBlock in AllowedStatements) then
          begin
            CompilerError('Blocks are not allowed here.');
            Exit;
          end;
          Proceed;

          // Early-out if we have a closing bracket immediately.
          if CurrentSym = tsCloseCurlyBracket then
          begin
            Proceed;
            Exit;
          end;

          SaveTable;
          // Parse the first statement...
          Statement(Offset);
          while CurrentSym <> tsCloseCurlyBracket do
          begin
            // Leave if there is an error
            if HasError then Exit;
            // Another statement
            Statement(Offset);
          end;
          RestoreTable(Offset);

          if not ExpectSymbol([tsCloseCurlyBracket]) then
            Exit;
          Proceed;
        end;
        tsIf:
        begin
          if not (tskIf in AllowedStatements) then
          begin
            CompilerError('If statements are not allowed here.');
            Exit;
          end;
          IfStatement;
        end;
        tsWhile:
        begin
          if not (tskWhile in AllowedStatements) then
          begin
            CompilerError('While statements are not allowed here.');
            Exit;
          end;
          WhileStatement(False);
        end;
        tsDo:
        begin
          if not (tskDoWhile in AllowedStatements) then
          begin
            CompilerError('DoWhile statements are not allowed here.');
            Exit;
          end;
          WhileStatement(True);
        end;
        tsSwitch:
        begin
          if not (tskSwitch in AllowedStatements) then
          begin
            CompilerError('Switch statements are not allowed here.');
            Exit;
          end;
          SwitchStatement;
        end;
        tsFor:
        begin
          if not (tskFor in AllowedStatements) then
          begin
            CompilerError('If statements are not allowed here.');
            Exit;
          end;
          ForStatement;
        end;
        tsBreak:
        begin
          if not (tskBreak in AllowedStatements) then
          begin
            CompilerError('Break statements are not allowed here.');
            Exit;
          end;
          Proceed;
          if not ExpectSymbol([tsSemicolon]) then
            Exit;
          Proceed;
          BreakJumps.AddEntry(GenCode(JMP(0)));
        end;
        tsPlusPlus, tsMinusMinus:
        begin
          NeedSemicolon := True;
          if not (tskAssignment in AllowedStatements) then
          begin
            CompilerError('Assignments are not allowed here.');
            Exit;
          end;
          Sym := CurrentSym;
          Proceed;
          if not ExpectSymbol([tsIdentifier]) then
            Exit;
          Identifier1 := CurrentStr;
          Proceed;
          if not FindTableEntry(Identifier1, Entry1, Module1) then
            Exit;
          if Entry1._Type <> etVariable then
          begin
            CompilerError('Cannot assign to static variable or function.');
            Exit;
          end;
          if Entry1.TypeSpec.ValueType <> vtBuiltIn then
          begin
            CompilerError('Cannot apply ++/-- on non-builtin types.');
            Exit;
          end;
          if Module1 <> -1 then
          begin
            // Apply inc/dec depending on given type
            case Sym of
              tsPlusPlus:
              begin
                case Entry1.TypeSpec.BuiltInType of
                  btInteger: GenCode(inci_s(Entry1.Scope, Entry1.Offset));
                  btFloat: GenCode(incf_s(Entry1.Scope, Entry1.Offset));
                else
                  CompilerError('Cannot apply ++/-- operator on the given type.');
                  Exit;
                end;
              end;
              tsMinusMinus:
              begin
                case Entry1.TypeSpec.BuiltInType of
                  btInteger: GenCode(deci_s(Entry1.Scope, Entry1.Offset));
                  btFloat: GenCode(decf_s(Entry1.Scope, Entry1.Offset));
                else
                  CompilerError('Cannot apply ++/-- operator on the given type.');
                  Exit;
                end;
              end;
            end;
          end
          else
          begin
            case Sym of
              tsPlusPlus:
              begin
                case Entry1.TypeSpec.BuiltInType of
                  btInteger: GenCode(inci_fs(Module1, Entry1.Offset));
                  btFloat: GenCode(incf_fs(Module1, Entry1.Offset));
                else
                  CompilerError('Cannot apply ++/-- operator on the given type.');
                  Exit;
                end;
              end;
              tsMinusMinus:
              begin
                case Entry1.TypeSpec.BuiltInType of
                  btInteger: GenCode(deci_fs(Module1, Entry1.Offset));
                  btFloat: GenCode(decf_fs(Module1, Entry1.Offset));
                else
                  CompilerError('Cannot apply ++/-- operator on the given type.');
                  Exit;
                end;
              end;
            end;
          end;
        end;
        tsIdentifier:
        begin
          NeedSemicolon := True;
          // Get the first identifier
          if not GetFreeRegister(trEXP, RegID1, True) then
            Exit;
          if not QualifyIdentifier(IdentInfo1, [ikComplex, ikVariable, ikUndeclared, ikType, ikStatic], RegID1) then
            Exit;
          if HasError then Exit;
          AddHostTypeUsages(IdentInfo1.UsedExtendedTypes);
          AddLibraryPropertyUsages(IdentInfo1.UsedLibraryProps);
          // Check if the next symbol can be valid
          if not ExpectSymbol([tsAssign, tsIdentifier, tsAdditiveAssign, tsDivideAssign, tsSubtractiveAssign, tsMultiplicativeAssign, tsPlusPlus, tsMinusMinus, tsSemicolon]) then
            Exit;
          case CurrentSym of
            tsIdentifier:
            begin
              // Check for declarations
              if not (tskDeclaration in AllowedStatements) then
              begin
                CompilerError('Declarations are not allowed here.');
                Exit;
              end;
              // Check if the first identifier is a type
              if (IdentInfo1.Kind <> ikType) then
              begin
                CompilerError('Type identifier expected, but '+THORIUM_IDENTIFIER_KIND_NAMES[IdentInfo1.Kind]+' found.');
                Exit;
              end;

              // Get the next identififer
              if not QualifyIdentifier(IdentInfo2, [ikUndeclared, ikNoFar], 0) then
                Exit;
              // Parse the obvious variable declaration
              VariableDeclaration(IdentInfo1, IdentInfo2, vsPrivate, Offset);
              NeedSemicolon := False;
            end;
            tsAssign:
            begin
              // Check for assignments
              if not (tskAssignment in AllowedStatements) then
              begin
                CompilerError('Assignments are not allowed here.');
                Exit;
              end;
              // If the first identifier is static, we cannot proceed.
              if IdentInfo1.IsStatic then
              begin
                CompilerError('Cannot assign to left side.');
                Exit;
              end;
              Proceed;
              // Parse the expression
              ExpressionType := Expression(RegID1);
              if HasError then Exit;
              // Check if the assignment is available
              if not NeedTypeOperation(IdentInfo1.FinalType, ExpressionType, toAssign, Dummy) then
                Exit;
              if not ThoriumCompareTypeEx(ExpressionType, IdentInfo1.FinalType) then
                GenCode(_cast(RegID1, RegID1, ExpressionType, IdentInfo1.FinalType));
              // And append the code
              AppendCode(IdentInfo1.SetCode);
            end;
            tsPlusPlus, tsMinusMinus:
            begin
              // If we have a direct variable, we can operate on it
              // directly which will have a performance gain.
              if (IdentInfo1.FinalType.ValueType <> vtBuiltIn) then
              begin
                CompilerError('Cannot apply ++/-- operator on extended values or functions.');
                Exit;
              end
              else if (not IsTypeOperationAvailable(IdentInfo1.FinalType, toINC_DEC, Dummy)) then
              begin
                CompilerError('Cannot apply ++/-- operator on the given type.');
                Exit;
              end;
              // Direct access ability is forced, so we can access it directly.
              case IdentInfo1.GetCode[0].Instruction of
                tiCOPYS:
                begin
                  // Apply inc/dec depending on given type
                  case CurrentSym of
                    tsPlusPlus:
                    begin
                      case IdentInfo1.FinalType.BuiltInType of
                        btInteger: GenCode(inci_s(TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                        btFloat: GenCode(incf_s(TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                    tsMinusMinus:
                    begin
                      case IdentInfo1.FinalType.BuiltInType of
                        btInteger: GenCode(deci_s(TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                        btFloat: GenCode(decf_s(TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Scope, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                  end;
                end;
                tiCOPYFS:
                begin
                  case CurrentSym of
                    tsPlusPlus:
                    begin
                      case IdentInfo1.FinalType.BuiltInType of
                        btInteger: GenCode(inci_fs(TThoriumInstructionCOPYFS(IdentInfo1.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                        btFloat: GenCode(incf_fs(TThoriumInstructionCOPYFS(IdentInfo1.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                    tsMinusMinus:
                    begin
                      case IdentInfo1.FinalType.BuiltInType of
                        btInteger: GenCode(deci_fs(TThoriumInstructionCOPYFS(IdentInfo1.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                        btFloat: GenCode(decf_fs(TThoriumInstructionCOPYFS(IdentInfo1.GetCode[0]).ModuleIndex, TThoriumInstructionCOPYS(IdentInfo1.GetCode[0]).Offset));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                  end;
                end;
                tiCOPYR:
                begin
                  case CurrentSym of
                    tsPlusPlus:
                    begin
                      case IdentInfo1.FinalType.BuiltInType of
                        btInteger: GenCode(inci(TThoriumInstructionCOPYR(IdentInfo1.GetCode[0]).SRI));
                        btFloat: GenCode(incf(TThoriumInstructionCOPYR(IdentInfo1.GetCode[0]).SRI));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                    tsMinusMinus:
                    begin
                      case IdentInfo1.FinalType.BuiltInType of
                        btInteger: GenCode(deci(TThoriumInstructionCOPYR(IdentInfo1.GetCode[0]).SRI));
                        btFloat: GenCode(decf(TThoriumInstructionCOPYR(IdentInfo1.GetCode[0]).SRI));
                      else
                        CompilerError('Cannot apply ++/-- operator on the given type.');
                        Exit;
                      end;
                    end;
                  end;
                end;
              end;
              Proceed;
            end;
            tsSemicolon:
            begin
              AppendCode(IdentInfo1.GetCode);
              if not ((IdentInfo1.FinalType.ValueType = vtBuiltIn) and (IdentInfo1.FinalType.BuiltInType <= btNil)) then
              begin
                // Discard result of operation
                if ThoriumTypeNeedsClear(IdentInfo1.FinalType) then
                  GenCode(clr(RegID1));
              end;
            end;
          end;
          ReleaseRegister(RegID1);
        end;
        tsReturn:
        begin
          Proceed;
          NeedSemicolon := True;
          // Check if the current function can have a return value.
          if not ((CurrentReturnType.ValueType = vtBuiltIn) and (CurrentReturnType.BuiltInType <= btNil)) then
          begin
            TableSize := GetTableEntriesTo(CurrentFunctionTableStack);
            if not GetFreeRegister(trEXP, RegID1) then
              Exit;
            // and exit on error
            if HasError then Exit;
            // Parse the expression
            ExpressionType := Expression(RegID1);
            // Check if an assignment between the return value type and the
            // returning type of the expression is possible
            if not NeedTypeOperation(CurrentReturnType, ExpressionType, toAssign, ExpressionType) then
              Exit;
            // Important: Kill the stack entries *after* having parsed the
            // expression =)
            if TableSize > 0 then
              GenCode(pop_s(TableSize));
            // And finally write the return code and add it to the FuncReturns
            // list.
            GenCode(mover_st(RegID1));
            ReleaseRegister(RegID1);
            GenCode(ret());
          end
          else
          begin
            // Pop the remaining stack entries
            TableSize := GetTableEntriesTo(CurrentFunctionTableStack);
            if TableSize > 0 then
              GenCode(pop_s(TableSize));
            // Write a return instruction. Any overflow of symbols will be
            // handled by the semicolon check.
            GenCode(ret());
          end;
        end;
        tsSemicolon:
        begin
          NeedSemicolon := True;
        end;
      else
        ExpectSymbol([]);
      end;
      if NeedSemicolon and ExpectSemicolon then
      begin
        ExpectSymbol([tsSemicolon]);
        Proceed;
      end;
    end;

    procedure FunctionDeclaration(TypeIdent, Ident: TThoriumQualifiedIdentifier;
      VisibilityLevel: TThoriumVisibilityLevel; var Offset: Integer);
    // Handles a declaration of a function. This can either be a prototyped
    // declaration or a final one.

    type
      TParam = record
        ParamName: String;
        ParamType: TThoriumType;
      end;
      TParams = array of TParam;

    var
      IsImplementationOfPrototype: Boolean;

      function PrototypedFunction: TThoriumFunction; inline;
      // Handle the previously prototyped function.
      var
        ReturnType: TThoriumType;
      begin
        // First, set the corresponding flag.
        IsImplementationOfPrototype := True;
        // Get the function already declared
        Result := Ident.FinalType.Func;
        // Check if the function has any return values
        if Result.FReturnValues.Count = 0 then
          ReturnType := ThoriumNilTypeSpec
        else
          // If so, get the spec for it.
          Result.FReturnValues.GetParameterSpec(0, ReturnType);
        // Check if the type given is equal to the previously declared type.
        if not ThoriumCompareType(ReturnType, TypeIdent.FinalType) then
        begin
          CompilerError('Function declaration differs from prototype (result).');
          Exit;
        end;
        // Set the new entry point
        Result.FEntryPoint := FInstructions.Count;
        // Fill any adresses perhaps added
        Result.FPrototypedCalls.FillAddresses(0, Result.FEntryPoint, FInstructions);
        // Remove the prototyped flag.
        Result.FPrototyped := False;
        SaveTable;

      end;

    var
      Func: TThoriumFunction;
      GivenType: TThoriumType;
      ParamTypeIdent: TThoriumQualifiedIdentifier;
      ParamIdent: TThoriumQualifiedIdentifier;
      ParamIndex: Integer;
      LocalOffset: Integer;
      Params: TParams;
      I: Integer;
    begin
      if Ident.Kind = ikPrototypedFunction then
        Func := PrototypedFunction
      else
      begin
        IsImplementationOfPrototype := False;
        // Create a new function entry.
        Func := TThoriumFunction.Create(FModule);
        // Check if the result type is not nil and save it.
        CurrentReturnType := TypeIdent.FinalType;
        if not ((TypeIdent.FinalType.ValueType = vtBuiltIn) and (TypeIdent.FinalType.BuiltInType <= btNil)) then
          Func.FReturnValues.AddParameter^ := TypeIdent.FinalType;

        // Set the entry point...
        Func.FEntryPoint := FInstructions.Count;
        // ... and function name
        Func.FName := Ident.FullStr;
        // Create a new table entry
        Table.AddFunctionIdentifier(Ident.FullStr, Func);
      end;
      SaveTable;
      // Parameters are not global
      CurrentScope := THORIUM_STACK_SCOPE_PARAMETERS;
      //CurrentFunction := Func;
      Proceed;
      ParamIndex := 1;
      while CurrentSym in [tsIdentifier, tsComma] do
      begin
        // Check for the comma first
        if ParamIndex > 1 then
        begin
          if not ExpectSymbol([tsComma]) then
          begin
            CurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
            Exit;
          end;
          Proceed;
        end;
        // Get type...
        if not QualifyIdentifier(ParamTypeIdent, [ikType], 0) then
        begin
          CurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
          Exit;
        end;
        AddHostTypeUsages(ParamTypeIdent.UsedExtendedTypes);
        // ... and name
        if not QualifyIdentifier(ParamIdent, [ikUndeclared], 0) then
        begin
          CurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
          Exit;
        end;
        SetLength(Params, ParamIndex);
        // If we implement a prototype
        if IsImplementationOfPrototype then
        begin
          // Check if this parameter is given in the prototype
          if Func.FParameters.Count < ParamIndex then
          begin
            CompilerError('Function declaration differs from prototype (parameters, '+IntToStr(ParamIndex-1)+').');
            Exit;
          end;
          Func.FParameters.GetParameterSpec(ParamIndex-1, GivenType);
          if not ThoriumCompareType(GivenType, ParamTypeIdent.FinalType) then
          begin
            CompilerError('Function declaration differs from prototype (parameters, '+IntToStr(ParamIndex-1)+').');
            Exit;
          end;
        end
        else
        begin
          // Add the parameter to the function list..
          Func.FParameters.AddParameter^ := ParamTypeIdent.FinalType;
          // ... and to the cache parameter list.
          Params[ParamIndex-1].ParamName := ParamIdent.FullStr;
          Params[ParamIndex-1].ParamType := ParamTypeIdent.FinalType;
        end;
        // Increase the index
        Inc(ParamIndex);
      end;
      if not IsImplementationOfPrototype then
      begin
        // Now we have to add the table entries for the parameters to allow access
        for I := High(Params) downto Low(Params) do
          Table.AddConstantIdentifier(Params[I].ParamName, THORIUM_STACK_SCOPE_PARAMETERS, I-Length(Params), Params[I].ParamType, ThoriumCreateBuiltInValue(btUnknown));
      end;
      // Check for the closing bracket
      if not ExpectSymbol([tsCloseBracket]) then
      begin
        CurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
        Exit;
      end;
      Proceed;

      // Check if this is a prototype.
      if CurrentSym = tsSemicolon then
      begin
        if IsImplementationOfPrototype then
        begin
          CompilerError('Duplicate function: '''+Ident.FullStr+'''.');
          Exit;
        end;
        // Flag it as prototyped
        Func.FPrototyped := True;
        // And add it to the public functions if it is public
        if VisibilityLevel > vsPrivate then
          FPublicFunctions.Add(Func.Duplicate);
        LocalOffset := 0;
        RestoreTable(LocalOffset, False);
        Proceed;
        Exit;
      end;

      // Switch to the scope for local variables
      CurrentScope := THORIUM_STACK_SCOPE_LOCAL;
      // Save the return instruction count
      //ReturnCount := FuncReturns.Count;
      // Init the local offset
      SaveTable;
      CurrentFunctionTableStack := CurrentTableStackPos;
      LocalOffset := 0;
      // Parse the statement(s)
      Statement(LocalOffset);
      // Return to global mode
      CurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
      // Exit on error
      if HasError then
        Exit;
      // The global Offset MUST NOT be influenced and there MUST NOT be a
      // POP instruction.
      RestoreTable(LocalOffset, True);
      RestoreTable(LocalOffset, False);
      // Fill the parameter count in the given return instructions
      //FuncReturns.FillParamCount(ReturnCount, Func.FParameters.Count, FInstructions);
      // Add return code
      if Func.FReturnValues.Count > 0 then
        PushEmpty(CurrentReturnType);
      GenCode(ret());

      // If it's public, add it to the public list as a duplicate
      if not IsImplementationOfPrototype then
        if VisibilityLevel > vsPrivate then
          FPublicFunctions.Add(Func.Duplicate);
      //CurrentFunction := nil;
    end;

  var
    LastJump: Integer;
    HadDeclarations: Boolean;

    procedure GenericDeclaration(Static: Boolean;
      VisibilityLevel: TThoriumVisibilityLevel; var Offset: Integer);
    // Switch between the three kinds of declarations
    var
      TypeIdent, Ident: TThoriumQualifiedIdentifier;
      IsFunc: Boolean;
      Pos: Integer;
    begin
      if not QualifyIdentifier(TypeIdent, [ikType], 0) then
        Exit;
      AddHostTypeUsages(TypeIdent.UsedExtendedTypes);

      if not QualifyIdentifier(Ident, [ikUndeclared, ikPrototypedFunction], 0) then
        Exit;
      Pos := FInstructions.Count;
      IsFunc := False;
      if Static then
        ConstantDeclaration(TypeIdent, Ident, VisibilityLevel, Offset)
      else
      begin
        if CurrentSym = tsOpenBracket then
        begin
          IsFunc := True;
          if HadDeclarations then
          begin
            LastJump := GenCode(JMP(THORIUM_JMP_EXIT));
            HadDeclarations := False;
          end;
          FunctionDeclaration(TypeIdent, Ident, VisibilityLevel, Offset);
        end
        else
        begin
          VariableDeclaration(TypeIdent, Ident, VisibilityLevel, Offset);
        end;
      end;
      HadDeclarations := HadDeclarations or (not IsFunc);
      if (not IsFunc) and (LastJump <> -1) then
      begin
        TThoriumInstructionJMP(FInstructions.Instruction[LastJump]^).NewAddress := Pos;
        LastJump := -1;
      end;
    end;

    procedure Require(const Name: String);
    // Handle a require statement.
    var
      NewModule: TThoriumModule;
      I: Integer;
    begin
      if FThorium <> nil then
      begin
        NewModule := FThorium.FindModule(Name);
        if NewModule = nil then
        begin
          CompilerError('Could not load module '''+Name+'''.');
          Exit;
        end;
        if not NewModule.Compiled then
        begin
          CompilerError('Circular module require (or threading problem). Cannot import '''+Name+'''.');
          Exit;
        end;
        I := FThorium.FModules.IndexOf(NewModule);
        if I < 0 then
        begin
          CompilerError('Internal error: Loaded module not in list.');
          Exit;
        end;
        FRequiredModules.AddEntry(I);
      end
      else
      begin
        CompilerError('Could not load module '''+Name+'''.');
        Exit;
      end;
    end;

    procedure LoadLibrary(const Name: String);
    var
      Lib: TThoriumLibrary;
      I: Integer;
    begin
      if FThorium <> nil then
      begin
        Lib := FThorium.FindLibrary(Name);
        if Lib = nil then
        begin
          CompilerError('Could not load library '''+Name+'''.');
          Exit;
        end;
        I := FThorium.FHostLibraries.IndexOf(Lib);
        if I < 0 then
        begin
          CompilerError('Internal error: Loaded library not in list.');
          Exit;
        end;
        FRequiredLibraries.AddEntry(I);
      end
      else
      begin
        CompilerError('Could not load library '''+Name+'''.');
        Exit;
      end;
    end;

  var
    VisibilityLevel: TThoriumVisibilityLevel;
    Offset: Integer;
  begin
    // Set the initial scope
    CurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
    Offset := 0;
    // While we have a declaration, do this loop. When we exit it, it should be
    // either a compiler error or the end of file, otherwise a new compiler
    // error is created.
    LastJump := GenCode(JMP(THORIUM_JMP_EXIT));
    HadDeclarations := False;
    while (CurrentSym in [tsPublic, tsPrivate, tsStatic, tsIdentifier, tsLoadModule, tsLoadLibrary]) and (not HasError) do
    begin
      case CurrentSym of
        tsPublic, tsPrivate: // There is a visibility directive
        begin
          if CurrentSym = tsPublic then
            VisibilityLevel := vsPublic
          else
            VisibilityLevel := vsPrivate;
          Proceed;
          // Check if the following symbol is valid (this means, identifier or
          // static)
          if not ExpectSymbol([tsIdentifier, tsStatic]) then
            Exit;
          // If it's static ...
          if CurrentSym = tsStatic then
          begin
            Proceed;
            // ... proceed to a static generic declaration.
            GenericDeclaration(True, VisibilityLevel, Offset);
          end
          else
            // ... otherwise proceed to a non-static declaration.
            GenericDeclaration(False, VisibilityLevel, Offset);
        end;
        tsStatic:
        begin
          Proceed;
          // Proceed to a static declaration, which is by default private.
          GenericDeclaration(True, vsPrivate, Offset);
        end;
        tsIdentifier:
        begin
          // ooh, how sad. neither static, nor private, nor public, so we assume
          // a non-static, private declaration.
          GenericDeclaration(False, vsPrivate, Offset);
        end;
        tsLoadModule:
        begin
          // Okay, include another module. First, we have to get the module
          // name.
          Proceed;
          if not ExpectSymbol([tsStringValue]) then
            Exit;
          Require(CurrentStr);
          if HasError then
            Exit;
          Proceed;
          while CurrentSym in [tsComma] do
          begin
            Proceed;
            if not ExpectSymbol([tsStringValue]) then
              Exit;
            Require(CurrentStr);
            if HasError then
              Exit;
            Proceed;
          end;
          {if not ExpectSymbol([tsSemicolon]) then
            Exit;
          Proceed;   }
        end;
        tsLoadLibrary:
        begin
          // Use a host library. Get the library name.
          Proceed;
          if not ExpectSymbol([tsStringValue]) then
            Exit;
          LoadLibrary(CurrentStr);
          if HasError then
            Exit;
          Proceed;
          while CurrentSym in [tsComma] do
          begin
            Proceed;
            if not ExpectSymbol([tsStringValue]) then
              Exit;
            LoadLibrary(CurrentStr);
            if HasError then
              Exit;
            Proceed;
          end;
          {if not ExpectSymbol([tsSemicolon]) then
            Exit;
          Proceed;   }
        end;
      end;
    end;
    if (CurrentSym = tsError) then
      CompilerError('Parser error: '+CurrentStr)
    else if (CurrentSym <> tsNone) and (not HasError) then
      ExpectSymbol([tsNone]);
  end;

begin
  // Clear all previous data
  if not (cfAppend in Flags) then
    FModule.ClearAll;
  Result := True;
  // Initialize the scanner
  Scanner := TThoriumScanner.Create(SourceStream);
  try
    FSourceHash := Scanner.FInputHash;
    FSourceLength := Scanner.FInputSize;
    CodeHook := False;
    CodeHook1 := nil;
    CodeHook2 := nil;
    FillByte(RegisterUsage, THORIUM_REGISTER_MASK_SIZE * SizeOf(TThoriumRegisterMaskBlock), 0);
    // Initialize the table and table stack, and also the break jump list.
    Table := TThoriumIdentifierTable.Create;
    BreakJumps := TThoriumJumpList.Create;
    Jumps := TThoriumJumpList.Create;
    TableSizes := TThoriumIntStack.Create;
    //FuncReturns := TThoriumReturnList.Create;
    FInstructions.RegisterAddressList(BreakJumps);
    FInstructions.RegisterAddressList(Jumps);
    //FInstructions.RegisterAddressList(FuncReturns);
    try
      // Select the first symbol
      Proceed;
      // Parse the module
      Module;
    finally
      // Free the table, the table stack and the break jump list.
      if Result then
      begin
        FInstructions.Finish;
        if cfOptimize in Flags then
          OptimizeCode;
        FInstructions.Finish;
        FindRelocationTargets;
      end;
      //FInstructions.UnRegisterAddressList(FuncReturns);
      FInstructions.UnRegisterAddressList(BreakJumps);
      FInstructions.UnRegisterAddressList(Jumps);
      //FuncReturns.Free;
      TableSizes.Free;
      Jumps.Free;
      BreakJumps.Free;
      Table.Free;
    end;
  finally
    Scanner.Free;
    Result := not HasError;
    // If the compilation failed, clear all.
    if (not Result) and not (cfAppend in Flags) then
      FModule.ClearAll;
  end;
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
  FPublicFunctions := TFPList.Create;
  FPublicVariables := TFPList.Create;
  FRequiredModules := TThoriumIntList.Create;
  FRequiredLibraries := TThoriumIntList.Create;
  FSourceFile := '';
  FillByte(FSourceHash, SizeOf(TThoriumHash), 0);
  FSourceLength := 0;
  FStringLibrary := TStringList.Create;
  FThorium := AThorium;
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

function TThoriumModule.AddPublicVariable: TThoriumVariable;
begin
  Result := TThoriumVariable.Create(Self);
  FPublicVariables.Add(Result);
end;

procedure TThoriumModule.ClearAll;
// Clears all relevant informations.
var
  I: Integer;
begin
  FSourceFile := '';
  FInstructions.ClearCode;
  FStringLibrary.Clear;
  FPublicVariables.Clear;
  for I := 0 to FPublicFunctions.Count - 1 do
    TThoriumFunction(FPublicFunctions[I]).Free;
  FPublicFunctions.Clear;
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

function TThoriumModule.FindHostFunction(const AName: String
  ): TThoriumHostFunctionBase;
var
  I: Integer;
begin
  Result := nil;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FThorium.FHostLibraries[FRequiredLibraries[I]]).FindHostFunction(AName);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumModule.FindHostObjectType(const AName: String
  ): TThoriumHostObjectType;
var
  I: Integer;
begin
  Result := nil;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FThorium.FHostLibraries[FRequiredLibraries[I]]).FindHostType(AName);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumModule.FindHostRTTIType(const AName: String
  ): TThoriumRTTIObjectType;
var
  I: Integer;
begin
  Result := nil;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FThorium.FHostLibraries[FRequiredLibraries[I]]).FindRTTIType(AName);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumModule.FindLibraryConstant(const AName: String
  ): TThoriumLibraryConstant;
var
  I: Integer;
begin
  Result := nil;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FThorium.FHostLibraries[FRequiredLibraries[I]]).FindConstant(AName);
    if Result <> nil then
      Exit;
  end;
end;

function TThoriumModule.FindLibraryProperty(const AName: String
  ): TThoriumLibraryProperty;
var
  I: Integer;
begin
  Result := nil;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    Result := TThoriumLibrary(FThorium.FHostLibraries[FRequiredLibraries[I]]).FindProperty(AName);
    if Result <> nil then
      Exit;
  end;
end;

procedure TThoriumModule.InternalLoadFromStream(Stream: TStream;
  const Header: TThoriumModuleHeader);
var
  I, J: Integer;
  Module: TThoriumModule;
  Lib: TThoriumLibrary;
  ObjName: String;
  CurrHash: TThoriumHash;
  ObjHash: TThoriumHash;
  ExtendedType: TThoriumHostObjectType;
  ExternalFunc: TThoriumHostFunctionBase;
  ExternalMethod: TThoriumHostMethodBase;
  Prop: TThoriumLibraryProperty;
  Mode: Byte;
  Reloc: PThoriumRelocation;
  ExportedValue: TThoriumPublicValue;
  Base: ptruint;
  TypeSpec: PThoriumType;
begin
  FHash := Header.Hash;
  FSourceHash := Header.SourceHash;
  FSourceLength := Header.SourceLength;
  if Header.SourceFileNameLength > 0 then
  begin
    SetLength(FSourceFile, Header.SourceFileNameLength);
    Stream.Read(FSourceFile[1], Header.SourceFileNameLength);
  end
  else
    FSourceFile := '';
  for I := 0 to Header.RequireCount - 1 do
  begin
    ObjName := Stream.ReadAnsiString;
    Stream.Read(CurrHash, SizeOf(TThoriumHash));
    Module := FThorium.FindModule(ObjName, False);
    if Module = nil then
    begin
      Module := FThorium.DoRequireModule(ObjName, @CurrHash);
      if Module = nil then
      begin
        ClearAll;
        raise EThoriumDependencyException.Create('Could not load module '''+ObjName+''' which is required by module '''+Name+'''.');
      end;
    end
    else
    begin
      ObjHash := Module.GetHash;
      if not CompareMem(@ObjHash, @CurrHash, SizeOf(TThoriumHash)) then
      begin
        ClearAll;
        raise EThoriumHashException.Create('Module '''+Name+''' was compiled with another version of '''+ObjName+'''.');
      end;
    end;
    FRequiredModules.AddEntry(FThorium.FModules.IndexOf(Module));
  end;
  for I := 0 to Header.LibraryCount - 1 do
  begin
    ObjName := Stream.ReadAnsiString;
    Lib := FThorium.FindLibrary(ObjName);
    if Lib = nil then
    begin
      ClearAll;
      raise EThoriumDependencyException.CreateFmt('Could not find host library ''%s'' which is required by module ''%s''.', [ObjName, Name]);
    end;
    FRequiredLibraries.AddEntry(FThorium.FHostLibraries.IndexOf(Lib));
  end;
  for I := 0 to Header.HostTypeDependencyCount - 1 do
  begin
    Stream.Read(Mode, 1);
    ObjName := Stream.ReadAnsiString;
    Stream.Read(CurrHash, SizeOf(TThoriumHash));
    case Mode of
      0: // Generic extended type
      begin
        // ExtendedType := FThorium.FExtendedTypeRegistry.FindExtendedType(ObjName);
        ExtendedType := FindHostObjectType(ObjName);
        if ExtendedType = nil then
        begin
          ClearAll;
          raise EThoriumDependencyException.Create('Extended type '''+ObjName+''' needed by module '''+Name+''' is currently not registered.');
        end;
        ObjHash := ExtendedType.GetHash;
        if not CompareMem(@ObjHash, @CurrHash, SizeOf(TThoriumHash)) then
        begin
          ClearAll;
          raise EThoriumHashException.Create('Hash mismatch for RTTI type for class '''+ObjName+''' when loading module '''+Name+'''.');
        end;
      end;
      1: // RTTI extended type
      begin
        //ExtendedType := FThorium.FExtendedTypeRegistry.FindRTTIType(ObjName);
        ExtendedType := FindHostRTTIType(ObjName);
        if ExtendedType = nil then
        begin
          ClearAll;
          raise EThoriumDependencyException.Create('RTTI type for class '''+ObjName+''' needed by module '''+Name+''' is currently not registered.');
        end;
        ObjHash := ExtendedType.GetHash;
        if not CompareMem(@ObjHash, @CurrHash, SizeOf(TThoriumHash)) then
        begin
          ClearAll;
          raise EThoriumHashException.Create('Hash mismatch for RTTI type for class '''+ObjName+''' when loading module '''+Name+'''.');
        end;
      end;
    else
      raise EThoriumVerificationException.Create('Corrupted file.');
    end;
    FHostTypeUsage.Add(ExtendedType);
  end;
  try
    for I := 0 to Header.HostTypeRelocationCount - 1 do
    begin
      Reloc := GetMem(SizeOf(TThoriumRelocation));
      Stream.Read(Reloc^, SizeOf(TThoriumRelocation));
      FHostTypeRelocations.Add(Reloc);
    end;
    for I := 0 to Header.HostFuncDependencyCount - 1 do
    begin
      ObjName := Stream.ReadAnsiString;
      Stream.Read(Mode, SizeOf(Byte));
      case Mode of
        0: // Normal function
        begin
          //ExternalFunc := FThorium.ExternalFunctionRegistry.FindFunctionDirect(ObjName);
          ExternalFunc := FindHostFunction(ObjName);
          if ExternalFunc = nil then
            raise EThoriumDependencyException.Create('External function '''+ObjName+''' needed by module '''+Name+''' is currently not registered.');
        end;
        1: // Method
        begin
          Stream.Read(J, SizeOf(Integer));
          ExtendedType := TThoriumHostObjectType(FHostTypeUsage[J]);
          //ExternalFunc := ExtendedType.FindMethod(ObjName);
          ExternalMethod := ExtendedType.FindMethod(ObjName);
          if ExternalMethod = nil then
          begin
            if ExtendedType is TThoriumRTTIObjectType then
              raise EThoriumDependencyException.CreateFmt('External method ''%s.%s'' needed by module ''%s'' is currently not registered.', [TThoriumRTTIObjectType(ExtendedType).FBaseClass.ClassName, ObjName, Name])
            else
              raise EThoriumDependencyException.CreateFmt('External method ''%s.%s'' needed by module ''%s'' is currently not registered.', [ExtendedType.ClassName, ObjName, Name]);
          end;
        end;
      else
        raise EThoriumVerificationException.Create('Corrupted file.');
      end;
      FHostFuncUsage.Add(ExternalFunc);
    end;
    for I := 0 to Header.HostFuncRelocationCount - 1 do
    begin
      GetMem(Reloc, SizeOf(TThoriumRelocation));
      Stream.Read(Reloc^, SizeOf(TThoriumRelocation));
      FHostFuncRelocations.Add(Reloc);
    end;
    for I := 0 to Header.LibPropDependencyCount - 1 do
    begin
      ObjName := Stream.ReadAnsiString;
      Prop := FindLibraryProperty(ObjName);
      if Prop = nil then
        raise EThoriumDependencyException.CreateFmt('Library property ''%s'' needed by module ''%s'' not found.', [ObjName, Name]);
      Stream.Read(CurrHash, SizeOf(TThoriumHash));
      ObjHash := Prop.GetHash;
      if not CompareMem(@ObjHash, @Currhash, SizeOf(TThoriumHash)) then
        raise EThoriumHashException.CreateFmt('Hash mismatch for library property ''%s'' needed by module ''%s''.', [ObjName, Name]);
      FLibPropUsage.Add(Prop);
    end;
    for I := 0 to Header.LibPropRelocationCount - 1 do
    begin
      GetMem(Reloc, SizeOf(TThoriumRelocation));
      Stream.Read(Reloc^, SizeOf(TThoriumRelocation));
      FLibPropRelocations.Add(Reloc);
    end;
    for I := 0 to Header.FunctionExportCount - 1 do
    begin
      ExportedValue := TThoriumFunction.Create(Self);
      ExportedValue.LoadFromStream(Stream);
      FPublicFunctions.Add(TThoriumFunction(ExportedValue));
      for J := 0 to TThoriumFunction(ExportedValue).FParameters.Count - 1 do
      begin
        TypeSpec := TThoriumFunction(ExportedValue).FParameters.FList[J];
        if TypeSpec^.ValueType = vtExtendedType then
          TypeSpec^.Extended := TThoriumHostObjectType(FHostTypeUsage[ptruint(TypeSpec^.Extended)]);
      end;
      for J := 0 to TThoriumFunction(ExportedValue).FReturnValues.Count - 1 do
      begin
        TypeSpec := TThoriumFunction(ExportedValue).FReturnValues.FList[J];
        if TypeSpec^.ValueType = vtExtendedType then
          TypeSpec^.Extended := TThoriumHostObjectType(FHostTypeUsage[ptruint(TypeSpec^.Extended)]);
      end;
    end;
    for I := 0 to Header.VariableExportCount - 1 do
    begin
      ExportedValue := AddPublicVariable;
      ExportedValue.LoadFromStream(Stream);
      TypeSpec := @TThoriumVariable(ExportedValue).FTypeSpec;
      if TypeSpec^.ValueType = vtExtendedType then
        TypeSpec^.Extended := TThoriumHostObjectType(FHostTypeUsage[ptruint(TypeSpec^.Extended)]);
    end;
    FStringLibrary.Capacity := Header.StringCount;
    for I := 0 to Header.StringCount - 1 do
      FStringLibrary.Add(Stream.ReadAnsiString);
    FInstructions.LoadFromStream(Stream);
    Base := ptruint(FInstructions.Instruction[0]);
    for I := 0 to Header.HostTypeRelocationCount - 1 do
    begin
      Reloc := PThoriumRelocation(FHostTypeRelocations[I]);
      PQWord(Base + Reloc^.ByteOffset)^ := ptruint(FHostTypeUsage[Reloc^.ObjectIndex]);
    end;
    for I := 0 to Header.HostFuncRelocationCount - 1 do
    begin
      Reloc := PThoriumRelocation(FHostFuncRelocations[I]);
      PQWord(Base + Reloc^.ByteOffset)^ := ptruint(FHostFuncUsage[Reloc^.ObjectIndex]);
    end;
    for I := 0 to Header.LibPropRelocationCount - 1 do
    begin
      Reloc := PThoriumRelocation(FLibPropRelocations[I]);
      PQWord(Base + Reloc^.ByteOffset)^ := ptruint(FLibPropUsage[Reloc^.ObjectIndex]);
    end;
  except
    ClearAll;
    raise;
  end;
end;

procedure TThoriumModule.InternalSaveToStream(Stream: TStream;
  const Header: TThoriumModuleHeader);
var
  I, J: Integer;
  Module: TThoriumModule;
  Hash: TThoriumHash;
  ExtendedType: TThoriumHostObjectType;
  ExternalCallable: TThoriumHostCallableBase;
  Prop: TThoriumLibraryProperty;
  PublicFunc: TThoriumFunction;
  PublicVar: TThoriumVariable;
  TypeSpec: TThoriumType;
  List: PPointerList;
  Zero, One: Byte;
begin
  Zero := 0;
  One := 1;
  if Header.SourceFileNameLength > 0 then
    Stream.Write(FSourceFile[1], Header.SourceFileNameLength);
  for I := 0 to Header.RequireCount - 1 do
  begin
    Module := FThorium.Module[FRequiredModules.Items[I]];
    Stream.WriteAnsiString(Module.Name);
    Hash := Module.GetHash;
    Stream.Write(Hash, SizeOf(TThoriumHash));
  end;
  for I := 0 to Header.HostTypeDependencyCount - 1 do
  begin
    ExtendedType := TThoriumHostObjectType(FHostTypeUsage[I]);
    if ExtendedType is TThoriumRTTIObjectType then
    begin
      Stream.Write(One, SizeOf(Byte));
      Stream.WriteAnsiString(TThoriumRTTIObjectType(ExtendedType).FBaseClass.ClassName);
    end
    else
    begin
      Stream.Write(Zero, SizeOf(Byte));
      Stream.WriteAnsiString(ExtendedType.ClassName);
    end;
    Hash := ExtendedType.GetHash;
    Stream.Write(Hash, SizeOf(TThoriumHash));
  end;
  for I := 0 to Header.HostTypeRelocationCount - 1 do
    Stream.Write(FHostTypeRelocations[I]^, SizeOf(TThoriumRelocation));
  for I := 0 to Header.HostFuncDependencyCount - 1 do
  begin
    ExternalCallable := TThoriumHostFunctionBase(FHostFuncUsage[I]);
    Stream.WriteAnsiString(ExternalCallable.Name);
    if ExternalCallable is TThoriumHostMethodBase then
    begin
      Stream.Write(One, SizeOf(Byte));
      J := FHostTypeUsage.IndexOf(TThoriumHostMethodBase(ExternalCallable).FHostObjectType);
      Stream.Write(J, SizeOf(Integer));
    end
    else
    begin
      Stream.Write(Zero, SizeOf(Byte));
    end;
  end;
  for I := 0 to Header.HostFuncRelocationCount - 1 do
  begin
    Stream.Write(FHostFuncRelocations[I]^, SizeOf(TThoriumRelocation));
  end;
  for I := 0 to Header.LibPropDependencyCount - 1 do
  begin
    Prop := TThoriumLibraryProperty(FLibPropUsage[I]);
    Stream.WriteAnsiString(Prop.FName);
    Stream.Write(Prop.GetHash, SizeOf(TThoriumHash));
  end;
  for I := 0 to Header.LibPropRelocationCount - 1 do
  begin
    Stream.Write(FLibPropRelocations[I]^, SizeOf(TThoriumRelocation));
  end;
  List := FPublicFunctions.List;
  for I := 0 to Header.FunctionExportCount - 1 do
  begin
    //FPublicFunctions.Functions[I].SaveToStream(Stream);
    PublicFunc := TThoriumFunction(List^[I]);
    Stream.WriteAnsiString(PublicFunc.Name);
    Stream.Write(PublicFunc.FEntryPoint, SizeOf(Integer));
    Stream.Write(PublicFunc.FVisibilityLevel, SizeOf(TThoriumVisibilityLevel));
    Stream.Write(PublicFunc.FParameters.Count, SizeOf(Integer));
    for J := 0 to PublicFunc.FParameters.Count - 1 do
    begin
      PublicFunc.FParameters.GetParameterSpec(J, TypeSpec);
      if not (TypeSpec.ValueType in [vtBuiltIn, vtExtendedType]) then
        raise EThoriumVerificationException.Create('Unsupported parameter type of a public function.');
      if TypeSpec.ValueType = vtExtendedType then
        TypeSpec.Extended := TThoriumHostObjectType(ptruint(FHostTypeUsage.IndexOf(TypeSpec.Extended)));
      Stream.Write(TypeSpec, SizeOf(TThoriumType));
    end;
    Stream.Write(PublicFunc.FReturnValues.Count, SizeOf(Integer));
    for J := 0 to PublicFunc.FReturnValues.Count - 1 do
    begin
      PublicFunc.FReturnValues.GetParameterSpec(J, TypeSpec);
      if not (TypeSpec.ValueType in [vtBuiltIn, vtExtendedType]) then
        raise EThoriumVerificationException.Create('Unsupported return value type of a public function.');
      if TypeSpec.ValueType = vtExtendedType then
        TypeSpec.Extended := TThoriumHostObjectType(ptruint(FHostTypeUsage.IndexOf(TypeSpec.Extended)));
      Stream.Write(TypeSpec, SizeOf(TThoriumType));
    end;
  end;
  List := FPublicVariables.List;
  for I := 0 to Header.VariableExportCount - 1 do
  begin
    PublicVar := TThoriumVariable(List^[I]);
    Stream.WriteAnsiString(PublicVar.Name);
    Stream.Write(PublicVar.FIsStatic, SizeOf(Byte));
    Stream.Write(PublicVar.FStackPosition, SizeOf(Integer));
    TypeSpec := PublicVar.FTypeSpec;
    if not (TypeSpec.ValueType in [vtBuiltIn, vtExtendedType] )then
      raise EThoriumVerificationException.Create('Unsupported type of a public variable.');
    if TypeSpec.ValueType = vtExtendedType then
      TypeSpec.Extended := TThoriumHostObjectType(ptruint(FHostTypeUsage.IndexOf(TypeSpec.Extended)));
    Stream.Write(TypeSpec, SizeOf(TThoriumType));
  end;
  for I := 0 to Header.StringCount - 1 do
    Stream.WriteAnsiString(FStringLibrary[I]);

  FInstructions.SaveToStream(Stream);
end;

function TThoriumModule.CompileFromStream(SourceStream: TStream;
  Flags: TThoriumCompilerFlags): Boolean;
var
  Compiler: TThoriumDefaultCompiler;
begin
  Result := False;
  FCompiled := False;
  Compiler := TThoriumDefaultCompiler.Create(Self);
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

procedure TThoriumModule.Dump;
var
  I, J: Integer;
  TypeSpec: TThoriumType;
  Callable: TThoriumHostCallableBase;
  List: PPointerList;
begin
  WriteLn('Module: ', FName);
  WriteLn('Code (', FOptimizedInstructions, ' removed by optimization): ');
  WriteLn(DumpCodeStr);
  //WriteLn('Optimized instructions: ', FOptimizedInstructions);
  if FPublicFunctions.Count > 0 then
  begin;
    //WriteLn('Exported functions: ');
    WriteLn(' EntryAddr   Header');
    List := FPublicFunctions.List;
    for I := 0 to FPublicFunctions.Count - 1 do
    begin
      with TThoriumFunction(List^[I]) do
      begin
        if FReturnValues.Count = 0 then
          Write('0x', IntToHex(EntryPoint, 8), '  void ')
        else
        begin
          FReturnValues.GetParameterSpec(0, TypeSpec);
          Write('0x', IntToHex(EntryPoint, 8), '  ', ThoriumTypeName(TypeSpec), ' ');
        end;
        Write(Name, '(');
        for J := 0 to FParameters.Count - 1 do
        begin
          if (J <> 0) then
            Write(', ');
          FParameters.GetParameterSpec(J, TypeSpec);
          Write(ThoriumTypeName(TypeSpec));
        end;
        WriteLn(')');
      end;
    end;
  end;
  if FPublicVariables.Count > 0 then
  begin
    //WriteLn('Exported variables:');
    WriteLn(' StackAddr   Flag    Name');
    List := FPublicVariables.List;
    for I := 0 to FPublicVariables.Count - 1 do
    begin
      with TThoriumVariable(List^[I]) do
      begin
        if IsStatic then
          WriteLn('0x', IntToHex(StackPosition, 8), '  static  ', ThoriumTypeName(TypeSpec), ' ', Name)
        else
          WriteLn('0x', IntToHex(StackPosition, 8), '          ', ThoriumTypeName(TypeSpec), ' ', Name);
      end;
    end;
  end;
  if FStringLibrary.Count > 0 then
  begin
    //WriteLn('Strings:');
    WriteLn(' Index       String');
    for I := 0 to FStringLibrary.Count - 1 do
      WriteLn('0x', IntToHex(I, 8), '  "', StringReplace(StringReplace(FStringLibrary[I], #10, '\n', [rfReplaceAll]), '"', '\"', [rfReplaceAll]), '"');
  end;
  if FHostTypeUsage.Count > 0 then
  begin
    //WriteLn('Extended type usage:');
    WriteLn(' Index       Flg   Type name');
    for I := 0 to FHostTypeUsage.Count - 1 do
    begin
      if TThoriumHostObjectType(FHostTypeUsage[I]) is TThoriumRTTIObjectType then
        WriteLn('0x', IntToHex(I, 8), '  RTTI  ', TThoriumRTTIObjectType(FHostTypeUsage[I]).BaseClass.ClassName)
      else
        WriteLn('0x', IntToHex(I, 8), '        ', TThoriumHostObjectType(FHostTypeUsage[I]).ClassName);
    end;
    if FHostTypeRelocations.Count > 0 then
    begin
      WriteLn(' UsByteOff   Index');
      for I := 0 to FHostTypeRelocations.Count - 1 do
      begin
        with TThoriumRelocation(FHostTypeRelocations[I]^) do
          WriteLn('0x', IntToHex(ByteOffset, 8), '  0x', IntToHex(ObjectIndex, 8));
      end;
    end;
  end;
  if FHostFuncUsage.Count > 0 then
  begin
    WriteLn(' Index       Name');
    for I := 0 to FHostFuncUsage.Count - 1 do
    begin
      Write('0x', IntToHex(I, 8), '  ');
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
      WriteLn(' UsByteOff   Index');
      for I := 0 to FHostFuncRelocations.Count - 1 do
        with PThoriumRelocation(FHostFuncRelocations[I])^ do
        begin
          WriteLn('0x', IntToHex(ByteOffset, 8), '  0x', IntToHex(ObjectIndex, 8));
        end;
    end;
  end;
  if FRequiredModules.Count > 0 then
  begin
    WriteLn(' Index       Module name');
    for I := 0 to FRequiredModules.Count - 1 do
    begin
      WriteLn('0x', IntToHex(I, 8), '  ', TThoriumModule(FThorium.FModules[FRequiredModules.Items[I]]).Name);
    end;
  end;
end;

function TThoriumModule.DumpCodeStr: String;
// Encapsulation for TThoriumInstructions.DumpCodeStr
begin
  Result := FInstructions.DumpCodeStr;
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
  HostType: TThoriumHostObjectType;
begin
  HostType := nil;
  for I := FRequiredLibraries.Count - 1 downto 0 do
  begin
    HostType := TThoriumLibrary(FThorium.FHostLibraries[FRequiredLibraries[I]]).FindHostTypeForType(AType);
    if HostType <> nil then
      Break;
  end;
  if HostType = nil then
    raise EThoriumRuntimeException.Create('Cannot enclose type.');
  Result._Type := vtExtendedType;
  Result.Extended.TypeClass := HostType;
  Result.Extended.Value := AValue;
end;

function TThoriumModule.FindPublicFunction(const AName: String
  ): TThoriumFunction;
var
  List: PPointerList;
  I: Integer;
begin
  List := FPublicFunctions.List;
  for I := 0 to FPublicFunctions.Count - 1 do
  begin
    Result := TThoriumFunction(List^[I]);
    if Result.FName = AName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumModule.IndexOfPublicFunction(const AName: String): Integer;
var
  List: PPointerList;
  I: Integer;
begin
  List := FPublicFunctions.List;
  for I := 0 to FPublicFunctions.Count - 1 do
    if TThoriumFunction(List^[I]).FName = AName then
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
  Result^._Type := etValue;
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
  //Result^._Type := etValue;
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
        if (FEntries[Idx]._Type = etValue) then
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
        etValue:
          ThoriumFreeValue(CurrEntry^.Value);
        etVarargs:
        begin
          FreeMem(CurrEntry^.VADataOrigin);
          if CurrEntry^.VABufferOrigin <> nil then
            FreeMem(CurrEntry^.VABufferOrigin);
          if CurrEntry^.VAToFreeOrigin <> nil then
          begin
            while CurrEntry^.VAToFree <> CurrEntry^.VAToFreeOrigin do
            begin
              Dec(CurrEntry^.VAToFree);
              ThoriumFreeValue(CurrEntry^.VAToFree[0]);
            end;
            FreeMem(CurrEntry^.VAToFreeOrigin);
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
    if FEntries[Idx]._Type = etValue then
      ThoriumFreeValue(FEntries[Idx].Value);
      // IF YOU EXPERIENCE A CRASH HERE, HAVE A LOOK IN THE COMMENT ABOVE!
  end;
  FCount := 0;
  FTop := FEntries;
  Dec(FTop);
end;

{ TThoriumVirtualMachine }

constructor TThoriumVirtualMachine.Create(AThorium: TThorium);
var
  I: Integer;
begin
  inherited Create;
  FThorium := AThorium;
  FStack := TThoriumStack.Create;
  FModuleStackIndicies := TThoriumIntList.Create;
  FCurrentModuleIdx := -1;
  FCurrentModule := nil;
  FCurrentInstructionIdx := -1;
  FCurrentStackFrame := -1;
  for I := 0 to FThorium.FModules.Count - 1 do
  begin
    FModuleStackIndicies.AddEntry(FStack.FCount);
    if TThoriumModule(FThorium.FModules[I]).FInstructions.Count > 0 then
      Execute(I, 0, False);
  end;
  FillByte(FRegisters, SizeOf(TThoriumRegisters), 0);
end;

destructor TThoriumVirtualMachine.Destroy;
begin
  FModuleStackIndicies.Free;
  FStack.Free;
  inherited Destroy;
end;

function TThoriumVirtualMachine.StackScopeToIndex(Scope: Int64): Integer;
begin
  case Scope of
    THORIUM_STACK_SCOPE_NOSCOPE: Result := 0;
    THORIUM_STACK_SCOPE_FROMTOP: Result := FStack.FCount-1;
    THORIUM_STACK_SCOPE_MODULEROOT: Result := FModuleStackIndicies.Items[FCurrentModuleIdx];
    THORIUM_STACK_SCOPE_PARAMETERS: Result := FCurrentStackFrame;
    THORIUM_STACK_SCOPE_LOCAL: Result := FCurrentStackFrame + 1;
  else
    Result := 0;
  end;
end;

procedure TThoriumVirtualMachine.ExecuteInstruction; inline;
var
  _NewEntry, _Operand1: PThoriumStackEntry;
  Intf: IThoriumPersistent;
  
  function FastStackScopeToIndex(Scope: SmallInt): Integer; inline;
  begin
    case Scope of
      THORIUM_STACK_SCOPE_NOSCOPE: Result := 0;
      THORIUM_STACK_SCOPE_FROMTOP: Result := FStack.FCount-1;
      THORIUM_STACK_SCOPE_MODULEROOT: Result := FModuleStackIndicies.Items[FCurrentModuleIdx];
      THORIUM_STACK_SCOPE_PARAMETERS: Result := FCurrentStackFrame;
      THORIUM_STACK_SCOPE_LOCAL: Result := FCurrentStackFrame + 1;
    else
      Result := 0;
    end;
  end;

  procedure int_s; inline;
  begin
    with TThoriumInstructionINT_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
      _NewEntry^.Value._Type := vtBuiltIn;
      _NewEntry^.Value.BuiltIn._Type := btInteger;
      _NewEntry^.Value.BuiltIn.Int := Value;
      {$ifdef Timecheck}EndTimecheck('int.s');{$endif}
    end;
  end;

  procedure int; inline;
  begin
    with TThoriumInstructionINT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := Value;
      {$ifdef Timecheck}EndTimecheck('int');{$endif}
    end;
  end;

  procedure intb; inline;
  begin
    with TThoriumInstructionINTB(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      case Kind of
        THORIUM_OP_EQUAL:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_EQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].BuiltIn.Int := 1
          else
            FRegisters[TRI].BuiltIn.Int := 0;
        end;
        THORIUM_OP_NOTEQUAL:
        begin
          if not (FStateRegister and THORIUM_STATE_ACCUM_EQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].BuiltIn.Int := 1
          else
            FRegisters[TRI].BuiltIn.Int := 0;
        end;
        THORIUM_OP_GREATER:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_GREATER = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].BuiltIn.Int := 1
          else
            FRegisters[TRI].BuiltIn.Int := 0;
        end;
        THORIUM_OP_LESS:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_LESS = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].BuiltIn.Int := 1
          else
            FRegisters[TRI].BuiltIn.Int := 0;
        end;
        THORIUM_OP_GREATEREQUAL:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_GREATEREQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].BuiltIn.Int := 1
          else
            FRegisters[TRI].BuiltIn.Int := 0;
        end;
        THORIUM_OP_LESSEQUAL:
        begin
          if (FStateRegister and THORIUM_STATE_ACCUM_LESSEQUAL = FStateRegister and THORIUM_STATE_COMPFLAGS) then
            FRegisters[TRI].BuiltIn.Int := 1
          else
            FRegisters[TRI].BuiltIn.Int := 0;
        end;
      else
        FRegisters[TRI].BuiltIn.Int := 0;
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
      _NewEntry^._Type := etValue;
      _NewEntry^.Value._Type := vtBuiltIn;
      _NewEntry^.Value.BuiltIn._Type := btFloat;
      _NewEntry^.Value.BuiltIn.Float := Value;
      {$ifdef Timecheck}EndTimecheck('flt.s');{$endif}
    end;
  end;

  procedure flt; inline;
  begin
    with TThoriumInstructionFLT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btFloat;
      FRegisters[TRI].BuiltIn.Float := Value;
      {$ifdef Timecheck}EndTimecheck('flt');{$endif}
    end;
  end;

  procedure str_s; inline;
  begin
    with TThoriumInstructionSTR_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
      _NewEntry^.Value._Type := vtBuiltIn;
      _NewEntry^.Value.BuiltIn._Type := btString;
      New(_NewEntry^.Value.BuiltIn.Str);
      {$ifdef Timecheck}EndTimecheck('str.s');{$endif}
    end;
  end;

  procedure strl_s; inline;
  begin
    with TThoriumInstructionSTRL_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
      _NewEntry^.Value._Type := vtBuiltIn;
      _NewEntry^.Value.BuiltIn._Type := btString;
      New(_NewEntry^.Value.BuiltIn.Str);
      _NewEntry^.Value.BuiltIn.Str^ := FCurrentModule.FStringLibrary[Index];
      {$ifdef Timecheck}EndTimecheck('strl.s');{$endif}
    end;
  end;

  procedure str; inline;
  begin
    with TThoriumInstructionSTR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btString;
      New(FRegisters[TRI].BuiltIn.Str);
      {$ifdef Timecheck}EndTimecheck('str');{$endif}
    end;
  end;

  procedure strl; inline;
  begin
    with TThoriumInstructionSTRL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btString;
      New(FRegisters[TRI].BuiltIn.Str);
      FRegisters[TRI].BuiltIn.Str^ := FCurrentModule.FStringLibrary[Index];
      {$ifdef Timecheck}EndTimecheck('strl');{$endif}
    end;
  end;

  procedure ext_s; inline;
  begin
    with TThoriumInstructionEXT_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
      _NewEntry^.Value._Type := vtExtendedType;
      _NewEntry^.Value.Extended.TypeClass := TThoriumHostObjectType(ExtendedType);
      _NewEntry^.Value.Extended.Value := TThoriumHostObjectType(ExtendedType).GetNewInstance;
      {$ifdef Timecheck}EndTimecheck('ext.s');{$endif}
    end;
  end;

  procedure ext; inline;
  begin
    with TThoriumInstructionEXT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtExtendedType;
      FRegisters[TRI].Extended.Value := TThoriumHostObjectType(ExtendedType).GetNewInstance;
      FRegisters[TRI].Extended.TypeClass := TThoriumHostObjectType(ExtendedType);
      {$ifdef Timecheck}EndTimecheck('ext');{$endif}
    end;
  end;

  procedure fnc; inline;
  begin
    with TThoriumInstructionFNC(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction fnc not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('fnc');{$endif}
    end;
  end;

  procedure fnce; inline;
  begin
    with TThoriumInstructionFNCE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction fnce not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('fnce');{$endif}
    end;
  end;

  procedure copyr_s; inline;
  begin
    with TThoriumInstructionCOPYR_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset);
      ThoriumFreeValue(_NewEntry^.Value);
      _NewEntry^.Value := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('copyr.s');{$endif}
    end;
  end;

  procedure copyr_st; inline;
  begin
    with TThoriumInstructionCOPYR_ST(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
      _NewEntry^.Value := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('copyr.st');{$endif}
    end;
  end;

  procedure copyr_fs; inline;
  begin
    with TThoriumInstructionCOPYR_FS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset);
      ThoriumFreeValue(_NewEntry^.Value);
      _NewEntry^.Value := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('copyr.fs');{$endif}
    end;
  end;

  procedure copys_st; inline;
  begin
    with TThoriumInstructionCOPYS_ST(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
      _NewEntry^.Value := ThoriumDuplicateValue(FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset)^.Value);
      {$ifdef Timecheck}EndTimecheck('copys.st');{$endif}
    end;
  end;

  procedure copyfs; inline;
  begin
    with TThoriumInstructionCOPYFS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := ThoriumDuplicateValue(FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset)^.Value);
      {$ifdef Timecheck}EndTimecheck('copyfs');{$endif}
    end;
  end;

  procedure copys; inline;
  begin
    with TThoriumInstructionCOPYS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := ThoriumDuplicateValue(FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset)^.Value);
      {$ifdef Timecheck}EndTimecheck('copys');{$endif}
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

  procedure moves_s; inline;
  begin
    with TThoriumInstructionMOVES_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTop;
      _NewEntry := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset);
      ThoriumFreeValue(_NewEntry^.Value);
      _NewEntry^._Type := etValue;
      Move(_Operand1^, _NewEntry^, SizeOf(TThoriumStackEntry));
      FStack.Pop(1, False);
      {$ifdef Timecheck}EndTimecheck('moves.s');{$endif}
    end;
  end;

  procedure mover_s; inline;
  begin
    with TThoriumInstructionMOVER_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset);
      ThoriumFreeValue(_NewEntry^.Value);
      _NewEntry^._Type := etValue;
      Move(FRegisters[SRI], _NewEntry^.Value, SizeOf(TThoriumValue));
      {$ifdef Timecheck}EndTimecheck('mover.s');{$endif}
    end;
  end;

  procedure mover_st; inline;
  begin
    with TThoriumInstructionMOVER_ST(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
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

  procedure moves; inline;
  begin
    with TThoriumInstructionMOVES(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      Move(FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset)^.Value, FRegisters[TRI], SizeOf(TThoriumValue));
      {$ifdef Timecheck}EndTimecheck('copys');{$endif}
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

  procedure mover_fs; inline;
  begin
    with TThoriumInstructionMOVER_FS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset);
      ThoriumFreeValue(_NewEntry^.Value);
      _NewEntry^.Value := ThoriumDuplicateValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('copyr.fs');{$endif}
    end;
  end;

  procedure movefs; inline;
  begin
    with TThoriumInstructionMOVEFS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI] := FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset)^.Value;
      {$ifdef Timecheck}EndTimecheck('copyfs');{$endif}
    end;
  end;

  procedure moves_st; inline;
  begin
    with TThoriumInstructionMOVES_ST(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etValue;
      _NewEntry^.Value := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset)^.Value;
      {$ifdef Timecheck}EndTimecheck('copys.st');{$endif}
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
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btFloat;
      FRegisters[TRI].BuiltIn.Float := FRegisters[SRI].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('castif');{$endif}
    end;
  end;

  procedure castie; inline;
  begin
    with TThoriumInstructionCASTIE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction castie not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('castie');{$endif}
    end;
  end;

  procedure castfe; inline;
  begin
    with TThoriumInstructionCASTFE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction castfe not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('castfe');{$endif}
    end;
  end;

  procedure castse; inline;
  begin
    with TThoriumInstructionCASTSE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction castse not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('castse');{$endif}
    end;
  end;

  procedure castei; inline;
  begin
    with TThoriumInstructionCASTEI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction castei not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('castei');{$endif}
    end;
  end;

  procedure castef; inline;
  begin
    with TThoriumInstructionCASTEF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction castef not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('castef');{$endif}
    end;
  end;

  procedure castes; inline;
  begin
    with TThoriumInstructionCASTES(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumException.Create('Instruction castes not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('castes');{$endif}
    end;
  end;

  procedure caste; inline;
  begin
    with TThoriumInstructionCASTE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction caste not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('caste');{$endif}
    end;
  end;

  procedure cmpi; inline;
  begin
    with TThoriumInstructionCMPI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      if FRegisters[Op1].BuiltIn.Int > FRegisters[Op2].BuiltIn.Int then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].BuiltIn.Int < FRegisters[Op2].BuiltIn.Int then
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
      if FRegisters[Op1].BuiltIn.Int > FRegisters[Op2].BuiltIn.Float then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].BuiltIn.Int < FRegisters[Op2].BuiltIn.Float then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmpif');{$endif}
    end;
  end;

  procedure cmpie; inline;
  begin
    with TThoriumInstructionCMPIE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction cmpie not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('cmpie');{$endif}
    end;
  end;

  procedure cmpf; inline;
  begin
    with TThoriumInstructionCMPF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      if FRegisters[Op1].BuiltIn.Float > FRegisters[Op2].BuiltIn.Float then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].BuiltIn.Float < FRegisters[Op2].BuiltIn.Float then
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
      if FRegisters[Op1].BuiltIn.Float > FRegisters[Op2].BuiltIn.Int then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if FRegisters[Op1].BuiltIn.Float < FRegisters[Op2].BuiltIn.Int then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmpfi');{$endif}
    end;
  end;

  procedure cmpfe; inline;
  begin
    with TThoriumInstructionCMPFE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction cmpfe not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('cmpfe');{$endif}
    end;
  end;

  procedure cmps; inline;
  begin
    with TThoriumInstructionCMPS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FStateRegister := FStateRegister and (not (FStateRegister and THORIUM_STATE_COMPFLAGS));
      if ThoriumDerefStr(FRegisters[Op1].BuiltIn.Str) > ThoriumDerefStr(FRegisters[Op2].BuiltIn.Str) then
        FStateRegister := FStateRegister or THORIUM_STATE_GREATER
      else if ThoriumDerefStr(FRegisters[Op1].BuiltIn.Str) < ThoriumDerefStr(FRegisters[Op2].BuiltIn.Str) then
        FStateRegister := FStateRegister or THORIUM_STATE_LESS
      else
        FStateRegister := FStateRegister or THORIUM_STATE_EQUAL;
      {$ifdef Timecheck}EndTimecheck('cmps');{$endif}
    end;
  end;

  procedure cmpse; inline;
  begin
    with TThoriumInstructionCMPSE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction cmpse not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('cmpse');{$endif}
    end;
  end;

  procedure cmpe; inline;
  begin
    with TThoriumInstructionCMPE(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction cmpe not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('cmpe');{$endif}
    end;
  end;

  procedure cmpei; inline;
  begin
    with TThoriumInstructionCMPEI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction cmpei not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('cmpei');{$endif}
    end;
  end;

  procedure cmpef; inline;
  begin
    with TThoriumInstructionCMPEF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException.Create('Instruction cmpef not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('cmpef');{$endif}
    end;
  end;

  procedure cmpes; inline;
  begin
    with TThoriumInstructionCMPES(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      raise EThoriumRuntimeException .Create('Instruction cmpes not implemented yet.');
      {$ifdef Timecheck}EndTimecheck('cmpes');{$endif}
    end;
  end;

  procedure addi; inline;
  begin
    with TThoriumInstructionADDI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int +
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('addi');{$endif}
    end;
  end;

  procedure addf; inline;
  begin
    with TThoriumInstructionADDF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btFloat;
      FRegisters[TRI].BuiltIn.Float := FRegisters[Op1].BuiltIn.Float +
        FRegisters[Op2].BuiltIn.Float;
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
        FRegisters[TRI] := ThoriumCreateStringValue(FRegisters[Op1].BuiltIn.Str^ + FRegisters[Op2].BuiltIn.Str^);
      end
      else
      begin
        FRegisters[TRI]._Type := vtBuiltIn;
        FRegisters[TRI].BuiltIn._Type := btString;
        New(FRegisters[TRI].BuiltIn.Str);
        FRegisters[TRI].BuiltIn.Str^ := FRegisters[Op1].BuiltIn.Str^ + FRegisters[Op2].BuiltIn.Str^;
      end;
      {$ifdef Timecheck}EndTimecheck('adds');{$endif}
    end;
  end;

  procedure subi; inline;
  begin
    with TThoriumInstructionSUBI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int -
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('subi');{$endif}
    end;
  end;

  procedure subf; inline;
  begin
    with TThoriumInstructionSUBF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btFloat;
      FRegisters[TRI].BuiltIn.Float := FRegisters[Op1].BuiltIn.Float -
        FRegisters[Op2].BuiltIn.Float;
      {$ifdef Timecheck}EndTimecheck('subf');{$endif}
    end;
  end;

  procedure muli; inline;
  begin
    with TThoriumInstructionMULI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int *
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('muli');{$endif}
    end;
  end;

  procedure mulf; inline;
  begin
    with TThoriumInstructionMULF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btFloat;
      FRegisters[TRI].BuiltIn.Float := FRegisters[Op1].BuiltIn.Float *
        FRegisters[Op2].BuiltIn.Float;
      {$ifdef Timecheck}EndTimecheck('mulf');{$endif}
    end;
  end;

  procedure divi; inline;
  begin
    with TThoriumInstructionDIVI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int div
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('divi');{$endif}
    end;
  end;

  procedure divf; inline;
  begin
    with TThoriumInstructionDIVF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btFloat;
      FRegisters[TRI].BuiltIn.Float := FRegisters[Op1].BuiltIn.Float /
        FRegisters[Op2].BuiltIn.Float;
      {$ifdef Timecheck}EndTimecheck('divf');{$endif}
    end;
  end;

  procedure negi; inline;
  begin
    with TThoriumInstructionNEGI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[Op1].BuiltIn.Int := -FRegisters[Op1].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('negi');{$endif}
    end;
  end;

  procedure negf; inline;
  begin
    with TThoriumInstructionNEGF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[Op1].BuiltIn.Float := -FRegisters[Op1].BuiltIn.Float;
      {$ifdef Timecheck}EndTimecheck('negf');{$endif}
    end;
  end;

  procedure _not; inline;
  begin
    with TThoriumInstructionNOT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[Op1].BuiltIn.Int := not FRegisters[Op1].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('not');{$endif}
    end;
  end;

  procedure bnot; inline;
  begin
    with TThoriumInstructionNOT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if FRegisters[Op1].BuiltIn.Int = 0 then
        FRegisters[Op1].BuiltIn.Int := 1
      else
        FRegisters[Op1].BuiltIn.Int := 0;
      {$ifdef Timecheck}EndTimecheck('not');{$endif}
    end;
  end;

  procedure _mod; inline;
  begin
    with TThoriumInstructionMOD(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int mod
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('mod');{$endif}
    end;
  end;

  procedure _and; inline;
  begin
    with TThoriumInstructionAND(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int and
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('and');{$endif}
    end;
  end;

  procedure _or; inline;
  begin
    with TThoriumInstructionOR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int or
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('or');{$endif}
    end;
  end;

  procedure _xor; inline;
  begin
    with TThoriumInstructionXOR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int xor
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('xor');{$endif}
    end;
  end;

  procedure _shl; inline;
  begin
    with TThoriumInstructionSHL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int shl
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('shl');{$endif}
    end;
  end;

  procedure _shr; inline;
  begin
    with TThoriumInstructionSHR(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI]._Type := vtBuiltIn;
      FRegisters[TRI].BuiltIn._Type := btInteger;
      FRegisters[TRI].BuiltIn.Int := FRegisters[Op1].BuiltIn.Int shr
        FRegisters[Op2].BuiltIn.Int;
      {$ifdef Timecheck}EndTimecheck('shr');{$endif}
    end;
  end;

  procedure inci_s; inline;
  begin
    with TThoriumInstructionINCI_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset);
      _Operand1^.Value.BuiltIn.Int := _Operand1^.Value.BuiltIn.Int + 1;
      {$ifdef Timecheck}EndTimecheck('inci.s');{$endif}
    end;
  end;

  procedure incf_s; inline;
  begin
    with TThoriumInstructionINCF_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset);
      _Operand1^.Value.BuiltIn.Float := _Operand1^.Value.BuiltIn.Float + 1;
      {$ifdef Timecheck}EndTimecheck('incf.s');{$endif}
    end;
  end;

  procedure inci_fs; inline;
  begin
    with TThoriumInstructionINCI_FS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset);
      _Operand1^.Value.BuiltIn.Int := _Operand1^.Value.BuiltIn.Int + 1;
      {$ifdef Timecheck}EndTimecheck('inci.fs');{$endif}
    end;
  end;

  procedure incf_fs; inline;
  begin
    with TThoriumInstructionINCF_FS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset);
      _Operand1^.Value.BuiltIn.Float := _Operand1^.Value.BuiltIn.Float + 1;
      {$ifdef Timecheck}EndTimecheck('incf.fs');{$endif}
    end;
  end;

  procedure inci; inline;
  begin
    with TThoriumInstructionINCI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].BuiltIn.Int+=1;
      {$ifdef Timecheck}EndTimecheck('inci');{$endif}
    end;
  end;

  procedure incf; inline;
  begin
    with TThoriumInstructionINCF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].BuiltIn.Float+=1;
      {$ifdef Timecheck}EndTimecheck('incf');{$endif}
    end;
  end;

  procedure deci_s; inline;
  begin
    with TThoriumInstructionDECI_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset);
      _Operand1^.Value.BuiltIn.Int := _Operand1^.Value.BuiltIn.Int - 1;
      {$ifdef Timecheck}EndTimecheck('deci.s');{$endif}
    end;
  end;

  procedure decf_s; inline;
  begin
    with TThoriumInstructionDECF_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FastStackScopeToIndex(Scope), Offset);
      _Operand1^.Value.BuiltIn.Float := _Operand1^.Value.BuiltIn.Float - 1;
      {$ifdef Timecheck}EndTimecheck('decf.s');{$endif}
    end;
  end;

  procedure deci_fs; inline;
  begin
    with TThoriumInstructionDECI_FS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset);
      _Operand1^.Value.BuiltIn.Int := _Operand1^.Value.BuiltIn.Int - 1;
      {$ifdef Timecheck}EndTimecheck('deci.fs');{$endif}
    end;
  end;

  procedure decf_fs; inline;
  begin
    with TThoriumInstructionDECF_FS(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(FModuleStackIndicies.Items[ModuleIndex], Offset);
      _Operand1^.Value.BuiltIn.Float := _Operand1^.Value.BuiltIn.Float - 1;
      {$ifdef Timecheck}EndTimecheck('decf.fs');{$endif}
    end;
  end;

  procedure deci; inline;
  begin
    with TThoriumInstructionDECI(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].BuiltIn.Int-=1;
      {$ifdef Timecheck}EndTimecheck('deci');{$endif}
    end;
  end;

  procedure decf; inline;
  begin
    with TThoriumInstructionDECF(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[TRI].BuiltIn.Float-=1;
      {$ifdef Timecheck}EndTimecheck('decf');{$endif}
    end;
  end;

  procedure xfget; inline;
  begin
    with TThoriumInstructionXFGET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if (TRI = ERI) then
      begin
        FRegisters[THORIUM_REGISTER_C1] := FRegisters[ERI];
        FRegisters[ERI] := FRegisters[THORIUM_REGISTER_C1].Extended.TypeClass.GetField(FRegisters[ERI], ID);
        //ThoriumFreeValue(FRegisters[THORIUM_REGISTER_C1]);
      end
      else
      begin
        FRegisters[TRI] := FRegisters[ERI].Extended.TypeClass.GetField(FRegisters[ERI], ID);
        //ThoriumFreeValue(FRegisters[ERI]);
      end;
      {$ifdef Timecheck}EndTimecheck('xfget');{$endif}
    end;
  end;

  procedure xfset; inline;
  begin
    with TThoriumInstructionXFSET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[ERI].Extended.TypeClass.SetField(FRegisters[ERI], ID, FRegisters[VRI]);
      // ThoriumFreeValue(FRegisters[ERI]);
      ThoriumFreeValue(FRegisters[VRI]);
      {$ifdef Timecheck}EndTimecheck('xfset');{$endif}
    end;
  end;

  procedure xiget; inline;
  begin
    with TThoriumInstructionXIGET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      if TRI = ERI then
      begin
        FRegisters[THORIUM_REGISTER_C1] := FRegisters[ERI];
        FRegisters[ERI] := FRegisters[ERI].Extended.TypeClass.GetIndex(FRegisters[ERI], FRegisters[IRI]);
        ThoriumFreeValue(FRegisters[THORIUM_REGISTER_C1]);
        ThoriumFreeValue(FRegisters[IRI]);
      end
      else if TRI = IRI then
      begin
        FRegisters[THORIUM_REGISTER_C1] := FRegisters[IRI];
        FRegisters[IRI] := FRegisters[ERI].Extended.TypeClass.GetIndex(FRegisters[ERI], FRegisters[IRI]);
        ThoriumFreeValue(FRegisters[THORIUM_REGISTER_C1]);
        //ThoriumFreeValue(FRegisters[ERI]);
      end
      else
      begin
        FRegisters[TRI] := FRegisters[ERI].Extended.TypeClass.GetIndex(FRegisters[ERI], FRegisters[IRI]);
        //ThoriumFreeValue(FRegisters[ERI]);
        ThoriumFreeValue(FRegisters[IRI]);
      end;
      {$ifdef Timecheck}EndTimecheck('xiget');{$endif}
    end;
  end;

  procedure xct; inline;
  begin
    with TThoriumInstructionXCT(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      TObject(FRegisters[ERI].Extended.Value).GetInterface(IThoriumPersistent, Intf);
      Intf.EnableHostControl;
      Intf := nil;
      {$ifdef Timecheck}EndTimecheck('xct');{$endif}
    end;
  end;

  procedure xiset; inline;
  begin
    with TThoriumInstructionXISET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      FRegisters[ERI].Extended.TypeClass.SetIndex(FRegisters[ERI], FRegisters[IRI], FRegisters[VRI]);
      //ThoriumFreeValue(FRegisters[ERI]);
      ThoriumFreeValue(FRegisters[IRI]);
      ThoriumFreeValue(FRegisters[VRI]);
      {$ifdef Timecheck}EndTimecheck('xiset');{$endif}
    end;
  end;

  procedure xpget; inline;
  begin
    with TThoriumInstructionXPGET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      TThoriumLibraryProperty(Prop).GetValue(@FRegisters[TRI]);
      {$ifdef Timecheck}EndTimecheck('xpset');{$endif}
    end;
  end;

  procedure xpset; inline;
  begin
    with TThoriumInstructionXPSET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      TThoriumLibraryProperty(Prop).SetValue(@FRegisters[SRI]);
      ThoriumFreeValue(FRegisters[SRI]);
      {$ifdef Timecheck}EndTimecheck('xpset');{$endif}
    end;
  end;

  procedure vastart; inline;
  begin
    with TThoriumInstructionVASTART(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etVarargs;
      if Pointers = 1 then
        _NewEntry^.VAData := GetMem(Length * SizeOf(ptruint))
      else
        _NewEntry^.VAData := GetMem(Length);
      _NewEntry^.VADataOrigin := _NewEntry^.VAData;
      _NewEntry^.VABufferOrigin := nil;
      _NewEntry^.VAToFreeOrigin := nil;

      {$ifdef Timecheck}EndTimecheck('vastart');{$endif}
    end;
  end;

  procedure va_i8; inline;
  begin
    with TThoriumInstructionVA_I8(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].BuiltIn.Int >= $FF then
        PByte(_Operand1^.VAData)^ := $FF
      else if FRegisters[SRI].BuiltIn.Int <= 0 then
        PByte(_Operand1^.VAData)^ := 0
      else
        PByte(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int;
      Inc(_Operand1^.VAData, 1);
      {$ifdef Timecheck}EndTimecheck('va.i8');{$endif}
    end;
  end;

  procedure va_i16; inline;
  begin
    with TThoriumInstructionVA_I16(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].BuiltIn.Int >= $FFFF then
        PWord(_Operand1^.VAData)^ := $FFFF
      else if FRegisters[SRI].BuiltIn.Int <= 0 then
        PWord(_Operand1^.VAData)^ := 0
      else
        PWord(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int;
      Inc(_Operand1^.VAData, 2);
      {$ifdef Timecheck}EndTimecheck('va.i16');{$endif}
    end;
  end;

  procedure va_i32; inline;
  begin
    with TThoriumInstructionVA_I32(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].BuiltIn.Int >= $FFFFFFFF then
        PDWord(_Operand1^.VAData)^ := $FFFFFFFF
      else if FRegisters[SRI].BuiltIn.Int <= 0 then
        PDWord(_Operand1^.VAData)^ := 0
      else
        PDWord(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int;
      Inc(_Operand1^.VAData, 4);
      {$ifdef Timecheck}EndTimecheck('va.i32');{$endif}
    end;
  end;

  procedure va_i64; inline;
  begin
    with TThoriumInstructionVA_I64(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PQWord(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int and $7FFFFFFFFFFFFFFF;
      Inc(_Operand1^.VAData, 8);
      {$ifdef Timecheck}EndTimecheck('va.i64');{$endif}
    end;
  end;

  procedure va_i8s; inline;
  begin
    with TThoriumInstructionVA_I8S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].BuiltIn.Int >= 127 then
        PShortInt(_Operand1^.VAData)^ := 127
      else if FRegisters[SRI].BuiltIn.Int <= -128 then
        PShortInt(_Operand1^.VAData)^ := -128
      else
        PShortInt(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int;
      Inc(_Operand1^.VAData, 1);
      {$ifdef Timecheck}EndTimecheck('va.i8s');{$endif}
    end;
  end;

  procedure va_i16s; inline;
  begin
    with TThoriumInstructionVA_I16S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].BuiltIn.Int >= 32767 then
        PSmallInt(_Operand1^.VAData)^ := 32767
      else if FRegisters[SRI].BuiltIn.Int <= -32768 then
        PSmallInt(_Operand1^.VAData)^ := -32768
      else
        PSmallInt(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int;
      Inc(_Operand1^.VAData, 2);
      {$ifdef Timecheck}EndTimecheck('va.i16s');{$endif}
    end;
  end;

  procedure va_i32s; inline;
  begin
    with TThoriumInstructionVA_I32S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      if FRegisters[SRI].BuiltIn.Int >= 2147483647 then
        PLongInt(_Operand1^.VAData)^ := 2147483647
      else if FRegisters[SRI].BuiltIn.Int <= -2147483648 then
        PLongInt(_Operand1^.VAData)^ := -2147483648
      else
        PLongInt(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int;
      Inc(_Operand1^.VAData, 4);
      {$ifdef Timecheck}EndTimecheck('va.i32s');{$endif}
    end;
  end;

  procedure va_i64s; inline;
  begin
    with TThoriumInstructionVA_I64S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PInt64(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Int;
      Inc(_Operand1^.VAData, 8);
      {$ifdef Timecheck}EndTimecheck('va.i64s');{$endif}
    end;
  end;

  procedure va_f32; inline;
  begin
    with TThoriumInstructionVA_F32(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PSingle(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Float;
      Inc(_Operand1^.VAData, 4);
      {$ifdef Timecheck}EndTimecheck('va.f32');{$endif}
    end;
  end;

  procedure va_f64; inline;
  begin
    with TThoriumInstructionVA_F64(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PDouble(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Float;
      Inc(_Operand1^.VAData, 8);
      {$ifdef Timecheck}EndTimecheck('va.f64');{$endif}
    end;
  end;

  procedure va_f80; inline;
  begin
    with TThoriumInstructionVA_F80(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PExtended(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Float;
      Inc(_Operand1^.VAData, 10);
      {$ifdef Timecheck}EndTimecheck('va.f80');{$endif}
    end;
  end;

  procedure va_s; inline;
  begin
    with TThoriumInstructionVA_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      // Use move instead of assignment to avoid hassle with the stringmanager
      _Operand1 := FStack.GetTopStackEntry;
      // Move(FRegisters[SRI].BuiltIn.Str^, PString(_Operand1^.VAData)^, SizeOf(String));
      PPointer(_Operand1^.VAData)^ := nil;
      PString(_Operand1^.VAData)^ := FRegisters[SRI].BuiltIn.Str^;
      Inc(_Operand1^.VAData, SizeOf(Ptruint));
      {$ifdef Timecheck}EndTimecheck('va.s');{$endif}
    end;
  end;

  procedure va_x; inline;
  begin
    with TThoriumInstructionVA_X(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PPointer(_Operand1^.VAData)^ := FRegisters[SRI].Extended.Value;
      Inc(_Operand1^.VAData, SizeOf(ptruint));
      {$ifdef Timecheck}EndTimecheck('va.x');{$endif}
    end;
  end;

  procedure vastart_t; inline;
  begin
    with TThoriumInstructionVASTART_T(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etVarargs;
      _NewEntry^.VAData := GetMem(Length * SizeOf(TVarRec));
      _NewEntry^.VADataOrigin := _NewEntry^.VAData;
      _NewEntry^.VABuffer := GetMem(Floats * SizeOf(Extended) + (Length - (ToClear + Floats)) * SizeOf(TThoriumInteger));
      _NewEntry^.VABufferOrigin := _NewEntry^.VABuffer;
      _NewEntry^.VAToFree := GetMem(ToClear * SizeOf(TThoriumValue));
      _NewEntry^.VAToFreeOrigin := _NewEntry^.VAToFree;
      {$ifdef Timecheck}EndTimecheck('vastart.t');{$endif}
    end;
  end;

  procedure vat_f; inline;
  begin
    with TThoriumInstructionVAT_F(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PExtended(_Operand1^.VABuffer)^ := FRegisters[SRI].BuiltIn.Float;
      with PVarRec(_Operand1^.VAData)^ do
      begin
        VType := vtExtended;
        VExtended := _Operand1^.VABuffer;
      end;
      Inc(_Operand1^.VABuffer, SizeOf(Extended));
      Inc(_Operand1^.VAData, SizeOf(TVarRec));
      {$ifdef Timecheck}EndTimecheck('vat.f');{$endif}
    end;
  end;

  procedure vat_i; inline;
  begin
    with TThoriumInstructionVAT_I(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      PThoriumInteger(_Operand1^.VABuffer)^ := FRegisters[SRI].BuiltIn.Int;
      with PVarRec(_Operand1^.VAData)^ do
      begin
        VType := vtInt64;
        VInt64 := _Operand1^.VABuffer;
      end;
      Inc(_Operand1^.VABuffer, SizeOf(TThoriumInteger));
      Inc(_Operand1^.VAData, SizeOf(TVarRec));
      {$ifdef Timecheck}EndTimecheck('vat.i');{$endif}
    end;
  end;

  procedure vat_s; inline;
  begin
    with TThoriumInstructionVAT_S(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      with PVarRec(_Operand1^.VAData)^ do
      begin
        VType := vtAnsiString;
        VAnsiString := PPointer(FRegisters[SRI].BuiltIn.Str)^;
      end;
      //Move(FRegisters[SRI], _Operand1^.VAToFree[0], SizeOf(TThoriumValue));
      //Inc(_Operand1^.VAToFree);
      Inc(_Operand1^.VAData, SizeOf(TVarRec));
      {$ifdef Timecheck}EndTimecheck('vat.s');{$endif}
    end;
  end;

  procedure vat_x; inline;
  begin
    with TThoriumInstructionVAT_X(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.GetTopStackEntry;
      with PVarRec(_Operand1^.VAData)^ do
      begin
        VType := vtPointer;
        VPointer := FRegisters[SRI].Extended.Value;
      end;
      Move(FRegisters[SRI], _Operand1^.VAToFree[0], SizeOf(TThoriumValue));
      Inc(_Operand1^.VAToFree);
      Inc(_Operand1^.VAData, SizeOf(TVarRec));
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
      _NewEntry^._Type := etStackFrame;
      _NewEntry^.ReturnAddress := FCurrentInstructionIdx;
      _NewEntry^.PreviousStackFrame := FCurrentStackFrame;
      _NewEntry^.ReturnModule := THORIUM_MODULE_INDEX_CURRENT;
      _NewEntry^.RegisterDumpRange := HRI;
      _NewEntry^.RetVals := RetVal;
      _NewEntry^.Params := Parameters;
      _NewEntry^.DropResult := False;
      GetMem(_NewEntry^.RegisterDump, SizeOf(TThoriumValue)*(HRI+1));
      Move(FRegisters[0], _NewEntry^.RegisterDump^, SizeOf(TThoriumValue)*(HRI+1));

      FCurrentStackFrame := FStack.FCount-1;
      Inc(FCurrentInstruction, (EntryPoint-1) - FCurrentInstructionIdx);
      Inc(FCurrentInstructionIdx, (EntryPoint-1) - FCurrentInstructionIdx);
      {$ifdef Timecheck}EndTimecheck('call');{$endif}
    end;
  end;

  procedure call_d; inline;
  begin
    with TThoriumInstructionCALL_D(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(0, FCurrentStackFrame);
      if KeepResult <> 0 then
        _Operand1^.DropResult := False;
      _Operand1^.RetVals := RetVal;
      _Operand1^.Params := Parameters + _Operand1^.Params + Pops;
      Inc(FCurrentInstruction, (EntryPoint-1) - FCurrentInstructionIdx);
      Inc(FCurrentInstructionIdx, (EntryPoint-1) - FCurrentInstructionIdx);
      {$ifdef Timecheck}EndTimecheck('call.d');{$endif}
    end;
  end;

  procedure fcall; inline;
  begin
    with TThoriumInstructionFCALL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _NewEntry := FStack.Push;
      _NewEntry^._Type := etStackFrame;
      _NewEntry^.ReturnAddress := FCurrentInstructionIdx;
      _NewEntry^.PreviousStackFrame := FCurrentStackFrame;
      _NewEntry^.ReturnModule := FCurrentModuleIdx;
      _NewEntry^.RegisterDumpRange := HRI;
      _NewEntry^.RetVals := RetVal;
      _NewEntry^.Params := Parameters;
      _NewEntry^.DropResult := False;
      GetMem(_NewEntry^.RegisterDump, SizeOf(TThoriumValue)*HRI);
      Move(FRegisters[0], _NewEntry^.RegisterDump^, SizeOf(TThoriumValue)*HRI);

      FCurrentInstructionIdx := EntryPoint - 1;
      FCurrentStackFrame := FStack.FCount-1;
      FCurrentModuleIdx := ModuleIndex;
      FCurrentModule := TThoriumModule(FThorium.FModules[FCurrentModuleIdx]);
      FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
      {$ifdef Timecheck}EndTimecheck('fcall');{$endif}
    end;
  end;

  procedure xcall; inline;
  begin
    with TThoriumInstructionXCALL(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      TThoriumHostFunctionBase(FunctionRef).CallFromVirtualMachine(Self);
      {$ifdef Timecheck}EndTimecheck('ecall');{$endif}
    end;
  end;

  procedure xcall_m; inline;
  begin
    with TThoriumInstructionXCALL_M(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      TThoriumHostMethodBase(MethodRef).CallFromVirtualMachine(TObject(FRegisters[RTTIValueRegister].Extended.Value), Self);
      // ThoriumFreeValue(FRegisters[RTTIValueRegister]);
      {$ifdef Timecheck}EndTimecheck('ecall');{$endif}
    end;
  end;

  procedure ret; inline;
  begin
    with TThoriumInstructionRET(FCurrentInstruction^) do
    begin
      {$ifdef Timecheck}BeginTimecheck;{$endif}
      _Operand1 := FStack.FastGetStackEntry(0, FCurrentStackFrame);
      FCurrentStackFrame := _Operand1^.PreviousStackFrame;
      FCurrentInstructionIdx := _Operand1^.ReturnAddress;
      if _Operand1^.ReturnModule <> THORIUM_MODULE_INDEX_CURRENT then
      begin
        FCurrentModuleIdx := _Operand1^.ReturnModule;
        FCurrentModule := TThoriumModule(FThorium.FModules[FCurrentModuleIdx]);
      end;
      FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
      if _Operand1^.RegisterDump <> nil then
      begin
        Move(_Operand1^.RegisterDump^, FRegisters[0], SizeOf(TThoriumValue)*(_Operand1^.RegisterDumpRange+1));
        FreeMem(_Operand1^.RegisterDump);
      end;
      if (_Operand1^.RetVals = 1) then
      begin
        if (_Operand1^.DropResult) then
        begin
          // Pop the result away
          FStack.Pop(2+_Operand1^.Params, False);
        end
        else
        begin
          // Assign the value immediatedly above the stack frame to the slot
          // reserved for the return value.
          if _Operand1^.Params = 0 then
          begin
            // If there are no parameters, we can directly overwrite the stack
            // frame.
            _Operand1^ := _Operand1[1];
            // _Operand1[1]._Type := etNull; // To avoid freeing by the pop
            FStack.Pop(1, False);
          end
          else
          begin
            // Otherwise we just overwrite the slot of the last parameter
            // ThoriumFreeValue(_Operand1[-(_Operand1^.Params)].Value);
            _Operand1[-(_Operand1^.Params)] := _Operand1[1];
            // Again, avoid freeing by the pop
            // _Operand1[1]._Type := etNull;
            FStack.Pop(1+_Operand1^.Params, False);
          end;
        end;
      end
      else
        FStack.Pop(1+_Operand1^.Params, False);
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
    tiFNCE: fnce;
    tiCOPYR_S: copyr_s;
    tiCOPYR_ST: copyr_st;
    tiCOPYR_FS: copyr_fs;
    tiCOPYS_ST: copys_st;
    tiCOPYFS: copyfs;
    tiCOPYS: copys;
    tiCOPYR: copyr;
    tiMOVES_S: moves_s;
    tiMOVER_S: mover_s;
    tiMOVER_ST: mover_st;
    tiMOVER: mover;
    tiMOVES: moves;
    tiMOVEST: movest;
    tiMOVER_FS: mover_fs;
    tiMOVEFS: movefs;
    tiMOVES_ST: moves_st;
    tiPOP_S: pop_s;
    tiCLR: clr;
    tiCASTIF: castif;
    tiCASTIE: castie;
    tiCASTFE: castfe;
    tiCASTSE: castse;
    tiCASTEI: castei;
    tiCASTEF: castef;
    tiCASTES: castes;
    tiCASTE: caste;
    tiCMPI: cmpi;
    tiCMPIF: cmpif;
    tiCMPIE: cmpie;
    tiCMPF: cmpf;
    tiCMPFI: cmpfi;
    tiCMPFE: cmpfe;
    tiCMPS: cmps;
    tiCMPSE: cmpse;
    tiCMPE: cmpe;
    tiCMPEI: cmpei;
    tiCMPEF: cmpef;
    tiCMPES: cmpes;
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
    tiINCI_S: inci_s;
    tiINCF_S: incf_s;
    tiINCI_FS: inci_fs;
    tiINCF_FS: incf_fs;
    tiINCI: inci;
    tiINCF: incf;
    tiDECI_S: deci_s;
    tiDECF_S: decf_s;
    tiDECI_FS: deci_fs;
    tiDECF_FS: decf_fs;
    tiDECI: deci;
    tiDECF: decf;
    tiXFGET: xfget;
    tiXFSET: xfset;
    tiXIGET: xiget;
    tiXISET: xiset;
    tiXPGET: xpget;
    tiXPSET: xpset;
    tiXCT: xct;
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
  else
    //Assert(False, 'Unknown opcode');
    raise EThoriumRuntimeException.CreateFmt('Unknown opcode: %d (%s)', [Ord(FCurrentInstruction^.Instruction), GetEnumName(TypeInfo(TThoriumInstructionCode), Ord(FCurrentInstruction^.Instruction))]);
  end;
  Inc(FCurrentInstruction);
  Inc(FCurrentInstructionIdx);
  //System.Inc(FCurrentInstructionIdx);
end;

function TThoriumVirtualMachine.GetStack: TThoriumStack;
begin
  Result := FStack;
end;

procedure TThoriumVirtualMachine.Execute(StartModuleIndex: Integer; StartInstruction: Integer; CreateDefaultStackFrame: Boolean = True);
var
  Frame: PThoriumStackEntry;
  {$ifdef DebugToConsole}
  OldIdx: Integer;
  {$endif}
begin
  FCurrentInstructionIdx := StartInstruction;
  FCurrentModuleIdx := StartModuleIndex;
  FCurrentModule := FThorium.Module[FCurrentModuleIdx];
  if (FCurrentInstructionIdx > FCurrentModule.FInstructions.Count - 1) or
    (FCurrentInstructionIdx < 0) then
    raise EThoriumRuntimeException.Create('Instruction index out of bounds.');
  if CreateDefaultStackFrame and (FCurrentStackFrame < 0) then
  begin
    FCurrentStackFrame := FStack.EntryCount;
    Frame := FStack.Push;
    Frame^._Type := etStackFrame;
    Frame^.PreviousStackFrame := -1;
    Frame^.ReturnModule := -1;
    Frame^.ReturnAddress := THORIUM_JMP_EXIT;
    Frame^.RegisterDumpRange := 0;
    Frame^.RegisterDump := nil;
    Frame^.RetVals := 0;
    Frame^.Params := 0;
    Frame^.DropResult := False;
  end;
  try
    FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
    repeat
      {$ifdef Stackdump}
      Write('$', IntToHex(FCurrentInstructionIdx, 8), ': ');
      {$endif}
      {$ifdef InstructionDump}
      {$ifndef Stackdump}Write('$', IntToHex(FCurrentInstructionIdx, 8), ': '); {$endif}WriteLn(ThoriumInstructionToStr(FCurrentInstruction^));
      {$endif}
      ExecuteInstruction;
      {$ifdef Stackdump}
        {$ifdef InstructionDump}
        Write('  ');
        {$endif}
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

procedure TThoriumVirtualMachine.DumpStack;
var
  I: Integer;
  Entry: PThoriumStackEntry;
begin
  Write('[[ ');
  Entry := FStack.FEntries;
  for I := 0 to FStack.FCount - 1 do
  begin
    case Entry^._Type of
      etStackFrame: Write('(SF)');
      etValue:
      begin
        case Entry^.Value._Type of
          vtBuiltIn:
          begin
            case Entry^.Value.BuiltIn._Type of
              btInteger: Write('(V(I):', Format('%.4d', [Entry^.Value.BuiltIn.Int]), ')');
              btFloat: Write('(V(F):', Format('%2.2f', [Entry^.Value.BuiltIn.Float]), ')');
              btString:
              begin
                if Entry^.Value.BuiltIn.Str = nil then
                  Write('(V(S):)')
                else
                begin
                  Write('(V(S):', Format('%.4s', [Entry^.Value.BuiltIn.Str^]), ')');
                end;
              end;
            end;
          end;
          vtExtendedType: Write('V(', ThoriumTypeName(Entry^.Value), '):', IntToHex(ptruint(Entry^.Value.Extended.Value), SizeOf(ptruint)*2));
        end;
      end;//Write('(V:'+ThoriumTypeName(Entry^.Value)+')');
      etVarargs: Write('(VA)');
      etNull: Write('(NULL)');
    else
      Write('(ERR)');
    end;
    Write(' ');
    Inc(Entry{, SizeOf(TThoriumStackEntry)});
  end;
  Write(']]');
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

procedure TThoriumDebuggingVirtualMachine.Execute(StartModuleIndex: Integer;
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
  FCurrentModuleIdx := StartModuleIndex;
  FCurrentModule := FThorium.Module[FCurrentModuleIdx];
  if (FCurrentInstructionIdx > FCurrentModule.FInstructions.Count - 1) or
    (FCurrentInstructionIdx < 0) then
    raise EThoriumRuntimeException.Create('Instruction index out of bounds.');
  if CreateDefaultStackFrame and (FCurrentStackFrame < 0) then
  begin
    FCurrentStackFrame := FStack.EntryCount;
    Frame := FStack.Push;
    Frame^._Type := etStackFrame;
    Frame^.PreviousStackFrame := -1;
    Frame^.ReturnModule := -1;
    Frame^.ReturnAddress := THORIUM_JMP_EXIT;
    Frame^.RegisterDumpRange := 0;
    Frame^.RegisterDump := nil;
    Frame^.RetVals := 0;
    Frame^.Params := 0;
    Frame^.DropResult := False;
  end;
  try
    FCurrentInstruction := FCurrentModule.FInstructions.Instruction[FCurrentInstructionIdx];
    repeat
      {$ifdef Stackdump}
      Write('$', IntToHex(FCurrentInstructionIdx, 8), ': ');
      {$endif}
      {$ifdef InstructionDump}
      {$ifndef Stackdump}
      Write('$', IntToHex(FCurrentInstructionIdx, 8), ': ');{$endif}Write(ThoriumInstructionToStr(FCurrentInstruction^), ' ');
      {$endif}
      ExecuteInstruction;
      {$ifdef Stackdump}
        {$ifdef InstructionDump}
        Write('  ');
        {$endif}
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
{ TThorium }

constructor TThorium.Create;
begin
  inherited Create;
  FModules := TFPList.Create;
  FHostLibraries := TFPList.Create;
  //FVirtualMachines := TList.Create;
  FVirtualMachine := nil;
  FOnCompilerOutput := nil;
  FOnRequireModule := nil;
  FOnOpenModule := nil;
  FAnonymousID := 0;
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

function TThorium.DoRequireModule(const Name: String; NeededHash: PThoriumHash = nil): TThoriumModule;
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
    Result := LoadModuleFromFile(Name, NeededHash);
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
  LowName: String;
  I: Integer;
  List: PPointerList;
begin
  LowName := LowerCase(Name);
  List := FHostLibraries.List;
  for I := 0 to FHostLibraries.Count - 1 do
  begin
    Result := TThoriumLibrary(List^[I]);
    if Result.FName = LowName then
      Exit;
  end;
  Result := nil;
end;

function TThorium.FindModule(const Name: String; AllowLoad: Boolean = True): TThoriumModule;
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    Result := TThoriumModule(FModules[I]);
    if Result.FName = Name then
      Exit;
  end;
  if AllowLoad then
  begin
    Result := DoRequireModule(Name);
    Exit;
  end;
  Result := nil;
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

function TThorium.LoadModuleFromFile(AModuleName: String; NeededHash: PThoriumHash = nil): TThoriumModule;

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
                Result := LoadModuleFromStream(SourceFS, StripExtension(ChangedFileName), nil);
                Result.FSourceFile := AModuleName + '.tss';
              end;
            finally
              SourceFS.Free;
            end;
          except

          end;
        end;
        Result := LoadModuleFromStream(FS, StripExtension(ChangedFileName), NeededHash);
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
          Result := LoadModuleFromStream(FS, StripExtension(ChangedFileName), NeededHash);
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
        Result := LoadModuleFromStream(FS, StripExtension(ChangedFileName), NeededHash);
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

function TThorium.LoadModuleFromStream(AStream: TStream; AName: String = ''; NeededHash: PThoriumHash = nil): TThoriumModule;
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
    if (not Result.CompileFromStream(AStream)) or (not Result.Compiled) then
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
STACKENTRY_VALUE_OFFSET := ptruint(@TestEntry.Value) - ptruint(@TestEntry);
VALUE_BUILTIN_OFFSET := ptruint(@TestEntry.Value.BuiltIn) - ptruint(@TestEntry.Value);
BUILTIN_VALUE_OFFSET := ptruint(@Testentry.Value.BuiltIn.Int) - ptruint(@TestEntry.Value.BuiltIn);
STACKENTRY_VADATA_OFFSET := ptruint(@TestEntry.VADataOrigin) - ptruint(@TestEntry);

{$ifdef HookSIGUSR1}
  FpSignal(Baseunix.SIGUSR1, @HandleSigUSR1);
{$endif}

end.
