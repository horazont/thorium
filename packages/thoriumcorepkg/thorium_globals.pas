(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thorium_globals.pas
** Last update: 2010-03-14
This file is part of the Thorium Scripting Language Project.
For more information and notes see the thorium.pas file delivered with the
Thorium Scripting Language Project main package.

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
*******************************************************************************)
unit Thorium_Globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo;

{%REGION 'Versioning'}
const
  THORIUM_FILE_VERSION = 19;

  THORIUM_MAJOR_VERSION : Word = 1;
  THORIUM_MINOR_VERSION : Word = 0;
  THORIUM_RELEASE_VERSION : Word = 6;
{%ENDREGION}

(*
   Region: Various constants
   Declarations: THORIUM_NUMBER_FORMAT, THORIUM_STACK_SCOPE_*,
                 THORIUM_MODULE_INDEX_CURRENT, THORIUM_RTTI_METHOD_BIT,
                 THORIUM_JMP_INVALID, THORIUM_JMP_EXIT,
                 THORIUM_REGISTER_*, THORIUM_STATE_*
   Description: The constants in this section are mainly used by the code areals
                in the compiler where the code is generated. These constants
                are used to make sure compiler and virtual machine interpret
                some values the same way.
                                                                              *)
{%REGION 'Various constants' /fold}
  THORIUM_NUMBER_FORMAT : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

  THORIUM_STACK_SCOPE_NOSCOPE = -2;
  THORIUM_STACK_SCOPE_FROMTOP = -1;
  THORIUM_STACK_SCOPE_MODULEROOT = 0;
  THORIUM_STACK_SCOPE_PARAMETERS = 1;
  THORIUM_STACK_SCOPE_LOCAL = 2;

  THORIUM_MODULE_INDEX_CURRENT = -1;

  THORIUM_RTTI_METHOD_BIT = $8000000000000000;

  THORIUM_JMP_INVALID = -3;
  THORIUM_JMP_EXIT = -2;
  THORIUM_RETURN_EXIT = THORIUM_JMP_EXIT;

  THORIUM_STATE_EQUAL =     $0001;
  THORIUM_STATE_GREATER =   $0002;
  THORIUM_STATE_LESS =      $0004;
  THORIUM_STATE_ZERO =      $0008;
  THORIUM_STATE_NEG =       $0010;

  THORIUM_STATE_COMPFLAGS = THORIUM_STATE_EQUAL or THORIUM_STATE_LESS or THORIUM_STATE_GREATER;

  THORIUM_STATE_ACCUM_NOTEQUAL = 0;
  THORIUM_STATE_ACCUM_EQUAL = THORIUM_STATE_EQUAL;
  THORIUM_STATE_ACCUM_LESS = THORIUM_STATE_LESS;
  THORIUM_STATE_ACCUM_LESSEQUAL = THORIUM_STATE_LESS or THORIUM_STATE_EQUAL;
  THORIUM_STATE_ACCUM_GREATER = THORIUM_STATE_GREATER;
  THORIUM_STATE_ACCUM_GREATEREQUAL = THORIUM_STATE_GREATER or THORIUM_STATE_EQUAL;

  THORIUM_TYPE_NAME_INTEGER = 'int';
  THORIUM_TYPE_NAME_STRING = 'string';
  THORIUM_TYPE_NAME_FLOAT = 'float';

  CPU_SIZE_FACTOR = {$ifdef CPU32}2{$else}{$ifdef CPU64}1{$else}{$error Unknown CPU}{$endif}{$endif};
{%ENDREGION}

(*
   Region: Native call types
   Description: These types are used by the native call implementation.
                                                                              *)
{%REGION 'Native call types' /fold}
type
  {$PACKENUM 2}
  TThoriumNativeCallInstructionCode = (
    ccData,
    ccPutRef, ccPutRefRef, ccPutDataRegInt, ccPutDataRegXMM, {ccPutDataRegMMX,}
    ccPutDataStack, ccPutLargeDataStack,
    ccCallNone, ccCallRetValue, ccCallRetRef, ccCallRetRefRef, ccCallRetMMX, ccCallRetExtended,
    ccClearStack,
    ccExit,
    ccNone);

  TThoriumNativeRegisterTarget = (rtInt, rtXMM, rtMMX);

type
  TThoriumNativeCallingConvention = (ncRegister, ncStdCall, ncCDecl);

  TThoriumNativeCallInstruction = packed record
    Instruction: TThoriumNativeCallInstructionCode;
    Data1, Data2: SizeInt;
  end;
  TThoriumNativeCallInstructions = array of TThoriumNativeCallInstruction;
  PThoriumNativeCallInstruction = ^TThoriumNativeCallInstruction;

  TThoriumNativeCallValueMode = (vmInt32, vmInt64, vmPointer, vmFloat, vmString);
  TThoriumNativeCallRefMode = (rmRef, rmNormal, rmDeref);
  TThoriumNativeCallFloatMode = (fmAsDouble, fmAsSingle, fmAsExtended);
  TThoriumNativeCallDirection = (ncdPass, ncdReturn);

(*const
  NativeCallInstruction : array [TThoriumNativeCallValueMode, TThoriumNativeCallRefMode, TThoriumNativeCallDirection] of TThoriumNativeCallInstructionCode = (
                {rmRef}                     {rmNormal}                      {rmDeref}
    {vmInt32}   ((ccIntRef, ccNone),        (ccInt, ccCallRetInt),          (ccIntDeref, ccNone)),
    {vmInt64}   ((ccInt64Ref, ccNone),      (ccInt64, ccCallRetInt64),      (ccInt64Deref, ccNone)),
    {vmPointer} ((ccPointerRef, ccNone),    (ccPointer, ccCallRetPointer),  (ccPointerDeref, ccNone)),
    {vmFloat}   ((ccDoubleRef, ccNone),     (ccDouble, ccCallRetDouble),    (ccDoubleDeref, ccNone)),
    {vmString}  ((ccStringRef, ccNone),     (ccString, ccCallRetString),    (ccStringDeref, ccNone))
  );
*)

{%ENDREGION}

(*
   Region: Registers
   Description: In this section the amount of registers available in the Thorium
                virtual machine is declared. You should not change them if you
                are not experiencing serious problems about performance and
                compilability of code.
                                                                              *)
{%REGION 'Registers' /fold}
const
  // The amount of cache registers must be at least 6 (currently). More won't
  // bring any benefit, less will bring up dragons.
  THORIUM_REGISTER_C_COUNT = 6;
  // The amount of expression registers determine the maximum nesting level of
  // expressions. Each operator precedence level (+-, /*) and each nesting with
  // brackets cost one register.
  THORIUM_REGISTER_EXP_COUNT = 512;
  // The full amount of registers must not exceed 65535 and it is recommended
  // to keep the full amount below 512 to save memory and to improve the
  // performance during in-script function calls.
  THORIUM_REGISTER_COUNT = THORIUM_REGISTER_C_COUNT + THORIUM_REGISTER_EXP_COUNT;

  // Expression register
  THORIUM_REGISTER_EXP_MIN = $0000;
  THORIUM_REGISTER_EXP_MAX = THORIUM_REGISTER_EXP_MIN + (THORIUM_REGISTER_EXP_COUNT - 1);
  // Cache register
  THORIUM_REGISTER_C_MIN = THORIUM_REGISTER_EXP_MAX + 1;
  THORIUM_REGISTER_C_MAX = THORIUM_REGISTER_C_MIN + (THORIUM_REGISTER_C_COUNT - 1);
  THORIUM_REGISTER_INVALID = $FFFF;

  // Some often used values
  THORIUM_REGISTER_C1 = THORIUM_REGISTER_C_MIN;
  THORIUM_REGISTER_C2 = THORIUM_REGISTER_C_MIN + 1;
  THORIUM_REGISTER_C3 = THORIUM_REGISTER_C_MIN + 2;
  THORIUM_REGISTER_C4 = THORIUM_REGISTER_C_MIN + 3;
  THORIUM_REGISTER_C5_RUNTIME = THORIUM_REGISTER_C_MIN + 4;
  THORIUM_REGISTER_C6_RUNTIME = THORIUM_REGISTER_C_MIN + 5;

  THORIUM_REGISTER_RUNTIME_MIN = THORIUM_REGISTER_C5_RUNTIME;
  THORIUM_REGISTER_RUNTIME_MAX = THORIUM_REGISTER_C6_RUNTIME;

  THORIUM_REGISTER_MASK_BLOCK_SIZE = 16; // One word. DO NOT CHANGE WITHOUT CHANGING THE TYPE DEFINITION BELOW
  THORIUM_REGISTER_MASK_SIZE = (THORIUM_REGISTER_COUNT div THORIUM_REGISTER_MASK_BLOCK_SIZE) + Integer(THORIUM_REGISTER_COUNT mod THORIUM_REGISTER_MASK_BLOCK_SIZE > 0);

type
  TThoriumRegisterMaskBlock = Word; // Word. DO NOT CHANGE WITHOUT CHANGING THE CONSTANT ABOVE.
  TThoriumRegisterMask = array [0..THORIUM_REGISTER_MASK_SIZE-1] of TThoriumRegisterMaskBlock;
{%ENDREGION}

(*
   Region: Thorium types and enums
   Description: This section defines some types which represent the built-in
                types of Thorium and some enums which are used in identifier
                tables and the Thorium values.
                                                                              *)
{%REGION 'Thorium types and enums' /fold}
type
  TThoriumInteger = Int64;
  PThoriumInteger = ^TThoriumInteger;
  TThoriumFloat = Double;
  PThoriumFloat = ^TThoriumFloat;
  TThoriumString = AnsiString;
  PThoriumString = PString;
  TThoriumHostObject = Pointer;
  TThoriumSizeInt = ptrint;

  TThoriumHash = array [0..7] of Word;
  PThoriumHash = ^TThoriumHash;

  TThoriumBuiltInType = (btNil = 0, btUnknown = 1, btInteger = 2, btFloat = 3,
    btString = 4, btArray = 5);
  TThoriumArrayKind = (akStatic, akDynamic);
  TThoriumRegisterID = Word;
  TThoriumRegisterKind = (trC, trEXP);
  TThoriumVisibilityLevel = (vsPrivate, vsPublic);
  TThoriumValueType = (vtBuiltIn, vtExtendedType, vtFunction,
    vtHostFunction, vtHostMethod);


  TThoriumTableEntryType = (ttGlobal, ttLocal, ttParameter,
    ttLocalRegisterVariable, ttGlobalCallable, ttHostCallable,
    ttLibraryProperty, ttLibraryConstant, ttType);
  TThoriumTableEntryTypes = set of TThoriumTableEntryType;
  TThoriumQualifiedIdentifierKind = (ikType, ikVariable, ikComplex,
    ikUndeclared, ikNoFar, ikPrototypedFunction, ikLibraryProperty);
  TThoriumQualifiedIdentifierKinds = set of TThoriumQualifiedIdentifierKind;

  TThoriumCompilerFlag = (cfOptimize, cfOnlyGlobalFunctions, cfAppend);
  TThoriumCompilerFlags = set of TThoriumCompilerFlag;

  TThoriumDebugEvent = (deException, deBreakpointLine, deBreakpointInstruction,
    deStepLine, deStepInstruction);
  TThoriumDebuggerStepMode = (smInstruction, smLine, smInto);
const
  THORIUM_IDENTIFIER_KIND_NAMES : array [TThoriumQualifiedIdentifierKind] of String = (
    'type',
    'variable',
    'complex expression',
    'undeclared identifier',
    '',
    'prototyped function',
    'library property'
  );
{%ENDREGION}

(*
   Region: Module header
   Description: The constants and types in this section are used to create and
                validate the file header of a saved binary module.
                                                                              *)
{%REGION 'Module header' /fold}
const
  {$ifdef ENDIAN_LITTLE}
  THORIUM_MODULE_HEADER_ID : QWord = $1A0A0D4D4C536854;
  {$else}
  THORIUM_MODULE_HEADER_ID : QWord = $5468534C4D0D0A1A;
  {$endif}

type
  TThoriumModuleHeader = packed record
    ID: QWord;
    FileVersion: Cardinal;
    HeaderSize: Cardinal;
    SourceFileNameLength: Cardinal; // Unused and MUST be zero
    RequireCount: Cardinal;
    LibraryCount: Cardinal;
    FunctionExportCount: Cardinal;
    VariableExportCount: Cardinal;
    StringCount: Cardinal;
    HostTypeDependencyCount: Cardinal;
    HostTypeRelocationCount: Cardinal;
    HostFuncDependencyCount: Cardinal;
    HostFuncRelocationCount: Cardinal;
    LibPropDependencyCount: Cardinal;
    LibPropRelocationCount: Cardinal;
    Hash: TThoriumHash;
    SourceHash: TThoriumHash;
    SourceLength: Cardinal;
    ZippedContent: Byte;
  end;
  TThoriumModuleHashBase = packed record
    FileVersion: Cardinal;
    RequireCount: Cardinal;
    LibraryCount: Cardinal;
    FunctionExportCount: Cardinal;
    VariableExportCount: Cardinal;
    StringCount: Cardinal;
    HostTypeDependencyCount: Cardinal;
    HostTypeRelocationCount: Cardinal;
    HostFuncDependencyCount: Cardinal;
    HostFuncRelocationCount: Cardinal;
    LibPropDependencyCount: Cardinal;
    LibPropRelocationCount: Cardinal;
    InstructionCount: Cardinal;
    CompilationTimestamp: Double;
    RandomFactor: Cardinal;
    InstructionHash: TThoriumHash;
  end;
  PThoriumModuleHashBase = ^TThoriumModuleHashBase;
{%ENDREGION}

(*
   Region: Instructions
   Description: This region declares the various instruction records which
                contain the information the virtual machine needs to execute a
                specific instruction.
                                                                              *)
{%REGION 'Instructions' /fold}
type
  TThoriumInstructionAddress = LongInt;
  PThoriumInstructionAddress = ^TThoriumInstructionAddress;

  // ! REMEMBER ! to also increase the file format version by one, since the
  // instruction indicies have changed when you change something below.
  TThoriumInstructionCode = (
    tiINT_S, tiINT, tiINTB,
    tiFLT_S, tiFLT,
    tiSTR_S, tiSTRL_S, tiSTR, tiSTRL,
    tiEXT_S, tiEXT,
    tiFNC, tiXFNC, tiXMETH,
    tiMOVER_G, tiCOPYR_G, tiMOVEG, tiCOPYG,
    tiMOVER_FG, tiCOPYR_FG, tiMOVEFG, tiCOPYFG,
    tiMOVER_L, tiCOPYR_L, tiMOVEL, tiCOPYL,
    tiMOVER_P, tiCOPYR_P, tiMOVEP, tiCOPYP,
    tiCOPYR_ST, tiCOPYR,
    tiMOVER_ST, tiMOVER, tiMOVEST,
    tiPOP_S, tiSTACKHINT,
    tiCLR,
    tiCASTIF, tiCASTIE, tiCASTFE, tiCASTSE, tiCASTEI, tiCASTEF, tiCASTES, tiCASTE,
    tiCMPI, tiCMPIF, tiCMPIE, tiCMPF, tiCMPFI, tiCMPFE, tiCMPS, tiCMPSE, tiCMPE, tiCMPEI, tiCMPEF, tiCMPES,
    tiEVALI,
    tiADDI, tiADDF, tiADDS,
    tiSUBI, tiSUBF,
    tiMULI, tiMULF,
    tiDIVI, tiDIVF,
    tiNEGI, tiNEGF,
    tiNOT, tiBNOT,
    tiMOD,
    tiAND,
    tiOR,
    tiXOR,
    tiSHL,
    tiSHR,
    tiINCI, tiINCF,
    tiDECI, tiDECF,
    tiXPGET, tiXPSET,
    tiXFGET, tiXFSET, tiXSFGET, tiXSFSET,
    tiXIGET, tiXISET,
    tiXCT,
    tiX2N, tiN2X, tiCLRN,
    tiVASTART, tiVA_I8, tiVA_I16, tiVA_I32, tiVA_I64,
    tiVA_I8S, tiVA_I16S, tiVA_I32S, tiVA_I64S,
    tiVA_F32, tiVA_F64, tiVA_F80, tiVA_S, tiVA_X,
    tiVASTART_T, tiVAT_F, tiVAT_I, tiVAT_S, tiVAT_X, tiVAFINISH,
    tiTOSTR_I, tiTOSTR_F,
    tiREF, tiDEREF,
    tiLEN_S, tiLEN_A,
    tiUPDATE_X,
    tiJMP,
    tiJE, tiJNE, tiJGT, tiJGE, tiJLT, tiJLE,
    tiCALL,
    tiCALL_D,
    tiFCALL,
    tiXCALL,
    tiXCALL_M,
    tiRET,
    tiNOOP,
    tiEmbeddedHint
  );
  // ! REMEMBER ! to also increase the file format version by one, since the
  // instruction indicies have changed when you change something above.

const
  THORIUM_JMP_INSTRUCTIONS = [tiJMP, tiJE, tiJNE, tiJGT, tiJGE, tiJLT, tiJLE];

type
  // Thorium instructions need to be packed to word-size to make them
  // equal-sized. The smallest chunk of data in an instruction is word-sized.
  {$PACKRECORDS 2}

  TThoriumInstructionREG = record
    Instruction: TThoriumInstructionCode;
    Reserved: array [0..11] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;
  
  TThoriumInstructionINT_S = record
    Instruction: TThoriumInstructionCode;
    Value: Int64;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionINT = record
    Instruction: TThoriumInstructionCode;
    Value: Int64;
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionINTB = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Kind: Cardinal;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionFLT_S = record
    Instruction: TThoriumInstructionCode;
    Value: Double;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionFLT = record
    Instruction: TThoriumInstructionCode;
    Value: Double;
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSTR_S = record
    Instruction: TThoriumInstructionCode;
    Reserved: array [0..11] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSTRL_S = record
    Instruction: TThoriumInstructionCode;
    Index: LongInt;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSTR = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSTRL = record
    Instruction: TThoriumInstructionCode;
    Index: LongInt;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionEXT_S = record
    Instruction: TThoriumInstructionCode;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionEXT = record
    Instruction: TThoriumInstructionCode;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionFNC = record
    Instruction: TThoriumInstructionCode;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    FunctionRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    FunctionRef: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    FunctionRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXFNC = record
    Instruction: TThoriumInstructionCode;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    FunctionRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    FunctionRef: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    FunctionRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXMETH = record
    Instruction: TThoriumInstructionCode;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    MethodRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    MethodRef: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    MethodRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    TRI: Word;
    ERI: Word;
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVER_G = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR_G = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVEG = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYG = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVER_FG = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    ModuleRef: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..4] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR_FG = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    ModuleRef: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..4] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVEFG = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    ModuleRef: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..4] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYFG = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    ModuleRef: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ModuleRefPointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..4] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVER_L = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR_L = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVEL = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYL = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVER_P = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR_P = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVEP = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYP = record
    Instruction: TThoriumInstructionCode;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR_S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR_ST = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR_FS = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    ModuleIndex: LongInt;
    Offset: LongInt;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;
  
  TThoriumInstructionCOPYS_ST = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYFS = record
    Instruction: TThoriumInstructionCode;
    ModuleIndex: LongInt;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYR = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCOPYS = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVES_S = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;
  
  TThoriumInstructionMOVER_S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVER_ST = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVER = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVES = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVEST = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVER_FS = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    ModuleIndex: LongInt;
    Offset: LongInt;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVEFS = record
    Instruction: TThoriumInstructionCode;
    ModuleIndex: LongInt;
    Offset: LongInt;
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOVES_ST = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionPOP_S = record
    Instruction: TThoriumInstructionCode;
    Amount: Cardinal;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSTACKHINT = record
    Instruction: TThoriumInstructionCode;
    Amount: Cardinal;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCLR = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
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

  TThoriumInstructionCASTIF = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCASTIE = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCASTFE = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCASTSE = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCASTEI = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCASTEF = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCASTES = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCASTE = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionOperator = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPIF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPIE = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPFI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPFE = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPS = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPSE = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPE = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPEI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPEF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCMPES = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionEVALI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionADDI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionADDF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;
  
  TThoriumInstructionADDS = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSUBI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSUBF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMULI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMULF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDIVI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDIVF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionNEGI = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionNEGF = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionNOT = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionBNOT = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionMOD = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionAND = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionOR = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXOR = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSHL = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionSHR = record
    Instruction: TThoriumInstructionCode;
    Op1: Word;
    Op2: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;
  
  TThoriumInstructionINCI_S = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionINCF_S = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionINCI_FS = record
    Instruction: TThoriumInstructionCode;
    ModuleIndex: LongInt;
    Offset: LongInt;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionINCF_FS = record
    Instruction: TThoriumInstructionCode;
    ModuleIndex: LongInt;
    Offset: LongInt;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionINCI = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionINCF = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDECI_S = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDECF_S = record
    Instruction: TThoriumInstructionCode;
    Scope: Word;
    Offset: LongInt;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDECI_FS = record
    Instruction: TThoriumInstructionCode;
    ModuleIndex: LongInt;
    Offset: LongInt;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDECF_FS = record
    Instruction: TThoriumInstructionCode;
    ModuleIndex: LongInt;
    Offset: LongInt;
    Reserved: array [0..7] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDECI = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDECF = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXPGET = record
    Instruction: TThoriumInstructionCode;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    PropPointerOverhead: LongInt;
    {$endif}
    {$endif}
    Prop: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    PropPointerOverhead: LongInt;
    {$endif}
    {$endif}
    TRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXPSET = record
    Instruction: TThoriumInstructionCode;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    PropPointerOverhead: LongInt;
    {$endif}
    {$endif}
    Prop: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    PropPointerOverhead: LongInt;
    {$endif}
    {$endif}
    SRI: Word;
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;
  
  TThoriumInstructionXFGET = record
    Instruction: TThoriumInstructionCode;
    ID: Int64;
    ERI: Word;
    TRI: Word;
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXFSET = record
    Instruction: TThoriumInstructionCode;
    ID: Int64;
    ERI: Word;
    VRI: Word;
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXSFGET = record
    Instruction: TThoriumInstructionCode;
    ID: Int64;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    TRI: Word;
    Reserved: array [0..2] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXSFSET = record
    Instruction: TThoriumInstructionCode;
    ID: Int64;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    ExtendedType: Pointer;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    ExtendedTypePointerOverhead: LongInt;
    {$endif}
    {$endif}
    VRI: Word;
    Reserved: array [0..2] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXIGET = record
    Instruction: TThoriumInstructionCode;
    IRI: Word;
    ERI: Word;
    TRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXISET = record
    Instruction: TThoriumInstructionCode;
    IRI: Word;
    ERI: Word;
    VRI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXCT = record
    Instruction: TThoriumInstructionCode;
    ERI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionX2N = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    {$ifdef ENDIAN_BIG}
    {$ifndef CPU64}
    HostTypeOverhead: LongInt;
    {$endif}
    {$endif}
    HostType: PTypeInfo;
    {$ifdef ENDIAN_LITTLE}
    {$ifndef CPU64}
    HostTypeOverhead: LongInt;
    {$endif}
    {$endif}
    Reserved: array [0..6] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionN2X = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCLRN = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVASTART = record
    Instruction: TThoriumInstructionCode;
    Length: Cardinal;
    Pointers: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I8 = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I16 = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I32 = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I64 = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I8S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I16S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I32S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_I64S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_F32 = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_F64 = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_F80 = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVA_X = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVASTART_T = record
    Instruction: TThoriumInstructionCode;
    Length: Cardinal;
    Floats: Cardinal;
    ToClear: Cardinal;
    Reserved: array [0..5] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVAT_F = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVAT_I = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVAT_S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVAT_X = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionVAFINISH = record
    Instruction: TThoriumInstructionCode;
    Reserved: array [0..11] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionTOSTR_I = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionTOSTR_F = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionREF = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionDEREF = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionLEN_S = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionLEN_A = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    TRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionUPDATE_X = record
    Instruction: TThoriumInstructionCode;
    TRI: Word;
    Reserved: array [0..10] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionJMP = record
    Instruction: TThoriumInstructionCode;
    NewAddress: TThoriumInstructionAddress;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionJE = record
    Instruction: TThoriumInstructionCode;
    NewAddress: TThoriumInstructionAddress;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionJNE = record
    Instruction: TThoriumInstructionCode;
    NewAddress: TThoriumInstructionAddress;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionJGT = record
    Instruction: TThoriumInstructionCode;
    NewAddress: TThoriumInstructionAddress;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionJGE = record
    Instruction: TThoriumInstructionCode;
    NewAddress: TThoriumInstructionAddress;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionJLT = record
    Instruction: TThoriumInstructionCode;
    NewAddress: TThoriumInstructionAddress;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionJLE = record
    Instruction: TThoriumInstructionCode;
    NewAddress: TThoriumInstructionAddress;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCALL = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    HRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionCALL_D = record
    Instruction: TThoriumInstructionCode;
    EntryPoint: TThoriumInstructionAddress;
    HRI: Word;
    RetVal: Word;
    Parameters: LongInt;
    KeepResult: Word;
    Pops: Cardinal;
    Reserved: array [0..2] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionFCALL = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    HRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXCALL = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    HRI: Word;
    Reserved: array [0..9] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionXCALL_M = record
    Instruction: TThoriumInstructionCode;
    SRI: Word;
    HRI: Word;
    ERI: Word;
    Reserved: array [0..8] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionRET = record
    Instruction: TThoriumInstructionCode;
    Reserved: array [0..11] of Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionNOOP = record
    Instruction: TThoriumInstructionCode;
    Kind: Word;
    Parameter1: Int64;
    Parameter2: Int64;
    Parameter3: LongInt;
    Reserved: Word;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstructionEmbeddedHint = record
    Instruction: TThoriumInstructionCode;
    Data: array [0..22] of Char;
    NullByte: Byte;
    // Debug infos
    CodeLine: Cardinal;
  end;

  TThoriumInstruction = record
    Instruction: TThoriumInstructionCode;
    Parameter1: Int64;
    Parameter2: Int64;
    Parameter3: Int64;
    // Debug infos
    CodeLine: Cardinal;
  end;
  {$PACKRECORDS DEFAULT}
  
  PThoriumInstruction = ^TThoriumInstruction;
  PThoriumInstructionNOOP = ^TThoriumInstructionNOOP;
  TThoriumInstructionArray = array of TThoriumInstruction;
  PThoriumInstructionArray = ^TThoriumInstructionArray;

const
  THORIUM_INSTRUCTION_CODE_NAME : array [TThoriumInstructionCode] of String = (
    'int.s', 'int', 'intb',
    'flt.s', 'flt',
    'str.s', 'strl.s', 'str', 'strl',
    'ext.s', 'ext',
    'fnc', 'xfnc', 'xmeth',
    'mover.g', 'copyr.g', 'moveg', 'copyg',
    'mover.fg', 'copyr.fg', 'movefg', 'copyfg',
    'mover.l', 'copyr.l', 'movel', 'copyl',
    'mover.p', 'copyr.p', 'movep', 'copyp',
    'copyr.st', 'copyr',
    'mover.st', 'mover', 'movest',
    'pop.s', '.stackhint',
    'clr',
    'castif', 'castie', 'castfe', 'castse', 'castei', 'castef','castes', 'caste',
    'cmpi', 'cmpif', 'cmpie', 'cmpf', 'cmpfi', 'cmpfe', 'cmps', 'cmpse', 'cmpe', 'cmpei', 'cmpef', 'cmpes',
    'evali',
    'addi', 'addf', 'adds',
    'subi', 'subf',
    'muli', 'mulf',
    'divi', 'divf',
    'negi', 'negf',
    'not', 'bnot',
    'mod',
    'and',
    'or',
    'xor',
    'shl',
    'shr',
    'inci', 'incf',
    'deci', 'decf',
    'xpget', 'xpset',
    'xfget', 'xfset', 'xsfget', 'xsfset',
    'xiget', 'xiset',
    'xct',
    'x2n', 'n2x', 'clrn',
    'vastart', 'va.i8', 'va.i16', 'va.i32', 'va.i64',
    'va.i8s', 'va.i16s', 'va.i32s', 'va.i64s', 'va.f32', 'va.f64', 'va.f80', 'va.s', 'va.x',
    'vastart.t', 'vat.f', 'vat.i', 'vat.s', 'vat.x', 'vafinish',
    'tostr.i', 'tostr.f',
    'ref', 'deref',
    'len.s', 'len.a',
    'update.x',
    'jmp',
    'je', 'jne', 'jgt', 'jge', 'jlt', 'jle',
    'call',
    'call.d',
    'fcall',
    'xcall',
    'xcall.m',
    'ret',
    'noop',
    '.'
  );

  THORIUM_NOOPMARK_PLACEHOLDER = 0;
  THORIUM_NOOPMARK_CALL = 1;
  THORIUM_NOOPMARK_INVALID_ACCESS = 2;
  THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET = 3;

const
  THORIUM_OP_EQUAL = 0;
  THORIUM_OP_NOTEQUAL = 1;
  THORIUM_OP_GREATER = 2;
  THORIUM_OP_LESS = 3;
  THORIUM_OP_GREATEREQUAL = 4;
  THORIUM_OP_LESSEQUAL = 5;

{%ENDREGION}

(*
   Region: Operations
                                                                              *)
{%REGION 'Operations' /fold}
type
  TThoriumOperation = (opAssignment, opIncrement, opDecrement, opCmpEqual,
    opCmpNotEqual, opCmpGreater, opCmpGreaterOrEqual, opCmpLess,
    opCmpLessOrEqual, opAddition, opSubtraction, opMultiplication, opDivision,
    opIntegerDivision, opModulus, opBitAnd, opBitOr, opBitXor, opBitShr,
    opBitShl, opBitNot, opLogicalAnd, opLogicalOr, opLogicalXor, opLogicalNot,
    opNegate, opCall, opIndexedRead, opIndexedWrite, opFieldRead,
    opFieldWrite, opEvaluate, opLen, opString, opDeref, opDevolatile,
    opVolatile, opClone, opAppend, opStackcreate, opToNative, opFromNative,
    opFreeNative);
  TThoriumOperations = set of TThoriumOperation;

const
  opCompare = [opCmpEqual, opCmpNotEqual, opCmpGreater, opCmpGreaterOrEqual,
    opCmpLess, opCmpLessOrEqual];
  opIncDec = [opIncrement, opDecrement];
  opBitwise = [opBitAnd, opBitOr, opBitXor, opBitShr, opBitShl, opBitNot];
  opLogical = [opLogicalAnd, opLogicalOr, opLogicalXor, opLogicalNot,
    opEvaluate];

  opIndexedAccess = [opIndexedWrite, opIndexedRead];
  opFieldAccess = [opFieldRead, opFieldWrite];

  opReflexive = [opIncrement, opDecrement, opBitNot, opLogicalNot, opNegate,
    opCall, opEvaluate, opDevolatile, opVolatile, opLen, opString, opDeref,
    opClone, opToNative, opFromNative, opFreeNative];
{%ENDREGION}

(*
   Region: Miscellaneous
   Declarations: TThoriumIntArray, TThoriumStatementKind,
                 TThoriumStatementKinds, THORIUM_ALL_STATEMENTS, THORIUM_LETTER,
                 THORIUM_DIGIT, THORIUM_HEXDIGIT.
                                                                              *)
{%REGION 'Miscellaneous' /fold}
type
  TThoriumIntArray = array of Integer;

const
  THORIUM_LETTER = ['A'..'Z', 'a'..'z', '_'];
  THORIUM_DIGIT = ['0'..'9'];
  THORIUM_HEXDIGIT = THORIUM_DIGIT + ['A'..'F', 'a'..'f'];
{%ENDREGION}

implementation

uses
  Thorium_Utils;


var
  MOVEFG: TThoriumInstructionMOVEFG;
  COPYFG: TThoriumInstructionCOPYFG;
  MOVER_FG: TThoriumInstructionMOVER_FG;
  COPYR_FG: TThoriumInstructionCOPYR_FG;

initialization

if not (Offset(MOVEFG, MOVEFG.ModuleRef) = Offset(COPYFG, COPYFG.ModuleRef)) then
  raise EAssertionFailed.Create('ModuleRef is located at a different offset in copyfg');
if not (Offset(MOVEFG, MOVEFG.ModuleRef) = Offset(MOVER_FG, MOVER_FG.ModuleRef)) then
  raise EAssertionFailed.Create('ModuleRef is located at a different offset in mover_fg');
if not (Offset(MOVEFG, MOVEFG.ModuleRef) = Offset(COPYR_FG, COPYR_FG.ModuleRef)) then
  raise EAssertionFailed.Create('ModuleRef is located at a different offset in copyr_fg');

end.

