unit Thorium_DefaultCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals, Thorium_Utils, typinfo;

type
  TThoriumDefaultSymbol = (
    tsUnknown,
    tsIntegerValue,
    tsFloatValue,
    tsStringValue,
    tsIdentifier,

    // Single char symbols
    tsOpenBracket,
    tsCloseBracket,
    tsOpenSquareBracket,
    tsCloseSquareBracket,
    tsOpenCurlyBracket,
    tsCloseCurlyBracket,
    tsSemicolon,
    tsColon,
    tsComma,
    tsDot,

    // "Maybe multi char" single char symbols
    tsBoolNot,
    tsLess,
    tsGreater,
    tsPlus,
    tsMinus,
    tsMultiply,
    tsDivide,
    tsAssign,

    // Multi char symbols
    tsEqual,
    tsGreaterEqual,
    tsLessEqual,
    tsNotEqual,
    tsAdditiveAssign,
    tsSubtractiveAssign,
    tsMultiplicativeAssign,
    tsDivideAssign,
    tsAppend,

    // Alphanumeric symbols
    tsShl,
    tsShr,
    tsNot,
    tsOr,
    tsXor,
    tsDiv,
    tsMod,
    tsAnd,
    tsIf,
    tsElseIf,
    tsElse,
    tsWhile,
    tsDo,
    tsFor,
    tsCase,
    tsExit,
    tsNull,
    tsPublic,
    tsPrivate,
    tsStatic,
    tsSwitch,
    tsTrue,
    tsFalse,
    tsDefault,
    tsBreak,
    tsReturn,
    tsLoadLibrary,
    tsLoadModule,

    // Special
    tsError,
    tsNone
  );
  TThoriumDefaultSymbols = set of TThoriumDefaultSymbol;

const
  THORIUM_DEFAULT_FIRST_KEYWORD = tsShl;
  THORIUM_DEFAULT_LAST_KEYWORD = tsLoadModule;

  THORIUM_DEFAULT_FIRST_SINGLECHARSYM = tsOpenBracket;
  THORIUM_DEFAULT_LAST_SINGLECHARSYM = tsDot;

  THORIUM_DEFAULT_FIRST_AMBISINGLECHARSYM = tsBoolNot;
  THORIUM_DEFAULT_LAST_AMBISINGLECHARSYM = tsAssign;

  THORIUM_DEFAULT_FIRST_MULTICHARSYM = tsEqual;
  THORIUM_DEFAULT_LAST_MULTICHARSYM = tsAppend;

  THORIUM_DEFAULT_TERM_OPERATOR = [tsMultiply, tsDivide, tsDiv, tsMod, tsAnd, tsShl, tsShr];
  THORIUM_DEFAULT_EXPRESSION_OPEARTOR = [tsPlus, tsMinus, tsOr, tsXor];
  THORIUM_DEFAULT_RELATIONAL_OPERATOR = [tsLess, tsNotEqual, tsGreater, tsLessEqual, tsGreaterEqual, tsEqual];

  THORIUM_DEFAULT_SYMBOL_CODE : array [TThoriumDefaultSymbol] of String = (
    '',
    '',
    '',
    '',
    '',

    // Single char symbols
    '(',
    ')',
    '[',
    ']',
    '{',
    '}',
    ';',
    ':',
    ',',
    '.',

    // "Maybe multi char" single char symbols
    '!',
    '<',
    '>',
    '+',
    '-',
    '*',
    '/',
    '=',

    // Multi char symbols
    '==',
    '>=',
    '<=',
    '!=',
    '+=',
    '-=',
    '*=',
    '/=',
    '~=',

    // Alphanumeric symbols
    'shl',
    'shr',
    'not',
    'or',
    'xor',
    'div',
    'mod',
    'and',
    'if',
    'elseif',
    'else',
    'while',
    'do',
    'for',
    'case',
    'exit',
    'null',
    'public',
    'private',
    'static',
    'switch',
    'true',
    'false',
    'default',
    'break',
    'return',
    'loadlibrary',
    'loadmodule',

    // Special
    '',
    ''
  );
type

  TThoriumDefaultStatementKind = (tskFor, tskWhile, tskDoWhile, tskIf, tskBlock,
    tskAssignment, tskDeclaration, tskCall, tskSwitch, tskBreak, tskNullStatement);
  TThoriumDefaultStatementKinds = set of TThoriumDefaultStatementKind;

const
  THORIUM_DEFAULT_ALL_STATEMENTS = [tskFor, tskWhile, tskDoWhile, tskIf, tskBlock,
    tskAssignment, tskDeclaration, tskCall, tskSwitch, tskBreak, tskNullStatement];

type
  TThoriumDeclarationHandler = procedure (const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer) of object;

  { TThoriumDefaultScanner }

  TThoriumDefaultScanner = class (TObject)
  public
    constructor Create(const AStream: TStream);
    constructor Create(const AString: String);
  private
    constructor Create;
  private
    FLastChar: Char;
    FLine: Integer;
    FPosition: Integer;
    FSourceLength: Integer;
    FSourceString: String;
    FX: Integer;
  protected
    function GetChar(out Ch: Char): Boolean;
    procedure SkipLine;
  public
    function NextSymbol(out Sym: TThoriumDefaultSymbol; out SymStr: String): Boolean;
  end;

  { TThoriumDefaultCompiler }

  TThoriumDefaultCompiler = class (TThoriumCustomCompiler)
  public
    constructor Create(ATarget: TThoriumModule); override;
  private
    FCurrentScope: Integer;
    FCurrentStr: String;
    FCurrentSym: TThoriumDefaultSymbol;
    FCurrentReturnType: IThoriumType;
    FCurrentFunctionTableStack: Integer;
    FCurrentFunc: TThoriumFunction;
    FScanner: TThoriumDefaultScanner;
  protected
    procedure CompilerError(const Msg: String); override;
    procedure CompilerError(const Msg: String; X, Y: Integer); override;
    function ExpectSymbol(SymbolMask: TThoriumDefaultSymbols; ThrowError: Boolean = True): Boolean;
    function GenCode(AInstruction: TThoriumInstruction): Integer; override;
    function GenCodeEx(var TargetArray: TThoriumInstructionArray;
       AInstruction: TThoriumInstruction): Integer; override;
    function Proceed(ExpectMask: TThoriumDefaultSymbols = []; ThrowError: Boolean = False): Boolean;
  protected
    procedure ConstantDeclaration(const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
    procedure GenericDeclaration(const AStatic: Boolean; const AVisibility: TThoriumVisibilityLevel; var Offset: Integer);
    function Expression(ATargetRegister: Word; out AState: TThoriumValueState; AStaticValue: PThoriumValue = nil; ATypeHint: IThoriumType = nil): IThoriumType;
    function Factor(ATargetRegister: Word; out AState: TThoriumValueState; out AStaticValue: TThoriumValue; ATypeHint: IThoriumType = nil): IThoriumType;
    function FindFilteredTableEntry(const Ident: String; AllowedKinds: TThoriumQualifiedIdentifierKinds; out Entry: TThoriumTableEntry; DropMultiple: Boolean = True; GetLast: Boolean = False): Boolean;
    procedure FilterTableEntries(Entries: TThoriumTableEntryResultList; AllowedKinds: TThoriumQualifiedIdentifierKinds);
    procedure FunctionDeclaration(const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
    procedure Module;
    procedure PlaceStatic(const StaticValue: TThoriumValue; ATargetRegister: Word);
    function SimpleExpression(ATargetRegister: Word; out AState: TThoriumValueState; AStaticValue: PThoriumValue = nil; ATypeHint: IThoriumType = nil): IThoriumType;
    function SolveIdentifier(ATargetRegister: Word;
      AllowedKinds: TThoriumQualifiedIdentifierKinds): TThoriumQualifiedIdentifier;
    procedure Statement(var Offset: Integer; const AllowedStatements: TThoriumDefaultStatementKinds = THORIUM_DEFAULT_ALL_STATEMENTS);
    procedure StatementDoWhile(var Offset: Integer);
    procedure StatementFor(var Offset: Integer);
    procedure StatementIdentifier(var Offset: Integer; const AllowedStatements: TThoriumDefaultStatementKinds);
    procedure StatementIf(var Offset: Integer);
    procedure StatementSwitch(var Offset: Integer);
    procedure StatementWhile(var Offset: Integer);
    function Term(ATargetRegister: Word; out AState: TThoriumValueState; out AStaticValue: TThoriumValue; ATypeHint: IThoriumType = nil): IThoriumType;
    procedure VariableDeclaration(const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
  public
    function CompileFromStream(SourceStream: TStream;
       Flags: TThoriumCompilerFlags=[cfOptimize]): Boolean; override;
  end;

implementation

{$I Thorium_InstructionConstructors.inc}

function SymbolToStr(const Sym: TThoriumDefaultSymbol): String;
begin
  Result := GetEnumName(TypeInfo(TThoriumDefaultSymbol), Ord(sym));
end;

function SymbolMaskToStr(const Mask: TThoriumDefaultSymbols): String;
var
  Sym: TThoriumDefaultSymbol;
begin
  Result := '';
  for Sym := Low(TThoriumDefaultSymbols) to High(TThoriumDefaultSymbols) do
    if Sym in Mask then
      Result += SymbolToStr(Sym)+', ';
  if Length(Result) > 0 then
    Delete(Result, Length(Result)-1, 2);
end;

function SymbolToOperation(A: TThoriumDefaultSymbol): TThoriumOperation;
begin
  case A of
    tsEqual: Result := opCmpEqual;
    tsNotEqual: Result := opCmpNotEqual;
    tsGreater: Result := opCmpGreater;
    tsGreaterEqual: Result := opCmpGreaterOrEqual;
    tsLess: Result := opCmpLess;
    tsLessEqual: Result := opCmpLessOrEqual;

    tsPlus: Result := opAddition;
    tsMinus: Result := opSubtraction;
    tsMultiply: Result := opMultiplication;
    tsDivide: Result := opDivision;
    tsDiv: Result := opIntegerDivision;
    tsMod: Result := opModulus;

    tsAnd: Result := opBitAnd;
    tsOr: Result := opBitOr;
    tsXor: Result := opBitXor;
    tsShr: Result := opBitShr;
    tsShl: Result := opBitShl;
    tsNot: Result := opBitNot;
  else
    raise EThoriumCompilerException.CreateFmt('Cannot cast ''%s'' to operation.', [SymbolToStr(A)]);
  end;
end;

{ TThoriumDefaultScanner }

constructor TThoriumDefaultScanner.Create(const AStream: TStream);
begin
  FSourceLength := AStream.Size;
  SetLength(FSourceString, FSourceLength);
  if AStream.Read(FSourceString[1], FSourceLength) < FSourceLength then
    raise EThoriumCompilerException.Create('Scanner input buffer failure. Did not get the amount of data the stream advertised.');
  Create;
end;

constructor TThoriumDefaultScanner.Create(const AString: String);
begin
  FSourceString := AString;
  FSourceLength := Length(AString);
  Create;
end;

constructor TThoriumDefaultScanner.Create;
begin
  FPosition := 0;
  FLine := 1;
  FX := 0;
  FLastChar := #10;
end;

function TThoriumDefaultScanner.GetChar(out Ch: Char): Boolean;
begin
  Result := FPosition < FSourceLength;
  if not Result then
    Exit;
  if FLastChar = #10 then
    Inc(FLine);
  Inc(FPosition);
  Ch := FSourceString[FPosition];
  if (Ch = #13) then
  begin
    Ch := #10;
    if (FPosition < FSourceLength) and (FSourceString[FPosition+1] = #10) then
      Inc(FPosition);
  end;
  FLastChar := Ch;
end;

procedure TThoriumDefaultScanner.SkipLine;
var
  Ch: Char;
begin
  while GetChar(Ch) and (Ch <> #10) do;
end;

function TThoriumDefaultScanner.NextSymbol(out Sym: TThoriumDefaultSymbol;
  out SymStr: String): Boolean;
var
  Ch: Char;
  I: TThoriumDefaultSymbol;
  Tmp: String;
  TmpUI: Cardinal;
begin
  Ch := FLastChar;
  Sym := tsNone;
  SymStr := '';

  while (Ch = ' ') or (Ch = #10) do
    if not GetChar(Ch) then
      Exit(False);

  case Ch of
    'A'..'Z', 'a'..'z', '_': // Identifier or string symbol
    begin
      repeat
        SymStr += Ch;
      until (not GetChar(Ch)) or not (Ch in (THORIUM_DIGIT+THORIUM_LETTER));

      Sym := tsIdentifier;
      SymStr := ThoriumCase(SymStr);

      for I := THORIUM_DEFAULT_FIRST_KEYWORD to THORIUM_DEFAULT_LAST_KEYWORD do
        if THORIUM_DEFAULT_SYMBOL_CODE[I] = SymStr then
        begin
          Sym := I;
          Exit(True);
        end;
      Exit(True);
    end;

    '(', ')', '[', ']', '{', '}', ';', ':', ',', '.':
    begin
      Sym := tsUnknown;
      SymStr := Ch;
      for I := THORIUM_DEFAULT_FIRST_SINGLECHARSYM to THORIUM_DEFAULT_LAST_SINGLECHARSYM do
        if THORIUM_DEFAULT_SYMBOL_CODE[I] = SymStr then
        begin
          Sym := I;
          GetChar(Ch);
          Exit(True);
        end;
    end;

    '/':
    begin
      SymStr := Ch;
      Sym := tsDivide;
      if not GetChar(Ch) then
        Exit(True);
      case Ch of
        '*':
        while GetChar(Ch) do
        begin
          if Ch = '*' then
          begin
            if not GetChar(Ch) then
            begin
              Sym := tsError;
              SymStr := 'Unexpected end of file.';
              Exit(False);
            end;
            if Ch = '/' then
            begin
              GetChar(Ch);
              // Scan for whatever comes afterwards
              Exit(NextSymbol(Sym, SymStr));
            end;
          end;
        end;

        '/':
        begin
          SkipLine;
          Exit(NextSymbol(Sym, SymStr));
        end;

        '=':
        begin
          Sym := tsDivideAssign;
          SymStr := '/=';
          Exit(True);
        end;
      else
        Exit(True);
      end;
    end;


    '!', '<', '>', '+', '-', '*', '=', '~':
    begin
      SymStr := Ch;
      if (not GetChar(Ch)) or (Ch <> '=') then
      begin
        for I := THORIUM_DEFAULT_FIRST_AMBISINGLECHARSYM to THORIUM_DEFAULT_LAST_AMBISINGLECHARSYM do
          if THORIUM_DEFAULT_SYMBOL_CODE[I] = SymStr then
          begin
            Sym := I;
            Exit(True);
          end;
      end
      else
      begin
        SymStr += Ch;
        GetChar(Ch);
        for I := THORIUM_DEFAULT_FIRST_MULTICHARSYM to THORIUM_DEFAULT_LAST_MULTICHARSYM do
          if THORIUM_DEFAULT_SYMBOL_CODE[I] = SymStr then
          begin
            Sym := I;
            Exit(True);
          end;
      end;
    end;

    '"': // String value
    begin
      Sym := tsStringValue;
      SymStr := '';
      while GetChar(Ch) do
      begin
        case Ch of
          '"':
          begin
            GetChar(Ch);
            Exit;
          end;
          '\':
          begin
            if not GetChar(Ch) then
            begin
              Sym := tsError;
              SymStr := 'Unexpected end of file.';
              Exit(False);
            end;
            case ThoriumCase(Ch) of
              'n': SymStr := SymStr + #10;
              'x':
              begin
                Tmp := '$';
                while GetChar(Ch) and (Ch in THORIUM_HEXDIGIT) do
                  Tmp += Ch;
                if Tmp = '$' then
                begin
                  Sym := tsError;
                  SymStr := 'Unexpected end of file.';
                  Exit(False);
                end;
                SymStr := SymStr + UTF8Chr(Cardinal(StrToInt(Tmp)));
              end;
              '"': SymStr += '"';
              '\': SymStr += '\';
            else
              SymStr += '\' + Ch;
            end;
          end;
        else
          SymStr += Ch;
        end;
      end;
      Exit(True);
    end;

    '0'..'9':
    begin
      SymStr := Ch;
      if not GetChar(Ch) then
      begin
        Sym := tsIntegerValue;
        Exit(True);
      end;
      if (SymStr = '0') then
      begin
        case Ch of
          'x': // Hex value
          begin
            SymStr := '$';
            while GetChar(Ch) and (Ch in THORIUM_HEXDIGIT) do
            begin
              SymStr += Ch;
            end;
            if SymStr = '$' then
            begin
              Sym := tsError;
              SymStr := 'Invaid numerical value "0x"';
              Exit(False);
            end;
            Sym := tsIntegerValue;
            Exit(True);
          end;
          'b': // Binary value (not supported)
          begin
            Sym := tsError;
            SymStr := 'Binary values are not supported.';
            Exit(False);
          end;
          '0'..'9': // Octal value (not supported)
          begin
            Sym := tsError;
            SymStr := 'Octal values are not supported';
            Exit(False);
          end;
        end;
      end
      else
      begin
        while (Ch in THORIUM_DIGIT) do
        begin
          SymStr += Ch;
          Ch := #0;
          if not GetChar(Ch) then
            Break;
        end;
        if Ch = '.' then
        begin
          Sym := tsFloatValue;
          SymStr += '.';
          while GetChar(Ch) and (Ch in THORIUM_DIGIT) do
            SymStr += Ch;
          SymStr += '0';
          Exit(True);
        end
        else
        begin
          Sym := tsIntegerValue;
          Exit(True);
        end;
      end;
    end;
  end;
  Result := False;
  if Sym <> tsError then
  begin
    Sym := tsError;
    if SymStr <> '' then
      SymStr := 'Unknown token "'+SymStr+'".'
    else
      SymStr := 'Unknown token "'+Ch+'".';
  end;
end;

{ TThoriumDefaultCompiler }

constructor TThoriumDefaultCompiler.Create(ATarget: TThoriumModule);
begin
  if ATarget = nil then
    raise EThoriumCompilerException.Create('Compiler must have a target module.');
  inherited Create(ATarget);
end;

procedure TThoriumDefaultCompiler.CompilerError(const Msg: String);
begin
  CompilerError(Msg, FScanner.FX, FScanner.FLine);
end;

procedure TThoriumDefaultCompiler.CompilerError(const Msg: String; X, Y: Integer
  );
begin
  raise EThoriumCompilerError.CreateFmt('(%d,%d): %s', [X, Y, Msg]);
end;

function TThoriumDefaultCompiler.ExpectSymbol(
  SymbolMask: TThoriumDefaultSymbols; ThrowError: Boolean): Boolean;
begin
  if FCurrentSym = tsError then
    CompilerError('Scanner error: '+FCurrentStr);
  if SymbolMask = [] then
    Exit(True);
  Result := FCurrentSym in SymbolMask;
  if not Result and ThrowError then
    CompilerError('Unexpected symbol: '+GetEnumName(TypeInfo(TThoriumDefaultSymbol), Ord(FCurrentSym))+'. Expected: '+SymbolMaskToStr(SymbolMask));
end;

function TThoriumDefaultCompiler.GenCode(AInstruction: TThoriumInstruction
  ): Integer;
begin
  inherited GenCode(AInstruction, FScanner.FLine);
end;

function TThoriumDefaultCompiler.GenCodeEx(
  var TargetArray: TThoriumInstructionArray; AInstruction: TThoriumInstruction
  ): Integer;
begin
  Result := inherited GenCodeEx(TargetArray, AInstruction, FScanner.FLine);
end;

function TThoriumDefaultCompiler.Proceed(ExpectMask: TThoriumDefaultSymbols;
  ThrowError: Boolean): Boolean;
begin
  Result := FScanner.NextSymbol(FCurrentSym, FCurrentStr);
  WriteLn('→ ', GetEnumName(TypeInfo(TThoriumDefaultSymbol), Ord(FCurrentSym)));
  if FCurrentSym = tsError then
    CompilerError('Scanner error: '+FCurrentStr);
  if Result and (ExpectMask <> []) then
    Result := ExpectSymbol(ExpectMask, ThrowError);
end;

procedure TThoriumDefaultCompiler.ConstantDeclaration(
  const AVisibility: TThoriumVisibilityLevel; ATypeIdent,
  AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
var
  State: TThoriumValueState;
  Value: TThoriumValue;
  InitialData: TThoriumInitialData;
  Entry: PThoriumTableEntry;
  Creation: TThoriumCreateInstructionDescription;
begin
  ExpectSymbol([tsAssign]);
  Proceed;
  SimpleExpression(THORIUM_REGISTER_INVALID, State, @Value, ATypeIdent.FinalType);
  if State <> vsStatic then
    CompilerError('Static value needed.');
  if Value.RTTI is TThoriumTypeString then
    InitialData.Int := AddLibraryString(Value.Str^)
  else
    InitialData.Int := Value.Int;
  if not ATypeIdent.FinalType.CanCreate(Value, False, Creation) then
    CompilerError('Cannot create such a value.');
  GenCreation(Creation);
  ExpectSymbol([tsSemicolon, tsComma]);

  Entry := FTable.AddConstantIdentifier(AValueIdent.FullStr, FCurrentScope, Offset, ATypeIdent.FinalType, Value);
  Inc(Offset);
  if AVisibility = vsPublic then
    AddPublicVariable.AssignFromTableEntry(Entry^);
  ThoriumFreeValue(Value);
end;

procedure TThoriumDefaultCompiler.GenericDeclaration(const AStatic: Boolean;
  const AVisibility: TThoriumVisibilityLevel; var Offset: Integer);
var
  TypeIdentifier, ValueIdentifier: TThoriumQualifiedIdentifier;
  DeclarationHandler: TThoriumDeclarationHandler;
begin
  TypeIdentifier := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikType]);
  ValueIdentifier := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikUndeclared, ikPrototypedFunction]);

  if ValueIdentifier.Kind = ikPrototypedFunction then
  begin
    if AStatic then
      CompilerError('Functions cannot be static.');
    Proceed([tsOpenBracket]);
    FunctionDeclaration(AVisibility, TypeIdentifier, ValueIdentifier, Offset);
  end
  else
  begin
    ExpectSymbol([tsOpenBracket, tsSemicolon, tsAssign, tsComma]);
    if FCurrentSym = tsOpenBracket then
    begin
      if AStatic then
        CompilerError('Functions cannot be static.');
      FunctionDeclaration(AVisibility, TypeIdentifier, ValueIdentifier, Offset);
    end
    else
    begin
      if AStatic then
        DeclarationHandler := @ConstantDeclaration
      else
        DeclarationHandler := @VariableDeclaration;

      repeat
        if AStatic then
          ExpectSymbol([tsAssign])
        else
          ExpectSymbol([tsSemicolon, tsComma, tsAssign]);
        DeclarationHandler(AVisibility, TypeIdentifier, ValueIdentifier, Offset);
        ExpectSymbol([tsComma, tsSemicolon]);
        if FCurrentSym = tsComma then
        begin
          Proceed([tsIdentifier]);
          ValueIdentifier := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikUndeclared]);
        end;
      until FCurrentSym = tsSemicolon;
      ExpectSymbol([tsSemicolon]);
      Proceed;
    end;
  end;
  TypeIdentifier.FinalType := nil;
  ValueIdentifier.FinalType := nil;
end;

function TThoriumDefaultCompiler.Expression(ATargetRegister: Word;
  out AState: TThoriumValueState; AStaticValue: PThoriumValue;
  ATypeHint: IThoriumType): IThoriumType;
var
  Sym: TThoriumDefaultSymbol;
  OperandType: IThoriumType;
  Operation: TThoriumOperationDescription;
  ResultValue: TThoriumValue;
  State1, State2: TThoriumValueState;
  Value1, Value2: TThoriumValue;
  RegID2: TThoriumRegisterID;
begin
  Result := SimpleExpression(ATargetRegister, State1, @Value1, ATypeHint);

  while FCurrentSym in THORIUM_DEFAULT_RELATIONAL_OPERATOR do
  begin
    Sym := FCurrentSym;
    Proceed;

    OperandType := SimpleExpression(RegID2, State2, @Value2, Result);

    Operation.Operation := SymbolToOperation(Sym);
    if not Result.CanPerformOperation(Operation, OperandType) then
      CompilerError('Invalid operands for this operation.');

    // Attempt to evaluate static values during compile time
    if (State1 = vsStatic) and (State2 = vsStatic) then
    begin
      ResultValue.RTTI := TThoriumTypeInteger.Create;
      if TThoriumType.PerformCmpOperation(Value1, Operation, @Value2) then
        ResultValue.Int := 1
      else
        ResultValue.Int := 0;
      ThoriumFreeValue(Value1);
      ThoriumFreeValue(Value2);
      Value1 := ResultValue;
      StoreType(Value1.RTTI);
      Result := TThoriumTypeInteger.Create;
      Continue;
    end;

    if State1 = vsStatic then
    begin
      PlaceStatic(Value1, ATargetRegister);
      ThoriumFreeValue(Value1);
    end;

    if State2 = vsStatic then
    begin
      PlaceStatic(Value2, RegID2);
      ThoriumFreeValue(Value2);
    end;

    GenOperation(Operation, THORIUM_REGISTER_INVALID, ATargetRegister, RegID2);
    case Operation.Operation of
      opCmpEqual:           GenCode(intb(ATargetRegister, THORIUM_OP_EQUAL));
      opCmpNotEqual:        GenCode(intb(ATargetRegister, THORIUM_OP_NOTEQUAL));
      opCmpGreater:         GenCode(intb(ATargetRegister, THORIUM_OP_GREATER));
      opCmpGreaterOrEqual:  GenCode(intb(ATargetRegister, THORIUM_OP_GREATEREQUAL));
      opCmpLessOrEqual:     GenCode(intb(ATargetRegister, THORIUM_OP_LESSEQUAL));
    else
      CompilerError('Invalid relational operator.');
    end;
    if not (Result.GetInstance is TThoriumTypeInteger) then
      Result := TThoriumTypeInteger.Create;
    State1 := vsDynamic;
  end;

  if (State1 = vsStatic) then
  begin
    if (AStaticValue = nil) then
    begin
      PlaceStatic(Value1, ATargetRegister);
      ThoriumFreeValue(Value1);
      State1 := vsDynamic;
    end
    else
      AStaticValue^ := Value1;
  end;
  AState := State1;
  ReleaseRegister(RegID2);
end;

function TThoriumDefaultCompiler.Factor(ATargetRegister: Word;
  out AState: TThoriumValueState; out AStaticValue: TThoriumValue;
  ATypeHint: IThoriumType): IThoriumType;
var
  InitialData: TThoriumInitialData;
  CreationDescription: TThoriumCreateInstructionDescription;
  TableEntry: TThoriumTableEntry;
  OperationDescription: TThoriumOperationDescription;
  Reg: TThoriumRegisterID;
  Identifier: TThoriumQualifiedIdentifier;
begin
  case FCurrentSym of
    tsMinus:
    begin
      GetFreeRegister(trEXP, Reg);
      try
        Result := Factor(Reg, AState, AStaticValue, ATypeHint);
        OperationDescription.Operation := opNegate;
        if not Result.CanPerformOperation(OperationDescription) then
          CompilerError('Cannot negate this kind of value.');
        if AState = vsStatic then
        begin
          Assert(AStaticValue.RTTI.IsEqualTo(Result));
          AStaticValue.RTTI.DoNegate(AStaticValue);
          AState := vsStatic;
        end
        else
        begin
          GenOperation(OperationDescription, ATargetRegister, Reg);
          AState := vsDynamic;
        end;
      finally
        ReleaseRegister(Reg);
      end;
    end;
    tsPlus:
    begin
      Proceed;
      // Just ignore it
      Exit(Factor(ATargetRegister, AState, AStaticValue, ATypeHint));
    end;
    tsIntegerValue:
    begin
      InitialData.Int := StrToInt64(FCurrentStr);
      Result := TThoriumTypeInteger.Create;
      if not Result.CanCreate(InitialData, True, CreationDescription) then
        CompilerError('Internal compiler error: Cannot create integer value.');
      AState := vsStatic;
      AStaticValue := Result.DoCreate(InitialData);
      StoreType(Result);
      Proceed;
    end;
    tsFloatValue:
    begin
      InitialData.Flt := StrToFloat(FCurrentStr, THORIUM_NUMBER_FORMAT);
      Result := TThoriumTypeFloat.Create;
      if not Result.CanCreate(InitialData, True, CreationDescription) then
        CompilerError('Internal compiler error: Cannot create float value.');
      AState := vsStatic;
      AStaticValue := Result.DoCreate(InitialData);
      StoreType(Result);
      Proceed;
    end;
    tsStringValue:
    begin
      if FCurrentStr <> '' then
        InitialData.Int := AddLibraryString(FCurrentStr)
      else
        InitialData.Int := -1;
      Result := TThoriumTypeString.Create;
      if not Result.CanCreate(InitialData, True, CreationDescription) then
        CompilerError('Internal compiler error: Cannot create string value.');
      AState := vsStatic;
      // Sadly, we need some static handling here.
      AStaticValue.RTTI := Result.GetInstance;
      AStaticValue.Str := NewStr(FCurrentStr);
      StoreType(Result);
      Proceed;
    end;
    tsIdentifier:
    begin
      Identifier := SolveIdentifier(ATargetRegister, [ikComplex, ikLibraryProperty, ikPrototypedFunction, ikStatic, ikVariable]);
      AState := Identifier.State;
      Result := Identifier.FinalType;
      if (AState = vsStatic) then
        AStaticValue := Identifier.Value
      else
      begin
        if AState = vsStatic then
          AState := vsAccessable;
        AppendOperations(Identifier.GetCode);
      end;
    end;
    tsOpenBracket:
    begin
      Proceed;
      if FCurrentSym = tsIdentifier then
      begin
        if FindFilteredTableEntry(FCurrentStr, [ikType], TableEntry) then
        begin
          // Type name. We should probably do cast handling here.
          // Problems:
          // * Make sure that the following symbol is a closing bracket
          // * Rewind to previous symbol if not.
        end;
      end;
      // Pass through
      Result := Expression(ATargetRegister, AState, @AStaticValue, ATypeHint);
      ExpectSymbol([tsCloseBracket]);
      Proceed;
    end;
  else
    CompilerError('Invalid factor (sym = '+GetEnumName(TypeInfo(TThoriumDefaultSymbol), Ord(FCurrentSym))+')');
  end;
end;

function TThoriumDefaultCompiler.FindFilteredTableEntry(const Ident: String;
  AllowedKinds: TThoriumQualifiedIdentifierKinds; out
  Entry: TThoriumTableEntry; DropMultiple: Boolean; GetLast: Boolean): Boolean;
var
  EntriesHandle: TThoriumTableEntryResults;
  Entries: TThoriumTableEntryResultList;
  I: Integer;
begin
  Entries := TThoriumTableEntryResultList.Create;
  try
    FindTableEntries(Ident, EntriesHandle);
    for I := 0 to High(EntriesHandle) do
      Entries.Add(@EntriesHandle[I]);

    FilterTableEntries(Entries, AllowedKinds);

    if Entries.Count = 0 then
      Exit(False)
    else if Entries.Count = 1 then
    begin
      Move(Entries[0]^, Entry, SizeOf(TThoriumTableEntry));
      Exit(True);
    end
    else
    begin
      if not DropMultiple then
        raise EThoriumCompilerException.Create('Ambigous identifier.')
      else
      begin
        if GetLast then
        begin
          Move(Entries[Entries.Count - 1]^, Entry, SizeOf(TThoriumTableEntry));
          Exit(True);
        end
        else
        begin
          Move(Entries[0]^, Entry, SizeOf(TThoriumTableEntry));
          Exit(True);
        end;
      end;
    end;
  finally
    Entries.Free;
  end;
  Result := False;
end;

procedure TThoriumDefaultCompiler.FilterTableEntries(
  Entries: TThoriumTableEntryResultList;
  AllowedKinds: TThoriumQualifiedIdentifierKinds);
var
  Entry: PThoriumTableEntryResult;
  I: Integer;
begin
  // Filter
  I := Entries.Count-1;
  while I >= 0 do
  begin
    Entry := Entries[I];
    if (ikNoFar in AllowedKinds) and (Entry^.SourceModule <> FModule) then
      Entries.Delete(I)
    else if (not (ikType in AllowedKinds) and not (ikComplex in AllowedKinds)) and (Entry^.Entry._Type = etType) then
      Entries.Delete(I)
    else if (not (ikComplex in AllowedKinds)) and (Entry^.Entry._Type in [etCallable, etHostCallable]) then
      Entries.Delete(I)
    else if (not (ikVariable in AllowedKinds) and not (ikComplex in AllowedKinds)) and (Entry^.Entry._Type = etVariable) then
      Entries.Delete(I)
    else if (not (ikStatic in AllowedKinds) and not (ikComplex in AllowedKinds)) and (Entry^.Entry._Type = etStatic) then
      Entries.Delete(I);
    Dec(I);
  end;
end;

procedure TThoriumDefaultCompiler.FunctionDeclaration(
  const AVisibility: TThoriumVisibilityLevel; ATypeIdent,
  AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
type
  TParam = record
    ParamName: String;
    ParamType: IThoriumType;
  end;
  TParams = array of TParam;

var
  ParamIndex, I: Integer;
  ParamTypeIdent, ParamIdent: TThoriumQualifiedIdentifier;
  Func: TThoriumFunction;
  Params: TParams;
  LocalOffset: Integer;
  CreationInstruction: TThoriumCreateInstructionDescription;
begin
  if AValueIdent.Kind = ikPrototypedFunction then
    CompilerError('Function prototyping is not supported yet.');

  Func := TThoriumFunction.Create(FModule);
  FCurrentFunc := Func;
  FCurrentReturnType := ATypeIdent.FinalType;
  Func.Prototype.ReturnType := FCurrentReturnType;
  SetupFunction(Func, FInstructions.Count, AValueIdent.FullStr);
  FTable.AddFunctionIdentifier(Func.Name, Func);

  SaveTable;
  FCurrentScope := THORIUM_STACK_SCOPE_PARAMETERS;
  Proceed;
  ParamIndex := 1;

  while FCurrentSym in [tsIdentifier, tsComma] do
  begin
    if ParamIndex > 1 then
    begin
      ExpectSymbol([tsComma]);
      Proceed;
    end;
    ParamTypeIdent := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikType]);
    ParamIdent := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikUndeclared]);
    SetLength(Params, ParamIndex);

    Func.Prototype.Parameters.Add(ParamTypeIdent.FinalType);
    Params[ParamIndex-1].ParamName := ParamIdent.FullStr;
    Params[ParamIndex-1].ParamType := ParamIdent.FinalType;

    Inc(ParamIndex);
  end;

  for I := High(Params) downto Low(Params) do
  begin
    FTable.AddParameterIdentifier(Params[I].ParamName, FCurrentScope, I-Length(Params), Params[I].ParamType);
  end;

  ExpectSymbol([tsCloseBracket]);
  Proceed;

  FCurrentScope := THORIUM_STACK_SCOPE_LOCAL;
  FCurrentFunctionTableStack := FTableSizes.Count;

  SaveTable;
  Statement(LocalOffset);
  FCurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;

  RestoreTable(LocalOffset, True);
  RestoreTable(LocalOffset, False);

  if Func.Prototype.ReturnType <> nil then
  begin
    Func.Prototype.ReturnType.CanCreateNone(False, CreationInstruction);
    GenCreation(CreationInstruction);
  end;
  GenCode(ret());

  if AVisibility > vsPrivate then
    FPublicFunctions.Add(Func)
  else
    Func.Free;
end;

procedure TThoriumDefaultCompiler.Module;
var
  IsStatic: Boolean;
  Visibility: TThoriumVisibilityLevel;
  NeedIdentifier: Boolean;
  Offset: Integer;
begin
  FCurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
  IsStatic := False;
  Offset := 0;
  Visibility := vsPrivate;
  NeedIdentifier := False;
  while Proceed([tsLoadLibrary, tsLoadModule, tsIdentifier, tsPublic, tsStatic], False) do
  begin
    case FCurrentSym of
      tsLoadModule:
      begin
        // ToDo: Load module
      end;
      tsLoadLibrary:
      begin
        // ToDo: Load library
      end;
      tsStatic:
      begin
        // Use default value for visibility
        IsStatic := True;
        Proceed([tsIdentifier], True);

        GenericDeclaration(IsStatic, Visibility, Offset);

        IsStatic := False;
        Visibility := vsPrivate;
      end;
      tsPublic, tsPrivate:
      begin
        if FCurrentSym = tsPublic then
          Visibility := vsPublic
        else
          Visibility := vsPrivate;
        Proceed([tsIdentifier, tsStatic], True);
        if FCurrentSym = tsStatic then
        begin
          IsStatic := True;
          Proceed([tsIdentifier], True);
        end
        else
          IsStatic := False;

        GenericDeclaration(IsStatic, Visibility, Offset);

        IsStatic := False;
        Visibility := vsPrivate;
      end;
      tsIdentifier:
      begin
        GenericDeclaration(IsStatic, Visibility, Offset);

        IsStatic := False;
        Visibility := vsPrivate;
      end;
    end;
  end;
  ExpectSymbol([tsNone], True);
end;

procedure TThoriumDefaultCompiler.PlaceStatic(const StaticValue: TThoriumValue;
  ATargetRegister: Word);
var
  InitialData: TThoriumInitialData;
  CreationDescription: TThoriumCreateInstructionDescription;
begin
  if not (StaticValue.RTTI.TypeKind in [tkSimple, Thorium.tkString]) then
    raise EThoriumCompilerException.Create('Cannot handle static values which are not simple or string.');
  if StaticValue.RTTI is TThoriumTypeString then
  begin
    if StaticValue.Str^ <> '' then
      InitialData.Int := AddLibraryString(StaticValue.Str^)
    else
      InitialData.Int := -1;
    Assert(StaticValue.RTTI.CanCreate(InitialData, True, CreationDescription));
  end
  else
  begin
    InitialData.Int := StaticValue.Int;
    Assert(StaticValue.RTTI.CanCreate(InitialData, True, CreationDescription));
  end;
  GenCreation(CreationDescription, ATargetRegister);
end;

function TThoriumDefaultCompiler.SimpleExpression(ATargetRegister: Word;
  out AState: TThoriumValueState; AStaticValue: PThoriumValue;
  ATypeHint: IThoriumType): IThoriumType;
var
  Sym: TThoriumDefaultSymbol;
  Operation: TThoriumOperationDescription;
  State1, State2: TThoriumValueState;
  Value1, Value2: TThoriumValue;
  RegID2: TThoriumRegisterID;
  OperandType: IThoriumType;
  ResultValue: TThoriumValue;
begin
  Result := Term(ATargetRegister, State1, Value1, ATypeHint);

  GetFreeRegister(trEXP, RegID2);
  while FCurrentSym in THORIUM_DEFAULT_EXPRESSION_OPEARTOR do
  begin
    Sym := FCurrentSym;
    Proceed;

    OperandType := Term(RegID2, State2, Value2, Result);

    Operation.Operation := SymbolToOperation(Sym);
    if not Result.CanPerformOperation(Operation, OperandType) then
      CompilerError('Invalid operands for this operation.');

    // Attempt to evaluate static values during compile time
    if (State1 = vsStatic) and (State2 = vsStatic) then
    begin
      ResultValue := TThoriumType.PerformOperation(Value1, Operation, @Value2);
      ThoriumFreeValue(Value1);
      ThoriumFreeValue(Value2);
      Value1 := ResultValue;
      StoreType(Value1.RTTI);
      Continue;
    end;

    if State1 = vsStatic then
    begin
      PlaceStatic(Value1, ATargetRegister);
      ThoriumFreeValue(Value1);
    end;

    if State2 = vsStatic then
    begin
      PlaceStatic(Value2, RegID2);
      ThoriumFreeValue(Value2);
    end;

    ClaimRegister(THORIUM_REGISTER_C3);
    GenOperation(Operation, THORIUM_REGISTER_C3, ATargetRegister, RegID2);
    GenCode(mover(THORIUM_REGISTER_C3, ATargetRegister));
    ReleaseRegister(THORIUM_REGISTER_C3);

    State1 := vsDynamic;
  end;

  if (State1 = vsStatic) then
  begin
    if (AStaticValue = nil) then
    begin
      PlaceStatic(Value1, ATargetRegister);
      ThoriumFreeValue(Value1);
      State1 := vsDynamic;
    end
    else
      AStaticValue^ := Value1;
  end;
  AState := State1;
  ReleaseRegister(RegID2);
end;

function TThoriumDefaultCompiler.SolveIdentifier(ATargetRegister: Word;
  AllowedKinds: TThoriumQualifiedIdentifierKinds): TThoriumQualifiedIdentifier;
(*
  @parserContext: Expects FCurrentSym to be tsIdentifier
*)

var
  Solutions: TThoriumQualifiedIdentifierList;
  Entries: TThoriumTableEntryResultList;

  procedure FillSolutionList;
  var
    I: Integer;
    Solution: PThoriumQualifiedIdentifier;
    Entry: PThoriumTableEntryResult;
  begin
    for I := 0 to Entries.Count - 1 do
    begin
      New(Solution);
      FillByte(Solution^, SizeOf(TThoriumQualifiedIdentifier), 0);
      Solutions.Add(Solution);
      Entry := Entries[I];
      Solution^.FinalType := Entry^.Entry.TypeSpec;
      ForceNewCustomOperation(Solution^.GetCode);
      ForceNewCustomOperation(Solution^.SetCode);
      case Entry^.Entry._Type of
        etStatic, etVariable:
        begin
          if Entry^.SourceModule <> FModule then
          begin
            AppendOperation(Solution^.GetCode,
              ThoriumEncapsulateOperation(
                movefsOperation(FThorium.IndexOfModule(Entry^.SourceModule), Entry^.Entry.Offset, ATargetRegister),
                ATargetRegister
              )
            );
            AppendOperation(Solution^.SetCode,
              ThoriumEncapsulateOperation(
                copyr_fsOperation(ATargetRegister, FThorium.IndexOfModule(Entry^.SourceModule), Entry^.Entry.Offset),
                ATargetRegister
              )
            );
          end
          else
          begin
            AppendOperation(Solution^.GetCode,
              ThoriumEncapsulateOperation(
                movesOperation(Entry^.Entry.Scope, Entry^.Entry.Offset, ATargetRegister),
                ATargetRegister
              )
            );
            AppendOperation(Solution^.SetCode,
              ThoriumEncapsulateOperation(
                copyr_sOperation(ATargetRegister, Entry^.Entry.Scope, Entry^.Entry.Offset),
                ATargetRegister
              )
            );
          end;
          if Entry^.Entry._Type = etStatic then
          begin
            Solution^.Kind := ikStatic;
            Solution^.State := vsStatic;
            Solution^.Writable := False;
            Solution^.Value := Entry^.Entry.Value;
          end
          else
          begin
            Solution^.Kind := ikVariable;
            Solution^.State := vsAccessable;
            Solution^.Writable := True;
          end;
        end;
        etRegisterVariable:
        begin
          Solution^.Kind := ikVariable;
          AppendOperation(Solution^.GetCode,
            ThoriumEncapsulateOperation(
              moverOperation(Entry^.Entry.Offset, ATargetRegister),
              ATargetRegister
            )
          );
          AppendOperation(Solution^.SetCode,
            ThoriumEncapsulateOperation(
              copyrOperation(ATargetRegister, Entry^.Entry.Offset),
              ATargetRegister
            )
          );
          Solution^.State := vsAccessable;
          Solution^.Writable := True;
        end;
        etCallable:
        begin
          Solution^.Kind := ikStatic;
          Solution^.Writable := False;
          Solution^.State := vsAccessable;
          AppendOperation(Solution^.GetCode,
            ThoriumEncapsulateOperation(
              fncOperation(TThoriumFunction(Entry^.Entry.Ptr), ATargetRegister),
              ATargetRegister
            )
          );
          AppendOperation(Solution^.SetCode,
            ThoriumEncapsulateOperation(
              fncOperation(TThoriumFunction(Entry^.Entry.Ptr), ATargetRegister),
              ATargetRegister
            )
          );
        end;
        etHostCallable:
        begin
          Solution^.Kind := ikStatic;
          Solution^.Writable := False;
          Solution^.State := vsAccessable;
          AppendOperation(Solution^.GetCode,
            ThoriumEncapsulateOperation(
              xfncOperation(TThoriumHostFunctionBase(Entry^.Entry.Ptr), ATargetRegister),
              ATargetRegister
            )
          );
          AppendOperation(Solution^.SetCode,
            ThoriumEncapsulateOperation(
              xfncOperation(TThoriumHostFunctionBase(Entry^.Entry.Ptr), ATargetRegister),
              ATargetRegister
            )
          );
        end;
        etProperty:
        begin
          Solution^.Kind := ikLibraryProperty;
          AppendOperation(Solution^.GetCode,
            ThoriumEncapsulateOperation(
              xpgetOperation(TThoriumLibraryProperty(Entry^.Entry.Ptr), ATargetRegister),
              ATargetRegister
            )
          );
          AppendOperation(Solution^.SetCode,
            ThoriumEncapsulateOperation(
              xpsetOperation(TThoriumLibraryProperty(Entry^.Entry.Ptr), ATargetRegister),
              ATargetRegister
            )
          );
          Solution^.Writable := TThoriumLibraryProperty(Entry^.Entry.Ptr).GetStatic;
          Solution^.State := vsAccessable;
        end;
        etLibraryConstant:
        begin
          Solution^.Kind := ikStatic;
          Solution^.Writable := False;
          Solution^.State := vsStatic;
          Solution^.Value := TThoriumLibraryConstant(Entry^.Entry.Ptr).Value;
        end;
        etType:
        begin
          Solution^.Kind := ikType;
          Solution^.Writable := False;
        end;
      end;
    end;
  end;

  procedure Discard(const I: Integer);
  begin
    Dispose(Solutions[I]);
    Solutions.Delete(I);
    Entries.Delete(I);
  end;

  procedure EnforceAccess(Sym: TThoriumDefaultSymbol);
  var
    I: Integer;
  begin
    I := Entries.Count - 1;
    while I >= 0 do
    begin
      case Sym of
        tsDot: // Attribute access
        begin
          if not Entries[I]^.Entry.TypeSpec.HasFieldAccess then
            Discard(I);
        end;
        tsOpenSquareBracket: // Element access
        begin
          if not Entries[I]^.Entry.TypeSpec.HasIndexedAccess then
            Discard(I);
        end;
        tsOpenBracket: // Call
        begin
          if not Entries[I]^.Entry.TypeSpec.CanCall then
            Discard(I);
        end;
      else
        raise EThoriumCompilerException.CreateFmt('TThoriumDefaultCompiler.SolveIdentifier EnforceAccess called with Sym=''%s''.', [GetEnumName(TypeInfo(TThoriumDefaultSymbol), Ord(Sym))]);
      end;
      Dec(I);
    end;
  end;

var
  CodeHook1: TThoriumInstructionArray;
  CodeHook2: TThoriumInstructionArray;

  procedure AttachHook;
  begin
    FCodeHook := True;
    FCodeHook1 := @CodeHook1;
    FCodeHook2 := nil;
  end;

  procedure FlushHook(ForceNew: Boolean = True; FlushToGet: Boolean = True; FlushToSet: Boolean = True);
  var
    I: Integer;
  begin
    Assert(FlushToGet or FlushToSet);
    for I := 0 to Solutions.Count - 1 do
    begin
      if FlushToGet then
      begin
        if ForceNew then
          ForceNewCustomOperation(Solutions[I]^.GetCode);
        AppendCodeToOperation(CodeHook1, Solutions[I]^.GetCode);
      end;
      if FlushToSet then
      begin
        if ForceNew then
          ForceNewCustomOperation(Solutions[I]^.SetCode);
        AppendCodeToOperation(CodeHook1, Solutions[I]^.SetCode);
      end;
    end;
  end;

var
  EntriesHandle: TThoriumTableEntryResults;
  I, J: Integer;
  Entry: PThoriumTableEntryResult;

  Operation, WriteOperation: TThoriumOperationDescription;
  OldCodeHook: Boolean;
  OldCodeHook1: PThoriumInstructionArray;
  OldCodeHook2: PThoriumInstructionArray;

  ExprType: IThoriumType;
  ExprState: TThoriumValueState;
  RegPreviousValue, RegID1: TThoriumRegisterID;

  Solution: PThoriumQualifiedIdentifier;
begin
  Assert(FCurrentSym = tsIdentifier);

  Assert(GetFreeRegister(trEXP, RegPreviousValue));

  Result.FinalType := nil;
  Result.GetCode := nil;
  Result.GetJumpMarks := nil;
  Result.SetCode := nil;
  Result.SetJumpMarks := nil;
  Result.UsedExtendedTypes := nil;
  Result.UsedLibraryProps := nil;
  Result.FullStr := FCurrentStr;

  OldCodeHook := FCodeHook;
  OldCodeHook1 := FCodeHook1;
  OldCodeHook2 := FCodeHook2;
  Entries := TThoriumTableEntryResultList.Create;
  Solutions := TThoriumQualifiedIdentifierList.Create;
  try
    FindTableEntries(FCurrentStr, EntriesHandle);
    // Add items in reverse order as we walk through the list in reverse order
    // later due to performance and safety reasons
    for I := High(EntriesHandle) downto 0 do
      Entries.Add(@EntriesHandle[I]);

    FilterTableEntries(Entries, AllowedKinds);

    if Entries.Count = 0 then
    begin
      if not (ikUndeclared in AllowedKinds) then
        CompilerError('Undeclared identifier: '+Result.FullStr);
      Result.Kind := ikUndeclared;
      Proceed;
      Exit;
    end;

    if (AllowedKinds = [ikUndeclared, ikPrototypedFunction]) then
    begin
      // Must not attempt to resolve this identifier further, because it will
      // look like a call.
      if Entries.Count = 0 then
      begin
        Result.Kind := ikUndeclared;
        Proceed;
        Exit;
      end
      else
        CompilerError('Prototyping not allowed yet.');
    end;

    FillSolutionList;

    while Proceed([tsDot, tsOpenSquareBracket, tsOpenBracket], False) do
    begin
      EnforceAccess(FCurrentSym);
      if Entries.Count = 0 then
        CompilerError('Illegal qualifier.');

      // Replace the last set operation with the last get operation, as we need
      // to operate on that result now, independant of whether it will be a
      // set or get operation.
      // Explanation:
      //  SomeObject.SomeRecord.SomeField = 10;
      //
      //  To execute this, you would need to get SomeObject, and from that
      //  SomeRecord as if you were going to read it. Only the last operation,
      //  the one for SomeField, needs to be an actual writing one.
      for I := 0 to Solutions.Count - 1 do
      begin
        Solution := Solutions[I];
        J := Length(Solution^.GetCode);
        if J = 0 then
          Continue;
        Solution^.GetCode[J-1].TargetRI := RegPreviousValue;
        if Solution^.State in [vsDynamic] then
          Solution^.GetCode[J-1].ClearRegisters := [gorTarget];
        Solution^.SetCode[Length(Solution^.SetCode)-1] := Solution^.GetCode[J-1];
      end;

      case FCurrentSym of
        tsDot:
        begin
          Proceed([tsIdentifier], True);
          Operation.Operation := opFieldRead;
          WriteOperation.Operation := opFieldWrite;

          I := Entries.Count - 1;
          while I >= 0 do
          begin
            Entry := Entries[I];
            Solution := Solutions[I];
            if not Entry^.Entry.TypeSpec.CanPerformOperation(Operation, nil, FCurrentStr) then
              Discard(I)
            else
            begin
              Solution^.Writable := Entry^.Entry.TypeSpec.CanPerformOperation(WriteOperation, nil, FCurrentStr);
              AppendOperation(Solution^.GetCode, ThoriumEncapsulateOperation(Operation, ATargetRegister, RegPreviousValue));
              if Solution^.Writable then
                AppendOperation(Solution^.SetCode, ThoriumEncapsulateOperation(WriteOperation, ATargetRegister, RegPreviousValue))
              else
              begin
                // Append the read operation. This will be used to optimize
                // later, if further qualification applies.
                AppendOperation(Solution^.SetCode, ThoriumEncapsulateOperation(Operation, ATargetRegister, RegPreviousValue));
              end;
            end;
            Dec(I);
          end;

          if Entries.Count = 0 then
            CompilerError('No node with '+Result.FullStr+' available with a field called '+FCurrentStr);

          Result.FullStr += '.'+FCurrentStr;
        end;

        tsOpenSquareBracket:
        begin
          Proceed;

          AttachHook;
          GetFreeRegister(trEXP, RegID1);
          ExprType := Expression(RegID1, ExprState);
          FlushHook(False);

          Operation.Operation := opIndexedRead;
          WriteOperation.Operation := opIndexedWrite;
          I := Entries.Count - 1;
          while I >= 0 do
          begin
            Entry := Entries[I];
            Solution := Solutions[I];
            if not Entry^.Entry.TypeSpec.CanPerformOperation(Operation, ExprType) then
              Discard(I)
            else
            begin
              Solution^.Writable := Entry^.Entry.TypeSpec.CanPerformOperation(WriteOperation, ExprType);
              AppendOperation(Solution^.GetCode, ThoriumEncapsulateOperation(Operation, ATargetRegister, RegPreviousValue, RegID1));
              if Solution^.Writable then
                AppendOperation(Solution^.SetCode, ThoriumEncapsulateOperation(WriteOperation, ATargetRegister, RegPreviousValue, RegID1))
              else
              begin
                // Append the read operation. This will be used to optimize
                // later, if further qualification applies.
                AppendOperation(Solution^.SetCode, ThoriumEncapsulateOperation(Operation, ATargetRegister, RegPreviousValue, RegID1));
              end;
            end;
            Dec(I);
          end;

          if Entries.Count = 0 then
            CompilerError('No node with '+Result.FullStr+' available which is accessable by a '+ExprType.Name+' typed index.');

          ReleaseRegister(RegID1);

          Result.FullStr += '[]';
        end;

        tsOpenBracket:
        begin
          Proceed;

          Operation.Operation := opCall;

          I := Entries.Count - 1;
          while I >= 0 do
          begin
            Entry := Entries[I];
            Solution := Solutions[I];
            if not Entry^.Entry.TypeSpec.CanPerformOperation(Operation) then
              Discard(I)
            else
            begin

            end;
            Dec(I);
          end;
        end;
      end;
    end;

    if Solutions.Count > 1 then
    begin
      CompilerError('Ambigous identifier: '+Result.FullStr);
      Exit;
    end
    else if Solutions.Count = 0 then
    begin
      CompilerError('Unresolvable identifier: '+Result.FullStr);
      Exit;
    end
    else
    begin
      Result := Solutions[0]^;
      WriteLn(GetEnumName(TypeInfo(TThoriumQualifiedIdentifierKind), Ord(Result.Kind)));
    end;

    for I := 0 to Solutions.Count - 1 do
      Dispose(Solutions[I]);
    Solutions.Clear;

  finally
    FCodeHook := OldCodeHook;
    FCodeHook1 := OldCodeHook1;
    FCodeHook2 := OldCodeHook2;
    Entries.Free;
    Solutions.Free;
    ReleaseRegister(RegPreviousValue);
  end;
end;

procedure TThoriumDefaultCompiler.Statement(var Offset: Integer;
  const AllowedStatements: TThoriumDefaultStatementKinds);
var
  NeedSemicolon: Boolean;
begin
  case FCurrentSym of
    tsOpenCurlyBracket:
    begin
      // Begin a new instruction block
      // { statement; [statement;] }

      if not (tskBlock in AllowedStatements) then
        CompilerError('Blocks are not allowed here.');
      Proceed;

      if FCurrentSym = tsCloseCurlyBracket then
      begin
        Proceed;
        Exit;
      end;

      SaveTable;
      Statement(Offset);
      while FCurrentSym <> tsCloseCurlyBracket do
        Statement(Offset);
      RestoreTable(Offset);

      ExpectSymbol([tsCloseCurlyBracket]);
      Proceed;
      NeedSemicolon := False;
    end;
    tsIdentifier:
    begin
      StatementIdentifier(Offset, AllowedStatements);
      NeedSemicolon := True;
    end;
    tsIf:
    begin
      if not (tskIf in AllowedStatements) then
        CompilerError('If statement not allowed here.');
      StatementIf(Offset);
      NeedSemicolon := False;
    end;
    tsFor:
    begin
      if not (tskFor in AllowedStatements) then
        CompilerError('For statement not allowed here.');
      StatementFor(Offset);
      NeedSemicolon := False;
    end;
    tsWhile:
    begin
      if not (tskWhile in AllowedStatements) then
        CompilerError('While statement not allowed here.');
      StatementWhile(Offset);
      NeedSemicolon := False;
    end;
    tsDo:
    begin
      if not (tskDoWhile in AllowedStatements) then
        CompilerError('Do-While statement not allowed here.');
      StatementDoWhile(Offset);
      NeedSemicolon := False;
    end;
  end;
  if NeedSemicolon then
    ExpectSymbol([tsSemicolon]);
  Proceed;
end;

procedure TThoriumDefaultCompiler.StatementDoWhile(var Offset: Integer);
begin

end;

procedure TThoriumDefaultCompiler.StatementFor(var Offset: Integer);
begin

end;

procedure TThoriumDefaultCompiler.StatementIdentifier(var Offset: Integer;
  const AllowedStatements: TThoriumDefaultStatementKinds);
var
  Ident1: TThoriumQualifiedIdentifier;
  ExpressionType: IThoriumType;
  ExpressionState: TThoriumValueState;
  RegID1, RegID2: TThoriumRegisterID;
  Assignment: TThoriumAssignmentDescription;
begin
  // Identifier handling
  GetFreeRegister(trEXP, RegID1);
  Ident1 := SolveIdentifier(RegID1, [ikStatic, ikType, ikComplex, ikLibraryProperty, ikVariable]);
  case Ident1.Kind of
    ikType:
    begin
      CompilerError('Local declarations are still to-do.');
    end;
    ikStatic:
    begin
      CompilerError('This cannot stand on the left side of an expression.');
    end;
  else
    // So see what we can do with it.
    ExpectSymbol([tsAssign, tsSemicolon]);
    case FCurrentSym of
      tsAssign:
      begin
        // Handle an assignment.
        // [Ident1] = [Expression];
        if not (tskAssignment in AllowedStatements) then
          CompilerError('Assignment is not allowed here.');
        if not (Ident1.Writable) then
          CompilerError('Cannot write to the left side.');
        // Handle an assignment
        Proceed;
        // Allocate a register for the expression evaluation
        GetFreeRegister(trEXP, RegID2);
        ExpressionType := SimpleExpression(RegID2, ExpressionState, nil, Ident1.FinalType);
        // Allow casting
        Assignment.Casting := True;
        // Check whether the expression result can be assigned to the ident
        if not ExpressionType.CanAssignTo(Assignment, Ident1.FinalType) then
          CompilerError('Cannot assign '+ExpressionType.Name+' to '+Ident1.FinalType.Name);
        // Cast if neccessary
        if Assignment.Cast.Needed then
        begin
          // Assign the registers to the cast instruction
          Assignment.Cast.Instruction.SRI := RegID2;
          Assignment.Cast.Instruction.TRI := RegID1;
          // Generate the code
          GenCode(TThoriumInstruction(Assignment.Cast.Instruction));
        end
        else
        begin
          // Move the expression result to the approprate register
          GenCode(mover(RegID2, RegID1));
        end;
        AppendOperations(Ident1.SetCode);
        // Release the expression register
        if (ExpressionState = vsDynamic) and (ExpressionType.NeedsClear) then
          GenCode(clr(RegID1));
        ReleaseRegister(RegID2);
      end;
      tsSemicolon:
      begin
        // Get the value and delete it after that. (null statement)
        // [Ident1];
        if not (tskNullStatement in AllowedStatements) then
          CompilerError('Null-statement not allowed here.');
        AppendOperations(Ident1.GetCode);
        if (Ident1.State = vsDynamic) and (Ident1.FinalType.NeedsClear) then
          GenCode(clr(RegID1));
      end;
    end;
  end;
  ReleaseRegister(RegID1);
end;

procedure TThoriumDefaultCompiler.StatementIf(var Offset: Integer);
begin

end;

procedure TThoriumDefaultCompiler.StatementSwitch(var Offset: Integer);
begin

end;

procedure TThoriumDefaultCompiler.StatementWhile(var Offset: Integer);
begin

end;

function TThoriumDefaultCompiler.Term(ATargetRegister: Word; out
  AState: TThoriumValueState; out AStaticValue: TThoriumValue;
  ATypeHint: IThoriumType): IThoriumType;
var
  State1, State2: TThoriumValueState;
  Value1, Value2: TThoriumValue;
  Sym: TThoriumDefaultSymbol;
  RegID2: TThoriumRegisterID;
  OperandType: IThoriumType;
  Operation: TThoriumOperationDescription;
  ResultValue: TThoriumValue;
begin
  Result := Factor(ATargetRegister, State1, Value1, ATypeHint);
  GetFreeRegister(trEXP, RegID2);

  while FCurrentSym in THORIUM_DEFAULT_TERM_OPERATOR do
  begin
    Sym := FCurrentSym;
    Proceed;

    OperandType := Factor(RegID2, State2, Value2, Result);

    Operation.Operation := SymbolToOperation(Sym);
    if not Result.CanPerformOperation(Operation, OperandType) then
      CompilerError('Invalid operands for this operation.');

    // Attempt to evaluate the expression during compilation
    if (State1 = vsStatic) and (State2 = vsStatic) then
    begin
      ResultValue := TThoriumType.PerformOperation(Value1, Operation, @Value2);
      ThoriumFreeValue(Value1);
      ThoriumFreeValue(Value2);
      Value1 := ResultValue;
      StoreType(Value1.RTTI);
      Continue;
    end;

    // Otherwise, dynamify static values
    if State1 = vsStatic then
    begin
      PlaceStatic(Value1, ATargetRegister);
      ThoriumFreeValue(Value1);
    end;

    if State2 = vsStatic then
    begin
      PlaceStatic(Value2, RegID2);
      ThoriumFreeValue(Value2);
    end;

    ClaimRegister(THORIUM_REGISTER_C3);
    GenOperation(Operation, THORIUM_REGISTER_C3, ATargetRegister, RegID2);
    if State1 = vsDynamic then
      GenCode(clr(ATargetRegister));
    if State2 = vsDynamic then
      GenCode(clr(RegID2));
    GenCode(mover(THORIUM_REGISTER_C3, ATargetRegister));
    ReleaseRegister(THORIUM_REGISTER_C3);
    State1 := vsDynamic;
  end;
  ReleaseRegister(RegID2);
  // All static evaluations are stored to Value1, so return that in case of a
  // static value
  AState := State1;
  if AState = vsStatic then
    AStaticValue := Value1;
end;

procedure TThoriumDefaultCompiler.VariableDeclaration(
  const AVisibility: TThoriumVisibilityLevel; ATypeIdent,
  AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
var
  State: TThoriumValueState;
  Value: TThoriumValue;
  InitialData: TThoriumInitialData;
  Entry: PThoriumTableEntry;
  Reg: TThoriumRegisterID;
  Creation: TThoriumCreateInstructionDescription;
begin
  if FCurrentSym = tsAssign then
  begin
    Proceed;
    SimpleExpression(THORIUM_REGISTER_INVALID, State, @Value, ATypeIdent.FinalType);
    if State <> vsStatic then
      CompilerError('Static value needed.');
    // Again, special handling for string!
    if Value.RTTI is TThoriumTypeString then
    begin
      InitialData.Int := AddLibraryString(Value.Str^);
    end
    else
      InitialData.Int := Value.Int;
    if not ATypeIdent.FinalType.CanCreate(InitialData, False, Creation) then
      CompilerError('Cannot create such a value.');
    ThoriumFreeValue(Value);
    GenCreation(Creation);
  end
  else
  begin
    if not ATypeIdent.FinalType.CanCreateNone(False, Creation) then
      CompilerError('Need an initial value, as `None'' creation failed.');
    GenCreation(Creation);
  end;
  ExpectSymbol([tsSemicolon, tsComma]);

  Entry := FTable.AddVariableIdentifier(AValueIdent.FullStr, FCurrentScope, Offset, ATypeIdent.FinalType);
  Inc(Offset);
  if AVisibility = vsPublic then
    AddPublicVariable.AssignFromTableEntry(Entry^);
end;

function TThoriumDefaultCompiler.CompileFromStream(SourceStream: TStream;
  Flags: TThoriumCompilerFlags): Boolean;
begin
  ResetState;
  FScanner := TThoriumDefaultScanner.Create(SourceStream);
  try
    try
      // Attempt to compile the module
      Module;
    finally

    end;
  finally
    FScanner.Free;
    Result := not HasError;
  end;
end;

end.

