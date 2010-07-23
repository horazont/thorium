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
  EThoriumCompilerError = class (EThoriumCompilerException);

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
    function Expression(ATargetRegister: Word; out AState: TThoriumValueState; AStaticValue: PThoriumValue = nil; ATypeHint: IThoriumType = nil): IThoriumType;
    function Factor(ATargetRegister: Word; out AState: TThoriumValueState; out AStaticValue: TThoriumValue; ATypeHint: IThoriumType = nil): IThoriumType;
    function FindFilteredTableEntry(const Ident: String; AllowedKinds: TThoriumQualifiedIdentifierKinds; out Entry: TThoriumTableEntry; DropMultiple: Boolean = True; GetLast: Boolean = False): Boolean;
    procedure FilterTableEntries(Entries: TThoriumTableEntryResultList; AllowedKinds: TThoriumQualifiedIdentifierKinds);
    procedure Module;
    procedure PlaceStatic(const StaticValue: TThoriumValue; ATargetRegister: Word);
    function SimpleExpression(ATargetRegister: Word; out AState: TThoriumValueState; AStaticValue: PThoriumValue = nil; ATypeHint: IThoriumType = nil): IThoriumType;
    function SolveIdentifier(ATargetRegister: Word;
      AllowedKinds: TThoriumQualifiedIdentifierKinds): TThoriumQualifiedIdentifier;
    function Term(ATargetRegister: Word; out AState: TThoriumValueState; out AStaticValue: TThoriumValue; ATypeHint: IThoriumType = nil): IThoriumType;
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
    Delete(Result, Length(Result)-2, 2);
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
  FLastChar := #0;
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
  Sym := tsError;
  if SymStr <> '' then
    SymStr := 'Unknown token "'+SymStr+'".'
  else
    SymStr := 'Unknown token "'+Ch+'".';
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
  raise EThoriumCompilerError.CreateFmt('%d|%d: %s', [X, Y, Msg]);
end;

function TThoriumDefaultCompiler.ExpectSymbol(
  SymbolMask: TThoriumDefaultSymbols; ThrowError: Boolean): Boolean;
begin
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
  if Result and (ExpectMask <> []) then
    Result := ExpectSymbol(ExpectMask, ThrowError);
end;

function TThoriumDefaultCompiler.Expression(ATargetRegister: Word;
  out AState: TThoriumValueState; AStaticValue: PThoriumValue;
  ATypeHint: IThoriumType): IThoriumType;
begin
  raise EThoriumCompilerException.Create('Expression not implemented.');
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
    end;
    tsFloatValue:
    begin
      InitialData.Flt := StrToFloat(FCurrentStr, THORIUM_NUMBER_FORMAT);
      Result := TThoriumTypeFloat.Create;
      if not Result.CanCreate(InitialData, True, CreationDescription) then
        CompilerError('Internal compiler error: Cannot create float value.');
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
      Result := SimpleExpression(ATargetRegister, AState, @AStaticValue, ATypeHint);
      ExpectSymbol([tsCloseBracket]);
      Proceed;
    end;
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

procedure TThoriumDefaultCompiler.Module;
var
  IsStatic: Boolean;
  Visibility: TThoriumVisibilityLevel;
begin
  IsStatic := False;
  Visibility := vsPrivate;
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
        Visibility := vsPrivate;
        IsStatic := True;
        Proceed([tsIdentifier], True);
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
      end;
    end;
  end;
  ExpectSymbol([tsUnknown], True);
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

    GenOperation(Operation, THORIUM_REGISTER_C3, ATargetRegister, RegID2);
    GenCode(mover(THORIUM_REGISTER_C3, ATargetRegister));
  end;

  if (State1 = vsStatic) and (AStaticValue = nil) then
  begin
    PlaceStatic(Value1, ATargetRegister);
    ThoriumFreeValue(Value1);
    State1 := vsDynamic;
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
            Solution^.State := vsStatic;
            Solution^.Writable := False;
            Solution^.Value := Entry^.Entry.Value;
          end
          else
          begin
            Solution^.State := vsAccessable;
            Solution^.Writable := True;
          end;
        end;
        etRegisterVariable:
        begin
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
          Solution^.State := vsDynamic;
        end;
        etLibraryConstant:
        begin
          Solution^.Writable := False;
          Solution^.State := vsStatic;
          Solution^.Value := TThoriumLibraryConstant(Entry^.Entry.Ptr).Value;
        end;
        etType:
        begin
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
  I: Integer;
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
  try
    FindTableEntries(FCurrentStr, EntriesHandle);
    // Add items in reverse order as we walk through the list in reverse order
    // later due to performance and safety reasons
    for I := High(EntriesHandle) downto 0 do
      Entries.Add(@EntriesHandle[I]);

    FilterTableEntries(Entries, AllowedKinds);

    if Entries.Count = 0 then
    begin
      Result.Kind := ikUndeclared;
      Exit;
    end;

    while Proceed([tsDot, tsOpenSquareBracket, tsOpenBracket], False) do
    begin
      EnforceAccess(FCurrentSym);
      if Entries.Count = 0 then
        CompilerError('Illegal qualifier.');

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

  finally
    FCodeHook := OldCodeHook;
    FCodeHook1 := OldCodeHook1;
    FCodeHook2 := OldCodeHook2;
    Entries.Free;
    ReleaseRegister(RegPreviousValue);
  end;
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

    GenOperation(Operation, THORIUM_REGISTER_C3, ATargetRegister, RegID2);
    if State1 = vsDynamic then
      GenCode(clr(ATargetRegister));
    if State2 = vsDynamic then
      GenCode(clr(RegID2));
    GenCode(mover(THORIUM_REGISTER_C3, ATargetRegister));
    State1 := vsDynamic;
  end;
  ReleaseRegister(RegID2);
  // All static evaluations are stored to Value1, so return that in case of a
  // static value
  AState := State1;
  if AState = vsStatic then
    AStaticValue := Value1;
end;

function TThoriumDefaultCompiler.CompileFromStream(SourceStream: TStream;
  Flags: TThoriumCompilerFlags): Boolean;
begin
  ResetState;
  FScanner := TThoriumDefaultScanner.Create(SourceStream);
  try
    try
      // Select the first symbol
      Proceed;
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

