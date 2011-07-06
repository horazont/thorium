unit Thorium_DefaultCompiler;

{$mode objfpc}{$H+}

// If this is set, the parser will raise an exception when it encounters a
// tsNone token.
{$define DebugTokenLoop}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals, Thorium_Utils, typinfo;

{$ifdef DebugTokenLoop}
const
  MAX_NONES = 3;
{$endif}

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
  THORIUM_DEFAULT_LOGICAL_TERM_OPERATOR = [tsAnd];
  THORIUM_DEFAULT_LOGICAL_EXPRESSION_OPERATOR = [tsOr];

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
    tskAssignment, tskDeclaration, tskCall, tskSwitch, tskBreak, tskReturn,
    tskNullStatement);
  TThoriumDefaultStatementKinds = set of TThoriumDefaultStatementKind;

const
  THORIUM_DEFAULT_ALL_STATEMENTS = [tskFor, tskWhile, tskDoWhile, tskIf, tskBlock,
    tskAssignment, tskDeclaration, tskCall, tskSwitch, tskBreak, tskReturn,
    tskNullStatement];

type
  TThoriumDeclarationHandler = procedure (const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer) of object;

  TThoriumDefaultIdentifierScope = (isLocal, isGlobal);

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
    FBreakList: TThoriumJumpList;
    FBreakOffsetTarget: Integer;
    FHadGlobalVariable: Boolean;
    FLastGlobalJump: TThoriumInstructionAddress;
    FCurrentScope: TThoriumDefaultIdentifierScope;
    FCurrentStr: String;
    FCurrentSym: TThoriumDefaultSymbol;
    FCurrentReturnType: TThoriumType;
    FCurrentFunctionTableStack: Integer;
    FCurrentFunc: TThoriumFunction;
    FScanner: TThoriumDefaultScanner;
  protected
    procedure CompilerError(const Msg: String); override;
    procedure CompilerError(const Msg: String; X, Y: Integer); override;
    procedure Debug_CurrentSym;
    function ExpectSymbol(SymbolMask: TThoriumDefaultSymbols; ThrowError: Boolean = True): Boolean;
    function GenCode(AInstruction: TThoriumInstruction): Integer; override;
    function GenCodeEx(var TargetArray: TThoriumInstructionArray;
       AInstruction: TThoriumInstruction): Integer; override;
    function Proceed(ExpectMask: TThoriumDefaultSymbols = []; ThrowError: Boolean = False): Boolean;
    procedure SetupJumps(AList: TThoriumIntList; ATarget: TThoriumInstructionAddress);
  protected
    procedure ConstantDeclaration(const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
    procedure GenericDeclaration(const AStatic: Boolean; const AVisibility: TThoriumVisibilityLevel; var Offset: Integer);
    function Factor(ATargetRegister: Word; out AState: TThoriumValueState; out AStaticValue: TThoriumValue; ATypeHint: TThoriumType = nil): TThoriumType;
    function FindFilteredTableEntry(const Ident: String; AllowedKinds: TThoriumQualifiedIdentifierKinds; out Entry: TThoriumTableEntry; DropMultiple: Boolean = True; GetLast: Boolean = False): Boolean;
    procedure FilterTableEntries(Entries: TThoriumTableEntryResultList; AllowedKinds: TThoriumQualifiedIdentifierKinds);
    procedure FunctionDeclaration(const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
    procedure JumpingLogicalExpression(TrueJumps, FalseJumps: TThoriumIntList);
    procedure JumpingLogicalTerm(TrueJumps, FalseJumps: TThoriumIntList);
    procedure JumpingRelationalExpression(out TrueJump, FalseJump: TThoriumInstructionAddress);
    procedure Module;
    function ParseLibraryName: String;
    function ParseModuleName: String;
    procedure PlaceStatic(const StaticValue: TThoriumValue; ATargetRegister: Word);
    function RelationalExpression(ATargetRegister: Word; out AState: TThoriumValueState; AStaticValue: PThoriumValue = nil; ATypeHint: TThoriumType = nil): TThoriumType;
    function SimpleExpression(ATargetRegister: Word; out AState: TThoriumValueState; AStaticValue: PThoriumValue = nil; ATypeHint: TThoriumType = nil): TThoriumType;
    function SolveIdentifier(ATargetRegister: Word;
      AllowedKinds: TThoriumQualifiedIdentifierKinds): TThoriumQualifiedIdentifier;
    procedure Statement(var Offset: Integer;
      const AllowedStatements: TThoriumDefaultStatementKinds = THORIUM_DEFAULT_ALL_STATEMENTS;
      DisableSemicolon: Boolean = False);
    procedure StatementDoWhile(var Offset: Integer);
    procedure StatementFor(var Offset: Integer);
    procedure StatementIdentifier(var Offset: Integer; const AllowedStatements: TThoriumDefaultStatementKinds);
    procedure StatementIf(var Offset: Integer);
    procedure StatementReturn(var Offset: Integer);
    procedure StatementSwitch(var Offset: Integer);
    procedure StatementWhile(var Offset: Integer);
    function Term(ATargetRegister: Word; out AState: TThoriumValueState; out AStaticValue: TThoriumValue; ATypeHint: TThoriumType = nil): TThoriumType;
    procedure VariableDeclarationCBC(const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
    procedure VariableDeclaration(const AVisibility: TThoriumVisibilityLevel;
      ATypeIdent, AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer;
      const ARegID: TThoriumRegisterID = THORIUM_REGISTER_INVALID);
  public
    function CompileFromStream(SourceStream: TStream;
       Flags: TThoriumCompilerFlags=[cfOptimize]): Boolean; override;
  end;

implementation

{$ifdef DebugTokenLoop}
var
  NoneCount: Integer = 0;
{$endif}

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
  FLine := 0;
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
        else
          // Just zero
          Sym := tsIntegerValue;
          SymStr := '0';
          Exit(True);
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

procedure TThoriumDefaultCompiler.Debug_CurrentSym;
begin
  WriteLn(GetEnumName(TypeInfo(TThoriumDefaultSymbol), Ord(FCurrentSym)));
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
  Result := inherited GenCode(AInstruction, FScanner.FLine);
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
  Write('→ ');
  Debug_CurrentSym;
  {$ifdef DebugTokenLoop}
  if FCurrentSym = tsNone then
  begin
    Inc(NoneCount);
    if NoneCount >= MAX_NONES then
      CompilerError('Ran into tsNone');
  end;
  {$endif}
  if FCurrentSym = tsError then
    CompilerError('Scanner error: '+FCurrentStr);
  if Result and (ExpectMask <> []) then
    Result := ExpectSymbol(ExpectMask, ThrowError);
end;

procedure TThoriumDefaultCompiler.SetupJumps(AList: TThoriumIntList;
  ATarget: TThoriumInstructionAddress);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    TThoriumInstructionJMP(GetInstruction(AList[I])^).NewAddress := ATarget;
end;

procedure TThoriumDefaultCompiler.ConstantDeclaration(
  const AVisibility: TThoriumVisibilityLevel; ATypeIdent,
  AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
(*
  @parserContext: Expects tsAssign and after that a valid static expression.
*)
var
  State: TThoriumValueState;
  Value: TThoriumValue;
  Entry: PThoriumTableEntry;
  InitialData: TThoriumInitialData;
  Creation: TThoriumCreateInstructionDescription;
begin
  EmbedHint('const');
  ExpectSymbol([tsAssign]);
  Proceed;

  // Parse initial value
  SimpleExpression(THORIUM_REGISTER_INVALID, State, @Value, ATypeIdent.FinalType);
  // Make sure it's static - statics cannot be initialized with dynamic values
  if State <> vsStatic then
    CompilerError('Static value needed.');

  // Special handling for strings, as they use the library
  if Value.RTTI is TThoriumTypeString then
    InitialData.Int := AddLibraryString(Value.Str^)
  else
    InitialData.Int := Value.Int;

  // Fetch the creation instruction
  if not ATypeIdent.FinalType.CanCreate(InitialData, False, Creation) then
    CompilerError('Cannot create a value of '''+ATypeIdent.FinalType.Name+'''.');
  GenCreation(Creation);

  // Add an entry to the identifier table
  Entry := FTable.AddVariableIdentifier(AValueIdent.FullStr, Offset, ATypeIdent.FinalType, FCurrentScope = isGlobal, False, @Value);
  // Increase the table offset
  Inc(Offset);

  // If its public, also add a copy to the public table
  if AVisibility = vsPublic then
    AddPublicVariable(AValueIdent.FullStr).AssignFromTableEntry(Entry^);

  // Free the initial value afterwards.
  ThoriumFreeValue(Value);
end;

procedure TThoriumDefaultCompiler.GenericDeclaration(const AStatic: Boolean;
  const AVisibility: TThoriumVisibilityLevel; var Offset: Integer);
var
  TypeIdentifier, ValueIdentifier: TThoriumQualifiedIdentifier;
  DeclarationHandler: TThoriumDeclarationHandler;
begin
  // Parse the two identifiers neccessary for a valid declaration
  TypeIdentifier := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikType]);
  ValueIdentifier := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikUndeclared, ikPrototypedFunction]);

  // Check whether its a prototyped function
  if ValueIdentifier.Kind = ikPrototypedFunction then
  begin
    // Just to annoy the user, we could also just ignore it ;)
    if AStatic then
      CompilerError('Functions cannot be static.');
    Proceed([tsOpenBracket]);

    // Setup the declaration jump if neccessary
    if FHadGlobalVariable then
    begin
      EmbedHint('initjump');
      FLastGlobalJump := GenCode(jmp(THORIUM_JMP_EXIT));
      FHadGlobalVariable := False;
    end;

    // Parse the function declaration
    FunctionDeclaration(AVisibility, TypeIdentifier, ValueIdentifier, Offset);
  end
  else
  begin
    ExpectSymbol([tsOpenBracket, tsSemicolon, tsAssign, tsComma]);
    // Check whether it is a function declaration
    if FCurrentSym = tsOpenBracket then
    begin
      // Again, just annoy the user
      if AStatic then
        CompilerError('Functions cannot be static.');

      // Setup the declaration jump if neccessary
      if FHadGlobalVariable then
      begin
        FLastGlobalJump := GenCode(jmp(THORIUM_JMP_EXIT));
        FHadGlobalVariable := False;
      end;

      // Parse the function declaration
      FunctionDeclaration(AVisibility, TypeIdentifier, ValueIdentifier, Offset);
    end
    else
    begin
      if not FHadGlobalVariable then
      begin
        TThoriumInstructionJMP(GetInstruction(FLastGlobalJump)^).NewAddress := GetNextInstructionAddress;
        FHadGlobalVariable := True;
      end;

      // Use the appropriate handler for constants / variables
      if AStatic then
        DeclarationHandler := @ConstantDeclaration
      else
        DeclarationHandler := @VariableDeclarationCBC;

      // Now parse the declarations
      repeat
        if AStatic then
          ExpectSymbol([tsAssign])
        else
          ExpectSymbol([tsSemicolon, tsComma, tsAssign]);
        // Call the handler to parse the declaration
        DeclarationHandler(AVisibility, TypeIdentifier, ValueIdentifier, Offset);
        // Ensure validity
        ExpectSymbol([tsComma, tsSemicolon]);
        // If another will follow, we need to parse another value.
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

function TThoriumDefaultCompiler.Factor(ATargetRegister: Word;
  out AState: TThoriumValueState; out AStaticValue: TThoriumValue;
  ATypeHint: TThoriumType): TThoriumType;
var
  InitialData: TThoriumInitialData;
  CreationDescription: TThoriumCreateInstructionDescription;
  OperationDescription: TThoriumOperationDescription;
  Reg: TThoriumRegisterID;
  Identifier: TThoriumQualifiedIdentifier;
begin
  case FCurrentSym of
    tsMinus: // Negate the value
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
    tsPlus: // Skip
    begin
      Proceed;
      // Just ignore it
      Exit(Factor(ATargetRegister, AState, AStaticValue, ATypeHint));
    end;
    tsIntegerValue: // Return that value
    begin
      // Read the value to an initial dataset
      InitialData.Int := StrToInt64(FCurrentStr);
      // Create the type
      Result := TypeInteger;
      // Get the creation instruction
      if not Result.CanCreate(InitialData, True, CreationDescription) then
        CompilerError('Internal compiler error: Cannot create integer value.');
      // But return it statically
      AState := vsStatic;
      AStaticValue := Result.DoCreate(InitialData);
      Proceed;
    end;
    tsFloatValue:
    begin
      // See tsIntegerValue for details
      InitialData.Flt := StrToFloat(FCurrentStr, THORIUM_NUMBER_FORMAT);
      Result := TypeFloat;
      if not Result.CanCreate(InitialData, True, CreationDescription) then
        CompilerError('Internal compiler error: Cannot create float value.');
      AState := vsStatic;
      AStaticValue := Result.DoCreate(InitialData);
      Proceed;
    end;
    tsStringValue:
    begin
      // Special handling for string as it uses the string library.
      if FCurrentStr <> '' then
        InitialData.Int := AddLibraryString(FCurrentStr)
      else
        InitialData.Int := -1;
      Result := TypeString;
      if not Result.CanCreate(InitialData, True, CreationDescription) then
        CompilerError('Internal compiler error: Cannot create string value.');
      AState := vsStatic;
      // Sadly, we need some static handling here.
      AStaticValue.RTTI := Result;
      AStaticValue.Str := NewStr(FCurrentStr);
      Proceed;
    end;
    tsIdentifier:
    begin
      // Resolve an identifier and return it appropriately
      Identifier := SolveIdentifier(ATargetRegister, [ikComplex, ikLibraryProperty, ikPrototypedFunction, ikVariable]);
      // Use the state and type of the identifier
      AState := Identifier.State;
      Result := Identifier.FinalType;
      if (AState = vsStatic) then
      begin
        // Adapt static value
        AStaticValue := Identifier.Value
      end
      else
      begin
        if AState = vsStatic then
          AState := vsAccessable;
        // Append the code to read that identifier
        AppendOperations(Identifier.GetCode);
      end;
    end;
    tsOpenBracket:
    begin
      Proceed;
      { TODO : Handle typecasting, if any. }
      (*
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
      *)
      // Pass through
      Result := RelationalExpression(ATargetRegister, AState, @AStaticValue, ATypeHint);
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
  EntriesHandle := nil;
  // Initialize a result list
  Entries := TThoriumTableEntryResultList.Create;
  try
    // Fetch the table entries
    FindTableEntries(Ident, EntriesHandle);
    // Store pointers to them in the result list for easier handling
    for I := 0 to High(EntriesHandle) do
      Entries.Add(@EntriesHandle[I]);

    // Filter them
    FilterTableEntries(Entries, AllowedKinds);

    // Return false if none is found
    if Entries.Count = 0 then
      Exit(False)
    else if Entries.Count = 1 then
    begin
      // True if exactly one is found
      Entry := Entries[0]^.Entry;
      Exit(True);
    end
    else
    begin
      // Raise an exception if multiple are found and DropMultiple is disabled
      if not DropMultiple then
        raise EThoriumCompilerException.Create('Ambigous identifier.')
      else
      begin
        // Or return the appropriate entry
        if GetLast then
        begin
          Entry := Entries[Entries.Count - 1]^.Entry;
          Exit(True);
        end
        else
        begin
          Entry := Entries[0]^.Entry;
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
  // Filter table entries depending on their kind
  I := Entries.Count-1;
  while I >= 0 do
  begin
    Entry := Entries[I];
    if (ikNoFar in AllowedKinds) and (Entry^.SourceModule <> FModule) then
      Entries.Delete(I)
    else if (not (ikType in AllowedKinds) and not (ikComplex in AllowedKinds)) and (Entry^.Entry._Type = ttType) then
      Entries.Delete(I)
    else if (not (ikComplex in AllowedKinds)) and (Entry^.Entry._Type in [ttGlobalCallable, ttHostCallable]) then
      Entries.Delete(I)
    else if (not (ikVariable in AllowedKinds) and not (ikComplex in AllowedKinds)) and (Entry^.Entry._Type in [ttLocal, ttGlobal, ttParameter]) then
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
    ParamType: TThoriumType;
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

  // Create a function instance
  Func := TThoriumFunction.Create(FModule, AValueIdent.FullStr);
  FCurrentFunc := Func;
  FCurrentReturnType := ATypeIdent.FinalType;
  Func.Prototype.ReturnType := FCurrentReturnType;
  SetupFunction(Func, FInstructions.Count, AValueIdent.FullStr);
  // Add the function to the public table
  FTable.AddFunctionIdentifier(Func.Name, Func);

  SaveTable;
  Proceed;
  ParamIndex := 1;

  // Parse the parameters
  while FCurrentSym in [tsIdentifier, tsComma] do
  begin
    // Expect comma for every parameter except the first
    if ParamIndex > 1 then
    begin
      ExpectSymbol([tsComma]);
      Proceed;
    end;
    // Get the parameter identifiers
    ParamTypeIdent := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikType]);
    ParamIdent := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikUndeclared, ikNoFar]);
    // Increase the array's size
    SetLength(Params, ParamIndex);

    // Add the parameter to the function's parameter list
    Func.Prototype.Parameters.Add(ParamTypeIdent.FinalType);
    Params[ParamIndex-1].ParamName := ParamIdent.FullStr;
    Params[ParamIndex-1].ParamType := ParamTypeIdent.FinalType;

    Inc(ParamIndex);
  end;

  // Add the parameters to the local table, in reverse order
  for I := High(Params) downto Low(Params) do
  begin
    FTable.AddParameterIdentifier(Params[I].ParamName, I-Length(Params), Params[I].ParamType);
  end;

  ExpectSymbol([tsCloseBracket]);
  Proceed;

  // Setup scoop and table stack
  FCurrentScope := isLocal;
  FCurrentFunctionTableStack := FTableSizes.Count;

  LocalOffset := 0;
  EmbedHint('func:start');
  SaveTable;
  Statement(LocalOffset);
  FCurrentScope := isGlobal;
  EmbedHint('func:end');

  // Restore the table - one time for the things happening in the statement and
  // one time for the parameters
  RestoreTable(LocalOffset, True);
  RestoreTable(LocalOffset, False);

  // Ensure a return value is created
  if Func.Prototype.ReturnType <> nil then
  begin
    if not Func.Prototype.ReturnType.CanCreateNone(False, CreationInstruction) then
      CompilerError('Cannot create `none'' value of type `'+Func.Prototype.ReturnType.Name+'''.');
    GenCreation(CreationInstruction);
  end;
  GenCode(ret());

  // If public, add the function to the global function table.
  if AVisibility > vsPrivate then
    FPublicFunctions.Add(Func)
  else
    FPrivateFunctions.Add(Func);
end;

procedure TThoriumDefaultCompiler.JumpingLogicalExpression(TrueJumps,
  FalseJumps: TThoriumIntList);
(*
  @parserContext: Expects current symbol to be a bracket or Factor compatible.
*)
var
  Target: TThoriumInstructionAddress;
  I: Integer;
begin
  Assert(TrueJumps <> nil);
  Assert(FalseJumps <> nil);

  if FCurrentSym = tsNot then
  begin
    // Thats tricky. This must only be parsed as logical not if it is followed
    // by an opening bracket. In the other case, we MUST jump back one step in
    // the parser, as it needs to be taken as bitwise not otherwise.
    CompilerError('Internal error: This construct needs a parser stack.');
  end;

  JumpingLogicalTerm(TrueJumps, FalseJumps);

  // And after that any further terms
  while FCurrentSym in THORIUM_DEFAULT_LOGICAL_EXPRESSION_OPERATOR do
  begin
    EmbedHint('jmpexp:or');
    case FCurrentSym of
      tsOr:
      begin
        // Connect them with the correct operator
        Target := GetNextInstructionAddress;
        // This is OR. That means, if any operand is true, the whole expression
        // is true. So we will only redirect the false jumps to the next
        // expression. True jumps will be returned for the user.
        for I := 0 to FalseJumps.Count - 1 do
          TThoriumInstructionJMP(GetInstruction(FalseJumps[I])^).NewAddress := Target;
        FalseJumps.Clear;
      end;
    else
      CompilerError('Unhandled logical operator.');
    end;
    Proceed;
    // Read the next term
    JumpingLogicalTerm(TrueJumps, FalseJumps);
  end;
end;

procedure TThoriumDefaultCompiler.JumpingLogicalTerm(TrueJumps,
  FalseJumps: TThoriumIntList);
(*
  @parserContext: Expects current symbol to be a bracket or Factor compatible.
*)
var
  SubTrueJump, SubFalseJump, Target: TThoriumInstructionAddress;
  I: Integer;
begin
  Assert(TrueJumps <> nil);
  Assert(FalseJumps <> nil);

  if FCurrentSym = tsNot then
  begin
    // Thats tricky. This must only be parsed as logical not if it is followed
    // by an opening bracket. In the other case, we MUST jump back one step in
    // the parser, as it needs to be taken as bitwise not otherwise.
    CompilerError('Internal error: This construct needs a parser stack.');
  end;

  if FCurrentSym = tsOpenBracket then
  begin
    Proceed;
    JumpingLogicalExpression(TrueJumps, FalseJumps);
    ExpectSymbol([tsCloseBracket]);
    Proceed;
  end
  else
  begin
    // Read the relational expression with jumping extension
    JumpingRelationalExpression(SubTrueJump, SubFalseJump);
    // And add the jumps
    TrueJumps.AddEntry(SubTrueJump);
    FalseJumps.AddEntry(SubFalseJump);
  end;

  // Read more expressions
  while FCurrentSym in THORIUM_DEFAULT_LOGICAL_TERM_OPERATOR do
  begin
    EmbedHint('jmpexp:and');
    case FCurrentSym of
      tsAnd:
      begin
        Target := GetNextInstructionAddress;
        // Handle the AND operator. AND means: if any operand is false, the
        // whole expression is false. So we will only redirect the true jumps
        // to the next expressions. False jumps will be left untouched for
        // handling by the caller.
        for I := 0 to TrueJumps.Count - 1 do
          TThoriumInstructionJMP(GetInstruction(TrueJumps[I])^).NewAddress := Target;
        TrueJumps.Clear;
      end;
    else
      CompilerError('Unhandled logical operator.');
    end;
    Proceed;
    // Parse moar.
    if FCurrentSym = tsOpenBracket then
    begin
      Proceed;
      JumpingLogicalExpression(TrueJumps, FalseJumps);
      ExpectSymbol([tsCloseBracket]);
      Proceed;
    end
    else
    begin
      JumpingRelationalExpression(SubTrueJump, SubFalseJump);
      TrueJumps.AddEntry(SubTrueJump);
      FalseJumps.AddEntry(SubFalseJump);
    end;
  end;
end;

procedure TThoriumDefaultCompiler.JumpingRelationalExpression(out TrueJump,
  FalseJump: TThoriumInstructionAddress);
var
  Sym: TThoriumDefaultSymbol;
  OperandType1, OperandType2: TThoriumType;
  Operation: TThoriumOperationDescription;
  State1, State2: TThoriumValueState;
  Value1, Value2: TThoriumValue;
  RegID1, RegID2: TThoriumRegisterID;
begin
  GetFreeRegister(trEXP, RegID1);
  OperandType1 := SimpleExpression(RegID1, State1, @Value1);

  if FCurrentSym in THORIUM_DEFAULT_RELATIONAL_OPERATOR then
  begin
    GetFreeRegister(trEXP, RegID2);
    Sym := FCurrentSym;
    Proceed;

    OperandType2 := SimpleExpression(RegID2, State2, @Value2);
    Operation.Operation := SymbolToOperation(Sym);
    if not OperandType1.CanPerformOperation(Operation, OperandType2) then
      CompilerError('Invalid operands for this operation');

    if (State1 = vsStatic) and (State2 = vsStatic) then
    begin
      if TThoriumType.PerformCmpOperation(Value1, Operation, @Value2) then
      begin
        // First the true-jump so that the false-jump is never reached.
        TrueJump := GenCode(jmp(THORIUM_JMP_INVALID));
        FalseJump := GenCode(jmp(THORIUM_JMP_INVALID));
      end
      else
      begin
        // First the false-jump so that the true-jump is never reached.
        FalseJump := GenCode(jmp(THORIUM_JMP_INVALID));
        TrueJump := GenCode(jmp(THORIUM_JMP_INVALID));
      end;
      ReleaseRegister(RegID2);
      Exit;
    end;

    if State1 = vsStatic then
    begin
      PlaceStatic(Value1, RegID1);
      ThoriumFreeValue(Value1);
    end;

    if State2 = vsStatic then
    begin
      PlaceStatic(Value2, RegID2);
      ThoriumFreeValue(Value2);
    end;

    // Perform a real comparision and place the jumps accordingly.
    GenOperation(Operation, THORIUM_REGISTER_INVALID, RegID1, RegID2);
    case Operation.Operation of
      opCmpEqual:
      begin
        TrueJump := GenCode(je(THORIUM_JMP_INVALID));
        FalseJump := GenCode(jne(THORIUM_JMP_INVALID));
      end;
      opCmpNotEqual:
      begin
        TrueJump := GenCode(jne(THORIUM_JMP_INVALID));
        FalseJump := GenCode(je(THORIUM_JMP_INVALID));
      end;
      opCmpGreater:
      begin
        TrueJump := GenCode(jgt(THORIUM_JMP_INVALID));
        FalseJump := GenCode(jle(THORIUM_JMP_INVALID));
      end;
      opCmpGreaterOrEqual:
      begin
        TrueJump := GenCode(jge(THORIUM_JMP_INVALID));
        FalseJump := GenCode(jlt(THORIUM_JMP_INVALID));
      end;
      opCmpLess:
      begin
        TrueJump := GenCode(jlt(THORIUM_JMP_INVALID));
        FalseJump := GenCode(jge(THORIUM_JMP_INVALID));
      end;
      opCmpLessOrEqual:
      begin
        TrueJump := GenCode(jle(THORIUM_JMP_INVALID));
        FalseJump := GenCode(jgt(THORIUM_JMP_INVALID));
      end;
    else
      CompilerError('Unhandled compare operation.');
    end;
    ReleaseRegister(RegID2);
  end
  else
  begin
    // If there is no other operand, we need to evaluate this one standalone
    Operation.Operation := opEvaluate;
    if not OperandType1.CanPerformOperation(Operation) then
      CompilerError('Cannot evaluate '''+OperandType1.Name+''' to boolean.');

    // Handle that during compile time
    if State1 = vsStatic then
    begin
      if TThoriumType.PerformCmpOperation(Value1, Operation) then
      begin
        TrueJump := GenCode(jmp(THORIUM_JMP_INVALID));
        FalseJump := GenCode(jmp(THORIUM_JMP_INVALID));
      end
      else
      begin
        FalseJump := GenCode(jmp(THORIUM_JMP_INVALID));
        TrueJump := GenCode(jmp(THORIUM_JMP_INVALID));
      end;
    end
    else
    begin
      // Create a real comparision instruction and the jumps
      GenOperation(Operation, THORIUM_REGISTER_INVALID, RegID1);
      // These are just "je" and "jne" at the moment. Maybe this will change
      // later.
      TrueJump := GenCode(jt(THORIUM_JMP_INVALID));
      FalseJump := GenCode(jf(THORIUM_JMP_INVALID));
    end;
  end;
  ReleaseRegister(RegID1);
end;

procedure TThoriumDefaultCompiler.Module;
var
  IsStatic: Boolean;
  Visibility: TThoriumVisibilityLevel;
  Offset: Integer;
begin
  FCurrentScope := isGlobal;
  IsStatic := False;
  Offset := 0;
  Visibility := vsPrivate;
  Proceed;
  FHadGlobalVariable := False;
  FLastGlobalJump := GenCode(jmp(THORIUM_JMP_EXIT));
  while ExpectSymbol([tsLoadLibrary, tsLoadModule, tsIdentifier, tsPublic, tsPrivate, tsStatic], False) do
  begin
    case FCurrentSym of
      tsLoadModule:
      begin
        Proceed;
        LoadModule(ParseModuleName);
      end;
      tsLoadLibrary:
      begin
        Proceed;
        LoadLibrary(ParseLibraryName);
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
  if FHadGlobalVariable then
  begin
    GenCode(jmp(THORIUM_JMP_EXIT));
  end;
  ExpectSymbol([tsNone], True);
end;

function TThoriumDefaultCompiler.ParseLibraryName: String;
begin
  ExpectSymbol([tsStringValue, tsIdentifier]);
  if FCurrentSym = tsStringValue then
  begin
    Result := FCurrentStr;
    Proceed;
    if FCurrentSym = tsSemicolon then
      Proceed;
  end
  else if FCurrentSym = tsIdentifier then
  begin
    Result := FCurrentStr;
    Proceed;
    while FCurrentSym in [tsIdentifier, tsDot] do
    begin
      Result += FCurrentStr;
      Proceed;
    end;
    ExpectSymbol([tsSemicolon]);
    Proceed;
  end;
end;

function TThoriumDefaultCompiler.ParseModuleName: String;
begin
  ExpectSymbol([tsStringValue, tsIdentifier]);
  Result := FCurrentStr;
  Proceed;
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
    // Special handling for string again
    if StaticValue.Str^ <> '' then
      InitialData.Int := AddLibraryString(StaticValue.Str^)
    else
      InitialData.Int := -1;
    Assert(StaticValue.RTTI.CanCreate(InitialData, True, CreationDescription));
  end
  else
  begin
    // This must work for any type except string
    InitialData.Int := StaticValue.Int;
    Assert(StaticValue.RTTI.CanCreate(InitialData, True, CreationDescription));
  end;
  // Create the value to the specified register
  GenCreation(CreationDescription, ATargetRegister);
end;

function TThoriumDefaultCompiler.RelationalExpression(ATargetRegister: Word;
  out AState: TThoriumValueState; AStaticValue: PThoriumValue;
  ATypeHint: TThoriumType): TThoriumType;
var
  Sym: TThoriumDefaultSymbol;
  OperandType: TThoriumType;
  Operation: TThoriumOperationDescription;
  ResultValue: TThoriumValue;
  State1, State2: TThoriumValueState;
  Value1, Value2: TThoriumValue;
  RegID2: TThoriumRegisterID;
begin
  Result := SimpleExpression(ATargetRegister, State1, @Value1, ATypeHint);

  while FCurrentSym in THORIUM_DEFAULT_RELATIONAL_OPERATOR do
  begin
    GetFreeRegister(trEXP, RegID2);
    Sym := FCurrentSym;
    Proceed;

    OperandType := SimpleExpression(RegID2, State2, @Value2, Result);

    Operation.Operation := SymbolToOperation(Sym);
    if not Result.CanPerformOperation(Operation, OperandType) then
      CompilerError('Invalid operands for this operation.');

    // Attempt to evaluate static values during compile time
    if (State1 = vsStatic) and (State2 = vsStatic) then
    begin
      ResultValue.RTTI := FThorium.TypeInteger;
      if TThoriumType.PerformCmpOperation(Value1, Operation, @Value2) then
        ResultValue.Int := 1
      else
        ResultValue.Int := 0;
      ThoriumFreeValue(Value1);
      ThoriumFreeValue(Value2);
      Value1 := ResultValue;
      Result := ResultValue.RTTI;
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
    if not (Result is TThoriumTypeInteger) then
      Result := FThorium.TypeInteger;
    State1 := vsDynamic;

    ReleaseRegister(RegID2);
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
end;

function TThoriumDefaultCompiler.SimpleExpression(ATargetRegister: Word;
  out AState: TThoriumValueState; AStaticValue: PThoriumValue;
  ATypeHint: TThoriumType): TThoriumType;
var
  Sym: TThoriumDefaultSymbol;
  Operation: TThoriumOperationDescription;
  State1, State2: TThoriumValueState;
  Value1, Value2: TThoriumValue;
  RegID2: TThoriumRegisterID;
  OperandType: TThoriumType;
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
        ttLocal, ttGlobal, ttParameter, ttLocalRegisterVariable:
        begin
          case Entry^.Entry._Type of
            ttGlobal:
            begin

              if Entry^.SourceModule = FModule then
              begin
                // Belongs to this module
                AppendOperation(Solution^.GetCode,
                  ThoriumEncapsulateOperation(
                    movegOperation(Entry^.Entry.Offset, ATargetRegister),
                    ATargetRegister
                  )
                );
                AppendOperation(Solution^.SetCode,
                  ThoriumEncapsulateOperation(
                    copyr_gOperation(ATargetRegister, Entry^.Entry.Offset),
                    ATargetRegister
                  )
                );
              end
              else
              begin
                // Is from another module
                AppendOperation(Solution^.GetCode,
                  ThoriumEncapsulateOperation(
                    movefgOperation(Entry^.Entry.Offset, ATargetRegister, Entry^.SourceModule),
                    ATargetRegister
                  )
                );
                AppendOperation(Solution^.SetCode,
                  ThoriumEncapsulateOperation(
                    copyr_fgOperation(ATargetRegister, Entry^.Entry.Offset, Entry^.SourceModule),
                    ATargetRegister
                  )
                );
              end;
            end;
            ttLocal:
            begin
              AppendOperation(Solution^.GetCode,
                ThoriumEncapsulateOperation(
                  movelOperation(Entry^.Entry.Offset, ATargetRegister),
                  ATargetRegister
                )
              );
              AppendOperation(Solution^.SetCode,
                ThoriumEncapsulateOperation(
                  copyr_lOperation(ATargetRegister, Entry^.Entry.Offset),
                  ATargetRegister
                )
              );
            end;
            ttParameter:
            begin
              AppendOperation(Solution^.GetCode,
                ThoriumEncapsulateOperation(
                  movepOperation(Entry^.Entry.Offset, ATargetRegister),
                  ATargetRegister
                )
              );
              AppendOperation(Solution^.SetCode,
                ThoriumEncapsulateOperation(
                  copyr_pOperation(ATargetRegister, Entry^.Entry.Offset),
                  ATargetRegister
                )
              );
            end;
            ttLocalRegisterVariable:
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
            end;
          end;

          Solution^.Kind := ikVariable;
          if not Entry^.Entry.Writable and Entry^.Entry.ValidValue then
          begin
            Solution^.State := vsStatic;
            Solution^.Value := Entry^.Entry.Value;
          end
          else
            Solution^.State := vsAccessable;
          Solution^.Writable := Entry^.Entry.Writable;
        end;

        ttGlobalCallable:
        begin
          Solution^.Kind := ikVariable;
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
        ttHostCallable:
        begin
          Solution^.Kind := ikVariable;
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
        ttLibraryProperty:
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
        ttLibraryConstant:
        begin
          Solution^.Kind := ikVariable;
          Solution^.Writable := False;
          Solution^.State := vsStatic;
          Solution^.Value := TThoriumLibraryConstant(Entry^.Entry.Ptr).Value;
        end;
        ttType:
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

  ExprType: TThoriumType;
  ExprState: TThoriumValueState;
  RegPreviousValue, RegID1: TThoriumRegisterID;

  Solution: PThoriumQualifiedIdentifier;

  procedure SolveCall;
  var
    Parameters: TThoriumTypes;
    ParameterRegIDs: TThoriumIntList;
    ParameterCastLocations, ParameterReadbackLocations: TThoriumIntList;
    ParamRegID, CastRegID, NativeRegID: TThoriumRegisterID;
    State: TThoriumValueState;
    DynamicParameterList, NativeParameterList: TThoriumIntList;
    I, J: Integer;
    Scores: array of Integer;
    Callable: IThoriumCallable;
    HostCallable: IThoriumHostCallable;
    CallableParameters: TThoriumParameters;
    Assignment: TThoriumAssignmentDescription;
    HighestScore: Integer;
    HighestScoreIdx: Integer;
    ParamType: TThoriumType;
    DynIdx: Integer;
    NeedsNativeData: Boolean;
    Operation: TThoriumOperationDescription;
    ParamTypeInfo: PTypeInfo;
  begin
    // Prefilter
    I := Entries.Count - 1;
    while I >= 0 do
    begin
      Entry := Entries[I];
      if not Entry^.Entry.TypeSpec.CanCall then
        Discard(I)
      else if Entry^.Entry.TypeSpec.QueryInterface(IThoriumCallable, Callable) <> S_OK then
        CompilerError('Type '''+Entry^.Entry.TypeSpec.Name+''' presents itself as callable, but does not implement IThoriumCallable.')
      else
        Callable := nil;
      Dec(I);
    end;
    if Entries.Count = 0 then
      CompilerError('Cannot call this.');

    Parameters := TThoriumTypes.Create;
    DynamicParameterList := TThoriumIntList.Create;
    ParameterCastLocations := TThoriumIntList.Create;
    ParameterRegIDs := TThoriumIntList.Create;
    try
      AttachHook;
      if FCurrentSym <> tsCloseBracket then
      begin
        repeat
          if FCurrentSym = tsComma then
            Proceed;
          EmbedHint('call:param');
          GetFreeRegister(trEXP, ParamRegID);
          ParamType := RelationalExpression(ParamRegID, State);
          Parameters.Add(ParamType);
          ParameterRegIDs.AddEntry(ParamRegID);
          // These are to insert casts later. Will be removed by
          // Instructions.Finish if not needed
          ParameterCastLocations.AddEntry(GenCode(noop(THORIUM_NOOPMARK_PLACEHOLDER, 0, 0, 0)));
          GenCode(noop(THORIUM_NOOPMARK_PLACEHOLDER, 0, 0, 0));
          GenCode(noop(THORIUM_NOOPMARK_PLACEHOLDER, 0, 0, 0));
          GenCode(mover_st(ParamRegID));
          if (State = vsDynamic) and (ParamType.NeedsClear) then
            DynamicParameterList.AddEntry(ParamRegID);
        until FCurrentSym <> tsComma;
      end;
      ExpectSymbol([tsCloseBracket]);
      // Don't proceed here - thats done in the while loop

      SetLength(Scores, Solutions.Count);
      for I := 0 to High(Scores) do
      begin
        Entry := Entries[I];
        Solution := Solutions[I];

        // This was checked in advance
        Entry^.Entry.TypeSpec.QueryInterface(IThoriumCallable, Callable);

        CallableParameters := Callable.GetParameters;
        if CallableParameters.Count <> Parameters.Count then
        begin
          Scores[I] := -10000;
          Continue;
        end;

        Assignment.Casting := True;
        for J := 0 to Parameters.Count - 1 do
        begin
          if CallableParameters[J].IsEqualTo(Parameters[J]) then
            Scores[I] += 10
          else if Parameters[J].CanAssignTo(Assignment, CallableParameters[J]) then
          begin
            if Assignment.Cast.Needed then
              Scores[I] += 5
            else
              Scores[I] += 10;
          end
          else
          begin
            Scores[I] := -10000;
            Break;
          end;
        end;
      end;

      HighestScore := -1;
      HighestScoreIdx := -1;
      I := Entries.Count - 1;
      while I >= 0 do
      begin
        if Scores[I] < 0 then
          Discard(I)
        else if Scores[I] > HighestScore then
        begin
          HighestScore := Scores[I];
          HighestScoreIdx := I;
        end;
        Dec(I);
      end;

      if Entries.Count = 0 then
        CompilerError('Cannot call this with that combination of paramters.');

      I := Entries.Count - 1;
      while I >= 0 do
      begin
        if I <> HighestScoreIdx then
          Discard(I);
        Dec(I);
      end;

      Assert(Entries.Count = 1);

      Entry := Entries[0];
      Solution := Solutions[0];
      Entry^.Entry.TypeSpec.QueryInterface(IThoriumCallable, Callable);
      HostCallable := nil;
      Entry^.Entry.TypeSpec.QueryInterface(IThoriumHostCallable, HostCallable);
      NeedsNativeData := (HostCallable <> nil) and (HostCallable.NeedsNativeData);

      CallableParameters := Callable.GetParameters;

      for I := 0 to Parameters.Count - 1 do
      begin
        Assignment.Casting := True;
        Parameters[I].CanAssignTo(Assignment, CallableParameters[I]);
        if Assignment.Cast.Needed then
        begin
          GetFreeRegister(trEXP, CastRegID);
          Assignment.Cast.Instruction.SRI := ParameterRegIDs[I];
          Assignment.Cast.Instruction.TRI := CastRegID;
          GetInstruction(ParameterCastLocations[I])^ := TThoriumInstruction(Assignment.Cast.Instruction);
          TThoriumInstructionMOVER_ST(GetInstruction(ParameterCastLocations[I]+3)^).SRI := CastRegID;
          DynIdx := DynamicParameterList.FindValue(ParameterRegIDs[I]);
          if DynIdx >= 0 then
          begin
            GetInstruction(ParameterCastLocations[I]+1)^ := clr(ParameterRegIDs[I]);
            DynamicParameterList.DeleteEntry(DynIdx);
          end;
          if not Assignment.Cast.TargetType.NeedsClear then
            ReleaseRegister(CastRegID)
          else
            DynamicParameterList.AddEntry(CastRegID);
          ParameterRegIDs[I] := CastRegID;
        end;
        if NeedsNativeData then
        begin
          // ParameterRegIDs has been modified before
          Operation.Operation := opToNative;
          ParamTypeInfo := HostCallable.GetHostParameters().Parameters[I];
          if not Parameters[I].CanPerformOperation(Operation, nil, '', ParamTypeInfo) then
            CompilerError(Format('Cannot convert ''%s'' to native data of type ''%s''.', [Entry^.Entry.TypeSpec.Name, ParamTypeInfo^.Name]));
          Operation.OperationInstruction.Instruction.Reserved[Operation.OperationInstruction.TargetRIOffset] := ParameterRegIDs[I];
          GetInstruction(ParameterCastLocations[I]+2)^ := TThoriumInstruction(Operation.OperationInstruction.Instruction);
        end;
      end;

      FlushHook(False);

      Operation.Operation := opCall;
      Entry^.Entry.TypeSpec.CanPerformOperation(Operation);

      AppendOperation(Solution^.GetCode, ThoriumEncapsulateOperation(Operation, ATargetRegister, GetHighestRegisterInUse));
      AppendOperation(Solution^.SetCode, ThoriumEncapsulateOperation(Operation, ATargetRegister, GetHighestRegisterInUse));


      if NeedsNativeData then
      begin
        for I := 0 to Parameters.Count - 1 do
        begin
          Operation.Operation := opFreeNative;
          if Parameters[I].CanPerformOperation(Operation) then
          begin
            // Ignore if nothing can be done here. This may be on purpose.
            Operation.OperationInstruction.Instruction.Reserved[Operation.OperationInstruction.TargetRIOffset] := ParameterRegIDs[I];
            GenCodeToOperation(Solution^.GetCode, TThoriumInstruction(Operation.OperationInstruction.Instruction));
            GenCodeToOperation(Solution^.SetCode, TThoriumInstruction(Operation.OperationInstruction.Instruction));
          end;
        end;
      end;
      for I := 0 to DynamicParameterList.Count - 1 do
      begin
        GenCodeToOperation(Solution^.GetCode, clr(DynamicParameterList[I]));
        GenCodeToOperation(Solution^.SetCode, clr(DynamicParameterList[I]));
      end;

      if Entry^.Entry.TypeSpec.NeedsClear then
      begin
        GenCodeToOperation(Solution^.GetCode, clr(ATargetRegister));
        GenCodeToOperation(Solution^.SetCode, clr(ATargetRegister));
      end;

      if Callable.GetReturnType <> nil then
      begin;
        GenCodeToOperation(Solution^.GetCode, movest(ATargetRegister));
        GenCodeToOperation(Solution^.SetCode, movest(ATargetRegister));
      end;

      Solution^.FinalType := Callable.GetReturnType;
      Solution^.FullStr += '()';
    finally
      for I := 0 to ParameterRegIDs.Count - 1 do
        ReleaseRegister(ParameterRegIDs[I]);
      ParameterRegIDs.Free;
      ParameterCastLocations.Free;
      DynamicParameterList.Free;
      Parameters.Free;
    end;
  end;

begin
  ExpectSymbol([tsIdentifier]);

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
  EntriesHandle := nil;
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
          ExprType := RelationalExpression(RegID1, ExprState);
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

          SolveCall;
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
  const AllowedStatements: TThoriumDefaultStatementKinds;
  DisableSemicolon: Boolean);
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
        WriteLn('early out statement block');
        Proceed;
        Exit;
      end;

      SaveTable;
      Statement(Offset);
      while FCurrentSym <> tsCloseCurlyBracket do
        Statement(Offset);
      EmbedHint('block:cleanup');
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
      NeedSemicolon := True;
    end;
    tsSemicolon:
    begin
      NeedSemicolon := True;
    end;
    tsSwitch:
    begin
      if not (tskSwitch in AllowedStatements) then
        CompilerError('Switch statement not allowed here.');
      StatementSwitch(Offset);
      NeedSemicolon := False;
    end;
    tsBreak:
    begin
      if not (tskBreak in AllowedStatements) then
        CompilerError('Break statement is not allowed here.');
      GenBreak;
      Proceed;
      NeedSemicolon := True;
    end;
    tsReturn:
    begin
      if not (tskReturn in AllowedStatements) then
        CompilerError('Return statement is not allowed here.');
      StatementReturn(Offset);
      NeedSemicolon := True;
    end
  else
    ExpectSymbol([tsUnknown]);
  end;
  if NeedSemicolon and not DisableSemicolon then
  begin
    ExpectSymbol([tsSemicolon]);
    Proceed;
  end;
end;

procedure TThoriumDefaultCompiler.StatementDoWhile(var Offset: Integer);
var
  PreStatement, PostLoop: TThoriumInstructionAddress;
  TrueJumps, FalseJumps, BreakJumps: TThoriumIntList;
begin
  BreakJumps := nil;
  TrueJumps := TThoriumIntList.Create;
  FalseJumps := TThoriumIntList.Create;
  try
    PreStatement := GetNextInstructionAddress;

    EmbedHint('do-while');
    Proceed;
    PushBreakContext;
    Statement(Offset);
    BreakJumps := PopBreakContext;
    EmbedHint('do-while:while');

    ExpectSymbol([tsWhile]);
    Proceed;
    JumpingLogicalExpression(TrueJumps, FalseJumps);
    SetupJumps(TrueJumps, PreStatement);
    PostLoop := GetNextInstructionAddress;
    SetupJumps(FalseJumps, PostLoop);
    SetupJumps(BreakJumps, PostLoop);
  finally
    BreakJumps.Free;
    FalseJumps.Free;
    TrueJumps.Free;
  end;
end;

procedure TThoriumDefaultCompiler.StatementFor(var Offset: Integer);
var
  UseBrackets: Boolean;
  TypeIdent, Ident: TThoriumQualifiedIdentifier;
  RegID: TThoriumRegisterID;
  OldHook: Boolean;
  OldHook1, OldHook2: PThoriumInstructionArray;
  AfterLoopCode: TThoriumInstructionArray;

  PreLoop, PostLoop: TThoriumInstructionAddress;
  TrueJumps, FalseJumps, BreakJumps: TThoriumIntList;
begin
  Proceed;
  if FCurrentSym = tsOpenBracket then
  begin
    UseBrackets := True;
    Proceed;
  end
  else
    UseBrackets := False;

  TypeIdent := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikType]);
  Ident := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikUndeclared, ikNoFar]);

  SaveTable;
  PushBreakContext;
  GetFreeRegister(trEXP, RegID);
  EmbedHint('for:init');
  VariableDeclaration(vsPrivate, TypeIdent, Ident, Offset, RegID);

  if not ExpectSymbol([tsSemicolon]) then
    Exit;
  Proceed;

  TrueJumps := TThoriumIntList.Create;
  FalseJumps := TThoriumIntList.Create;
  BreakJumps := nil;
  try
    EmbedHint('for:cond');
    PreLoop := GetNextInstructionAddress;
    JumpingLogicalExpression(TrueJumps, FalseJumps);

    if not ExpectSymbol([tsSemicolon]) then
      Exit;
    Proceed;

    OldHook := FCodeHook;
    OldHook1 := FCodeHook1;
    OldHook2 := FCodeHook2;
    try
      FCodeHook := True;
      FCodeHook1 := @AfterLoopCode;
      FCodeHook2 := nil;

      EmbedHint('for:loopcmd');
      Statement(Offset, [tskAssignment], True);
    finally
      FCodeHook := OldHook;
      FCodeHook1 := OldHook1;
      FCodeHook2 := OldHook2;
    end;

    if UseBrackets then
    begin
      ExpectSymbol([tsCloseBracket]);
      Proceed;
    end;

    SetupJumps(TrueJumps, GetNextInstructionAddress);

    EmbedHint('for:body');
    Statement(Offset);
    BreakJumps := PopBreakContext;

    AppendCode(AfterLoopCode);

    GenCode(jmp(PreLoop));

    PostLoop := GetNextInstructionAddress;
    SetupJumps(FalseJumps, PostLoop);
    SetupJumps(BreakJumps, PostLoop);

    RestoreTable(Offset);

    ReleaseRegister(RegID);
    EmbedHint('for:end');
  finally
    BreakJumps.Free;
    FalseJumps.Free;
    TrueJumps.Free;
  end;
end;

procedure TThoriumDefaultCompiler.StatementIdentifier(var Offset: Integer;
  const AllowedStatements: TThoriumDefaultStatementKinds);
var
  Ident1, Ident2: TThoriumQualifiedIdentifier;
  ExpressionType: TThoriumType;
  ExpressionState: TThoriumValueState;
  RegID1, RegID2: TThoriumRegisterID;
  Assignment: TThoriumAssignmentDescription;
begin
  // Identifier handling
  GetFreeRegister(trEXP, RegID1);
  Ident1 := SolveIdentifier(RegID1, [ikType, ikComplex, ikLibraryProperty, ikVariable]);
  case Ident1.Kind of
    ikType:
    begin
      // Local declaration
      // [Ident1] [Ident2] ( = expression)? (, [Ident2] ( = expression)?)*;
      repeat
        if FCurrentSym = tsComma then
          Proceed;
        Ident2 := SolveIdentifier(THORIUM_REGISTER_INVALID, [ikNoFar, ikUndeclared]);
        ExpectSymbol([tsAssign, tsSemicolon, tsComma]);
        EmbedHint('local');
        VariableDeclaration(vsPrivate, Ident1, Ident2, Offset);
        ExpectSymbol([tsSemicolon, tsComma]);
      until FCurrentSym <> tsComma;
    end;
  else
    // So see what we can do with it.
    ExpectSymbol([tsAssign, tsSemicolon]);
    case FCurrentSym of
      tsAssign:
      begin
        EmbedHint('assignment:right');
        // Handle an assignment.
        // [Ident1] = [RelationalExpression];
        if not (tskAssignment in AllowedStatements) then
          CompilerError('Assignment is not allowed here.');
        if not (Ident1.Writable) then
          CompilerError('Cannot write to the left side.');
        // Handle an assignment
        Proceed;
        // Allocate a register for the RelationalExpression evaluation
        GetFreeRegister(trEXP, RegID2);
        ExpressionType := SimpleExpression(RegID2, ExpressionState, nil, Ident1.FinalType);
        if ExpressionType = nil then
          CompilerError('Cannot assign None type to anything.');
        // Allow casting
        Assignment.Casting := True;
        // Check whether the RelationalExpression result can be assigned to the ident
        if not ExpressionType.CanAssignTo(Assignment, Ident1.FinalType) then
          CompilerError('Cannot assign '+ExpressionType.Name+' to '+Ident1.FinalType.Name);
        // Cast if neccessary
        if Assignment.Cast.Needed then
        begin
          EmbedHint('assignment:cast');
          // Assign the registers to the cast instruction
          Assignment.Cast.Instruction.SRI := RegID2;
          Assignment.Cast.Instruction.TRI := RegID1;
          // Generate the code
          GenCode(TThoriumInstruction(Assignment.Cast.Instruction));
        end
        else
        begin
          // Move the RelationalExpression result to the approprate register
          GenCode(mover(RegID2, RegID1));
        end;
        EmbedHint('assignment');
        AppendOperations(Ident1.SetCode);
        // Release the RelationalExpression register
        if (ExpressionState = vsDynamic) and (ExpressionType.NeedsClear) then
          GenCode(clr(RegID1));
        ReleaseRegister(RegID2);
      end;
      tsSemicolon:
      begin
        EmbedHint('call|null');
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
var
  TrueJumps, FalseJumps, JumpsOut: TThoriumIntList;
begin
  Proceed;
  TrueJumps := TThoriumIntList.Create;
  FalseJumps := TThoriumIntList.Create;
  JumpsOut := TThoriumIntList.Create;
  try
    EmbedHint('if:cond');
    JumpingLogicalExpression(TrueJumps, FalseJumps);
    SetupJumps(TrueJumps, GetNextInstructionAddress);

    EmbedHint('if:body');
    Statement(Offset);
    JumpsOut.AddEntry(GenCode(jmp(THORIUM_JMP_INVALID)));
    SetupJumps(FalseJumps, GetNextInstructionAddress);
    EmbedHint('if:body:end');

    while FCurrentSym = tsElseIf do
    begin
      Proceed;
      TrueJumps.Clear;
      FalseJumps.Clear;

      EmbedHint('if:elif:cond');
      JumpingLogicalExpression(TrueJumps, FalseJumps);
      SetupJumps(TrueJumps, GetNextInstructionAddress);

      EmbedHint('if:elif:body');
      Statement(Offset, THORIUM_DEFAULT_ALL_STATEMENTS);
      EmbedHint('if:elif:body:end');
      JumpsOut.AddEntry(GenCode(jmp(THORIUM_JMP_INVALID)));
      SetupJumps(FalseJumps, GetNextInstructionAddress);
    end;

    if FCurrentSym = tsElse then
    begin
      EmbedHint('if:else');
      Proceed;
      Statement(Offset, THORIUM_DEFAULT_ALL_STATEMENTS);
      EmbedHint('if:else:end');
    end;

    SetupJumps(JumpsOut, GetNextInstructionAddress);
  finally
    JumpsOut.Free;
    FalseJumps.Free;
    TrueJumps.Free;
  end;
end;

procedure TThoriumDefaultCompiler.StatementReturn(var Offset: Integer);
var
  RegID: TThoriumRegisterID;
  State: TThoriumValueState;
  Assignment: TThoriumAssignmentDescription;
  ExprType: TThoriumType;
  Tmp: Integer;
begin
  Proceed;
  GetFreeRegister(trEXP, RegID);

  Assignment.Casting := True;
  ExprType := RelationalExpression(RegID, State, nil, FCurrentFunc.Prototype.ReturnType);
  if not ExprType.CanAssignTo(Assignment, FCurrentFunc.Prototype.ReturnType) then
    CompilerError('Incompatible types: `'+ExprType.Name+''' (expression) and `'+FCurrentFunc.Prototype.ReturnType.Name+''' (return type).');

  PopAndClearByTable(FTableSizes[FCurrentFunctionTableStack + 1]);
  case State of
    vsDynamic: GenCode(copyr_st(RegID));
    vsAccessable: GenCode(copyr_st(RegID));
    vsStatic: GenCode(mover_st(RegID));
  else
    raise EThoriumCompilerException.Create('Invalid value state.');
  end;
  GenCode(ret());
  ReleaseRegister(RegID);
end;

procedure TThoriumDefaultCompiler.StatementSwitch(var Offset: Integer);
var
  SwitchRegID, CaseRegID: TThoriumRegisterID;
  SwitchType, CaseType: TThoriumType;
  SwitchState, CaseState: TThoriumValueState;
  CaseValue: TThoriumValue;
  Operation: TThoriumOperationDescription;
  OldHook: Boolean;
  OldHook1, OldHook2: PThoriumInstructionArray;
  ComparisionBuffer: TThoriumInstructionArray;
  HasDefault: Boolean;
  PreStatements, PostStatements, DefaultPos: TThoriumInstructionAddress;
  BreakJumps: TThoriumIntList;
begin
  if FCodeHook then
    CompilerError('Cannot create a switch statement here.');

  GetFreeRegister(trEXP, SwitchRegID);
  GetFreeRegister(trEXP, CaseRegID);

  SaveTable;

  ExpectSymbol([tsSwitch]);
  Proceed;

  EmbedHint('switch:expression');
  SwitchType := RelationalExpression(SwitchRegID, SwitchState);

  ExpectSymbol([tsOpenCurlyBracket]);
  Proceed;

  PreStatements := GetNextInstructionAddress;

  PushBreakContext;

  while FCurrentSym = tsCase do
  begin
    Proceed;
    OldHook := FCodeHook;
    OldHook1 := FCodeHook1;
    OldHook2 := FCodeHook2;
    try
      FCodeHook := True;
      FCodeHook1 := @ComparisionBuffer;
      FCodeHook2 := nil;

      EmbedHint('switch:case:cmp');
      CaseType := RelationalExpression(CaseRegID, CaseState, @CaseValue, SwitchType);
      if CaseState <> vsStatic then
        CompilerError('Illegal value for case statement (must be static).');

      Operation.Operation := opCmpEqual;
      if not SwitchType.CanPerformOperation(Operation, CaseType) then
        CompilerError('Cannot compare '''+SwitchType.Name+''' and '''+CaseType.Name+''' for equality.');

      PlaceStatic(CaseValue, CaseRegID);

      GenOperation(Operation, THORIUM_REGISTER_INVALID, SwitchRegID, CaseRegID);
      // Force the usage of FInstructions.Position
      GenCode(je(FInstructions.Position));

      if CaseType.NeedsClear then
      begin
        GenCode(clr(CaseRegID));
        ThoriumFreeValue(CaseValue);
      end;

    finally
      FCodeHook2 := OldHook2;
      FCodeHook1 := OldHook1;
      FCodeHook := OldHook;
    end;

    ExpectSymbol([tsColon]);
    Proceed;

    EmbedHint('switch:case:body');
    while not (FCurrentSym in [tsCase, tsCloseCurlyBracket, tsDefault]) do
      Statement(Offset);
  end;

  if FCurrentSym = tsDefault then
  begin
    HasDefault := True;
    DefaultPos := GetNextInstructionAddress;
    Proceed([tsColon]);
    Proceed;
    EmbedHint('switch:default');
    while not (FCurrentSym in [tsCloseCurlyBracket]) do
      Statement(Offset);
  end
  else
    HasDefault := False;

  ExpectSymbol([tsCloseCurlyBracket]);
  Proceed;

  PostStatements := GetNextInstructionAddress;
  FInstructions.AddInstructionPointer(@PostStatements);

  FInstructions.Position := PreStatements;
  AppendCode(ComparisionBuffer);
  if HasDefault then
    GenCode(jmp(DefaultPos))
  else
    GenCode(jmp(PostStatements));
  FInstructions.Position := FInstructions.Count;
  FInstructions.RemoveInstructionPointer(@PostStatements);

  BreakJumps := PopBreakContext;
  SetupJumps(BreakJumps, GetNextInstructionAddress);

  if (SwitchState = vsDynamic) and (SwitchType.NeedsClear) then
    GenCode(clr(SwitchRegID));

  EmbedHint('switch:end');

  RestoreTable(Offset);

  ReleaseRegister(CaseRegID);
  ReleaseRegister(SwitchRegID);
end;

procedure TThoriumDefaultCompiler.StatementWhile(var Offset: Integer);
var
  PreCondition, PostStatements: TThoriumInstructionAddress;
  TrueJumps, FalseJumps, BreakJumps: TThoriumIntList;
begin
  BreakJumps := nil;
  TrueJumps := TThoriumIntList.Create;
  FalseJumps := TThoriumIntList.Create;
  try
    PreCondition := GetNextInstructionAddress;
    Proceed;
    EmbedHint('while');
    JumpingLogicalExpression(TrueJumps, FalseJumps);
    SetupJumps(TrueJumps, GetNextInstructionAddress);

    EmbedHint('while:body');
    PushBreakContext;
    Statement(Offset);
    BreakJumps := PopBreakContext;

    EmbedHint('while:end');

    GenCode(jmp(PreCondition));
    PostStatements := GetNextInstructionAddress;
    SetupJumps(FalseJumps, PostStatements);
    SetupJumps(BreakJumps, PostStatements);
  finally
    BreakJumps.Free;
    FalseJumps.Free;
    TrueJumps.Free;
  end;
end;

function TThoriumDefaultCompiler.Term(ATargetRegister: Word; out
  AState: TThoriumValueState; out AStaticValue: TThoriumValue;
  ATypeHint: TThoriumType): TThoriumType;
var
  State1, State2: TThoriumValueState;
  Value1, Value2: TThoriumValue;
  Sym: TThoriumDefaultSymbol;
  RegID2: TThoriumRegisterID;
  OperandType: TThoriumType;
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

    // Attempt to evaluate the RelationalExpression during compilation
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

procedure TThoriumDefaultCompiler.VariableDeclarationCBC(
  const AVisibility: TThoriumVisibilityLevel; ATypeIdent,
  AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer);
// CallBack Compatible (thus CBC) variant of VariableDeclaration
begin
  VariableDeclaration(AVisibility, ATypeIdent, AValueIdent, Offset);
end;

procedure TThoriumDefaultCompiler.VariableDeclaration(
  const AVisibility: TThoriumVisibilityLevel; ATypeIdent,
  AValueIdent: TThoriumQualifiedIdentifier; var Offset: Integer;
  const ARegID: TThoriumRegisterID);
var
  State: TThoriumValueState;
  Value: TThoriumValue;
  InitialData: TThoriumInitialData;
  Entry: PThoriumTableEntry;
  Creation: TThoriumCreateInstructionDescription;
begin
  EmbedHint('var');
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
    if not ATypeIdent.FinalType.CanCreate(InitialData, ARegID <> THORIUM_REGISTER_INVALID, Creation) then
      CompilerError('Cannot create such a value.');
    ThoriumFreeValue(Value);
    GenCreation(Creation, ARegID);
  end
  else
  begin
    if not ATypeIdent.FinalType.CanCreateNone(ARegID <> THORIUM_REGISTER_INVALID, Creation) then
      CompilerError('Need an initial value, as `None'' creation failed.');
    GenCreation(Creation, ARegID);
  end;
  ExpectSymbol([tsSemicolon, tsComma]);

  if ARegID <> THORIUM_REGISTER_INVALID then
  begin
    Entry := FTable.AddRegisterVariableIdentifier(AValueIdent.FullStr, ARegID, ATypeIdent.FinalType);
    if AVisibility = vsPublic then
      CompilerError('Register variables cannot be public.');
  end
  else
  begin
    Entry := FTable.AddVariableIdentifier(AValueIdent.FullStr, Offset, ATypeIdent.FinalType, FCurrentScope = isGlobal, True);
    if FCurrentScope = isGlobal then
      Inc(FGlobalValueCount^);
    Inc(Offset);
    if AVisibility = vsPublic then
      AddPublicVariable(AValueIdent.FullStr).AssignFromTableEntry(Entry^);
  end;
end;

function TThoriumDefaultCompiler.CompileFromStream(SourceStream: TStream;
  Flags: TThoriumCompilerFlags): Boolean;
begin
  ResetState;
  FScanner := TThoriumDefaultScanner.Create(SourceStream);
  try
    try
      // Embed some compiler metadata
      EmbedMetadata('compiler=th234.sotecware.net');
      // Attempt to compile the module
      Module;
    finally

    end;
    FInstructions.Finish;
  finally
    FScanner.Free;
    Result := not HasError;
  end;
end;

end.

