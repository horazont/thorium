unit Thorium_DefaultCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals, Thorium_Utils;

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
    function ExpectSymbol(SymbolMask: TThoriumDefaultSymbols; ThrowError: Boolean = True): Boolean;
    function GenCode(AInstruction: TThoriumInstruction): Integer; override;
    procedure Proceed;
  protected
    procedure GenericDeclaration(IsStatic: Boolean;
      VisibilityLevel: TThoriumVisibilityLevel; var Offset: Integer);
    procedure Module;
    function QualifyIdentifier(out Ident: TThoriumQualifiedIdentifier;
      AllowedKinds: TThoriumQualifiedIdentifierKinds;
      TargetRegister: TThoriumRegisterID): Boolean;
  public
    function CompileFromStream(SourceStream: TStream;
       Flags: TThoriumCompilerFlags=[cfOptimize]): Boolean; override;
  end;

implementation

{$I Thorium_InstructionConstructors.inc}

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
  inherited CompilerError(Msg, FScanner.FX, FScanner.FLine);
end;

function TThoriumDefaultCompiler.ExpectSymbol(
  SymbolMask: TThoriumDefaultSymbols; ThrowError: Boolean): Boolean;
var
  SymStr: String;
begin
  Result := (FCurrentSym in SymbolMask);
  if (not Result) and (ThrowError) then
  begin
    SymStr := THORIUM_DEFAULT_SYMBOL_CODE[FCurrentSym];
    if (FCurrentSym = tsUnknown) then
      SymStr := SymStr + '(''' + FCurrentStr + ''')';
    if SymbolMask <> [tsNone] then
    begin
      case FCurrentSym of
        tsIdentifier: CompilerError('Unexpected symbol: '+SymStr+' ('''+FCurrentStr+''')');
      else
        CompilerError('Unexpected symbol: '+SymStr);
      end;
    end
    else
      CompilerError('Unexpected symbol: '+SymStr+' (expected end of stream).');
  end;
end;

function TThoriumDefaultCompiler.GenCode(AInstruction: TThoriumInstruction
  ): Integer;
begin
  Result := inherited GenCode(AInstruction, FScanner.FLine);
end;

procedure TThoriumDefaultCompiler.Proceed;
begin
  FScanner.NextSymbol(FCurrentSym, FCurrentStr);
end;

procedure TThoriumDefaultCompiler.GenericDeclaration(IsStatic: Boolean;
  VisibilityLevel: TThoriumVisibilityLevel; var Offset: Integer);
// Detect which kind of declaration is meant here and call the appropriate
// function
var
  TypeIdent, Ident: TThoriumQualifiedIdentifier;
  IsFunc: Boolean;
  Pos: Integer;
begin
  if not QualifyIdentifier(TypeIdent, [ikType], 0) then
    Exit;
  AddHostTypeUsages(TypeIdent.UsedExtendedTypes);
end;

procedure TThoriumDefaultCompiler.Module;
// This method compiles a whole module. It must not be called from any other
// method than CompileFromStream.
var
  Offset: Integer;
  VisibilityLevel: TThoriumVisibilityLevel;
  LastJump: Integer;
  HadDeclarations: Boolean;
begin
  // Setup the initial stack scope and initialize local variables
  FCurrentScope := THORIUM_STACK_SCOPE_MODULEROOT;
  Offset := 0;
  VisibilityLevel := vsPrivate;

  // Generate an exit jump for the initialization code. Later this may be
  // changed when global variable declarations occur.
  LastJump := GenCode(jmp(THORIUM_JMP_EXIT));
  HadDeclarations := False;

  // Go through the given code. While there is a declaration, attempt to parse
  // it. Exiting the loop means either expected end of file or a compiler error.
  while (FCurrentSym in [tsPublic, tsPrivate, tsStatic, tsIdentifier, tsLoadLibrary, tsLoadModule]) and (not HasError) do
  begin
    // Check which symbol has been found.
    case FCurrentSym of
      tsPublic:
      begin
        // Set the next visibility level to public and continue.
        VisibilityLevel := vsPublic;
        Proceed;
      end;
      tsPrivate:
      begin
        // Set the next visibility level to private and continue.
        VisibilityLevel := vsPrivate;
        Proceed;
      end;
      tsStatic:
      begin
        Proceed;
        // Process a static declarataion
        GenericDeclaration(True, VisibilityLevel, Offset);
        VisibilityLevel := vsPrivate;
      end;
      tsIdentifier:
      begin
        // Process a non-static declaration
        GenericDeclaration(False, VisibilityLevel, Offset);
        VisibilityLevel := vsPrivate;
      end;
      tsLoadLibrary:
      begin
        repeat
          Proceed;
          if not ExpectSymbol([tsStringValue]) then
            Exit;
          LoadLibrary(FCurrentStr);
          if HasError then
            Exit;
          Proceed;
        until FCurrentSym <> tsComma;
      end;
      tsLoadModule:
      begin
        repeat
          Proceed;
          if not ExpectSymbol([tsStringValue]) then
            Exit;
          LoadModule(FCurrentStr);
          if HasError then
            Exit;
          Proceed;
        until FCurrentSym <> tsComma;
      end;
    end;
  end;
  if FCurrentSym = tsError then
    CompilerError('Tokenzier error: '+FCurrentStr)
  else if (FCurrentSym <> tsNone) and (not HasError) then
    ExpectSymbol([tsNone]);
end;

function TThoriumDefaultCompiler.QualifyIdentifier(out
  Ident: TThoriumQualifiedIdentifier;
  AllowedKinds: TThoriumQualifiedIdentifierKinds;
  TargetRegister: TThoriumRegisterID): Boolean;
// Qualifies a whole identifier including array index access and stuff. This
// is a quite complex process.
var
  RegID: TThoriumRegisterID;
  OldHook: Boolean;
  OldHook1: PThoriumInstructionArray;
  OldHook2: PThoriumInstructionArray;

  CurrIdent: String;
  CurrEntry: TThoriumTableEntry;
  CurrModule: TThoriumModule;
  CurrType: IThoriumType;
  Prop: TThoriumLibraryProperty;
  LibConst: TThoriumLibraryConstant;
begin
  Result := False;
  if not ExpectSymbol([tsIdentifier]) then
    Exit;
  // Save hook state
  OldHook := FCodeHook;
  OldHook1 := FCodeHook1;
  OldHook2 := FCodeHook2;
  try
    // Prepare and install own hooks
    SetLength(Ident.GetCode, 0);
    SetLength(Ident.SetCode, 0);
    FCodeHook := True;
    FCodeHook1 := @Ident.GetCode;
    FCodeHook2 := @Ident.SetCode;

    // Save the current identifier to work with it
    CurrIdent := FCurrentStr;
    Ident.FullStr := CurrIdent;
    Proceed;
    Result := FindTableEntry(CurrIdent, CurrEntry, CurrModule, False, not (ikNoFar in AllowedKinds));
    if not Result then
    begin
      // This can either be a type or an undeclared identifier.
      Result := TypeSpecByName(CurrIdent, CurrType);
      if not Result then
      begin
        // This is an undeclared identifier.
        if not (ikUndeclared in AllowedKinds) then
        begin
          CompilerError('Undeclared identifier: '''+CurrIdent+'''.');
          Exit(False);
        end;
        Ident.Kind := ikUndeclared;
        Exit(True);
      end;
      // Okay, type reference here.
      Ident.Kind := ikType;
      Ident.FinalType := CurrType;
    end
    else
    begin
      case CurrEntry._Type of
        etVariable, etRegisterVariable:
        begin
          Ident.Kind := ikVariable;
          Ident.FinalType := CurrEntry.TypeSpec;
          Ident.IsStatic := False;
          if CurrEntry._Type = etRegisterVariable then
          begin
            // It's one of the shiny-new register variables
            GenCodeEx(Ident.GetCode, mover(CurrEntry.Offset, TargetRegister));
            GenCodeEx(Ident.SetCode, mover(TargetRegister, CurrEntry.Offset));
          end
          else
          begin
            if CurrModule <> nil then
            begin
              // The value is located in another module, so use the far operations
(*              GenCodeEx(Ident.GetCode, movefs(.IndexOf(CurrModule), CurrEntry.Offset, TargetRegister));
              GenCodeEx(Ident.SetCode, mover_fs(TargetRegister, FThorium.IndexOf(CurrModule), CurrEntry.Offset));*)
            end
            else
            begin
              // Just an ordinary access to a local variable
              GenCodeEx(Ident.GetCode, moves(CurrEntry.Scope, CurrEntry.Offset, TargetRegister));
              GenCodeEx(Ident.SetCode, mover_s(TargetRegister, CurrEntry.Scope, CurrEntry.Offset));
            end;
          end;
        end;
        etStatic:
        begin
          Ident.Kind := ikStatic;
          Ident.IsStatic := True;
          Ident.FinalType := CurrEntry.TypeSpec;
          Ident.Value := CurrEntry.Value.TypeInfo.DuplicateValue(CurrEntry.Value);
          if CurrModule <> nil then
          begin
            // The value is located in another module, so use the far operations
//            GenCodeEx(Ident.GetCode, movefs(CurrModule, CurrEntry.Offset, TargetRegister));
            GenCodeEx(Ident.SetCode, noop(THORIUM_NOOPMARK_INVALID_ACCESS, 0, 0, 0));
          end
          else
          begin
            // Just an ordinary access to a local variable
//            GenCodeEx(Ident.GetCode, moves(CurrEntry.Scope, CurrEntry.Offset, TargetRegister));
            GenCodeEx(Ident.SetCode, noop(THORIUM_NOOPMARK_INVALID_ACCESS, 0, 0, 0));
          end;
        end;
        etCallable:
        begin
          Ident.Kind := ikVariable;
          if TThoriumFunction(CurrEntry.TypeSpec.GetInstance).Prototyped then
            Ident.Kind := ikPrototypedFunction;
          Ident.IsStatic := True;
        end;
        etHostCallable:
        begin
          raise Exception.Create('Not re-implemented yet.');
        end;
        etProperty:
        begin
          Ident.Kind := ikLibraryProperty;
          Prop := TThoriumLibraryProperty(CurrEntry.Ptr);
          Ident.IsStatic := Prop.GetStatic;
          Ident.FinalType := Prop.GetType;
          GenCodeEx(Ident.GetCode, xpget(Prop, TargetRegister));
          if Ident.IsStatic then
            GenCodeEx(Ident.SetCode, noop(THORIUM_NOOPMARK_INVALID_ACCESS, 0, 0, 0))
          else
            GenCodeEx(Ident.SetCode, xpset(Prop, TargetRegister));
        end;
        etLibraryConstant:
        begin
          Ident.Kind := ikStatic;
          LibConst := TThoriumLibraryConstant(CurrEntry.Ptr);
          Ident.IsStatic := True;
          Ident.FinalType := LibConst.Value.TypeInfo;
          Ident.Value := LibConst.Value.TypeInfo.DuplicateValue(LibConst.Value);
          GenCodeEx(Ident.GetCode, noop(THORIUM_NOOPMARK_INVALID_ACCESS, 0, 0, 0));
          GenCodeEx(Ident.SetCode, noop(THORIUM_NOOPMARK_INVALID_ACCESS, 0, 0, 0));
        end;
      else
        raise EThoriumCompilerException.Create('Unknown table entry type in QualifyIdentifier.');
      end;
    end;

    while FCurrentSym in [tsDot, tsOpenBracket, tsOpenSquareBracket] do
    begin

      Proceed;
    end;
  finally
    // Restore hook state
    FCodeHook := OldHook;
    FCodeHook1 := OldHook1;
    FCodeHook2 := OldHook2;
  end;
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

