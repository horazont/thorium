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
    function Proceed(ExpectMask: TThoriumDefaultSymbols = []; ThrowError: Boolean = False): Boolean;
  protected
    procedure Module;
    function SolveIdentifier(TargetRegister: Word;
      AllowedKinds: TThoriumQualifiedIdentifierKinds): TThoriumQualifiedIdentifier;
  public
    function CompileFromStream(SourceStream: TStream;
       Flags: TThoriumCompilerFlags=[cfOptimize]): Boolean; override;
  end;

implementation

{$I Thorium_InstructionConstructors.inc}

function SymbolMaskToStr(const Mask: TThoriumDefaultSymbols): String;
var
  Sym: TThoriumDefaultSymbol;
begin
  Result := '';
  for Sym := Low(TThoriumDefaultSymbols) to High(TThoriumDefaultSymbols) do
    if Sym in Mask then
      Result += GetEnumName(TypeInfo(TThoriumDefaultSymbol), Ord(Sym))+', ';
  if Length(Result) > 0 then
    Delete(Result, Length(Result)-2, 2);
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

function TThoriumDefaultCompiler.Proceed(ExpectMask: TThoriumDefaultSymbols;
  ThrowError: Boolean): Boolean;
begin
  Result := FScanner.NextSymbol(FCurrentSym, FCurrentStr);
  if Result and (ExpectMask <> []) then
    Result := ExpectSymbol(ExpectMask, ThrowError);
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

function TThoriumDefaultCompiler.SolveIdentifier(TargetRegister: Word;
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
  begin
    for I := 0 to Entries.Count - 1 do
    begin
      New(Solution);
      FillByte(Solution^, SizeOf(TThoriumQualifiedIdentifier), 0);
      Solutions.Add(Solution);
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
  EntriesHandle: TThoriumTableEntryResults;
  I: Integer;
  Entry: PThoriumTableEntryResult;
begin
  Assert(FCurrentSym = tsIdentifier);

  Result.FinalType := nil;
  Result.GetCode := nil;
  Result.GetJumpMarks := nil;
  Result.SetCode := nil;
  Result.SetJumpMarks := nil;
  Result.UsedExtendedTypes := nil;
  Result.UsedLibraryProps := nil;
  Result.FullStr := FCurrentStr;

  Entries := TThoriumTableEntryResultList.Create;
  try
    FindTableEntries(FCurrentStr, EntriesHandle);
    for I := 0 to High(EntriesHandle) do
      Entries.Add(@EntriesHandle[I]);

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

    if Entries.Count = 0 then
    begin
      Result.Kind := ikUndeclared;
      Exit;
    end;

    while Proceed([tsDot, tsOpenSquareBracket, tsOpenBracket], False) do
    begin

    end;

  finally
    Entries.Free;
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

