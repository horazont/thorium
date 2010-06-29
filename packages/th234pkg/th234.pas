unit Th234;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals;

type
  TTh234Symbol = (thIndent,
    thIntegerValue,
    thStringValue,
    thFloatValue,
    thIdentifier,
    thOpenBracket,
    thCloseBracket,
    thOpenSquareBracket,
    thCloseSquareBracket,
    thOpenCurlyBracket,
    thCloseCurlyBracket,
    thOpenAngleBracket,
    thCloseAngleBracket,
    thComma,
    thDot,
    thSemicolon,
    thAsterisk,
    thSlash,
    thPlus,
    thMinus,
    thColon,
    thEqual,
    thGreater,
    thLesser,
    thGreaterEqual,
    thLesserEqual,
    thNotEqual,
    thAdditiveAssign,
    thSubtractiveAssign,
    thMultiplicativeAssign,
    thDivideAssign,
    thAnd,
    thOr,
    thXor,
    thIf,
    thElse,
    thElseIf,
    thSwitch,
    thCase,
    thTry,
    thFinally,
    thExcept,
    thImport,
    thDef,
    thReturn,
    thPass,
    thError,
    thNone);

  { TTh234Scanner }

  TTh234Scanner = class (TObject)
  public
    constructor Create(const InputString: String);
    constructor Create(const InputStream: TStream);
    destructor Destroy; override;
  private
    FInputPosition: LongInt;
    FInputSize: LongInt;
    FInputString: String;
    FInputHash: TThoriumHash;
    FCurrentSym: TTh234Symbol;
    FCurrentStr: String;
    FCurrentChar: Char;
    FCurrentLine: Integer;
    FCurrentX: Integer;
    FIsLineBreak: Boolean;
    FEndOfStream: Boolean;

    procedure Read(var C: Char); inline;
  protected
    procedure ScanForSymbol(var Sym: TTh234Symbol; var Str: String);
  public
    property CurrentSym: TTh234Symbol read FCurrentSym;
    property CurrentStr: String read FCurrentStr;
    property CurrentLine: Integer read FCurrentLine;
  end;

  { TTh234Compiler }

  TTh234Compiler = class (TThoriumCustomCompiler)
  public
    function CompileFromStream(SourceStream: TStream;
       Flags: TThoriumCompilerFlags=[cfOptimize]): Boolean; override;
  end;

implementation

{ TTh234Scanner }

constructor TTh234Scanner.Create(const InputString: String);
begin
  FInputString := InputString;
  FInputPosition := 0;
  FInputSize := Length(FInputString);
  FInputHash := MD5String(FInputString);
  FCurrentSym := thNone;
  FCurrentStr := '';
  FCurrentChar := #0;
  FCurrentLine := 0;
  FCurrentX := 0;
  FIsLineBreak := False;
  FEndOfStream := False;
end;

constructor TTh234Scanner.Create(const InputStream: TStream);
begin

end;

destructor TTh234Scanner.Destroy;
begin
  inherited Destroy;
end;

procedure TTh234Scanner.Read(var C: Char); inline;
begin

end;

procedure TTh234Scanner.ScanForSymbol(var Sym: TTh234Symbol; var Str: String);
begin

end;

{ TTh234Compiler }

function TTh234Compiler.CompileFromStream(SourceStream: TStream;
  Flags: TThoriumCompilerFlags): Boolean;
begin
  Result := False;
end;

end.

