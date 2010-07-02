unit Thorium_ObjectInstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals, fgl;

type

  { TThoriumStatement }

  TThoriumStatement = class (TObject)
  public
    constructor Create; virtual;
  public
    procedure AfterCompilation; virtual;
    procedure Execute; virtual; abstract;
  end;

  TThoriumStatementDynamicList = specialize TFPGList<TThoriumStatement>;

  { TThoriumStatementList }

  TThoriumStatementList = class (TThoriumStatement)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FStatementsDynamic: TThoriumStatementDynamicList;
    FStatementsCompiled: array of TThoriumStatement;
  public
    procedure AfterCompilation; override;
    procedure Execute; override;
  end;

(******************************************************************************)
// Expressions

  { TThoriumExpression }

  TThoriumExpression = class (TThoriumStatement)
  public
    function Evaluate: TThoriumValue; virtual; abstract;
    function EvaluateStr: TThoriumString; virtual;
    function EvaluateInt: TThoriumInteger; virtual;
    function EvaluateFloat: TThoriumFloat; virtual;
    procedure Execute; override;
  end;
  TThoriumExpressionDynamicList = specialize TFPGList<TThoriumStatement>;

  TThorium

implementation

{ TThoriumStatement }

constructor TThoriumStatement.Create;
begin

end;

procedure TThoriumStatement.AfterCompilation;
begin

end;

{ TThoriumExpression }

function TThoriumExpression.EvaluateStr: TThoriumString;
var
  Value: TThoriumValue;
begin
  Value := Evaluate;
  Result := Value.Str;
  ThoriumFreeValue(Value);
end;

function TThoriumExpression.EvaluateInt: TThoriumInteger;
var
  Value: TThoriumValue;
begin
  Value := Evaluate;
  Result := Value.Int;
  ThoriumFreeValue(Value);
end;

function TThoriumExpression.EvaluateFloat: TThoriumFloat;
var
  Value: TThoriumValue;
begin
  Value := Evaluate;
  Result := Value.Float;
  ThoriumFreeValue(Value);
end;

procedure TThoriumExpression.Execute;
begin
  ThoriumFreeValue(Evaluate);
end;

{ TThoriumStatementList }

constructor TThoriumStatementList.Create;
begin
  inherited Create;
  FStatementsDynamic := TThoriumStatementDynamicList.Create;
end;

destructor TThoriumStatementList.Destroy;
var
  I: Integer;
begin
  if FStatementsDynamic = nil then
  begin
    for I := 0 to FStatementsDynamic.Count - 1 do
      FStatementsDynamic[I].Free;
    FreeAndNil(FStatementsDynamic);
  end
  else
  begin
    for I := 0 to High(FStatementsCompiled) do
      FreeAndNil(FStatementsCompiled[I]);
  end;
  inherited Destroy;
end;

procedure TThoriumStatementList.AfterCompilation;
var
  I: Integer;
begin
  SetLength(FStatementsCompiled, FStatementsDynamic.Count);
  for I := 0 to High(FStatementsCompiled) do
    FStatementsCompiled[I] := FStatementsDynamic[I];
  FreeAndNil(FStatementsDynamic);
  for I := 0 to High(FStatementsCompiled) do
    FStatementsCompiled[I].AfterCompilation;
  inherited AfterCompilation;
end;

procedure TThoriumStatementList.Execute;
var
  I: Integer;
begin
  for I := 0 to High(FStatementsCompiled) do
    FStatementsCompiled[I].Execute;
end;

end.

