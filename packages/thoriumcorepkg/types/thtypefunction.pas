unit ThTypeFunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, ThGlobals, Thorium;

type

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
    function GetTypeKind: TThoriumTypeKind; override;
  public
    procedure Assign(ASource: TThoriumTypeFunction);
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

implementation

uses
  ThDescriptors;

{$I ThInstructionConstructors.inc}

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

end.

