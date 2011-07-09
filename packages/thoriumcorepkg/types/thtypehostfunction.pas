unit ThTypeHostFunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Thorium, ThGlobals;

type

  { TThoriumTypeHostFunction }

  TThoriumTypeHostFunction = class (TThoriumType, IThoriumCallable, IThoriumHostCallable)
  public
    constructor Create(const AThorium: TThorium; const AHostFunction: TThoriumHostCallableBase);
    destructor Destroy; override;
  private
    FHostFunction: TThoriumHostCallableBase;
    FParameters: TThoriumParameters;
    FReturnType: TThoriumType;
    function GetHasReturnValue: Boolean;
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function GetParameters: TThoriumParameters;
    function GetHostParameters: TThoriumHostParameters;
    function GetReturnType: TThoriumType;
    function GetHostReturnType: TThoriumHostType;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
    function NeedsNativeData: Boolean;
  published
    property HasReturnValue: Boolean read GetHasReturnValue;
    property Parameters: TThoriumParameters read FParameters;
    property ReturnType: TThoriumType read FReturnType;
    property HostFunction: TThoriumHostCallableBase read FHostFunction;
  end;

implementation

uses
  ThDescriptors;

{$I ThInstructionConstructors.inc}

{ TThoriumTypeHostFunction }

constructor TThoriumTypeHostFunction.Create(const AThorium: TThorium;
  const AHostFunction: TThoriumHostCallableBase);
var
  I: Integer;
  TypeSpec: TThoriumType;
  Op: TThoriumOperationDescription;
begin
  inherited Create(AThorium);
  FHostFunction := AHostFunction;
  FParameters := TThoriumParameters.Create;
  for I := 0 to AHostFunction.Parameters.Count - 1 do
  begin
    TypeSpec := AHostFunction.OwnerLibrary.DeepFindTypeForHostType(AHostFunction.Parameters.Parameters[I]);
    if TypeSpec = nil then
      raise EThoriumException.CreateFmt('Cannot find respective thorium type for %s.', [AHostFunction.Parameters.Parameters[I]^.Name]);
    if AHostFunction.NeedsNativeData then
    begin;
      Op.Operation := opToNative;
      if not TypeSpec.CanPerformOperation(Op, nil, '', AHostFunction.Parameters.Parameters[I]) then
        raise EThoriumException.CreateFmt('''%s'' cannot be passed to this native call function (opToNative not supported for ''%s'').', [TypeSpec.Name, AHostFunction.Parameters.Parameters[I]^.Name]);
      Op.Operation := opFromNative;
      if not TypeSpec.CanPerformOperation(Op) then
        raise EThoriumException.CreateFmt('''%s'' cannot be passed to a native call function (opFromNative not supported).', [TypeSpec.Name]);
    end;
    FParameters.Add(TypeSpec);
  end;
  if AHostFunction.ReturnType.HostType <> nil then
  begin
    TypeSpec := AHostFunction.OwnerLibrary.DeepFindTypeForHostType(AHostFunction.ReturnType.HostType);
    if TypeSpec = nil then
      raise EThoriumException.CreateFmt('Cannot find respective thorium type for %s.', [AHostFunction.ReturnType.HostType^.Name]);
    if AHostFunction.NeedsNativeData then
    begin;
      Op.Operation := opToNative;
      if not TypeSpec.CanPerformOperation(Op, nil, '', AHostFunction.ReturnType.HostType) then
        raise EThoriumException.CreateFmt('''%s'' cannot be return type of this native call function (opToNative not supported for ''%s'').', [TypeSpec.Name, AHostFunction.ReturnType.HostType^.Name]);
      Op.Operation := opFromNative;
      if not TypeSpec.CanPerformOperation(Op) then
        raise EThoriumException.CreateFmt('''%s'' cannot be return type of a native call function (opFromNative not supported).', [TypeSpec.Name]);
    end;
    FReturnType := TypeSpec;
  end
  else
    FReturnType := nil;
end;

destructor TThoriumTypeHostFunction.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

function TThoriumTypeHostFunction.GetHasReturnValue: Boolean;
begin
  Result := FReturnType <> nil;
end;

function TThoriumTypeHostFunction.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkHostFunction;
end;

function TThoriumTypeHostFunction.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  case Operation.Operation of
    opCall:
    begin
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(xcall(0, 0), 1, -1, 0);
      Operation.ResultType := FReturnType;
      Result := True;
    end;
  else
    Result := False;
  end;
end;

function TThoriumTypeHostFunction.GetParameters: TThoriumParameters;
begin
  Result := FParameters;
end;

function TThoriumTypeHostFunction.GetHostParameters: TThoriumHostParameters;
begin
  Result := HostFunction.Parameters;
end;

function TThoriumTypeHostFunction.GetReturnType: TThoriumType;
begin
  Result := FReturnType;
end;

function TThoriumTypeHostFunction.GetHostReturnType: TThoriumHostType;
begin
  Result := HostFunction.ReturnType;
end;

function TThoriumTypeHostFunction.IsEqualTo(const AnotherType: TThoriumType
  ): Boolean;
begin
  raise EThoriumException.Create('TThoriumTypeHostFunction.IsEqualTo not implemented yet.');
end;

function TThoriumTypeHostFunction.NeedsNativeData: Boolean;
begin
  Exit(FHostFunction.NeedsNativeData);
end;

end.

