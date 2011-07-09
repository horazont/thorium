unit ThTypeArrays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Thorium, ThGlobals;

type

  { TThoriumTypeArray }

  TThoriumTypeArray = class (TThoriumType)
  public
    constructor Create(const AThorium: TThorium; const AValueType: TThoriumType);
  private
    FValueType: TThoriumType;
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    property ValueType: TThoriumType read FValueType;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType = nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
    function HasIndexedAccess: Boolean; override;
    function NeedsClear: Boolean; override;
  end;

  { TThoriumTypeDynamicArray }

  TThoriumTypeDynamicArray = class (TThoriumTypeArray)
  public
    constructor Create(const AThorium: TThorium; const AValueType: TThoriumType
       );
  public
    function CanCreateNone(const ToRegister: Boolean;
      out Instruction: TThoriumCreateInstructionDescription): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType = nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function NeedsClear: Boolean; override;

    procedure DoAppend(const AValue, ToValue: TThoriumValue); override;
    function DoCreateNone: TThoriumValue; override;
    procedure DoFree(var AValue: TThoriumValue); override;
    function DoGetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue): TThoriumValue; override;
    function DoGetLength(const AValue: TThoriumValue): Integer; override;
    procedure DoSetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue; const NewValue: TThoriumValue); override;
    function DoToString(const AValue: TThoriumValue): String; override;
  end;

  { TThoriumTypeStaticArray }

  TThoriumTypeStaticArray = class (TThoriumTypeArray)
  public
    constructor Create(const AThorium: TThorium; const AValueType: TThoriumType;
       Count: Integer);
  private
    FCount: Integer;
  public
    property Count: Integer read FCount;
  public
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType = nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;

    function DoGetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue): TThoriumValue; override;
    function DoGetLength(const AValue: TThoriumValue): Integer; override;
    procedure DoSetIndexed(const AValue: TThoriumValue;
       const AIndex: TThoriumValue; const NewValue: TThoriumValue); override;
  end;

implementation

uses
  ThDescriptors, ThTypeInt;

{$I ThInstructionConstructors.inc}

{ TThoriumTypeArray }

constructor TThoriumTypeArray.Create(const AThorium: TThorium;
  const AValueType: TThoriumType);
begin
  inherited Create(AThorium);
  FValueType := AValueType;
end;

function TThoriumTypeArray.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkArray;
end;

function TThoriumTypeArray.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  if Operation.Operation in opReflexive then
  begin
    case Operation.Operation of
      opLen:
      begin
        Operation.ResultType := FThorium.TypeInteger;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(len_a(0, 0), 0, -1, 1);
        Exit(True);
      end;
      opDevolatile:
      begin
        Operation.ResultType := nil;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(update_x(0), -1, -1, 0);
        Exit(True);
      end;
    end;
  end
  else if (Operation.Operation in opIndexedAccess) and (TheObject is TThoriumTypeInteger) then
  begin
    case Operation.Operation of
      opIndexedRead:
      begin
        Operation.ResultType := FValueType;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(xiget(0, 0, 0), 1, 0, 2);
        Exit(True);
      end;
      opIndexedWrite:
      begin
        Operation.ResultType := FValueType;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(xiset(0, 0, 0), 1, 0, 2);
        Exit(True);
      end;
    end;
  end
  else if TheObject.IsEqualTo(Self) then
  begin
    case Operation.Operation of
      opAddition:
      begin
        Operation.ResultType := Self;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
      opMultiplication:
      begin
        Operation.ResultType := Self;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
      opCmpEqual:
      begin
        Operation.ResultType := FThorium.TypeInteger;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
      opCmpNotEqual:
      begin
        Operation.ResultType := FThorium.TypeInteger;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(TThoriumInstruction(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0)), 0, 0, 0);
        Exit(True);
      end;
    else
      Exit(False);
    end;
  end;
  Result := False;
end;

function TThoriumTypeArray.IsEqualTo(const AnotherType: TThoriumType): Boolean;
begin
  Result := (AnotherType is TThoriumTypeArray) and (TThoriumTypeArray(AnotherType).FValueType.IsEqualTo(FValueType));
end;

function TThoriumTypeArray.HasIndexedAccess: Boolean;
begin
  Result := True;
end;

function TThoriumTypeArray.NeedsClear: Boolean;
begin
  Result := True;
end;

{ TThoriumTypeDynamicArray }

constructor TThoriumTypeDynamicArray.Create(const AThorium: TThorium;
  const AValueType: TThoriumType);
begin
  inherited Create(AThorium, AValueType);
  SetName(AValueType.Name+'[]');
end;

function TThoriumTypeDynamicArray.CanCreateNone(const ToRegister: Boolean;
  out Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  Result := True;
  if ToRegister then
  begin
    Instruction.Instruction := TThoriumInstructionREG(none(Self, 0));
    Instruction.TargetRegisterOffset := 4;
  end
  else
  begin
    Instruction.Instruction := TThoriumInstructionREG(none_s(Self));
    Instruction.TargetRegisterOffset := -1;
  end;
end;

function TThoriumTypeDynamicArray.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  case Operation.Operation of
    opAppend:
    begin
      if not TheObject.IsEqualTo(FValueType) then
        Exit(inherited CanPerformOperation(Operation, TheObject, ExName));
      NewNonCastOperation(Operation);
      Operation.ResultType := nil;
      Operation.OperationInstruction := OperationInstructionDescription(append(0, 0), 0, -1, 1);
      Exit(True);
    end;
  else
    Exit(inherited CanPerformOperation(Operation, TheObject, ExName));
  end;
end;

function TThoriumTypeDynamicArray.NeedsClear: Boolean;
begin
  Result := True;
end;

procedure TThoriumTypeDynamicArray.DoAppend(const AValue, ToValue: TThoriumValue
  );
var
  L: PtrInt;
begin
  L := Length(ToValue.DynamicArray^);
  SetLength(ToValue.DynamicArray^, L+1);
  ToValue.DynamicArray^[L] := ThoriumDuplicateValue(AValue);
end;

function TThoriumTypeDynamicArray.DoCreateNone: TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.DynamicArray);
end;

procedure TThoriumTypeDynamicArray.DoFree(var AValue: TThoriumValue);
var
  I: Integer;
  Arr: TThoriumArray;
begin
  Arr := AValue.DynamicArray^;
  for I := 0 to High(Arr) do
  begin
    if Arr[I].RTTI <> nil then
      Arr[I].RTTI.DoFree(Arr[I]);
  end;
  Dispose(AValue.DynamicArray);
end;

function TThoriumTypeDynamicArray.DoGetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue): TThoriumValue;
begin
  if (AIndex.Int < 0) or (AIndex.Int > High(AValue.DynamicArray^)) then
    raise EThoriumRuntimeException.CreateFmt('Dynamic array index %d out of bounds (%d..%d)', [AIndex.Int, 0, High(AValue.DynamicArray^)]);
  Result := AValue.DynamicArray^[AIndex.Int];
end;

function TThoriumTypeDynamicArray.DoGetLength(const AValue: TThoriumValue): Integer;
begin
  Result := Length(AValue.DynamicArray^);
end;

procedure TThoriumTypeDynamicArray.DoSetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue; const NewValue: TThoriumValue);
begin
  if (AIndex.Int < 0) or (AIndex.Int > High(AValue.DynamicArray^)) then
    raise EThoriumRuntimeException.CreateFmt('Dynamic array index %d out of bounds (%d..%d)', [AIndex.Int, 0, High(AValue.DynamicArray^)]);
  ThoriumFreeValue(AValue.DynamicArray^[AIndex.Int]);
  AValue.DynamicArray^[AIndex.Int] := ThoriumDuplicateValue(NewValue);
end;

function TThoriumTypeDynamicArray.DoToString(const AValue: TThoriumValue
  ): String;
var
  I: Integer;
  Arr: TThoriumArray;
begin
  Arr := AValue.DynamicArray^;
  if Length(Arr) < 1 then
    Exit('[]');

  Result := '[' + ThoriumValueToStr(Arr[0]);
  for I := 1 to High(Arr) do
  begin
    Result += ', ' + ThoriumValueToStr(Arr[I]);
  end;
  Result += ']';
end;

{ TThoriumTypeStaticArray }

constructor TThoriumTypeStaticArray.Create(const AThorium: TThorium;
  const AValueType: TThoriumType; Count: Integer);
begin
  if Count <= 0 then
    raise EThoriumException.CreateFmt('Cannot create a static array type with a length of %d.', [Count]);
  inherited Create(AThorium, AValueType);
  FCount := Count;
  SetName(Format('%s[%d]', [AValueType.Name, Count]));
end;

function TThoriumTypeStaticArray.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  Result := inherited;
  if Operation.Operation in [opAddition, opMultiplication] then
    Exit(False);
  if Operation.Operation = opLen then
    Operation.OperationInstruction := OperationInstructionDescription(int(FCount, 0), -1, -1, 4);
end;

function TThoriumTypeStaticArray.IsEqualTo(const AnotherType: TThoriumType
  ): Boolean;
begin
  Result := (AnotherType is TThoriumTypeStaticArray) and
    (TThoriumTypeStaticArray(AnotherType).FCount = FCount) and
    (TThoriumTypeStaticArray(AnotherType).FValueType.IsEqualTo(FValueType));
end;

function TThoriumTypeStaticArray.DoGetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue): TThoriumValue;
begin
  if (AIndex.Int < 0) or (AIndex.Int >= FCount) then
    raise EThoriumRuntimeException.CreateFmt('Static array index %d out of bounds (%d..%d)', [AIndex.Int, 0, FCount-1]);
  Result := AValue.StaticArray^[AIndex.Int];
end;

function TThoriumTypeStaticArray.DoGetLength(const AValue: TThoriumValue
  ): Integer;
begin
  Result := FCount;
end;

procedure TThoriumTypeStaticArray.DoSetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue; const NewValue: TThoriumValue);
begin
  if (AIndex.Int < 0) or (AIndex.Int >= FCount) then
    raise EThoriumRuntimeException.CreateFmt('Static array index %d out of bounds (%d..%d)', [AIndex.Int, 0, FCount-1]);
  ThoriumFreeValue(AValue.StaticArray^[AIndex.Int]);
  AValue.StaticArray^[AIndex.Int] := ThoriumDuplicateValue(NewValue);
end;

end.

