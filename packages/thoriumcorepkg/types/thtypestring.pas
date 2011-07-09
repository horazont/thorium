unit ThTypeString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Thorium, ThGlobals;

type
  { TThoriumTypeString }

  TThoriumTypeString = class (TThoriumTypeSimple)
  protected
    function GetNoneInitialData(out InitialData: TThoriumInitialData
       ): Boolean; override;
    function GetTypeKind: TThoriumTypeKind; override;
  public
    function CreateFromString(const Str: String): TThoriumValue;
  public
    function CanCreate(const InitialData: TThoriumInitialData;
       const ToRegister: Boolean; out
       Instruction: TThoriumCreateInstructionDescription): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String='';
       const ExType: PTypeInfo = nil): Boolean; override;
    function CreateValueFromPtr(const Ptr: Pointer): TThoriumValue; override;
    function DuplicateValue(const Input: TThoriumValue): TThoriumValue;
       override;
    function GetNativeCallSpecification(const ForType: PTypeInfo
      ): TThoriumNativeCallSpecification; override;
    function HasIndexedAccess: Boolean; override;
    function HasFieldAccess: Boolean; override;
    function NeedsClear: Boolean; override;

    function DoAddition(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoCmpEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreater(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpGreaterOrEqual(const AValue, BValue: TThoriumValue
       ): Boolean; override;
    function DoCmpLess(const AValue, BValue: TThoriumValue): Boolean; override;
    function DoCmpLessOrEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    function DoCmpNotEqual(const AValue, BValue: TThoriumValue): Boolean;
       override;
    procedure DoFree(var AValue: TThoriumValue); override;
    procedure DoFreeNative(var AValue: TThoriumValue); override;
    procedure DoFromNative(var AValue: TThoriumValue); override;
    function DoGetField(const AValue: TThoriumValue; const AFieldID: QWord
       ): TThoriumValue; override;
    function DoGetIndexed(const AValue: TThoriumValue; const AIndex: TThoriumValue
       ): TThoriumValue; override;
    procedure DoToNative(var AValue: TThoriumValue; const AType: PTypeInfo);
       override;
    function DoToString(const AValue: TThoriumValue): String; override;
  end;

implementation

uses
  ThDescriptors, ThTypeInt;

{$I ThInstructionConstructors.inc}

{ TThoriumTypeString }

function TThoriumTypeString.GetNoneInitialData(out
  InitialData: TThoriumInitialData): Boolean;
begin
  Result := True;
  InitialData.Int := -1;
end;

function TThoriumTypeString.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkString;
end;

function TThoriumTypeString.CreateFromString(const Str: String): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := Str;
end;

function TThoriumTypeString.CanCreate(const InitialData: TThoriumInitialData;
  const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  if ToRegister then
  begin
    if InitialData.Int < 0 then
      Instruction := CreationDescription(str(0), 0)
    else
      Instruction := CreationDescription(strl(InitialData.Int, 0), 2);
  end
  else
  begin
    if InitialData.Int < 0 then
      Instruction := CreationDescription(str_s())
    else
      Instruction := CreationDescription(strl_s(InitialData.Int));
  end;
  Result := True;
end;

function TThoriumTypeString.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;

  function StrStrOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeString then
    begin
      Result := True;
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
    end
    else
      Result := False;
  end;

  function StrIndexOp(Instruction: TThoriumInstruction; AOp1, AOp2, ATarget: Integer): Boolean;
  begin
    if TheObject is TThoriumTypeInteger then
    begin
      Result := True;
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction, AOp1, AOp2, ATarget);
    end
    else
      Result := False;
  end;

begin
  Result := True;
  if Operation.Operation in opReflexive then
  begin
    Operation.ResultType := nil;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    case Operation.Operation of
      opString:
      begin
        NewNonCastOperation(Operation);
        Operation.ResultType := Self;
        Operation.OperationInstruction := OperationInstructionDescription(copyr(0, 0), 0, -1, 1);
        Exit(True);
      end;
      opLen:
      begin
        NewNonCastOperation(Operation);
        Operation.ResultType := FThorium.TypeInteger;
        Operation.OperationInstruction := OperationInstructionDescription(len_s(0, 0), 0, -1, 1);
        Exit(True);
      end;
      opToNative:
      begin
        if ExType^.Kind in [tkAString] then
          Operation.OperationInstruction := OperationInstructionDescription(x2n(0, ExType), -1, -1, 0)
        else
          Exit(False);
        Exit;
      end;
      opFromNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(n2x(0), -1, -1, 0);
        Exit;
      end;
      opFreeNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(clrn(0), -1, -1, 0);
        Exit;
      end;
    else
      Exit(inherited CanPerformOperation(Operation, TheObject, ExName));
    end;
  end
  else if TheObject = nil then
    RaiseMissingTheObject
  else
  begin
    case Operation.Operation of
      opAddition:
        if StrStrOp(adds(0, 0, 0)) then
          Exit;
      opIndexedRead:
        if StrIndexOp(noop(THORIUM_NOOPMARK_NOT_IMPLEMENTED_YET, 0, 0, 0), -1, -1, -1) then
          Exit;

      opCmpEqual, opCmpGreater, opCmpGreaterOrEqual, opCmpLess,
      opCmpLessOrEqual, opCmpNotEqual:
      begin
        if (TheObject is TThoriumTypeString) then
        begin
          Operation.ResultType := nil;
          Operation.Casts[0].Needed := False;
          Operation.Casts[1].Needed := False;
          Operation.OperationInstruction := OperationInstructionDescription(cmps(0, 0), 0, 1, -1);
          Exit;
        end;
      end;
    end;
  end;
  Result := inherited;
end;

function TThoriumTypeString.CreateValueFromPtr(const Ptr: Pointer
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := PThoriumString(Ptr)^;
end;

function TThoriumTypeString.DuplicateValue(const Input: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := Input.Str^;
end;

function TThoriumTypeString.GetNativeCallSpecification(const ForType: PTypeInfo
  ): TThoriumNativeCallSpecification;
begin
  Result.Count := 1;
  Result.ForceStack := False;
  Result.ValueMode := vmString;
  Result.RefMode := rmData;
end;

function TThoriumTypeString.HasIndexedAccess: Boolean;
begin
  Result := True;
end;

function TThoriumTypeString.HasFieldAccess: Boolean;
begin
  Result := True;
end;

function TThoriumTypeString.NeedsClear: Boolean;
begin
  Result := True;
end;

function TThoriumTypeString.DoAddition(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := AValue.Str^ + BValue.Str^;
end;

function TThoriumTypeString.DoCmpEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ = BValue.Str^;
end;

function TThoriumTypeString.DoCmpGreater(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ > BValue.Str^;
end;

function TThoriumTypeString.DoCmpGreaterOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  Result := AValue.Str^ >= BValue.Str^;
end;

function TThoriumTypeString.DoCmpLess(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ < BValue.Str^;
end;

function TThoriumTypeString.DoCmpLessOrEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ <= BValue.Str^;
end;

function TThoriumTypeString.DoCmpNotEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  Result := AValue.Str^ <> BValue.Str^;
end;

procedure TThoriumTypeString.DoFree(var AValue: TThoriumValue);
begin
  Dispose(AValue.Str);
  AValue.RTTI := nil;
end;

procedure TThoriumTypeString.DoFreeNative(var AValue: TThoriumValue);
begin

end;

procedure TThoriumTypeString.DoFromNative(var AValue: TThoriumValue);
begin

end;

function TThoriumTypeString.DoGetField(const AValue: TThoriumValue;
  const AFieldID: QWord): TThoriumValue;
begin
  {$ifdef DebugToConsole}
  case AFieldID of
    0:
    begin
      Result.RTTI := FThorium.FTypeInteger;
      Result.Int := Length(AValue.Str^);
    end;
  end;
  {$else}
  case AFieldID of
    0: Result.Int := Length(AValue.Str^);
  end;
  {$endif}
end;

function TThoriumTypeString.DoGetIndexed(const AValue: TThoriumValue;
  const AIndex: TThoriumValue): TThoriumValue;
begin
  Result.RTTI := Self;
  New(Result.Str);
  Result.Str^ := AValue.Str^[AIndex.Int];
end;

procedure TThoriumTypeString.DoToNative(var AValue: TThoriumValue;
  const AType: PTypeInfo);
begin
  AValue.NativeData.AlignedSize := 1;
  AValue.NativeData.Data := AValue.Str;
end;

function TThoriumTypeString.DoToString(const AValue: TThoriumValue): String;
begin
  Result:=AValue.Str^;
end;

end.

