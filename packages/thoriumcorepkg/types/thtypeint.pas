unit ThTypeInt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, ThGlobals, typinfo;

type

  { TThoriumTypeInteger }

  TThoriumTypeInteger = class (TThoriumTypeSimple)
  protected
    function GetNoneInitialData(out InitialData: TThoriumInitialData
       ): Boolean; override;
  public
    function CreateFromInt(const Int: TThoriumInteger): TThoriumValue;
  public
    function CanAssignTo(var Assignment: TThoriumAssignmentDescription;
       const AnotherType: TThoriumType=nil): Boolean; override;
    function CanCreate(const InitialData: TThoriumInitialData;
       const ToRegister: Boolean; out
       Instruction: TThoriumCreateInstructionDescription): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function CreateValueFromPtr(const Ptr: Pointer): TThoriumValue; override;
    function GetNativeCallSpecification(const ForType: PTypeInfo): TThoriumNativeCallSpecification; override;

    function DoAddition(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitAnd(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitOr(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitXor(const AValue, BValue: TThoriumValue): TThoriumValue;
         override;
    function DoBitShr(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitShl(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoBitNot(const AValue: TThoriumValue): TThoriumValue;
       override;
    function DoCast(const AValue: TThoriumValue; const TargetType: TThoriumType
       ): TThoriumValue; override;
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
    function DoCreate(const InitialData: TThoriumInitialData): TThoriumValue;
       override;
    procedure DoDecrement(var ASubject: TThoriumValue); override;
    function DoDivision(const AValue, BValue: TThoriumValue): TThoriumValue;
         override;
    function DoEvaluate(const AValue: TThoriumValue): Boolean; override;
    procedure DoFreeNative(var AValue: TThoriumValue); override;
    procedure DoFromNative(var AValue: TThoriumValue); override;
    procedure DoIncrement(var ASubject: TThoriumValue); override;
    function DoIntegerDivision(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    function DoModulus(const AValue, BValue: TThoriumValue): TThoriumValue;
       override;
    function DoMultiplication(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    procedure DoNegate(var AValue: TThoriumValue); override;
    function DoSubtraction(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    procedure DoToNative(var AValue: TThoriumValue; const AType: PTypeInfo);
       override;
    function DoToString(const AValue: TThoriumValue): String; override;
  end;

implementation

uses
  ThDescriptors, ThTypeFloat;

{$I ThInstructionConstructors.inc}

{ TThoriumTypeInteger }

function TThoriumTypeInteger.GetNoneInitialData(out
  InitialData: TThoriumInitialData): Boolean;
begin
  Result := True;
  InitialData.Int := 0;
end;

function TThoriumTypeInteger.CreateFromInt(const Int: TThoriumInteger
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := Int;
end;

function TThoriumTypeInteger.CanAssignTo(
  var Assignment: TThoriumAssignmentDescription; const AnotherType: TThoriumType
  ): Boolean;
begin
  if AnotherType is TThoriumTypeFloat then
  begin
    if not Assignment.Casting then
      Result := inherited;
    Assignment.Cast.Needed := True;
    Assignment.Cast.Instruction := TThoriumInstructionCAST(castif(0, 0));
    Assignment.Cast.TargetType := AnotherType;
  end
  else
    Result := inherited;
end;

function TThoriumTypeInteger.CanCreate(const InitialData: TThoriumInitialData;
  const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  if ToRegister then
    Instruction := CreationDescription(int(InitialData.Int, 0), 4)
  else
    Instruction := CreationDescription(int_s(InitialData.Int));
  Result := True;
end;

function TThoriumTypeInteger.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;

  function IntIntOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeInteger then
    begin
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
      Result := True;
    end
    else
      Result := False;
  end;

  function IntFltOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeFloat then
    begin
      Operation.ResultType := TheObject;
      Operation.Casts[0].Needed := True;
      Operation.Casts[0].Instruction := TThoriumInstructionCAST(castif(0, 0));
      Operation.Casts[0].TargetType := Operation.ResultType;
      Operation.Casts[1].Needed := False;
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
      Result := True;
    end
    else
      Result := False;
  end;

  procedure IntOp(Instruction: TThoriumInstruction);
  begin
    Operation.ResultType := Self;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    Operation.OperationInstruction := OperationInstructionDescription(Instruction, -1, -1, 0);
  end;

begin
  Result := True;
  if Operation.Operation in opReflexive then
  begin
    Operation.ResultType := nil;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    case Operation.Operation of
      opIncrement:
        IntOp(inci(0));
      opDecrement:
        IntOp(deci(0));
      opBitNot:
        IntOp(_not(0));
      opNegate:
        IntOp(negi(0));
      opToNative:
      begin
        Assert(ExType <> nil);
        if ExType^.Kind in [tkInteger,tkChar,tkEnumeration,tkWChar,tkSet,tkInt64,tkQWord] then
          Operation.OperationInstruction := OperationInstructionDescription(x2n(0, ExType), -1, -1, 0)
        else
          Exit(False);
        Exit;
      end;
      opFromNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(x2n(0, ExType), -1, -1, 0);
      end;
      opFreeNative:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(clrn(0), -1, -1, 0);
      end;
      opEvaluate:
      begin
        Operation.OperationInstruction := OperationInstructionDescription(evali(0), 0, -1, -1);
        Exit;
      end;
    else
      Result := inherited;
    end;
    Exit;
  end
  else if TheObject = nil then
    RaiseMissingTheObject
  else
  begin
    case Operation.Operation of
      opAddition:
      begin
        if IntIntOp(addi(0, 0, 0)) then
          Exit
        else if IntFltOp(addf(0, 0, 0)) then
          Exit;
      end;

      opSubtraction:
      begin
        if IntIntOp(subi(0, 0, 0)) then
          Exit
        else if IntFltOp(subf(0, 0, 0)) then
          Exit;
      end;

      opMultiplication:
      begin
        if IntIntOp(muli(0, 0, 0)) then
          Exit
        else if IntFltOp(mulf(0, 0, 0)) then
          Exit;
      end;

      opDivision:
      begin
        if TheObject is TThoriumTypeInteger then
        begin
          Operation.ResultType := FThorium.TypeFloat;
          Operation.Casts[0].Needed := True;
          Operation.Casts[0].Instruction := TThoriumInstructionCAST(castif(0, 0));
          Operation.Casts[0].TargetType := Operation.ResultType;
          Operation.Casts[1].Needed := True;
          Operation.Casts[1].Instruction := TThoriumInstructionCAST(castif(0, 0));
          Operation.Casts[1].TargetType := Operation.ResultType;
          Operation.OperationInstruction := OperationInstructionDescription(divf(0, 0, 0));
          Exit;
        end
        else if IntFltOp(divf(0, 0, 0)) then
          Exit;
      end;

      opIntegerDivision:
        if IntIntOp(divi(0, 0, 0)) then
          Exit;

      opModulus:
        if IntIntOp(_mod(0, 0, 0)) then
          Exit;

      opBitOr:
        if IntIntOp(_or(0, 0, 0)) then
          Exit;

      opBitXor:
        if IntIntOp(_xor(0, 0, 0)) then
          Exit;

      opBitAnd:
        if IntIntOp(_and(0, 0, 0)) then
          Exit;

      opBitShl:
        if IntIntOp(_shl(0, 0, 0)) then
          Exit;

      opBitShr:
        if IntIntOp(_shr(0, 0, 0)) then
          Exit;

      opCmpLessOrEqual, opCmpGreaterOrEqual, opCmpGreater, opCmpLess,
      opCmpNotEqual, opCmpEqual:
      begin
        Operation.ResultType := nil;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        if TheObject is TThoriumTypeInteger then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpi(0, 0), 0, 1, -1);
          Exit;
        end
        else if TheObject is TThoriumTypeFloat then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpif(0, 0), 0, 1, -1);
          Exit;
        end;
      end;

      opString:
      begin
        Operation.ResultType := FThorium.TypeString;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        Operation.OperationInstruction := OperationInstructionDescription(tostr_i(0, 0), 0, -1, 1);
        Exit(True);
      end;
    end;
  end;
  Result := inherited;
end;

function TThoriumTypeInteger.CreateValueFromPtr(const Ptr: Pointer
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := PThoriumInteger(Ptr)^;
end;

function TThoriumTypeInteger.GetNativeCallSpecification(const ForType: PTypeInfo
  ): TThoriumNativeCallSpecification;
begin
  if ForType^.Kind in [tkInt64, tkQWord] then
    Result.ValueMode := vmInt64
  else
    Result.ValueMode := vmInt32;
  Result.ForceStack := False;
  Result.Count := 1;
  Result.RefMode := rmData;
end;

function TThoriumTypeInteger.DoAddition(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int + BValue.Int;
end;

function TThoriumTypeInteger.DoBitAnd(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int and BValue.Int;
end;

function TThoriumTypeInteger.DoBitOr(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int or BValue.Int;
end;

function TThoriumTypeInteger.DoBitXor(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int xor BValue.Int;
end;

function TThoriumTypeInteger.DoBitShr(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int shr BValue.Int;
end;

function TThoriumTypeInteger.DoBitShl(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int shl BValue.Int;
end;

function TThoriumTypeInteger.DoBitNot(const AValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := not AValue.Int;
end;

function TThoriumTypeInteger.DoCast(const AValue: TThoriumValue;
  const TargetType: TThoriumType): TThoriumValue;
begin
  if TargetType is TThoriumTypeFloat then
  begin
    Result.RTTI := TargetType;
    Result.Float := AValue.Int;
  end
  else
    raise EThoriumRuntimeException.CreateFmt('Cannot cast %s to %s.', [Name, TargetType.Name]);
end;

function TThoriumTypeInteger.DoCmpEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int = BValue.Float
  else
    Result := AValue.Int = BValue.Int;
end;

function TThoriumTypeInteger.DoCmpGreater(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int > BValue.Float
  else
    Result := AValue.Int > BValue.Int;
end;

function TThoriumTypeInteger.DoCmpGreaterOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int >= BValue.Float
  else
    Result := AValue.Int >= BValue.Int;
end;

function TThoriumTypeInteger.DoCmpLess(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int < BValue.Float
  else
    Result := AValue.Int < BValue.Int;
end;

function TThoriumTypeInteger.DoCmpLessOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int <= BValue.Float
  else
    Result := AValue.Int <= BValue.Int;
end;

function TThoriumTypeInteger.DoCmpNotEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeFloat then
    Result := AValue.Int <> BValue.Float
  else
    Result := AValue.Int <> BValue.Int;
end;

procedure TThoriumTypeInteger.DoDecrement(var ASubject: TThoriumValue);
begin
  ASubject.Int -= 1;
end;

function TThoriumTypeInteger.DoDivision(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := FThorium.TypeFloat;
  Result.Float := AValue.Int / BValue.Int;
end;

function TThoriumTypeInteger.DoEvaluate(const AValue: TThoriumValue): Boolean;
begin
  Result := AValue.Int <> 0;
end;

procedure TThoriumTypeInteger.DoFreeNative(var AValue: TThoriumValue);
begin
  AValue.NativeData.Data := nil;
end;

procedure TThoriumTypeInteger.DoFromNative(var AValue: TThoriumValue);
begin
  AValue.NativeData.Data := nil;
end;

function TThoriumTypeInteger.DoCreate(const InitialData: TThoriumInitialData
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := InitialData.Int;
end;

procedure TThoriumTypeInteger.DoIncrement(var ASubject: TThoriumValue);
begin
  ASubject.Int += 1;
end;

function TThoriumTypeInteger.DoIntegerDivision(const AValue,
  BValue: TThoriumValue): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int div BValue.Int;
end;

function TThoriumTypeInteger.DoModulus(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int mod BValue.Int;
end;

function TThoriumTypeInteger.DoMultiplication(const AValue,
  BValue: TThoriumValue): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int * BValue.Int;
end;

procedure TThoriumTypeInteger.DoNegate(var AValue: TThoriumValue);
begin
  AValue.Int := -AValue.Int;
end;

function TThoriumTypeInteger.DoSubtraction(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := AValue.Int - BValue.Int;
end;

procedure TThoriumTypeInteger.DoToNative(var AValue: TThoriumValue;
  const AType: PTypeInfo);
begin
  AValue.NativeData.ForType := nil;
  AValue.NativeData.AlignedSize := 1 * CPU_SIZE_FACTOR;
  AValue.NativeData.Data := @AValue.Int;
  if AType^.Kind = tkInteger then
    AValue.NativeData.Size := 4
  else
    AValue.NativeData.Size := 8;
end;

function TThoriumTypeInteger.DoToString(const AValue: TThoriumValue): String;
begin
  Result := IntToStr(AValue.Int);
end;

end.

