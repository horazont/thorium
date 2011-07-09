unit ThTypeFloat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, ThGlobals, typinfo, ThUtils;

type
  { TThoriumTypeFloat }

  TThoriumTypeFloat = class (TThoriumTypeSimple)
  protected
    function GetNoneInitialData(out InitialData: TThoriumInitialData
       ): Boolean; override;
  public
    function CanCreate(const InitialData: TThoriumInitialData;
       const ToRegister: Boolean; out
       Instruction: TThoriumCreateInstructionDescription): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    function CreateValueFromPtr(const Ptr: Pointer): TThoriumValue; override;
    function GetNativeCallSpecification(const ForType: PTypeInfo
      ): TThoriumNativeCallSpecification; override;

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
    function DoCreate(const InitialData: TThoriumInitialData
         ): TThoriumValue; override;
    procedure DoDecrement(var ASubject: TThoriumValue); override;
    function DoDivision(const AValue, BValue: TThoriumValue): TThoriumValue;
        override;
    procedure DoFreeNative(var AValue: TThoriumValue); override;
    procedure DoFromNative(var AValue: TThoriumValue); override;
    procedure DoIncrement(var ASubject: TThoriumValue); override;
    procedure DoNegate(var AValue: TThoriumValue); override;
    function DoMultiplication(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    function DoSubtraction(const AValue, BValue: TThoriumValue
       ): TThoriumValue; override;
    procedure DoToNative(var AValue: TThoriumValue; const AType: PTypeInfo); override;
    function DoToString(const AValue: TThoriumValue): String; override;
  end;

implementation

uses
  ThDescriptors, ThTypeInt;

{$I ThInstructionConstructors.inc}

{ TThoriumTypeFloat }

function TThoriumTypeFloat.GetNoneInitialData(out
  InitialData: TThoriumInitialData): Boolean;
begin
  Result := True;
  InitialData.Flt := 0.0;
end;

function TThoriumTypeFloat.CanCreate(const InitialData: TThoriumInitialData;
  const ToRegister: Boolean; out
  Instruction: TThoriumCreateInstructionDescription): Boolean;
begin
  if ToRegister then
    Instruction := CreationDescription(flt(InitialData.Flt, 0), 4)
  else
    Instruction := CreationDescription(flt_s(InitialData.Flt));
  Result := True;
end;

function TThoriumTypeFloat.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;

  function FltFltOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeFloat then
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

  function FltIntOp(Instruction: TThoriumInstruction): Boolean;
  begin
    if TheObject is TThoriumTypeInteger then
    begin
      Result := True;
      Operation.ResultType := Self;
      Operation.Casts[0].Needed := False;
      Operation.Casts[1].Needed := True;
      Operation.Casts[1].Instruction := TThoriumInstructionCAST(castif(0, 0));
      Operation.OperationInstruction := OperationInstructionDescription(Instruction);
    end
    else
      Result := False;
  end;

  procedure FltOp(Instruction: TThoriumInstruction);
  begin
    Operation.ResultType := Self;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    Operation.OperationInstruction := OperationInstructionDescription(Instruction, 0, -1, -1);
  end;

begin
  Result := True;
  if Operation.Operation in opReflexive then
  begin
    Operation.ResultType := nil;
    Operation.Casts[0].Needed := False;
    Operation.Casts[1].Needed := False;
    case Operation.Operation of
      opNegate:
        FltOp(negf(0));
      opIncrement:
        FltOp(incf(0));
      opDecrement:
        FltOp(decf(0));
      opToNative:
      begin
        if ExType^.Kind in [tkFloat] then
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
        if FltFltOp(addf(0, 0, 0)) then
          Exit
        else if FltIntOp(addf(0, 0, 0)) then
          Exit;
      opSubtraction:
        if FltFltOp(subf(0, 0, 0)) then
          Exit
        else if FltIntOp(subf(0, 0, 0)) then
          Exit;
      opMultiplication:
        if FltFltOp(mulf(0, 0, 0)) then
          Exit
        else if FltIntOp(mulf(0, 0, 0)) then
          Exit;
      opDivision:
        if FltFltOp(divf(0, 0, 0)) then
          Exit
        else if FltIntOp(divf(0, 0, 0)) then
          Exit;

      opCmpLessOrEqual, opCmpGreaterOrEqual, opCmpGreater, opCmpLess,
      opCmpNotEqual, opCmpEqual:
      begin
        Operation.ResultType := nil;
        Operation.Casts[0].Needed := False;
        Operation.Casts[1].Needed := False;
        if TheObject is TThoriumTypeInteger then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpfi(0, 0), 0, 1, -1);
          Exit;
        end
        else if TheObject is TThoriumTypeFloat then
        begin
          Operation.OperationInstruction := OperationInstructionDescription(cmpf(0, 0), 0, 1, -1);
          Exit;
        end;
      end;
    end;
  end;
  Result := inherited;
end;

function TThoriumTypeFloat.CreateValueFromPtr(const Ptr: Pointer
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := PThoriumFloat(Ptr)^;
end;

function TThoriumTypeFloat.GetNativeCallSpecification(const ForType: PTypeInfo
  ): TThoriumNativeCallSpecification;
begin
  Result.Count := 1;
  Result.ForceStack := False;
  Result.ValueMode := vmFloat;
  Result.RefMode := rmData;
  case GetTypeData(ForType)^.FloatType of
    ftSingle:
      Result.FloatMode := fmAsSingle;
    ftDouble:
      Result.FloatMode := fmAsDouble;
    ftExtended:
      Result.FloatMode := fmAsExtended;
  else
    raise EThoriumException.CreateFmt('Invalid float type: ''%s''.', [ForType^.Name]);
  end;
end;

function TThoriumTypeFloat.DoAddition(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float + BValue.Float;
end;

function TThoriumTypeFloat.DoCmpEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float = BValue.Int
  else
    Result := AValue.Float = BValue.Float;
end;

function TThoriumTypeFloat.DoCmpGreater(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float > BValue.Int
  else
    Result := AValue.Float > BValue.Float;
end;

function TThoriumTypeFloat.DoCmpGreaterOrEqual(const AValue,
  BValue: TThoriumValue): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float >= BValue.Int
  else
    Result := AValue.Float >= BValue.Float;
end;

function TThoriumTypeFloat.DoCmpLess(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float < BValue.Int
  else
    Result := AValue.Float < BValue.Float;
end;

function TThoriumTypeFloat.DoCmpLessOrEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float <= BValue.Int
  else
    Result := AValue.Float <= BValue.Float;
end;

function TThoriumTypeFloat.DoCmpNotEqual(const AValue, BValue: TThoriumValue
  ): Boolean;
begin
  if BValue.RTTI is TThoriumTypeInteger then
    Result := AValue.Float <> BValue.Int
  else
    Result := AValue.Float <> BValue.Float;
end;

function TThoriumTypeFloat.DoCreate(const InitialData: TThoriumInitialData
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Int := InitialData.Int;
end;

procedure TThoriumTypeFloat.DoDecrement(var ASubject: TThoriumValue);
begin
  ASubject.Float -= 1;
end;

function TThoriumTypeFloat.DoDivision(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float / BValue.Float;
end;

procedure TThoriumTypeFloat.DoFreeNative(var AValue: TThoriumValue);
begin
  if AValue.NativeData.ForType <> nil then
    FreeMem(AValue.NativeData.Data);
  AValue.NativeData.Data := nil;
end;

procedure TThoriumTypeFloat.DoFromNative(var AValue: TThoriumValue);
begin
  if AValue.NativeData.ForType = nil then
  begin
    AValue.NativeData.Data := nil;
    Exit;
  end;
  case GetTypeData(AValue.NativeData.ForType)^.FloatType of
    ftSingle:
    begin
      AValue.Float := PSingle(AValue.NativeData.Data)^;
    end;
    ftExtended:
    begin
      AValue.Float := PExtended(AValue.NativeData.Data)^;
    end;
    ftComp:
    begin
      AValue.Float := PComp(AValue.NativeData.Data)^;
    end;
    ftCurr:
    begin
      AValue.Float := PCurrency(AValue.NativeData.Data)^;
    end;
  end;
  FreeMem(AValue.NativeData.Data);
  AValue.NativeData.Data := nil;
  AValue.NativeData.ForType := nil;
end;

procedure TThoriumTypeFloat.DoIncrement(var ASubject: TThoriumValue);
begin
  ASubject.Float += 1;
end;

procedure TThoriumTypeFloat.DoNegate(var AValue: TThoriumValue);
begin
  AValue.Float := -AValue.Float;
end;

function TThoriumTypeFloat.DoMultiplication(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float * BValue.Float;
end;

function TThoriumTypeFloat.DoSubtraction(const AValue, BValue: TThoriumValue
  ): TThoriumValue;
begin
  Result.RTTI := Self;
  Result.Float := AValue.Float - BValue.Float;
end;

procedure TThoriumTypeFloat.DoToNative(var AValue: TThoriumValue;
  const AType: PTypeInfo);
var
  Size: SizeUInt;
begin
  AValue.NativeData.ForType := AType;
  case GetTypeData(AType)^.FloatType of
    ftSingle:
    begin
      AValue.NativeData.Data := GetMem(FitStackSize(SizeOf(Single)));
      AValue.NativeData.AlignedSize := 1;
      {$ifdef CPU64}
      PDWord(AValue.NativeData.Data)[1] := 0;
      {$endif}
      PSingle(AValue.NativeData.Data)^ := AValue.Float;
    end;
    ftDouble:
    begin
      AValue.NativeData.ForType := nil;
      AValue.NativeData.AlignedSize := 1 * CPU_SIZE_FACTOR;
      AValue.NativeData.Data := @AValue.Float;
    end;
{ TODO : Fix passing of Extended float type. }
    {
    ftExtended:
    begin
      Size := FitStackSize(SizeOf(Extended));
      AValue.NativeData.Data := GetMem(Size);
      FillDWord(AValue.NativeData.Data^, Size shr 2, 0);
      AValue.NativeData.AlignedSize := Size shr ({$ifdef CPU64}3{$else}2{$endif});
      PWord(AValue.NativeData.Data)[3] := PWord(@AValue.Float)[0];
      PWord(AValue.NativeData.Data)[4] := PWord(@AValue.Float)[1];
      PWord(AValue.NativeData.Data)[5] := PWord(@AValue.Float)[2];
      PWord(AValue.NativeData.Data)[6] := PWord(@AValue.Float)[3];
      PWord(AValue.NativeData.Data)[7] := PWord(@AValue.Float)[4];
      //PExtended(AValue.NativeData.Data)^ := AValue.Float;
    end;
    ftComp:
    begin
      AValue.NativeData.Data := GetMem(FitStackSize(SizeOf(Comp)));
      AValue.NativeData.AlignedSize := ceil(SizeOf(Comp) / STACK_SLOT_SIZE);
      PComp(AValue.NativeData.Data)^ := AValue.Float;
    end;
    ftCurr:
    begin
      AValue.NativeData.Data := GetMem(FitStackSize(SizeOf(Currency)));
      AValue.NativeData.AlignedSize := ceil(SizeOf(Currency) / STACK_SLOT_SIZE);
      PCurrency(AValue.NativeData.Data)^ := AValue.Float;
    end;
    }
  else
    raise EThoriumRuntimeException.CreateFmt('Cannot convert to this native float type: %s.', [GetEnumName(TypeInfo(TFloatType), Ord(GetTypeData(AType)^.FloatType))]);
  end;
end;

function TThoriumTypeFloat.DoToString(const AValue: TThoriumValue): String;
begin
  Result := FloatToStr(AValue.Float, THORIUM_NUMBER_FORMAT);
end;

end.

