unit Thorium_NativeCall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals, typinfo, Thorium_Utils;

procedure PrecompileNativeCall(var AInstructions: Pointer;
    AParameters: TThoriumHostParameters; AThParameters: TThoriumParameters;
    AReturnType: TThoriumHostType; AThReturnType: TThoriumType; HasData: Boolean;
    CallingConvention: TThoriumNativeCallingConvention);
procedure ExecuteNativeCall(Instructions: Pointer; StackTop: Pointer; Method: Pointer; Data: Pointer = nil);

implementation

var
  // These offsets are needed by GenericPrecompile. Don't ask me why there is no
  // possibility to let the compiler do the work.
  STACKENTRY_TYPE_OFFSET: SizeUInt;
  STACKENTRY_NATIVE_DATA_OFFSET: SizeUInt;
  STACKENTRY_VADATA_OFFSET: SizeUInt;


{ The following two types, one constant and two functions were taken from
  astrings.inc of the FreePascal RunTimeLibrary and have been modified by the
  author of Thorium to fit the special needs of the direct call feature. }

type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = Packed Record
    Ref,
    Len   : SizeInt;
    First : Char;
  end;

const
  FirstOff   = SizeOf(TAnsiRec)-1;

procedure ExtractedAnsiStrIncrRef(S: Pointer);
{Original function name: fpc_ansistr_incr_ref}
begin
  If S=Nil then
    exit;
  if IsMultiThread then
    {$ifdef CPU64}
    InterLockedIncrement64(PAnsiRec(S-FirstOff)^.Ref)
    {$else}
    InterLockedIncrement(PAnsiRec(S-FirstOff)^.Ref)
    {$endif}
  else
    Inc(PAnsiRec(S-FirstOff)^.Ref);
end;

procedure ExtractedAnsiStrDecrRef(var S: Pointer);
{Original function name: fpc_ansistr_decr_ref}
Var
  l : pSizeInt;
Begin
  If S=Nil then exit;
  l:=@PAnsiRec(S-FirstOff)^.Ref;
  if IsMultiThread then
  begin
    {$ifdef CPU64}
    If InterLockedDecrement64(l^) = 0 then
    {$else}
    If InterLockedDecrement(l^) = 0 then
    {$endif}
    begin
      Dec(S, FirstOff);
      FreeMem(S);
      S := nil;
    end;
  end
  else
  begin
    Dec(l^);
    if l^ = 0 then
    begin
      Dec(S, FirstOff);
      FreeMem(S);
      S := nil;
    end;
  end;
end;

(* Generates NativeCall subscript code which can later be used to call a native
function without knowing its signature at (program) compile time. *)
(*procedure GenericPrecompile(var AInstructions: Pointer; var AVAOffset: Integer; Parameters: TThoriumHostParameters; ReturnType: TThoriumHostType; HasData: Boolean; CallingConvention: TThoriumNativeCallingConvention);
var
  Instructions: array of TThoriumNativeCallInstruction;
  Capacity: Integer;
  Count: Integer;
  ParamCount: Integer;
  VAOffset: Integer; // Used for varargs

  function GetNextBlock: PThoriumNativeCallInstruction;
  begin
    if Count = Capacity then
    begin
      Inc(Capacity, 16);
      SetLength(Instructions, Capacity);
    end;
    Result := @Instructions[Count];
    Inc(Count);
  end;

  function EntryBaseOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := AIndex * SizeOf(TThoriumStackEntry);
  end;

  function TypeOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := EntryBaseOffset(AIndex) - STACKENTRY_TYPE_OFFSET;
  end;

  function IntOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := EntryBaseOffset(AIndex) - STACKENTRY_DATA_OFFSET;
  end;

  function VADataOffset(AIndex: Integer): SizeInt; inline;
  begin
    Result := EntryBaseOffset(AIndex) - STACKENTRY_VADATA_OFFSET;
  end;

var
  I: Integer;
  HostType, CleanHostType: TThoriumHostType;
  PreCalcVAOffset: Integer;
begin
  Capacity := 0;
  Count := 0;
  PreCalcVAOffset := 0;
  {$ifndef CPU64}
  if CallingConvention <> ncRegister then
    with GetNextBlock()^ do
    begin
      Instruction := ccSkipRegister;
      Offset := 0;
      Mask := 0;
    end;
  {$endif}

  ParamCount := Parameters.Count;
  for I := 0 to ParamCount - 1 do
    if Parameters.Types[I] and htArray = htArray then
      Inc(PreCalcVAOffset);
  AVAOffset := PreCalcVAOffset;
  if HasData then
  begin
    with GetNextBlock()^ do
    begin
      Instruction := ccData;
      Offset := 0;
      Mask := 0;
    end;
  end;
  {$ifndef CPU64}
  if CallingConvention = ncRegister then
  begin
  {$endif}
    VAOffset := PreCalcVAOffset;
    for I := 0 to ParamCount - 1 do
    begin
      HostType := Parameters.Types[I];
      CleanHostType := HostType and (not htFlagSection);
      with GetNextBlock()^ do
      begin
        if HostType and htArray = htArray then
        begin
          Instruction := ccVA;
          Mask := VADataOffset((ParamCount-(I+1))+VAOffset); // address of the array
          Dec(VAOffset);
          Offset := IntOffset((ParamCount-(I+1))+VAOffset); // address of the count
        end
        else
        begin
          case CleanHostType of
            htIntS8, htIntS16, htIntS32, htIntS64,
            htIntU8, htIntU16, htIntU32, htIntU64:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt32:
            begin
              Instruction := ccSingle;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt64:
            begin
              Instruction := ccDouble;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt80:
            begin
              Instruction := ccExtended;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htString:
            begin
              Instruction := ccPtrDeref;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htExt:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
          end;
          if Parameters.Types[I] and htByRef = htByRef then
            Inc(Instruction, DEREF_OFFSET);
        end;
      end;
    end;
    with GetNextBlock()^ do
    begin
      Mask := 0;
      case ReturnType.HostType and (not htFlagSection) of
        htNone:
        begin
          Instruction := ccCall;
          Offset := 0;
        end;
        htIntS8, htIntS16, htIntS32, htIntS64,
        htIntU8, htIntU16, htIntU32, htIntU64:
        begin
          Instruction := ccCallRetInt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt32:
        begin
          Instruction := ccCallRetSingle;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt64:
        begin
          Instruction := ccCallRetDouble;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt80:
        begin
          Instruction := ccCallRetExtended;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htString:
        begin
          Instruction := ccCallRetString;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htExt:
        begin
          Instruction := ccCallRetExt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
          Mask := SizeInt(ReturnType.Extended);
        end;
      end;
    end;
  {$ifndef CPU64}
  end
  else
  begin
    VAOffset := 0;
    for I := ParamCount - 1 downto 0 do
    begin
      HostType := Parameters.Types[I];
      CleanHostType := HostType and (not htFlagSection);
      with GetNextBlock()^ do
      begin
        if HostType and htArray = htArray then
        begin
          Instruction := ccVARev;
          Offset := IntOffset((ParamCount-(I+1))+VAOffset); // address of the count
          Inc(VAOffset);
          Mask := VADataOffset((ParamCount-(I+1))+VAOffset); // address of the array
        end
        else
        begin
          case CleanHostType of
            htIntS8, htIntS16, htIntS32,
            htIntU8, htIntU16, htIntU32:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htIntS64, htIntU64:
            begin
              Instruction := ccInt64;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt32:
            begin
              Instruction := ccSingle;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htFlt64:
            begin
              Instruction := ccDouble;
              Offset := IntOffset((ParamCount-(I+1))+VAOffsetccPutDataRegMMX);
              Mask := 0;
            end;
            htFlt80:
            begin
              Instruction := ccExtended;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htString:
            begin
              Instruction := ccPtrDeref;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
            htExt:
            begin
              Instruction := ccInt;
              Offset := IntOffset((ParamCount-(I+1))+VAOffset);
              Mask := 0;
            end;
          end;
          if HostType and htFlagSection = htByRef then
            Inc(Instruction, DEREF_OFFSET);
        end;
      end;
    end;
    with GetNextBlock()^ do
    begin
      Mask := 0;
      case ReturnType.HostType and (not htFlagSection) of
        htNone:
        begin
          Instruction := ccCall;
          Offset := 0;
        end;
        htIntS8, htIntS16, htIntS32,
        htIntU8, htIntU16, htIntU32:
        begin
          Instruction := ccCallRetInt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htIntS64, htIntU64:
        begin
          Instruction := ccCallRetInt64;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt32:
        begin
          Instruction := ccCallRetSingle;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt64:
        begin
          Instruction := ccCallRetDouble;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htFlt80:
        begin
          Instruction := ccCallRetExtended;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htString:
        begin
          Instruction := ccCallRetString;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
        end;
        htExt:
        begin
          Instruction := ccCallRetExt;
          Offset := IntOffset(ParamCount+PreCalcVAOffset);
          Mask := SizeInt(ReturnType.Extended);
        end;
      end;
    end;
  end;
  {$endif}
  VAOffset := PreCalcVAOffset;
  for I := 0 to ParamCount - 1 do
  begin
    HostType := Parameters.Types[I];
    if HostType and htArray = htArray then
      Dec(VAOffset)
    else if HostType = htFlt32 then
    begin
      with GetNextBlock()^ do
      begin
        Instruction := ccRevSingle;
        Offset := IntOffset((ParamCount-(I+1))+VAOffset);
        Mask := 0;
      end;
    end
    else if HostType = htFlt80 then
    begin
      with GetNextBlock()^ do
      begin
        Instruction := ccRevExtended;
        Offset := IntOffset((ParamCount-(I+1))+VAOffset);
        Mask := 0;
      end;
    end;
  end;
  with GetNextBlock()^ do
  begin
    Instruction := ccExit;
    Offset := 0;
    Mask := 0;
  end;
  if AInstructions <> nil then
    FreeMem(AInstructions);
  AInstructions := GetMem(Count * SizeOf(TThoriumNativeCallInstruction));
  Move(Instructions[0], AInstructions^, Count * SizeOf(TThoriumNativeCallInstruction));
end;    *)

procedure PrecompileNativeCall(var AInstructions: Pointer;
    AParameters: TThoriumHostParameters; AThParameters: TThoriumParameters;
    AReturnType: TThoriumHostType; AThReturnType: TThoriumType; HasData: Boolean;
    CallingConvention: TThoriumNativeCallingConvention);

const
  NATIVE_REGISTER_COUNT = {$ifdef CPU64}6{$else}3{$endif};
  NATIVE_DOUBLE_REGISTER_COUNT = {$ifdef CPU64}8{$else}0{$endif};

var
  Instructions: array of TThoriumNativeCallInstruction;
  Count, Capacity: Integer;
  NativeRegistersUsed: Integer;
  NativeDoubleRegistersUsed: Integer;

  function NewInstruction: PThoriumNativeCallInstruction;
  begin
    if Count = Capacity then
    begin
      Capacity += 16;
      SetLength(Instructions, Capacity);
    end;
    Result := @Instructions[Count];
    Result^.Instruction := ccNone;
    Result^.Data1 := 0;
    Result^.Data2 := 0;
    Inc(Count);
  end;

  function BaseOffset(const StackOffset: Integer): SizeInt; inline;
  begin
    Exit(StackOffset * SizeOf(TThoriumStackEntry));
  end;

  function NativeDataOffset(const StackOffset: Integer): SizeInt; inline;
  begin
    Exit(BaseOffset(StackOffset) - STACKENTRY_NATIVE_DATA_OFFSET);
  end;

var
  I: Integer;
  ParamType: PTypeInfo;
  ThParamType: TThoriumType;
  TypeData: PTypeData;
  Instruction: PThoriumNativeCallInstruction;
  CurrStackOffset: Integer;
  Spec: TThoriumNativeCallSpecification;
  ParamCount: Integer;
begin
  NativeRegistersUsed := 0;
  NativeDoubleRegistersUsed := 0;
  Capacity := 0;
  Count := 0;

  if HasData then
  begin
    Inc(NativeRegistersUsed);
    Instruction := NewInstruction;
    Instruction^.Instruction := ccData;
  end;

  // Redesign made all data available as pointers. So we should have a way
  // easier process of appliance now.
  ParamCount := AParameters.Count;
  for I := 0 to ParamCount - 1 do
  begin
    CurrStackOffset := ParamCount - (I+1);
    ParamType := AParameters[I];
    ThParamType := AThParameters[I];
    Spec := ThParamType.GetNativeCallSpecification(ParamType);
    Instruction := nil;
    case Spec.RefMode of
      rmRefToPtr:
      begin
        if (Spec.Count <> 1) or (Spec.ForceStack) then
          raise EThoriumCompilerException.Create('Unsupported parameters for this RefMode.');
        Instruction := NewInstruction;
        Instruction^.Instruction := ccPutRefRef;
      end;
      rmPtr:
      begin
        if (Spec.Count <> 1) or (Spec.ForceStack) then
          raise EThoriumCompilerException.Create('Unsupported parameters for this RefMode.');
        Instruction := NewInstruction;
        Instruction^.Instruction := ccPutRef;
      end;
      rmData:
      begin
        if Spec.ForceStack then
          Instruction^.Instruction := ccPutLargeDataStack
        else
        begin
          case Spec.ValueMode of
            vmInt32, vmPointer{$ifdef CPU64}, vmInt64{$endif}:
            begin
              Instruction := NewInstruction;
              if (Spec.Count <> 1) then
                Instruction^.Instruction := ccPutLargeDataInt
              else
                Instruction^.Instruction := ccPutDataRegInt;
            end;
            {$ifndef CPU64}
            vmInt64:
            begin
              if Spec.Count <> 1 then
                raise EThoriumCompilerException.Create('Cannot pass multiple Int64 on this platform. You should probably pass a reference instead.');
              Instruction := NewInstruction;
              Instruction^.Instruction := ccPutLargeDataStack;
            end;
            {$endif}
            vmFloat:
            begin
              Instruction := NewInstruction;
              case Spec.FloatMode of
                fmAsSingle{$ifdef CPU64}, fmAsDouble{$endif}:
                begin
                  {$ifdef CPU64}
                  if Spec.Count <> 1 then
                    Instruction^.Instruction := ccPutLargeDataFloat
                  else
                    Instruction^.Instruction := ccPutDataRegXMM;
                  {$else}
                  if Spec.Count <> 1 then
                    Instruction^.Instruction := ccPutLargeDataStack
                  else
                    Instruction^.Instruction := ccPutDataStack;
                  {$endif}
                end;
                fmAsExtended{$ifdef CPU32}, fmAsDouble{$endif}:
                begin
                  if Spec.Count <> 1 then
                    raise EThoriumCompilerException.Create('Cannot pass multiple Extended/Double floats at once on this platform. You should probably pass a reference instead.');
                  Instruction^.Instruction := ccPutLargeDataStack;
                end;
              end;
            end;
            vmString:
            begin
              if Spec.Count <> 1 then
                raise EThoriumCompilerException.Create('Cannot pass multiple strings at once. You should probably pass a reference instead.');
              Instruction := NewInstruction;
              Instruction^.Instruction := ccIncrStrRef;
              Instruction^.Data1 := NativeDataOffset(CurrStackOffset);
              Instruction := NewInstruction;
              Instruction^.Instruction := ccPutDataRegInt;
            end;
          end;
        end;
      end;
    end;
    if Instruction <> nil then
      Instruction^.Data1 := NativeDataOffset(CurrStackOffset);
  end;

  ParamType := AReturnType.HostType;
  ThParamType := AThReturnType;
  if ParamType <> nil then
  begin
    raise EThoriumCompilerException.CreateFmt('Unsupported NativeCall return type: %d (%s).', [Ord(ParamType^.Kind), GetEnumName(TypeInfo(TTypeKind), Ord(ParamType^.Kind))]);
    {case ParamType^.Kind of
      tkBool, tkInteger, tkEnumeration, tkSet, tkUChar, tkWChar, tkChar:
      begin
        // These are all 32bits or less
        Instruction := NewInstruction;
        Instruction^.Instruction := cc
        Instruction^.Data1 := NativeDataOffset(CurrStackOffset);
      end;

    end;}
  end
  else
  begin
    Instruction := NewInstruction;
    Instruction^.Instruction := ccCallNone;
  end;

  Instruction := NewInstruction;
  Instruction^.Instruction := ccExit;
  AInstructions := GetMem(SizeOf(TThoriumNativeCallInstruction) * Count);
  Move(Instructions[0], AInstructions^, SizeOf(TThoriumNativeCallInstruction) * Count);
end;

label
  lccData, lccPutRef, lccPutRefRef, lccPutDataRegInt, lccPutDataRegXMM,
  lccPutDataStack, lccPutLargeDataStack, lccPutLargeDataInt, lccPutLargeDataFloat,
  lccCallNone, lccCallRetValue, lccCallRetRef, lccCallRetRefRef, lccCallRetMMX, lccCallRetExtended,
  lccClearStack, lccIncrStrRef, lccDecrStrRef,
  lccExit,

  irRDI, irRSI, irRDX, irRCX, irR8, irR9, irPush,
  frXMM0, frXMM1, frXMM2, frXMM3, frXMM4, frXMM5, frXMM6, frXMM7, frPush,

  lLoop, lLargeDataStackLoop, lLargeDataIntLoop, lLargeDataIntLoopEnd,
  lLargeDataFloatLoop, lLargeDataFloatLoopEnd;
const
  InstrMap: array [TThoriumNativeCallInstructionCode] of Pointer = (
    @lccData, @lccPutRefRef, @lccPutRef, @lccPutDataRegInt, @lccPutDataRegXMM,
    @lccPutDataStack, @lccPutLargeDataStack, @lccPutLargeDataInt, @lccPutLargeDataFloat,
    @lccCallNone, @lccCallRetValue, @lccCallRetRefRef, @lccCallRetRef, @lccCallRetMMX, @lccCallRetExtended,
    @lccClearStack, @lccIncrStrRef, @lccDecrStrRef, @lccExit, nil
  );
const
  IntReg : array [0..6] of Pointer = (
    @irRDI, @irRSI, @irRDX, @irRCX, @irR8, @irR9, @irPush
  );
  FloatReg : array [0..8] of Pointer = (
    @frXMM0, @frXMM1, @frXMM2, @frXMM3, @frXMM4, @frXMM5, @frXMM6, @frXMM7, @frPush
  );

procedure ExecuteNativeCall(Instructions: Pointer; StackTop: Pointer; Method: Pointer; Data: Pointer = nil);
var
  RDI, RAX, RBX, R10, R11, R12, R13, R14, R15: QWord;
  RDI2: QWord;
begin
  // rax: buffer
  // rbx: back jump addr
  // rcx: param
  // rdx: param
  // rdi: param
  // rsi: param
  // r8 : param
  // r9 : param
  // r10: stack top
  // r11: int reg instruction ptr
  // r12: Instruction ptr
  // r13: Instruction map ptr
  // r14: value ptr
  // r15: flt reg instruction ptr
  asm
    movq %rax, RAX
    movq %rbx, RBX
    movq %r10, R10
    movq %r11, R11
    movq %r12, R12
    movq %r13, R13
    movq %r14, R14
    movq %r15, R15
    movq Instructions, %r12
    movq StackTop, %r10
    leaq InstrMap, %r13
    leaq IntReg, %r11
    leaq FloatReg, %r15
    leaq lLoop, %rbx
lLoop:
      xorq %rax, %rax
      movw (%r12), %ax
      addq $2, %r12

      movq %r10, %r14
      subq (%r12), %r14
      // addq $16, %r14

      addq $8, %r12

      imulq $8, %rax
      addq %r13, %rax
      movq (%rax), %rax
      jmp %rax
lccData:
      addq $8, %r12
      movq Data, %r14
      jmp (%r11)
lccPutRefRef:
      addq $8, %r12
      jmp (%r11)
lccPutRef:
      addq $8, %r12
      // %r14 contains the adress of the value (per def. of NativeData)
      // For some types, it may be the adress of a pointer, this needs to be
      // applied in the compilation though
      movq (%r14), %r14
      jmp (%r11)
lccPutDataRegInt:
      addq $8, %r12
      movq (%r14), %r14
      movq (%r14), %r14
      jmp (%r11)
lccPutDataRegXMM:
      // note that, due to the nature of the movlps instruction, no second
      // dereferentiation is possible
      addq $8, %r12
      movq (%r14), %r14
      jmp (%r15)
lccPutDataStack:
      addq $8, %r12
      pushq (%r14)
      jmp %rbx
lccPutLargeDataStack:
      addq $8, %r12
      movq %r14, %rax
      addq $8, %rax
      movq (%rax), %rax // get the size from the NativeValue data
      movq (%r14), %r14
  lLargeDataStackLoop:
        test %rax, %rax
        jz lLoop
        decq %rax
        pushq (%r14)
        addq $8, %r14
        jmp lLargeDataStackLoop
lccPutLargeDataInt:
      addq $8, %r12
      movq %r14, %rax
      addq $8, %rax
      movq (%rax), %rax
      leaq lLargeDataIntLoop, %rbx
      movq (%r14), %r14
      subq $8, %r14
  lLargeDataIntLoop:
        test %rax, %rax
        jz lLargeDataIntLoopEnd
        decq %rax
        addq $8, %r14
        jmp (%r11)
  lLargeDataIntLoopEnd:
      leaq lLoop, %rbx
      jmp %rbx
lccPutLargeDataFloat:
      addq $8, %r12
      movq %r14, %rax
      addq $8, %rax
      movq (%rax), %rax
      leaq lLargeDataFloatLoop, %rbx
      movq (%r14), %r14
      subq $8, %r14
  lLargeDataFloatLoop:
        test %rax, %rax
        jz lLargeDataFloatLoopEnd
        decq %rax
        addq $8, %r14
        jmp (%r11)
  lLargeDataFloatLoopEnd:
      leaq lLoop, %rbx
      jmp %rbx
lccCallNone:
      addq $8, %r12
      xorq %rax, %rax
      call Method
      jmp %rbx
lccCallRetValue:
      addq $8, %r12
      xorq %rax, %rax
      movq (%r14), %r14
      movq (%r14), %rax
      call Method
      movq %rax, (%r14)
      jmp %rbx
lccCallRetRefRef:
      addq $8, %r12
      call Method
      jmp %rbx
lccCallRetRef:
      addq $8, %r12
      movq (%r14), %r14
      call Method
      jmp %rbx
lccCallRetMMX:
      addq $8, %r12
      movq (%r14), %r14
      movlps (%r14), %xmm0
      call Method
      movlps %xmm0, (%r14)
      jmp %rbx
lccCallRetExtended:
      addq $8, %r12
      call Method
      movq (%r14), %r14
      fstpl (%r14)
      jmp %rbx
lccClearStack:
      addq $8, %r12
      // not implemented in x64
      jmp %rbx
lccIncrStrRef:
      addq $8, %r12
      movq (%r14), %r14
      movq %rdi, RDI2
      movq (%r14), %rdi
      call ExtractedAnsiStrIncrRef
      movq RDI2, %rdi
      jmp %rbx
lccDecrStrRef:
      addq $8, %r12
      movq (%r14), %r14
      movq (%r14), %rdi
      call ExtractedAnsiStrDecrRef
      jmp %rbx
irRDI:
      movq %r14, %rdi
      addq $8, %r11
      jmp %rbx
irRSI:
      movq %r14, %rsi
      addq $8, %r11
      jmp %rbx
irRDX:
      movq %r14, %rdx
      addq $8, %r11
      jmp %rbx
irRCX:
      movq %r14, %rcx
      addq $8, %r11
      jmp %rbx
irR8:
      movq %r14, %r8
      addq $8, %r11
      jmp %rbx
irR9:
      movq %r14, %r9
      addq $8, %r11
      jmp %rbx
irPush:
      pushq %r14
      jmp %rbx
frXMM0:
      movlpd (%r14), %xmm0
      addq $8, %r15
      jmp %rbx
frXMM1:
      movlpd (%r14), %xmm1
      addq $8, %r15
      jmp %rbx
frXMM2:
      movlpd (%r14), %xmm2
      addq $8, %r15
      jmp %rbx
frXMM3:
      movlpd (%r14), %xmm3
      addq $8, %r15
      jmp %rbx
frXMM4:
      movlpd (%r14), %xmm4
      addq $8, %r15
      jmp %rbx
frXMM5:
      movlpd (%r14), %xmm5
      addq $8, %r15
      jmp %rbx
frXMM6:
      movlpd (%r14), %xmm6
      addq $8, %r15
      jmp %rbx
frXMM7:
      movlpd (%r14), %xmm7
      addq $8, %r15
      jmp %rbx
frPush:
      pushq (%r14)
      jmp %rbx
lccExit:
      movq RAX, %rax
      movq RBX, %rbx
      movq R10, %r10
      movq R11, %r11
      movq R12, %r12
      movq R13, %r13
      movq R14, %r14
      movq R15, %r15
  end;
end;

var
  // Used for the offset calculation below.
  TestEntry: TThoriumStackEntry;

initialization
// See the comment about the variables in the Native call helpers region.
STACKENTRY_TYPE_OFFSET := Offset(TestEntry, TestEntry.Value.RTTI);
STACKENTRY_NATIVE_DATA_OFFSET := Offset(TestEntry, TestEntry.Value.NativeData.Data);
STACKENTRY_VADATA_OFFSET := Offset(TestEntry, TestEntry.VarargsBuffer.DataOrigin);

end.

