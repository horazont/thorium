(* These functions are helpers to fill the instruction record for the
   instruction given.*)

function int_s(AValue: Int64): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINT_S(Result) do
  begin
    Instruction := tiINT_S;
    Value := AValue;
    CodeLine := 0;
  end;
end;

function int(AValue: Int64; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINT(Result) do
  begin
    Instruction := tiINT;
    Value := AValue;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function intb(ATRI: Word; AKind: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINTB(Result) do
  begin
    Instruction := tiINTB;
    TRI := ATRI;
    Kind := AKind;
    CodeLine := 0;
  end;
end;

function flt_s(AValue: Double): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFLT_S(Result) do
  begin
    Instruction := tiFLT_S;
    Value := AValue;
    CodeLine := 0;
  end;
end;

function flt(AValue: Double; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFLT(Result) do
  begin
    Instruction := tiFLT;
    Value := AValue;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function str_s(): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTR_S(Result) do
  begin
    Instruction := tiSTR_S;
    CodeLine := 0;
  end;
end;

function strl_s(AIndex: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTRL_S(Result) do
  begin
    Instruction := tiSTRL_S;
    Index := AIndex;
    CodeLine := 0;
  end;
end;

function str(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTR(Result) do
  begin
    Instruction := tiSTR;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function strl(AIndex: Cardinal; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSTRL(Result) do
  begin
    Instruction := tiSTRL;
    Index := AIndex;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function ext_s(AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionEXT_S(Result) do
  begin
    Instruction := tiEXT_S;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function ext(AExtendedType: TThoriumHostObjectType; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionEXT(Result) do
  begin
    Instruction := tiEXT;
    ExtendedType := AExtendedType;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function fnc(AFunctionRef: TThoriumFunction; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFNC(Result) do
  begin
    Instruction := tiFNC;
    FunctionRef := AFunctionRef;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xfnc(AFunctionRef: TThoriumHostFunctionBase; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXFNC(Result) do
  begin
    Instruction := tiXFNC;
    FunctionRef := AFunctionRef;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xmeth(AMethodRef: TThoriumHostMethodBase; ATRI, AERI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXMETH(Result) do
  begin
    Instruction := tiXMETH;
    MethodRef := AMethodRef;
    TRI := ATRI;
    ERI := AERI;
    CodeLine := 0;
  end;
end;

function mover_g(ASRI: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_G(Result) do
  begin
    Instruction := tiMOVER_G;
    SRI := ASRI;
    Offset := AOffset;
  end;
end;

function copyr_g(ASRI: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_G(Result) do
  begin
    Instruction := tiCOPYR_G;
    SRI := ASRI;
    Offset := AOffset;
  end;
end;

function moveg(AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVEG(Result) do
  begin
    Instruction := tiMOVEG;
    Offset := AOffset;
    TRI := ATRI;
  end;
end;

function copyg(AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYG(Result) do
  begin
    Instruction := tiCOPYG;
    Offset := AOffset;
    TRI := ATRI;
  end;
end;

function mover_fg(ASRI: Word; AOffset: LongInt; AModuleRef: TThoriumModule): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_FG(Result) do
  begin
    Instruction := tiMOVER_FG;
    SRI := ASRI;
    Offset := AOffset;
    ModuleRef := AModuleRef;
  end;
end;

function copyr_fg(ASRI: Word; AOffset: LongInt; AModuleRef: TThoriumModule): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_FG(Result) do
  begin
    Instruction := tiCOPYR_FG;
    SRI := ASRI;
    Offset := AOffset;
    ModuleRef := AModuleRef;
  end;
end;

function movefg(AOffset: LongInt; ATRI: Word; AModuleRef: TThoriumModule): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVEFG(Result) do
  begin
    Instruction := tiMOVEFG;
    Offset := AOffset;
    TRI := ATRI;
    ModuleRef := AModuleRef;
  end;
end;

function copyfg(AOffset: LongInt; ATRI: Word; AModuleRef: TThoriumModule): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYFG(Result) do
  begin
    Instruction := tiCOPYFG;
    Offset := AOffset;
    TRI := ATRI;
    ModuleRef := AModuleRef;
  end;
end;

function mover_l(ASRI: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_L(Result) do
  begin
    Instruction := tiMOVER_L;
    SRI := ASRI;
    Offset := AOffset;
  end;
end;

function copyr_l(ASRI: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_L(Result) do
  begin
    Instruction := tiCOPYR_L;
    SRI := ASRI;
    Offset := AOffset;
  end;
end;

function movel(AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVEL(Result) do
  begin
    Instruction := tiMOVEL;
    Offset := AOffset;
    TRI := ATRI;
  end;
end;

function copyl(AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYL(Result) do
  begin
    Instruction := tiCOPYL;
    Offset := AOffset;
    TRI := ATRI;
  end;
end;

function mover_p(ASRI: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_P(Result) do
  begin
    Instruction := tiMOVER_P;
    SRI := ASRI;
    Offset := AOffset;
  end;
end;

function copyr_p(ASRI: Word; AOffset: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_P(Result) do
  begin
    Instruction := tiCOPYR_P;
    SRI := ASRI;
    Offset := AOffset;
  end;
end;

function movep(AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVEP(Result) do
  begin
    Instruction := tiMOVEP;
    Offset := AOffset;
    TRI := ATRI;
  end;
end;

function copyp(AOffset: LongInt; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYP(Result) do
  begin
    Instruction := tiCOPYP;
    Offset := AOffset;
    TRI := ATRI;
  end;
end;

function copyr_st(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR_ST(Result) do
  begin
    Instruction := tiCOPYR_ST;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function copyr(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCOPYR(Result) do
  begin
    Instruction := tiCOPYR;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function mover_st(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER_ST(Result) do
  begin
    Instruction := tiMOVER_ST;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function mover(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVER(Result) do
  begin
    Instruction := tiMOVER;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function movest(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOVEST(Result) do
  begin
    Instruction := tiMOVEST;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function pop_s(AAmount: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionPOP_S(Result) do
  begin
    Instruction := tiPOP_S;
    Amount := AAmount;
    CodeLine := 0;
  end;
end;

function clr(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCLR(Result) do
  begin
    Instruction := tiCLR;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castif(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTIF(Result) do
  begin
    Instruction := tiCASTIF;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castie(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTIE(Result) do
  begin
    Instruction := tiCASTIE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function castfe(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTFE(Result) do
  begin
    Instruction := tiCASTFE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function castse(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTSE(Result) do
  begin
    Instruction := tiCASTSE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

function castei(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTEI(Result) do
  begin
    Instruction := tiCASTEI;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castef(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTEF(Result) do
  begin
    Instruction := tiCASTEF;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function castes(ASRI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTES(Result) do
  begin
    Instruction := tiCASTES;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function caste(ASRI: Word; ATRI: Word; AExtendedType: TThoriumHostObjectType): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCASTE(Result) do
  begin
    Instruction := tiCASTE;
    SRI := ASRI;
    TRI := ATRI;
    ExtendedType := AExtendedType;
    CodeLine := 0;
  end;
end;

(*function _cast(ASRI: Word; ATRI: Word; ATypeA, ATypeB: TThoriumType): TThoriumInstruction;
const
  typeInteger = 0;
  typeFloat = 1;
  typeString = 2;
  typeExtended = 3;
var
  TypeA, TypeB: Word;
begin
  if ATypeA.ValueType = vtBuiltIn then
  begin
    if ATypeA.BuiltInType = btInteger then
      TypeA := typeInteger
    else if ATypeA.BuiltInType = btFloat then
      TypeA := typeFloat
    else if ATypeA.BuiltInType = btString then
      TypeA := typeString
    else
      Exit;
  end
  else if ATypeA.ValueType = vtExtendedType then
    TypeA := typeExtended
  else
    Exit;
  if ATypeB.ValueType = vtBuiltIn then
  begin
    if ATypeB.BuiltInType = btInteger then
      TypeB := typeInteger
    else if ATypeB.BuiltInType = btFloat then
      TypeB := typeFloat
    else if ATypeB.BuiltInType = btString then
      TypeB := typeString
    else
      Exit;
  end
  else if ATypeB.ValueType = vtExtendedType then
    TypeB := typeExtended
  else
    Exit;

  case TypeA of
    typeInteger: case TypeB of
      typeFloat: Result := castif(ASRI, ATRI);
      typeExtended: Result := castie(ASRI, ATRI, ATypeB.Extended);
    end;
    typeFloat: case TypeB of
      typeExtended: Result := castfe(ASRI, ATRI, ATypeB.Extended);
    end;
    typeString: case TypeB of
      typeExtended: Result := castse(ASRI, ATRI, ATypeB.Extended);
    end;
    typeExtended: case TypeB of
      typeInteger: Result := castei(ASRI, ATRI);
      typeFloat: Result := castef(ASRI, ATRI);
      typeString: Result := castes(ASRI, ATRI);
      typeExtended: Result := caste(ASRI, ATRI, ATypeB.Extended);
    end;
  end;
end;    *)

function cmpi(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPI(Result) do
  begin
    Instruction := tiCMPI;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpif(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPIF(Result) do
  begin
    Instruction := tiCMPIF;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpie(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPIE(Result) do
  begin
    Instruction := tiCMPIE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpf(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPF(Result) do
  begin
    Instruction := tiCMPF;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpfi(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPFI(Result) do
  begin
    Instruction := tiCMPFI;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpfe(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPFE(Result) do
  begin
    Instruction := tiCMPFE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmps(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPS(Result) do
  begin
    Instruction := tiCMPS;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpse(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPSE(Result) do
  begin
    Instruction := tiCMPSE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpe(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPE(Result) do
  begin
    Instruction := tiCMPE;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpei(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPEI(Result) do
  begin
    Instruction := tiCMPEI;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpef(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPEF(Result) do
  begin
    Instruction := tiCMPEF;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function cmpes(AOp1: Word; AOp2: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCMPES(Result) do
  begin
    Instruction := tiCMPES;
    Op1 := AOp1;
    Op2 := AOp2;
    CodeLine := 0;
  end;
end;

function evali(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionEVALI(Result) do
  begin
    Instruction := tiEVALI;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function addi(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionADDI(Result) do
  begin
    Instruction := tiADDI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function addf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionADDF(Result) do
  begin
    Instruction := tiADDF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function adds(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionADDS(Result) do
  begin
    Instruction := tiADDS;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function subi(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSUBI(Result) do
  begin
    Instruction := tiSUBI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function subf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSUBF(Result) do
  begin
    Instruction := tiSUBF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function muli(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMULI(Result) do
  begin
    Instruction := tiMULI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function mulf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMULF(Result) do
  begin
    Instruction := tiMULF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function divi(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDIVI(Result) do
  begin
    Instruction := tiDIVI;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function divf(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDIVF(Result) do
  begin
    Instruction := tiDIVF;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function negi(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNEGI(Result) do
  begin
    Instruction := tiNEGI;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function negf(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNEGF(Result) do
  begin
    Instruction := tiNEGF;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function _not(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNOT(Result) do
  begin
    Instruction := tiNOT;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function bnot(AOp1: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionBNOT(Result) do
  begin
    Instruction := tiBNOT;
    Op1 := AOp1;
    CodeLine := 0;
  end;
end;

function _mod(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionMOD(Result) do
  begin
    Instruction := tiMOD;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _and(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionAND(Result) do
  begin
    Instruction := tiAND;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _or(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionOR(Result) do
  begin
    Instruction := tiOR;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _xor(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXOR(Result) do
  begin
    Instruction := tiXOR;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _shl(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSHL(Result) do
  begin
    Instruction := tiSHL;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function _shr(AOp1: Word; AOp2: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionSHR(Result) do
  begin
    Instruction := tiSHR;
    Op1 := AOp1;
    Op2 := AOp2;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function inci(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCI(Result) do
  begin
    Instruction := tiINCI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function incf(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionINCF(Result) do
  begin
    Instruction := tiINCF;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function deci(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECI(Result) do
  begin
    Instruction := tiDECI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function decf(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDECF(Result) do
  begin
    Instruction := tiDECF;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xpget(AProp: TThoriumLibraryProperty; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXPGET(Result) do
  begin
    Instruction := tiXPGET;
    Prop := AProp;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xpset(AProp: TThoriumLibraryProperty; ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXPSET(Result) do
  begin
    Instruction := tiXPSET;
    Prop := AProp;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function xfget(AID: Int64; AERI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXFGET(Result) do
  begin
    Instruction := tiXFGET;
    ID := AID;
    ERI := AERI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xfset(AID: Int64; AERI: Word; AVRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXFSET(Result) do
  begin
    Instruction := tiXFSET;
    ID := AID;
    ERI := AERI;
    VRI := AVRI;
    CodeLine := 0;
  end;
end;

function xsfget(AID: Int64; AExtendedType: TThoriumHostObjectType; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXSFGET(Result) do
  begin
    Instruction := tiXSFGET;
    ID := AID;
    ExtendedType := AExtendedType;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xsfset(AID: Int64; AExtendedType: TThoriumHostObjectType; AVRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXSFSET(Result) do
  begin
    Instruction := tiXSFSET;
    ID := AID;
    ExtendedType := AExtendedType;
    VRI := AVRI;
    CodeLine := 0;
  end;
end;

function xiget(AIRI: Word; AERI: Word; ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXIGET(Result) do
  begin
    Instruction := tiXIGET;
    IRI := AIRI;
    ERI := AERI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function xiset(AIRI: Word; AVRI: Word; AERI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXISET(Result) do
  begin
    Instruction := tiXISET;
    IRI := AIRI;
    VRI := AVRI;
    ERI := AERI;
    CodeLine := 0;
  end;
end;

function xct(AERI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXCT(Result) do
  begin
    Instruction := tiXCT;
    ERI := AERI;
    CodeLine := 0;
  end;
end;

function x2n(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionX2N(Result) do
  begin
    Instruction := tiX2N;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function n2x(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionN2X(Result) do
  begin
    Instruction := tiN2X;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function clrn(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCLRN(Result) do
  begin
    Instruction := tiCLRN;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function vastart(ALength: Cardinal; AIsPointerBased: Boolean): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVASTART(Result) do
  begin
    Instruction := tiVASTART;
    Length := ALength;
    if AIsPointerBased then
      Pointers := 1
    else
      Pointers := 0;
    CodeLine := 0;
  end;
end;

function va_i8(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I8(Result) do
  begin
    Instruction := tiVA_I8;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i16(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I16(Result) do
  begin
    Instruction := tiVA_I16;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i32(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I32(Result) do
  begin
    Instruction := tiVA_I32;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i64(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I64(Result) do
  begin
    Instruction := tiVA_I64;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i8s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I8S(Result) do
  begin
    Instruction := tiVA_I8S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i16s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I16S(Result) do
  begin
    Instruction := tiVA_I16S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i32s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I32S(Result) do
  begin
    Instruction := tiVA_I32S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_i64s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_I64S(Result) do
  begin
    Instruction := tiVA_I64S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_f32(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_F32(Result) do
  begin
    Instruction := tiVA_F32;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_f64(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_F64(Result) do
  begin
    Instruction := tiVA_F64;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_f80(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_F80(Result) do
  begin
    Instruction := tiVA_F80;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_S(Result) do
  begin
    Instruction := tiVA_S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function va_x(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVA_X(Result) do
  begin
    Instruction := tiVA_X;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vastart_t(ALength: Cardinal; AFloatCount: Cardinal; AToClear: Cardinal): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVASTART_T(Result) do
  begin
    Instruction := tiVASTART_T;
    Length := ALength;
    Floats := AFloatCount;
    ToClear := AToClear;
    CodeLine := 0;
  end;
end;

function vat_f(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_F(Result) do
  begin
    Instruction := tiVAT_F;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vat_i(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_I(Result) do
  begin
    Instruction := tiVAT_I;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vat_s(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_S(Result) do
  begin
    Instruction := tiVAT_S;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vat_x(ASRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAT_X(Result) do
  begin
    Instruction := tiVAT_X;
    SRI := ASRI;
    CodeLine := 0;
  end;
end;

function vafinish(): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionVAFINISH(Result) do
  begin
    Instruction := tiVAFINISH;
    CodeLine := 0;
  end;
end;

function tostr_i(ASRI, ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionTOSTR_I(Result) do
  begin
    Instruction := tiTOSTR_I;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function tostr_f(ASRI, ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionTOSTR_F(Result) do
  begin
    Instruction := tiTOSTR_F;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function ref(ASRI, ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionREF(Result) do
  begin
    Instruction := tiREF;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function deref(ASRI, ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionDEREF(Result) do
  begin
    Instruction := tiDEREF;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function len_s(ASRI, ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionLEN_S(Result) do
  begin
    Instruction := tiLEN_S;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function len_a(ASRI, ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionLEN_A(Result) do
  begin
    Instruction := tiLEN_A;
    SRI := ASRI;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function update_x(ATRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionUPDATE_X(Result) do
  begin
    Instruction := tiUPDATE_X;
    TRI := ATRI;
    CodeLine := 0;
  end;
end;

function jmp(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJMP(Result) do
  begin
    Instruction := tiJMP;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function je(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJE(Result) do
  begin
    Instruction := tiJE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jne(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJNE(Result) do
  begin
    Instruction := tiJNE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jgt(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJGT(Result) do
  begin
    Instruction := tiJGT;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jge(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJGE(Result) do
  begin
    Instruction := tiJGE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jlt(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJLT(Result) do
  begin
    Instruction := tiJLT;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jle(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJLE(Result) do
  begin
    Instruction := tiJLE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

// These two are aliases which are used in expression evaluation. They may be
// substitued later by actually distinct instructions.
function jt(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJE(Result) do
  begin
    Instruction := tiJE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function jf(ANewAddress: TThoriumInstructionAddress): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionJNE(Result) do
  begin
    Instruction := tiJNE;
    NewAddress := ANewAddress;
    CodeLine := 0;
  end;
end;

function call(ASRI, AHRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionCALL(Result) do
  begin
    Instruction := tiCALL;
    SRI := ASRI;
    HRI := AHRI;
    CodeLine := 0;
  end;
end;

function fcall(ASRI, AHRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionFCALL(Result) do
  begin
    Instruction := tiFCALL;
    SRI := ASRI;
    HRI := AHRI;
    CodeLine := 0;
  end;
end;

function xcall(ASRI, AHRI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXCALL(Result) do
  begin
    Instruction := tiXCALL;
    SRI := ASRI;
    HRI := AHRI;
    CodeLine := 0;
  end;
end;

function xcall_m(ASRI, AHRI, AERI: Word): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionXCALL_M(Result) do
  begin
    Instruction := tiXCALL_M;
    SRI := ASRI;
    HRI := AHRI;
    ERI := AERI;
    CodeLine := 0;
  end;
end;

function ret(): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionRET(Result) do
  begin
    Instruction := tiRET;
    CodeLine := 0;
  end;
end;

function noop(AKind: Word; AParameter1, AParameter2: Int64; AParameter3: LongInt): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionNOOP(Result) do
  begin
    Instruction := tiNOOP;
    Kind := AKind;
    Parameter1 := AParameter1;
    Parameter2 := AParameter2;
    Parameter3 := AParameter3;
    CodeLine := 0;
  end;
end;

function EmbeddedHint(AStr: String): TThoriumInstruction;
begin
  FillByte(Result, SizeOf(TThoriumInstruction), 0);
  with TThoriumInstructionEmbeddedHint(Result) do
  begin
    Instruction := tiEmbeddedHint;
    if Length(AStr) > Length(Data) then
      raise EThoriumCompilerException.Create('Data is longer than allowed for embedded hint.');
    Move(AStr[1], Data[0], Length(AStr));
  end;
end;

procedure NewNonCastOperation(var Operation: TThoriumOperationDescription);
begin
  Operation.ResultType := nil;
  Operation.Casts[0].Needed := False;
  Operation.Casts[1].Needed := False;
end;

function movegOperation(const Offset: Integer; const TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(moveg(Offset, TRI));
  Result.OperationInstruction.TargetRIOffset := 2;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function copyr_gOperation(const SRI: TThoriumRegisterID; const Offset: Integer): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(copyr_g(SRI, Offset));
  Result.OperationInstruction.TargetRIOffset := 0;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function movefgOperation(const Offset: Integer; const TRI: TThoriumRegisterID; const AModuleRef: TThoriumModule): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(movefg(Offset, TRI, AModuleRef));
  Result.OperationInstruction.TargetRIOffset := 2;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function copyr_fgOperation(const SRI: TThoriumRegisterID; const Offset: Integer; const AModuleRef: TThoriumModule): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(copyr_fg(SRI, Offset, AModuleRef));
  Result.OperationInstruction.TargetRIOffset := 0;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function movelOperation(const Offset: Integer; const TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(movel(Offset, TRI));
  Result.OperationInstruction.TargetRIOffset := 2;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function copyr_lOperation(const SRI: TThoriumRegisterID; const Offset: Integer): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(copyr_l(SRI, Offset));
  Result.OperationInstruction.TargetRIOffset := 0;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function movepOperation(const Offset: Integer; const TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(movep(Offset, TRI));
  Result.OperationInstruction.TargetRIOffset := 2;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function copyr_pOperation(const SRI: TThoriumRegisterID; const Offset: Integer): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(copyr_p(SRI, Offset));
  Result.OperationInstruction.TargetRIOffset := 0;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function moverOperation(const SRI, TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(mover(SRI, TRI));
  Result.OperationInstruction.TargetRIOffset := 1;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function copyrOperation(const SRI, TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(copyr(SRI, TRI));
  Result.OperationInstruction.TargetRIOffset := 0;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function fncOperation(const FunctionPointer: TThoriumFunction; const TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(fnc(FunctionPointer, TRI));
  Result.OperationInstruction.TargetRIOffset := 8;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function xfncOperation(const FunctionPointer: TThoriumHostFunctionBase; const TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(xfnc(FunctionPointer, TRI));
  Result.OperationInstruction.TargetRIOffset := 8;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function xpgetOperation(const Prop: TThoriumLibraryProperty; const TRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(xpget(Prop, TRI));
  Result.OperationInstruction.TargetRIOffset := 8;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;

function xpsetOperation(const Prop: TThoriumLibraryProperty; const SRI: TThoriumRegisterID): TThoriumOperationDescription;
begin
  NewNonCastOperation(Result);
  Result.OperationInstruction.Instruction := TThoriumInstructionREG(xpset(Prop, SRI));
  Result.OperationInstruction.TargetRIOffset := 8;
  Result.OperationInstruction.Value1RIOffset := -1;
  Result.OperationInstruction.Value2RIOffset := -1;
end;
