(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms
of the GNU General Public license (the  "GPL License"), in which case the
provisions of GPL License are applicable instead of those
above.


For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
j.wielicki@sotecware.net
*******************************************************************************)
program scripttest;

{$mode objfpc}{$H+}
{$C+}

{$DEFINE EvalTime}
{$ifdef EvalTime}
  {.$DEFINE PerfCount}
  {.$DEFINE Benchmark}
  {$ifdef Benchmark}
    {.$DEFINE Luatest}
  {$endif}
{$endif}
{$ifndef Benchmark}
  {$define UseTestModule}
{$endif}

{$define HeapTrc}

uses
  {$ifdef HeapTrc}Heaptrc, {$endif}Classes, SysUtils, thorium, thorium_utils, thorium_globals,
  ThoriumLibStd, ThoriumLibStdIO, ThoriumLibString, ThoriumLibStreams;

{$ifdef UseTestModule}
const
  MODULE_TEST_NAME = 'scripts/testscript.expressions.txt';
{$endif}
{$ifdef Benchmark}
const
  MODULE_BENCHMARK_NAME = 'scripts/testscript.benchmark.txt';
{$endif}

type

  { TTestClass }

  TTestClass = class (TThoriumPersistent)
  public
    destructor Destroy; override;
  private
    FTestFlt: Double;
    FTestInt: Integer;
    FTestStr: String;
  protected
    class procedure GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
       override;
    class procedure GetStaticMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIStaticMethods);
       override;
    procedure Thorium_TestMethod(const Input: array of Pointer; Result: PThoriumValue);
    class procedure Thorium_TestStaticMethod(const Input: array of Pointer; Result: PThoriumValue);
  published
    property TestFlt: Double read FTestFlt write FTestFlt;
    property TestInt: Integer read FTestInt;
    property TestStr: String read FTestStr write FTestStr;
  end;

  { TTestClass2 }

  TTestClass2 = class (TThoriumPersistent)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FTestObj: TTestClass;
  protected
    class procedure GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
       override;
    procedure Thorium_TestMethod(const Input: array of Pointer; Result: PThoriumValue);
  published
    property TestObj: TTestClass read FTestObj;
  end;

type
  TSimpleRecord = packed record
    A: Byte;
    B: Word;
    C: Cardinal;
    D: QWord;
  end;
  PSimpleRecord = ^TSimpleRecord;

  TStringRecord = packed record
    A: Word;
    S: String;
  end;
  PStringRecord = ^TStringRecord;

  { TTestLibrary }

  TSimpleRecordRegistration = specialize TThoriumHostRecordType<TSimpleRecord>;
  TStringRecordRegistration = specialize TThoriumHostRecordType<TStringRecord>;

  TTestLibrary = class (TThoriumLibrary)
  protected
    class function GetName: String; override;
    procedure InitializeLibrary; override;
  end;

procedure RecordTest(ARecord: PSimpleRecord);
begin
  WriteLn(IntToHex(ptrint(ARecord), SizeOf(ptruint)*2));
  WriteLn(ARecord^.A);
  WriteLn(ARecord^.B);
  WriteLn(ARecord^.C);
  WriteLn(ARecord^.D);
end;

procedure RecordTest2(ARecord: PStringRecord);
begin
  WriteLn(IntToHex(ptrint(ARecord), SizeOf(ptruint)*2));
  WriteLn(ARecord^.A);
  WriteLn(ARecord^.S);
end;
{ TTestLibrary }

class function TTestLibrary.GetName: String;
begin
  Result := 'scripttest';
end;

procedure TTestLibrary.InitializeLibrary;
var
  RecordType, RecordType2: TThoriumHostObjectType;
begin
  RecordType := RegisterFinishedObjectType('TSimpleRecord', TSimpleRecordRegistration.Create(Self, [
      HostRecordField(HostVarType(htByte), 'a', 0),
      HostRecordField(HostVarType(htWord), 'b', 1),
      HostRecordField(HostVarType(htDWord), 'c', 3),
      HostRecordField(HostVarType(htQWord), 'd', 7)
    ]),
    TypeInfo(TSimpleRecord)
  );
  RecordType2 := RegisterFinishedObjectType('TStringRecord', TStringRecordRegistration.Create(Self, [
      HostRecordField(HostVarType(htByte), 'a', 0),
      HostRecordField(HostVarType(htString), 's', 2)
    ]),
    TypeInfo(TStringRecord)
  );
  RegisterNativeCallFunction('RecordTest', @RecordTest, [
      htExt
    ], htNone, ncRegister).Parameters.ExtendedTypes[0] := RecordType;
  RegisterNativeCallFunction('RecordTest2', @RecordTest2, [
      htExt
    ], htNone, ncRegister).Parameters.ExtendedTypes[0] := RecordType2;
  PrecompileFunctions;
end;

{ TTestClass }

destructor TTestClass.Destroy;
begin
  inherited Destroy;
end;

class procedure TTestClass.GetStaticMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIStaticMethods);
var
  Offset: Integer;
begin
  inherited GetStaticMethodList(Sender, Methods);
  Offset := Length(Methods);
  SetLength(Methods, Offset+1);
  Methods[Offset] := TThoriumHostFunctionSimpleMethod.Create;
  with TThoriumHostFunctionSimpleMethod(Methods[Offset]) do
  begin
    Name := 'TESTSTATICMETHOD';
    Method := @Thorium_TestStaticMethod;
  end;
end;

class procedure TTestClass.GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
var
  Offset: Integer;
begin
  inherited GetMethodList(Sender, Methods);
  Offset := Length(Methods);
  SetLength(Methods, Offset+1);
  Methods[Offset] := TThoriumHostMethodSimple.Create;
  with TThoriumHostMethodSimple(Methods[Offset]) do
  begin
    Name := 'TESTMETHOD';
    ClassMethod := @Thorium_TestMethod;
  end;
end;

procedure TTestClass.Thorium_TestMethod(const Input: array of Pointer;
  Result: PThoriumValue);
begin
end;

class procedure TTestClass.Thorium_TestStaticMethod(const Input: array of Pointer;
  Result: PThoriumValue);
begin
  WriteLn('Static method called');
end;

constructor TTestClass2.Create;
begin
  inherited Create;
  FTestObj := TTestClass.Create;
end;

destructor TTestClass2.Destroy;
begin
  FTestObj.ThoriumReference.FreeReference;
  inherited Destroy;
end;

class procedure TTestClass2.GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
var
  Offset: Integer;
begin
  inherited GetMethodList(Sender, Methods);
  Offset := Length(Methods);
  SetLength(Methods, Offset+1);
  Methods[Offset] := TThoriumHostMethodSimple.Create;
  with TThoriumHostMethodSimple(Methods[Offset]) do
  begin
    Name := 'TESTMETHOD';
    ClassMethod := @Thorium_TestMethod;
  end;
end;

procedure TTestClass2.Thorium_TestMethod(const Input: array of Pointer;
  Result: PThoriumValue);
begin
end;

function DirectCallTest(A: String; B: array of const): String; stdcall;
begin
  WriteLn(A);
  WriteLn(High(B));
  WriteLn(TVarRec(B[1]).VInt64^);
  Result := Format(A, B);
  WriteLn(Result);
end;

(*procedure RegisterSimpleMethod(AEngine: TThorium; AName: String; ACode: TThoriumSimpleMethod; AParameters: array of TVarType; AReturnValue: TVarType = varempty);
var
  I: Integer;
begin
  with TThoriumExternalFunctionSimpleMethod(AEngine.ExternalFunctionRegistry.RegisterFunction(TThoriumExternalFunctionSimpleMethod, AName)) do
  begin
    Method := ACode;
    for I := 0 to Length(AParameters) - 1 do
      Parameters.AddType(AParameters[I]);
    ReturnType := AReturnValue;
  end;
end;

procedure Testcall(Userdata: Pointer; const Input: array of Variant; out Result: Variant);
var
  Mgr: tvariantmanager;
  S: String;
var
  VA: array of Variant;
  I: Integer;
begin
  WriteLn('Teststuff');
  GetVariantManager(Mgr);
  VA := Input[0];
  for I := 0 to Length(VA) - 1 do
  begin
    Mgr.vartolstr(S, VA[I]);
    WriteLn(S);
  end;
  //GetVariantManager(Mgr);
  //S := Mgr.vartolstr(S, Input[0]);
end;     *)

procedure CompilerOutput(Userdata: Pointer; Sender: TThorium;
  const Module: TThoriumModule; const Msg: String);
begin
  WriteLn(Module.Name, ': [Error] ', Msg);
end;
    

var
  Module: TThoriumModule;
  Engine: TThorium;
  Test, Bin: TFileStream;
  {$ifdef EvalTime}
  OldClass: DWORD;
  {$ifdef PerfCount}
  Freq: Int64;
  Time1, Time2: Int64;
  {$else}
  Time: Cardinal;
  {$endif}
  CTime, HCTime, SCTime, LuaTime: Double;
  {$endif}
  I, J: Integer;
  F: Double = 0.0;
  Fac: Double = 10;
  Reg1, Reg2: Pointer;
  TestValue: TThoriumValue;
  TestClassRTTI: TThoriumRTTIObjectType;
  {$ifdef Benchmark}
  BenchmarkTest: TThoriumFunction;
  BenchmarkTestCount: Integer;
  BenchmarkMax, BenchmarkMin, BenchmarkCurr: Double;
  BenchmarkMaxNumber, BenchmarkMinNumber: Cardinal;
  StackSize: Cardinal;
  D: Integer;
  {$endif}
  RelocationDevil: Pointer;

{$ifdef Luatest}
procedure LuaPerftest; inline;
var
  state: lua_State;
  Result: int;
begin
  state := luaL_newstate;
  luaL_openlibs(state);
  if (luaL_loadfile(state, 'testscript.benchmark.lua') <> LUA_STATE_OK) then
  begin
    raise Exception.Create('Lua compiler error');
  end;
  for J := 0 to BenchmarkTestCount - 1 do
  begin
    {$ifdef PerfCount}
    QueryPerformanceFrequency(Freq);
    QueryPerformanceCounter(Time1);
    {$else}
    Time := GetTickCount;
    {$endif}

    lua_pushvalue(state, -1);
    Result := lua_pcall(state, 0, 0, 0);
    if (Result <> LUA_STATE_OK) then
    begin
      raise Exception.CreateFmt('Lua returned an error (%d: %s).', [Result, lua_tostring(state, -1)]);
    end;

    {$ifdef PerfCount}
    QueryPerformanceCounter(Time2);
    BenchmarkCurr := ((Time2 - Time1) / Freq) * 1000;
    {$else}
    BenchmarkCurr := GetTickCount - Time;
    {$endif}
    if (BenchmarkMinNumber = 0) or (BenchmarkMin > BenchmarkCurr) then
    begin
      BenchmarkMin := BenchmarkCurr;
      BenchmarkMinNumber := J + 1;
    end;
    if (BenchmarkMaxNumber = 0) or (BenchmarkMax < BenchmarkCurr) then
    begin
      BenchmarkMax := BenchmarkCurr;
      BenchmarkMaxNumber := J + 1;
    end;
    HCTime := HCTime + BenchmarkCurr;
    Write(' ', J+1);
  end;
  lua_pop(state, 1);
  lua_close(state);
end;
{$endif}

procedure WriteHash(const AHash: TThoriumHash);
type
  THash = array [0..15] of Byte;
var
  I: Integer;
begin
  for I := 0 to 15 do
    Write(IntToHex(THash(AHash)[I], 2));
end;

procedure DoOpenModule(Userdata: Pointer; Sender: TThorium; const ModuleName: String;
  var Stream: TStream);
begin
  if FileExists('/home/horazont/Projects/fpc/thorium/'+ModuleName) then
    Stream := TFileStream.Create('/home/horazont/Projects/fpc/thorium/'+ModuleName, fmOpenRead)
  else
    Stream := nil;
end;

{$if defined(EvalTime) and not defined(WINDOWS)}
function GetTickCount: Cardinal;
begin
  Result := DateTimeToTimeStamp(Now).Time;
end;
{$endif}

begin
  {$ifdef HeapTrc}
    DeleteFile(ExtractFilePath(ParamStr(0))+'heaptrc.txt');
    SetHeapTraceOutput(ExtractFilePath(ParamStr(0))+'heaptrc.txt');
    WriteLn('HeapTrc ENABLED');
  {$else}
    WriteLn('HeapTrc DISABLED');
  {$endif}
  {$ifdef EvalTime}
  {$ifdef PerfCount}
  //SetThreadAffinityMask(GetCurrentThread, 1);
  {$ifdef Windows}
  SetProcessAffinityMask(GetCurrentProcess, 1);
  {$endif}
  for I := 0 to 1000 do
    F := F+I;
  {$endif}
  {$endif}
  Randomize;
  GetMem(RelocationDevil, Random(10)+10);
  try
    try
      Engine := TThorium.Create;
      try
        WriteLn('Engine information: ');
        WriteLn(       ' Numeric field             # Value  (Unit) # Additional info');
        WriteLn(       '===========================#===============#================================');
        WriteLn(Format('  Instruction size         # %6d Bytes  #', [SizeOf(TThoriumInstruction)]));
        WriteLn(Format('  Value base size          # %6d Bytes  #', [SizeOf(TThoriumValue)]));
        WriteLn(Format('  Stack entry size         # %6d Bytes  #', [SizeOf(TThoriumStackEntry)]));
        WriteLn(Format('  Type base size           # %6d Bytes  #', [SizeOf(TThoriumType)]));
        WriteLn(Format('  Register count           # %6d        #', [THORIUM_REGISTER_COUNT]));
        WriteLn(Format('    Cache registers        # %6d        #', [THORIUM_REGISTER_C_COUNT]));
        WriteLn(Format('    Expression registers   # %6d        #', [THORIUM_REGISTER_EXP_COUNT]));
        Write  (Format('  Complete register size   # %6d Bytes  #', [SizeOf(TThoriumValue) * (THORIUM_REGISTER_EXP_MAX+1)]));
        Engine.LoadLibrary(TTestLibrary);
      {$ifdef EvalTime}
        GetMem(Reg1, SizeOf(TThoriumValue) * (THORIUM_REGISTER_EXP_MAX+1));
        {$ifdef PerfCount}
        QueryPerformanceFrequency(Freq);
        QueryPerformanceCounter(Time1);
        {$else}
        Time := GetTickCount;
        {$endif}
        for I := 0 to 999 do
        begin
          GetMem(Reg2, SizeOf(TThoriumValue) * (THORIUM_REGISTER_EXP_MAX+1));
          Move(Reg1^, Reg2^, SizeOf(TThoriumValue) * (THORIUM_REGISTER_EXP_MAX+1));
          FreeMem(Reg2);
        end;
        {$ifdef PerfCount}
        QueryPerformanceCounter(Time2);
        CTime := ((Time2 - Time1) / Freq);
        WriteLn(Format(' %.4f ms to duplicate & free', [CTime]));
        {$else}
        CTime := (GetTickCount - Time) / 1000;
        WriteLn(Format(' %.4f ms to duplicate & free', [CTime]));
        {$endif}
        FreeMem(Reg1);
        {$else}
        WriteLn;
        {$endif}
        WriteLn(Format('  Register mask size       # %6d Bytes  # %d bit(s) unused', [THORIUM_REGISTER_MASK_SIZE, (8-(THORIUM_REGISTER_COUNT mod 8)) * Integer(THORIUM_REGISTER_COUNT mod 8 > 0)]));
        WriteLn('END Engine information');
        WriteLn;
        //Engine.OnRequireModule := TThoriumOnRequireModule(ThoriumMakeOOPEvent(@RequireModule, nil));
        Engine.OnCompilerOutput := TThoriumOnCompilerOutput(ThoriumMakeOOPEvent(@CompilerOutput, nil));
        Engine.OnOpenModule := TThoriumOnOpenModule(ThoriumMakeOOPEvent(@DoOpenModule, nil));
        // ===
        //  Register functions and classes here
        // ===
        //Engine.ExtendedTypeRegistry.RegisterRTTIType(TTestClass);
        Engine.LoadLibrary(TThoriumLibStd);
        Engine.LoadLibrary(TThoriumLibStdIO);
        Engine.LoadLibrary(TThoriumLibString);
        Engine.LoadLibrary(TThoriumLibStreams);

        {$ifdef Benchmark}
        Module := Engine.NewModule(ChangeFileExt(ExtractFileName(MODULE_BENCHMARK_NAME), ''));
        Test := TFileStream.Create(MODULE_BENCHMARK_NAME, fmOpenRead);
        try
        {$else}
        {$ifdef UseTestModule}
        Module := Engine.NewModule(ChangeFileExt(ExtractFileName(MODULE_TEST_NAME), ''));
        Test := TFileStream.Create(MODULE_TEST_NAME, fmOpenRead);
        {$else}
        Module := Engine.NewModule(ChangeFileExt(ExtractFileName(ParamStr(1)), ''));
        Test := TFileStream.Create(ParamStr(1), fmOpenRead);
        {$endif}
        try
        {$endif}
          WriteLn('Compiling... ');
          {$ifdef EvalTime}
          {$ifdef PerfCount}
          QueryPerformanceFrequency(Freq);
          QueryPerformanceCounter(Time1);
          {$else}
          Time := GetTickCount;
          {$endif}
          {$endif}
          if not Module.CompileFromStream(Test, []) then
          begin
            raise Exception.Create('Compilation failed: "'+Module.LastCompilerError+'"');
          end;
          {$ifdef EvalTime}
          {$ifdef PerfCount}
          QueryPerformanceCounter(Time2);
          CTime := ((Time2 - Time1) / Freq) * 1000;
          WriteLn('Done.');
          WriteLn('Needed ', Format('%.8f', [CTime]), 'ms to compile.');
          {$else}
          CTime := GetTickCount - Time;
          WriteLn('Done.');
          WriteLn('Needed ', Trunc(CTime), 'ms to compile.');
          {$endif}
          if CTime = 0 then
            WriteLn('This evaluates to DIV0 bytes/ms for compilation.')
          else
            WriteLn('This evaluates to ', Format('%.4f bytes/ms', [Test.Size / CTime]), ' for compilation.');
          WriteLn;
          {$endif}
        finally
          Test.Free;
        end;
        for I := 0 to Engine.ModuleCount - 1 do
        begin
          Engine.Module[I].Dump;
          Bin := TFileStream.Create(Engine.Module[I].Name+'.tsb', fmCreate);
          try
            Engine.Module[I].SaveToStream(Bin);
          finally
            Bin.Free;
          end;
          WriteLn;
        end;
        WriteLn('----------------');
        WriteLn('--- Executing generated code');
        Engine.InitializeVirtualMachine;
        try
          (*{$ifdef EvalTime}
          OldClass := GetPriorityClass(GetCurrentProcess);
          SetPriorityClass(GetCurrentProcess, THREAD_PRIORITY_TIME_CRITICAL);
          {$ifdef PerfCount}
          QueryPerformanceFrequency(Freq);
          QueryPerformanceCounter(Time1);
          {$else}
          Time := GetTickCount;
          {$endif}
          for I := 0 to 100000 do
          begin
            F := I / Fac;
          end;
          {$ifdef PerfCount}
          QueryPerformanceCounter(Time2);
          HCTime := ((Time2 - Time1) / Freq) * 1000;
          WriteLn('Hardcoded loop needs ', Format('%13.8f', [HCTime]), 'ms.');
          {$else}
          HCTime := GetTickCount - Time;
          WriteLn('Hardcoded loop needs ', Trunc(HCTime), 'ms.');
          {$endif}
          {$endif}        *)

          WriteLn;
          {$ifdef Benchmark}
          {$ifdef Windows}
          OldClass := GetPriorityClass(GetCurrentProcess);
          SetPriorityClass(GetCurrentProcess, THREAD_PRIORITY_TIME_CRITICAL);
          {$endif}
          WriteLn('BENCHMARK MODE');
          Write('How many times shall a test be executed? ');
          //Engine.VirtualMachine.GetStack.Capacity := 1000000;
          ReadLn(BenchmarkTestCount);
          for I := 0 to Module.PublicFunctions.FunctionCount-1 do
          begin
            BenchmarkTest := Module.PublicFunctions.Functions[I];
            if BenchmarkTest.Parameters.Count > 0 then
              Continue;
            if BenchmarkTest.Name = 'COMPARE_TEST' then
              Continue;
            Write('Test ''', BenchmarkTest.Name, ''':');

            SCTime := 0;
            BenchmarkMinNumber := 0;
            BenchmarkMaxNumber := 0;
            for J := 0 to BenchmarkTestCount - 1 do
            begin
              {$ifdef PerfCount}
              QueryPerformanceFrequency(Freq);
              QueryPerformanceCounter(Time1);
              {$else}
              Time := GetTickCount;
              {$endif}
              BenchmarkTest.Call([]);
              {$ifdef PerfCount}
              QueryPerformanceCounter(Time2);
              BenchmarkCurr := ((Time2 - Time1) / (Freq / 1000));
              {$else}
              BenchmarkCurr := (GetTickCount - Time);
              {$endif}
              if (BenchmarkMinNumber = 0) or (BenchmarkMin > BenchmarkCurr) then
              begin
                BenchmarkMin := BenchmarkCurr;
                BenchmarkMinNumber := J + 1;
              end;
              if (BenchmarkMaxNumber = 0) or (BenchmarkMax < BenchmarkCurr) then
              begin
                BenchmarkMax := BenchmarkCurr;
                BenchmarkMaxNumber := J + 1;
              end;
              SCTime := SCTime + BenchmarkCurr;
              Write(' ', J+1);
            end;
            WriteLn;
            WriteLn(Format('  Time min. %14.8fms (at run %d).', [BenchmarkMin, BenchmarkMinNumber]));
            WriteLn(Format('  Time avg. %14.8f', [SCTime / BenchmarkTestCount]), 'ms.');
            WriteLn(Format('  Time max. %14.8fms (at run %d).', [BenchmarkMax, BenchmarkMaxNumber]));
            WriteLn('  Stack cap. ', Engine.VirtualMachine.GetStack.Capacity, ' (', Format('%.2f', [Engine.VirtualMachine.GetStack.Capacity * SizeOf(TThoriumStackEntry) / 1048576]), ' MiB)');
            WriteLn;
          end;
          BenchmarkTest := Module.PublicFunctions.FindFunction('COMPARE_TEST');
          if BenchmarkTest <> nil then
          begin
            Write('Test ''COMPARE_TEST'' [Host Environment Comparsion]:');
            SCTime := 0;
            BenchmarkMinNumber := 0;
            BenchmarkMaxNumber := 0;
            for J := 0 to BenchmarkTestCount - 1 do
            begin
              {$ifdef PerfCount}
              QueryPerformanceFrequency(Freq);
              QueryPerformanceCounter(Time1);
              {$else}
              Time := GetTickCount;
              {$endif}
              BenchmarkTest.Call([]);
              {$ifdef PerfCount}
              QueryPerformanceCounter(Time2);
              BenchmarkCurr := ((Time2 - Time1) / (Freq / 1000));
              {$else}
              BenchmarkCurr := (GetTickCount - Time);
              {$endif}
              if (BenchmarkMinNumber = 0) or (BenchmarkMin > BenchmarkCurr) then
              begin
                BenchmarkMin := BenchmarkCurr;
                BenchmarkMinNumber := J + 1;
              end;
              if (BenchmarkMaxNumber = 0) or (BenchmarkMax < BenchmarkCurr) then
              begin
                BenchmarkMax := BenchmarkCurr;
                BenchmarkMaxNumber := J + 1;
              end;
              SCTime := SCTime + BenchmarkCurr;
              Write(' ', J+1);
            end;
            WriteLn;
            WriteLn(Format('  Time min. %14.8fms (at run %d).', [BenchmarkMin, BenchmarkMinNumber]));
            WriteLn(Format('  Time avg. %14.8f', [SCTime / BenchmarkTestCount]), 'ms.');
            WriteLn(Format('  Time max. %14.8fms (at run %d).', [BenchmarkMax, BenchmarkMaxNumber]));
            Write(' Host environment: ');
            HCTime := 0;
            BenchmarkMinNumber := 0;
            BenchmarkMaxNumber := 0;
            for J := 0 to BenchmarkTestCount - 1 do
            begin
              {$ifdef PerfCount}
              QueryPerformanceFrequency(Freq);
              QueryPerformanceCounter(Time1);
              {$else}
              Time := GetTickCount;
              {$endif}
              for D := 0 to 999999 do
              begin
                f := d / fac;
              end;
              {$ifdef PerfCount}
              QueryPerformanceCounter(Time2);
              BenchmarkCurr := ((Time2 - Time1) / (Freq / 1000));
              {$else}
              BenchmarkCurr := (GetTickCount - Time);
              {$endif}
              if (BenchmarkMinNumber = 0) or (BenchmarkMin > BenchmarkCurr) then
              begin
                BenchmarkMin := BenchmarkCurr;
                BenchmarkMinNumber := J + 1;
              end;
              if (BenchmarkMaxNumber = 0) or (BenchmarkMax < BenchmarkCurr) then
              begin
                BenchmarkMax := BenchmarkCurr;
                BenchmarkMaxNumber := J + 1;
              end;
              HCTime := HCTime + BenchmarkCurr;
              Write(' ', J+1);
            end;
            WriteLn;
            WriteLn(Format('  Time min. %14.8fms (at run %d).', [BenchmarkMin, BenchmarkMinNumber]));
            WriteLn(Format('  Time avg. %14.8f', [HCTime / BenchmarkTestCount]), 'ms.');
            WriteLn(Format('  Time max. %14.8fms (at run %d).', [BenchmarkMax, BenchmarkMaxNumber]));
            WriteLn(Format('  Factor    %14.8f', [SCTime / HCTime]));
            {$ifdef Luatest}
            HCTime := 0;
            BenchmarkMinNumber := 0;
            BenchmarkMaxNumber := 0;
            Write(' Lua script: ');
            LuaPerftest;
            WriteLn;
            WriteLn(Format('  Time min. %14.8fms (at run %d).', [BenchmarkMin, BenchmarkMinNumber]));
            WriteLn(Format('  Time avg. %14.8f', [HCTime / BenchmarkTestCount]), 'ms.');
            WriteLn(Format('  Time max. %14.8fms (at run %d).', [BenchmarkMax, BenchmarkMaxNumber]));
            WriteLn(Format('  Factor    %14.8f', [SCTime / HCTime]));
            {$endif}
            StackSize := Engine.VirtualMachine.GetStack.EntryCount;
            if StackSize = 0 then
              Engine.VirtualMachine.GetStack.Capacity := 1
            else
              Engine.VirtualMachine.GetStack.Capacity := StackSize;

          end;
          WriteLn('DONE Benchmark.');
          {$else}
          WriteLn('Scripting output:');
          {$ifdef EvalTime}
          {$ifdef PerfCount}
          QueryPerformanceFrequency(Freq);
          QueryPerformanceCounter(Time1);
          {$else}
          Time := GetTickCount;
          {$endif}
          {$endif}
          {TestValue := ThoriumCreateExtendedTypeValue(TestClassRTTI);
          TestValue.Extended.Value := TTestClass.Create.GetReference;
          TTestClass(TestValue.Extended.Value).FreeReference;}
          Module.FindPublicFunction('MAIN').SafeCall([]);
          {$ifdef EvalTime}
          {$ifdef PerfCount}
          QueryPerformanceCounter(Time2);
          SCTime := ((Time2 - Time1) / Freq) * 1000;
          WriteLn('Script needed ', Format('%14.8f', [SCTime]), 'ms to execute.');
          //WriteLn('Scripted loop needs ', Format('%14.8f', [SCTime]), 'ms.');
          {$else}
          SCTime := GetTickCount - Time;
          WriteLn('Script needed  ', Trunc(SCTime), 'ms to execute.');
          //WriteLn('Scripted loop needs  ', Trunc(SCTime), 'ms.');
          {$endif}
          //Engine.VirtualMachine.Execute(0, Module.PublicFunctions.Functions[0].EntryPoint, True);
          WriteLn('END Scripting output');
          {$endif}
          WriteLn;
          //WriteLn('The script needs ', Format('%.2f', [SCTime / HCTime]), ' times more time than');
          //WriteLn('the hardcoded version.');
          //SetPriorityClass(GetCurrentProcess, OldClass);
          {$endif}
        finally
          {$ifdef Benchmark}
          {$ifdef Windows}
          SetPriorityClass(GetCurrentProcess, OldClass);
          {$endif}
          {$endif}
          Engine.ReleaseVirtualMachine;
        end;
        WriteLn('--- DONE execution');
      finally
        Engine.Free;
        ReadLn;
      end;
    except
      on E: Exception do
      begin
        WriteLn('EXCEPTION');
        WriteLn(E.ClassName);
        WriteLn(E.Message);
        ReadLn;
        raise;
      end;
    end;
  finally
    FreeMem(RelocationDevil);
  end;
end.

