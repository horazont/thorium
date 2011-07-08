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

{.$define HeapTrc}

uses
  {$ifdef HeapTrc}Heaptrc, {$endif}Classes, SysUtils, thorium, ThUtils,
  ThGlobals, (*ThoriumLibStd, ThoriumLibStdIO, ThoriumLibString,
  ThoriumLibStreams, *)ThCompiler, variants, ThoriumTestNativeCall;

{$ifdef UseTestModule}
const
  MODULE_TEST_NAME = 'scripts/testscript.new.txt';
{$endif}

var
  Engine: TThorium;
  Module: TThoriumModule;
  FS: TFileStream;
begin
  {$ifdef HeapTrc}
    DeleteFile(ExtractFilePath(ParamStr(0))+'heaptrc.txt');
    SetHeapTraceOutput(ExtractFilePath(ParamStr(0))+'heaptrc.txt');
    WriteLn('HeapTrc ENABLED');
  {$else}
    WriteLn('HeapTrc DISABLED');
  {$endif}

  Engine := TThorium.Create;
  try
    //Engine.LoadLibrary(TThoriumLibStd);
    (*Engine.LoadLibrary(TThoriumLibStdIO);
    Engine.LoadLibrary(TThoriumLibString);
    Engine.LoadLibrary(TThoriumLibStreams);*)
    Engine.LoadLibrary(TTestNativeCall);
    Module := Engine.NewModule('__main__');

    FS := TFileStream.Create(MODULE_TEST_NAME, fmOpenread);
    try
      try
        if Module.CompileFromStream(FS, TThoriumDefaultCompiler, []) then
          WriteLn('Successfully compiled:')
        else
          WriteLn('Compilation failed (you should not see this as an exception should''ve been raised).');
        Module.Dump(True);
      except
        (*on E: EThoriumCompilerError do
        begin
          WriteLn('[Error] ', E.Message);
          Exit;
        end
        else*)
          raise;
      end;
    finally
      FS.Free;
    end;

    if ParamStr(1) = '-c' then
      Exit;
    Engine.InitializeVirtualMachine;
    try
      try
        Engine.VirtualMachine.CallFunction(Engine.VirtualMachine.GetRuntimeFunction(Module.PublicFunction[0]), []);
      except
        on E: EThoriumRuntimeExecutionDebugException do
        begin
          WriteLn('== Debug exception == ', E.Message);
          raise;
        end;
        on E: EThoriumRuntimeExecutionException do
        begin
          WriteLn('== Execution exception == ', E.Message);
          raise;
        end
        else
          raise;
      end;
    finally
      Engine.ReleaseVirtualMachine;
    end;
  finally
    Engine.Free;
  end;
end.

