(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: benchmark.pas
** Last update: 2009-07-08
This file is part of the Thorium Scripting Language Project.

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
program benchmark;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Thorium, DateUtils{$if defined(linux) or defined(unix)}, baseunix{$endif}{$ifdef WINDOWS}, Windows{$endif};


{$if defined(linux) or defined(unix)}
type
  {$ifdef CPU64}
  clongint = int64;
  {$else}
  clongint = longint;
  {$endif}
  cint = longint;
  timeeval = packed record
    sec: clongint;
    usec: clongint;
  end;

  rusage = packed record
    ru_utime: timeeval;
    ru_stime: timeeval;
    ru_maxrss: clongint;
    ru_ixrss: clongint;
    ru_idrss: clongint;
    ru_isrss: clongint;
    ru_minflt: clongint;
    ru_majflt: clongint;
    ru_nswap: clongint;
    ru_inblock: clongint;
    ru_oublock: clongint;
    ru_msgsnd: clongint;
    ru_msgrcv: clongint;
    ru_nsignals: clongint;
    ru_nvcsw: clongint;
    ru_nivcsw: clongint;
  end;
  prusage = ^rusage;
var
  usage: rusage;

function fpGetRUsage( who : cint; data : pRUsage ) : cint; cdecl; external 'c' name 'getrusage';
{$endif}

function GetTime: Double; inline;
{$if defined(linux) or defined(unix)}
begin
  fpGetRUsage(0, @usage);
  Result := usage.ru_utime.sec + usage.ru_utime.usec / 1000000;
end;
{$elseif defined(windows)}
begin
  Result := GetTickCount;
end;
{$else}
begin
  Result := 0;
end;
{$endif}

function GetSecondaryTime: Double; inline;
{$if defined(linux) or defined(unix)}
begin
  Result := DateTimeToUnix(Now);
end;
{$elseif defined(windows)}
begin
  Result := 0;
end;
{$else}
begin
  Result := 0;
end;
{$endif}

const
  SCRIPT_NAME = '../../benchmark';

var
  Engine: TThorium;
  Module: TThoriumModule;
  Func: TThoriumFunction;
  I, Test: Integer;
  Time1, Time2: QWord;
  TestCount: Integer;
  CurrTime: Double;
  Min, Max: Double;
  Avg, Full, Avg2: Double;
  MinIdx, MaxIdx: Integer;

  F: Double;
  Fac: Double;
begin
  // Create a thorium context
  Engine := TThorium.Create;
  try
    // Load the script
    Write('Loading module...');
    try
      Module := Engine.LoadModuleFromFile(SCRIPT_NAME);
    except
      on E: Exception do
      begin
        WriteLn(' Failed. ', E.Message);
        Exit;
      end
      else
      begin
        WriteLn(' Failed');
        Exit;
      end;
    end;
    WriteLn(' Success.');
    // No need for a try-finally section since the module will be freed by the
    // engine afterwards.
    // Initialize the virtual machine.
    Engine.InitializeVirtualMachine;
    WriteLn('BENCHMARK MODE');
    {$if defined(linux) or defined(unix)}
    WriteLn('  UNIX MODE. Quite accurate, can use user cpu time.');
    {$elseif defined(windows)}
    WriteLn('  WINDOWS MODE. Set the priority of the process as high as possible to get');
    WriteLn('                the best results');
    {$else}
    WriteLn('  COULD NOT DETECT OS. CANCELLING NOW.');
    Exit;
    {$endif}
    Write  ('  How many times shall a test be executed? ');
    ReadLn(TestCount);
    if TestCount <= 0 then
      Exit;
    for I := 0 to Module.PublicFunctionCount - 1 do
    begin
      Func := Module.PublicFunction[I];
      if Func.Name = 'COMPARE_TEST' then
        Continue;
      Write('Current test "', Func.Name, '" :');
      MinIdx := 0;
      MaxIdx := 0;
      Min := 0.0;
      Max := 0.0;
      Full := 0;
      for Test := 0 to TestCount-1 do
      begin
        CurrTime := GetTime;
        Func.Call([]);
        CurrTime := GetTime - CurrTime;
        if (MinIdx = 0) or (CurrTime < Min) then
        begin
          MinIdx := Test + 1;
          Min := CurrTime;
        end;
        if (MaxIdx = 0) or (CurrTime > Max) then
        begin
          MaxIdx := Test + 1;
          Max := CurrTime;
        end;
        Full += CurrTime;
        Write(' ', Test+1);
      end;
      WriteLn;
      Avg := Full / TestCount;
      WriteLn(Format('  Min: %4.4fs (#%2d)', [Min, MinIdx]));
      WriteLn(Format('  Max: %4.4fs (#%2d)', [Max, MaxIdx]));
      WriteLn(Format('  Avg: %4.4fs', [Avg]));
      WriteLn;
    end;
    Func := Module.FindPublicFunction('COMPARE_TEST');
    if Func <> nil then
    begin
      Fac := 10.0;
      WriteLn('Host environment comparison test: ');
      Write('  Thorium: ');
      MinIdx := 0;
      MaxIdx := 0;
      Min := 0.0;
      Max := 0.0;
      Full := 0;
      for Test := 0 to TestCount-1 do
      begin
        CurrTime := GetTime;
        Func.Call([]);
        CurrTime := GetTime - CurrTime;
        if (MinIdx = 0) or (CurrTime < Min) then
        begin
          MinIdx := Test + 1;
          Min := CurrTime;
        end;
        if (MaxIdx = 0) or (CurrTime > Max) then
        begin
          MaxIdx := Test + 1;
          Max := CurrTime;
        end;
        Full += CurrTime;
        Write(' ', Test+1);
      end;
      WriteLn;
      Avg := Full / TestCount;
      WriteLn(Format('    Min: %4.4fs (#%2d)', [Min, MinIdx]));
      WriteLn(Format('    Max: %4.4fs (#%2d)', [Max, MaxIdx]));
      WriteLn(Format('    Avg: %4.4fs', [Avg]));
      Write('  Host environment: ');
      Avg2 := Avg;
      MinIdx := 0;
      MaxIdx := 0;
      Min := 0.0;
      Max := 0.0;
      Full := 0;
      for Test := 0 to TestCount-1 do
      begin
        CurrTime := GetTime;
        for I := 0 to 999999 do
        begin
          F := I / Fac;
        end;
        CurrTime := GetTime - CurrTime;
        if (MinIdx = 0) or (CurrTime < Min) then
        begin
          MinIdx := Test + 1;
          Min := CurrTime;
        end;
        if (MaxIdx = 0) or (CurrTime > Max) then
        begin
          MaxIdx := Test + 1;
          Max := CurrTime;
        end;
        Full += CurrTime;
        Write(' ', Test+1);
      end;
      WriteLn;
      Avg := Full / TestCount;
      WriteLn(Format('    Min: %4.4fs (#%2d)', [Min, MinIdx]));
      WriteLn(Format('    Max: %4.4fs (#%2d)', [Max, MaxIdx]));
      WriteLn(Format('    Avg: %4.4fs', [Avg]));
      WriteLn('  Performance factor');
      WriteLn(Format('    Thorium needs %4.4f times more time than host code.', [Avg2/Avg]));
    end;
  finally
    // This can be called even if the vm has not been initialized yet.
    Engine.ReleaseVirtualMachine;
    Engine.Free;
  end;
end.
