(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: records.pas
** Last update: 2010-03-12
This file is part of the Thorium Scripting Language Project.

As this file is part of an example, you may do with it what you want, except
sue me for anything I've coded here. When you are going to redistribute this
file in its original functionallity (that is, the same effective code, removing
comments or this notice does not count), you must name the source where you
got it.

For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
j.wielicki@sotecware.net
*******************************************************************************)
program records;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Thorium, ThoriumLibStd, ThoriumLibStdIO, recordslib;

const
  SCRIPT_NAME = '../../records';

var
  Engine: TThorium;
  Module: TThoriumModule;
  Rec: TStringRecord;
begin
  // Create a thorium context
  Engine := TThorium.Create;
  try
    // Load the standard libraries
    Engine.LoadLibrary(TThoriumLibStd);
    Engine.LoadLibrary(TThoriumLibStdIO);
    Engine.LoadLibrary(TThoriumLibExampleRecords);
    // Load the script
    Module := Engine.LoadModuleFromFile(SCRIPT_NAME);
    Module.Dump;
    // No need for a try-finally section since the module will be freed by the
    // engine afterwards.
    // Initialize the virtual machine.
    Engine.InitializeVirtualMachine;
    // Execute the function main without any parameters
    Module.FindPublicFunction('TESTSIMPLE').SafeCall([]);
    Module.FindPublicFunction('TESTEXTENDED').SafeCall([]);
    Rec.S := 'Hello World!';
    Module.FindPublicFunction('TESTASPARAMETER').SafeCall([Module.EncloseHostValue(@Rec, TypeInfo(TStringRecord))]);
  finally
    // This can be called even if the vm has not been initialized yet.
    Engine.ReleaseVirtualMachine;
    Engine.Free;
  end;
end.

