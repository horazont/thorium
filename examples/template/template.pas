(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: template.pas
** Last update: 2009-07-08
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
program template;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Thorium, ThoriumLibStd, ThoriumLibStdIO;

const
  SCRIPT_NAME = '../../template';

var
  Engine: TThorium;
  Module: TThoriumModule;
begin
  // Create a thorium context
  Engine := TThorium.Create;
  try
    // Load the standard libraries
    Engine.LoadLibrary(TThoriumLibStd);
    Engine.LoadLibrary(TThoriumLibStdIO);
    // Load the script
    Module := Engine.LoadModuleFromFile(SCRIPT_NAME);
    Module.Dump;
    // No need for a try-finally section since the module will be freed by the
    // engine afterwards.
    // Initialize the virtual machine.
    Engine.InitializeVirtualMachine;
    // Execute the function main without any parameters
    Module.FindPublicFunction('MAIN').SafeCall([]);
  finally
    // This can be called even if the vm has not been initialized yet.
    Engine.ReleaseVirtualMachine;
    Engine.Free;
  end;
end.

