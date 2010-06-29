(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: compiler.pas
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
program compiler;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Thorium, ThoriumLibStd, ThoriumLibStdIO, ThoriumLibStreams,
  ThoriumLibString;

var
  Path1, Path2, Path3: String;

procedure OnOpenModule(Userdata: Pointer; Sender: TThorium; const ModuleName: String;
  var Stream: TStream);
begin
  if FileExists(Path1 + ModuleName) then
    Stream := TFileStream.Create(Path1 + ModuleName, fmOpenRead)
  else if FileExists(Path2 + ModuleName) then
    Stream := TFileStream.Create(Path2 + ModuleName, fmOpenRead)
  else if FileExists(Path3 + ModuleName) then
    Stream := TFileStream.Create(Path3 + ModuleName, fmOpenRead)
  else if FileExists(ModuleName) then
    Stream := TFileStream.Create(ModuleName, fmOpenRead);
end;

var
  Engine: TThorium;
  Module: TThoriumModule;
  I: Integer;
  BinStream: TFileStream;
  Zip: Boolean;
begin
  Path1 := ExtractFilePath(ParamStr(1));
  Path2 := IncludeTrailingPathDelimiter(GetCurrentDir);
  Path3 := ExtractFilePath(ParamStr(0));
  // Create a thorium context
  Engine := TThorium.Create;
  try
    // Set callback for loading modules
    Engine.OnOpenModule := TThoriumOnOpenModule(ThoriumMakeOOPEvent(@OnOpenModule, nil));
    // Load the standard libraries
    Engine.LoadLibrary(TThoriumLibStd);
    Engine.LoadLibrary(TThoriumLibStdIO);
    Engine.LoadLibrary(TThoriumLibStreams);
    Engine.LoadLibrary(TThoriumLibString);
    WriteLn('Thorium sample compiler');
    Zip := (ParamStr(2) = 'z') or (ParamStr(2) = 'zip');
    if Zip then
      WriteLn(' will try to compress binaries.')
    else
      WriteLn(' won''t try to compress binaries.');
    WriteLn('Search paths:');
    WriteLn('  ', Path1);
    WriteLn('  ', Path2);
    WriteLn('  ', Path3);
    // Load the script
    Engine.LoadModuleFromFile(ParamStr(1));
    // Now lets save all loaded and compiled modules.
    for I := 0 to Engine.ModuleCount - 1 do
    begin
      Module := Engine.Module[I];
      Module.Compress := Zip;
      BinStream := TFileStream.Create(Module.Name + '.tsb', fmCreate);
      try
        Module.SaveToStream(BinStream);
      finally
        BinStream.Free;
      end;
    end;
  finally
    // This can be called even if the vm has not been initialized yet.
    Engine.ReleaseVirtualMachine;
    Engine.Free;
  end;
end.

